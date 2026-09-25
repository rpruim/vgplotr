// The mosaic JS runtime (mosaic-spec + mosaic-core + duckdb-wasm's own JS
// API) is vendored locally (lib/mosaic-bundle.min.js, built by
// data-raw/js/build.js) -- no CDN needed for the library code itself.
//
// duckdb-wasm's actual database engine (a compiled WebAssembly binary,
// ~35MB) is a different matter: it's too large to ship inside the R
// package, so by default it's still fetched from jsdelivr on first use,
// same as before. A user can instead run vg_cache_duckdb() once to cache
// it locally (see R/duckdb_cache.R); when that cache exists, vg_render()
// attaches it to the page as an HTML dependency (an `attachment` link,
// which just holds a URL -- the file itself is copied alongside the
// rendered output, not embedded in it) and this script uses that instead
// of touching the network at all.
//
// This is all the *default* connector (vg_wasm_connector()). A widget can
// instead ask for vg_duckdb_connector() (R/connector.R) -- a real, native
// DuckDB reached over a local REST server (R/duckdb_server.R) instead of
// DuckDB-Wasm in-browser -- flagged by an `x.connector` field on the widget
// payload; see getCoordinator()/renderVgplotr() below for where the two
// paths diverge.
(function () {
  // The bundle (inst/htmlwidgets/vgplotr.yaml declares it, auto-injected as
  // a plain classic <script> -- deliberately not `type: module`, see
  // data-raw/js/build.js's own comment on why) sets window.__vgplotrBundle
  // as a side effect rather than being import()-ed by this script
  // directly: it and vgplotr.js are two separately-versioned htmlwidgets
  // dependencies that land in sibling (not nested) directories once
  // copied into a rendered document, so this script can't reliably
  // compute a relative path to it without hardcoding that layout. Waiting
  // on a global sidesteps that -- a synchronous classic script runs
  // immediately as the parser reaches it, well before HTMLWidgets' own
  // static render pass calls renderValue(), so in practice the bundle is
  // already there by the time this runs; the wait is just a safety margin
  // against ordering differences across renderers/viewers.
  function waitForBundle() {
    return new Promise(function (resolve) {
      (function check() {
        if (window.__vgplotrBundle) resolve(window.__vgplotrBundle);
        else setTimeout(check, 10);
      })();
    });
  }

  // If vg_cache_duckdb() has populated the cache, vg_render() attaches it
  // as an HTML dependency named "vgplotr-duckdb-wasm" with `wasm`/`worker`
  // attachments -- see htmltools::htmlDependency()'s `attachment` docs for
  // this <link rel="attachment"> + getElementById(...).href convention.
  //
  // The cache can't be used from a page opened directly from disk, though:
  // there the attachments resolve to file:// URLs, which a browser won't
  // let the page fetch() (the wasm binary) or importScripts() into a worker
  // (the worker script) -- Firefox reports "NetworkError when attempting to
  // fetch resource", Chrome "importScripts ... failed to load" (confirmed
  // directly, a Positron/RStudio console opens exactly such a page). Only
  // an http(s) URL works, so in that case return null and let the caller
  // fall back to fetching the engine from the CDN, which does work from a
  // file:// page.
  function localDuckdbBundle() {
    var wasmLink = document.getElementById("vgplotr-duckdb-wasm-wasm-attachment");
    var workerLink = document.getElementById("vgplotr-duckdb-wasm-worker-attachment");
    if (!wasmLink || !workerLink) return null;
    if (/^file:/i.test(wasmLink.href) || /^file:/i.test(workerLink.href)) {
      console.info(
        "vgplotr: the locally cached DuckDB-Wasm engine can't be loaded from a " +
        "file:// page; fetching it from the CDN instead."
      );
      return null;
    }
    return { mainModule: wasmLink.href, mainWorker: workerLink.href, pthreadWorker: null };
  }

  // Replicates mosaic-core's own DuckDBWASMConnector initialization
  // (packages/mosaic/core/src/connectors/wasm.ts), but with explicit
  // local bundle URLs instead of duckdb.getJsDelivrBundles()/
  // selectBundle(), which only ever point at jsdelivr.
  async function instantiateLocalDuckDB(duckdbWasm, bundle) {
    var worker_url = URL.createObjectURL(
      new Blob(['importScripts("' + bundle.mainWorker + '");'], { type: "text/javascript" })
    );
    var worker = new Worker(worker_url);
    var db = new duckdbWasm.AsyncDuckDB(new duckdbWasm.VoidLogger(), worker);
    await db.instantiate(bundle.mainModule, bundle.pthreadWorker);
    URL.revokeObjectURL(worker_url);
    return db;
  }

  // A page can hold multiple vgplotr widgets; those using the same backend
  // share one Coordinator (and, for wasm, one DuckDB-WASM instance) rather
  // than each spinning up their own -- keyed so a page mixing the default
  // wasm connector with a vg_duckdb_connector() one (R/connector.R) gets
  // two independent coordinators instead of the first widget's choice
  // silently winning for every other widget on the page. Tables are
  // namespaced by the names the user gave vg_data(), so sharing one
  // coordinator is fine as long as different widgets don't reuse the same
  // table name for different data. Each entry is built from a promise, set
  // synchronously on first use, so concurrent widget renders don't race to
  // create it.
  function coordinatorKey(x) {
    return x.connector && x.connector.type === "rest" ? "rest:" + x.connector.uri : "wasm";
  }

  // vg_coordinator()'s options (R/coordinator.R), sent as `x.coordinator`:
  // { cache, consolidate, preagg: { enabled, schema }, logging }. Applied
  // once, when this page's coordinator is created; see getCoordinator().
  function makeLogger(level) {
    if (level === "none") return null; // mosaic swaps in its silent logger
    if (level === "errors") {
      var quiet = function () {};
      return {
        debug: quiet, info: quiet, log: quiet, group: quiet, groupCollapsed: quiet, groupEnd: quiet,
        warn: console.warn.bind(console), error: console.error.bind(console),
      };
    }
    return console; // mosaic's own default
  }

  function applyCoordinatorOptions(coord, o) {
    // consolidate(true) keeps hold of the cache in use when it is switched on,
    // so it has to be switched off and on again around a change of cache.
    coord.manager.consolidate(false);
    coord.manager.cache(o.cache);
    coord.manager.consolidate(o.consolidate);
    coord.preaggregator.enabled = o.preagg.enabled;
    coord.preaggregator.schema = o.preagg.schema;
    coord.logger(makeLogger(o.logging));
  }

  // The coordinator is one per page (per connector), created by whichever
  // widget renders first, so its options are that widget's. A later widget
  // asking for different options can't change them: say so, instead of
  // silently ignoring it. A widget that asks for none is fine either way.
  function noteCoordinatorOptions(x, key) {
    var applied = (window.__vgplotrCoordinatorOptions = window.__vgplotrCoordinatorOptions || {});
    var requested = x.coordinator ? JSON.stringify(x.coordinator) : null;
    if (!(key in applied)) {
      applied[key] = requested;
    } else if (requested !== null && requested !== applied[key]) {
      console.warn(
        "vgplotr: this page's coordinator was already created " +
        (applied[key] === null ? "with mosaic's defaults" : "with " + applied[key]) +
        " by an earlier widget, so this widget's coordinator options (" + requested + ") are ignored."
      );
    }
  }

  function getCoordinator(mosaicCore, duckdbWasm, x) {
    window.__vgplotrCoordinators = window.__vgplotrCoordinators || {};
    var key = coordinatorKey(x);
    noteCoordinatorOptions(x, key);
    if (!window.__vgplotrCoordinators[key]) {
      window.__vgplotrCoordinators[key] = (async function () {
        var coord = mosaicCore.coordinator();
        var connector;
        if (key === "wasm") {
          var bundle = localDuckdbBundle();
          connector = bundle
            ? mosaicCore.wasmConnector({ duckdb: await instantiateLocalDuckDB(duckdbWasm, bundle) })
            : mosaicCore.wasmConnector();
        } else {
          // A real DuckDB reached over vg_duckdb_connector()'s local REST
          // server (R/duckdb_server.R) -- its tables are already registered
          // server-side by the time this page ever asks, so unlike wasm
          // there's no loadTables()/loadFiles() step for this connector.
          connector = mosaicCore.restConnector({ uri: x.connector.uri });
        }
        coord.databaseConnector(connector);
        if (x.coordinator) applyCoordinatorOptions(coord, x.coordinator);
        return coord;
      })();
    }
    return window.__vgplotrCoordinators[key];
  }

  async function loadTables(coord, tables) {
    if (!tables) return;
    var db = await coord.databaseConnector().getDuckDB();
    for (var name in tables) {
      var fname = name + ".json";
      await db.registerFileText(fname, JSON.stringify(tables[name]));
      await coord.exec([
        'CREATE OR REPLACE TABLE "' + name + '" AS SELECT * FROM read_json_auto(\'' + fname + "')",
      ]);
    }
  }

  var READERS = { csv: "read_csv_auto", json: "read_json_auto", parquet: "read_parquet" };

  function base64ToBytes(base64) {
    var binary = atob(base64);
    var bytes = new Uint8Array(binary.length);
    for (var i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
    return bytes;
  }

  // Local data files (as opposed to genuine http(s) URLs, which stay in
  // the spec's own data: block -- see as_spec_payload() in R/serialize.R)
  // arrive here with their content already embedded by R, so loading them
  // never involves fetching anything: register the bytes, then run the
  // same kind of CREATE TABLE ... FROM read_*(...) that mosaic's own
  // declarative data loading would, replicating its `where`/`select`
  // handling since we're bypassing that loading path entirely.
  async function loadFiles(coord, files) {
    if (!files) return;
    var db = await coord.databaseConnector().getDuckDB();
    for (var name in files) {
      var f = files[name];
      var fname = name + "." + f.ext;
      if (f.encoding === "base64") {
        await db.registerFileBuffer(fname, base64ToBytes(f.content));
      } else {
        await db.registerFileText(fname, f.content);
      }

      var opts = f.options || {};
      var cols = "*";
      if (opts.select && opts.select.length) {
        cols = opts.select.map(function (c) { return '"' + c + '"'; }).join(", ");
      }
      var sql = 'CREATE OR REPLACE TABLE "' + name + '" AS SELECT ' + cols +
        " FROM " + READERS[f.ext] + "('" + fname + "')";
      if (opts.where) {
        var clauses = Array.isArray(opts.where) ? opts.where.join(" AND ") : opts.where;
        sql += " WHERE " + clauses;
      }
      await coord.exec([sql]);
    }
  }

  // R's js("() => 0.7") reaches the page as {js: "() => 0.7"} (a vgplotr
  // extension to mosaic-spec: JSON can't carry a function, and mosaic's
  // parseSpec() would read that object as a transform). Swap each such
  // marker for the value its code evaluates to *before* parseSpec(), which
  // passes a non-object option value (a function) through as a literal.
  // Copies rather than edits x.spec, so a re-render starts from markers
  // again. Never descends into the spec's data block or an inline `data:
  // [...]` array: those are table rows, where a column named "js" would
  // otherwise look like a marker.
  function evalJs(code) {
    try {
      return (0, eval)("(" + code + ")");
    } catch (err) {
      throw new Error("vgplotr: could not evaluate js(" + JSON.stringify(code) + "): " + err.message);
    }
  }

  function resolveJs(v, key) {
    if (Array.isArray(v)) return key === "data" ? v : v.map(function (e) { return resolveJs(e); });
    if (v === null || typeof v !== "object") return v;
    var keys = Object.keys(v);
    if (keys.length === 1 && keys[0] === "js" && typeof v.js === "string") return evalJs(v.js);
    var out = {};
    keys.forEach(function (k) { out[k] = resolveJs(v[k], k); });
    return out;
  }

  function resolveSpecJs(spec) {
    var out = {};
    Object.keys(spec).forEach(function (k) { out[k] = k === "data" ? spec[k] : resolveJs(spec[k], k); });
    return out;
  }

  // WORKAROUND for an upstream bug in mosaic-spec 0.31.0 (the latest release
  // when written): parseWindowFrame() builds each literal frame offset (a
  // number or null in `rows`/`range`/`groups`) with mosaic-*sql*'s
  // LiteralNode, which has no instantiate(), and WindowFrameNode.instantiate()
  // then calls it -- "s.instantiate is not a function", so any frame with a
  // plain offset fails to render (and takes the whole page down with it).
  // Only offsets that are themselves transforms, like {days: 7}, worked.
  //
  // TransformNode is the one piece of this that mosaic-spec exports, and it
  // owns the frame (options.frame), so wrap its instantiate() to give each
  // offset that lacks one a stand-in that returns the literal value -- which
  // is exactly what mosaic-sql's own frame code wants (a number, or null for
  // unbounded). Only an offset with no instantiate() is touched, so once
  // mosaic fixes this the wrapper does nothing; delete it then. Rebuilding
  // the bundle (data-raw/js/build.js) prints a notice when it can tell the
  // bug is gone; see "Upstream issues to watch" in AGENTS.md.
  function patchWindowFrames(mosaicSpec) {
    var TN = mosaicSpec.TransformNode;
    if (!TN || TN.prototype.__vgplotrFramePatched) return;
    var original = TN.prototype.instantiate;
    TN.prototype.instantiate = function (ctx) {
      var frame = this.options && this.options.frame;
      if (frame && Array.isArray(frame.extent)) {
        frame.extent = frame.extent.map(function (v) {
          return v && typeof v.instantiate !== "function"
            ? { instantiate: function () { return v.value; } }
            : v;
        });
      }
      return original.call(this, ctx);
    };
    TN.prototype.__vgplotrFramePatched = true;
  }

  // Linked widgets (vg_widget(link = "name")): widgets on one page that name
  // the same group share one params map, handed to astToDOM(), which skips
  // defining any param name already in it -- so a brush declared in one
  // widget is the very same Selection the other widgets' plots filter by.
  //
  // A param is shared by *name*, and whichever widget instantiates first
  // would otherwise define it: a widget that only references $brush (mosaic
  // then invents a default `intersect` selection) could beat the one that
  // declares it a crossfilter. So every widget first *registers* what it
  // declares -- synchronously from renderValue(), before any await, so all
  // the widgets of a static page have registered before the first one
  // instantiates -- and then parses with the union of the group's
  // declarations. The first declaration of a name wins; a later one that
  // differs is reported in the console.
  //
  // A widget that is rendered again (renderValue() called twice on one
  // element) leaves its earlier plots' clients connected to the group's
  // selections; nothing here disconnects them.
  function linkGroup(name) {
    var groups = (window.__vgplotrLinks = window.__vgplotrLinks || {});
    return (groups[name] = groups[name] || { params: new Map(), declared: {}, connector: null });
  }

  function registerLink(x) {
    if (!x.link) return;
    var group = linkGroup(x.link);
    var connector = coordinatorKey(x);
    if (group.connector && group.connector !== connector) {
      console.warn(
        "vgplotr: link group '" + x.link + "' mixes widgets with different connectors; " +
        "selections are shared, but the widgets query different databases."
      );
    }
    group.connector = group.connector || connector;
    var params = (x.spec && x.spec.params) || {};
    Object.keys(params).forEach(function (name) {
      var json = JSON.stringify(params[name]);
      if (!(name in group.declared)) {
        group.declared[name] = json;
      } else if (group.declared[name] !== json) {
        console.warn(
          "vgplotr: link group '" + x.link + "': param '" + name + "' is declared differently " +
          "in another widget; using the first declaration, " + group.declared[name] + "."
        );
      }
    });
  }

  function withLinkedParams(spec, group) {
    var params = Object.assign({}, spec.params);
    Object.keys(group.declared).forEach(function (name) {
      params[name] = JSON.parse(group.declared[name]);
    });
    return Object.assign({}, spec, { params: params });
  }

  async function renderVgplotr(el, x) {
    var mod = await waitForBundle();
    patchWindowFrames(mod.mosaicSpec);

    var coord = await getCoordinator(mod.mosaicCore, mod.duckdbWasm, x);
    if (coordinatorKey(x) === "wasm") {
      await loadTables(coord, x.tables);
      await loadFiles(coord, x.files);
    }

    var spec = resolveSpecJs(x.spec);
    var group = x.link ? linkGroup(x.link) : null;
    var ast = mod.mosaicSpec.parseSpec(group ? withLinkedParams(spec, group) : spec);
    var app = await mod.mosaicSpec.astToDOM(ast, group ? { params: group.params } : undefined);
    el.appendChild(app.element);
  }

  HTMLWidgets.widget({
    name: "vgplotr",
    type: "output",

    factory: function (el, width, height) {
      return {
        renderValue: function (x) {
          registerLink(x);
          el.innerHTML = "";
          renderVgplotr(el, x).catch(function (err) {
            console.error(err);
            var pre = document.createElement("pre");
            pre.style.color = "red";
            pre.style.whiteSpace = "pre-wrap";
            pre.textContent = String((err && err.stack) || err);
            el.appendChild(pre);
          });
        },
        resize: function () {},
      };
    },
  });
})();
