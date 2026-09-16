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
(function () {
  // The bundle (inst/htmlwidgets/vgplotr.yaml declares it, auto-injected as
  // <script type="module">) sets window.__vgplotrBundle as a side effect
  // rather than being import()-ed by this script directly: it and
  // vgplotr.js are two separately-versioned htmlwidgets dependencies that
  // land in sibling (not nested) directories once copied into a rendered
  // document, so this script can't reliably compute a relative path to it
  // without hardcoding that layout. Waiting on a global sidesteps that --
  // module scripts execute before HTMLWidgets' own static render pass
  // calls renderValue(), so in practice the bundle is already there by the
  // time this runs; the wait is just a safety margin against ordering
  // differences across renderers/viewers.
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
  function localDuckdbBundle() {
    var wasmLink = document.getElementById("vgplotr-duckdb-wasm-wasm-attachment");
    var workerLink = document.getElementById("vgplotr-duckdb-wasm-worker-attachment");
    if (!wasmLink || !workerLink) return null;
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

  // A page can hold multiple vgplotr widgets; they share one Coordinator
  // (and one DuckDB-WASM instance) rather than each spinning up their own.
  // Tables are namespaced by the names the user gave vg_data(), so this is
  // fine as long as different widgets don't reuse the same table name for
  // different data. The singleton is built from a promise, set synchronously
  // on the first call, so concurrent widget renders don't race to create it.
  function getCoordinator(mosaicCore, duckdbWasm) {
    if (!window.__vgplotrCoordinatorPromise) {
      window.__vgplotrCoordinatorPromise = (async function () {
        var coord = mosaicCore.coordinator();
        var bundle = localDuckdbBundle();
        var connector = bundle
          ? mosaicCore.wasmConnector({ duckdb: await instantiateLocalDuckDB(duckdbWasm, bundle) })
          : mosaicCore.wasmConnector();
        coord.databaseConnector(connector);
        return coord;
      })();
    }
    return window.__vgplotrCoordinatorPromise;
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

  async function renderVgplotr(el, x) {
    var mod = await waitForBundle();

    var coord = await getCoordinator(mod.mosaicCore, mod.duckdbWasm);
    await loadTables(coord, x.tables);
    await loadFiles(coord, x.files);

    var ast = mod.mosaicSpec.parseSpec(x.spec);
    var app = await mod.mosaicSpec.astToDOM(ast);
    el.appendChild(app.element);
  }

  HTMLWidgets.widget({
    name: "vgplotr",
    type: "output",

    factory: function (el, width, height) {
      return {
        renderValue: function (x) {
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
