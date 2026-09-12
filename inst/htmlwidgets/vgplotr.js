// First working version of the vgplotr htmlwidget.
//
// This fetches the mosaic JS runtime and a client-side DuckDB
// (duckdb-wasm) from a CDN at *view* time, rather than from locally
// vendored assets -- so viewing a rendered plot requires an internet
// connection. Vendoring these assets for offline/reproducible use is
// planned but not implemented yet (see design/api-brainstorming.qmd and
// the project's architecture notes).
//
// duckdb-wasm is pinned explicitly (via esm.sh's `?deps=` param) because
// the version mosaic-spec/mosaic-core resolve to automatically on jsdelivr
// was, at the time this was written, a broken dev prerelease that never
// finished instantiating.
(function () {
  var MOSAIC_VERSION = "0.31.0";
  var DUCKDB_WASM_VERSION = "1.29.0";
  var DEPS = "?deps=@duckdb/duckdb-wasm@" + DUCKDB_WASM_VERSION;

  function cdn(pkg) {
    return "https://esm.sh/" + pkg + "@" + MOSAIC_VERSION + DEPS;
  }

  // A page can hold multiple vgplotr widgets; they share one Coordinator
  // (and one DuckDB-WASM instance) rather than each spinning up their own.
  // Tables are namespaced by the names the user gave vg_data(), so this is
  // fine as long as different widgets don't reuse the same table name for
  // different data. The singleton is built from a promise, set synchronously
  // on the first call, so concurrent widget renders don't race to create it.
  function getCoordinator(mosaicCore) {
    if (!window.__vgplotrCoordinatorPromise) {
      window.__vgplotrCoordinatorPromise = Promise.resolve().then(function () {
        var coord = mosaicCore.coordinator();
        coord.databaseConnector(mosaicCore.wasmConnector());
        return coord;
      });
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
    var mosaicSpec = await import(cdn("@uwdata/mosaic-spec"));
    var mosaicCore = await import(cdn("@uwdata/mosaic-core"));

    var coord = await getCoordinator(mosaicCore);
    await loadTables(coord, x.tables);
    await loadFiles(coord, x.files);

    var ast = mosaicSpec.parseSpec(x.spec);
    var app = await mosaicSpec.astToDOM(ast);
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
