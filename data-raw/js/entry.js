// Bundle entry point -- see build.js. Loaded by inst/htmlwidgets/vgplotr.yaml
// as an auto-injected <script type="module">, so this runs once per page and
// sets window.__vgplotrBundle as a side effect; inst/htmlwidgets/vgplotr.js
// waits for that instead of trying to import this file itself (which would
// need to know, and keep in sync, exactly where htmlwidgets' dependency
// resolution happens to copy this file relative to vgplotr.js -- two
// separately-versioned dependencies that land in sibling, not nested,
// directories).
import * as mosaicSpec from "@uwdata/mosaic-spec";
import * as mosaicCore from "@uwdata/mosaic-core";
import * as duckdbWasm from "@duckdb/duckdb-wasm";

window.__vgplotrBundle = { mosaicSpec: mosaicSpec, mosaicCore: mosaicCore, duckdbWasm: duckdbWasm };
