# vgplotr (development version)

* Initial public beta. The main functionality is in place, built on Mosaic
  0.31.0 (bundled with the package, so no JavaScript setup is needed).
  * Every mark, interactor, input, scale and guide in Mosaic's specification
    has a vgplotr function with snake_case arguments, generated from Mosaic's
    schema.
  * Params, selections and cross-filtering, plus vgplotr's own computed params
    (`vg_computed_param()`) and change handlers (`vg_on_change()`).
  * Specs can be rendered as htmlwidgets in Quarto and R Markdown documents,
    as static snapshots, or exported with `to_json()` and `to_yaml()`.
  * Data can come from R data frames, files or URLs, and is queried by DuckDB,
    either in the browser (DuckDB-WASM) or in the R session.
* All the examples at <https://idl.uw.edu/mosaic/examples/> have been
  recreated using vgplotr; see the package website,
  <https://rpruim.github.io/vgplotr/>.
