base_spec <- function() {
  vg_create() |>
    vg_data(name = "sales", data = data.frame(x = 1:3, price = c(3, 8, 5))) |>
    vg_params(width = 10, height = 5, scale = 2, dataset = "a", zoom = 3, clicks = 0) |>
    vg_mark_dot(data_from = "sales", x = ~x, y = ~price)
}
computed_sql <- function(f, from = NULL) {
  vg_computed_param(base_spec(), out = f, from = from)$computed_params$out$sql
}

test_that("expressions translate to SQL, with bare names as params or columns", {
  expect_identical(computed_sql(~ width * height), "($width * $height)")
  expect_identical(computed_sql(~ scale * max(price), from = "sales"), "($scale * max(\"price\"))")
  expect_identical(computed_sql(~ -width + 2^height), "(-($width) + power(2, $height))")
  expect_identical(computed_sql(~ width %% 3 == 1 & !(height > 2)),
                   "((($width % 3) = 1) AND (NOT (($height > 2))))")
  expect_identical(computed_sql(~ ifelse(dataset == "a", 1, NA)),
                   "(CASE WHEN ($dataset = 'a') THEN 1 ELSE NULL END)")
  expect_identical(computed_sql(~ mean(price) + n(), from = "sales"), "(avg(\"price\") + count(*))")
  expect_identical(computed_sql(~ pmax(width, height)), "greatest($width, $height)")
  expect_identical(computed_sql(~ round(sqrt(width), 2)), "round(sqrt($width), 2)")
  expect_identical(computed_sql(~ is.na(width)), "($width IS NULL)")
})

test_that("param() and .data$ say which one is meant when names are shared", {
  spec <- vg_create() |>
    vg_data(name = "d", data = data.frame(width = 1:3)) |>
    vg_params(width = 2)
  f <- function(expr) vg_computed_param(spec, out = expr, from = "d")$computed_params$out$sql
  expect_identical(f(~ width), "$width")
  expect_identical(f(~ max(.data$width)), "max(\"width\")")
  expect_identical(f(~ max(.data[["width"]]) * param(width)), "(max(\"width\") * $width)")
})

test_that("sql() inserts raw SQL, and a variable holding a param() works", {
  expect_identical(computed_sql(sql("$width * 2")), "$width * 2")
  expect_identical(computed_sql(~ sql("$width * 2") + 1), "(($width * 2) + 1)")
  w <- param(width)
  expect_identical(computed_sql(~ w * 2), "($width * 2)")
})

test_that("a computed param records the params it uses, and can use earlier ones", {
  spec <- base_spec() |> vg_computed_param(area = ~ width * height, half = ~ area / 2)
  expect_identical(spec$computed_params$area$params, c("width", "height"))
  expect_identical(spec$computed_params$half$params, "area")
})

test_that("vg_computed_param() errors clearly", {
  spec <- base_spec()
  expect_error(vg_computed_param(spec, out = ~ width * price), "`price` isn't a declared param.*from = ")
  expect_error(vg_computed_param(spec, width = ~ height), "`width` already exists")
  expect_error(vg_computed_param(spec, ~ height), "Give each computed param a name")
  expect_error(vg_computed_param(spec, out = ~ price, from = "nope"), "no data source named `nope`")
  expect_error(vg_computed_param(spec, out = ~ f(a = 1)), "named arguments")
  expect_error(vg_computed_param(spec, out = sql("$nope + 1")), "`nope` isn't a declared param")
  expect_error(vg_computed_param(spec, out = 3), "one-sided formula")
})

test_that("vg_on_change() stores constant and computed actions", {
  spec <- base_spec() |>
    vg_on_change(param(dataset), vg_set_param(param(zoom), 1), vg_set_param(param(clicks), ~ clicks + 1))
  h <- spec$on_change[[1]]
  expect_identical(h$trigger, "dataset")
  expect_identical(h$actions[[1]], list(target = "zoom", value = 1))
  expect_identical(h$actions[[2]]$sql, "($clicks + 1)")
  expect_identical(h$actions[[2]]$params, "clicks")
})

test_that("vg_on_change() rejects self-updates, computed targets, unknown params and loops", {
  spec <- base_spec() |> vg_computed_param(area = ~ width * height)
  expect_error(vg_on_change(spec, param(zoom), vg_set_param(param(zoom), 1)), "can't set the param it watches")
  expect_error(vg_on_change(spec, param(zoom), vg_set_param(param(area), 1)), "is a computed param")
  expect_error(vg_on_change(spec, param(nope), vg_set_param(param(zoom), 1)), "`nope` isn't a declared param")
  expect_error(vg_on_change(spec, param(zoom), vg_set_param(param(nope), 1)), "`nope` isn't a declared param")
  expect_error(vg_on_change(spec, param(zoom), "x"), "vg_set_param")
  expect_error(vg_on_change(spec, param(area), vg_set_param(param(width), 1)),
               "loop: width -> area -> width")
  looped <- vg_on_change(spec, param(zoom), vg_set_param(param(clicks), 1))
  expect_error(vg_on_change(looped, param(clicks), vg_set_param(param(zoom), 1)), "loop")
})

spec_with_derived <- function() {
  base_spec() |>
    vg_computed_param(area = ~ width * height) |>
    vg_computed_param(top = ~ scale * max(price), from = "sales", filter_by = param(brush)) |>
    vg_on_change(param(dataset), vg_set_param(param(zoom), 1))
}

test_that("computed params are declared as null params, and the rest goes in the vgplotr key", {
  json <- jsonlite::fromJSON(to_json(spec_with_derived()), simplifyVector = FALSE)
  expect_null(json$params$area)
  expect_true("area" %in% names(json$params))
  expect_identical(json$vgplotr$computedParams[[2]],
                   list(name = "top", sql = "($scale * max(\"price\"))", params = list("scale"),
                        from = "sales", filterBy = "brush"))
  expect_identical(json$vgplotr$onChange[[1]], list(trigger = "dataset", actions = list(list(target = "zoom", value = 1L))))
  yaml <- yaml::yaml.load(to_yaml(spec_with_derived()))
  expect_identical(yaml$vgplotr$computedParams[[1]]$sql, "($width * $height)")
})

test_that("vgplotr_keys = FALSE leaves the vgplotr key out", {
  expect_false("vgplotr" %in% names(jsonlite::fromJSON(to_json(spec_with_derived(), vgplotr_keys = FALSE))))
  expect_false("vgplotr" %in% names(yaml::yaml.load(to_yaml(spec_with_derived(), vgplotr_keys = FALSE))))
  expect_false("vgplotr" %in% names(jsonlite::fromJSON(to_json(base_spec()))))
})

test_that("the widget gets the additions separately, including from exported JSON/YAML", {
  spec <- spec_with_derived()
  payload <- as_spec_payload(spec)
  expect_null(payload$spec$vgplotr)
  expect_identical(payload$derived, derived_payload(spec))
  for (text in list(as.character(to_json(spec)), as.character(to_yaml(spec)))) {
    from_text <- as_spec_payload(text)
    expect_null(from_text$spec$vgplotr)
    expect_identical(from_text$derived$computedParams[[1]]$sql, "($width * $height)")
    expect_identical(from_text$derived$onChange[[1]]$trigger, "dataset")
  }
})
