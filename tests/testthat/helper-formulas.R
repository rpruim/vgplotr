# Helpers for testing how mapping formulas are checked. There are two layers:
# check_transform_calls() rejects a bad transform when the mark is *built*, and
# serialize_expr() still rejects it at render/export time as a safety net. A
# test of the second layer can't build the mark normally (the first layer would
# stop it), so these inject the formulas into an already-built mark.

# A one-mark spec whose mark is built with plain, valid formulas, and whose
# encodings are then overwritten with `...`, bypassing the early check.
spec_with_unchecked_encodings <- function(...) {
  spec <- vg_create() |>
    vg_data(name = "d", data = data.frame(delay = 1:3)) |>
    vg_mark_dot(data_from = "d", x = ~delay, y = ~delay)
  encodings <- list(...)
  spec$layout$items[[1]]$encodings[names(encodings)] <- encodings
  spec
}

# as_spec_payload() of that spec -- i.e., what render/export does.
serialize_unchecked <- function(...) as_spec_payload(spec_with_unchecked_encodings(...))

# The same, but through the normal constructor (so the early check runs too).
serialize_with <- function(...) {
  spec <- vg_create() |>
    vg_data(name = "d", data = data.frame(delay = 1:3)) |>
    vg_mark_dot(data_from = "d", ...)
  as_spec_payload(spec)
}
