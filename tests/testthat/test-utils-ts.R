
test_that("collapse/expand", {

  tbl <- ts_tbl(x = 1:5, y = hours(1:5), z = hours(2:6), val = rnorm(5),
                index_var = "y")

  exp <- expand(tbl, "y", "z", step_size = 1L, new_index = "y",
                keep_vars = c("x", "val"))

  expect_s3_class(exp, "ts_tbl")
  expect_identical(meta_vars(tbl), meta_vars(exp))
  expect_identical(nrow(exp), sum(as.integer(tbl$z - tbl$x)) + nrow(tbl))

  col <- collapse(exp, start_var = "y", end_var = "z", val = unique(val))

  expect_s3_class(col, "ts_tbl")
  expect_equal(tbl, col, ignore_attr = TRUE)

  tbl <- win_tbl(x = 1:5, y = hours(1:5), z = mins(1:5 * 30), val = rnorm(5))
  exp <- expand(tbl)

  expect_s3_class(exp, "ts_tbl")
  expect_identical(meta_vars(exp), setdiff(meta_vars(tbl), "z"))
  expect_identical(nrow(exp),
    as.integer(sum(re_time(tbl$z, interval(tbl)))) + nrow(tbl)
  )
})

test_that("gaps", {

  tbl <- ts_tbl(x = rep(1L, 10L), y = hours(seq.int(1L, 13L)[-c(2L, 5L, 8L)]),
                z = rnorm(10L))

  expect_true(has_gaps(tbl))
  expect_false(has_no_gaps(tbl))
  expect_false(is_regular(tbl))
})

test_that("slide", {

  fun <- function(x) sum(x)

  tbl <- ts_tbl(x = c(rep(1, 9), rep(2, 12), rep(3, 15)),
                y = hours(c(1:9, seq(-2, 20, 2), rev(6:20))),
                z = seq_len(12 * 3))

  res <- slide(tbl, sum(z), before = hours(3L))

  expect_s3_class(res, "ts_tbl")
  expect_identical(ncol(tbl), ncol(tbl))

  expect_identical(res, slide(tbl, fun(z), before = hours(3L)))

  expect_lt(nrow(slide(tbl, sum(z), before = hours(3L), full_window = TRUE)),
            nrow(res))

  tmp <- data.table::copy(tbl)

  expect_identical(
    slide(tmp, list(zz = sum(z)), before = hours(0L), after = hours(3L)),
    slide(tmp, zz := sum(z), before = hours(0L), after = hours(3L))[,
      c("z") := NULL]
  )

  tmp <- data.table::copy(tbl)

  expect_warning(slide(tmp, zz := sum(z), before = hours(3L)),
                 class = "by_ref_slide")

  res <- slide_index(tbl, sum(z), hours(c(2L, 6L)), before = hours(3L))

  expect_s3_class(res, "ts_tbl")

  expect_identical(res, slide_index(tbl, fun(z), hours(c(2L, 6L)),
                                    before = hours(3L)))

  expect_warning(slide_index(tbl, zz := sum(z), hours(c(2L, 6L)),
                             before = hours(3L)),
                 class = "by_ref_slide")

  wins <- id_tbl(x = rep(1:3, each = 2), min_time = hours(rep(c(2, 4), 3)),
                 max_time = hours(rep(c(6, 5), 3)))

  res <- hop(tbl, sum(z), wins)

  expect_s3_class(res, "id_tbl")

  expect_identical(res, hop(tbl, fun(z), wins))

  tmp <- data.table::copy(tbl)

  expect_warning(hop(tmp, zz := sum(z), wins), class = "by_ref_slide")

  expect_error(hop(tbl, fun(z), wins, sub_env = emptyenv()))
})
