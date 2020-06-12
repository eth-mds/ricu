
test_that("slide", {

  fun <- function(x) sum(x)

  tbl <- ts_tbl(x = c(rep(1, 9), rep(2, 12), rep(3, 15)),
                y = hours(c(1:9, seq(-2, 20, 2), rev(6:20))),
                z = seq_len(12 * 3))

  res <- slide(tbl, sum(z), before = hours(3L))

  expect_is(res, "ts_tbl")
  expect_identical(ncol(tbl), ncol(tbl))

  expect_identical(res, slide(tbl, fun(z), before = hours(3L)))

  res <- slide_index(tbl, sum(z), hours(c(2L, 6L)), before = hours(3L))

  expect_is(res, "ts_tbl")

  expect_identical(res, slide_index(tbl, fun(z), hours(c(2L, 6L)),
                                    before = hours(3L)))

  wins <- id_tbl(x = rep(1:3, each = 2), min_time = hours(rep(c(2, 4), 3)),
                 max_time = hours(rep(c(6, 5), 3)), id = "x")

  res <- hop(tbl, sum(z), wins)

  expect_is(res, "ts_tbl")

  expect_identical(res, hop(tbl, fun(z), wins))

  expect_error(hop(tbl, fun(z), wins, sub_env = emptyenv()))
})
