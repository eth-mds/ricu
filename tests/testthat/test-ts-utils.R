
test_that("slide", {

  tbl <- ts_tbl(x = c(rep(1, 9), rep(2, 12), rep(3, 15)),
                y = hours(c(1:9, seq(-2, 20, 2), rev(6:20))),
                z = seq_len(12 * 3), id = "x")

  res <- slide(tbl, sum(z), before = hours(3L))

  expect_is(res, "ts_tbl")

  res <- slide_index(tbl, sum(z), hours(c(2L, 6L)), before = hours(3L))

  expect_is(res, "ts_tbl")

  wins <- id_tbl(x = rep(1:3, each = 2), min_time = hours(rep(c(2, 4), 3)),
                 max_time = hours(rep(c(6, 5), 3)), id = "x")

  res <- hop(tbl, sum(z), wins)

  expect_is(res, "ts_tbl")
})
