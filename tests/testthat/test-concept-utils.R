
test_that("item constructor", {

  itms1 <- new_item("a", "src", "tbl", "col")

  expect_is(itms1, "item")
  expect_true(is_item(itms1))

  expect_length(itms1, 1L)
  expect_named(itms1, "a")

  itms2 <- c(itm_a, new_item("b", "src", "tbl", "col"))

  expect_is(itms2, "item")
  expect_true(is_item(itms2))

  expect_length(itms2, 2L)
  expect_named(itms2, c("a", "b"))

  itms3 <- c(itms2, new_item("c", "src", "tbl", "col"))

  expect_is(itms3, "item")
  expect_true(is_item(itms3))

  expect_length(itms3, 3L)
  expect_named(itms3, c("a", "b", "c"))
})

test_that("concept/dictionary constructor", {

  conc1 <- new_concept("a", new_item("a", "src", "tbl", "col"))

  expect_is(conc1, "concept")
  expect_true(is_concept(conc1))

  expect_length(conc1, 1L)
  expect_named(conc1, "a")

  conc2 <- new_concept("b", c(new_item("b", "src", "tbl", "col"),
                              new_item("b", "src", "tbl", "col")))

  expect_is(conc2, "concept")
  expect_true(is_concept(conc2))

  expect_length(conc2, 1L)
  expect_named(conc2, "b")

  conc3 <- c(conc1, conc2)

  expect_is(conc3, "concept")
  expect_true(is_concept(conc3))

  expect_length(conc3, 2L)
  expect_named(conc3, c("a", "b"))

  dict <- new_dictionary(conc3)

  expect_is(dict, "dictionary")
  expect_true(is_dictionary(dict))

  expect_length(dict, 2L)
  expect_named(dict, c("a", "b"))
})
