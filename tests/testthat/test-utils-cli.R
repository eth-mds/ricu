skip_on_cran()

test_that("cli progress", {

  skip_if_not_installed("mockthat")

  do_stuff <- function(pb = NULL) {

    for (ntk in split(seq_len(10), (rep(1:7, times = c(rep(1:2, 3), 1))))) {

      if (not_null(pb)) {
        expect_false(pb$finished)
      }

      if (length(ntk) == 1L) {

        msg <- paste0(rep(paste0(letters[seq_len(ntk)], collapse = ""), ntk),
                      collapse = " ")

        expect_null(expect_invisible(progress_tick(msg, pb, 1L)))

      } else {

        expect_null(
          expect_invisible(
            progress_tick(progress_bar = pb, length = length(ntk))
          )
        )
      }

      for (i in ntk) {
        msg_progress("hello from index {i}")
      }
    }
  }

  pb <- mockthat::with_mock(
    is_interactive = FALSE,
    progress_init(10, NULL)
  )

  expect_null(pb)

  expect_snapshot(with_progress(do_stuff(pb), pb))

  pb <- mockthat::with_mock(
    is_interactive = TRUE,
    progress_init(10, NULL)
  )

  expect_s3_class(pb, "progress_bar")

  expect_snapshot(with_progress(do_stuff(pb), pb))
})
