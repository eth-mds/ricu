
progress_init <- function(lenth = NULL, msg = "loading", ...) {

  if (interactive() && is_pkg_available("progress") && lenth > 1L) {

    cb_fun <- function(x) {

      out <- c(attr(x, "output"), combine_messages(x))

      if (has_length(out)) {
        msg_ricu(out)
      }
    }

    res <- progress::progress_bar$new(
      format = ":what [:bar] :percent", total = lenth, callback = cb_fun, ...
    )

    attr(res, "output") <- character(0L)

  } else {
    res <- NULL
  }

  if (not_null(msg)) {
    msg_ricu(paste0("\n", msg, "\n"))
  }

  res
}

progress_tick <- function(info = NULL, progress_bar = NULL, length = 1L) {

  if (isFALSE(progress_bar)) {
    return(invisible(NULL))
  }

  if (not_null(info)) {
    header <- paste(symbol$bullet, info)
  }

  if (is.null(progress_bar)) {

    if (not_null(info)) {
      msg_ricu(header)
    }

    return(invisible(NULL))
  }

  assert_that(inherits(progress_bar, "progress_bar"))

  old_token <- attr(progress_bar, "token")

  if (not_null(info)) {

    if (nchar(info) > 15L) {
      elli <- symbol$ellipsis
      info <- paste0(substr(info, 1L, 15L - nchar(elli)), elli)
    } else {
      info <- sprintf("%-15s", info)
    }

    progress_bar$tick(len = length, tokens = list(what = info))
    attr(progress_bar, "token") <- info


    attr(progress_bar, "output") <- c(
      attr(progress_bar, "output"), combine_messages(progress_bar)
    )

    attr(progress_bar, "header") <- header
    attr(progress_bar, "messages") <- character(0L)

  } else if (not_null(old_token)) {

    progress_bar$tick(len = length, tokens = list(what = old_token))

  } else {

    progress_bar$tick(len = length)
  }

  invisible(NULL)
}

combine_messages <- function(x) {

  cur_msgs <- attr(x, "messages")

  if (has_length(cur_msgs)) {

    cur_head <- attr(x, "header")

    assert_that(has_length(cur_head))

    paste(paste0(cur_head, ":"), paste(cur_msgs, collapse = ""), sep = "\n")

  } else {

    NULL
  }
}

#' @rdname load_concepts
#' @export
progress_msg <- function(...) msg_ricu(..., class = "progress_msg")

create_progress_handler <- function(progress_bar) {

  if (inherits(progress_bar, "progress_bar")) {

    function(msg) {

      if (inherits(msg, "progress_msg")) {

        msgs <- c(attr(progress_bar, "messages"), conditionMessage(msg))
        attr(progress_bar, "messages") <- msgs

        invokeRestart("muffleMessage")
      }
    }

  } else if (isFALSE(progress_bar)) {

    function(msg) {
      if (inherits(msg, "progress_msg")) invokeRestart("muffleMessage")
    }

  } else {

    stop("Cannot deal with progress_bar argument")
  }
}

with_progress <- function(expr, progress_bar = NULL) {

  if (is.null(progress_bar)) {

    expr

  } else {

    res <- withCallingHandlers(expr,
      message = create_progress_handler(progress_bar)
    )

    if (inherits(progress_bar, "progress_bar") && !progress_bar$finished) {
      progress_bar$update(1)
    }

    res
  }
}

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, format = "d", ...)
}

quote_bt <- function(x) encodeString(x, quote = "`")

concat <- function(...) paste0(..., collapse = ", ")

prcnt <- function(x, tot = sum(x)) {
  paste0(round(x / tot * 100, digits = 2), "%")
}

assert_that <- function(..., env = parent.frame(), msg = NULL) {

  res <- see_if(..., env = env, msg = msg)
  if (res) return(TRUE)

  stop_ricu(attr(res, "msg"), "assertError")
}

#' @importFrom cli style_reset
#' @importFrom rlang inform
msg_ricu <- function(..., class = NULL, reset = TRUE) {

  message <- paste(lapply(list(...), paste, collapse = ""), collapse = "")

  if (reset) {
    message <- style_reset(message)
  }

  inform(message = message, class = c(class, "ricu_msg"))
}

#' @importFrom rlang abort
stop_ricu <- function(message = NULL, class = NULL, ...) {
  abort(message, class = c(class, "ricu_error"), ...)
}

top_n <- function(x, n = 1L) head(order(x), n = n)

suggest <- function(x, opts, n = 1L, fixed = FALSE, ...) {

  dis <- adist(x, opts, fixed = fixed, ...)
  res <- apply(dis, 1L, top_n, n = n)

  if (is.null(nrow(res)) && all_equal(n, 1L)) {
    res <- t(res)
  }

  res <- split(res, col(res))
  res <- Map(`[`, list(opts), res)
  names(res) <- x

  res
}
