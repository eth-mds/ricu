
progress_init <- function(lenth = NULL, msg = "loading", ...) {

  if (interactive() && is_pkg_available("progress") && lenth > 1L) {

    cb_fun <- function(x) {

      out <- attr(x, "output")

      if (has_length(out)) {
        lapply(out, message, appendLF = FALSE)
      }
    }

    res <- progress::progress_bar$new(
      format = ":what [:bar] :percent", total = lenth, callback = cb_fun, ...
    )

    if (not_null(msg)) {
      res$message(msg)
    }

    attr(res, "output") <- character(0L)

  } else {

    message(msg)
    res <- NULL
  }

  res
}

progress_tick <- function(info = NULL, progress_bar = NULL, length = 1L) {

  if (isFALSE(progress_bar)) {
    return(invisible(NULL))
  }

  if (not_null(info)) {
    header <- paste0("* `", info, "`")
  }

  if (is.null(progress_bar)) {

    if (not_null(info)) {
      message(header)
    }

    return(invisible(NULL))
  }

  assert_that(inherits(progress_bar, "progress_bar"))

  old_token <- attr(progress_bar, "token")

  if (not_null(info)) {

    if (nchar(info) > 15L) {
      info <- paste0(substr(info, 1L, 15L - nchar(ellipsis())), ellipsis())
    } else {
      info <- sprintf("%-15s", info)
    }

    progress_bar$tick(len = length, tokens = list(what = info))
    attr(progress_bar, "token") <- info

    cur_msgs <- attr(progress_bar, "messages")

    if (has_length(cur_msgs)) {

      cur_head <- attr(progress_bar, "header")

      assert_that(has_length(cur_head))

      msg <- paste(cur_head, paste(cur_msgs, collapse = ""), sep = "\n")

      attr(progress_bar, "output") <- c(attr(progress_bar, "output"), msg)
    }

    attr(progress_bar, "header") <- header
    attr(progress_bar, "messages") <- character(0L)

  } else if (not_null(old_token)) {

    progress_bar$tick(len = length, tokens = list(what = old_token))

  } else {

    progress_bar$tick(len = length)
  }

  invisible(NULL)
}

#' @rdname load_concepts
#' @export
progress_msg <- function(...) {

  msg <- .makeMessage(...)
  call <- sys.call()

  msg <- simpleMessage(msg, call)
  class(msg) <- c("progress_msg", class(msg))

  message(msg)
}

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

cat_line <- function(...) {
  line <- trimws(paste0(...), "right")
  cat(paste0(line, "\n"), sep = "")
}

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, format = "d", ...)
}

times <- function(fancy = l10n_info()$`UTF-8`) if (fancy) "\u00d7" else "x"

arrow <- function(fancy = l10n_info()$`UTF-8`) if (fancy) "\u2192" else "->"

ellipsis <- function(fancy = l10n_info()$`UTF-8`) {
  if (fancy) "\u2026" else "..."
}

quote_bt <- function(x) encodeString(x, quote = "`")

concat <- function(...) paste0(..., collapse = ", ")

prcnt <- function(x, tot = sum(x)) {
  paste0(round(x / tot * 100, digits = 2), "%")
}
