
progress_init <- function(lenth = NULL, msg = "loading", ...) {

  cb_fun <- function(x) {

    for (out in combine_messages(x)) {
      if (has_length(out$msgs)) {
        cli::cli_alert_warning(out$head)
        cli_ul(out$msgs)
      } else if (has_length(out$head)) {
        cli::cli_alert_success(out$head)
      }
    }

    cli::cli_rule()
  }

  if (interactive() && is_pkg_available("progress") && lenth > 1L) {

    res <- progress::progress_bar$new(
      format = ":what [:bar] :percent", total = lenth, callback = cb_fun, ...
    )

  } else {
    res <- NULL
  }

  if (not_null(msg)) {
    cli::cli_rule(msg, .envir = parent.frame())
  }

  res
}

progress_tick <- function(info = NULL, progress_bar = NULL, length = 1L) {

  if (isFALSE(progress_bar)) {
    return(invisible(NULL))
  }

  if (is.null(progress_bar)) {

    if (not_null(info)) {
      msg_ricu(paste(symbol$bullet, info))
    }

    return(invisible(NULL))
  }

  assert_that(inherits(progress_bar, "progress_bar"))

  old_token <- attr(progress_bar, "token")

  if (not_null(info)) {

    if (nchar(info) > 15L) {
      ellip <- symbol$ellipsis
      token <- paste0(substr(info, 1L, 15L - nchar(ellip)), ellip)
    } else {
      token <- sprintf("%-15s", info)
    }

    progress_bar$tick(len = length, tokens = list(what = token))
    attr(progress_bar, "token") <- token


    attr(progress_bar, "output")   <- combine_messages(progress_bar)
    attr(progress_bar, "header")   <- info
    attr(progress_bar, "messages") <- character(0L)

  } else if (not_null(old_token)) {

    progress_bar$tick(len = length, tokens = list(what = old_token))

  } else {

    progress_bar$tick(len = length)
  }

  invisible(NULL)
}

combine_messages <- function(x) {

  head <- attr(x, "header")
  msgs <- attr(x, "messages")
  prev <- attr(x, "output")

  if (is.null(head) && is.null(msgs)) {
    prev
  } else {
    c(prev, list(list(head = head, msgs = msgs)))
  }
}

#' @rdname load_concepts
#' @export
progress_msg <- function(...) {

  msg <- simpleMessage(.makeMessage(...))
  class(msg) <- c("progress_msg", "ricu_msg", class(msg))

  message(msg)
}

create_progress_handler <- function(prog) {

  if (isFALSE(prog)) {

    function(msg) invokeRestart("muffleMessage")

  } else if (is.null(prog)) {

    function(msg) {

      cli_ul(conditionMessage(msg))
      invokeRestart("muffleMessage")
    }

  } else {

    function(msg) {

      msgs <- c(attr(prog, "messages"), conditionMessage(msg))
      attr(prog, "messages") <- msgs

      invokeRestart("muffleMessage")
    }
  }
}

with_progress <- function(expr, progress_bar = NULL) {

  res <- withCallingHandlers(expr,
    progress_msg = create_progress_handler(progress_bar)
  )

  if (inherits(progress_bar, "progress_bar") && !progress_bar$finished) {
    progress_bar$update(1)
  } else if (is.null(progress_bar)) {
    cli::cli_rule()
  }

  res
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

  if (not_null(msg)) {
    msg <- cli_format( {{ msg }}, env)
  }

  res <- see_if(..., env = env, msg = msg)

  if (isTRUE(res)) {
    return(TRUE)
  }

  msg <- attr(res, "msg")
  cls <- c(attr(msg, "assert_class"), "assertError", "ricu_err")

  rlang::abort(msg, class = cls)
}

cli_format <- function(message, envir) {

  msg <- rlang::enquo(message)

  if (rlang::quo_is_symbol(msg) || quo_is_syntactic_literal(msg)) {
    msg <- cli::cli_format_method(cli_text(message, .envir = envir))
  } else {
    msg <- cli::cli_format_method(message)
  }

  paste(msg, collapse = "\n")
}

quo_is_syntactic_literal <- function(x) {
  rlang::is_quosure(x) && rlang::is_syntactic_literal(rlang::quo_get_expr(x))
}

msg_ricu <- function(message = NULL, class = NULL, envir = parent.frame(),
                     ...) {
  rlang::inform(
    cli_format( {{ message }}, envir), class = c(class, "ricu_msg"), ...
  )
}

warn_ricu <- function(message = NULL, class = NULL, envir = parent.frame(),
                     ...) {
  rlang::warn(
    cli_format( {{ message }}, envir), class = c(class, "ricu_warn"), ...
  )
}

stop_ricu <- function(message = NULL, class = NULL, envir = parent.frame(),
                      ...) {
  rlang::abort(
    cli_format( {{ message }}, envir), class = c(class, "ricu_err"), ...
  )
}

top_n <- function(x, n = 1L) head(order(x), n = n)

#' @importFrom utils adist
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

warn_arg <- function(args) {
  assert_that(is.character(args), has_length(args))
  warn_ricu("Ignoring argument{?s} passed as {quote_bt(args)}",
             class = "arg_ignored")
}

warn_dots <- function(...) {

  if (...length() > 0L) {
    warn_arg("...")
  }

  invisible(NULL)
}

warn_dot_ident <- function(x, ...)  {

  warn_dots(...)

  x
}

format_assert <- function(message, class, envir = parent.frame()) {

  res <- cli_format( {{ message }}, envir)
  attr(res, "assert_class") <- class

  res
}
