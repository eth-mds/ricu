
is_interactive <- function() {
  !isTRUE(getOption('knitr.in.progress')) && interactive()
}

progress_init <- function(lenth = NULL, msg = "loading", ...) {

  cb_fun <- function(x) {

    for (out in combine_messages(x)) {
      msg_ricu(bullet(out$head), "progress_header", exdent = 2L)
      for (msg in out$msgs) {
        msg_ricu(bullet(msg, level = 2L), "progress_body", indent = 2L,
                 exdent = 4L)
      }
    }

    cli::cli_rule()
  }

  if (is_interactive() && is_pkg_installed("progress") && lenth > 1L) {

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
      msg_ricu(bullet(info), "progress_header", exdent = 2L)
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

#' Message signaling nested with progress reporting
#'
#' In order to not interrupt progress reporting by a [progress::progress_bar],
#' messages are wrapped with class `msg_progress` which causes them to be
#' captured printed after progress bar completion. This function is intended to
#' be used when signaling messages in callback functions.
#'
#' @param ... Passed to [base::.makeMessage()]
#' @param envir Passed to [glue::glue()].
#'
#' @return Called for side effects and returns `NULL` invisibly.
#'
#' @examples
#' msg_progress("Foo", "bar")
#'
#' capt_fun <- function(x) {
#'   message("captured: ", conditionMessage(x))
#' }
#'
#' tryCatch(msg_progress("Foo", "bar"), msg_progress = capt_fun)
#'
#' @export
msg_progress <- function(..., envir = parent.frame()) {
  msg_ricu(.makeMessage(...), "msg_progress", envir = envir)
}

create_progress_handler <- function(prog) {

  if (isFALSE(prog)) {

    function(msg) invokeRestart("muffleMessage")

  } else if (is.null(prog)) {

    function(msg) {

      msg_ricu(bullet(conditionMessage(msg), level = 2L), "progress_body",
               indent = 2L, exdent = 4L)

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
    msg_progress = create_progress_handler(progress_bar)
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

bullet <- function(..., level = 1L) {
  assert_that(is.count(level), level <= 3L)
  paste0(switch(level, symbol$bullet, symbol$circle, "-"), " ", ...)
}

fmt_msg <- function(msg, envir = parent.frame(), indent = 0L, exdent = 0L) {

  msg <- chr_ply(msg, pluralize, .envir = envir)
  msg <- map(fansi::strwrap2_ctl, msg, indent = indent, exdent = exdent,
             MoreArgs = list(simplify = TRUE, wrap.always = TRUE))

  paste(do.call("c", msg), collapse = "\n")
}

msg_ricu <- function(message, class = NULL, envir = parent.frame(),
                     indent = 0L, exdent = 0L, ...) {
  rlang::inform(
    fmt_msg(message, envir = envir, indent = indent, exdent = exdent),
    class = c(class, "ricu_msg"), ...
  )
}

warn_ricu <- function(message, class = NULL, envir = parent.frame(),
                     indent = 0L, exdent = 0L, ...) {
  rlang::warn(
    fmt_msg(message, envir = envir, indent = indent, exdent = exdent),
    class = c(class, "ricu_warn"), ...
  )
}

stop_ricu <- function(message, class = NULL, envir = parent.frame(),
                      indent = 0L, exdent = 0L, ...) {
  rlang::abort(
    fmt_msg(message, envir = envir, indent = indent, exdent = exdent),
    class = c(class, "ricu_err"), ...
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

warn_dots <- function(..., ok_args = NULL) {

  if (...length() > 0L) {

    args <- setdiff(names(match.call(expand.dots = FALSE)$`...`), ok_args)

    if (has_length(args)) {
      warn_arg("...")
    }
  }

  invisible(NULL)
}

warn_dot_ident <- function(x, ...)  {

  warn_dots(...)

  x
}

format_assert <- function(message, class, envir = parent.frame(), ...) {

  res <- fmt_msg(message, envir = envir, ...)
  attr(res, "assert_class") <- class

  res
}

stop_generic <- function(x, fun) {

  assert_that(is.string(fun))

  stop_ricu("No applicable method for generic function `{fun}()` and
             class{?es} {quote_bt(class(x))}.", class = "generic_no_fun")
}

# the following functions are added until `cli` is next updated on CRAN
# see https://github.com/r-lib/cli/pull/155

pluralize <- function(..., .envir = parent.frame(),
                      .transformer = glue::identity_transformer) {

  values <- new.env(parent = emptyenv())
  values$empty <- new_names(length = 7L)
  values$qty <- values$empty
  values$num_subst <- 0L
  values$postprocess <- FALSE
  values$pmarkers <- list()

  tf <- function(text, envir) {
    if (substr(text, 1, 1) == "?") {
      if (identical(values$qty, values$empty)) {
        values$postprocess <- TRUE
        id <- new_names(length = 7L)
        values$pmarkers[[id]] <- text
        return(id)
      } else {
        return(process_plural(make_quantity(values$qty), text))
      }

    } else {
      values$num_subst <- values$num_subst + 1
      qty <- .transformer(text, envir)
      values$qty <- qty
      return(inline_collapse(qty))
    }
  }

  raw <- glue::glue(..., .envir = .envir, .transformer = tf)
  post_process_plurals(raw, values)
}

make_quantity <- function(object) {
  val <- if (is.numeric(object)) {
    stopifnot(length(object) == 1)
    as.integer(object)
  } else {
    length(object)
  }
}

process_plural <- function(qty, code) {
  parts <- strsplit(str_tail(code), "/", fixed = TRUE)[[1]]
  if (length(parts) == 1) {
    if (qty != 1) parts[1] else ""
  } else if (length(parts) == 2) {
    if (qty == 1) parts[1] else parts[2]
  } else if (length(parts) == 3) {
    if (qty == 0) {
      parts[1]
    } else if (qty == 1) {
      parts[2]
    } else {
      parts[3]
    }
  } else {
    stop("Invalid pluralization directive: `", code, "`")
  }
}

post_process_plurals <- function(str, values) {
  if (!values$postprocess) return(str)
  if (values$num_subst == 0) {
    stop("Cannot pluralize without a quantity")
  }
  if (values$num_subst != 1) {
    stop("Multiple quantities for pluralization")
  }

  qty <- make_quantity(values$qty)
  for (i in seq_along(values$pmarkers)) {
    mark <- values$pmarkers[i]
    str <- sub(names(mark), process_plural(qty, mark[[1]]), str)
  }

  str
}

str_tail <- function(x) {
  substr(x, 2, nchar(x))
}

inline_collapse <- function(x) {
  if (length(x) >= 3) {
    glue::glue_collapse(x, sep = ", ", last = ", and ")
  } else {
    glue::glue_collapse(x, sep = ", ", last = " and ")
  }
}
