
#' @rdname data_attach
#'
#' @export
#'
data <- new.env()

#' @rdname data_attach
#'
#' @export
#'
aux <- new.env()

pkg_env <- function() asNamespace(methods::getPackageName())

get_env <- function(envir = c("data", "aux")) {
  get(match.arg(envir), envir = pkg_env(), mode = "environment")
}

get_source <- function(source, envir = "data") {

  assert_that(is.string(source))

  res <- get0(source, envir = get_env(envir), mode = "environment",
              ifnotfound = NULL)

  if (is.null(res)) {
    stop("Source `", source, "` not found in ", envir, " environment. For ",
         "further information on how to set up a data source, refer to ",
         "`?attach_datasource`.")
  }

  res
}

get_table <- function(table, source, envir = "data") {

  assert_that(is.string(table))

  res <- get0(table, envir = get_source(source, envir))

  if (is.null(res)) {
    stop("Table `", table, "` not found for `", source, "` data source. For ",
         "further information on how to set up a data source, refer to ",
         "`?attach_datasource`.")
  }

  if (is_dt(res)) {
    data.table::copy(res)
  } else if (prt::is_prt(res)) {
    res
  } else {
    stop("Expecting `", source, "::", table, "` to either be a `prt` object ",
         "or inherit from `data.table`.")
  }
}