
#' @rdname src_cfg
#' @keywords internal
#'
src_url <- function(x) {
  assert_that(is_src_cfg(x))
  x[["url"]]
}

#' @export
vec_ptype_full.id_cfg <- function(x, ...) {
  x <- sort(x)
  paste0(class(x)[2L], "{", src_name(x), ": ",
         paste0(names(x), id_cfg_op(x), collapse = " "), "}")
}

id_cfg_op <- function(x) {
  op <- setNames(c(" <", " =", " >"), c("-1", "0", "1"))
  c(op[as.character(vec_compare(x[-length(x)], x[-1L]))], "")
}

#' @export
vec_ptype_abbr.id_cfg <- function(x, ...) class(x)[2L]

#' @export
format.id_cfg <- function(x, ...) paste0("`", field(x, "id"), "`")

#' @export
vec_proxy_compare.id_cfg <- function(x, ...) field(x, "pos")

#' @export
names.id_cfg <- function(x) field(x, "name")

#' @export
as.list.id_cfg <- function(x, ...) {
  warn_dots(...)
  vec_chop(x)
}

#' @rdname id_cfg
#' @keywords internal
#' @export
as_id_cfg <- function(x) UseMethod("as_id_cfg", x)

#' @rdname id_cfg
#' @keywords internal
#' @export
as_id_cfg.id_cfg <- function(x) x

#' @rdname id_cfg
#' @keywords internal
#' @export
#'
as_id_cfg.src_cfg <- function(x) x[["id_cfg"]]

#' @rdname id_cfg
#' @keywords internal
#' @export
#'
as_id_cfg.src_env <- function(x) attr(x, "id_cfg")

#' @rdname id_cfg
#' @keywords internal
#' @export
#'
as_id_cfg.default <- function(x) as_id_cfg(as_src_env(x))

get_id_var <- function(x, id_type = NULL) {

  x <- as_id_cfg(x)

  if (is.null(id_type)) {
    return(field(x, "id"))
  }

  assert_that(is.string(id_type), has_name(x, id_type))

  field(x[id_type], "id")
}

select_ids <- function(x, ids = NULL) {

  x <- as_id_cfg(x)

  if (is.null(ids)) {
    return(x)
  }

  all_ids <- field(x, "id")

  assert_that(is.character(ids), all(ids %in% all_ids))

  x[all_ids %in% ids]
}

#' @rdname col_cfg
#' @keywords internal
#' @export
#'
as_col_cfg <- function(x) UseMethod("as_col_cfg", x)

#' @rdname col_cfg
#' @keywords internal
#' @export
#'
as_col_cfg.col_cfg <- function(x) x

#' @rdname col_cfg
#' @keywords internal
#' @export
#'
as_col_cfg.src_cfg <- function(x) x[["col_cfg"]]

#' @rdname col_cfg
#' @keywords internal
#' @export
#'
as_col_cfg.src_tbl <- function(x) attr(x, "col_cfg")

#' @rdname tbl_cfg
#' @keywords internal
#' @export
#'
as_tbl_cfg <- function(x) UseMethod("as_tbl_cfg", x)

#' @rdname tbl_cfg
#' @keywords internal
#' @export
#'
as_tbl_cfg.tbl_cfg <- function(x) x

#' @rdname tbl_cfg
#' @keywords internal
#' @export
#'
as_tbl_cfg.src_cfg <- function(x) x[["tbl_cfg"]]

#' @export
dim.tbl_cfg <- function(x) {
  c(x[["nrow"]], length(x[["cols"]]))
}

src_files <- function(x) {
  assert_that(is_tbl_cfg(x))
  names(x[["files"]])
}

dst_files <- function(x) {
  assert_that(is_tbl_cfg(x))
  unlist(x[["files"]], recursive = FALSE, use.names = FALSE)
}

n_partitions <- function(x) {
  assert_that(is_tbl_cfg(x))
  length(x[["partitioning"]][["breaks"]]) + 1L
}

fst_names <- function(x) {

  assert_that(is_tbl_cfg(x))

  n_part  <- n_partitions(x)
  tbl_nme <- tbl_name(x)

  if (n_part > 1L) {
    tbl_nme <- file.path(tbl_nme, seq_len(n_part))
  }

  paste0(tbl_nme, ".fst")
}

col_spec <- function(x) {
  assert_that(is_tbl_cfg(x))
  x[["spec"]]
}

check_n_row <- function(x, n_row) {

  assert_that(is_tbl_cfg(x), is.count(n_row))

  expec <- nrow(x)

  if (is.null(expec)) {
    return(invisible(n_row))
  }

  if (!all_equal(expec, n_row)) {
    warning("Table ", quote_bt(tbl_name(x)), " has ",
            big_mark(n_row), " instead of ", big_mark(expec), " rows")
  }

  invisible(n_row)
}

col_names <- function(x) {
  setNames(x[["cols"]], names(col_spec(x)[["cols"]]))
}

partition_fun <- function(x, orig_names = FALSE) {

  assert_that(is_tbl_cfg(x), is.flag(orig_names), n_partitions(x) > 1L)

  part <- x[["partitioning"]]

  col <- part[["col"]]
  breaks <- force(part[["breaks"]])

  if (orig_names) {
    nms <- col_names(x)
    col <- names(nms[nms == col])
  }

  assert_that(is.string(col))

  function(x) {
    findInterval(x[[col]], breaks) + 1L
  }
}

#' Get default columns
#'
#' For a table, query the default columns as specified by an `id_cfg`
#' ([new_id_cfg()]) or `col_cfg` ([new_col_cfg()]) object.
#'
#' @param x Object used for dispatch
#' @param ... Generic consistency
#'
#' @rdname cfg_utils
#' @export
#'
default_var <- function(x, ...) UseMethod("default_var", x)

#' @param type The column type, e.g. `id`, `index`, `unit`, `value`
#' @rdname cfg_utils
#' @export
default_var.col_cfg <- function(x, type = "id_var", ...) {

  warn_dots(...)

  assert_that(is.string(type), has_name(x, type))

  x[[type]]
}

#' @rdname cfg_utils
#' @export
default_var.id_cfg <- function(x, ...) {
  warn_dots(...)
  field(max(x), "id")
}

#' @rdname cfg_utils
#' @export
default_var.src_tbl <- function(x, type = "id_var", ...) {

  warn_dots(...)

  assert_that(is.string(type))

  res <- default_var(as_col_cfg(x), type)

  if (identical(type, "id_var") && is.null(res)) {
    return(default_var(as_id_cfg(x)))
  }

  res
}

#' @rdname cfg_utils
#' @export
default_var.src_env <- function(x, ...) eapply(x, default_var, ...)

#' @rdname cfg_utils
#' @export
src_name <- function(x) UseMethod("src_name", x)

#' @rdname cfg_utils
#' @export
src_name.src_cfg <- function(x) x[["name"]]

#' @rdname cfg_utils
#' @export
src_name.id_cfg <- function(x) attr(x, "src")

#' @rdname cfg_utils
#' @export
src_name.col_cfg <- function(x) x[["src"]]

#' @rdname cfg_utils
#' @export
src_name.tbl_cfg <- function(x) x[["src"]]

#' @rdname cfg_utils
#' @export
tbl_name <- function(x) UseMethod("tbl_name", x)

#' @rdname cfg_utils
#' @export
tbl_name.col_cfg <- function(x) x[["table"]]

#' @rdname cfg_utils
#' @export
tbl_name.tbl_cfg <- function(x) x[["table"]]

