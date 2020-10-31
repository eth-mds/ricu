
#' @param x Object to coerce/query
#'
#' @rdname src_cfg
#' @keywords internal
is_src_cfg <- is_type("src_cfg")

#' @rdname src_cfg
#' @keywords internal
is_id_cfg <- is_type("id_cfg")

#' @rdname src_cfg
#' @keywords internal
is_col_cfg <- is_type("col_cfg")

#' @rdname src_cfg
#' @keywords internal
is_tbl_cfg <- is_type("tbl_cfg")

#' @rdname src_cfg
#' @keywords internal
#' @export
as_src_cfg <- function(x) UseMethod("as_src_cfg", x)

#' @export
as_src_cfg.src_cfg <- function(x) x

#' @export
as_src_cfg.src_env <- function(x) {
  new_src_cfg(src_name(x), as_id_cfg(x), eapply(x, as_col_cfg),
              eapply(x, as_tbl_cfg), sub("_env", "", head(class(x), n = -1L)))
}

#' @export
as_src_cfg.default <- function(x) as_src_cfg(as_src_env(x))

#' @rdname src_cfg
#' @keywords internal
#' @export
as_id_cfg <- function(x) UseMethod("as_id_cfg", x)

#' @export
as_id_cfg.id_cfg <- function(x) x

#' @export
as_id_cfg.src_cfg <- function(x) x[["id_cfg"]]

#' @export
as_id_cfg.src_env <- function(x) attr(x, "id_cfg")

#' @export
as_id_cfg.default <- function(x) as_id_cfg(as_src_env(x))

#' @rdname src_cfg
#' @keywords internal
#' @export
as_col_cfg <- function(x) UseMethod("as_col_cfg", x)

#' @export
as_col_cfg.col_cfg <- function(x) x

#' @export
as_col_cfg.src_cfg <- function(x) x[["col_cfg"]]

#' @export
as_col_cfg.src_tbl <- function(x) attr(x, "col_cfg")

#' @export
as_col_cfg.default <- function(x) as_col_cfg(as_src_tbl(x))

#' @rdname src_cfg
#' @keywords internal
#' @export
as_tbl_cfg <- function(x) UseMethod("as_tbl_cfg", x)

#' @export
as_tbl_cfg.tbl_cfg <- function(x) x

#' @export
as_tbl_cfg.src_cfg <- function(x) x[["tbl_cfg"]]

#' @export
as_tbl_cfg.src_tbl <- function(x) attr(x, "tbl_cfg")

#' @export
as_tbl_cfg.default <- function(x) as_tbl_cfg(as_src_tbl(x))

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

#' @rdname src_cfg
#' @keywords internal
#' @export
id_var_opts <- function(x) {
  field(as_id_cfg(x), "id")
}

id_name_to_type <- function(x, name) {

  id_cfg  <- as_id_cfg(x)
  id_opts <- id_var_opts(id_cfg)

  assert_that(is_in(name, id_opts))

  names(id_cfg)[id_opts == name]
}

id_type_to_name <- function(x, name) id_var_opts(as_id_cfg(x)[name])

val_var <- function(x) as_col_cfg(x)[["val_var"]]

unit_var <- function(x) as_col_cfg(x)[["unit_var"]]

#' @export
dim.tbl_cfg <- function(x) {
  c(x[["nrow"]], length(x[["cols"]]))
}

n_partitions <- function(x) {
  assert_that(is_tbl_cfg(x))
  length(x[["partitioning"]][["breaks"]]) + 1L
}

raw_file_names <- function(x) {
  assert_that(is_tbl_cfg(x))
  x[["files"]]
}

fst_file_names <- function(x) {

  assert_that(is_tbl_cfg(x))

  n_part  <- n_partitions(x)
  tbl_nme <- tbl_name(x)

  if (n_part > 1L) {
    tbl_nme <- file.path(tbl_nme, seq_len(n_part))
  }

  paste0(tbl_nme, ".fst")
}

src_file_exist <- function(x, dir, type = c("fst", "raw")) {

  are_avail <- function(x) all(file.exists(file.path(dir, x)))

  fun <- switch(match.arg(type), fst = fst_file_names, raw = raw_file_names)

  if (is_tbl_cfg(x)) {
    are_avail(fun(x))
  } else {
    lgl_ply(lapply(x, fun), are_avail)
  }
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
    warn_ricu("Table {quote_bt(tbl_name(x))} has {big_mark(n_row)} instead of
               {big_mark(expec)} {qty(expec)} row{?s}",
              class = "src_tbl_row_mismatch")
  }

  invisible(n_row)
}

col_names <- function(x) {
  setNames(x[["cols"]], names(col_spec(x)[["cols"]]))
}

partition_fun <- function(x, orig_names = FALSE) {

  assert_that(is_tbl_cfg(x), n_partitions(x) > 1L)

  part <- x[["partitioning"]]

  col <- partition_col(x, orig_names)
  breaks <- force(part[["breaks"]])

  function(x) {
    findInterval(x[[col]], breaks) + 1L
  }
}

partition_col <- function(x, orig_names = FALSE) {

  assert_that(is_tbl_cfg(x), is.flag(orig_names), n_partitions(x) > 1L)

  part <- x[["partitioning"]]

  col <- part[["col"]]

  if (orig_names) {
    nms <- col_names(x)
    col <- names(nms[nms == col])
  }

  assert_that(is.string(col))

  col
}

#' @export
id_vars.col_cfg <- function(x) x[["id_var"]]

#' @export
id_vars.id_cfg <- function(x) field(max(x), "id")

#' @export
index_var.col_cfg <- function(x) x[["index_var"]]

#' @export
time_vars.col_cfg <- function(x) x[["time_vars"]]

#' @rdname src_cfg
#' @keywords internal
#' @export
src_name <- function(x) UseMethod("src_name", x)

#' @export
src_name.src_cfg <- function(x) x[["name"]]

#' @export
src_name.id_cfg <- function(x) attr(x, "src")

#' @export
src_name.col_cfg <- function(x) x[["src"]]

#' @export
src_name.tbl_cfg <- function(x) x[["src"]]

#' @export
src_name.default <- function(x) stop_generic(x, .Generic)

#' @rdname src_cfg
#' @keywords internal
#' @export
tbl_name <- function(x) UseMethod("tbl_name", x)

#' @export
tbl_name.col_cfg <- function(x) x[["table"]]

#' @export
tbl_name.tbl_cfg <- function(x) x[["table"]]

#' @export
tbl_name.default <- function(x) stop_generic(x, .Generic)
