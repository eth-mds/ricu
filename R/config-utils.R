
#' @param x Object to coerce/query
#'
#' @rdname src_cfg
#' @keywords internal
is_src_cfg <- is_type("src_cfg")

#' @rdname src_cfg
#' @keywords internal
#' @export
as_src_cfg <- function(x) UseMethod("as_src_cfg", x)

#' @export
as_src_cfg.src_cfg <- function(x) x

#' @export
as_src_cfg.src_env <- function(x) {

  args <- list(name = src_name(x), id_cfg = as_id_cfg(x),
    col_cfg = vec_unchop(lapply(x, as_col_cfg), name_spec = "{inner}"),
    tbl_cfg = vec_unchop(lapply(x, as_tbl_cfg), name_spec = "{inner}")
  )

  do.call(new_src_cfg,
    c(args, src_extra_cfg(x), list(class_prefix = src_prefix(x)))
  )
}

#' @export
as_src_cfg.default <- function(x) as_src_cfg(as_src_env(x))

#' @export
print.src_cfg <- function(x, ...) {
  id <- as_id_cfg(x)
  cat_line(class_descr(x, names(id), id_cfg_op(id), sep = "", collapse = " "))
  vctrs::obj_print_data(as_tbl_cfg(x), ...)
  invisible(x)
}

#' @rdname src_cfg
#' @keywords internal
is_id_cfg <- is_type("id_cfg")

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

#' @export
vec_ptype_abbr.id_cfg <- function(x, ...) main_class(x)

#' @export
vec_ptype_full.id_cfg <- function(x, ...) {
  class_descr(x, names(x), id_cfg_op(x), sep = "", collapse = " ")
}

#' @export
format.id_cfg <- function(x, ...) quote_bt(field(x, "id"))

#' @export
names.id_cfg <- function(x) field(x, "name")

#' @export
`names<-.id_cfg` <- function(x, value) {
  if (has_length(value)) `field<-`(x, "name", value) else x
}

#' @export
vec_proxy_compare.id_cfg <- function(x, ...) field(x, "pos")

#' @rdname src_cfg
#' @keywords internal
is_col_cfg <- is_type("col_cfg")

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

#' @export
vec_ptype_abbr.col_cfg <- function(x, ...) main_class(x)

#' @export
vec_ptype_full.col_cfg <- function(x, ...) {
  class_descr(x, default_var_names(x), collapse = ", ")
}

#' @export
format.col_cfg <- function(x, ...) {

  lens <- lapply(unclass(x)[default_var_names(x)], lengths)

  enbraket(do_call(lens, paste, sep = ", "))
}

#' @export
names.col_cfg <- function(x) field(x, "table")

#' @export
`names<-.col_cfg` <- function(x, value) {
  if (has_length(value)) `field<-`(x, "table", value) else x
}

#' @rdname src_cfg
#' @keywords internal
is_tbl_cfg <- is_type("tbl_cfg")

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

#' @export
vec_ptype_abbr.tbl_cfg <- function(x, ...) main_class(x)

#' @export
vec_ptype_full.tbl_cfg <- function(x, ...) {
  class_descr(x, "rows", symbol$cross, "cols; partitions")
}

#' @export
format.tbl_cfg <- function(x, ...) {
  paste0("[", dim_desc(x), "; ", n_part(x), "]")
}

#' @export
names.tbl_cfg <- function(x) field(x, "table")

#' @export
`names<-.tbl_cfg` <- function(x, value) {
  if (has_length(value)) `field<-`(x, "table", value) else x
}

#' @rdname src_cfg
#' @keywords internal
#' @export
src_name <- function(x) UseMethod("src_name", x)

#' @export
src_name.src_cfg <- function(x) x[["name"]]

#' @export
src_name.id_cfg <- function(x) attr(x, "src")

#' @export
src_name.col_cfg <- function(x) attr(x, "src")

#' @export
src_name.tbl_cfg <- function(x) attr(x, "src")

#' @export
src_name.default <- function(x) stop_generic(x, .Generic)

#' @rdname src_cfg
#' @keywords internal
#' @export
tbl_name <- function(x) UseMethod("tbl_name", x)

#' @export
tbl_name.col_cfg <- function(x) field(x, "table")

#' @export
tbl_name.tbl_cfg <- function(x) field(x, "table")

#' @export
tbl_name.default <- function(x) stop_generic(x, .Generic)

#' @rdname src_cfg
#' @keywords internal
#' @export
src_extra_cfg <- function(x) UseMethod("src_extra_cfg", x)

#' @export
src_extra_cfg.src_cfg <- function(x) x[["extra"]]

#' @export
src_extra_cfg.src_env <- function(x) attr(x, "extra")

#' @export
src_extra_cfg.default <- function(x) stop_generic(x, .Generic)

#' @rdname src_cfg
#' @keywords internal
#' @export
src_prefix <- function(x) UseMethod("src_prefix", x)

#' @export
src_prefix.src_cfg <- function(x) x[["prefix"]]

#' @export
src_prefix.src_env <- function(x) attr(x, "prefix")

#' @export
src_prefix.default <- function(x) stop_generic(x, .Generic)

class_descr <- function(x, ...) {
  paste0(main_class(x), "<", src_name(x), "[", paste(...), "]>")
}

main_class <- function(x) {
  tail(strip_class(x, c("vctrs_rcrd", "vctrs_vctr")), n = 1L)
}

id_cfg_op <- function(x) {
  x <- sort(as_id_cfg(x))
  c(c(" =", " <")[abs(vec_compare(x[-length(x)], x[-1L])) + 1L], "")
}

n_row <- function(x) {
  field(as_tbl_cfg(x), "num_rows")
}

n_col <- function(x) {
  lengths(field(as_tbl_cfg(x), "cols"))
}

n_part <- function(x) {
  lengths(lst_xtr(field(as_tbl_cfg(x), "partitioning"), "breaks")) + 1L
}

fst_file_names <- function(x) {

  multi_part <- function(nme, len) {
    paste0(if (len > 1L) file.path(nme, seq_len(len)) else nme, ".fst")
  }

  x <- as_tbl_cfg(x)

  map(multi_part, tbl_name(x), n_part(x))
}

raw_file_names <- function(x) field(as_tbl_cfg(x), "files")

fst_file_name <- function(x) get_one(fst_file_names(x))

raw_file_name <- function(x) get_one(raw_file_names(x))

src_file_exist <- function(x, dir, type = c("fst", "raw")) {

  are_avail <- function(x, d) all(file.exists(file.path(d, x)))

  files <- switch(match.arg(type), fst = fst_file_names(x),
                                   raw = raw_file_names(x))

  lgl_ply(files, are_avail, dir)
}

#' @rdname src_cfg
#' @keywords internal
#'
src_url <- function(x) src_extra_cfg(as_src_cfg(x))[["url"]]

#' @rdname src_cfg
#' @keywords internal
#' @export
id_var_opts <- function(x) field(as_id_cfg(x), "id")

id_name_to_type <- function(x, name) {

  id_cfg  <- as_id_cfg(x)
  id_opts <- id_var_opts(id_cfg)

  assert_that(is_in(name, id_opts))

  names(id_cfg)[id_opts == name]
}

id_type_to_name <- function(x, type) id_var_opts(as_id_cfg(x)[type])

default_var_names <- function(x) setdiff(fields(as_col_cfg(x)), "table")

#' @rdname src_cfg
#' @keywords internal
#' @export
default_vars <- function(x, type) {
  assert_that(is.string(type))
  UseMethod("default_vars", x)
}

#' @export
default_vars.col_cfg <- function(x, type) {

  if (type %in% fields(x)) {
    res <- field(x, type)
  } else {
    res <- rep_along(list(NULL), x)
  }

  setNames(res, names(x))
}

val_var <- function(x) default_vars(x, "val_var")

unit_var <- function(x) default_vars(x, "unit_var")

#' @export
id_vars.col_cfg <- function(x) default_vars(x, "id_var")

#' @export
id_vars.id_cfg <- function(x) field(max(x), "id")

#' @export
index_var.col_cfg <- function(x) default_vars(x, "index_var")

#' @export
time_vars.col_cfg <- function(x) default_vars(x, "time_vars")

get_one <- function(x) {
  assert_that(length(x) == 1L)
  x[[1L]]
}

col_spec <- function(x) get_one(field(as_tbl_cfg(x), "spec"))

orig_cols <- function(x) names(col_spec(x)[["cols"]])

ricu_cols <- function(x) get_one(field(as_tbl_cfg(x), "cols"))

partition_fun <- function(x, orig_names = FALSE) {

  x <- as_tbl_cfg(x)

  assert_that(length(x) == 1L, n_part(x) > 1L)

  part <- field(x, "partitioning")[[1L]]

  col <- partition_col(x, orig_names)
  breaks <- force(part[["breaks"]])

  function(x) {
    findInterval(x[[col]], breaks) + 1L
  }
}

partition_col <- function(x, orig_names = FALSE) {

  x <- as_tbl_cfg(x)

  assert_that(length(x) == 1L, n_part(x) > 1L, is.flag(orig_names))

  part <- field(x, "partitioning")[[1L]]

  col <- part[["col"]]

  if (orig_names) {
    col <- orig_cols(x)[match(col, ricu_cols(x))]
  }

  assert_that(is.string(col))

  col
}

#' @export
n_tick.tbl_cfg <- function(x) {

  n_pt <- n_part(x)
  n_pt <- ifelse(n_pt > 1L, n_pt + lengths(raw_file_names(x)), 1L)

  sum(ifelse(is.na(n_row(x)), n_pt, n_row(x)))
}
