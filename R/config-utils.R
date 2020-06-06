
#' Data source configuration
#'
#' In order to use a data source with `ricu`, a configuration object has to be
#' created.
#'
#' @param name Name of the data source
#' @param id_cfg List passed to [new_id_cfg()]
#' @param tables List passed to [as_col_cfg()] and potentially to
#' [as_tbl_spec()]
#' @param url String valued url used for data downloading (see
#' [download_src()])
#'
#' @rdname src_cfg
#' @keywords internal
#'
#' @import vctrs
#'
new_src_cfg <- function(name, id_cfg, tables, url = NULL) {

  assert_that(is.string(name), null_or(url, is.string),
              all_fun(id_cfg, has_name, c("id", "position")),
              all_fun(tables, has_name, c("name", "defaults")))

  if (all_fun(tables, has_name, c("files", "cols"))) {
    tbl_specs <- as_tbl_spec(tables, name)
  } else {
    tbl_specs <- NULL
  }

  structure(
    list(name = name, id_cfg = as_id_cfg(id_cfg, name),
         col_cfg = as_col_cfg(tables, name), url = url, tbl_specs = tbl_specs),
    class = c(paste0(name, "_cfg"), "src_cfg")
  )
}

#' @param x Object to test/coerce
#'
#' @rdname src_cfg
#' @keywords internal
#'
is_src_cfg <- function(x) inherits(x, "src_cfg")

#' @rdname src_cfg
#' @keywords internal
#' @export
as_src_cfg <- function(x, ...) UseMethod("as_src_cfg", x)

#' @rdname src_cfg
#' @keywords internal
#' @export
as_src_cfg.src_cfg <- function(x, ...) warn_dot_ident(x, ...)

#' @rdname src_cfg
#' @keywords internal
#' @export
as_src_cfg.list <- function(x, ...) {
  assert_that(...length() == 0L, has_name(x, c("id_cfg", "tables")))
  do.call(new_src_cfg, x[c("name", "id_cfg", "tables", "url")])
}

#' @export
src_name.src_cfg <- function(x) x[["name"]]

#' Load configuration for a data source
#'
#' For a data source to become available to ricu, a JSON base configuration
#' file is required which is parsed into a `src_cfg` object (see
#' [new_src_cfg()]).
#'
#' @param src (Optional) name(s) of data sources used for subsetting
#' @param name String valued name of a config file which will be looked up in
#' the default config directors
#' @param file Full file name to load
#'
#' @export
#'
read_src_cfg <- function(src = NULL, name = "data-sources", file = NULL) {

  if (is.null(file)) {

    cfg <- get_config(name)

  } else {

    assert_that(missing(name), file.exists(file))

    cfg <- read_json(file)
  }

  cfg_nme <- chr_xtr(cfg, "name")

  if (is.null(src)) {
    src <- cfg_nme
  }

  assert_that(is.character(src), all(src %in% cfg_nme))

  cfg <- cfg[cfg_nme %in% src]

  lapply(cfg, as_src_cfg)
}

#' @rdname src_cfg
#' @keywords internal
#'
src_url <- function(x) {
  assert_that(is_src_cfg(x))
  res <- x[["url"]]
  assert_that(is.string(res), not_na(res))
  res
}

#' ID columns
#'
#' A set of id columns corresponding to `patient`, `hadm` (hospital stay) and
#' `icustay` (ICU stay) can be represented by an `id_cfg` object.
#'
#' @param cfg List containing string valued column names
#'
#' @rdname id_cfg
#' @keywords internal
#'
new_id_cfg <- function(name, id, pos, start, end, table, src) {

  assert_that(is.character(name), is_unique(name), is.character(id),
              is.integer(pos), is.character(start), is.character(end),
              is.character(table), is.string(src))

  check <- ifelse(is.na(start) & is.na(end), is.na(table), !is.na(table))

  assert_that(all(check), msg = paste0("Invalid ID config for IDs ",
    concat(quote_bt(id[!check])), ": table name needed due to start/end args")
  )

  new_rcrd(list(name = name, id = id, pos = pos, start = start, end = end,
                table = table), class = c(paste0(src, "_ids"), "id_cfg"))
}

#' @param x Object to test/coerce
#'
#' @rdname id_cfg
#' @keywords internal
#'
is_id_cfg <- function(x) inherits(x, "id_cfg")

#' @export
vec_ptype_full.id_cfg <- function(x, ...) {
  x <- sort(x)
  op <- setNames(c(" <", " =", " >"), c("-1", "0", "1"))[
    as.character(vec_compare(x[-length(x)], x[-1L]))
  ]
  paste0(class(x)[2L], "{", src_name(x), ": ",
         paste0(names(x), c(op, ""), collapse = " "), "}")
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
as_id_cfg <- function(x, ...) UseMethod("as_id_cfg", x)

#' @rdname id_cfg
#' @keywords internal
#' @export
as_id_cfg.list <- function(x, src, ...) {

  warn_dots(...)

  new_id_cfg(
    name = names(x), id = chr_xtr(x, "id"), pos = int_xtr(x, "position"),
    start = chr_xtr_null(x, "start"), end = chr_xtr_null(x, "end"),
    table = chr_xtr_null(x, "table"), src = src
  )
}

#' @rdname id_cfg
#' @keywords internal
#' @export
as_id_cfg.id_cfg <- function(x, ...) warn_dot_ident(x, ...)

#' @rdname id_cfg
#' @keywords internal
#' @export
#'
as_id_cfg.src_cfg <- function(x, ...) {
  warn_dots(...)
  x[["id_cfg"]]
}

#' @rdname id_cfg
#' @keywords internal
#' @export
#'
as_id_cfg.src_env <- function(x, ...) {
  warn_dots(...)
  attr(x, "id_cfg")
}

#' @rdname id_cfg
#' @keywords internal
#' @export
#'
as_id_cfg.default <- function(x, ...) as_id_cfg(as_src_env(x), ...)

#' @export
src_name.id_cfg <- function(x) sub("_ids$", "", class(x)[1L])

get_id_col <- function(x, id_type = NULL) {

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

#' Default columns
#'
#' A table can have some of its columns marked as default columns for the
#' following concepts:
#'
#' * `id_col`: column will be used for as id for `icu_tbl` objects
#' * `index_col`: column represents a timestamp variable and will be use as
#'   such for `ts_tbl` objects
#' * `val_col`: column contains the measured variable of interest
#' * `unit_col`: column specifies the unit of measurement in the corresponding
#'   `val_col`
#'
#' @param table Table name (string)
#' @param defaults Named list containing column names
#' @param times Character vector enumerating columns that contain time
#' information
#'
#' @rdname col_cfg
#' @keywords internal
#'
new_col_cfg <- function(table, id, index, val, unit, time, src) {

  assert_that(is.character(table), is_unique(table), is.character(id),
              is.character(index), is.character(val), is.character(unit),
              all_null_or(time, is.character), is.string(src))

  time[lgl_ply(time, is.null)] <- list(character(0L))

  check <- is.na(index) | lgl_ply(Map(`%in%`, index, time), isTRUE)

  assert_that(all(check), msg = paste0("Index column(s) ",
    paste0("`", index[!check], "`", collapse = ", "), " are not listed in ",
    "the respective time column entries")
  )

  new_rcrd(list(table = table, id = id, index = index, val = val, unit = unit,
                time = time), class = c(paste0(src, "_cols"), "col_cfg"))
}

#' @param x Object to test/coerce
#'
#' @rdname col_cfg
#' @keywords internal
#'
is_col_cfg <- function(x) inherits(x, "col_cfg")

#' @export
vec_ptype_full.col_cfg <- function(x, ...) {
  paste0(class(x)[2L], "{", src_name(x), ": ",
         paste0(c("id", "index", "val", "unit", "time"), collapse = ", "), "}")
}

#' @export
vec_ptype_abbr.col_cfg <- function(x, ...) class(x)[2L]

#' @export
format.col_cfg <- function(x, ...) {

  cnt <- function(y) int_ply(field(x, y), length)

  apply(
    cbind(cnt("id"), cnt("index"), cnt("val"), cnt("unit"), cnt("time")), 1L,
    function(row) paste0("[", paste0(row, collapse = ", "), "]")
  )
}

#' @export
names.col_cfg <- function(x) field(x, "table")

#' @export
as.list.col_cfg <- function(x, ...) {
  warn_dots(...)
  vec_chop(x)
}

#' @export
src_name.col_cfg <- function(x) sub("_cols$", "", class(x)[1L])

#' @rdname col_cfg
#' @keywords internal
#' @export
#'
as_col_cfg <- function(x, ...) UseMethod("as_col_cfg", x)

#' @rdname col_cfg
#' @keywords internal
#' @export
#'
as_col_cfg.col_cfg <- function(x, ...) warn_dot_ident(x, ...)

#' @rdname col_cfg
#' @keywords internal
#' @export
#'
as_col_cfg.list <- function(x, src, ...) {

  assert_that(all_fun(x, has_name, "defaults"), ...length() == 0L)

  def <- lapply(x, `[[`, "defaults")

  new_col_cfg(
    table = chr_xtr(x, "name"), id = chr_xtr_null(def, "id_col"),
    index = chr_xtr_null(def, "index_col"), val = chr_xtr_null(def, "val_col"),
    unit = chr_xtr_null(def, "unit_col"), time = lst_xtr(x, "time_cols"),
    src = src
  )
}

#' @rdname col_cfg
#' @keywords internal
#' @export
#'
as_col_cfg.src_cfg <- function(x, ...) {
  warn_dots(...)
  x[["col_cfg"]]
}

#' @rdname col_cfg
#' @keywords internal
#' @export
#'
as_col_cfg.src_tbl <- function(x, ...) {
  warn_dots(...)
  attr(x, "col_cfg")
}

get_default_cols <- function(x, fields = c("id", "index", "val", "unit",
                                           "time")) {

  do_one <- function(i) if (is.na(res <- field(x, i))) NULL else res

  x <- as_col_cfg(x)

  assert_that(length(x) == 1L, is.character(fields), length(fields) > 0L,
              all(fields %in% fields(x)))

  setNames(lapply(fields, do_one), paste0(fields, "_col"))
}

#' Table specification
#'
#' Used when importing a data source, a table specification defines column
#' data types, source file names, a mapping of source to imported column
#' names and optionally a number of expected rows and a partitioning scheme.
#'
#' @param table String valued table name
#' @param files A character vector of file names
#' @param cols List containing a list per column each holding string valued
#' entries `name` (column name as used by `ricu`), `col` (column name as used
#' in the raw data) and `spec` (name of [readr::cols()] column specification).
#' Further entries will be passed as argument to the respective `readr` column
#' specification
#' @param nrow A count indicating the expected number of rows
#' @param partitioning A table partitioning is defined by a column name and a
#' vector of numeric values that are passed as `vec` argument to
#' `base::findInterval()`
#'
#' @rdname tbl_spec
#' @keywords internal
#'
new_tbl_spec <- function(table, files, cols, nrow, partitioning, src) {

  col_spc <- function(name, col, spec, ...) {
    do.call(call, c(list(spec), list(...)))
  }

  tbl_spc <- function(...) substitute(cols(...))

  assert_that(is.character(table), all_fun(files, is.character),
              is.integer(nrow), all_fun(cols, is.list),
              all_fun(cols, all_fun, has_name, c("name", "col", "spec")),
              all_fun(partitioning, null_or, is.list),
              all_fun(partitioning, null_or, has_name, c("col", "breaks")))

  spec <- lapply(cols, lapply, do_call, col_spc)
  spec <- Map(`names<-`, spec, lapply(cols, chr_xtr, "col"))

  spec <- lapply(spec, do_call, tbl_spc)
  spec <- lapply(spec, eval, envir = asNamespace("readr"))

  cols <- lapply(cols, chr_xtr, "name")

  new_rcrd(list(table = table, files = files, cols = cols, spec = spec,
                 nrow = nrow, partitioning = partitioning),
            class = c(paste0(src, "_spec"), "tbl_spec"))
}

#' @param x Object to test/coerce
#'
#' @rdname tbl_spec
#' @keywords internal
#'
is_tbl_spec <- function(x) inherits(x, "tbl_spec")

#' @export
vec_ptype_full.tbl_spec <- function(x, ...) {
  paste0(class(x)[2L], "{", src_name(x), ": ",
         paste("rows", times(), "cols"), "}")
}

#' @export
vec_ptype_abbr.tbl_spec <- function(x, ...) class(x)[2L]

#' @export
format.tbl_spec <- function(x, ...) {

  n_col <- big_mark(int_ply(field(x, "cols"), length))
  n_row <- big_mark(n_rows(x))

  paste0("[", n_row, " ", times(), " ", n_col, "]")
}

#' @export
names.tbl_spec <- function(x) field(x, "table")

#' @export
as.list.tbl_spec <- function(x, ...) {
  warn_dots(...)
  vec_chop(x)
}

#' @export
src_name.tbl_spec <- function(x) sub("_spec$", "", class(x)[1L])

#' @rdname tbl_spec
#' @keywords internal
#' @export
#'
as_tbl_spec <- function(x, ...) UseMethod("as_tbl_spec", x)

#' @rdname tbl_spec
#' @keywords internal
#' @export
#'
as_tbl_spec.tbl_spec <- function(x, ...) warn_dot_ident(x, ...)

#' @rdname tbl_spec
#' @keywords internal
#' @export
#'
as_tbl_spec.list <- function(x, src, ...) {

  assert_that(all_fun(x, has_name, c("files", "cols")), ...length() == 0L)

  new_tbl_spec(
    table = chr_xtr(x, "name"), files = lst_xtr(x, "files"),
    cols = lst_xtr(x, "cols"), nrow = int_xtr(x, "num_rows"),
    partitioning = lst_xtr(x, "partitioning"), src = src
  )
}

#' @rdname tbl_spec
#' @keywords internal
#' @export
#'
as_tbl_spec.src_cfg <- function(x, ...) {
  warn_dots(...)
  field(x, "tbl_specs")
}

n_rows <- function(x) {
  assert_that(is_tbl_spec(x))
  field(x, "nrow")
}

file_names <- function(x) {
  assert_that(is_tbl_spec(x))
  field(x, "files")
}

file_name <- function(x) {
  assert_that(length(x) == 1L)
  file_names(x)[[1L]]
}

n_partitions <- function(x) {
  assert_that(is_tbl_spec(x))
  abs(lengths(lapply(field(x, "partitioning"), `[[`, "breaks")) - 1L)
}

fst_names <- function(x) {

  to_filenames <- function(tbl_name, n_part) {

    if (n_part > 1L) {
      tbl_name <- file.path(tbl_name, seq_len(n_part))
    }

    paste0(tbl_name, ".fst")
  }

  Map(to_filenames, names(x), n_partitions(x))
}

fst_name <- function(x) {
  assert_that(length(x) == 1L)
  fst_names(x)[[1L]]
}

col_spec <- function(x) {
  assert_that(is_tbl_spec(x), length(x) == 1L)
  field(x, "spec")[[1L]]
}

check_n_row <- function(x, n_row) {

  assert_that(is_tbl_spec(x), length(x) == 1L, is.count(n_row))

  expec <- n_rows(x)

  assert_that(isTRUE(all.equal(expec, n_row)), msg = paste0("Table ",
    quote_bt(names(x)), " has ", big_mark(n_row), " instead of ",
    big_mark(expec), " rows")
  )

  TRUE
}

col_name <- function(x) {
  setNames(field(x, "cols")[[1L]], names(col_spec(x)[["cols"]]))
}

partition_fun <- function(x, orig_names = FALSE) {

  assert_that(is_tbl_spec(x), length(x) == 1L, is.flag(orig_names),
              n_partitions(x) > 1L)

  part <- field(x, "partitioning")[[1L]]

  col <- part[["col"]]
  breaks <- force(part[["breaks"]])

  if (orig_names) {
    nms <- col_name(x)
    col <- names(nms[nms == col])
  }

  assert_that(is.string(col))

  function(x) {
    findInterval(x[[col]], breaks, all.inside = TRUE)
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
#' @rdname default_col
#' @export
#'
default_col <- function(x, ...) UseMethod("default_col", x)

#' @param type The column type, e.g. `id`, `index`, `unit`, `value`
#' @rdname default_col
#' @export
default_col.col_cfg <- function(x, type = "id", ...) {
  warn_dots(...)
  field(x, type)
}

#' @rdname default_col
#' @export
default_col.id_cfg <- function(x, ...) {
  warn_dots(...)
  field(max(x), "id")
}

#' @rdname default_col
#' @export
default_col.src_tbl <- function(x, type = "id", ...) {

  warn_dots(...)

  assert_that(is.string(type))

  res <- default_col(as_col_cfg(x), type)

  if (identical(type, "id") && anyNA(res)) {
    res[is.na(res)] <- default_col(as_id_cfg(x))
  }

  res
}
