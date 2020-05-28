
#' Data item
#'
#' In order to specify the location of data items in data sets, `item` objects
#' are used.
#'
#' @param src The data source name
#' @param table Name of the table containing the data
#' @param column Column name
#' @param ids Vector of ids used to subset table rows. If `NULL`, all rows are
#' considered corresponding to the data item
#' @param regex Logical flag indicating whether the vector of id should be
#' interpreted as regular expressions
#' @param callback Name of a function to be called on the returned data used
#' for data cleanup operations
#' @param load_fun Custom function used for overriding default loading of data
#' @param ... Further name/string pairs specifying additional columns needed to
#' define the data item
#'
#' @rdname data_items
#'
#' @export
#'
new_item <- function(src, itm, class = "sel_itm") {

  if (!all_fun(itm, is.list)) {
    itm <- list(itm)
  }

  assert_that(is.character(src), all_fun(itm, is.list), is.character(class))

  if (length(itm) == 0L || length(itm[[1L]]) == 0L) {
    return(new_rcrd(list(src = character(0L), itm = list()), class = "item"))
  }

  itm <- Map(structure, itm, class = lapply(class, c, "itm"))
  itm <- Map(validate_itm, itm, src)

  res <- vec_recycle_common(src = src, itm = itm)

  new_rcrd(res, class = "item")
}

#' @export
format.item <- function(x, ...) {
  paste0("<", chr_xtr(lapply(field(x, "itm"), class), 1L), ">")
}

#' @export
names.item <- function(x) field(x, "src")

#' @export
`names<-.item` <- function(x, value) {

  if (length(value)) {
    assert_that(identical(names(x), value))
  }

  x
}

#' @export
as.list.item <- function(x, ...) {
  warn_dots(...)
  vec_chop(x)
}

#' @param x A data item object
#'
#' @rdname data_items
#'
#' @export
#'
is_item <- function(x) inherits(x, "item")

#' @rdname data_items
#' @export
validate_itm <- function(x, src) {
  assert_that(is.string(src))
  UseMethod("validate_itm", x)
}

#' @rdname data_items
#' @export
validate_itm.sel_itm <- function(x, src) {

  tbl <- x[["table"]]
  xtr <- c(x["sub_col"], get_extra_cols(x))

  assert_that(
    is.string(tbl), both(x[["ids"]], has_length, chr_or_int),
    null_or(x[["callback"]], is_fun_name),
    all_fun(xtr, is_colname, as_src_tbl(tbl, src))
  )

  x
}

#' @rdname data_items
#' @export
validate_itm.col_itm <- function(x, src) {

  tbl <- x[["table"]]

  assert_that(is.string(tbl), null_or(x[["callback"]], is_fun_name),
              all_fun(get_extra_cols(x), is_colname, as_src_tbl(tbl, src)))

  x
}

#' @rdname data_items
#' @export
validate_itm.rgx_itm <- function(x, src) {

  tbl <- x[["table"]]
  xtr <- c(x["sub_col"], get_extra_cols(x))

  assert_that(is.string(tbl), is.string(x[["regex"]]),
              null_or(x[["callback"]], is_fun_name),
              all_fun(xtr, is_colname, as_src_tbl(tbl, src)))

  x
}

#' @rdname data_items
#' @export
validate_itm.los_itm <- function(x, src) {

  assert_that(is.string(x[["win_type"]]), length(x) == 1L)

  x
}

#' @rdname data_items
#' @export
validate_itm.itm <- function(x, src) x

#' @export
get_extra_cols <- function(x) UseMethod("get_extra_cols", x)

#' @export
get_extra_cols.sel_itm <- function(x) {
  x[setdiff(names(x), c("ids", "table", "sub_col", "callback"))]
}

#' @export
get_extra_cols.col_itm <- function(x) {
  x[setdiff(names(x), c("table", "callback"))]
}

#' @export
get_extra_cols.rgx_itm <- function(x) {
  x[setdiff(names(x), c("regex", "table", "sub_col", "callback"))]
}

#' @export
prepare_query <- function(x) UseMethod("prepare_query", x)

#' @export
prepare_query.sel_itm <- function(x) {

  ids <- x[["ids"]]
  lst <- list(col = as.name(x[["sub_col"]]), id = ids)

  if (length(ids) == 1L) {
    substitute(is_fun(col, id), c(lst, list(is_fun = is_val)))
  } else if (is.character(ids)) {
    substitute(col %chin% id, lst)
  } else {
    substitute(col %in% id, lst)
  }
}

#' @export
prepare_query.rgx_itm <- function(x) {
  substitute(grepl(rgx, col, ignore.case = TRUE),
    list(col = as.name(x[["sub_col"]]), rgx = x[["regex"]])
  )
}

#' @export
src_name.item <- function(x) names(x)

#' @rdname data_items
#' @export
as_item <- function(x, ...) UseMethod("as_item", x)

#' @rdname data_items
#' @export
as_item.item <- function(x, ...) x

#' @export
as_item.concept <- function(x) do.call(vec_c, unname(field(x, "items")))

#' Data concept
#'
#' Clinical concepts are represented by `concept` objects.
#'
#' @param name The name of the concept
#' @param items One or more `item` objects
#' @param unit A string, specifying the measurement unit of the concept (can
#' be `NULL`)
#' @param min,max Scalar valued; defines a range of plausible values
#'
#' @rdname data_concepts
#'
#' @export
new_concept <- function(name, items, unit = NULL, min = NULL,
                        max = NULL, levels = NULL, target_class = "ts_tbl") {

  wrap_null(unit, min, max, levels)

  if (is_item(items)) {
    items <- list(items)
  }

  items <- lapply(items, as_item)

  assert_that(is.character(name), all_null_or(unit, is.string),
              all_null_or(min, is.number), all_null_or(max, is.number),
              all_null_or(levels, both, is.atomic, has_length),
              is.character(target_class))

  list2env(
    res <- vec_recycle_common(name = name, items = items, unit = unit,
                              min = min, max = max, levels = levels,
                              class = target_class)
  )

  check <- lgl_ply(levels, is.null) | (lgl_ply(min, is.null) &
                                       lgl_ply(max, is.null))

  assert_that(all(check), msg = paste0("specifying both `levels` and ",
    "`min`/`max` is not possible for concepts ", concat(name[!check])))

  new_rcrd(res, class = "concept")
}

#' @param x A concept object
#'
#' @rdname data_concepts
#'
#' @export
#'
is_concept <- function(x) inherits(x, "concept")

#' @export
vec_ptype_full.concept <- function(x, ...) {
  srcs <- sort(
    unique(unlist(lapply(field(x, "items"), names), use.names = FALSE))
  )
  paste0(class(x)[1L], "{", concat(srcs), "}")
}

#' @export
vec_ptype_abbr.concept <- function(x, ...) class(x)[1L]

#' @export
format.concept <- function(x, ...) {

  cnt  <- function(i, nm) int_ply(nm, function(x) sum(x == i))

  nms  <- lapply(field(x, "items"), names)
  srcs <- sort(unique(unlist(nms, use.names = FALSE)))
  cnts <- int_ply(srcs, cnt, nms, length = length(nms))

  apply(
    matrix(cnts, nrow = length(nms)), 1L,
    function(row) paste0("[", paste0(row, collapse = ", "), "]")
  )
}

#' @export
names.concept <- function(x) field(x, "name")

#' @export
units.concept <- function(x) field(x, "unit")

#' @export
as.list.concept <- function(x, ...) {
  warn_dots(...)
  vec_chop(x)
}

#' @export
src_name.concept <- function(x) lapply(field(x, "items"), src_name)

#' @param name Name of the dictionary to be read
#' @param file File name of the dictionary
#'
#' @rdname data_concepts
#'
#' @export
read_dictionary <- function(src = NULL, name = "concept-dict", file = NULL,
                            ...) {

  get_src <- function(x) {
    if (length(y <- x[names(x) == src])) y else setNames(list(list()), src)
  }

  get_class <- function(x, default) {
    if (is.null(y <- x[["class"]])) default else y
  }

  get_rest <- function(x) x[setdiff(names(x), "class")]

  do_itm <- function(itms, nme) {
    new_item(nme, lapply(itms, get_rest), chr_ply(itms, get_class, "sel_itm"))
  }

  do_src <- function(x) {
    do.call(vec_c, Map(do_itm, x, names(x), USE.NAMES = FALSE))
  }

  if (is.null(file)) {

    x <- get_config(name, ...)

  } else {

    assert_that(missing(name), file.exists(file))

    x <- read_json(file, ...)
  }

  if (not_null(src)) {

    assert_that(is.string(src))

    x <- Map(`[[<-`, x, "sources",
               lapply(lst_xtr(x, "sources"), get_src))
  }

  new_concept(name = names(x), items = lapply(lst_xtr(x, "sources"), do_src),
              unit = lst_xtr(x, "unit"), min = lst_xtr(x, "min"),
              max = lst_xtr(x, "max"), levels = lst_xtr(x, "levels"),
              target_class = chr_ply(x, get_class, "ts_tbl"))
}
