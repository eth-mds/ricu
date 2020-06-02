
#' Data item
#'
#' In order to specify the location of data items in data sets, `item` objects
#' are used.
#'
#' @param src The data source name
#' @param ... Further specification of the `itm` object (passed to
#' [init_itm()])
#' @param target The target object yielded by loading
#' @param class Sub class for customizing `itm` behavior
#'
#' @rdname data_items
#'
#' @export
#'
new_itm <- function(src, ..., target = c("ts_tbl", "id_tbl"),
                    class = "sel_itm") {

  assert_that(is.string(src), is.string(class))

  target <- match.arg(target)

  init_itm(
    structure(list(src = src, targ = target), class = c(class, "itm")), ...
  )
}

#' @param x Object to query/dispatch on
#'
#' @rdname data_items
#' @export
is_itm <- function(x) inherits(x, "itm")

#' @export
src_name.itm <- function(x) x[["src"]]

#' @export
as_src_tbl.sel_itm <- function(x, ...) {
  as_src_tbl(x[["table"]], src_name(x), ...)
}

#' @export
as_src_tbl.col_itm <- function(x, ...) {
  as_src_tbl(x[["table"]], src_name(x), ...)
}

#' @export
as_src_tbl.rgx_itm <- function(x, ...) {
  as_src_tbl(x[["table"]], src_name(x), ...)
}

itm_col_helper <- function(x, col) {
  res <- if (is.null(col)) default_col(as_src_tbl(x), "val") else col
  if (is.string(res)) c(val_col = res) else {
    assert_that(has_name(res, "val_col"))
    res
  }
}

need_idx <- function(x) identical(x[["targ"]], "ts_tbl")

idx_col_helper <- function(x, col) {
  if (need_idx(x))
    if (is.null(col)) default_col(as_src_tbl(x), "index") else col
  else NULL
}

cbk_col_helper <- function(...) {
  if (...length()) as.character(list(...)) else NULL
}

#' @rdname data_items
#' @export
init_itm <- function(x, ...) UseMethod("init_itm", x)

#' @param table Name of the table containing the data
#' @param sub_col Column name used for subsetting
#' @param ids Vector of ids used to subset table rows. If `NULL`, all rows are
#' considered corresponding to the data item
#' @param itm_cols Columns returned as [data_cols()]
#' @param index_col Column used as index
#' @param callback Name of a function to be called on the returned data used
#' for data cleanup operations
#'
#' @rdname data_items
#' @export
init_itm.sel_itm <- function(x, table, sub_col, ids, itm_cols = NULL,
                             index_col = NULL,
                             callback = "identity_callback", ...) {

  x[["table"]] <- table

  itm_cols  <- itm_col_helper(x, itm_cols)
  index_col <- idx_col_helper(x, index_col)
  cb_cols   <- cbk_col_helper(...)

  tbl <- as_src_tbl(x)

  assert_that(
    is.string(table), is_fun_name(callback), both(ids, has_length, chr_or_int),
    all_fun(c(list(sub_col, itm_cols), index_col, cb_cols), is_colname, tbl)
  )

  todo <- c("ids", "sub_col", "itm_cols", "index_col", "cb_cols", "callback")
  x[todo] <- mget(todo)

  x
}

#' @rdname data_items
#' @export
init_itm.col_itm <- function(x, table, itm_cols = NULL, index_col = NULL,
                             callback = "identity_callback", ...) {

  x[["table"]] <- table

  itm_cols  <- itm_col_helper(x, itm_cols)
  index_col <- idx_col_helper(x, index_col)
  cb_cols   <- cbk_col_helper(...)

  assert_that(
    is.string(table), is_fun_name(callback),
    all_fun(c(list(itm_cols), index_col, cb_cols), is_colname, as_src_tbl(x))
  )

  todo <- c("itm_cols", "index_col", "cb_cols", "callback")
  x[todo] <- mget(todo)

  x
}

#' @param regex String-valued regular expression which will be evaluated by
#' [base::grepl()] with `ignore.case = TRUE`
#'
#' @rdname data_items
#' @export
init_itm.rgx_itm <- function(x, table, sub_col, regex, itm_cols = NULL,
                             index_col = NULL,
                             callback = "identity_callback", ...) {

  x[["table"]] <- table

  itm_cols  <- itm_col_helper(x, itm_cols)
  index_col <- idx_col_helper(x, index_col)
  cb_cols   <- cbk_col_helper(...)

  tbl <- as_src_tbl(x)

  assert_that(
    all_fun(list(table, regex), is.string), is_fun_name(callback),
    all_fun(c(list(sub_col, itm_cols), index_col, cb_cols), is_colname, tbl)
  )

  todo <- c("regex", "sub_col", "itm_cols", "index_col", "cb_cols", "callback")
  x[todo] <- mget(todo)

  x
}

#' @param win_type Passed to [stay_windows()]
#'
#' @rdname data_items
#' @export
init_itm.los_itm <- function(x, win_type, ...) {

  warn_dots(...)

  assert_that(is.string(win_type))

  x[["win_type"]] <- win_type

  x
}

#' @rdname data_items
#' @export
init_itm.itm <- function(x, ...) {

  dots <- list(...)

  assert_that(is_disjoint(names(x), names(dots)))

  x[names(dots)] <- dots

  x
}

#' Internal utilities for `item`/`concept` objects
#'
#' @param x Object defining the row-subsetting
#'
#' @rdname item_utils
#' @keywords internal
#' @export
prepare_query <- function(x) UseMethod("prepare_query", x)

#' @keywords internal
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

#' @keywords internal
#' @export
prepare_query.rgx_itm <- function(x) {
  substitute(grepl(rgx, col, ignore.case = TRUE),
    list(col = as.name(x[["sub_col"]]), rgx = x[["regex"]])
  )
}

unt_col_helper <- function(x) {

  itm <- x[["itm_cols"]]
  cbc <- x[["cb_cols"]]

  assert_that(is.string(itm))

  if (has_name(cbc, "unit_col")) {

    unt <- cbc[["unit_col"]]

    if (length(cbc) == 1L) {
      x["cb_cols"] <- list(NULL)
    } else {
      x[["cb_cols"]] <- cbc[setdiff(names(cbc), "unit_col")]
    }

  } else {

    unt <- default_col(as_src_tbl(x), "unit")

    if (is.na(unt)) {
      unt <- NULL
    }
  }

  x[["itm_cols"]] <- c(val_col = unname(itm), unit_col = unname(unt))

  x
}

#' @rdname item_utils
#' @keywords internal
#' @export
add_unit_col <- function(x) UseMethod("add_unit_col", x)

#' @export
add_unit_col.sel_itm <- function(x) unt_col_helper(x)

#' @export
add_unit_col.col_itm <- function(x) unt_col_helper(x)

#' @export
add_unit_col.rgx_itm <- function(x) unt_col_helper(x)

#' @export
add_unit_col.itm <- function(x) x

#' @rdname data_items
#' @export
new_item <- function(x) {

  assert_that(is.list(x), all_fun(x, is_itm))

  new_vctr(x, class = "item")
}

#' @rdname data_concepts
#' @export
item <- function(...) {
  new_item(do.call(Map, c(list(new_itm), vec_recycle_common(...))))
}

#' @rdname data_concepts
#' @export
as_item <- function(x) UseMethod("as_item", x)

#' @export
as_item.item <- function(x) x

#' @export
as_item.list <- function(x) new_item(x)

#' @export
as_item.itm <- function(x) as_item(list(x))

#' @export
format.item <- function(x, ...) {
  paste0("<", chr_xtr(lapply(x, class), 1L), ">")
}

#' @export
names.item <- function(x) chr_xtr(x, "src")

#' @export
as.list.item <- function(x, ...) vec_data(x)

#' @rdname data_items
#' @export
is_item <- function(x) inherits(x, "item")

#' @export
src_name.item <- function(x) names(x)

#' Data concept
#'
#' Clinical concepts are represented by `concept` objects.
#'
#' @param name The name of the concept
#' @param items Zero or more `itm` objects
#' @param ... Further specification of the `cncpt` object (passed to
#' [init_cncpt()])
#' @param class `NULL` or a string-valued sub-class name used for customizing
#' concept behavior
#'
#' @rdname data_concepts
#'
#' @export
new_cncpt <- function(name, items, ..., class = "num_cncpt") {

  assert_that(is.string(name), null_or(class, is.string))

  res <- structure(list(name = name, items = as_item(items)),
                   class = c(class, "cncpt"))

  init_cncpt(res, ...)
}

#' @param x Object to query/dispatch on
#'
#' @rdname data_concepts
#' @export
is_cncpt <- function(x, ...) inherits(x, "cncpt")

#' @rdname data_concepts
#' @export
init_cncpt <- function(x, ...) UseMethod("init_cncpt", x)

#' @param unit A string, specifying the measurement unit of the concept (can
#' be `NULL`)
#' @param min,max Scalar valued; defines a range of plausible values for a
#' numeric concept
#'
#' @rdname data_concepts
#' @export
init_cncpt.num_cncpt <- function(x, unit = NULL, min = NULL, max = NULL, ...) {

  warn_dots(...)

  assert_that(null_or(unit, is.string), null_or(min, is.number),
              null_or(max, is.number))

  todo <- c("unit", "min", "max")
  x[todo] <- mget(todo)

  x[["items"]] <- new_item(lapply(x[["items"]], add_unit_col))

  x
}

#' @param levels A vector of possible values a categorical concept may take on
#'
#' @rdname data_concepts
#' @export
init_cncpt.fct_cncpt <- function(x, levels, ...) {

  warn_dots(...)

  assert_that(is.atomic(levels), has_length(levels))

  x[["levels"]] <- levels

  x
}

#' @rdname data_concepts
#' @export
init_cncpt.cncpt <- function(x, ...) {

  dots <- list(...)

  assert_that(is_disjoint(names(x), names(dots)))

  x[names(dots)] <- dots

  x
}

#' @rdname data_concepts
#' @export
src_name.cncpt <- function(x) src_name(x[["items"]])

#' @rdname data_concepts
#' @export
new_concept <- function(x) {

  assert_that(is.list(x), all_fun(x, is_cncpt))

  res <- new_vctr(x, class = "concept")

  assert_that(is_unique(names(res)))

  res
}

#' @rdname data_concepts
#' @export
concept <- function(...) {
  new_concept(do.call(Map, c(list(new_cncpt), vec_recycle_common(...))))
}

#' @rdname data_concepts
#' @export
is_concept <- function(x) inherits(x, "concept")

#' @export
vec_ptype_full.concept <- function(x, ...) {

  srcs <- sort(
    unique(unlist(lapply(lst_xtr(x, "items"), names), use.names = FALSE))
  )

  paste0(class(x)[1L], "{", concat(srcs), "}")
}

#' @export
format.concept <- function(x, ...) {

  cnt  <- function(i, nm) int_ply(nm, function(x) sum(x == i))

  nms  <- lapply(lst_xtr(x, "items"), names)
  srcs <- sort(unique(unlist(nms, use.names = FALSE)))
  cnts <- int_ply(srcs, cnt, nms, length = length(nms))

  apply(
    matrix(cnts, nrow = length(nms)), 1L,
    function(row) paste0("[", paste0(row, collapse = ", "), "]")
  )
}

#' @export
names.concept <- function(x) chr_xtr(x, "name")

#' @export
as.list.concept <- function(x, ...) vec_data(x)

#' @export
src_name.concept <- function(x) lapply(x, src_name)

#' @param src `NULL` or the name of a data source
#' @param concepts A character vector used to subset the concept dictionary or
#' `NULL` indicating no subsetting
#' @param name Name of the dictionary to be read
#' @param file File name of the dictionary
#'
#' @rdname data_concepts
#'
#' @export
read_dictionary <- function(src = NULL, concepts = NULL,
                            name = "concept-dict", file = NULL, ...) {

  do_itm <- function(sr, tr, x) {
    lapply(lapply(x, c, src = sr, target = tr), do_call, new_itm)
  }

  do_cncpt <- function(name, sources, target = "ts_tbl", ...) {

    if (not_null(src)) {
      sources <- sources[src]
    }

    itms <- new_item(
      do.call(c, Map(do_itm, names(sources), target, sources))
    )

    lst <- list(...)

    if (has_length(lst)) {
      do.call(new_cncpt, c(list(name = name, items = itms), lst))
    } else {
      do.call(new_cncpt, c(list(name = name, items = itms, class = NULL)))
    }
  }

  if (is.null(file)) {

    x <- get_config(name, ...)

  } else {

    assert_that(missing(name), file.exists(file))

    x <- read_json(file, ...)
  }

  assert_that(null_or(src, is.string))

  if (not_null(concepts)) {
    assert_that(has_name(x, concepts))
    x <- x[concepts]
  }

  new_concept(lapply(Map(c, name = names(x), x), do_call, do_cncpt))
}

identity_callback <- function(x, ...) x
