
#' Data item
#'
#' In order to specify the location of data items in data sets, `item` objects
#' are used.
#'
#' @param concept The name of the associated clinical concept
#' @param source The data source name
#' @param table Name of the table containing the data
#' @param column Column name
#' @param ids Vector of ids used to subset table rows. If `NULL`, all rows are
#' considered corresponding to the data item
#' @param regex Logical flag indicating whether the vector of id should be
#' interpreted as regular expressions
#' @param callback Name of a function to be called on the returned data used
#' for data cleanup operations
#' @param ... Further name/string pairs specifying additional columns needed to
#' define the data item
#'
#' @rdname data_items
#'
#' @export
#'
new_item <- function(concept, source, table, column = NULL, ids = NULL,
                     regex = FALSE, callback = NULL, ...) {

  assert_that(is.string(concept), is.string(source), is.string(table),
              is.null(column) || is.string(column),
              is.atomic(ids), is.flag(regex),
              is.null(callback) || is.string(callback))

  if (!is.null(callback)) {
    assert_that(exists(callback, mode = "function"))
  }

  item <- list(concept = concept, source = source, table = table,
               column = column, ids = ids, regex = regex, callback = callback)

  extra <- list(...)

  if (length(extra) > 0L) {

    assert_that(all(lgl_ply(extra, is.string)),
                !is.null(names(extra)), is_unique(names(extra)))

    item <- c(item, extra)
  }

  structure(list(item), class = "item")
}

#' @rdname data_items
#'
#' @export
#'
item <- function(...) {

  args <- list(...)
  lens <- lengths(args)

  assert_that(all(lens == max(lens) | lens == 1L))

  args[lens == 1L] <- lapply(args[lens == 1L], rep, max(lens))

  do.call(c, do.call(map, c(new_item, args)))
}

#' @param x A data item object
#'
#' @rdname data_items
#'
#' @export
#'
is_item <- function(x) inherits(x, "item")

#' @export
names.item <- function(x) chr_ply(x, .subset2, "concept")

#' @export
c.item <- function(...) {

  items <- list(...)
  items <- Filter(Negate(is.null), items)

  if (all(lgl_ply(items, is_item))) {
    structure(NextMethod(), class = "item")
  } else {
    NextMethod()
  }
}

#' @export
`[.item` <- function(x, i, source = NULL, ...) {

  recreate <- function(y) do.call(new_item, y)

  if (!missing(i)) {
    x <- .subset(x, i)
  }

  if (!is.null(source)) {
    assert_that(is.character(source), length(source) > 0L)
    x <- .subset(x, get_source(x) %in% source)
  }

  do.call(c, lapply(x, recreate))
}

#' @export
get_source.item <- function(x) chr_ply(x, .subset2, "source")

#' @export
as_item.item <- function(x) x

#' @export
as_item.list <- function(x) try_new(x, new_item)

#' Data concept
#'
#' Clinical concepts are represented by `concept` objects.
#'
#' @param name The name of the concept
#' @param items One or more `item` objects
#' @param unit A string, specifying the measurement unit of the concept (can
#' be `NULL`)
#'
#' @rdname data_concepts
#'
#' @export
new_concept <- function(name, items, unit = NULL) {

  assert_that(is.string(name), is_item(items),
              all(lgl_ply(names(items), identical, name)),
              is.null(unit) || is.string(unit))

  concept <- list(name = name, unit = unit, items = items)

  structure(list(concept), class = "concept")
}

#' @param x A concept object
#'
#' @rdname data_concepts
#'
#' @export
#'
is_concept <- function(x) inherits(x, "concept")

#' @export
names.concept <- function(x) chr_ply(x, .subset2, "name")

#' @export
c.concept <- function(...) {

  concepts <- list(...)
  concepts <- Filter(Negate(is.null), concepts)

  if (all(lgl_ply(concepts, is_concept))) {
    res <- structure(NextMethod(), class = "concept")
    assert_that(is_unique(names(res)))
  } else {
    res <- NextMethod()
  }

  res
}

#' @param ... Forwarded to [new_item()]
#'
#' @rdname data_concepts
#'
#' @export
#'
concept <- function(name, ..., unit = NULL) {

  args <- list(...)
  lens <- lengths(args)

  assert_that(all(lens == max(lens) | lens == 1L))

  items <- do.call(c,
    do.call(map, c(new_item, list(concept = name), args))
  )

  new_concept(name, items, unit)
}

#' @export
`[.concept` <- function(x, i, source = NULL, ...) {

  do_one <- function(y, src) {

    if (!is.null(src)) {

      itms <- .subset2(y, "items")[source = source]

      if (is.null(itms)) {
        return(NULL)
      }

      y[["items"]] <- itms
    }

    do.call(new_concept, y)
  }

  if (missing(i)) {
    i <- TRUE
  } else if (is.character(i)) {
    assert_that(length(i) > 0L, all(i %in% names(x)), !anyNA(i))
    i <- match(i, names(x))
  }

  res <- lapply(.subset(x, i), do_one, source)

  do.call(c, Filter(Negate(is.null), res))
}

#' @export
get_source.concept <- function(x) {
  res <- lapply(x, `[[`, "items")
  res <- lapply(res, get_source)
  chr_ply(res, unique)
}

#' @export
as_item.concept <- function(x) do.call(c, lapply(x, `[[`, "items"))

#' @export
as_concept.concept <- function(x, ...) x

#' @export
as_concept.item <- function(x, ...) {

  nms <- unique(names(x))

  assert_that(length(nms) == 1L)

  new_concept(nms, x, ...)
}

#' @export
as_concept.list <- function(x, ...) try_new(x, new_concept)

#' @export
as_dictionary.concept <- function(x) new_dictionary(x)

#' Concept dictionary
#'
#' Multiple concepts, each specifying data items in several sources, together
#' form a dictionary.
#'
#' @param concepts A vector of concept objects
#'
#' @rdname data_dictionary
#'
#' @export
new_dictionary <- function(concepts) {

  concepts <- as_concept(concepts)

  assert_that(is_unique(names(concepts)))

  structure(list(concepts), class = "dictionary")
}

#' @param ... Wrapped by [base::list()] and forwarded to [as_concept()]
#'
#' @rdname data_dictionary
#'
#' @export
dictionary <- function(...) new_dictionary(list(...))

#' @param x A potential `dictionary` object
#'
#' @rdname data_dictionary
#'
#' @export
is_dictionary <- function(x) inherits(x, "dictionary")

#' @export
as_dictionary.dictionary <- function(x) x

#' @export
as_dictionary.list <- function(x) do.call(new_dictionary, x)

#' @export
names.dictionary <- function(x) names(as_concept(x))

#' @export
length.dictionary <- function(x) length(as_concept(x))

#' @export
`[.dictionary` <- function(x, i, source = NULL, ...) {
  new_dictionary(as_concept(x)[i, source = source, ...])
}

#' @export
get_source.dictionary <- function(x) {
  res <- unique(get_source(as_concept(x)))
  assert_that(length(res) == 1L)
  res
}

#' @export
str.dictionary <- function(object, ...) str(as_concept(object), ...)

#' @export
as_item.dictionary <- function(x) as_item(as_concept(x))

#' @export
as_concept.dictionary <- function(x, ...) .subset2(x, 1L)

#' @param name Name of the dictionary to be read
#' @param file File name of the dictionary
#'
#' @rdname data_dictionary
#'
#' @export
read_dictionary <- function(name = "concept-dict", file = NULL, ...) {

  do_itm <- function(x, nme, conc) {
    do.call(new_item, c(list(concept = conc, source = nme), x))
  }

  do_itms <- function(itms, nme, conc) {
    do.call(c, lapply(itms, do_itm, nme, conc))
  }

  do_conc <- function(conc, name) {
    items <- Map(do_itms, conc[["sources"]], names(conc[["sources"]]), name,
                 USE.NAMES = FALSE)
    items <- Filter(Negate(is.null), items)
    args <- c(list(name = name), list(do.call(c, items)),
              conc[names(conc) != "sources"])
    do.call(new_concept, args)
  }

  if (!is.null(file)) {

    assert_that(missing(name), file.exists(file))

    dat <- read_json(file, ...)

  } else {

    dat <- get_config(name, ...)
  }

  concepts <- Map(do_conc, dat, names(dat), USE.NAMES = FALSE)
  concepts <- do.call(c, concepts)

  new_dictionary(concepts)
}

do_new <- function(x, fun, ...) do.call(fun, x, ...)

try_new <- function(x, fun) {
  tryCatch(do.call(fun, x),
           error = function(...) unname(do.call("c", lapply(x, do_new, fun))))
}
