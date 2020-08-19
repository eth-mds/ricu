
#' Data items
#'
#' Item objects are used in `ricu` as a way to specify how individual data
#' items corresponding to clinical concepts (see also [concept()]), such as
#' heart rate can be loaded from a data source. Several functions are
#' available for constructing `item` (and related auxillary) objects either
#' from code or by parsing a JSON formatted concept dictionary using
#' [load_dicionary()].
#'
#' @details
#' In order to allow for a large degree of flexibility (and extensibility),
#' which is much needed owing to considerable heterogeneity presented by
#' different data sources, several nested S3 classes are involved in
#' representing a concept. An outline of this hierarchy can be described as
#'
#' * `concept`: contains many `cncpt` objects (of potentially differing
#'   sub-types), each comprising of some meta-data and an `item` object
#' * `item`: contains many `itm` objects (of potentially differing
#'   sub-types), each encoding how to retrieve a data item.
#'
#' The design choice for wrapping a vector of `itm` objects with a container
#' class `item` is motivated by the requirement of having several different
#' sub-types of `itm` objects (all inheriting from the parent type `itm`),
#' while retaining control over how this homogeneous w.r.t. parent type, but
#' heterogeneous w.r.t. sub-type vector of objects behaves in terms of S3
#' generic functions.
#'
#' The following sub-classes to `itm` are available, each representing a
#' different data-scenario:
#'
#' * `sel_itm`: The most widely used item class is intended for the situation
#' where rows of interest can be identified by looking for occurrences of a
#' set of IDs (`ids`) in a column (`sub_var`). An example for this is heart
#' rate `hr` on mimic, where the IDs `211` and 220045` are looked up in the
#' `itemid` column of `chartevents`.
#' * `col_itm`: This item class can be used if no row-subsetting is required.
#' An example for this is heart rate (`hr`) on `eicu`, where the table
#' `vitalperiodic` contains an entire column dedicated to heart rate
#' measurements.
#' * `rgx_itm`: As alternative to the value-matching approach of `sel_itm`
#' objects, this class identifies rows using regular expressions. Used for
#' example for insulin in `eicu`, where the regular expression `^insulin
#' (250.+)?\\(((ml|units)/hr)?\\)$` is matched against the `drugname` column
#' of `infusiondrug`. The regular expression is evaluated by [base::grepl()]
#' with `ignore.case = TRUE`.
#' * `fun_itm`: Intended for the scenario where data of interest is not
#' directly available from a table, this `itm` class offers most flexbility. A
#' function can be specified as `callback` and this function will be called
#' with arguments `x` (the object itself), `patient_ids`, `id_type` and
#' `interval` (see [load_concepts()]) and is expected to return an object as
#' specified by the `target` entry.
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

  assert_that(is.string(src), is.character(class), has_length(class))

  target <- match.arg(target)

  init_itm(
    structure(list(src = src, target = target), class = c(class, "itm")), ...
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
tbl_name.sel_itm <- function(x) x[["table"]]

#' @export
tbl_name.col_itm <- function(x) x[["table"]]

#' @export
tbl_name.rgx_itm <- function(x) x[["table"]]

#' @export
as_src_tbl.itm <- function(x, ...) {
  as_src_tbl(tbl_name(x), src_name(x), ...)
}

itm_var_helper <- function(x, col) {

  res <- coalesce(col, val_var(as_src_tbl(x)))

  if (is.string(res)) {
    res <- c(val_var = res)
  }

  assert_that(has_name(res, "val_var"))

  res
}

idx_var_helper <- function(x, col) {
  if (need_idx(x)) coalesce(col, index_var(as_src_tbl(x))) else NULL
}

cbk_var_helper <- function(...) {
  if (...length()) unlist(list(...), recursive = FALSE) else NULL
}

#' @rdname data_items
#' @export
init_itm <- function(x, ...) UseMethod("init_itm", x)

#' @param table Name of the table containing the data
#' @param sub_var Column name used for subsetting
#' @param ids Vector of ids used to subset table rows. If `NULL`, all rows are
#' considered corresponding to the data item
#' @param itm_vars Columns returned as [data_vars()]
#' @param index_var Column used as index
#' @param callback Name of a function to be called on the returned data used
#' for data cleanup operations (or a string that evaluates to a function)
#'
#' @rdname data_items
#' @export
init_itm.sel_itm <- function(x, table, sub_var, ids, itm_vars = NULL,
                             index_var = NULL,
                             callback = "identity_callback", ...) {

  x[["table"]] <- table

  itm_vars  <- itm_var_helper(x, itm_vars)
  index_var <- idx_var_helper(x, index_var)
  cb_vars   <- cbk_var_helper(...)

  cols <- colnames(as_src_tbl(x))

  assert_that(
    is.string(table), evals_to_fun(callback), has_length(ids),
    is.character(ids) || is.integer(ids),
    all_fun(c(list(sub_var, itm_vars), index_var, cb_vars), is_in, cols)
  )

  todo <- c("ids", "sub_var", "itm_vars", "index_var", "cb_vars", "callback")
  x[todo] <- mget(todo)

  x
}

#' @param unit_val String valued unit to be used in case no `unit_var` is
#' available for the given table
#'
#' @rdname data_items
#' @export
init_itm.col_itm <- function(x, table, itm_vars = NULL, index_var = NULL,
                             unit_val = NULL, callback = "identity_callback",
                             ...) {

  x[["table"]] <- table

  itm_vars  <- itm_var_helper(x, itm_vars)
  index_var <- idx_var_helper(x, index_var)
  cb_vars   <- cbk_var_helper(...)

  cols <- colnames(as_src_tbl(x))

  assert_that(
    is.string(table), evals_to_fun(callback), null_or(unit_val, is.string),
    all_fun(c(list(itm_vars), index_var, cb_vars), is_in, cols)
  )

  todo <- c("itm_vars", "index_var", "cb_vars", "unit_val", "callback")
  x[todo] <- mget(todo)

  x
}

#' @param regex String-valued regular expression which will be evaluated by
#' [base::grepl()] with `ignore.case = TRUE`
#'
#' @rdname data_items
#' @export
init_itm.rgx_itm <- function(x, table, sub_var, regex, itm_vars = NULL,
                             index_var = NULL,
                             callback = "identity_callback", ...) {

  x[["table"]] <- table

  itm_vars  <- itm_var_helper(x, itm_vars)
  index_var <- idx_var_helper(x, index_var)
  cb_vars   <- cbk_var_helper(...)

  cols <- colnames(as_src_tbl(x))

  assert_that(
    all_fun(list(table, regex), is.string), evals_to_fun(callback),
    all_fun(c(list(sub_var, itm_vars), index_var, cb_vars), is_in, cols)
  )

  todo <- c("regex", "sub_var", "itm_vars", "index_var", "cb_vars", "callback")
  x[todo] <- mget(todo)

  x
}

#' @rdname data_items
#' @export
init_itm.fun_itm <- function(x, callback, ...) {

  dots <- list(...)

  assert_that(evals_to_fun(callback), is_disjoint(names(x), names(dots)))

  x[c("callback", names(dots))] <- c(list(callback), dots)

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
  lst <- list(col = as.name(x[["sub_var"]]), id = ids)

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
    list(col = as.name(x[["sub_var"]]), rgx = x[["regex"]])
  )
}

#' @keywords internal
#' @export
prepare_query.col_itm <- function(x) rlang::quo(NULL)

unt_col_helper <- function(x) {

  itm <- x[["itm_vars"]]
  cbc <- x[["cb_vars"]]

  assert_that(is.string(itm))

  if (has_name(cbc, "unit_var")) {

    unt <- cbc[["unit_var"]]

    if (length(cbc) == 1L) {
      x["cb_vars"] <- list(NULL)
    } else {
      x[["cb_vars"]] <- cbc[setdiff(names(cbc), "unit_var")]
    }

  } else {

    unt <- unit_var(as_src_tbl(x))
  }

  x[["itm_vars"]] <- c(val_var = unname(itm), unit_var = unname(unt))

  x
}

#' @rdname item_utils
#' @keywords internal
#' @export
add_unit_var <- function(x) UseMethod("add_unit_var", x)

#' @export
add_unit_var.sel_itm <- function(x) {

  if (identical(src_name(x), "hirid") &&
      identical(tbl_name(x), "observations")) {

    class(x) <- unique(c("hrd_itm", class(x)))

  } else {

    x <- unt_col_helper(x)
  }

  x
}

#' @export
add_unit_var.col_itm <- function(x) unt_col_helper(x)

#' @export
add_unit_var.rgx_itm <- function(x) unt_col_helper(x)

#' @export
add_unit_var.itm <- function(x) x

#' @rdname data_items
#' @export
new_item <- function(x, target = NULL) {

  trg <- chr_ply(x, target_class)

  if (is.null(target)) {
    target <- trg[1L]
  }

  assert_that(is.list(x), all_fun(x, is_itm), all_fun(trg, identical, target))

  new_vctr(x, target = target, class = "item")
}

#' @rdname data_items
#' @export
item <- function(...) {
  new_item(do.call(Map, c(list(new_itm), vec_recycle_common(...))))
}

#' @rdname data_items
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

#' @rdname item_utils
#' @keywords internal
#' @export
n_tick <- function(x) UseMethod("n_tick", x)

#' @export
n_tick.itm <- function(x) 1L

#' @export
n_tick.item <- function(x) length(x)

#' @rdname item_utils
#' @keywords internal
#' @export
target_class <- function(x) UseMethod("target_class", x)

#' @export
target_class.itm <- function(x) x[["target"]]

#' @export
target_class.item <- function(x) attr(x, "target")

#' @export
target_class.cncpt <- function(x) target_class(x[["items"]])

#' @export
target_class.rec_cncpt <- function(x) x[["target"]]

need_idx <- function(x) identical(target_class(x), "ts_tbl")

#' Data items
#'
#' Concept objects are used in `ricu` as a way to specify how a clinical
#' concept, such as heart rate can be loaded from a data source and are mainly
#' consumed by [load_concepts()]. Several functions are avialable for
#' constructing `concept` (and related auxillary) objects either from code or
#' by parsing a JSON formatted concept dictionary using [load_dicionary()].
#'
#' @details:
#' In order to allow for a large degree of flexibility (and extensibility),
#' which is much needed owing to considerable heterogeneity presented by
#' different data sources, several nested S3 classes are involved in
#' representing a concept. An outline of this hierarchy can be described as
#'
#' * `concept`: contains many `cncpt` objects (of potentially differing
#'   sub-types), each comprising of some meta-data and an `item` object
#' * `item`: contains many `itm` objects (of potentially differing
#'   sub-types), each encoding how to retrieve a data item.
#'
#' The design choice for wrapping a vector of `cncpt` objects with a container
#' class `concept` is motivated by the requirement of having several different
#' sub-types of `cncpt` objects (all inheriting from the parent type `cncpt`),
#' while retaining control over how this homogeneous w.r.t. parent type, but
#' heterogeneous w.r.t. sub-type vector of objects behaves in terms of S3
#' generic functions.
#'
#' @param name The name of the concept
#' @param items Zero or more `itm` objects
#' @param description String-valued concept description
#' @param aggregate NULL or a string denoting a function used to aggregate per
#' id and if applicable per time step
#' @param ... Further specification of the `cncpt` object (passed to
#' [init_cncpt()])
#' @param class `NULL` or a string-valued sub-class name used for customizing
#' concept behavior
#'
#' @rdname data_concepts
#'
#' @export
new_cncpt <- function(name, items, description = NULL, aggregate = NULL, ...,
                      class = "num_cncpt") {

  assert_that(is.string(name), null_or(class, is.string),
              null_or(description, is.string))

  if (!is_concept(items)) {
    items <- as_item(items)
  }

  res <- structure(list(name = name, items = items, description = description,
                        aggregate = aggregate), class = c(class, "cncpt"))

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

  assert_that(null_or(unit, is.character), null_or(unit, has_length),
              null_or(min, is.number), null_or(max, is.number),
              null_or(x[["aggregate"]], is.string))

  todo <- c("unit", "min", "max")
  x[todo] <- mget(todo)

  targ <- attr(x[["items"]], "target")

  x[["items"]] <- new_item(lapply(x[["items"]], add_unit_var), targ)

  x
}

#' @param levels A vector of possible values a categorical concept may take on
#'
#' @rdname data_concepts
#' @export
init_cncpt.fct_cncpt <- function(x, levels, ...) {

  warn_dots(...)

  assert_that(is.atomic(levels), has_length(levels),
              null_or(x[["aggregate"]], is.string))

  x[["levels"]] <- levels

  x
}

#' @rdname data_concepts
#' @export
init_cncpt.cncpt <- function(x, ...) {

  dots <- list(...)

  assert_that(is_disjoint(names(x), names(dots)),
              null_or(x[["aggregate"]], is.string))

  x[names(dots)] <- dots

  x
}

#' @param callback Name of a function to be called on the returned data used
#' for data cleanup operations
#' @param target The target object yielded by loading
#' @param interval Time interval used for data loading; if NULL, the respective
#' interval passed as argument to [load_concepts()] is taken
#'
#' @rdname data_concepts
#' @export
init_cncpt.rec_cncpt <- function(x, callback, target = c("ts_tbl", "id_tbl"),
                                 interval = NULL, ...) {

  really_na <- function(x) not_null(x) && is.na(x)

  warn_dots(...)

  target <- match.arg(target)

  assert_that(is_concept(x[["items"]]), is.string(callback),
              null_or(interval, is.string))

  if (not_null(interval)) {
    interval <- as.difftime(interval)
  }

  agg <- rep_arg(x[["aggregate"]], names(x[["items"]]))
  agg[lgl_ply(agg, really_na)] <- list(NULL)

  x[["aggregate"]] <- agg

  todo <- c("callback", "target", "interval")
  x[todo] <- mget(todo)

  x
}

#' @export
src_name.cncpt <- function(x) src_name(x[["items"]])

#' @importFrom stats aggregate
#' @export
aggregate.cncpt <- function(x, tbl, fun = NULL, ...) {

  fun <- coalesce(fun, x[["aggregate"]])

  if (!isFALSE(fun)) {
    tbl <- make_unique(tbl, fun)
  }

  tbl
}

#' @export
aggregate.rec_cncpt <- function(x, ...) {
  stop_ricu("please use `callback` for aggregating within time-steps",
            class = "aggregate_rec_cncpt")
}

#' @export
n_tick.cncpt <- function(x) sum(int_ply(x[["items"]], n_tick)) + 1L

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
format.concept <- function(x, ...) {

  desc <- lst_xtr(x, "description")
  desc[lgl_ply(desc, is.null)] <- ""

  paste0(
    desc, " (<", chr_xtr(lapply(x, "class"), 1L), "[",
    int_ply(lst_xtr(x, "items"), length), "]> ", symbol$arrow_right, " <",
    chr_ply(x, target_class), ">)"
  )
}

#' @export
names.concept <- function(x) chr_xtr(x, "name")

#' @export
as.list.concept <- function(x, ...) vec_data(x)

#' @export
src_name.concept <- function(x) lapply(x, src_name)

#' @export
n_tick.concept <- function(x) sum(int_ply(x, n_tick))

#' Load concept dictionaries
#'
#' Data can be specified in JSON format and parsed into `concept` objects.
#'
#' @param src `NULL` or the name of a data source
#' @param concepts A character vector used to subset the concept dictionary or
#' `NULL` indicating no subsetting
#' @param name Name of the dictionary to be read
#' @param file File name of the dictionary
#'
#' @rdname concept_dictionary
#'
#' @export
load_dictionary <- function(src = NULL, concepts = NULL,
                            name = "concept-dict", file = NULL) {

  parse_dictionary(read_dictionary(name, file), src, concepts)
}

read_dictionary <- function(name = "data-sources", file = NULL) {

  combine_sources <- function(x, y) {

    assert_that(
      !identical(x[["class"]], "rec_cncpt"), not_null(names(y[["sources"]])),
      length(y) == 1L, has_name(y, "sources"), is.list(y[["sources"]])
    )

    new_sources    <- c(y[["sources"]], x[["sources"]])
    x[["sources"]] <- new_sources[!duplicated(names(new_sources))]

    x
  }

  if (is.null(file)) {

    file <- paste0(name, ".json")

    usr_file <- file.path(user_config_path(), file)
    usr_exst <- isTRUE(file.exists(usr_file))

    if (usr_exst) {

      usr_dict <- read_json(usr_file)

      assert_that(is.list(usr_dict), not_null(names(usr_dict)))
    }

    res <- read_json(file.path(default_config_path(), file))

    if (usr_exst) {

      dups <- intersect(names(res), names(usr_dict))

      if (has_length(dups)) {
        res[dups] <- map(combine_sources, res[dups], usr_dict[dups])
        usr_dict[dups] <- NULL
      }

      res <- c(usr_dict, res)
    }

  } else {

    res <- read_json(file)
  }

  res
}

parse_dictionary <- function(dict, src = NULL, concepts = NULL) {

  do_itm <- function(sr, tr, x) {
    lapply(lapply(x, c, src = sr, target = tr), do_call, new_itm)
  }

  do_cncpt <- function(name, sources, target = "ts_tbl", ...) {

    lst <- list(...)

    if (is_concept(sources)) {

      itms <- sources
      lst[["concepts"]] <- NULL

    } else {

      if (not_null(src)) {
        sources <- sources[src]
      }

      itms <- new_item(
        do.call(c, Map(do_itm, names(sources), target, sources)),
        target
      )
    }

    if (has_length(lst)) {

      if ("rec_cncpt" %in% lst[["class"]]) {

        do.call(new_cncpt,
          c(list(name = name, items = itms, target = target), lst)
        )

      } else {
        do.call(new_cncpt, c(list(name = name, items = itms), lst))
      }
    } else {
      do.call(new_cncpt, c(list(name = name, items = itms, class = NULL)))
    }
  }

  do_new <- function(sel, ful) {

    if (is.null(sel)) {
      return(sel)
    }

    assert_that(are_in(sel, names(ful)))

    sub <- ful[sel]

    re_con <- lapply(lst_xtr(sub, "concepts"), do_new, ful)
    is_rec <- lgl_ply(lst_xtr(sub, "class"), identical, "rec_cncpt")

    if (any(is_rec)) {
      sub[is_rec] <- Map(`[[<-`, sub[is_rec], "sources", re_con[is_rec])
    }

    new_concept(lapply(Map(c, name = names(sub), sub), do_call, do_cncpt))
  }

  assert_that(null_or(src, is.string))

  if (is.null(concepts)) {
    concepts <- names(dict)
  }

  do_new(concepts, dict)
}

identity_callback <- function(x, ...) x
