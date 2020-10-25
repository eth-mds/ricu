
#' Data items
#'
#' Item objects are used in `ricu` as a way to specify how individual data
#' items corresponding to clinical concepts (see also [concept()]), such as
#' heart rate can be loaded from a data source. Several functions are
#' available for constructing `item` (and related auxiliary) objects either
#' from code or by parsing a JSON formatted concept dictionary using
#' [load_dictionary()].
#'
#' @details
#' In order to allow for a large degree of flexibility (and extensibility),
#' which is much needed owing to considerable heterogeneity presented by
#' different data sources, several nested S3 classes are involved in
#' representing a concept. An outline of this hierarchy can be described as
#'
#' * [`concept`][concept()]: contains many `cncpt` objects (of potentially
#'   differing sub-types), each comprising of some meta-data and an `item`
#'   object
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
#'   where rows of interest can be identified by looking for occurrences of a
#'   set of IDs (`ids`) in a column (`sub_var`). An example for this is heart
#'   rate `hr` on mimic, where the IDs `211` and 220045` are looked up in the
#'   `itemid` column of `chartevents`.
#' * `col_itm`: This item class can be used if no row-subsetting is required.
#'   An example for this is heart rate (`hr`) on `eicu`, where the table
#'   `vitalperiodic` contains an entire column dedicated to heart rate
#'   measurements.
#' * `rgx_itm`: As alternative to the value-matching approach of `sel_itm`
#'   objects, this class identifies rows using regular expressions. Used for
#'   example for insulin in `eicu`, where the regular expression `^insulin
#'   (250.+)?\\(((ml|units)/hr)?\\)$` is matched against the `drugname` column
#'   of `infusiondrug`. The regular expression is evaluated by [base::grepl()]
#'   with `ignore.case = TRUE`.
#' * `fun_itm`: Intended for the scenario where data of interest is not
#'   directly available from a table, this `itm` class offers most flexibility.
#'   A function can be specified as `callback` and this function will be called
#'   with arguments `x` (the object itself), `patient_ids`, `id_type` and
#'   `interval` (see [load_concepts()]) and is expected to return an object as
#'   specified by the `target` entry.
#' * `hrd_itm`: A special case of `sel_itm` for HiRID data where measurement
#'    units are not available as separate column, but as separate table with
#'    units fixed per concept.
#'
#' All `itm` objects have to specify a data source (`src`) as well as a
#' sub-class. Further arguments then are specific to the respective sub-class
#' and encode information that define data loading, such as the table to
#' query, the column name and values to use for identifying relevant rows,
#' etc. The S3 generic function `init_itm()` is responsible for input
#' validation of class-specific arguments as well as class initialization. A
#' list of `itm` objects, created by calls to `new_itm()` can be passed to
#' `new_item` in order to instantiate an `item` object. An alternative
#' constructor for `item` objects is given by `item()` which calls `new_itm()`
#' on the passed arguments (see examples). Finally `as_item()` can be used
#' for coercion of related objects such as `list`, `concept`, and the like.
#' Several additional S3 generic functions exist for manipulation of
#' `item`-like objects but are marked `internal` (see
#' [item/concept utilities][prepare_query()]).
#'
#' @param src The data source name
#' @param ... Further specification of the `itm` object (passed to
#' [init_itm()])
#' @param target Item target class (e.g. "id_tbl"), `NA` indicates no specific
#' class requirement
#' @param class Sub class for customizing `itm` behavior
#'
#' @return Constructors and coercion functions return `itm` and `item` objects,
#' while inheritance tester functions return logical flags.
#'
#' @rdname data_items
#'
#' @examples
#' if (require(mimic.demo)) {
#' gluc <- item("mimic_demo", "labevents", "itemid", list(c(50809L, 50931L)),
#'              unit_var = TRUE, target = "ts_tbl")
#'
#' is_item(gluc)
#'
#' all.equal(gluc, as_item(load_dictionary("mimic_demo", "glu")))
#'
#' hr1 <- new_itm(src = "mimic_demo", table = "chartevents",
#'                sub_var = "itemid", ids = c(211L, 220045L))
#'
#' hr2 <- item(src = c("mimic_demo", "eicu_demo"),
#'             table = c("chartevents", "vitalperiodic"),
#'             sub_var = list("itemid", NULL),
#'             val_var = list(NULL, "heartrate"),
#'             ids = list(c(211L, 220045L), NULL),
#'             class = c("sel_itm", "col_itm"))
#'
#' identical(as_item(hr1), hr2[1])
#' identical(new_item(list(hr1)), hr2[1])
#' }
#'
#' @export
#'
new_itm <- function(src, ..., target = NA_character_, class = "sel_itm") {

  assert_that(is.string(src), is.character(class), has_length(class))

  init_itm(structure(list(src = src, target = target),
           class = c(class, "itm")), ...)
}

#' @param x Object to query/dispatch on
#'
#' @rdname data_items
#' @export
is_itm <- is_type("itm")

#' @export
src_name.itm <- function(x) x[["src"]]

#' @export
tbl_name.itm <- function(x) {
  res <- x[["table"]]
  assert_that(is.string(res))
  res
}

#' @export
tbl_name.fun_itm <- function(x) NULL

#' @export
as_src_tbl.itm <- function(x, ...) as_src_tbl(tbl_name(x), src_name(x), ...)

#' @rdname data_items
#' @export
init_itm <- function(x, ...) UseMethod("init_itm", x)

#' @param table Name of the table containing the data
#' @param sub_var Column name used for subsetting
#' @param ids Vector of ids used to subset table rows. If `NULL`, all rows are
#' considered corresponding to the data item
#' @param callback Name of a function to be called on the returned data used
#' for data cleanup operations (or a string that evaluates to a function)
#'
#' @rdname data_items
#' @export
init_itm.sel_itm <- function(x, table, sub_var, ids,
                             callback = "identity_callback", ...) {

  assert_that(is.string(table), has_length(ids),
              is.character(ids) || is_intish(ids))

  todo <- c("table", "ids")
  x[todo] <- mget(todo)

  complete_tbl_itm(x, callback, sub_var, ...)
}

#' @rdname data_items
#' @export
init_itm.hrd_itm <- function(x, table, sub_var, ids,
                             callback = "identity_callback", ...) {

  assert_that(is.string(table), has_length(ids),
              is.character(ids) || is_intish(ids))

  x[["table"]] <- table

  units <- load_id(as_src_tbl("variables", x), .data$id %in% .env$ids,
                   cols = "unit", id_var = "id")
  units <- rename_cols(rm_na(units), sub_var, "id")

  todo <- c("ids", "units")
  x[todo] <- mget(todo)

  complete_tbl_itm(x, callback, sub_var, ...)
}

#' @param unit_val String valued unit to be used in case no `unit_var` is
#' available for the given table
#'
#' @rdname data_items
#' @export
init_itm.col_itm <- function(x, table, unit_val = NULL,
                             callback = "identity_callback", ...) {

  assert_that(is.string(table), null_or(unit_val, is.string))

  todo <- c("table", "unit_val")
  x[todo] <- mget(todo)

  complete_tbl_itm(x, callback, FALSE, ...)
}

#' @param regex String-valued regular expression which will be evaluated by
#' [base::grepl()] with `ignore.case = TRUE`
#'
#' @rdname data_items
#' @export
init_itm.rgx_itm <- function(x, table, sub_var, regex,
                             callback = "identity_callback", ...) {

  assert_that(is.string(table), is.string(regex))

  todo <- c("table", "regex")
  x[todo] <- mget(todo)

  complete_tbl_itm(x, callback, sub_var, ...)
}

complete_tbl_itm <- function(x, callback, sub_var, id_var = NULL,
                             index_var = NULL, ...) {

  res <- set_callback(x, callback)
  res <- try_add_vars(res, sub_var = sub_var, ...)
  res <- try_add_vars(res, val_var = TRUE)

  tbl <- as_src_tbl(x)

  res[["id_var"]]    <- coalesce(id_var,    id_var(tbl))
  res[["index_var"]] <- coalesce(index_var, index_var(tbl))

  res
}

#' @rdname data_items
#' @export
init_itm.fun_itm <- function(x, callback, ...) {
  init_itm.itm(set_callback(x, callback), ...)
}

#' @rdname data_items
#' @export
init_itm.itm <- function(x, ...) {

  dots <- list(...)

  assert_that(is_disjoint(names(x), names(dots)))

  x[names(dots)] <- dots

  x
}

#' @export
init_itm.default <- function(x, ...) stop_generic(x, .Generic)

#' Internal utilities for `item`/`concept` objects
#'
#' Several internal utilities for modifying and querying item and concept
#' objects, including getters and setters for `itm` variables, callback
#' functions, `cncpt` target classes, as well as utilities for data loading
#' such as `prepare_query()` which creates a row-subsetting expression,
#' `do_callback()`, which applies a callback function to data or
#' `do_itm_load()`, which performs data loading corresponding to an `itm`.
#'
#' @param x Object defining the row-subsetting
#'
#' @return
#' * `prepare_query()`: an unevaluated expression used for row-subsetting
#' * `try_add_vars()`: a (potentially) modified item object with added
#'    variables
#' * `get_itm_var()`: character vector of `itm` variables
#' * `set_callback()`: a modified object with added callback function
#' * `do_callback()`: result of the callback function applied to data, most
#'    likely (`id_tbl`/`ts_tbl`)
#' * `do_itm_load()`: result of item loading (`id_tbl`/`ts_tbl`)
#' * `n_tick()`: Integer valued number of progress bar ticks
#' * `set_target()`: a modified object with newly set target class
#' * `get_target()`: string valued target class of an object
#'
#' @rdname item_utils
#' @keywords internal
#' @export
prepare_query <- function(x) UseMethod("prepare_query", x)

prep_sel <- function(x) {

  ids <- x[["ids"]]
  lst <- list(col = as.name(get_itm_var(x, "sub_var")), id = ids)

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
prepare_query.sel_itm <- prep_sel

#' @keywords internal
#' @export
prepare_query.hrd_itm <- prep_sel

#' @keywords internal
#' @export
prepare_query.rgx_itm <- function(x) {
  substitute(grepl(rgx, col, ignore.case = TRUE),
    list(col = as.name(get_itm_var(x, "sub_var")), rgx = x[["regex"]])
  )
}

#' @keywords internal
#' @export
prepare_query.col_itm <- function(x) rlang::quo(NULL)

#' @export
prepare_query.default <- function(x) stop_generic(x, .Generic)

#' @param ... Variable specification
#'
#' @rdname item_utils
#' @keywords internal
#' @export
try_add_vars <- function(x, ...) {

  if (...length() == 0L) {
    return(x)
  }

  UseMethod("try_add_vars", x)
}

#' @keywords internal
#' @export
try_add_vars.itm <- function(x, ...) {

  vars <- list(...)
  nmes <- names(vars)

  assert_that(same_length(nmes, vars), is_unique(nmes))

  for (var in nmes) {

    cur <- vars[[var]]

    if (isFALSE(cur)) {

      x[["vars"]] <- x[["vars"]][setdiff(names(x[["vars"]]), cur)]

    } else {

      if (isTRUE(cur)) {
        if (is.null(tbl_name(x))) next
        cur <- as_col_cfg(x)[[var]]
      }

      old <- if (has_name(x[["vars"]], var)) x[["vars"]][[var]] else NULL
      cur <- coalesce(old, cur)

      if (is.null(cur)) next

      assert_that(is.string(cur))

      x[["vars"]][[var]] <- cur
    }
  }

  if (has_length(x[["vars"]])) {
    assert_that(is_unique(unlist(x[["vars"]])))
    x[["vars"]] <- x[["vars"]][order(names(x[["vars"]]))]
  }

  x
}

#' @keywords internal
#' @export
try_add_vars.item <- function(x, ...) {

  vars <- list(...)

  for (i in names(vars)) {
    vars[[i]] <- vec_recycle(
      if (is.null(vars[[i]])) list(NULL) else vars[[i]], length(x), x_arg = i
    )
  }

  new_item(do.call(Map, c(list(try_add_vars, x), vars)))
}

#' @keywords internal
#' @export
try_add_vars.cncpt <- function(x, ...) {
  x[["items"]] <- try_add_vars(x[["items"]], ...)
  x
}

#' @keywords internal
#' @export
try_add_vars.rec_cncpt <- function(x, ...) x

#' @export
try_add_vars.default <- function(x, ...) stop_generic(x, .Generic)

#' @param ... Variable name
#'
#' @rdname item_utils
#' @keywords internal
#' @export
get_itm_var <- function(x, var = NULL) UseMethod("get_itm_var", x)

#' @keywords internal
#' @export
get_itm_var.itm <- function(x, var = NULL) {

  res <- x[["vars"]]

  if (is.null(var)) {

    res

  } else {

    if (has_name(res, var)) {
      res[[var]]
    } else {
      NULL
    }
  }
}

#' @export
get_itm_var.default <- function(x, ...) stop_generic(x, .Generic)

#' @param fun Callback function (passed as string)
#'
#' @rdname item_utils
#' @keywords internal
#' @export
set_callback <- function(x, fun) UseMethod("set_callback", x)

#' @keywords internal
#' @export
set_callback.itm <- function(x, fun) {

  assert_that(evals_to_fun(fun))

  x[["callback"]] <- fun

  x
}

#' @export
set_callback.default <- function(x, ...) stop_generic(x, .Generic)

str_to_fun <- function(x) {
  res <- eval(parse(text = x))
  assert_that(is.function(res))
  res
}

#' @rdname item_utils
#' @keywords internal
#' @export
do_callback <- function(x, ...) UseMethod("do_callback", x)

#' @keywords internal
#' @export
do_callback.itm <- function(x, ...) {

  fun <- str_to_fun(x[["callback"]])
  var <- x[["vars"]]
  env <- as_src_env(x)

  (function(x) {

    res <- do.call(fun, c(list(x), var, list(env = env)))

    res <- rename_cols(res, names(var), unlst(var), skip_absent = TRUE,
                       by_ref = TRUE)
    res

  })(...)
}

#' @keywords internal
#' @export
do_callback.hrd_itm <- function(x, ...) {

  if (is.null(get_itm_var(x, "unit_var"))) {
    x <- try_add_vars(x, unit_var = "unit")
  }

  NextMethod()
}

#' @keywords internal
#' @export
do_callback.col_itm <- function(x, ...) {

  if (is.null(get_itm_var(x, "unit_var")) && not_null(x[["unit_val"]])) {
    x <- try_add_vars(x, unit_var = "unit")
  }

  NextMethod()
}

#' @keywords internal
#' @export
do_callback.fun_itm <- function(x, ...) identity_callback(...)

#' @keywords internal
#' @export
do_callback.rec_cncpt <- function(x, lst, ...) {
  do.call(str_to_fun(x[["callback"]]), c(lst, list(...)))
}

#' @export
do_callback.default <- function(x, ...) stop_generic(x, .Generic)

#' @inheritParams load_concepts
#' @rdname item_utils
#' @keywords internal
#' @export
do_itm_load <- function(x, id_type = "icustay", interval = hours(1L)) {
  UseMethod("do_itm_load", x)
}

#' @export
do_itm_load.itm <- function(x, id_type = "icustay", interval = hours(1L)) {

  trg <- get_target(x)
  fun <- switch(trg, id_tbl = load_id, ts_tbl = load_ts,
                stop_ricu("Cannot load object with target class {trg}"))

  fun(x, id_var = id_var(as_id_cfg(x)[id_type]), interval = interval)
}

#' @export
do_itm_load.hrd_itm <- function(x, id_type = "icustay", interval = hours(1L)) {

  res <- NextMethod()

  if (is.null(get_itm_var(x, "unit_var"))) {
    unt <- x[["units"]]
    res <- merge(res, unt, by = get_itm_var(x, "sub_var"), all.x = TRUE)
  }

  res
}

#' @export
do_itm_load.col_itm <- function(x, id_type = "icustay", interval = hours(1L)) {

  res <- NextMethod()

  if (is.null(get_itm_var(x, "unit_var"))) {

    unt <- x[["unit_val"]]

    if (not_null(unt)) {
      res <- res[, c("unit") := unt]
    }
  }

  res
}

#' @export
do_itm_load.fun_itm <- function(x, id_type = "icustay", interval = hours(1L)) {
  str_to_fun(x[["callback"]])(x, id_type = id_type, interval = interval)
}

#' @export
do_itm_load.default <- function(x, ...) stop_generic(x, .Generic)

#' @export
id_vars.itm <- function(x) {
  coalesce(x[["id_var"]], id_vars(as_src_tbl(x)))
}

#' @export
index_var.itm <- function(x) {
  coalesce(x[["index_var"]], index_var(as_src_tbl(x)))
}

#' @export
meta_vars.itm <- function(x) c(id_vars(x), index_var(x))

#' @export
dimnames.itm <- function(x) list(NULL, unlst(x[["vars"]]))

#' @rdname data_items
#' @export
new_item <- function(x) {

  if (is_itm(x)) {
    return(as_item(x))
  }

  assert_that(is.list(x), all_fun(x, is_itm))

  new_vctr(x, class = "item")
}

#' @rdname data_items
#' @export
item <- function(...) {
  new_item(do.call(map, c(list(new_itm), vec_recycle_common(...))))
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
as_item.cncpt <- function(x) x[["items"]]

#' @export
as_item.concept <- function(x) do.call(c, unname(lapply(x, as_item)))

#' @export
as_item.default <- function(x) stop_generic(x, .Generic)

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
is_item <- is_type("item")

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

#' @export
n_tick.default <- function(x) stop_generic(x, .Generic)

#' @rdname item_utils
#' @keywords internal
#' @export
set_target <- function(x, target) UseMethod("set_target", x)

#' @export
set_target.item <- function(x, target) new_item(lapply(x, set_target, target))

#' @export
set_target.itm <- function(x, target) {

  assert_that(is.string(target))

  curr <- get_target(x)

  if (is.null(curr) || is.na(curr)) {
    x[["target"]] <- target
  }

  x
}

set_target.default <- function(x, target) stop_generic(x, .Generic)

#' @rdname item_utils
#' @keywords internal
#' @export
get_target <- function(x) UseMethod("get_target", x)

#' @export
get_target.cncpt <- function(x) x[["target"]]

#' @export
get_target.itm <- function(x) x[["target"]]

#' @export
get_target.default <- function(x) stop_generic(x, .Generic)

#' Data Concepts
#'
#' Concept objects are used in `ricu` as a way to specify how a clinical
#' concept, such as heart rate can be loaded from a data source and are mainly
#' consumed by [load_concepts()]. Several functions are available for
#' constructing `concept` (and related auxiliary) objects either from code or
#' by parsing a JSON formatted concept dictionary using [load_dictionary()].
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
#' The design choice for wrapping a vector of `cncpt` objects with a container
#' class `concept` is motivated by the requirement of having several different
#' sub-types of `cncpt` objects (all inheriting from the parent type `cncpt`),
#' while retaining control over how this homogeneous w.r.t. parent type, but
#' heterogeneous w.r.t. sub-type vector of objects behaves in terms of S3
#' generic functions.
#'
#' Each individual `cncpt` object contains the following information: a string-
#' valued name, an [`item`][new_itm()] vector containing [`itm`][new_itm()]
#' objects, a string-valued description (can be missing), a string-valued
#' category designation (can be missing), a character vector-valued
#' specification for an aggregation function and a target class specification
#' (e.g. [`id_tbl`][id_tbl()] or [`ts_tbl`][id_tbl()]). Additionally, a sub-
#' class to `cncpt` has to be specified, each representing a different
#' data-scenario and holding further class-specific information. The following
#' sub-classes to `cncpt` are available:
#'
#' * `num_cncpt`: The most widely used concept type is indented for concepts
#'   representing numerical measurements. Additional information that can be
#'   specified includes a string-valued unit specification, alongside a
#'   plausible range which can be used during data loading.
#' * `fct_cncpt`: In case of categorical concepts, such as `sex`, a set of
#'   factor levels can be specified, against which the loaded data is checked.
#' * `lgl_cncpt`: A special case of `fct_cncpt`, this allows only for logical
#'   values (`TRUE`, `FALSE` and `NA`).
#' * `rec_cncpt`: More involved concepts, such as a [SOFA score][sofa_score()]
#'   can pull in other concepts. Recursive concepts can build on other
#'   recursive concepts up to arbitrary recursion depth. Owing to the more
#'   complicated nature of such concepts, a `callback` function can be
#'   specified which is used in data loading for concept-specific post-
#'   processing steps.
#'
#' Class instantiation is organized in the same fashion as for
#' [`item`][new_itm()] objects: `concept()` maps vector-valued arguments
#' to `new_cncpt()`, which internally calls the S3 generic function
#' `init_cncpt()`, while `new_concept()` instantiates a `concept` object from
#' a list of `cncpt` objects (created by calls to `new_cncpt()`). Coercion is
#' only possible from `list` and `cncpt`, by calling `as_concept()` and
#' inheritance can be checked using `is_concept()` or `is_cncpt()`.
#'
#' @param name The name of the concept
#' @param items Zero or more `itm` objects
#' @param description String-valued concept description
#' @param category String-valued category
#' @param aggregate NULL or a string denoting a function used to aggregate per
#' id and if applicable per time step
#' @param ... Further specification of the `cncpt` object (passed to
#' [init_cncpt()])
#' @param target The target object yielded by loading
#' @param class `NULL` or a string-valued sub-class name used for customizing
#' concept behavior
#'
#' @rdname data_concepts
#'
#' @return Constructors and coercion functions return `cncpt` and `concept`
#' objects, while inheritance tester functions return logical flags.
#'
#' @examples
#' if (require(mimic.demo)) {
#' gluc <- concept("glu",
#'   item("mimic_demo", "labevents", "itemid", list(c(50809L, 50931L))),
#'   description = "glucose", category = "chemistry",
#'   unit = "mg/dL", min = 0, max = 1000
#' )
#'
#' is_concept(gluc)
#'
#' identical(gluc, load_dictionary("mimic_demo", "glu"))
#'
#' gl1 <- new_cncpt("glu",
#'   item("mimic_demo", "labevents", "itemid", list(c(50809L, 50931L))),
#'   description = "glucose"
#' )
#'
#' is_cncpt(gl1)
#' is_concept(gl1)
#'
#' conc <- concept(c("glu", "lact"),
#'   list(
#'     item("mimic_demo", "labevents", "itemid", list(c(50809L, 50931L))),
#'     item("mimic_demo", "labevents", "itemid", 50813L)
#'   ),
#'   description = c("glucose", "lactate")
#' )
#'
#' conc
#'
#' identical(as_concept(gl1), conc[1L])
#' }
#'
#' @export
#'
new_cncpt <- function(name, items, description = NA_character_,
                      category = NA_character_, aggregate = NULL, ...,
                      target = "ts_tbl", class = "num_cncpt") {

  assert_that(is.string(name), null_or(class, is.string), is.string(target),
              is.string(description), is.string(category))

  if (!is_concept(items)) {
    items <- set_target(as_item(items), target)
  }

  res <- list(name = name, items = items, description = description,
              category = category, aggregate = aggregate, target = target)

  init_cncpt(structure(res, class = c(class, "cncpt")), ...)
}

#' @param x Object to query/dispatch on
#'
#' @rdname data_concepts
#' @export
is_cncpt <- is_type("cncpt")

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

  try_add_vars(x, unit_var = TRUE)
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
#' @param interval Time interval used for data loading; if NULL, the respective
#' interval passed as argument to [load_concepts()] is taken
#'
#' @rdname data_concepts
#' @export
init_cncpt.rec_cncpt <- function(x, callback = "identity_callback",
                                 interval = NULL, ...) {

  really_na <- function(x) not_null(x) && is.na(x)

  warn_dots(...)

  assert_that(is_concept(x[["items"]]), is.string(callback),
              null_or(interval, is.string))

  if (not_null(interval)) {
    interval <- as.difftime(interval)
  }

  agg <- rep_arg(x[["aggregate"]], names(x[["items"]]))
  agg[lgl_ply(agg, really_na)] <- list(NULL)

  x[["aggregate"]] <- agg

  todo <- c("callback", "interval")
  x[todo] <- mget(todo)

  x
}

#' @export
init_cncpt.default <- function(x, ...) stop_generic(x, .Generic)

#' @export
src_name.cncpt <- function(x) src_name(x[["items"]])

#' @importFrom stats aggregate
#' @export
aggregate.cncpt <- function(x, tbl, fun = NULL, ...) {

  fun <- coalesce(fun, x[["aggregate"]])

  if (!isFALSE(fun)) {
    tbl <- aggregate(tbl, fun)
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
is_concept <- is_type("concept")

#' @rdname data_concepts
#' @export
as_concept <- function(x) UseMethod("as_concept", x)

#' @export
as_concept.concept <- function(x) x

#' @export
as_concept.list <- function(x) new_concept(x)

#' @export
as_concept.cncpt <- function(x) new_concept(list(x))

#' @export
as_concept.default <- function(x) stop_generic(x, .Generic)

#' @export
format.concept <- function(x, ...) {

  desc <- lst_xtr(x, "description")
  desc[lgl_ply(desc, is.null)] <- ""

  paste0(desc, " <", chr_xtr(lapply(x, "class"), 1L), "[",
         int_ply(lst_xtr(x, "items"), length), "]>")
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
#' Data concepts can be specified in JSON format as a concept dictionary which
#' can be read and parsed into `concept`/`item` objects. Dictionary loading
#' can either be performed on the default included dictionary or on a user-
#' specified custom dictionary. Furthermore, a mechanism is provided for adding
#' concepts and/or data sources to the existing dictionary (see the Details
#' section).
#'
#' @details
#' A default dictionary is provided at
#'
#' ```
#' system.file(
#'   file.path("extdata", "config", "concept-dict.json"),
#'   package = "ricu"
#' )
#' ```
#'
#' and can be loaded in to an R session by calling
#' `get_config("concept-dict")`. The default dictionary can be extended by
#' adding a file `concept-dict.json` to the path specified by the environment
#' variable `RICU_CONFIG_PATH`. New concepts can be added to this file and
#' existing concepts can be extended (by adding new data sources).
#' Alternatively, `load_dictionary()` can be called on non-default
#' dictionaries using the `file` argument.
#'
#' In order to specify a concept as JSON object, for example the numeric
#' concept for glucose, is given by
#'
#' ```
#' {
#'   "glu": {
#'     "unit": "mg/dL",
#'     "min": 0,
#'     "max": 1000,
#'     "description": "glucose",
#'     "category": "chemistry",
#'     "sources": {
#'       "mimic_demo": [
#'         {
#'           "ids": [50809, 50931],
#'           "table": "labevents",
#'           "sub_var": "itemid"
#'         }
#'       ]
#'     }
#'   }
#' }
#' ```
#'
#' Using such a specification, constructors for [`cncpt`][new_cncpt()] and
#' [`itm`][new_itm()] objects are called either using default arguments or as
#' specified by the JSON object, with the above corresponding to a call like
#'
#' ```
#' concept(
#'   name = "glu",
#'   items = item(
#'     src = "mimic_demo", table = "labevents", sub_var = "itemid",
#'     ids = list(c(50809L, 50931L))
#'   ),
#'   description = "glucose", category = "chemistry",
#'   unit = "mg/dL", min = 0, max = 1000
#' )
#' ```
#'
#' The arguments `src` and `concepts` can be used to only load a subset of a
#' dictionary by specifying a character vector of data sources and/or concept
#' names.
#'
#' @param src `NULL` or the name of one or several data sources
#' @param concepts A character vector used to subset the concept dictionary or
#' `NULL` indicating no subsetting
#' @param name Name of the dictionary to be read
#' @param cfg_dirs File name of the dictionary
#'
#' @rdname concept_dictionary
#'
#' @return A `concept` object containing several data concepts as `cncpt`
#' objects.
#'
#' @examples
#' if (require(mimic.demo)) {
#' head(load_dictionary("mimic_demo"))
#' load_dictionary("mimic_demo", c("glu", "lact"))
#' }
#'
#' @export
load_dictionary <- function(src = NULL, concepts = NULL,
                            name = "concept-dict", cfg_dirs = NULL) {

  avail <- src_data_avail()
  avail <- setNames(avail[["available"]], avail[["name"]])

  if (is.null(src)) {
    src <- names(avail[avail])
  }

  assert_that(are_in(src, names(avail)), all(avail[src]))

  parse_dictionary(read_dictionary(name, cfg_dirs), src, concepts)
}

read_dictionary <- function(name = "data-sources", cfg_dirs = NULL) {

  combine_sources <- function(x, y) {

    assert_that(
      !identical(x[["class"]], "rec_cncpt"), not_null(names(y[["sources"]])),
      length(y) == 1L, has_name(y, "sources"), is.list(y[["sources"]])
    )

    new_sources    <- c(y[["sources"]], x[["sources"]])
    x[["sources"]] <- new_sources[!duplicated(names(new_sources))]

    x
  }

  combine_concepts <- function(x, y) {

    assert_that(is.list(y), not_null(names(y)))

    if (is.null(x)) {
      return(y)
    } else if (is.null(y)) {
      return(x)
    }

    dups <- intersect(names(x), names(y))

    if (has_length(dups)) {
      x[dups] <- map(combine_sources, x[dups], y[dups])
      y[dups] <- NULL
    }

    c(x, y)
  }

  get_config(name, unique(c(rev(config_paths()), cfg_dirs)), combine_concepts)
}

parse_dictionary <- function(dict, src = NULL, concepts = NULL) {

  do_itm <- function(sr, x) {
    res <- lapply(x, c, src = sr)
    lapply(res, do_call, new_itm)
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

      itms <- do.call(c, Map(do_itm, names(sources), sources))
      itms <- new_item(itms)
    }

    cncpt_info <- list(name = name, items = itms, target = target)

    if (has_length(lst)) {

      if ("rec_cncpt" %in% lst[["class"]]) {

        do.call(new_cncpt, c(cncpt_info, lst))

      } else {
        do.call(new_cncpt, c(cncpt_info, lst))
      }
    } else {
      do.call(new_cncpt, c(cncpt_info, list(class = NULL)))
    }
  }

  do_new <- function(sel, ful) {

    if (is.null(sel)) {
      return(sel)
    }

    assert_that(are_in(sel, names(ful)))

    sub <- ful[sel]

    is_rec <- lgl_ply(lst_xtr(sub, "class"), identical, "rec_cncpt")

    if (any(is_rec)) {
      re_con      <- lapply(lst_xtr(sub, "concepts"), do_new, ful)
      sub[is_rec] <- Map(`[[<-`, sub[is_rec], "sources", re_con[is_rec])
    }

    res <- lapply(Map(c, name = names(sub), sub), do_call, do_cncpt)
    new_concept(res)
  }

  assert_that(null_or(src, is.character))

  if (is.null(concepts)) {
    concepts <- names(dict)
  }

  do_new(concepts, dict)
}

identity_callback <- function(x, ...) x
