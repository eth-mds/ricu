
#' ICU datasets
#'
#' The [Laboratory for Computational Physiology
#' ](https://lcp.mit.edu/index.html) (LCP) at MIT hosts several large-scale
#' databases of hospital intensive care units (ICUs), two of which can be
#' either downloaded in full ([MIMIC-III
#' ](https://physionet.org/content/mimiciii/1.4/) and [eICU
#' ](https://physionet.org/content/eicu-crd/2.0/)) or as demo subsets
#' ([MIMIC-III demo](https://physionet.org/content/mimiciii-demo/1.4/) and
#' [eICU demo](https://physionet.org/content/eicu-crd-demo/2.0/)), while a
#' third data set is available only in full ([HiRID
#' ](https://physionet.org/content/hirid/1.0/)). While demo data sets are
#' freely available, full download requires credentialed access which can be
#' gained by applying for an account with [PhysioNet
#' ](https://physionet.org/register/). Even though registration is required,
#' the described datasets are all publicly available. With [AmsterdamUMCdb
#' ](https://amsterdammedicaldatascience.nl/#amsterdamumcdb), a non-PhysioNet
#' hosted data source is available as well. As with the PhysioNet datasets,
#' access is public but has to be granted by the data collectors.
#'
#' @details
#' Setting up a dataset for use with `ricu` requires a configuration object.
#' For the included datasets, configuration can be loaded from
#'
#' ```
#' system.file("extdata", "config", "data-sources.json", package = "ricu")
#' ```
#'
#' by calling [load_src_cfg()] and for dataset that are external to `ricu`,
#' additional configuration can be made available by setting the environment
#' variable `RICU_CONFIG_PATH` (for more information, refer to
#' [load_src_cfg()]). Using the dataset configuration object, data can be
#' downloaded ([download_src()]), imported ([import_src()]) and attached
#' ([attach_src()]). While downloading and importing are one-time procedures,
#' attaching of the dataset is repeated every time the package is loaded.
#' Briefly, downloading loads the raw dataset from the internet (most likely
#' in `.csv` format), importing consists of some preprocessing to make the
#' data available more efficiently (by converting it to [`.fst`][fst::fst()]
#' format) and attaching sets up the data for use by the package. For more
#' information on the individual steps, refer to the respective documentation
#' pages.
#'
#' A dataset that has been successfully made available can interactively be
#' explored by typing its name into the console and individual tables can be
#' inspected using the `$` function. For example for the MIMIC-III demo
#' dataset and the `icustays` table, this gives
#'
#' ```{r, eval = is_data_avail("mimic_demo")}
#' mimic_demo
#' mimic_demo$icustays
#' ```
#'
#' Table subsets can be loaded into memory for example using the
#' [base::subset()] function, which uses non-standard evaluation (NSE) to
#' determine a row-subsetting. This design choice stems form the fact that
#' some tables can have on the order of 10^8 rows, which makes loading full
#' tables into memory an expensive operation. Table subsets loaded into
#' memory are represented as [`data.table`][data.table::data.table()] objects.
#' Extending the above example, if only ICU stays corresponding to the patient
#' with `subject_id == 10124` are of interest, the respective data can be
#' loaded as
#'
#' ```{r, eval = is_data_avail("mimic_demo")}
#' subset(mimic_demo$icustays, subject_id == 10124)
#' ```
#'
#' Much care has been taken to make `ricu` extensible to new datasets. For
#' example the publicly available ICU database [AmsterdamUMCdb
#' ](https://www.amsterdammedicaldatascience.nl/#amsterdamumcdb)
#' provided by the Amsterdam University Medical Center, currently is not part
#' of the core datasets of `ricu`, but code for integrating this dataset is
#' available on [github](https://github.com/septic-tank/aumc).
#'
#' @section MIMIC-III:
#' The Medical Information Mart for Intensive Care
#' ([MIMIC](https://physionet.org/content/mimiciii/)) database holds
#' detailed clinical data from roughly 60,000 patient stays in Beth Israel
#' Deaconess Medical Center (BIDMC) intensive care units between 2001 and 2012.
#' The database includes information such as demographics, vital sign
#' measurements made at the bedside (~1 data point per hour), laboratory test
#' results, procedures, medications, caregiver notes, imaging reports, and
#' mortality (both in and out of hospital). For further information, please
#' refer to the [MIMIC-III documentation
#' ](https://mimic.physionet.org/about/mimic/).
#'
#' The corresponding
#' [demo dataset](https://physionet.org/content/mimiciii-demo/)
#' contains the full data of a randomly selected subset of 100 patients from
#' the patient cohort with conformed in-hospital mortality. The only notable
#' data omission is the `noteevents` table, which contains unstructured text
#' reports on patients.
#'
#' @section eICU:
#' More recently, Philips Healthcare and LCP began assembling the [eICU
#' Collaborative Research Database
#' ](https://physionet.org/content/eicu-crd/2.0/) as a multi-center resource
#' for ICU data. Combining data of several critical care units throughout the
#' continental United States from the years 2014 and 2015, this database
#' contains de-identified health data associated with over 200,000 admissions,
#' including vital sign measurements, care plan documentation, severity of
#' illness measures, diagnosis information, and treatment information. For
#' further information, please refer to the [eICU documentation
#' ](https://eicu-crd.mit.edu/about/eicu/).
#'
#' For the [demo subset](https://physionet.org/content/eicu-crd-demo/2.0/),
#' data associated with ICU stays for over 2,500 unit stays selected from 20
#' of the larger hospitals is included. An important caveat that applied to the
#' eICU-based datasets is considerable variability among the large number of
#' hospitals in terms of data availability.
#'
#' @section HiRID:
#' Moving to higher time-resolution, [HiRID
#' ](https://physionet.org/content/hirid/1.0/) is a freely accessible critical
#' care dataset containing data relating to almost 34,000 patient admissions
#' to the Department of Intensive Care Medicine of the Bern University
#' Hospital, Switzerland. The dataset contains de-identified demographic
#' information and a total of 681 routinely collected physiological variables,
#' diagnostic test results and treatment parameters, collected during the
#' period from January 2008 to June 2016. Dependent on the type of measurement,
#' time resolution can be on the order of 2 minutes.
#'
#' @section AmsterdamUMCdb:
#' With similar time-resolution (for vital-sign measurements) as HiRID,
#' [AmsterdamUMCdb](https://amsterdammedicaldatascience.nl/#amsterdamumcdb)
#' contains data from 23,000 admissions of adult patients from 2003-2016 to
#' the department of Intensive Care, of Amsterdam University Medical Center.
#' In total, nearly 10^9^ individual observations consisting of vitals signs,
#' clinical scoring systems, device data and lab results data, as well as
#' nearly 5*10^6^ million medication entries, alongside de-identified
#' demographic information corresponding to the 20,000 individual patients
#' is spread over 7 tables.
#'
#' @format
#' The exported `data` environment contains all datasets that have been made
#' available to `ricu`. For datasets that are attached during package loading
#' (see [attach_src()]), shortcuts to the datasets are set up in the package
#' namespace, allowing the object `ricu::data::mimic_demo` to be accessed as
#' `ricu::mimic_demo` (or in case the package namespace has been attached,
#' simply as `mimic_demo`). Datasets that are made available after the package
#' namespace has been sealed will have their proxy object by default located
#' in `.GlobalEnv`. Datasets are represented by [`src_env`][new_src_env()]
#' objects, while individual tables are [`src_tbl`][new_src_tbl()] and do not
#' represent in-memory data, but rather data stored on disk, subsets of which
#' can be loaded into memory.
#'
#' @references
#' Johnson, A., Pollard, T., & Mark, R. (2016). MIMIC-III Clinical Database
#' (version 1.4). PhysioNet. https://doi.org/10.13026/C2XW26.
#'
#' MIMIC-III, a freely accessible critical care database. Johnson AEW, Pollard
#' TJ, Shen L, Lehman L, Feng M, Ghassemi M, Moody B, Szolovits P, Celi LA,
#' and Mark RG. Scientific Data (2016). DOI: 10.1038/sdata.2016.35.
#'
#' Johnson, A., Pollard, T., Badawi, O., & Raffa, J. (2019). eICU
#' Collaborative Research Database Demo (version 2.0). PhysioNet.
#' https://doi.org/10.13026/gxmm-es70.
#'
#' The eICU Collaborative Research Database, a freely available multi-center
#' database for critical care research. Pollard TJ, Johnson AEW, Raffa JD,
#' Celi LA, Mark RG and Badawi O. Scientific Data (2018). DOI:
#' http://dx.doi.org/10.1038/sdata.2018.178.
#'
#' Faltys, M., Zimmermann, M., Lyu, X., Hüser, M., Hyland, S., Rätsch, G., &
#' Merz, T. (2020). HiRID, a high time-resolution ICU dataset (version 1.0).
#' PhysioNet. https://doi.org/10.13026/hz5m-md48.
#'
#' Hyland, S.L., Faltys, M., Hüser, M. et al. Early prediction of circulatory
#' failure in the intensive care unit using machine learning. Nat Med 26,
#' 364–373 (2020). https://doi.org/10.1038/s41591-020-0789-4
#'
#' Thoral PJ, Peppink JM, Driessen RH, et al (2020) AmsterdamUMCdb: The First
#' Freely Accessible European Intensive Care Database from the ESICM Data
#' Sharing Initiative. https://www.amsterdammedicaldatascience.nl.
#'
#' Elbers, Dr. P.W.G. (Amsterdam UMC) (2019): AmsterdamUMCdb v1.0.2. DANS.
#' https://doi.org/10.17026/dans-22u-f8vd
#'
#' @encoding UTF-8
#' @rdname data_env
#' @export
data <- new.env()

#' @name mimic
#' @rdname data_env
NULL

#' @name mimic_demo
#' @rdname data_env
NULL

#' @name eicu
#' @rdname data_env
NULL

#' @name eicu_demo
#' @rdname data_env
NULL

#' @name hirid
#' @rdname data_env
NULL

#' @name aumc
#' @rdname data_env
NULL

data_env <- function() get("data", envir = pkg_env(), mode = "environment")

#' Data source environments
#'
#' Attaching a data source (see [attach_src()]) instantiates two types of S3
#' classes: a single `src_env` object, representing the data source as
#' collection of tables, as well as a `src_tbl` objects per table,
#' representing the given table. Upon package loading, `src_env` objects
#' including the respective `src_tbl` objects are created for all data sources
#' that are configured for auto-attaching, irrespective of whether data is
#' actually available. If some (or all) data is missing, the user is asked for
#' permission to download in interactive sessions and an error is thrown in
#' non-interactive sessions. See [setup_src_env()] for manually downloading
#' and setting up data sources.
#'
#' @details
#' A `src_env` object is an environment with attributes `src_name` (a
#' string-valued data source name, such as `mimic_demo`) and `id_cfg`
#' (describing the possible patient IDs for the given data source). In
#' addition to the `src_env` class attribute, sub-classes are defined by the
#' source `class_prefix` configuration setting (see [load_src_cfg()]). Such
#' data source environments are intended to contain several corresponding
#' `src_tbl` objects (or rather active bindings that evaluate to `src_tbl`
#' objects; see [setup_src_env()]).
#'
#' The S3 class `src_tbl` inherits from [`prt`][prt::new_prt()], which
#' represents a partitioned [`fst`][fst::fst()] file. In addition to the `prt`
#' object, meta data in the form of `col_cfg` and `tbl_cfg` is associated with
#' a `src_tbl` object (see [load_src_cfg()]). Furthermore, sub-classes are
#' added as specified by the source configuration `class_prefix` entry, as
#' with `src_env` objects. This allows certain functionality, for example data
#' loading, to be adapted to data source-specific requirements.
#'
#' Instantiation and set up of `src_env` objects is possible irrespectively of
#' whether the underlying data is available. If some (or all) data is missing,
#' the user is asked for permission to download in interactive sessions and an
#' error is thrown in non-interactive sessions upon first access of a
#' `src_tbl` bound as set up by [setup_src_env()]. Data availability can be
#' checked with the following utilities:
#'
#' * `is_tbl_avail()`: Returns a logical flag indicating whether all required
#'   data for the table passed as `tbl` which may be a string or any object
#'   that has a `tbl_name()` implementation is available from the environment
#'   `env` (requires an `as_src_env()` method).
#' * `src_tbl_avail()`: Returns a named logical vector, indicating which tables
#'   have all required data available. As above, both `tbls` (arbitrary
#'   length) and `env` (scalar-valued) may be character vectors or objects
#'   with corresponding `tbl_name()` and `as_src_env()` methods.
#' * `src_data_avail()`: The most comprehensive data availability report can
#'   be generated by calling `src_data_avail()`, returning a `data.frame` with
#'   columns `name` (the data source name), `available` (logical vector
#'   indicating whether all data is available), `tables` (the number of
#'   available tables) and `total` (the total number of tables). As input,
#'   `src` may be an arbitrary length character vector, an object for which an
#'   `as_src_env()` method is defined or an arbitrary-length list thereof.
#' * `is_data_avail()`: Returns a named logical vector, indicating for which
#'   data sources all required data is available. As above, `src` may be an
#'   arbitrary length character vector, an object for which an `as_src_env()`
#'   method is defined or an arbitrary-length list thereof.
#'
#' @param files File names of `fst` files that will be used to create a `prt`
#' object (see also [prt::new_prt()])
#' @param col_cfg Coerced to `col_cfg` by calling [as_col_cfg()]
#' @param tbl_cfg Coerced to `tbl_cfg` by calling [as_tbl_cfg()]
#' @param prefix Character vector valued data source name(s) (used as class
#' prefix)
#' @param src_env The data source environment (as `src_env` object)
#'
#' @return The constructors `new_src_env()`/`new_src_tbl()` as well as coercion
#' functions `as_src_env()`/`as_src_tbl()` return `src_env` and `src_tbl`
#' objects respectively, while inheritance testers `is_src_env()`/
#' `is_src_tbl()` return logical flags. For data availability utilities, see
#' Details section.
#'
#' @keywords internal
#' @rdname src_env
#' @export
new_src_tbl <- function(files, col_cfg, tbl_cfg, prefix, src_env) {

  assert_that(is_src_env(src_env))

  res <- prt::new_prt(files)
  class(res) <- c(paste0(c(prefix, "src"), "_tbl"), class(res))

  attr(res, "col_cfg") <- as_col_cfg(col_cfg)
  attr(res, "tbl_cfg") <- as_tbl_cfg(tbl_cfg)
  attr(res, "src_env") <- src_env

  res
}

#' @keywords internal
#' @rdname src_env
#' @export
is_src_tbl <- is_type("src_tbl")

#' @export
default_vars.src_tbl <- function(x, type) {

  res <- default_vars(as_col_cfg(x), type)

  assert_that(is.list(res), length(res) == 1L)

  res <- res[[1L]]

  if (not_null(res)) {
    assert_that(is.character(res), has_length(res), !anyNA(res))
  }

  res
}

#' @export
id_vars.src_tbl <- function(x) {
  coalesce(default_vars(x, "id_var"), id_vars(as_id_cfg(x)))
}

#' @export
index_var.src_tbl <- function(x) default_vars(x, "index_var")

#' @export
time_vars.src_tbl <- function(x) default_vars(x, "time_vars")

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.src_tbl <- function(x) {

  out <- c(setNames(dim_brak(x), paste0("<", class(x)[2L], ">")),
           Partitions = part_desc(x))

  ids <- id_var_opts(x)

  if (length(ids) > 1L && any(ids %in% colnames(x))) {
    out <- c(out, "ID options" = id_desc(x))
  }

  c(out, def_desc(x))
}

part_desc <- function(x) {

  part_nrow <- prt::part_nrow(x)

  if (length(part_nrow) > 1L) {
    paste0("[", concat(chr_ply(part_nrow, big_mark)), "] rows")
  } else {
    NULL
  }
}

id_desc <- function(x) {
  id <- as_id_cfg(x)
  paste0(id_var_opts(id), " (", names(id), ")", id_cfg_op(id), collapse = " ")
}

def_desc <- function(x) {

  var_def <- function(var, y) default_vars(y, var)
  cap_str <- function(x) paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))

  vars <- setdiff(default_var_names(x), "id_var")
  vars <- Filter(Negate(is.null), setNames(lapply(vars, var_def, x), vars))

  scal <- unlist(filter_vars(vars, "scalar"))
  vctr <- filter_vars(vars, "vector")

  if (has_length(scal)) {
    scal <- paste0(quote_bt(scal), " (", sub("_var$", "", names(scal)), ")",
                   collapse = ", ")
    scal <- list(Defaults = scal)
  }

  if (has_length(vctr)) {
    vctr <- lapply(vctr, quote_bt)
    vctr <- chr_ply(vctr, concat, use_names = TRUE)
    names(vctr) <- cap_str(sub("_", " ", names(vctr)))
  }

  c(scal, vctr)
}

#' @export
src_name.src_tbl <- function(x) src_name(as_col_cfg(x))

#' @export
tbl_name.src_tbl <- function(x) tbl_name(as_col_cfg(x))

#' @param x Object to test/coerce
#'
#' @keywords internal
#' @rdname src_env
#' @export
as_src_tbl <- function(x, ...) UseMethod("as_src_tbl", x)

#' @export
as_src_tbl.src_tbl <- function(x, ...) warn_dot_ident(x, ...)

#' @param tbl String-valued table name
#'
#' @keywords internal
#' @rdname src_env
#' @export
as_src_tbl.src_env <- function(x, tbl, ...) {

  warn_dots(...)

  assert_that(is.string(tbl))

  res <- get0(tbl, envir = x, inherits = FALSE, ifnotfound = NULL)

  if (is.null(res)) {

    opts <- ls(envir = x)

    stop_ricu(
      c("Table `{tbl}` not found for `{src_name(x)}`. Available are:",
        bullet(quote_bt(opts))),
      class = "src_tbl_not_found", exdent = c(0L, rep_along(2L, opts))
    )
  }

  res
}

#' @export
as_src_tbl.character <- function(x, env, ...) {
  as_src_tbl(as_src_env(env), x, ...)
}

#' @export
as_src_tbl.default <- function(x, ...) stop_generic(x, .Generic)

#' @param env Environment used as `src_env`
#' @param link `NULL` or a second environment (in addition to `data_env()`) in
#' which the resulting `src_env` is bound to a name
#'
#' @keywords internal
#' @rdname src_env
#' @export
new_src_env <- function(x, env = new.env(parent = data_env()), link = NULL) {

  assert_that(is_src_cfg(x), is.environment(env),
              null_or(link, is.environment))

  nme <- src_name(x)
  pfx <- src_prefix(x)

  if (not_null(link) && exists(nme, envir = link, inherits = FALSE)) {
    warn_ricu("Name `{nme}` already bound in environment {format(link)}",
              class = "src_name_bound_in_link_env")
    link <- NULL
  }

  res <- structure(env, class = paste0(c(pfx, "src"), "_env"),
                   src_name = nme, id_cfg = as_id_cfg(x), link = link,
                   extra = src_extra_cfg(x), prefix = pfx)

  if (not_null(link)) {
    assign(nme, res, envir = link)
  }

  assign(nme, res, envir = data_env())
}

#' @keywords internal
#' @rdname src_env
#' @export
is_src_env <- is_type("src_env")

#' @export
print.src_env <- function(x, ...) {
  cat_line("<", class(x)[1L], "[", length(x), "]>")
  print(setNames(format(x), names(x)), quote = FALSE)
  invisible(x)
}

#' @export
format.src_env <- function(x, ...) {
  chr_ply(lapply(ls(envir = x), safe_tbl_get, x), dim_brak)
}

#' @rdname src_env
#' @keywords internal
#' @method as.list src_env
#' @export
as.list.src_env <- function(x, ...) {
  mget(names(x), envir = x)
}

dim_str <- function(x) ifelse(is.na(x), "??", big_mark(x))

dim_desc <- function(x) {

  if (is.null(x)) {
    res <- rep(NA, 2L)
  } else if (is_df(x) || is_fst(x) || is_prt(x)) {
    res <- dim(x)
  } else {
    res <- list(n_row(x), n_col(x))
  }

  do_call(lapply(res, dim_str), paste, sep = paste0(" ", symbol$cross, " "))
}

dim_brak <- function(x) paste0("[", dim_desc(x), "]")

#' @importFrom utils ls.str
#' @export
str.src_env <- function(object, ...) ls.str(object)

#' @export
names.src_env <- function(x) ls(envir = x)

#' @export
src_name.src_env <- function(x) attr(x, "src_name")

#' @keywords internal
#' @rdname src_env
#' @export
as_src_env <- function(x) UseMethod("as_src_env", x)

#' @export
as_src_env.src_env <- function(x) x

#' @export
as_src_env.character <- function(x) {

  assert_that(is.string(x))

  env <- data_env()

  res <- get0(x, envir = env, mode = "environment", ifnotfound = NULL)

  if (is.null(res)) {
    stop_ricu("Source `{x}` not found in {format(env)}",
              class = "src_env_not_found")
  }

  res
}

#' @export
as_src_env.default <- function(x) as_src_env(src_name(x))

#' @export
as_src_env.src_tbl <- function(x) attr(x, "src_env")

safe_tbl_get <- function(x, env) {

  assert_that(is.string(x))

  env <- safe_src_get(env)

  if (is.null(env)) {
    return(NULL)
  }

  tryCatch(
    get0(x, envir = env, inherits = FALSE),
    miss_tbl_msg = function(msg) NULL
  )
}

safe_src_get <- function(src) {

  if (is.character(src)) {
    assert_that(is.string(src))
  } else {
    src <- src_name(src)
  }

  get0(src, envir = data_env(), mode = "environment", ifnotfound = NULL)
}

#' @keywords internal
#' @rdname src_env
#' @export
attached_srcs <- function() ls(envir = data_env())

#' @keywords internal
#' @rdname src_env
#' @export
is_tbl_avail <- function(tbl, env) is_src_tbl(safe_tbl_get(tbl, env))

#' @param tbls Character vector of table names
#'
#' @keywords internal
#' @rdname src_env
#' @export
src_tbl_avail <- function(env, tbls = ls(envir = env)) {

  env <- safe_src_get(env)

  if (is.null(env)) {
    return(NA)
  }

  if (!is.character(tbls)) {

    if (is.object(tbls)) {
      tbls <- list(tbls)
    }

    tbls <- chr_ply(tbls, tbl_name)
  }

  lgl_ply(tbls, is_tbl_avail, env, use_names = TRUE)
}

#' @param src Character vector of data source names or any other object (or
#' list thereof) for which an `as_src_env()` method exists
#'
#' @keywords internal
#' @rdname src_env
#' @export
src_data_avail <- function(src = auto_attach_srcs()) {

  num_non_na <- function(x) sum(x | !x)
  all_true   <- function(x) all(is_true(x))

  if (identical(length(src), 0L)) {
    return(NULL)
  }

  if (is.object(src)) {
    src <- list(src)
  }

  tbls <- lapply(src, src_tbl_avail)

  if (!is.character(src)) {
    src <- chr_ply(src, src_name)
  }

  data.frame(name = src, available = lgl_ply(tbls, all_true),
             tables = int_ply(tbls, sum, na.rm = TRUE),
             total = int_ply(tbls, num_non_na))
}

#' @keywords internal
#' @rdname src_env
#' @export
is_data_avail <- function(src = auto_attach_srcs()) {
  srcs <- src_data_avail(src)
  setNames(srcs$available, srcs$name)
}
