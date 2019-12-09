
#' @export
ts_meta <- function(x, ...) UseMethod("ts_meta", x)

#' @export
meta_names <- function(x) UseMethod("meta_names", x)

#' @export
is_required <- function(x, ...) UseMethod("is_required", x)

#' @export
has_aux_names <- function(x) UseMethod("has_aux_names", x)

#' @export
aux_names <- function(x, ...) UseMethod("aux_names", x)

#' @export
has_aux_data <- function(x, ...) UseMethod("has_aux_data", x)

#' @export
aux_data <- function(x, ...) UseMethod("aux_data", x)

#' @export
as_ts_def <- function(x) UseMethod("as_ts_def", x)

#' @export
rename_cols <- function(x, new, old, ...) UseMethod("rename_cols", x)

#' @export
index <- function(x) UseMethod("index", x)

#' @export
key <- function(x) UseMethod("key", x)

#' @export
interval <- function(x) UseMethod("interval", x)

#' @export
ts_def <- function(x) UseMethod("ts_def", x)

#' @export
is_valid <- function(x, ...) UseMethod("is_valid", x)

#' @export
rm_cols <- function(x, cols, ...) UseMethod("rm_cols", x)

#' @export
is_unique <- function(x, ...) UseMethod("is_unique", x)
