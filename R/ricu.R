
#' @keywords internal
#' @importFrom data.table setattr setcolorder set setnames setorderv
#' @importFrom data.table setDT setDF fifelse rbindlist data.table
#' @importFrom data.table := .SD .EACHI .I %chin% as.data.table copy
#' @importFrom data.table first last
#' @importFrom stats setNames na.omit median sd var
#' @importFrom utils str head tail
#' @importFrom cli symbol no qty
#' @importFrom rlang !!
#' @import vctrs
"_PACKAGE"

is_pkg_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

pkg_name <- function() methods::getPackageName()

pkg_env <- function() asNamespace(pkg_name())

release_questions <- function() {
  c(
    "Was `release()` called with `args = \"--compact-vignettes=gs+qpdf\"`?"
  )
}
