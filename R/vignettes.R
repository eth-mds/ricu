
on_cran <- function() !identical(Sys.getenv("NOT_CRAN"), "true")

create_cache_name <- function(base_dir, file_name) {
  paste0(file.path(base_dir, "extdata", "vignettes", file_name), ".rds")
}

#' Internal utilities for data loading in vignettes
#'
#' @param prefix Object defining the row-subsetting
#'
#' @rdname vignette
#' @keywords internal
#' @export
cached_concept_loader <- function(prefix) {

  assert_that(is.string(prefix))

  function(concepts, src, ...) {

    if (missing(src)) {
      file_name <- paste(prefix, unique(unlist(src_name(concepts))), sep = "-")
    } else {
      file_name <- paste(prefix, src, sep = "-")
    }

    cached_file <- create_cache_name(system.file(package = "ricu"), file_name)

    if (file.exists(cached_file) && on_cran()) {
      return(readRDS(cached_file))
    }

    if (missing(src)) {
      res <- load_concepts(concepts, ...)
    } else {
      res <- load_concepts(concepts, src, ...)
    }

    dst <- create_cache_name(file.path("..", "inst"), file_name)

    if (!dir.exists(dirname(dst))) {
      dir.create(dirname(dst), recursive = TRUE)
    }

    saveRDS(res, dst, version = 2L)

    res
  }
}
