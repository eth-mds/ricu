
srcs_avail <- function(x) all(ricu::is_data_avail(x))

quick_build <- function() {
  identical(Sys.getenv("RICU_VIGNETTE_QUICK_BUILD"), "true")
}

paste1 <- function(x) {

  x <- paste0("`", x, "`")

  if (length(x) <= 2L) {
    return(paste0(x, collapse = " and "))
  }

  paste0(paste0(head(x, n = length(x) - 2L), collapse = ", "), ", ",
         paste0(tail(x, n = 2L), collapse = " and "))
}

demo_instead_full_msg <- function(demo, full, file) {
  cat(
    "> Note: The following code blocks are run using demo (",
    paste1(demo), ") instead of full (", paste1(full), ") datasets and ",
    "therefore might be less useful. For data set up, please consult the ",
    "manual at `?attach_src`. The full version of this vignette is available ",
    "from [CRAN](https://CRAN.R-project.org/package=ricu/vignettes/", file,
    ").", sep = ""
  )
}

demo_missing_msg <- function(demo, file) {
  cat(
    "> Note: Code in this vignette requires that datasets ", paste1(demo),
    " are available, which can be installed by running\n",
    ">\n",
    "> ```{r}\n",
    "> install.packages(\n",
    ">   c(", paste0("\"", sub("_", ".", demo), "\"", collapse = ", "), "),\n",
    ">   repos = \"https://eth-mds.github.io/physionet-demo\"\n",
    "> )\n",
    "> ```\n",
    ">\n",
    "> As long as the corresponding datasets are not accessible, certain code ",
    "blocks are not evaluated. The full version of this vignette is available ",
    "from [CRAN](https://CRAN.R-project.org/package=ricu/vignettes/", file,
    ").", sep = ""
  )
}

if (requireNamespace("pillar", quietly = TRUE) &&
    utils::packageVersion("pillar") >= "1.9.0" &&
    utils::packageVersion("prt") < "0.2.0") {

  fix_print_fun <- function(x, n = 10, ...) {
    print(head(x, n = n))
  }

  registerS3method("print", "prt", fix_print_fun)
}
