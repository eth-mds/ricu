srcs_avail <- function(x) all(is_data_avail(x))

paste1 <- function(x) {

  x <- paste0("`", x, "`")

  if (length(x) <= 2L) {
    return(paste0(x, collapse = " and "))
  }

  paste0(paste0(head(x, n = length(x) - 2L), collapse = ", "), ", ",
         paste0(tail(x, n = 2L), collapse = " and "))
}

demo_instead_full_msg <- function(demo, full) {
  paste0(
    "> Note: The following code blocks are run using demo (",
    paste1(demo), ", ) instead of full datasets (", paste1(full), ", ) and ",
    "therefore might be less useful. For data set up, please consult the ",
    "manual at `?attach_src`. The full version of this vignette is available ",
    "from [CRAN](https://CRAN.R-project.org/package=ricu/vignettes/uom.html)."
  )
}

demo_missing_msg <- function(demo) {
  paste0(
    "> Note: Code in this vignette requires that datasets ", paste1(demo),
    " are available, which can be installed by running\n",
    ">\n",
    ">     install.packages(\n",
    ">       c(", paste0("\"", sub("_", ".", demo), "\"", collapse = ", "),
             "),\n",
    ">       repos = \"https://eth-mds.github.io/physionet-demo\"\n",
    ">     )\n",
    ">\n",
    "> As long as the corresponding datasets are not accessible, certain code ",
    "blocks are not evaluated. The full version of this vignette is available ",
    "from [CRAN](https://CRAN.R-project.org/package=ricu/vignettes/uom.html)."
  )
}

assign_dataset_names <- function(srcs) {
  name <- deparse(substitute(srcs))
  paste0(
    "```{r, assign-", name, ", eval = FALSE}\n",
    name, " <- c(", paste0("\"", srcs, "\"", collapse = ", "), ")\n",
    "```"
  )
}

demo <- c("mimic_demo", "eicu_demo")
srcs <- c("mimic", "eicu", "aumc", "hirid", "miiv")
