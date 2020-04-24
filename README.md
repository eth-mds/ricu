
<!-- README.md is generated from README.Rmd. Please edit that file -->
[ricu](https://septic-tank.github.io/ricu/)
===========================================

<!-- badges: start -->
[![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![R build status](https://github.com/septic-tank/ricu/workflows/build/badge.svg)](https://github.com/septic-tank/ricu/actions) [![Codecov test coverage](https://codecov.io/gh/septic-tank/ricu/branch/master/graph/badge.svg?token=HvOM3yosW3)](https://codecov.io/gh/septic-tank/ricu) <!-- badges: end -->

Working with ICU datasets, especially with publicly available ones as provided by [PhysioNet](https://physionet.org) in R is facilitated by `ricu`, which provides data access, a level of abstraction to encode clinical concepts in a data source agnostic way, as well as classes and utilities for working with the arising types of time series datasets.

Installation
------------

Currently, installation is only possible from github directly, using the `remotes` if installed

``` r
remotes::install_github("septic-tank/ricu")
```

or by sourcing the required code for installation from github by running

``` r
rem <- source(
  paste0("https://raw.githubusercontent.com/r-lib/remotes/master/",
         "install-github.R"
)
rem$value("septic-tank/ricu")
```

In order to make sure that some useful utility packages are installed as well, consider installing the packages marked as `Suggests` as well by running

``` r
remotes::install_github("septic-tank/ricu", dependencies = TRUE)
```

instead, or by installing some of the utility packages (relevant for downloading and preprocessing PhysioNet datasets)

``` r
install.packages(c("getPass", "keyring", "openssl", "xml2"))
```

and demo dataset packages

``` r
install.packages(c("mimic.demo", "eicu.demo"),
                 repos = "https://septic-tank.github.io/physionet-demo")
```

explicitly.

Data access
-----------

Out of the box (provided the two data packages `mimic.demo` and `eicu.demo` are available), `ricu` provides access to the demo datasets corresponding to the PhysioNet Clinical Databases eICU and MIMIC-III. Tables are available as

``` r
mimic_demo$admissions
#> # A prt:        129 × 19
#> # Partitioning: [129] rows
#>     row_id subject_id hadm_id admittime           dischtime
#>      <int>      <int>   <int> <dttm>              <dttm>
#> 1    12258      10006  142345 2164-10-23 21:09:00 2164-11-01 17:15:00
#> 2    12263      10011  105331 2126-08-14 22:32:00 2126-08-28 18:59:00
#> 3    12265      10013  165520 2125-10-04 23:36:00 2125-10-07 15:13:00
#> 4    12269      10017  199207 2149-05-26 17:19:00 2149-06-03 18:42:00
#> 5    12270      10019  177759 2163-05-14 20:43:00 2163-05-15 12:00:00
#> …
#> 125  41055      44083  198330 2112-05-28 15:45:00 2112-06-07 16:50:00
#> 126  41070      44154  174245 2178-05-14 20:29:00 2178-05-15 09:45:00
#> 127  41087      44212  163189 2123-11-24 14:14:00 2123-12-30 14:31:00
#> 128  41090      44222  192189 2180-07-19 06:55:00 2180-07-20 13:00:00
#> 129  41092      44228  103379 2170-12-15 03:14:00 2170-12-24 18:00:00
#> # … with 119 more rows, and 14 more variables: deathtime <dttm>,
#> #   admission_type <chr>, admission_location <chr>, discharge_location <chr>,
#> #   insurance <chr>, language <chr>, religion <chr>, marital_status <chr>,
#> #   ethnicity <chr>, edregtime <dttm>, edouttime <dttm>, diagnosis <chr>,
#> #   hospital_expire_flag <int>, has_chartevents_data <int>
```

and data can be loaded into an R session for example using

``` r
data_ts("mimic_demo", "labevents", itemid == 50862L, c("valuenum", "valueuom"))
```

<PRE class="fansi fansi-output"><CODE>#&gt; # A `ts_tbl`: 299 × 4
#&gt; # Id:         `icustay_id` (patient &lt; hadm &lt; <span style='text-decoration: underline;'>icustay</span><span>)
#&gt; # Index:      `charttime` (1 hours)
#&gt;     icustay_id charttime valuenum valueuom
#&gt;          &lt;int&gt; &lt;drtn&gt;       &lt;dbl&gt; &lt;chr&gt;
#&gt; 1       201006   1 hours      2.4 g/dL
#&gt; 2       203766 -19 hours      2   g/dL
#&gt; 3       203766   4 hours      1.7 g/dL
#&gt; 4       204132   7 hours      3.6 g/dL
#&gt; 5       204201   9 hours      2.3 g/dL
#&gt; …
#&gt; 295     298685 130 hours      1.9 g/dL
#&gt; 296     298685 154 hours      2   g/dL
#&gt; 297     298685 203 hours      2   g/dL
#&gt; 298     298685 273 hours      2.2 g/dL
#&gt; 299     298685 299 hours      2.5 g/dL
#&gt; # … with 289 more rows
</span></CODE></PRE>
which returns time series data as `ts_tbl` object.
