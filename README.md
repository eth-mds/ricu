<!-- README.md is generated from README.Rmd. Please edit that file -->

# [ricu](https://eth-mds.github.io/ricu/)

<!-- badges: start -->

[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN](https://www.r-pkg.org/badges/version/ricu)](https://cran.r-project.org/package=ricu)
[![R build
status](https://github.com/eth-mds/ricu/workflows/check/badge.svg)](https://github.com/eth-mds/ricu/actions?query=workflow%3Acheck)
[![pkgdown build
status](https://github.com/eth-mds/ricu/workflows/pkgdown/badge.svg)](https://github.com/eth-mds/ricu/actions?query=workflow%3Apkgdown)
[![covr
status](https://github.com/eth-mds/ricu/workflows/coverage/badge.svg)](https://github.com/eth-mds/ricu/actions?query=workflow%3Acoverage)
[![Codecov test
coverage](https://codecov.io/gh/eth-mds/ricu/branch/main/graph/badge.svg?token=gQisoami9F)](https://app.codecov.io/gh/eth-mds/ricu)
<!-- badges: end -->

Working with ICU datasets, especially with publicly available ones as
provided by [PhysioNet](https://physionet.org) in R is facilitated by
`ricu`, which provides data access, a level of abstraction to encode
clinical concepts in a data source agnostic way, as well as classes and
utilities for working with the arising types of time series datasets.

## Installation

Currently, installation is only possible from github directly, using the
`remotes` if installed

``` r
remotes::install_github("eth-mds/ricu")
```

or by sourcing the required code for installation from github by running

``` r
rem <- source(
  paste0("https://raw.githubusercontent.com/r-lib/remotes/main/",
         "install-github.R")
)
rem$value("eth-mds/ricu")
```

In order to make sure that some useful utility packages are installed as
well, consider installing the packages marked as `Suggests` as well by
running

``` r
remotes::install_github("eth-mds/ricu", dependencies = TRUE)
```

instead, or by installing some of the utility packages (relevant for
downloading and preprocessing PhysioNet datasets)

``` r
install.packages("xml2")
```

and demo dataset packages

``` r
install.packages(c("mimic.demo", "eicu.demo"),
                 repos = "https://eth-mds.github.io/physionet-demo")
```

explicitly.

## Data access

Out of the box (provided the two data packages `mimic.demo` and
`eicu.demo` are available), `ricu` provides access to the demo datasets
corresponding to the PhysioNet Clinical Databases eICU and MIMIC-III.
Tables are available as

``` r
mimic_demo$admissions
```

<PRE class="fansi fansi-output"><CODE>#&gt; <span style='color: #555555;'># &lt;mimic_tbl&gt;: [129 ✖ 19]</span>
#&gt; <span style='color: #555555;'># ID options:  subject_id (patient) &lt; hadm_id (hadm) &lt; icustay_id (icustay)</span>
#&gt; <span style='color: #555555;'># Defaults:    `admission_type` (val)</span>
#&gt; <span style='color: #555555;'># Time vars:   `admittime`, `dischtime`, `deathtime`, `edregtime`, `edouttime`</span>
#&gt;     row_id subject_id hadm_id admittime           dischtime
#&gt;      <span style='color: #555555; font-style: italic;'>&lt;int&gt;</span>      <span style='color: #555555; font-style: italic;'>&lt;int&gt;</span>   <span style='color: #555555; font-style: italic;'>&lt;int&gt;</span> <span style='color: #555555; font-style: italic;'>&lt;dttm&gt;</span>              <span style='color: #555555; font-style: italic;'>&lt;dttm&gt;</span>
#&gt; <span style='color: #555555;'>1  </span>  <span style='text-decoration: underline;'>12</span>258      <span style='text-decoration: underline;'>10</span>006  <span style='text-decoration: underline;'>142</span>345 2164-10-23 <span style='color: #555555;'>21:09:00</span> 2164-11-01 <span style='color: #555555;'>17:15:00</span>
#&gt; <span style='color: #555555;'>2  </span>  <span style='text-decoration: underline;'>12</span>263      <span style='text-decoration: underline;'>10</span>011  <span style='text-decoration: underline;'>105</span>331 2126-08-14 <span style='color: #555555;'>22:32:00</span> 2126-08-28 <span style='color: #555555;'>18:59:00</span>
#&gt; <span style='color: #555555;'>3  </span>  <span style='text-decoration: underline;'>12</span>265      <span style='text-decoration: underline;'>10</span>013  <span style='text-decoration: underline;'>165</span>520 2125-10-04 <span style='color: #555555;'>23:36:00</span> 2125-10-07 <span style='color: #555555;'>15:13:00</span>
#&gt; <span style='color: #555555;'>4  </span>  <span style='text-decoration: underline;'>12</span>269      <span style='text-decoration: underline;'>10</span>017  <span style='text-decoration: underline;'>199</span>207 2149-05-26 <span style='color: #555555;'>17:19:00</span> 2149-06-03 <span style='color: #555555;'>18:42:00</span>
#&gt; <span style='color: #555555;'>5  </span>  <span style='text-decoration: underline;'>12</span>270      <span style='text-decoration: underline;'>10</span>019  <span style='text-decoration: underline;'>177</span>759 2163-05-14 <span style='color: #555555;'>20:43:00</span> 2163-05-15 <span style='color: #555555;'>12:00:00</span>
#&gt; <span style='color: #555555;'>…</span>
#&gt; <span style='color: #555555;'>125</span>  <span style='text-decoration: underline;'>41</span>055      <span style='text-decoration: underline;'>44</span>083  <span style='text-decoration: underline;'>198</span>330 2112-05-28 <span style='color: #555555;'>15:45:00</span> 2112-06-07 <span style='color: #555555;'>16:50:00</span>
#&gt; <span style='color: #555555;'>126</span>  <span style='text-decoration: underline;'>41</span>070      <span style='text-decoration: underline;'>44</span>154  <span style='text-decoration: underline;'>174</span>245 2178-05-14 <span style='color: #555555;'>20:29:00</span> 2178-05-15 <span style='color: #555555;'>09:45:00</span>
#&gt; <span style='color: #555555;'>127</span>  <span style='text-decoration: underline;'>41</span>087      <span style='text-decoration: underline;'>44</span>212  <span style='text-decoration: underline;'>163</span>189 2123-11-24 <span style='color: #555555;'>14:14:00</span> 2123-12-30 <span style='color: #555555;'>14:31:00</span>
#&gt; <span style='color: #555555;'>128</span>  <span style='text-decoration: underline;'>41</span>090      <span style='text-decoration: underline;'>44</span>222  <span style='text-decoration: underline;'>192</span>189 2180-07-19 <span style='color: #555555;'>06:55:00</span> 2180-07-20 <span style='color: #555555;'>13:00:00</span>
#&gt; <span style='color: #555555;'>129</span>  <span style='text-decoration: underline;'>41</span>092      <span style='text-decoration: underline;'>44</span>228  <span style='text-decoration: underline;'>103</span>379 2170-12-15 <span style='color: #555555;'>03:14:00</span> 2170-12-24 <span style='color: #555555;'>18:00:00</span>
#&gt; <span style='color: #555555;'># ℹ 124 more rows</span>
#&gt; <span style='color: #555555;'># ℹ 14 more variables: deathtime &lt;dttm&gt;, admission_type &lt;chr&gt;,</span>
#&gt; <span style='color: #555555;'>#   admission_location &lt;chr&gt;, discharge_location &lt;chr&gt;, insurance &lt;chr&gt;,</span>
#&gt; <span style='color: #555555;'>#   language &lt;chr&gt;, religion &lt;chr&gt;, marital_status &lt;chr&gt;, ethnicity &lt;chr&gt;,</span>
#&gt; <span style='color: #555555;'>#   edregtime &lt;dttm&gt;, edouttime &lt;dttm&gt;, diagnosis &lt;chr&gt;,</span>
#&gt; <span style='color: #555555;'>#   hospital_expire_flag &lt;int&gt;, has_chartevents_data &lt;int&gt;</span>
</CODE></PRE>

and data can be loaded into an R session for example using

``` r
load_ts("labevents", "mimic_demo", itemid == 50862L,
        cols = c("valuenum", "valueuom"))
```

<PRE class="fansi fansi-output"><CODE>#&gt; <span style='color: #555555;'># A `ts_tbl`: 299 ✖ 4</span>
#&gt; <span style='color: #555555;'># Id var:     `icustay_id`</span>
#&gt; <span style='color: #555555;'># Index var:  `charttime` (1 hours)</span>
#&gt;     icustay_id charttime valuenum valueuom
#&gt;          <span style='color: #555555; font-style: italic;'>&lt;int&gt;</span> <span style='color: #555555; font-style: italic;'>&lt;drtn&gt;</span>       <span style='color: #555555; font-style: italic;'>&lt;dbl&gt;</span> <span style='color: #555555; font-style: italic;'>&lt;chr&gt;</span>
#&gt; <span style='color: #555555;'>1  </span>     <span style='text-decoration: underline;'>201</span>006   0 hours      2.4 g/dL
#&gt; <span style='color: #555555;'>2  </span>     <span style='text-decoration: underline;'>203</span>766 -18 hours      2   g/dL
#&gt; <span style='color: #555555;'>3  </span>     <span style='text-decoration: underline;'>203</span>766   4 hours      1.7 g/dL
#&gt; <span style='color: #555555;'>4  </span>     <span style='text-decoration: underline;'>204</span>132   7 hours      3.6 g/dL
#&gt; <span style='color: #555555;'>5  </span>     <span style='text-decoration: underline;'>204</span>201   9 hours      2.3 g/dL
#&gt; <span style='color: #555555;'>…</span>
#&gt; <span style='color: #555555;'>295</span>     <span style='text-decoration: underline;'>298</span>685 130 hours      1.9 g/dL
#&gt; <span style='color: #555555;'>296</span>     <span style='text-decoration: underline;'>298</span>685 154 hours      2   g/dL
#&gt; <span style='color: #555555;'>297</span>     <span style='text-decoration: underline;'>298</span>685 203 hours      2   g/dL
#&gt; <span style='color: #555555;'>298</span>     <span style='text-decoration: underline;'>298</span>685 272 hours      2.2 g/dL
#&gt; <span style='color: #555555;'>299</span>     <span style='text-decoration: underline;'>298</span>685 299 hours      2.5 g/dL
#&gt; <span style='color: #555555;'># ℹ 294 more rows</span>
</CODE></PRE>

which returns time series data as `ts_tbl` object.

## Acknowledgments

This work was supported by grant \#2017-110 of the Strategic Focal Area
“Personalized Health and Related Technologies (PHRT)” of the ETH Domain
for the SPHN/PHRT Driver Project “Personalized Swiss Sepsis Study”.
