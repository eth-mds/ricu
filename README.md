
<!-- README.md is generated from README.Rmd. Please edit that file -->

# [ricu](https://septic-tank.github.io/ricu/)

<!-- badges: start -->

[![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R build
status](https://github.com/septic-tank/ricu/workflows/build/badge.svg)](https://github.com/septic-tank/ricu/actions?query=workflow%3Abuild)
[![R check
status](https://github.com/septic-tank/ricu/workflows/check/badge.svg)](https://github.com/septic-tank/ricu/actions?query=workflow%3Acheck)
[![pkgdown build
status](https://github.com/septic-tank/ricu/workflows/pkgdown/badge.svg)](https://github.com/septic-tank/ricu/actions?query=workflow%3Apkgdown)
[![covr
status](https://github.com/septic-tank/ricu/workflows/coverage/badge.svg)](https://github.com/septic-tank/ricu/actions?query=workflow%3Acoverage)
[![Codecov test
coverage](https://codecov.io/gh/septic-tank/ricu/branch/master/graph/badge.svg?token=HvOM3yosW3)](https://codecov.io/gh/septic-tank/ricu)
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

In order to make sure that some useful utility packages are installed as
well, consider installing the packages marked as `Suggests` as well by
running

``` r
remotes::install_github("septic-tank/ricu", dependencies = TRUE)
```

instead, or by installing some of the utility packages (relevant for
downloading and preprocessing PhysioNet datasets)

``` r
install.packages(c("getPass", "keyring", "openssl", "xml2"))
```

and demo dataset packages

``` r
install.packages(c("mimic.demo", "eicu.demo"),
                 repos = "https://septic-tank.github.io/physionet-demo")
```

explicitly.

## Data access

Out of the box (provided the two data packages `mimic.demo` and
`eicu.demo` are available), `ricu` provides access to the demo datasets
corresponding to the PhysioNet Clinical Databases eICU and MIMIC-III.
Tables are available as

``` r
print(mimic_demo$admissions)
```

<PRE class="fansi fansi-output"><CODE>#&gt; <span style='color: #949494;'># &lt;mimic_tbl&gt;: [129 ✖ 19]</span><span>
#&gt; </span><span style='color: #949494;'># ID options:  subject_id (patient) &lt; hadm_id (hadm) &lt; icustay_id (icustay)</span><span>
#&gt; </span><span style='color: #949494;'># Defaults:    admission_type (value)</span><span>
#&gt; </span><span style='color: #949494;'># Time vars:   admittime, dischtime, deathtime, edregtime, edouttime</span><span>
#&gt;     row_id subject_id hadm_id admittime           dischtime
#&gt;      </span><span style='color: #949494;font-style: italic;'>&lt;int&gt;</span><span>      </span><span style='color: #949494;font-style: italic;'>&lt;int&gt;</span><span>   </span><span style='color: #949494;font-style: italic;'>&lt;int&gt;</span><span> </span><span style='color: #949494;font-style: italic;'>&lt;dttm&gt;</span><span>              </span><span style='color: #949494;font-style: italic;'>&lt;dttm&gt;</span><span>
#&gt; </span><span style='color: #949494;'>1</span><span>    </span><span style='text-decoration: underline;'>12</span><span>258      </span><span style='text-decoration: underline;'>10</span><span>006  </span><span style='text-decoration: underline;'>142</span><span>345 2164-10-23 </span><span style='color: #949494;'>21:09:00</span><span> 2164-11-01 </span><span style='color: #949494;'>17:15:00</span><span>
#&gt; </span><span style='color: #949494;'>2</span><span>    </span><span style='text-decoration: underline;'>12</span><span>263      </span><span style='text-decoration: underline;'>10</span><span>011  </span><span style='text-decoration: underline;'>105</span><span>331 2126-08-14 </span><span style='color: #949494;'>22:32:00</span><span> 2126-08-28 </span><span style='color: #949494;'>18:59:00</span><span>
#&gt; </span><span style='color: #949494;'>3</span><span>    </span><span style='text-decoration: underline;'>12</span><span>265      </span><span style='text-decoration: underline;'>10</span><span>013  </span><span style='text-decoration: underline;'>165</span><span>520 2125-10-04 </span><span style='color: #949494;'>23:36:00</span><span> 2125-10-07 </span><span style='color: #949494;'>15:13:00</span><span>
#&gt; </span><span style='color: #949494;'>4</span><span>    </span><span style='text-decoration: underline;'>12</span><span>269      </span><span style='text-decoration: underline;'>10</span><span>017  </span><span style='text-decoration: underline;'>199</span><span>207 2149-05-26 </span><span style='color: #949494;'>17:19:00</span><span> 2149-06-03 </span><span style='color: #949494;'>18:42:00</span><span>
#&gt; </span><span style='color: #949494;'>5</span><span>    </span><span style='text-decoration: underline;'>12</span><span>270      </span><span style='text-decoration: underline;'>10</span><span>019  </span><span style='text-decoration: underline;'>177</span><span>759 2163-05-14 </span><span style='color: #949494;'>20:43:00</span><span> 2163-05-15 </span><span style='color: #949494;'>12:00:00</span><span>
#&gt; </span><span style='color: #949494;'>…</span><span>
#&gt; </span><span style='color: #949494;'>125</span><span>  </span><span style='text-decoration: underline;'>41</span><span>055      </span><span style='text-decoration: underline;'>44</span><span>083  </span><span style='text-decoration: underline;'>198</span><span>330 2112-05-28 </span><span style='color: #949494;'>15:45:00</span><span> 2112-06-07 </span><span style='color: #949494;'>16:50:00</span><span>
#&gt; </span><span style='color: #949494;'>126</span><span>  </span><span style='text-decoration: underline;'>41</span><span>070      </span><span style='text-decoration: underline;'>44</span><span>154  </span><span style='text-decoration: underline;'>174</span><span>245 2178-05-14 </span><span style='color: #949494;'>20:29:00</span><span> 2178-05-15 </span><span style='color: #949494;'>09:45:00</span><span>
#&gt; </span><span style='color: #949494;'>127</span><span>  </span><span style='text-decoration: underline;'>41</span><span>087      </span><span style='text-decoration: underline;'>44</span><span>212  </span><span style='text-decoration: underline;'>163</span><span>189 2123-11-24 </span><span style='color: #949494;'>14:14:00</span><span> 2123-12-30 </span><span style='color: #949494;'>14:31:00</span><span>
#&gt; </span><span style='color: #949494;'>128</span><span>  </span><span style='text-decoration: underline;'>41</span><span>090      </span><span style='text-decoration: underline;'>44</span><span>222  </span><span style='text-decoration: underline;'>192</span><span>189 2180-07-19 </span><span style='color: #949494;'>06:55:00</span><span> 2180-07-20 </span><span style='color: #949494;'>13:00:00</span><span>
#&gt; </span><span style='color: #949494;'>129</span><span>  </span><span style='text-decoration: underline;'>41</span><span>092      </span><span style='text-decoration: underline;'>44</span><span>228  </span><span style='text-decoration: underline;'>103</span><span>379 2170-12-15 </span><span style='color: #949494;'>03:14:00</span><span> 2170-12-24 </span><span style='color: #949494;'>18:00:00</span><span>
#&gt; </span><span style='color: #949494;'># … with 119 more rows, and 14 more variables: deathtime </span><span style='color: #949494;font-style: italic;'>&lt;dttm&gt;</span><span style='color: #949494;'>,
#&gt; #   admission_type </span><span style='color: #949494;font-style: italic;'>&lt;chr&gt;</span><span style='color: #949494;'>, admission_location </span><span style='color: #949494;font-style: italic;'>&lt;chr&gt;</span><span style='color: #949494;'>, discharge_location </span><span style='color: #949494;font-style: italic;'>&lt;chr&gt;</span><span style='color: #949494;'>,
#&gt; #   insurance </span><span style='color: #949494;font-style: italic;'>&lt;chr&gt;</span><span style='color: #949494;'>, language </span><span style='color: #949494;font-style: italic;'>&lt;chr&gt;</span><span style='color: #949494;'>, religion </span><span style='color: #949494;font-style: italic;'>&lt;chr&gt;</span><span style='color: #949494;'>, marital_status </span><span style='color: #949494;font-style: italic;'>&lt;chr&gt;</span><span style='color: #949494;'>,
#&gt; #   ethnicity </span><span style='color: #949494;font-style: italic;'>&lt;chr&gt;</span><span style='color: #949494;'>, edregtime </span><span style='color: #949494;font-style: italic;'>&lt;dttm&gt;</span><span style='color: #949494;'>, edouttime </span><span style='color: #949494;font-style: italic;'>&lt;dttm&gt;</span><span style='color: #949494;'>, diagnosis </span><span style='color: #949494;font-style: italic;'>&lt;chr&gt;</span><span style='color: #949494;'>,
#&gt; #   hospital_expire_flag </span><span style='color: #949494;font-style: italic;'>&lt;int&gt;</span><span style='color: #949494;'>, has_chartevents_data </span><span style='color: #949494;font-style: italic;'>&lt;int&gt;</span><span>
</span></CODE></PRE>

and data can be loaded into an R session for example using

``` r
print(
  load_ts("labevents", "mimic_demo", itemid == 50862L,
          cols = c("valuenum", "valueuom"))
)
```

<PRE class="fansi fansi-output"><CODE>#&gt; <span style='color: #949494;'># A `ts_tbl`: 299 ✖ 4</span><span>
#&gt; </span><span style='color: #949494;'># Id var:     `icustay_id`</span><span>
#&gt; </span><span style='color: #949494;'># Index var:  `charttime` (1 hours)</span><span>
#&gt;     icustay_id charttime valuenum valueuom
#&gt;          </span><span style='color: #949494;font-style: italic;'>&lt;int&gt;</span><span> </span><span style='color: #949494;font-style: italic;'>&lt;drtn&gt;</span><span>       </span><span style='color: #949494;font-style: italic;'>&lt;dbl&gt;</span><span> </span><span style='color: #949494;font-style: italic;'>&lt;chr&gt;</span><span>
#&gt; </span><span style='color: #949494;'>1</span><span>       </span><span style='text-decoration: underline;'>201</span><span>006   0 hours      2.4 g/dL
#&gt; </span><span style='color: #949494;'>2</span><span>       </span><span style='text-decoration: underline;'>203</span><span>766 -18 hours      2   g/dL
#&gt; </span><span style='color: #949494;'>3</span><span>       </span><span style='text-decoration: underline;'>203</span><span>766   4 hours      1.7 g/dL
#&gt; </span><span style='color: #949494;'>4</span><span>       </span><span style='text-decoration: underline;'>204</span><span>132   7 hours      3.6 g/dL
#&gt; </span><span style='color: #949494;'>5</span><span>       </span><span style='text-decoration: underline;'>204</span><span>201   9 hours      2.3 g/dL
#&gt; </span><span style='color: #949494;'>…</span><span>
#&gt; </span><span style='color: #949494;'>295</span><span>     </span><span style='text-decoration: underline;'>298</span><span>685 130 hours      1.9 g/dL
#&gt; </span><span style='color: #949494;'>296</span><span>     </span><span style='text-decoration: underline;'>298</span><span>685 154 hours      2   g/dL
#&gt; </span><span style='color: #949494;'>297</span><span>     </span><span style='text-decoration: underline;'>298</span><span>685 203 hours      2   g/dL
#&gt; </span><span style='color: #949494;'>298</span><span>     </span><span style='text-decoration: underline;'>298</span><span>685 272 hours      2.2 g/dL
#&gt; </span><span style='color: #949494;'>299</span><span>     </span><span style='text-decoration: underline;'>298</span><span>685 299 hours      2.5 g/dL
#&gt; </span><span style='color: #949494;'># … with 289 more rows</span><span>
</span></CODE></PRE>

which returns time series data as `ts_tbl` object.
