# ricu 0.5.4

* maintenance release: tests using `mockthat` are only run when available

# ricu 0.5.3

* maintenance release for compatibility with an update to `prt`

# ricu 0.5.2

* maintenance release due to issue with `units` (#301)

# ricu 0.5.1

* maintenance release due to upcoming API change of `rlang` v1.0.0

# ricu 0.5.0

* adds `miiv` data source (MIMIC-IV)
* new data class `win_tbl` (inheriting from `ts_tbl`), which allows for
  specifying a validity duration (for example for infusions consisting of both
  a rate and a duration)
* automatic unit conversion is available by specifying the `unt_cncpt` concept
  type

# ricu 0.4.0

* CRAN release which includes `aumc` data source and `sep3` concept

# ricu 0.3.0

* revamped `src_env` setup

# ricu 0.2.1

* concept harmonization (mostly `aumc` and `hirid` uom fixes)
* split of `setup_src_data()` from `setup_src_env()` for convenient setup in
  non-interactive scenarios

# ricu 0.2.0

* add `aumc` data source

# ricu 0.1.3

* concept fixes (`susp_inf`, non-hadm loading of `abx` on `mimic`, `map` on
  `hirid`)
* fix CRAN policy violation

# ricu 0.1.2

* restructure SOFA computation
* add concept caching
* add `\value{}` entries to docs

# ricu 0.1.1

* make CRAN compliant (`.GlobalEnv` default in `attach_src()`)
* concept fixes: weight, vasopressors & height

# ricu 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* CRAN submission
