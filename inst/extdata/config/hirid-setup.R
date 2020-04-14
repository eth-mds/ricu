
cfg <- list(
  name = "hirid",
  base_url = "https://physionet.org/files/hirid",
  version = "0.1",
  setup_hook = "setup_hirid_aux_tables",
  tables = list(
    general = list(
      col_spec = list(
        patientid = list(type = "col_integer"),
        admissiontime = list(type = "col_datetime",
                             format = "%Y-%m-%d %H:%M:%S"),
        sex = list(type = "col_character"),
        age = list(type = "col_integer")
      )
    ),
    observations = list(
      col_spec = list(
        patientid = list(type = "col_integer"),
        datetime = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        entertime = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        status = list(type = "col_integer"),
        stringvalue = list(type = "col_character"),
        type = list(type = "col_character"),
        value = list(type = "col_double"),
        variableid = list(type = "col_integer")
      ),
      partitioning = list(patientid = 1:16)
    ),
    ordinal = list(
      col_spec = list(
        variableid = list(type = "col_integer"),
        code = list(type = "col_integer"),
        stringvalue = list(type = "col_character")
      )
    ),
    pharma = list(
      col_spec = list(
        patientid = list(type = "col_integer"),
        pharmaid = list(type = "col_integer"),
        givenat = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        enteredentryat = list(type = "col_datetime",
                              format = "%Y-%m-%d %H:%M:%S"),
        givendose = list(type = "col_double"),
        cumulativedose = list(type = "col_double"),
        fluidamount_calc = list(type = "col_double"),
        cumulfluidamount_calc = list(type = "col_double"),
        doseunit = list(type = "col_character"),
        route = list(type = "col_character"),
        infusionid = list(type = "col_integer"),
        typeid = list(type = "col_integer"),
        subtypeid = list(type = "col_double"),
        recordstatus = list(type = "col_integer")
      )
    )
  )
)

pkg_dir <- rprojroot::find_root(rprojroot::is_r_package)
cfg_dir <- file.path(pkg_dir, "inst", "extdata", "config")

ricu::set_config(cfg, "hirid-setup", cfg_dir)

devtools::install(pkg_dir)
