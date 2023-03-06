# load concepts

    Code
      print(gluc)
    Output
      <concept[1]>
                     gluc 
      gluc <num_cncpt[1]> 

---

    Code
      print(albu)
    Output
      <concept[1]>
                     albu 
      albu <num_cncpt[1]> 

---

    Code
      print(glal)
    Output
      <concept[2]>
                     albu                gluc 
      albu <num_cncpt[1]> gluc <num_cncpt[1]> 

---

    Code
      print(gcs_con)
    Output
      <concept[1]>
                                                  gcs 
      Glasgow coma scale (non-sedated) <rec_cncpt[5]> 

---

    Code
      print(gcs_raw)
    Output
      <concept[1]>
                     gcs_raw 
      gcs_raw <rec_cncpt[1]> 

---

    Code
      print(dat1)
    Output
      # A `ts_tbl`: 1,914 x 3
      # Id var:     `icustay_id`
      # Units:      `omop_4144235` [mg/dL]
      # Index var:  `charttime` (1 hours)
            icustay_id charttime omop_4144235
                 <int> <drtn>           <dbl>
          1     201006 -58 hours          116
          2     201006 -45 hours           83
          3     201006 -21 hours           91
          4     201006   0 hours          175
          5     201006  11 hours          129
        ...
      1,910     298685 260 hours          159
      1,911     298685 272 hours          153
      1,912     298685 290 hours          182
      1,913     298685 293 hours          122
      1,914     298685 299 hours          121
      # ... with 1,904 more rows

---

    Code
      print(dat2)
    Output
      # A `ts_tbl`: 1,920 x 4
      # Id var:     `icustay_id`
      # Units:      `omop_4144235` [mg/dL], `omop_4017497` [g/dL]
      # Index var:  `charttime` (1 hours)
            icustay_id charttime omop_4144235 omop_4017497
                 <int> <drtn>           <dbl>        <dbl>
          1     201006 -58 hours          116         NA
          2     201006 -45 hours           83         NA
          3     201006 -21 hours           91         NA
          4     201006   0 hours          175          2.4
          5     201006  11 hours          129         NA
        ...
      1,916     298685 260 hours          159         NA
      1,917     298685 272 hours          153          2.2
      1,918     298685 290 hours          182         NA
      1,919     298685 293 hours          122         NA
      1,920     298685 299 hours          121          2.5
      # ... with 1,910 more rows

# load external dictionary

    Code
      print(dict)
    Output
      <concept[6]>
                                                  age 
                           patient age <num_cncpt[2]> 
                                                  alb 
                               albumin <num_cncpt[2]> 
                                                  glu 
                               glucose <num_cncpt[3]> 
                                                  gcs 
      Glasgow coma scale (non-sedated) <rec_cncpt[5]> 
                                                  esr 
        erythrocyte sedimentation rate <num_cncpt[2]> 
                                                  fgn 
                            fibrinogen <num_cncpt[2]> 

---

    Code
      print(itms)
    Output
      <item[21]>
      mimic_demo  eicu_demo mimic_demo  eicu_demo mimic_demo  eicu_demo  eicu_demo 
       <col_itm>  <col_itm>  <sel_itm>  <sel_itm>  <sel_itm>  <sel_itm>  <sel_itm> 
      mimic_demo  eicu_demo mimic_demo  eicu_demo mimic_demo  eicu_demo mimic_demo 
       <sel_itm>  <sel_itm>  <sel_itm>  <sel_itm>  <sel_itm>  <sel_itm>  <sel_itm> 
       eicu_demo mimic_demo  eicu_demo mimic_demo  eicu_demo mimic_demo  eicu_demo 
       <sel_itm>  <sel_itm>  <fun_itm>  <sel_itm>  <nul_itm>  <sel_itm>  <sel_itm> 

---

    {
      "type": "logical",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [5, 2]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["age", "alb", "glu", "esr", "fgn"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["eicu_demo", "mimic_demo"]
            }
          ]
        }
      },
      "value": [true, true, true, false, true, true, true, true, true, true]
    }

---

    {
      "type": "logical",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [6, 2]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["age", "alb", "glu", "gcs", "esr", "fgn"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["eicu_demo", "mimic_demo"]
            }
          ]
        }
      },
      "value": [true, true, true, false, false, true, true, true, true, false, true, true]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["name", "category", "description"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["age", "alb", "glu", "gcs", "esr", "fgn"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["demographics", "chemistry", "chemistry", "neurological", "hematology", "hematology"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["patient age", "albumin", "glucose", "Glasgow coma scale (non-sedated)", "erythrocyte sedimentation rate", "fibrinogen"]
        }
      ]
    }

---

    Code
      print(itms)
    Output
      <item[2]>
      mimic_demo  eicu_demo 
       <sel_itm>  <sel_itm> 

---

    Code
      print(itm)
    Output
      <item[1]>
      mimic_demo 
       <sel_itm> 

