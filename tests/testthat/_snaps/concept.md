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
      <item[23]>
      mimic_demo  eicu_demo mimic_demo  eicu_demo mimic_demo  eicu_demo  eicu_demo 
       <col_itm>  <col_itm>  <sel_itm>  <sel_itm>  <sel_itm>  <sel_itm>  <sel_itm> 
      mimic_demo  eicu_demo mimic_demo  eicu_demo mimic_demo  eicu_demo mimic_demo 
       <sel_itm>  <sel_itm>  <sel_itm>  <sel_itm>  <sel_itm>  <sel_itm>  <sel_itm> 
       eicu_demo mimic_demo  eicu_demo mimic_demo  eicu_demo mimic_demo  eicu_demo 
       <sel_itm>  <sel_itm>  <nul_itm>  <sel_itm>  <sel_itm>  <sel_itm>  <nul_itm> 
      mimic_demo  eicu_demo 
       <sel_itm>  <sel_itm> 

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

