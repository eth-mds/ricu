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

# load dictionary

    Code
      print(dict)
    Output
      <concept[4]>
                                                  age 
                           patient age <num_cncpt[2]> 
                                                  alb 
                               albumin <num_cncpt[2]> 
                                                  glu 
                               glucose <num_cncpt[3]> 
                                                  gcs 
      Glasgow coma scale (non-sedated) <rec_cncpt[5]> 

---

    {
      "type": "logical",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [3, 2]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["age", "alb", "glu"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["eicu_demo", "mimic_demo"]
            }
          ]
        }
      },
      "value": [true, true, true, true, true, true]
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
          "value": [1, 2, 3, 4]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["age", "alb", "glu", "gcs"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["demographics", "chemistry", "chemistry", "neurological"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["patient age", "albumin", "glucose", "Glasgow coma scale (non-sedated)"]
        }
      ]
    }

