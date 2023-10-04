# config classes

    Code
      print(id)
    Output
      <mimic_demo_ids[3]>
           patient         hadm      icustay 
      `subject_id`    `hadm_id` `icustay_id` 

---

    Code
      print(co)
    Output
      <mimic_demo_cols[25]>
              admissions            callout         caregivers        chartevents 
         [0, 0, 5, 0, 1]    [0, 1, 6, 0, 1]    [1, 0, 0, 0, 1]    [0, 1, 2, 1, 1] 
               cptevents              d_cpt    d_icd_diagnoses   d_icd_procedures 
         [0, 1, 1, 0, 1]    [1, 0, 0, 0, 1]    [1, 0, 0, 0, 1]    [1, 0, 0, 0, 1] 
                 d_items         d_labitems     datetimeevents      diagnoses_icd 
         [1, 0, 0, 0, 1]    [1, 0, 0, 0, 1]    [0, 1, 3, 0, 1]    [0, 0, 0, 0, 1] 
                drgcodes           icustays     inputevents_cv     inputevents_mv 
         [0, 0, 0, 0, 1]    [0, 1, 2, 0, 1]    [0, 1, 2, 1, 1]    [0, 1, 4, 1, 1] 
               labevents microbiologyevents       outputevents           patients 
         [0, 1, 1, 1, 1]    [0, 1, 2, 0, 1]    [0, 1, 2, 1, 1]    [0, 0, 4, 0, 1] 
           prescriptions procedureevents_mv     procedures_icd           services 
         [0, 1, 2, 1, 1]    [0, 1, 4, 1, 1]    [0, 0, 0, 0, 1]    [0, 1, 1, 0, 1] 
               transfers 
         [0, 1, 2, 0, 1] 

---

    Code
      print(tb)
    Output
      <mimic_demo_tbls[25]>
              admissions            callout         caregivers        chartevents 
            [?? x 19; 1]       [?? x 24; 1]        [?? x 4; 1]       [?? x 15; 2] 
               cptevents              d_cpt    d_icd_diagnoses   d_icd_procedures 
            [?? x 12; 1]        [?? x 9; 1]        [?? x 4; 1]        [?? x 4; 1] 
                 d_items         d_labitems     datetimeevents      diagnoses_icd 
            [?? x 10; 1]        [?? x 6; 1]       [?? x 14; 1]        [?? x 5; 1] 
                drgcodes           icustays     inputevents_cv     inputevents_mv 
             [?? x 8; 1]       [?? x 12; 1]       [?? x 22; 1]       [?? x 31; 1] 
               labevents microbiologyevents       outputevents           patients 
             [?? x 9; 1]       [?? x 16; 1]       [?? x 13; 1]        [?? x 8; 1] 
           prescriptions procedureevents_mv     procedures_icd           services 
            [?? x 19; 1]       [?? x 25; 1]        [?? x 5; 1]        [?? x 6; 1] 
               transfers 
            [?? x 13; 1] 

