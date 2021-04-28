# demo envs

    Code
      print(mimic_demo)
    Output
      <mimic_demo_env[25]>
              admissions            callout         caregivers        chartevents 
              [129 x 19]          [77 x 24]        [7,567 x 4]     [758,355 x 15] 
               cptevents              d_cpt    d_icd_diagnoses   d_icd_procedures 
            [1,579 x 12]          [134 x 9]       [14,567 x 4]        [3,882 x 4] 
                 d_items         d_labitems     datetimeevents      diagnoses_icd 
           [12,487 x 10]          [753 x 6]      [15,551 x 14]        [1,761 x 5] 
                drgcodes           icustays     inputevents_cv     inputevents_mv 
               [297 x 8]         [136 x 12]      [34,799 x 22]      [13,224 x 31] 
               labevents microbiologyevents       outputevents           patients 
            [76,074 x 9]       [2,003 x 16]      [11,320 x 13]          [100 x 8] 
           prescriptions procedureevents_mv     procedures_icd           services 
           [10,398 x 19]         [753 x 25]          [506 x 5]          [163 x 6] 
               transfers 
              [524 x 13] 

---

    Code
      print(eicu_demo)
    Output
      <eicu_demo_env[31]>
                  admissiondrug               admissiondx                   allergy 
                   [7,417 x 14]               [7,578 x 6]              [2,475 x 13] 
                   apacheapsvar       apachepatientresult             apachepredvar 
                   [2,205 x 26]              [3,676 x 23]              [2,205 x 51] 
           careplancareprovider               careplaneol           careplangeneral 
                    [5,627 x 8]                  [15 x 5]              [33,148 x 6] 
                   careplangoal careplaninfectiousdisease                 customlab 
                    [3,633 x 7]                 [112 x 8]                  [30 x 7] 
                      diagnosis                  hospital              infusiondrug 
                   [24,978 x 7]                 [186 x 4]              [38,256 x 9] 
                   intakeoutput                       lab                medication 
                 [100,466 x 12]            [434,660 x 10]             [75,604 x 15] 
                       microlab                      note           nurseassessment 
                      [342 x 7]              [24,758 x 8]              [91,589 x 8] 
                      nursecare             nursecharting               pasthistory 
                   [42,080 x 8]           [1,477,163 x 8]              [12,109 x 8] 
                        patient              physicalexam           respiratorycare 
                   [2,520 x 29]              [84,058 x 6]              [5,436 x 34] 
            respiratorycharting                 treatment            vitalaperiodic 
                  [176,089 x 7]              [38,290 x 5]            [274,088 x 13] 
                  vitalperiodic 
               [1,634,960 x 19] 

---

    Code
      tbl_sum(mi_tbl)
    Output
      $`<mimic_tbl>`
      [1] "[129 x 19]"
      
      $`ID options`
      [1] "subject_id (patient) < hadm_id (hadm) < icustay_id (icustay)"
      
      $Defaults
      [1] "`admission_type` (val)"
      
      $`Time vars`
      [1] "`admittime`, `dischtime`, `deathtime`, `edregtime`, `edouttime`"
      

---

    Code
      tbl_sum(ei_tbl)
    Output
      $`<eicu_tbl>`
      [1] "[7,417 x 14]"
      
      $`ID options`
      [1] "patienthealthsystemstayid (hadm) < patientunitstayid (icustay)"
      
      $Defaults
      [1] "`drugoffset` (index), `drugunit` (unit), `drugdosage` (val)"
      
      $`Time vars`
      [1] "`drugoffset`, `drugenteredoffset`"
      

