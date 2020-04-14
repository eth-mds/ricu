
mk_cfg <- function(is_demo = FALSE) {
  res <- list(
    admissionDrug = list(
      col_spec = list(
        admissiondrugid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        drugoffset = list(type = "col_integer"),
        drugenteredoffset = list(type = "col_integer"),
        drugnotetype = list(type = "col_character"),
        specialtytype = list(type = "col_character"),
        usertype = list(type = "col_character"),
        rxincluded = list(type = "col_character"),
        writtenineicu = list(type = "col_character"),
        drugname = list(type = "col_character"),
        drugdosage = list(type = "col_double"),
        drugunit = list(type = "col_character"),
        drugadmitfrequency = list(type = "col_character"),
        drughiclseqno = list(type = "col_integer")
      )
    ),
    admissionDx = list(
      col_spec = list(
        admissiondxid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        admitdxenteredoffset = list(type = "col_integer"),
        admitdxpath = list(type = "col_character"),
        admitdxname = list(type = "col_character"),
        admitdxtext = list(type = "col_character")
      )
    ),
    allergy = list(
      col_spec = list(
        allergyid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        allergyoffset = list(type = "col_integer"),
        allergyenteredoffset = list(type = "col_integer"),
        allergynotetype = list(type = "col_character"),
        specialtytype = list(type = "col_character"),
        usertype = list(type = "col_character"),
        rxincluded = list(type = "col_character"),
        writtenineicu = list(type = "col_character"),
        drugname = list(type = "col_character"),
        allergytype = list(type = "col_character"),
        allergyname = list(type = "col_character"),
        drughiclseqno = list(type = "col_integer")
      )
    ),
    apacheApsVar = list(
      col_spec = list(
        apacheapsvarid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        intubated = list(type = "col_integer"),
        vent = list(type = "col_integer"),
        dialysis = list(type = "col_integer"),
        eyes = list(type = "col_integer"),
        motor = list(type = "col_integer"),
        verbal = list(type = "col_integer"),
        meds = list(type = "col_integer"),
        urine = list(type = "col_double"),
        wbc = list(type = "col_double"),
        temperature = list(type = "col_double"),
        respiratoryrate = list(type = "col_double"),
        sodium = list(type = "col_double"),
        heartrate = list(type = "col_double"),
        meanbp = list(type = "col_double"),
        ph = list(type = "col_double"),
        hematocrit = list(type = "col_double"),
        creatinine = list(type = "col_double"),
        albumin = list(type = "col_double"),
        pao2 = list(type = "col_double"),
        pco2 = list(type = "col_double"),
        bun = list(type = "col_double"),
        glucose = list(type = "col_double"),
        bilirubin = list(type = "col_double"),
        fio2 = list(type = "col_double")
      )
    ),
    apachePatientResult = list(
      col_spec = list(
        apachepatientresultsid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        physicianspeciality = list(type = "col_character"),
        physicianinterventioncategory = list(type = "col_character"),
        acutephysiologyscore = list(type = "col_integer"),
        apachescore = list(type = "col_integer"),
        apacheversion = list(type = "col_character"),
        predictedicumortality = list(type = "col_character"),
        actualicumortality = list(type = "col_character"),
        predictediculos = list(type = "col_double"),
        actualiculos = list(type = "col_double"),
        predictedhospitalmortality = list(type = "col_character"),
        actualhospitalmortality = list(type = "col_character"),
        predictedhospitallos = list(type = "col_double"),
        actualhospitallos = list(type = "col_double"),
        preopmi = list(type = "col_integer"),
        preopcardiaccath = list(type = "col_integer"),
        ptcawithin24h = list(type = "col_integer"),
        unabridgedunitlos = list(type = "col_double"),
        unabridgedhosplos = list(type = "col_double"),
        actualventdays = list(type = "col_double"),
        predventdays = list(type = "col_double"),
        unabridgedactualventdays = list(type = "col_double")
      )
    ),
    apachePredVar = list(
      col_spec = list(
        apachepredvarid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        sicuday = list(type = "col_integer"),
        saps3day1 = list(type = "col_integer"),
        saps3today = list(type = "col_integer"),
        saps3yesterday = list(type = "col_integer"),
        gender = list(type = "col_integer"),
        teachtype = list(type = "col_integer"),
        region = list(type = "col_integer"),
        bedcount = list(type = "col_integer"),
        admitsource = list(type = "col_integer"),
        graftcount = list(type = "col_integer"),
        meds = list(type = "col_integer"),
        verbal = list(type = "col_integer"),
        motor = list(type = "col_integer"),
        eyes = list(type = "col_integer"),
        age = list(type = "col_integer"),
        admitdiagnosis = list(type = "col_character"),
        thrombolytics = list(type = "col_integer"),
        diedinhospital = list(type = "col_integer"),
        aids = list(type = "col_integer"),
        hepaticfailure = list(type = "col_integer"),
        lymphoma = list(type = "col_integer"),
        metastaticcancer = list(type = "col_integer"),
        leukemia = list(type = "col_integer"),
        immunosuppression = list(type = "col_integer"),
        cirrhosis = list(type = "col_integer"),
        electivesurgery = list(type = "col_integer"),
        activetx = list(type = "col_integer"),
        readmit = list(type = "col_integer"),
        ima = list(type = "col_integer"),
        midur = list(type = "col_integer"),
        ventday1 = list(type = "col_integer"),
        oobventday1 = list(type = "col_integer"),
        oobintubday1 = list(type = "col_integer"),
        diabetes = list(type = "col_integer"),
        managementsystem = list(type = "col_integer"),
        var03hspxlos = list(type = "col_double"),
        pao2 = list(type = "col_double"),
        fio2 = list(type = "col_double"),
        ejectfx = list(type = "col_double"),
        creatinine = list(type = "col_double"),
        dischargelocation = list(type = "col_integer"),
        visitnumber = list(type = "col_integer"),
        amilocation = list(type = "col_integer"),
        day1meds = list(type = "col_integer"),
        day1verbal = list(type = "col_integer"),
        day1motor = list(type = "col_integer"),
        day1eyes = list(type = "col_integer"),
        day1pao2 = list(type = "col_double"),
        day1fio2 = list(type = "col_double")
      )
    ),
    carePlanCareProvider = list(
      col_spec = list(
        cplcareprovderid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        careprovidersaveoffset = list(type = "col_integer"),
        providertype = list(type = "col_character"),
        specialty = list(type = "col_character"),
        interventioncategory = list(type = "col_character"),
        managingphysician = list(type = "col_character"),
        activeupondischarge = list(type = "col_character")
      )
    ),
    carePlanEOL = list(
      col_spec = list(
        cpleolid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        cpleolsaveoffset = list(type = "col_integer"),
        cpleoldiscussionoffset = list(type = "col_integer"),
        activeupondischarge = list(type = "col_character")
      )
    ),
    carePlanGeneral = list(
      col_spec = list(
        cplgeneralid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        activeupondischarge = list(type = "col_character"),
        cplitemoffset = list(type = "col_integer"),
        cplgroup = list(type = "col_character"),
        cplitemvalue = list(type = "col_character")
      )
    ),
    carePlanGoal = list(
      col_spec = list(
        cplgoalid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        cplgoaloffset = list(type = "col_integer"),
        cplgoalcategory = list(type = "col_character"),
        cplgoalvalue = list(type = "col_character"),
        cplgoalstatus = list(type = "col_character"),
        activeupondischarge = list(type = "col_character")
      )
    ),
    carePlanInfectiousDisease = list(
      col_spec = list(
        cplinfectid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        activeupondischarge = list(type = "col_character"),
        cplinfectdiseaseoffset = list(type = "col_integer"),
        infectdiseasesite = list(type = "col_character"),
        infectdiseaseassessment = list(type = "col_character"),
        responsetotherapy = list(type = "col_character"),
        treatment = list(type = "col_character")
      )
    ),
    customLab = list(
      col_spec = list(
        customlabid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        labotheroffset = list(type = "col_integer"),
        labothertypeid = list(type = "col_integer"),
        labothername = list(type = "col_character"),
        labotherresult = list(type = "col_character"),
        labothervaluetext = list(type = "col_character")
      )
    ),
    diagnosis = list(
      col_spec = list(
        diagnosisid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        activeupondischarge = list(type = "col_character"),
        diagnosisoffset = list(type = "col_integer"),
        diagnosisstring = list(type = "col_character"),
        icd9code = list(type = "col_character"),
        diagnosispriority = list(type = "col_character")
      )
    ),
    hospital = list(
      col_spec = list(
        hospitalid = list(type = "col_integer"),
        numbedscategory = list(type = "col_character"),
        teachingstatus = list(type = "col_logical"),
        region = list(type = "col_character")
      )
    ),
    infusionDrug = list(
      col_spec = list(
        infusiondrugid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        infusionoffset = list(type = "col_integer"),
        drugname = list(type = "col_character"),
        drugrate = list(type = "col_character"),
        infusionrate = list(type = "col_character"),
        drugamount = list(type = "col_character"),
        volumeoffluid = list(type = "col_character"),
        patientweight = list(type = "col_character")
      )
    ),
    intakeOutput = list(
      col_spec = list(
        intakeoutputid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        intakeoutputoffset = list(type = "col_integer"),
        intaketotal = list(type = "col_double"),
        outputtotal = list(type = "col_double"),
        dialysistotal = list(type = "col_double"),
        nettotal = list(type = "col_double"),
        intakeoutputentryoffset = list(type = "col_integer"),
        cellpath = list(type = "col_character"),
        celllabel = list(type = "col_character"),
        cellvaluenumeric = list(type = "col_double"),
        cellvaluetext = list(type = "col_character")
      )
    ),
    lab = list(
      col_spec = list(
        labid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        labresultoffset = list(type = "col_integer"),
        labtypeid = list(type = "col_double"),
        labname = list(type = "col_character"),
        labresult = list(type = "col_double"),
        labresulttext = list(type = "col_character"),
        labmeasurenamesystem = list(type = "col_character"),
        labmeasurenameinterface = list(type = "col_character"),
        labresultrevisedoffset = list(type = "col_integer")
      )
    ),
    medication = list(
      col_spec = list(
        medicationid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        drugorderoffset = list(type = "col_integer"),
        drugstartoffset = list(type = "col_integer"),
        drugivadmixture = list(type = "col_character"),
        drugordercancelled = list(type = "col_character"),
        drugname = list(type = "col_character"),
        drughiclseqno = list(type = "col_integer"),
        dosage = list(type = "col_character"),
        routeadmin = list(type = "col_character"),
        frequency = list(type = "col_character"),
        loadingdose = list(type = "col_character"),
        prn = list(type = "col_character"),
        drugstopoffset = list(type = "col_integer"),
        gtc = list(type = "col_integer")
      )
    ),
    microLab = list(
      col_spec = list(
        microlabid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        culturetakenoffset = list(type = "col_integer"),
        culturesite = list(type = "col_character"),
        organism = list(type = "col_character"),
        antibiotic = list(type = "col_character"),
        sensitivitylevel = list(type = "col_character")
      )
    ),
    note = list(
      col_spec = list(
        noteid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        noteoffset = list(type = "col_integer"),
        noteenteredoffset = list(type = "col_integer"),
        notetype = list(type = "col_character"),
        notepath = list(type = "col_character"),
        notevalue = list(type = "col_character"),
        notetext = list(type = "col_character")
      )
    ),
    nurseAssessment = list(
      col_spec = list(
        nurseassessid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        nurseassessoffset = list(type = "col_integer"),
        nurseassessentryoffset = list(type = "col_integer"),
        cellattributepath = list(type = "col_character"),
        celllabel = list(type = "col_character"),
        cellattribute = list(type = "col_character"),
        cellattributevalue = list(type = "col_character")
      )
    ),
    nurseCare = list(
      col_spec = list(
        nursecareid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        celllabel = list(type = "col_character"),
        nursecareoffset = list(type = "col_integer"),
        nursecareentryoffset = list(type = "col_integer"),
        cellattributepath = list(type = "col_character"),
        cellattribute = list(type = "col_character"),
        cellattributevalue = list(type = "col_character")
      )
    ),
    nurseCharting = list(
      col_spec = list(
        nursingchartid = list(type = "col_double"),
        patientunitstayid = list(type = "col_integer"),
        nursingchartoffset = list(type = "col_integer"),
        nursingchartentryoffset = list(type = "col_integer"),
        nursingchartcelltypecat = list(type = "col_character"),
        nursingchartcelltypevallabel = list(type = "col_character"),
        nursingchartcelltypevalname = list(type = "col_character"),
        nursingchartvalue = list(type = "col_character")
      ),
      partitioning = list(
        patientunitstayid = `if`(is_demo,
          c(0L, 1775421L, 999999999L),
          c(
            0L, 514528L, 1037072L, 1453997L, 1775421L, 2499831L, 2937948L,
            3213286L, 999999999L
          )
        )
      )
    ),
    pastHistory = list(
      col_spec = list(
        pasthistoryid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        pasthistoryoffset = list(type = "col_integer"),
        pasthistoryenteredoffset = list(type = "col_integer"),
        pasthistorynotetype = list(type = "col_character"),
        pasthistorypath = list(type = "col_character"),
        pasthistoryvalue = list(type = "col_character"),
        pasthistoryvaluetext = list(type = "col_character")
      )
    ),
    patient = list(
      col_spec = list(
        patientunitstayid = list(type = "col_integer"),
        patienthealthsystemstayid = list(type = "col_integer"),
        gender = list(type = "col_character"),
        age = list(type = "col_character"),
        ethnicity = list(type = "col_character"),
        hospitalid = list(type = "col_integer"),
        wardid = list(type = "col_integer"),
        apacheadmissiondx = list(type = "col_character"),
        admissionheight = list(type = "col_double"),
        hospitaladmittime24 = list(type = "col_character"),
        hospitaladmitoffset = list(type = "col_integer"),
        hospitaladmitsource = list(type = "col_character"),
        hospitaldischargeyear = list(type = "col_integer"),
        hospitaldischargetime24 = list(type = "col_character"),
        hospitaldischargeoffset = list(type = "col_integer"),
        hospitaldischargelocation = list(type = "col_character"),
        hospitaldischargestatus = list(type = "col_character"),
        unittype = list(type = "col_character"),
        unitadmittime24 = list(type = "col_character"),
        unitadmitsource = list(type = "col_character"),
        unitvisitnumber = list(type = "col_integer"),
        unitstaytype = list(type = "col_character"),
        admissionweight = list(type = "col_double"),
        dischargeweight = list(type = "col_double"),
        unitdischargetime24 = list(type = "col_character"),
        unitdischargeoffset = list(type = "col_integer"),
        unitdischargelocation = list(type = "col_character"),
        unitdischargestatus = list(type = "col_character"),
        uniquepid = list(type = "col_character")
      )
    ),
    physicalExam = list(
      col_spec = list(
        physicalexamid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        physicalexamoffset = list(type = "col_integer"),
        physicalexampath = list(type = "col_character"),
        physicalexamvalue = list(type = "col_character"),
        physicalexamtext = list(type = "col_character")
      )
    ),
    respiratoryCare = list(
      col_spec = list(
        respcareid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        respcarestatusoffset = list(type = "col_integer"),
        currenthistoryseqnum = list(type = "col_integer"),
        airwaytype = list(type = "col_character"),
        airwaysize = list(type = "col_character"),
        airwayposition = list(type = "col_character"),
        cuffpressure = list(type = "col_double"),
        ventstartoffset = list(type = "col_integer"),
        ventendoffset = list(type = "col_integer"),
        priorventstartoffset = list(type = "col_integer"),
        priorventendoffset = list(type = "col_integer"),
        apneaparms = list(type = "col_character"),
        lowexhmvlimit = list(type = "col_double"),
        hiexhmvlimit = list(type = "col_double"),
        lowexhtvlimit = list(type = "col_double"),
        hipeakpreslimit = list(type = "col_double"),
        lowpeakpreslimit = list(type = "col_double"),
        hirespratelimit = list(type = "col_double"),
        lowrespratelimit = list(type = "col_double"),
        sighpreslimit = list(type = "col_double"),
        lowironoxlimit = list(type = "col_double"),
        highironoxlimit = list(type = "col_double"),
        meanairwaypreslimit = list(type = "col_double"),
        peeplimit = list(type = "col_double"),
        cpaplimit = list(type = "col_double"),
        setapneainterval = list(type = "col_character"),
        setapneatv = list(type = "col_character"),
        setapneaippeephigh = list(type = "col_character"),
        setapnearr = list(type = "col_character"),
        setapneapeakflow = list(type = "col_character"),
        setapneainsptime = list(type = "col_character"),
        setapneaie = list(type = "col_character"),
        setapneafio2 = list(type = "col_character")
      )
    ),
    respiratoryCharting = list(
      col_spec = list(
        respchartid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        respchartoffset = list(type = "col_integer"),
        respchartentryoffset = list(type = "col_integer"),
        respcharttypecat = list(type = "col_character"),
        respchartvaluelabel = list(type = "col_character"),
        respchartvalue = list(type = "col_character")
      )
    ),
    treatment = list(
      col_spec = list(
        treatmentid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        treatmentoffset = list(type = "col_integer"),
        treatmentstring = list(type = "col_character"),
        activeupondischarge = list(type = "col_character")
      )
    ),
    vitalAperiodic = list(
      col_spec = list(
        vitalaperiodicid = list(type = "col_integer"),
        patientunitstayid = list(type = "col_integer"),
        observationoffset = list(type = "col_integer"),
        noninvasivesystolic = list(type = "col_double"),
        noninvasivediastolic = list(type = "col_double"),
        noninvasivemean = list(type = "col_double"),
        paop = list(type = "col_double"),
        cardiacoutput = list(type = "col_double"),
        cardiacinput = list(type = "col_double"),
        svr = list(type = "col_double"),
        svri = list(type = "col_double"),
        pvr = list(type = "col_double"),
        pvri = list(type = "col_double")
      )
    ),
    vitalPeriodic = list(
      col_spec = list(
        vitalperiodicid = list(type = "col_double"),
        patientunitstayid = list(type = "col_integer"),
        observationoffset = list(type = "col_integer"),
        temperature = list(type = "col_double"),
        sao2 = list(type = "col_integer"),
        heartrate = list(type = "col_integer"),
        respiration = list(type = "col_integer"),
        cvp = list(type = "col_integer"),
        etco2 = list(type = "col_integer"),
        systemicsystolic = list(type = "col_integer"),
        systemicdiastolic = list(type = "col_integer"),
        systemicmean = list(type = "col_integer"),
        pasystolic = list(type = "col_integer"),
        padiastolic = list(type = "col_integer"),
        pamean = list(type = "col_integer"),
        st1 = list(type = "col_double"),
        st2 = list(type = "col_double"),
        st3 = list(type = "col_double"),
        icp = list(type = "col_integer")
      ),
      partitioning = list(
        patientunitstayid = `if`(is_demo,
          c(0L, 1775421L, 999999999L),
          c(
            0L, 514528L, 1037072L, 1453997L, 1775421L, 2499831L, 2937948L,
            3213286L, 999999999L
          )
        )
      )
    )
  )

  if (is_demo) {
    names(res[["respiratoryCare"]][["col_spec"]])[
      names(res[["respiratoryCare"]][["col_spec"]]) == "apneaparms"
    ] <- "apneaparams"
    all_lower <- grepl("Drug$", names(res))
    names(res)[all_lower] <- tolower(names(res)[all_lower])
  }

  names(res) <- paste0(names(res), ".csv.gz")

  res
}

pkg_dir <- rprojroot::find_root(rprojroot::is_r_package)
cfg_dir <- file.path(pkg_dir, "inst", "extdata", "config")

ricu::set_config(
  list(
    name = "eicu",
    base_url = "https://physionet.org/files/eicu-crd",
    version = "2.0",
    setup_hook = "setup_eicu_aux_tables",
    tables = mk_cfg(is_demo = FALSE)
  ),
  "eicu-setup",
  cfg_dir
)

ricu::set_config(
  list(
    name = "eicu_demo",
    base_url = "https://physionet.org/files/eicu-crd-demo",
    version = "2.0",
    setup_hook = "setup_eicu_aux_tables",
    tables = mk_cfg(is_demo = TRUE)
  ),
  "eicu-demo",
  cfg_dir
)

devtools::install(pkg_dir)
