# =============================================================================
#  CDISC oncology data checks: raw (EDC/CRF), SDTM and ADaM
#  Author: Atanu Bhattacharjee
#
#  Contents
#    1. Parameters and helpers
#    2. RECIST 1.1 and endpoint derivations (shared by simulator and checks)
#    3. Check specification (raw, SDTM, ADaM)
#    4. Check engine (automated checks)
#    5. Simulated RECIST 1.1 oncology study (raw, SDTM, ADaM)
#    6. Seeded errors and expected findings
#    7. Excel writers (specification, study, findings)
#
#  Standards: SDTMIG v3.4, ADaMIG v1.3, CDISC Controlled Terminology,
#  RECIST 1.1, CTCAE (protocol-specified version), FDA guidance on clinical
#  trial endpoints for cancer drugs (2018).
#
#  Packages: openxlsx, readxl
# =============================================================================

suppressPackageStartupMessages({
  library(openxlsx)
  library(readxl)
})

# -----------------------------------------------------------------------------
# 1. Parameters and helpers
# -----------------------------------------------------------------------------
ONC_PAR <- list(
  study          = "ONC-001",
  dco            = as.Date("2026-06-30"),  # data cut-off
  scan_interval  = 56,   # days between scheduled tumour assessments
  scan_window    = 7,    # +/- days allowed around the scheduled assessment
  baseline_days  = 28,   # baseline imaging window before first dose
  confirm_days   = 28,   # minimum interval for confirmation of CR/PR
  sd_min_days    = 42,   # minimum time from first dose for SD to count as BOR
  missed_gap     = 126,  # gap (days) defining two or more missed assessments
  teae_days      = 30,   # TEAE window after last dose
  dose_levels    = list("Drug X" = c(200, 150), "Standard of care" = c(100, 75))
)

MON <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; trimws(x) }
num <- function(x) suppressWarnings(as.numeric(as.character(x)))

fmt_raw <- function(d) ifelse(is.na(d), "",
  sprintf("%02d-%s-%s", as.integer(format(d, "%d")), MON[as.integer(format(d, "%m"))], format(d, "%Y")))

parse_raw <- function(x) {
  x <- toupper(chr(x))
  out <- as.Date(rep(NA_character_, length(x)))
  ok <- grepl("^[0-9]{2}-[A-Z]{3}-[0-9]{4}$", x)
  if (any(ok)) {
    mm <- match(substr(x[ok], 4, 6), MON)
    s <- ifelse(is.na(mm), NA, sprintf("%s-%02d-%s", substr(x[ok], 8, 11), mm, substr(x[ok], 1, 2)))
    out[ok] <- as.Date(s, format = "%Y-%m-%d")
  }
  out
}
raw_partial <- function(x) grepl("^UN-[A-Z]{3}-[0-9]{4}$", toupper(chr(x)))

iso <- function(d) ifelse(is.na(d), "", format(d, "%Y-%m-%d"))

dt <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  x <- chr(x)
  as.Date(ifelse(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x), substr(x, 1, 10), NA), format = "%Y-%m-%d")
}

iso_valid <- function(x) {
  x <- chr(x)
  pat <- "^[0-9]{4}(-[0-9]{2}(-[0-9]{2}(T[0-9]{2}:[0-9]{2}(:[0-9]{2})?)?)?)?$"
  full_ok <- nchar(x) < 10 | !is.na(as.Date(substr(x, 1, 10), format = "%Y-%m-%d"))
  x == "" | (grepl(pat, x) & full_ok)
}

study_day <- function(d, ref) ifelse(is.na(d) | is.na(ref), NA_real_,
                                    as.numeric(d - ref) + (as.numeric(d - ref) >= 0))

fnd <- function(id, ds, usubjid, record, value, msg) {
  if (length(usubjid) == 0) return(NULL)
  data.frame(check_id = id, dataset = ds, usubjid = as.character(usubjid),
             record = as.character(record), value = as.character(value), message = msg,
             stringsAsFactors = FALSE)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -----------------------------------------------------------------------------
# 2. RECIST 1.1 and endpoint derivations
# -----------------------------------------------------------------------------

# RECIST 1.1 Table 1 (subjects with target disease)
recist_overall <- function(trg, ntrg, newl) {
  trg <- chr(trg); ntrg <- chr(ntrg); newl <- chr(newl)
  ifelse(trg == "PD" | ntrg == "PD" | newl == "Y", "PD",
  ifelse(trg == "CR" & ntrg == "CR", "CR",
  ifelse(trg == "CR" & ntrg %in% c("NON-CR/NON-PD", "NE"), "PR",
  ifelse(trg == "PR", "PR",
  ifelse(trg == "SD", "SD", "NE")))))
}

# Target-lesion response over visits from individual diameters.
# Integer arithmetic: PD if 5*S >= 6*nadir and S - nadir >= 5; PR if 10*S <= 7*baseline.
# Incomplete visits (a baseline target not measured) return NA and do not update the nadir.
recist_target_series <- function(vis, lid, diam, nodal, base_vis) {
  bl <- unique(lid[vis == base_vis])
  bsum <- sum(diam[vis == base_vis & lid %in% bl])
  vv <- sort(unique(vis[vis != base_vis]))
  out <- data.frame(visit = vv, sum = rep(NA_real_, length(vv)), trg = rep(NA_character_, length(vv)), stringsAsFactors = FALSE)
  nadir <- bsum
  for (i in seq_along(vv)) {
    sel <- vis == vv[i] & lid %in% bl
    d <- diam[sel]; nd <- nodal[sel]
    if (length(unique(lid[sel])) < length(bl) || any(is.na(d))) next
    s <- sum(d); out$sum[i] <- s
    out$trg[i] <- if (5 * s >= 6 * nadir && s - nadir >= 5) "PD" else
      if (all(d[!nd] == 0) && all(d[nd] < 10)) "CR" else
      if (10 * s <= 7 * bsum) "PR" else "SD"
    nadir <- min(nadir, s)
  }
  out
}

# Best overall response with confirmation
bor_derive <- function(dates, resp, start, par = ONC_PAR) {
  o <- order(dates); dates <- dates[o]; resp <- chr(resp)[o]
  if (!length(resp)) return(list(bor = "NE", adt = as.Date(NA)))
  pd <- which(resp == "PD")[1]
  if (!is.na(pd)) { dates <- dates[seq_len(pd)]; resp <- resp[seq_len(pd)] }
  conf <- function(set) {
    idx <- which(resp %in% set)
    for (i in idx) for (j in idx) if (j > i && as.numeric(dates[j] - dates[i]) >= par$confirm_days) return(i)
    NA
  }
  i <- conf("CR"); if (!is.na(i)) return(list(bor = "CR", adt = dates[i]))
  i <- conf(c("CR", "PR")); if (!is.na(i)) return(list(bor = "PR", adt = dates[i]))
  s <- which(resp %in% c("CR", "PR", "SD") & (as.numeric(dates - start) + 1) >= par$sd_min_days)[1]
  if (!is.na(s)) return(list(bor = "SD", adt = dates[s]))
  if (!is.na(pd)) return(list(bor = "PD", adt = dates[pd]))
  list(bor = "NE", adt = as.Date(NA))
}

# Progression-free survival (event or censoring date) per SAP rules
pfs_derive <- function(rand, dates, resp, death, par = ONC_PAR) {
  o <- order(dates); d <- dates[o]; r <- chr(resp)[o]
  pd <- which(r == "PD")[1]
  pd_dt <- if (is.na(pd)) as.Date(NA) else d[pd]
  pre <- if (is.na(pd)) seq_along(r) else seq_len(pd - 1)
  ad <- pre[r[pre] != "NE"]
  lad <- if (length(ad)) d[max(ad)] else rand
  cand <- c(pd_dt, death); cand <- cand[!is.na(cand)]
  if (length(cand)) {
    ev <- min(cand)
    desc <- if (!is.na(pd_dt) && pd_dt == ev) "Disease progression" else "Death"
    if (as.numeric(ev - lad) > par$missed_gap)
      return(list(adt = lad, cnsr = 1, desc = "Event after two or more missed assessments", missed = TRUE))
    return(list(adt = ev, cnsr = 0, desc = desc, missed = FALSE))
  }
  list(adt = lad, cnsr = 1, missed = FALSE,
       desc = if (length(ad)) "Last adequate tumour assessment" else "Randomisation (no post-baseline assessment)")
}

# -----------------------------------------------------------------------------
# 3. Check specification
# -----------------------------------------------------------------------------
REF <- list(
  recist = "RECIST 1.1 (Eisenhauer et al., Eur J Cancer 2009;45:228-247)",
  ctcae  = "NCI CTCAE, protocol-specified version (v5.0 or v6.0)",
  sdtm   = "SDTMIG v3.4",
  adam   = "ADaMIG v1.3",
  ct     = "CDISC Controlled Terminology",
  fda    = "FDA guidance: Clinical Trial Endpoints for the Approval of Cancer Drugs and Biologics (2018)",
  ich    = "ICH E6(R3) Good Clinical Practice",
  prot   = "Protocol / SAP"
)

spec_row <- function(id, level, ds, cat, check, logic, sev, ref, auto = "Automated") {
  data.frame(Check_ID = id, Level = level, Dataset = ds, Category = cat, Check = check,
             Logic = logic, Severity = sev, Reference = ref, Automation = auto,
             stringsAsFactors = FALSE)
}

CHECK_SPEC <- rbind(
  # ---- Raw (EDC / CRF) ------------------------------------------------------
  spec_row("RAW-001", "Raw", "RAW_DM", "Identity", "Subject identifier present and unique on the demography form",
           "SUBJID non-blank; no SUBJID occurs more than once", "Major", REF$ich),
  spec_row("RAW-002", "Raw", "RAW_DM", "Consent", "Informed consent obtained on or before randomisation",
           "ICDT <= RANDDT", "Critical", paste(REF$ich, REF$prot, sep = "; ")),
  spec_row("RAW-003", "Raw", "RAW_DM", "Eligibility", "Birth date valid and age at consent within 18 to 100 years",
           "age = floor((ICDT - BRTHDT) / 365.25); 18 <= age <= 100", "Major", REF$prot),
  spec_row("RAW-004", "Raw", "RAW_DM", "Eligibility", "Randomised subject has ECOG 0 or 1 and measurable disease at baseline",
           "ECOG in {0, 1} and MEASDIS = Y", "Critical", paste(REF$prot, REF$recist, sep = "; ")),
  spec_row("RAW-005", "Raw", "All raw forms", "Date validity",
           "Every date is a valid DD-MMM-YYYY calendar date on or before the data cut-off; AE dates may have an unknown day (UN-MMM-YYYY)",
           "Parse each *DT field; flag unparseable dates and dates after the data cut-off", "Major", REF$ich),
  spec_row("RAW-006", "Raw", "RAW_EX", "Dosing", "Dose start on or before dose end, and no dose before randomisation",
           "EXSTDT <= EXENDT; EXSTDT >= RANDDT", "Major", REF$prot),
  spec_row("RAW-007", "Raw", "RAW_EX", "Dosing", "Dose is a protocol dose level for the arm and every dose adjustment has a reason",
           "Drug X: 200 or 150 mg; Standard of care: 100 or 75 mg; DOSEADJ = Y requires ADJREAS", "Major", REF$prot),
  spec_row("RAW-008", "Raw", "RAW_EX", "Dosing", "Cycle numbers unique and sequential within subject",
           "CYCLE values = 1, 2, ..., k with no repeats", "Minor", REF$prot),
  spec_row("RAW-009", "Raw", "RAW_TUMOR", "Tumour assessment", "Baseline imaging within 28 days before first dose",
           "0 <= first dose date - baseline scan date <= 28", "Critical", paste(REF$recist, REF$prot, sep = "; ")),
  spec_row("RAW-010", "Raw", "RAW_TUMOR", "RECIST 1.1", "No more than 5 target lesions in total and 2 per organ at baseline",
           "count(TARGET) <= 5; count(TARGET by ORGAN) <= 2", "Critical", REF$recist),
  spec_row("RAW-011", "Raw", "RAW_TUMOR", "RECIST 1.1", "Baseline target lesions are measurable",
           "Non-nodal longest diameter >= 10 mm; nodal short axis >= 15 mm", "Critical", REF$recist),
  spec_row("RAW-012", "Raw", "RAW_TUMOR", "RECIST 1.1", "Every baseline target lesion is measured at every post-baseline assessment",
           "For each post-baseline visit, each baseline TARGET lesion has a non-missing DIAM_MM (5 mm default entered when too small to measure)",
           "Major", REF$recist),
  spec_row("RAW-013", "Raw", "RAW_TUMOR", "RECIST 1.1", "Follow-up lesion identifiers match baseline and lesion type does not change",
           "Post-baseline LESIONID (other than NEW) exists at baseline; LESTYPE equals baseline LESTYPE", "Major", REF$recist),
  spec_row("RAW-014", "Raw", "RAW_TUMOR", "RECIST 1.1", "Same imaging method for a lesion at baseline and follow-up",
           "METHOD at each visit equals baseline METHOD for that lesion", "Major", REF$recist),
  spec_row("RAW-015", "Raw", "RAW_TUMOR", "Schedule", "Post-baseline scans within the protocol window",
           "|SCANDT - (first dose + 56 x k)| <= 7 days for visit WEEK 8k", "Minor", REF$prot),
  spec_row("RAW-016", "Raw", "RAW_RESP", "RECIST 1.1", "Investigator overall response consistent with target, non-target and new-lesion responses",
           "OVRLRESP equals RECIST 1.1 Table 1 applied to TRGRESP, NTRGRESP, NEWLES", "Critical", REF$recist),
  spec_row("RAW-017", "Raw", "RAW_RESP", "RECIST 1.1", "Investigator target response consistent with the recorded diameters",
           "PD: sum >= 1.2 x nadir and >= nadir + 5 mm; CR: non-nodal 0 and nodes < 10 mm; PR: sum <= 0.7 x baseline sum; otherwise SD",
           "Critical", REF$recist),
  spec_row("RAW-018", "Raw", "RAW_AE", "Adverse events", "AE start on or before end; ongoing flag consistent with end date",
           "AESTDT <= AEENDT; ONGOING = Y if and only if AEENDT blank", "Major", REF$ich),
  spec_row("RAW-019", "Raw", "RAW_AE", "Adverse events", "CTCAE grade valid; grade 5, fatal outcome and recorded death agree",
           "GRADE in 1-5; GRADE = 5 <=> OUTCOME = FATAL <=> death date in RAW_DS", "Critical", REF$ctcae),
  spec_row("RAW-020", "Raw", "RAW_AE / RAW_EX", "Adverse events", "Action 'drug withdrawn' followed by no further dosing",
           "ACTION = DRUG WITHDRAWN => no EXSTDT after AESTDT", "Major", REF$prot),
  spec_row("RAW-021", "Raw", "RAW_DS", "Survival", "Death and last contact dates not earlier than last dose or last scan",
           "DTHDT and LASTCONTDT >= max(last EXSTDT, last SCANDT)", "Critical", REF$prot),
  spec_row("RAW-022", "Raw", "Subsequent therapy form", "Survival",
           "Start date of new anticancer therapy recorded for every subject who stopped study treatment",
           "Form completed for each subject with an end-of-treatment record", "Major", REF$fda, "Manual review"),
  spec_row("RAW-023", "Raw", "Laboratory", "Laboratory", "Lab values carry units and reference ranges; CTCAE lab grade agrees with value and baseline",
           "Grade from value against ULN/LLN per protocol CTCAE version", "Major", REF$ctcae, "Manual review"),
  spec_row("RAW-024", "Raw", "RAW_AE", "Coding", "AE verbatim term codable (one event per term, no abbreviations); MedDRA coding reviewed",
           "Medical review of verbatim and coded terms", "Minor", REF$ich, "Manual review"),
  spec_row("RAW-025", "Raw", "RAW_TUMOR", "Source verification", "Tumour measurements and responses verified against radiology reports or BICR",
           "Source data verification of measurements, dates and lesion identity", "Critical", paste(REF$recist, REF$ich, sep = "; "), "Manual review"),

  # ---- SDTM ----------------------------------------------------------------
  spec_row("SDTM-GEN-001", "SDTM", "All domains", "Conformance", "Required variables present and populated",
           "DM: STUDYID DOMAIN USUBJID SUBJID SITEID SEX ARMCD ARM ACTARMCD ACTARM COUNTRY; EX: ...EXSEQ EXTRT EXSTDTC; AE: ...AESEQ AETERM; TU: ...TUSEQ TULNKID TUTESTCD TUORRES; TR: ...TRSEQ TRTESTCD; RS: ...RSSEQ RSTESTCD RSORRES; DS: ...DSSEQ DSTERM DSDECOD DSCAT",
           "Critical", REF$sdtm),
  spec_row("SDTM-GEN-002", "SDTM", "All domains", "Conformance", "DOMAIN equals the dataset name", "DOMAIN = dataset name", "Major", REF$sdtm),
  spec_row("SDTM-GEN-003", "SDTM", "All domains", "Conformance", "Every USUBJID exists in DM", "USUBJID in DM.USUBJID", "Critical", REF$sdtm),
  spec_row("SDTM-GEN-004", "SDTM", "All domains", "Conformance", "--SEQ unique within USUBJID", "No duplicated (USUBJID, --SEQ)", "Major", REF$sdtm),
  spec_row("SDTM-GEN-005", "SDTM", "All domains", "Conformance", "--DTC values in ISO 8601 format (complete or partial)",
           "YYYY[-MM[-DD[Thh:mm[:ss]]]] and a valid calendar date when complete", "Major", REF$sdtm),
  spec_row("SDTM-GEN-006", "SDTM", "All domains", "Derivation", "--DY derived from RFSTDTC with no day 0",
           "--DY = date - RFSTDTC + 1 on or after RFSTDTC; date - RFSTDTC before", "Major", REF$sdtm),
  spec_row("SDTM-GEN-007", "SDTM", "All domains", "Controlled terminology", "Values conform to CDISC CT codelists",
           "SEX; NY (AESER, AESDTH, ...); OUT; ACN; AETOXGR 1-5; TUORRES (TARGET, NON-TARGET, NEW); RSORRES by RSTESTCD; EXDOSU, TRSTRESU",
           "Major", REF$ct),
  spec_row("SDTM-GEN-008", "SDTM", "Submission package", "Conformance", "Define-XML v2.1, dataset and variable labels, lengths, SAS XPT v5 and SUPP-- datasets",
           "Define-XML validation and dataset metadata review", "Major", REF$sdtm, "Manual review"),
  spec_row("SDTM-GEN-009", "SDTM", "Submission package", "Conformance", "Conformance report (Pinnacle 21 / CDISC CORE) reviewed and every remaining issue explained in the cSDRG",
           "Run validator; resolve or explain each issue", "Major", REF$sdtm, "Manual review"),
  spec_row("SDTM-DM-001", "SDTM", "DM", "Derivation", "Reference exposure dates agree with EX",
           "RFXSTDTC = min(EXSTDTC); RFXENDTC = max(EXENDTC); RFSTDTC = RFXSTDTC", "Critical", REF$sdtm),
  spec_row("SDTM-DM-002", "SDTM", "DM", "Survival", "Death flag and death date agree", "DTHFL = Y <=> DTHDTC populated", "Critical", REF$sdtm),
  spec_row("SDTM-DM-003", "SDTM", "DM / EX", "Treatment", "Actual arm consistent with treatment received",
           "ACTARM matches EXTRT; ACTARM = Not Treated when no EX record", "Critical", REF$sdtm),
  spec_row("SDTM-EX-001", "SDTM", "EX", "Exposure", "Exposure start on or before end, and records do not overlap",
           "EXSTDTC <= EXENDTC; next EXSTDTC > previous EXENDTC", "Major", REF$sdtm),
  spec_row("SDTM-EX-002", "SDTM", "EX", "Exposure", "Dose positive with units", "EXDOSE > 0 and EXDOSU populated", "Major", REF$sdtm),
  spec_row("SDTM-AE-001", "SDTM", "AE", "Adverse events", "AE start on or before end", "AESTDTC <= AEENDTC (complete dates)", "Major", REF$sdtm),
  spec_row("SDTM-AE-002", "SDTM", "AE / DM", "Adverse events", "Fatal outcome, grade 5, death criterion and DM death flag agree",
           "AEOUT = FATAL <=> AETOXGR = 5 <=> AESDTH = Y; fatal AE => DM.DTHFL = Y", "Critical", paste(REF$sdtm, REF$ctcae, sep = "; ")),
  spec_row("SDTM-AE-003", "SDTM", "AE", "Adverse events", "Seriousness flag consistent with seriousness criteria",
           "AESER = Y <=> any of AESDTH, AESHOSP, AESLIFE, AESDISAB, AESCONG, AESMIE = Y", "Critical", REF$sdtm),
  spec_row("SDTM-AE-004", "SDTM", "AE", "Coding", "AE coded to MedDRA", "AEDECOD and AEBODSYS populated", "Major", REF$sdtm),
  spec_row("SDTM-TU-001", "SDTM", "TU", "RECIST 1.1", "Lesion link identifier unique per subject and evaluator",
           "No duplicated (USUBJID, TUEVAL, TULNKID)", "Critical", REF$sdtm),
  spec_row("SDTM-TU-002", "SDTM", "TU", "RECIST 1.1", "Target and non-target lesions identified at baseline only; new lesions post-baseline only",
           "TUORRES in {TARGET, NON-TARGET} => baseline visit; TUORRES = NEW => post-baseline visit", "Critical", REF$recist),
  spec_row("SDTM-TU-003", "SDTM", "TU", "RECIST 1.1", "No more than 5 target lesions and 2 per organ",
           "count(TARGET) <= 5; count(TARGET by TULOC) <= 2", "Critical", REF$recist),
  spec_row("SDTM-TR-001", "SDTM", "TR / TU", "Traceability", "Every lesion result links to an identified lesion",
           "(USUBJID, TRLNKID) exists in TU", "Critical", REF$sdtm),
  spec_row("SDTM-TR-002", "SDTM", "TR", "RECIST 1.1", "Diameters in mm with a numeric result unless not done with a reason",
           "TRSTRESU = mm for LDIAM, LPERP, SUMDIAM; TRSTRESN populated unless TRSTAT = NOT DONE; TRSTAT = NOT DONE requires TRREASND",
           "Major", REF$sdtm),
  spec_row("SDTM-TR-003", "SDTM", "TR", "RECIST 1.1", "Sum of diameters equals the sum of target lesion diameters at each visit",
           "SUMDIAM TRSTRESN = sum of LDIAM/LPERP TRSTRESN for TARGET lesions", "Critical", REF$recist),
  spec_row("SDTM-TR-004", "SDTM", "TR", "RECIST 1.1", "Baseline target lesions measurable",
           "Baseline LDIAM >= 10 mm (non-nodal); LPERP >= 15 mm (nodal)", "Critical", REF$recist),
  spec_row("SDTM-RS-001", "SDTM", "RS", "RECIST 1.1", "Overall response follows RECIST 1.1 Table 1",
           "OVRLRESP = f(TRGRESP, NTRGRESP, NEWLPROG) at each visit", "Critical", REF$recist),
  spec_row("SDTM-RS-002", "SDTM", "RS / TR", "RECIST 1.1", "Target response consistent with TR diameters",
           "TRGRESP recomputed from baseline sum and nadir (see RAW-017)", "Critical", REF$recist),
  spec_row("SDTM-RS-003", "SDTM", "RS / TR", "Traceability", "Each response time point has tumour results and the response date matches the scan",
           "TR exists at RS VISITNUM; RSDTC = latest TRDTC at that visit", "Major", REF$sdtm),
  spec_row("SDTM-DS-001", "SDTM", "DS / DM", "Survival", "Death recorded consistently in DS and DM; one disposition event per epoch",
           "DS DSDECOD = DEATH date = DM.DTHDTC; no duplicated (USUBJID, EPOCH) among disposition events", "Critical", REF$sdtm),
  spec_row("SDTM-TS-001", "SDTM", "TS / trial design", "Trial design", "TS records RECIST and CTCAE versions and data cut-off; TA, TE, TV complete",
           "Review of trial design domains", "Major", REF$sdtm, "Manual review"),

  # ---- ADaM ----------------------------------------------------------------
  spec_row("ADAM-GEN-001", "ADaM", "ADSL", "Structure", "ADSL has one record per subject and the same subjects as DM",
           "No duplicated USUBJID; set(ADSL.USUBJID) = set(DM.USUBJID)", "Critical", REF$adam),
  spec_row("ADAM-GEN-002", "ADaM", "BDS datasets", "Structure", "PARAMCD and PARAM map one to one", "Each PARAMCD has a single PARAM", "Major", REF$adam),
  spec_row("ADAM-GEN-003", "ADaM", "BDS / OCCDS", "Traceability", "Variables copied from ADSL equal ADSL",
           "TRT01A, TRTSDT, TRTEDT, SAFFL in ADRS, ADTTE, ADAE equal ADSL", "Major", REF$adam),
  spec_row("ADAM-GEN-004", "ADaM", "All datasets", "Conformance", "Flag values valid",
           "ADSL ITTFL, SAFFL in {Y, N}; DTHFL, ANL01FL, TRTEMFL in {Y, blank}", "Major", REF$adam),
  spec_row("ADAM-GEN-005", "ADaM", "ADRS", "Traceability", "Source record pointers resolve to SDTM",
           "SRCDOM = RS and (USUBJID, SRCSEQ) exists in RS for RSTESTCD = OVRLRESP", "Major", REF$adam),
  spec_row("ADAM-GEN-006", "ADaM", "Submission package", "Quality control", "ADRG and define complete; key endpoints independently double-programmed",
           "Independent programming of ADRS, ADTTE and primary outputs", "Critical", REF$adam, "Manual review"),
  spec_row("ADAM-ADSL-001", "ADaM", "ADSL / EX", "Treatment", "Treatment start and end dates equal first and last exposure",
           "TRTSDT = min(EXSTDTC); TRTEDT = max(EXENDTC)", "Critical", REF$adam),
  spec_row("ADAM-ADSL-002", "ADaM", "ADSL", "Populations", "Population flags consistent with randomisation and dosing",
           "ITTFL = Y <=> randomised (DS RANDOMIZED); SAFFL = Y <=> any EX record", "Critical", REF$prot),
  spec_row("ADAM-ADSL-003", "ADaM", "ADSL / DM", "Survival", "Death flag and date equal DM", "DTHFL = DM.DTHFL; DTHDT = DM.DTHDTC", "Critical", REF$adam),
  spec_row("ADAM-ADSL-004", "ADaM", "ADSL / EX", "Treatment", "Actual treatment consistent with exposure", "TRT01A matches EXTRT; blank when not treated", "Critical", REF$adam),
  spec_row("ADAM-ADRS-001", "ADaM", "ADRS / RS", "Response", "Time-point overall response equals SDTM RS",
           "PARAMCD = OVR: AVALC = RS.OVRLRESP at AVISITN = VISITNUM", "Critical", REF$adam),
  spec_row("ADAM-ADRS-002", "ADaM", "ADRS / RS", "Response", "Best overall response derived per SAP",
           "Confirmed CR/PR (second assessment >= 28 days later); SD >= 42 days from first dose; assessments after PD (or new anticancer therapy) excluded; otherwise PD or NE",
           "Critical", paste(REF$recist, REF$prot, sep = "; ")),
  spec_row("ADAM-ADRS-003", "ADaM", "ADRS", "Response", "Exactly one BOR record per ITT subject", "count(PARAMCD = BOR) = 1 for each ITTFL = Y subject", "Major", REF$adam),
  spec_row("ADAM-ADTTE-001", "ADaM", "ADTTE", "Time to event", "Analysis value equals elapsed time", "AVAL = ADT - STARTDT + 1 and AVAL > 0", "Critical", REF$adam),
  spec_row("ADAM-ADTTE-002", "ADaM", "ADTTE", "Time to event", "PFS and OS event status and date derived per SAP",
           "PFS: event = first PD or death; censor at last adequate assessment (randomisation if none). OS: event = death; censor at last known alive date",
           "Critical", paste(REF$fda, REF$prot, sep = "; ")),
  spec_row("ADAM-ADTTE-003", "ADaM", "ADTTE", "Time to event", "PFS event after two or more missed assessments is censored",
           "If event date - last adequate assessment > 126 days: CNSR = 1 at last adequate assessment", "Critical", REF$fda),
  spec_row("ADAM-ADTTE-004", "ADaM", "ADTTE", "Time to event", "Overall survival not shorter than PFS", "OS AVAL >= PFS AVAL per subject", "Major", REF$prot),
  spec_row("ADAM-ADTTE-005", "ADaM", "ADTTE / ADRS", "Time to event", "Duration of response only for confirmed responders, starting at first response",
           "PARAMCD = DOR only when BOR in {CR, PR}; STARTDT = first CR/PR date", "Critical", REF$prot),
  spec_row("ADAM-ADTTE-006", "ADaM", "ADTTE", "Time to event", "No event or censoring date after the data cut-off", "ADT <= DCUTDT", "Critical", REF$prot),
  spec_row("ADAM-ADAE-001", "ADaM", "ADAE", "Adverse events", "Treatment-emergent flag derived correctly",
           "TRTEMFL = Y <=> TRTSDT <= ASTDT <= TRTEDT + 30", "Critical", REF$prot),
  spec_row("ADAM-ADAE-002", "ADaM", "ADAE / AE", "Traceability", "ADAE and AE contain the same records", "Each (USUBJID, AESEQ) in AE appears once in ADAE and vice versa", "Critical", REF$adam),
  spec_row("ADAM-ADAE-003", "ADaM", "ADAE", "Derivation", "Imputed start dates flagged",
           "Partial AESTDTC => ASTDTF populated; complete AESTDTC => ASTDTF blank", "Major", REF$adam),
  spec_row("ADAM-ADAE-004", "ADaM", "ADAE", "Quality control", "Worst-grade summaries by subject, SOC and PT reconcile with ADAE",
           "Independent tabulation of worst AETOXGRN per subject", "Major", REF$adam, "Manual review")
)

# -----------------------------------------------------------------------------
# 4. Check engine
# -----------------------------------------------------------------------------
REQ_VARS <- list(
  DM = c("STUDYID", "DOMAIN", "USUBJID", "SUBJID", "SITEID", "SEX", "ARMCD", "ARM", "ACTARMCD", "ACTARM", "COUNTRY"),
  EX = c("STUDYID", "DOMAIN", "USUBJID", "EXSEQ", "EXTRT", "EXSTDTC"),
  AE = c("STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM"),
  TU = c("STUDYID", "DOMAIN", "USUBJID", "TUSEQ", "TULNKID", "TUTESTCD", "TUORRES"),
  TR = c("STUDYID", "DOMAIN", "USUBJID", "TRSEQ", "TRTESTCD"),
  RS = c("STUDYID", "DOMAIN", "USUBJID", "RSSEQ", "RSTESTCD", "RSORRES"),
  DS = c("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT"))

CT <- list(
  SEX = c("M", "F", "U", "UNDIFFERENTIATED"),
  NY = c("Y", "N", ""),
  OUT = c("FATAL", "NOT RECOVERED/NOT RESOLVED", "RECOVERED/RESOLVED", "RECOVERED/RESOLVED WITH SEQUELAE",
          "RECOVERING/RESOLVING", "UNKNOWN"),
  ACN = c("DOSE INCREASED", "DOSE NOT CHANGED", "DOSE REDUCED", "DRUG INTERRUPTED", "DRUG WITHDRAWN",
          "NOT APPLICABLE", "UNKNOWN"),
  TOXGR = as.character(1:5),
  TUORRES = c("TARGET", "NON-TARGET", "NEW"),
  RESP = list(TRGRESP = c("CR", "PR", "SD", "PD", "NE"), NTRGRESP = c("CR", "NON-CR/NON-PD", "PD", "NE"),
              NEWLPROG = c("Y", "N"), OVRLRESP = c("CR", "PR", "SD", "PD", "NE")),
  UNIT = "mg", LUNIT = "mm")

raw_sid <- function(st, sid) paste(ONC_PAR$study, sid, sep = "-")

CHECKS <- list()
reg <- function(id, needs, fun) CHECKS[[id]] <<- list(id = id, needs = needs, fun = fun)

# ---- Raw ---------------------------------------------------------------------
reg("RAW-001", "RAW_DM", function(st, p) {
  s <- chr(st$RAW_DM$SUBJID)
  b <- which(s == "" | duplicated(s) | duplicated(s, fromLast = TRUE))
  fnd("RAW-001", "RAW_DM", s[b], paste("row", b), s[b], "Missing or duplicated subject identifier")
})
reg("RAW-002", "RAW_DM", function(st, p) {
  d <- st$RAW_DM; ic <- parse_raw(d$ICDT); rd <- parse_raw(d$RANDDT)
  b <- which(!is.na(ic) & !is.na(rd) & ic > rd)
  fnd("RAW-002", "RAW_DM", d$SUBJID[b], "ICDT/RANDDT", paste(d$ICDT[b], d$RANDDT[b], sep = " / "),
      "Consent date after randomisation date")
})
reg("RAW-003", "RAW_DM", function(st, p) {
  d <- st$RAW_DM; ic <- parse_raw(d$ICDT); bd <- parse_raw(d$BRTHDT)
  age <- floor(as.numeric(ic - bd) / 365.25)
  b <- which(is.na(bd) | (!is.na(age) & (age < 18 | age > 100)))
  fnd("RAW-003", "RAW_DM", d$SUBJID[b], "BRTHDT", paste0(d$BRTHDT[b], " (age ", age[b], ")"),
      "Birth date invalid or age at consent outside 18-100 years")
})
reg("RAW-004", "RAW_DM", function(st, p) {
  d <- st$RAW_DM
  b <- which(!(chr(d$ECOG) %in% c("0", "1")) | chr(d$MEASDIS) != "Y")
  fnd("RAW-004", "RAW_DM", d$SUBJID[b], "ECOG/MEASDIS", paste(chr(d$ECOG)[b], chr(d$MEASDIS)[b], sep = " / "),
      "Eligibility: ECOG not 0-1 or no measurable disease")
})
reg("RAW-005", character(), function(st, p) {
  out <- list()
  for (sh in grep("^RAW_", names(st), value = TRUE)) {
    d <- st[[sh]]
    for (v in grep("DT$", names(d), value = TRUE)) {
      x <- chr(d[[v]]); pd <- parse_raw(x)
      part <- if (sh == "RAW_AE") raw_partial(x) else rep(FALSE, length(x))
      b <- which(x != "" & ((is.na(pd) & !part) | (!is.na(pd) & pd > p$dco)))
      out[[length(out) + 1]] <- fnd("RAW-005", sh, d$SUBJID[b], v, x[b], "Invalid date or date after data cut-off")
    }
  }
  do.call(rbind, out)
})
reg("RAW-006", c("RAW_EX", "RAW_DM"), function(st, p) {
  e <- st$RAW_EX; s <- parse_raw(e$EXSTDT); en <- parse_raw(e$EXENDT)
  rd <- parse_raw(st$RAW_DM$RANDDT)[match(e$SUBJID, st$RAW_DM$SUBJID)]
  b <- which((!is.na(s) & !is.na(en) & s > en) | (!is.na(s) & !is.na(rd) & s < rd))
  fnd("RAW-006", "RAW_EX", e$SUBJID[b], paste("cycle", e$CYCLE[b]), paste(e$EXSTDT[b], e$EXENDT[b], sep = " / "),
      "Dose start after dose end, or dose before randomisation")
})
reg("RAW-007", c("RAW_EX", "RAW_DM"), function(st, p) {
  e <- st$RAW_EX; arm <- chr(st$RAW_DM$ARM)[match(e$SUBJID, st$RAW_DM$SUBJID)]
  dose <- num(e$DOSE)
  okdose <- mapply(function(a, x) !is.na(x) && a %in% names(p$dose_levels) && x %in% p$dose_levels[[a]], arm, dose)
  b <- which(!okdose | (chr(e$DOSEADJ) == "Y" & chr(e$ADJREAS) == ""))
  fnd("RAW-007", "RAW_EX", e$SUBJID[b], paste("cycle", e$CYCLE[b]), paste(e$DOSE[b], "mg;", arm[b]),
      "Dose not a protocol level for the arm, or adjustment without reason")
})
reg("RAW-008", "RAW_EX", function(st, p) {
  e <- st$RAW_EX; out <- list()
  for (s in unique(e$SUBJID)) {
    cy <- num(e$CYCLE[e$SUBJID == s])
    if (any(duplicated(cy)) || !identical(sort(cy), as.numeric(seq_along(cy))))
      out[[length(out) + 1]] <- fnd("RAW-008", "RAW_EX", s, "CYCLE", paste(cy, collapse = ","), "Cycle numbers repeated or not sequential")
  }
  do.call(rbind, out)
})
first_dose_raw <- function(st) {
  e <- st$RAW_EX; s <- parse_raw(e$EXSTDT)
  fd <- tapply(s, e$SUBJID, function(z) if (all(is.na(z))) NA else min(z, na.rm = TRUE))
  as.Date(fd, origin = "1970-01-01")
}
reg("RAW-009", c("RAW_TUMOR", "RAW_EX"), function(st, p) {
  t <- st$RAW_TUMOR; t <- t[chr(t$VISIT) == "BASELINE", ]
  bs <- tapply(parse_raw(t$SCANDT), t$SUBJID, function(z) min(z, na.rm = TRUE))
  fd <- first_dose_raw(st)
  s <- intersect(names(bs), names(fd))
  gap <- as.numeric(fd[s]) - as.numeric(bs[s])
  b <- which(!is.na(gap) & (gap < 0 | gap > p$baseline_days))
  fnd("RAW-009", "RAW_TUMOR", s[b], "BASELINE SCANDT", paste(gap[b], "days before first dose"),
      "Baseline imaging outside the 28-day window before first dose")
})
reg("RAW-010", "RAW_TUMOR", function(st, p) {
  t <- st$RAW_TUMOR; t <- t[chr(t$VISIT) == "BASELINE" & chr(t$LESTYPE) == "TARGET", ]
  out <- list()
  for (s in unique(t$SUBJID)) {
    x <- t[t$SUBJID == s, ]; po <- table(chr(x$ORGAN))
    if (nrow(x) > 5 || any(po > 2))
      out[[length(out) + 1]] <- fnd("RAW-010", "RAW_TUMOR", s, "BASELINE TARGET",
                                    paste0(nrow(x), " targets; ", paste(names(po), po, collapse = ", ")),
                                    "More than 5 target lesions or more than 2 per organ")
  }
  do.call(rbind, out)
})
reg("RAW-011", "RAW_TUMOR", function(st, p) {
  t <- st$RAW_TUMOR; t <- t[chr(t$VISIT) == "BASELINE" & chr(t$LESTYPE) == "TARGET", ]
  d <- num(t$DIAM_MM); nd <- chr(t$NODAL) == "Y"
  b <- which(is.na(d) | (!nd & d < 10) | (nd & d < 15))
  fnd("RAW-011", "RAW_TUMOR", t$SUBJID[b], t$LESIONID[b], paste(d[b], "mm", ifelse(nd[b], "(nodal)", "")),
      "Baseline target lesion not measurable")
})
reg("RAW-012", "RAW_TUMOR", function(st, p) {
  t <- st$RAW_TUMOR; out <- list()
  for (s in unique(t$SUBJID)) {
    x <- t[t$SUBJID == s, ]
    bl <- unique(x$LESIONID[chr(x$VISIT) == "BASELINE" & chr(x$LESTYPE) == "TARGET"])
    for (v in setdiff(unique(chr(x$VISIT)), "BASELINE")) for (l in bl) {
      r <- x[chr(x$VISIT) == v & x$LESIONID == l, ]
      if (!nrow(r) || all(is.na(num(r$DIAM_MM))))
        out[[length(out) + 1]] <- fnd("RAW-012", "RAW_TUMOR", s, paste(v, l), "missing", "Target lesion not measured at this assessment")
    }
  }
  do.call(rbind, out)
})
reg("RAW-013", "RAW_TUMOR", function(st, p) {
  t <- st$RAW_TUMOR; out <- list()
  for (s in unique(t$SUBJID)) {
    x <- t[t$SUBJID == s, ]; b <- x[chr(x$VISIT) == "BASELINE", ]
    f <- x[chr(x$VISIT) != "BASELINE" & chr(x$LESTYPE) != "NEW", ]
    bt <- chr(b$LESTYPE)[match(f$LESIONID, b$LESIONID)]
    bad <- which(is.na(bt) | bt != chr(f$LESTYPE))
    out[[length(out) + 1]] <- fnd("RAW-013", "RAW_TUMOR", rep(s, length(bad)), paste(f$VISIT[bad], f$LESIONID[bad]),
                                  paste(f$LESTYPE[bad], "baseline:", ifelse(is.na(bt[bad]), "absent", bt[bad])),
                                  "Lesion not identified at baseline or lesion type changed")
  }
  do.call(rbind, out)
})
reg("RAW-014", "RAW_TUMOR", function(st, p) {
  t <- st$RAW_TUMOR
  key <- paste(t$SUBJID, t$LESIONID)
  bm <- chr(t$METHOD)[chr(t$VISIT) == "BASELINE"]; names(bm) <- key[chr(t$VISIT) == "BASELINE"]
  m <- bm[key]
  b <- which(chr(t$VISIT) != "BASELINE" & !is.na(m) & chr(t$METHOD) != m)
  fnd("RAW-014", "RAW_TUMOR", t$SUBJID[b], paste(t$VISIT[b], t$LESIONID[b]), paste(t$METHOD[b], "vs baseline", m[b]),
      "Imaging method differs from baseline")
})
reg("RAW-015", c("RAW_TUMOR", "RAW_EX"), function(st, p) {
  t <- st$RAW_TUMOR; t <- t[chr(t$VISIT) != "BASELINE", ]
  u <- unique(t[c("SUBJID", "VISIT", "SCANDT")])
  wk <- num(sub("WEEK ", "", u$VISIT)); fd <- first_dose_raw(st)[u$SUBJID]
  dev <- as.numeric(parse_raw(u$SCANDT) - (fd + wk * 7))
  b <- which(!is.na(dev) & abs(dev) > p$scan_window)
  fnd("RAW-015", "RAW_TUMOR", u$SUBJID[b], u$VISIT[b], paste(dev[b], "days from schedule"), "Scan outside the protocol window")
})
reg("RAW-016", "RAW_RESP", function(st, p) {
  r <- st$RAW_RESP; ex <- recist_overall(r$TRGRESP, r$NTRGRESP, r$NEWLES)
  b <- which(chr(r$OVRLRESP) != ex)
  fnd("RAW-016", "RAW_RESP", r$SUBJID[b], r$VISIT[b], paste0(r$OVRLRESP[b], " (expected ", ex[b], ")"),
      "Overall response inconsistent with RECIST 1.1 Table 1")
})
target_expected_raw <- function(st) {
  t <- st$RAW_TUMOR; t <- t[chr(t$LESTYPE) == "TARGET", ]; out <- list()
  for (s in unique(t$SUBJID)) {
    x <- t[t$SUBJID == s, ]
    vn <- ifelse(chr(x$VISIT) == "BASELINE", 0, num(sub("WEEK ", "", x$VISIT)))
    rs <- recist_target_series(vn, x$LESIONID, num(x$DIAM_MM), chr(x$NODAL) == "Y", 0)
    if (nrow(rs)) out[[length(out) + 1]] <- data.frame(SUBJID = s, VISIT = paste("WEEK", rs$visit), exp = rs$trg)
  }
  do.call(rbind, out)
}
reg("RAW-017", c("RAW_RESP", "RAW_TUMOR"), function(st, p) {
  r <- st$RAW_RESP; e <- target_expected_raw(st)
  ex <- e$exp[match(paste(r$SUBJID, r$VISIT), paste(e$SUBJID, e$VISIT))]
  b <- which(!is.na(ex) & chr(r$TRGRESP) != ex)
  fnd("RAW-017", "RAW_RESP", r$SUBJID[b], r$VISIT[b], paste0(r$TRGRESP[b], " (diameters give ", ex[b], ")"),
      "Target response inconsistent with recorded diameters")
})
reg("RAW-018", "RAW_AE", function(st, p) {
  a <- st$RAW_AE; s <- parse_raw(a$AESTDT); e <- parse_raw(a$AEENDT); en <- chr(a$AEENDT)
  b <- which((!is.na(s) & !is.na(e) & s > e) | (chr(a$ONGOING) == "Y" & en != "") | (chr(a$ONGOING) == "N" & en == ""))
  fnd("RAW-018", "RAW_AE", a$SUBJID[b], paste("AE", a$AENUM[b]), paste(a$AESTDT[b], a$AEENDT[b], "ongoing", a$ONGOING[b]),
      "AE start after end, or ongoing flag inconsistent with end date")
})
reg("RAW-019", c("RAW_AE", "RAW_DS"), function(st, p) {
  a <- st$RAW_AE; g <- chr(a$GRADE)
  dead <- chr(st$RAW_DS$DTHDT)[match(a$SUBJID, st$RAW_DS$SUBJID)]; dead[is.na(dead)] <- ""
  fat <- chr(a$OUTCOME) == "FATAL"
  b <- which(!(g %in% as.character(1:5)) | (g == "5") != fat | (g == "5" & dead == ""))
  fnd("RAW-019", "RAW_AE", a$SUBJID[b], paste("AE", a$AENUM[b]), paste("grade", g[b], "/", a$OUTCOME[b]),
      "Grade invalid, or grade 5 / fatal outcome / death record disagree")
})
reg("RAW-020", c("RAW_AE", "RAW_EX"), function(st, p) {
  a <- st$RAW_AE; a <- a[chr(a$ACTION) == "DRUG WITHDRAWN", ]
  e <- st$RAW_EX; es <- parse_raw(e$EXSTDT)
  b <- which(vapply(seq_len(nrow(a)), function(i) {
    s <- parse_raw(a$AESTDT[i]); !is.na(s) && any(es[e$SUBJID == a$SUBJID[i]] > s, na.rm = TRUE)
  }, logical(1)))
  fnd("RAW-020", "RAW_AE", a$SUBJID[b], paste("AE", a$AENUM[b]), a$AESTDT[b], "Dosing recorded after drug withdrawn for AE")
})
reg("RAW-021", c("RAW_DS", "RAW_EX", "RAW_TUMOR"), function(st, p) {
  d <- st$RAW_DS
  le <- tapply(parse_raw(st$RAW_EX$EXSTDT), st$RAW_EX$SUBJID, function(z) max(z, na.rm = TRUE))
  ls <- tapply(parse_raw(st$RAW_TUMOR$SCANDT), st$RAW_TUMOR$SUBJID, function(z) max(z, na.rm = TRUE))
  last <- pmax(as.numeric(le[d$SUBJID]), as.numeric(ls[d$SUBJID]), na.rm = TRUE)
  dth <- as.numeric(parse_raw(d$DTHDT)); lc <- as.numeric(parse_raw(d$LASTCONTDT))
  b <- which((!is.na(dth) & !is.na(last) & dth < last) | (!is.na(lc) & !is.na(last) & lc < last))
  fnd("RAW-021", "RAW_DS", d$SUBJID[b], "DTHDT/LASTCONTDT", paste(d$DTHDT[b], d$LASTCONTDT[b], sep = " / "),
      "Death or last contact date before last dose or last scan")
})

# ---- SDTM --------------------------------------------------------------------
SDTM_DOMAINS <- c("DM", "EX", "AE", "TU", "TR", "RS", "DS")
reg("SDTM-GEN-001", character(), function(st, p) {
  out <- list()
  for (dm in intersect(names(REQ_VARS), names(st))) {
    d <- st[[dm]]
    miss <- setdiff(REQ_VARS[[dm]], names(d))
    if (length(miss)) out[[length(out) + 1]] <- fnd("SDTM-GEN-001", dm, "(dataset)", "variables", paste(miss, collapse = ", "), "Required variable absent")
    for (v in intersect(REQ_VARS[[dm]], names(d))) {
      b <- which(chr(d[[v]]) == "")
      out[[length(out) + 1]] <- fnd("SDTM-GEN-001", dm, chr(d$USUBJID)[b], paste0(dm, "SEQ ", chr(d[[paste0(dm, "SEQ")]] %||% "")[b]), v,
                                    "Required variable not populated")
    }
  }
  do.call(rbind, out)
})
reg("SDTM-GEN-002", character(), function(st, p) {
  out <- lapply(intersect(SDTM_DOMAINS, names(st)), function(dm) {
    d <- st[[dm]]; b <- which(chr(d$DOMAIN) != dm)
    fnd("SDTM-GEN-002", dm, d$USUBJID[b], paste0(dm, "SEQ ", chr(d[[paste0(dm, "SEQ")]])[b]), d$DOMAIN[b], "DOMAIN differs from dataset name")
  })
  do.call(rbind, out)
})
reg("SDTM-GEN-003", "DM", function(st, p) {
  ids <- chr(st$DM$USUBJID)
  out <- lapply(setdiff(intersect(SDTM_DOMAINS, names(st)), "DM"), function(dm) {
    d <- st[[dm]]; b <- which(!(chr(d$USUBJID) %in% ids))
    fnd("SDTM-GEN-003", dm, d$USUBJID[b], paste0(dm, "SEQ ", chr(d[[paste0(dm, "SEQ")]])[b]), d$USUBJID[b], "USUBJID not in DM")
  })
  do.call(rbind, out)
})
reg("SDTM-GEN-004", character(), function(st, p) {
  out <- lapply(setdiff(intersect(SDTM_DOMAINS, names(st)), "DM"), function(dm) {
    d <- st[[dm]]; k <- paste(d$USUBJID, chr(d[[paste0(dm, "SEQ")]]))
    b <- which(duplicated(k) | duplicated(k, fromLast = TRUE))
    fnd("SDTM-GEN-004", dm, d$USUBJID[b], paste0(dm, "SEQ ", chr(d[[paste0(dm, "SEQ")]])[b]), "duplicated", "--SEQ not unique within USUBJID")
  })
  do.call(rbind, out)
})
reg("SDTM-GEN-005", character(), function(st, p) {
  out <- list()
  for (dm in intersect(SDTM_DOMAINS, names(st))) {
    d <- st[[dm]]
    for (v in grep("DTC$", names(d), value = TRUE)) {
      b <- which(!iso_valid(d[[v]]))
      out[[length(out) + 1]] <- fnd("SDTM-GEN-005", dm, d$USUBJID[b], v, chr(d[[v]])[b], "Date not ISO 8601")
    }
  }
  do.call(rbind, out)
})
reg("SDTM-GEN-006", "DM", function(st, p) {
  ref <- dt(st$DM$RFSTDTC); names(ref) <- chr(st$DM$USUBJID)
  pairs <- list(EX = c("EXSTDTC", "EXSTDY"), EX = c("EXENDTC", "EXENDY"), AE = c("AESTDTC", "AESTDY"),
                AE = c("AEENDTC", "AEENDY"), TU = c("TUDTC", "TUDY"), TR = c("TRDTC", "TRDY"),
                RS = c("RSDTC", "RSDY"), DS = c("DSSTDTC", "DSSTDY"))
  out <- list()
  for (i in seq_along(pairs)) {
    dm <- names(pairs)[i]; if (!dm %in% names(st)) next
    d <- st[[dm]]; v <- pairs[[i]]; if (!all(v %in% names(d))) next
    x <- dt(ifelse(nchar(chr(d[[v[1]]])) >= 10, chr(d[[v[1]]]), NA))
    ex <- study_day(x, ref[chr(d$USUBJID)]); ac <- num(d[[v[2]]])
    b <- which(!is.na(ex) & (is.na(ac) | ac != ex))
    out[[length(out) + 1]] <- fnd("SDTM-GEN-006", dm, d$USUBJID[b], v[2], paste0(ac[b], " (expected ", ex[b], ")"), "Study day incorrect")
  }
  do.call(rbind, out)
})
reg("SDTM-GEN-007", character(), function(st, p) {
  out <- list()
  add <- function(dm, v, ok, label) {
    if (!dm %in% names(st) || !v %in% names(st[[dm]])) return()
    d <- st[[dm]]; x <- chr(d[[v]]); b <- which(!(x %in% ok))
    out[[length(out) + 1]] <<- fnd("SDTM-GEN-007", dm, d$USUBJID[b], v, x[b], paste("Value not in codelist", label))
  }
  add("DM", "SEX", CT$SEX, "SEX")
  for (v in c("AESER", "AESDTH", "AESHOSP", "AESLIFE", "AESDISAB", "AESCONG", "AESMIE")) add("AE", v, CT$NY, "NY")
  add("AE", "AEOUT", CT$OUT, "OUT"); add("AE", "AEACN", CT$ACN, "ACN"); add("AE", "AETOXGR", CT$TOXGR, "AETOXGR")
  add("TU", "TUORRES", CT$TUORRES, "TUORRES"); add("EX", "EXDOSU", c(CT$UNIT, ""), "UNIT")
  if ("RS" %in% names(st)) {
    r <- st$RS; ok <- mapply(function(t, v) t %in% names(CT$RESP) && v %in% CT$RESP[[t]], chr(r$RSTESTCD), chr(r$RSSTRESC))
    b <- which(!ok)
    out[[length(out) + 1]] <- fnd("SDTM-GEN-007", "RS", r$USUBJID[b], r$RSTESTCD[b], r$RSSTRESC[b], "Response value not in codelist for RSTESTCD")
  }
  do.call(rbind, out)
})
reg("SDTM-DM-001", c("DM", "EX"), function(st, p) {
  dm <- st$DM; e <- st$EX
  s <- tapply(dt(e$EXSTDTC), chr(e$USUBJID), function(z) if (all(is.na(z))) NA else min(z, na.rm = TRUE))
  en <- tapply(dt(e$EXENDTC), chr(e$USUBJID), function(z) if (all(is.na(z))) NA else max(z, na.rm = TRUE))
  u <- chr(dm$USUBJID)
  es <- as.Date(as.numeric(s[u]), origin = "1970-01-01"); ee <- as.Date(as.numeric(en[u]), origin = "1970-01-01")
  rs <- dt(dm$RFXSTDTC); re <- dt(dm$RFXENDTC); rf <- dt(dm$RFSTDTC)
  neq <- function(a, b) (is.na(a) != is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
  b <- which(neq(rs, es) | neq(re, ee) | neq(rf, rs))
  fnd("SDTM-DM-001", "DM", u[b], "RFXSTDTC/RFXENDTC/RFSTDTC",
      paste0(iso(rs[b]), " / ", iso(re[b]), " (EX: ", iso(es[b]), " / ", iso(ee[b]), ")"),
      "Reference exposure dates differ from EX")
})
reg("SDTM-DM-002", "DM", function(st, p) {
  d <- st$DM; b <- which((chr(d$DTHFL) == "Y") != (chr(d$DTHDTC) != ""))
  fnd("SDTM-DM-002", "DM", d$USUBJID[b], "DTHFL/DTHDTC", paste(chr(d$DTHFL)[b], chr(d$DTHDTC)[b], sep = " / "), "Death flag and death date disagree")
})
reg("SDTM-DM-003", c("DM", "EX"), function(st, p) {
  d <- st$DM; e <- st$EX
  tr <- tapply(chr(e$EXTRT), chr(e$USUBJID), function(z) paste(sort(unique(z)), collapse = "+"))
  exp <- toupper(chr(tr[chr(d$USUBJID)])); exp[exp == ""] <- "NOT TREATED"
  b <- which(toupper(chr(d$ACTARM)) != exp)
  fnd("SDTM-DM-003", "DM", d$USUBJID[b], "ACTARM", paste0(d$ACTARM[b], " (EX: ", exp[b], ")"), "Actual arm differs from treatment received")
})
reg("SDTM-EX-001", "EX", function(st, p) {
  e <- st$EX; out <- list()
  for (u in unique(chr(e$USUBJID))) {
    x <- e[chr(e$USUBJID) == u, ]; s <- dt(x$EXSTDTC); en <- dt(x$EXENDTC)
    o <- order(s); s <- s[o]; en <- en[o]; sq <- chr(x$EXSEQ)[o]
    b <- which(!is.na(s) & !is.na(en) & s > en)
    if (length(s) > 1) b <- c(b, which(c(FALSE, s[-1] <= en[-length(en)]) %in% TRUE))
    out[[length(out) + 1]] <- fnd("SDTM-EX-001", "EX", rep(u, length(unique(b))), paste("EXSEQ", sq[unique(b)]),
                                  paste(iso(s[unique(b)]), iso(en[unique(b)])), "Exposure start after end or overlapping records")
  }
  do.call(rbind, out)
})
reg("SDTM-EX-002", "EX", function(st, p) {
  e <- st$EX; b <- which(is.na(num(e$EXDOSE)) | num(e$EXDOSE) <= 0 | chr(e$EXDOSU) == "")
  fnd("SDTM-EX-002", "EX", e$USUBJID[b], paste("EXSEQ", e$EXSEQ[b]), paste(e$EXDOSE[b], chr(e$EXDOSU)[b]), "Dose not positive or units missing")
})
reg("SDTM-AE-001", "AE", function(st, p) {
  a <- st$AE; s <- dt(a$AESTDTC); e <- dt(a$AEENDTC)
  b <- which(!is.na(s) & !is.na(e) & s > e)
  fnd("SDTM-AE-001", "AE", a$USUBJID[b], paste("AESEQ", a$AESEQ[b]), paste(a$AESTDTC[b], a$AEENDTC[b]), "AE start after end")
})
reg("SDTM-AE-002", c("AE", "DM"), function(st, p) {
  a <- st$AE; fat <- chr(a$AEOUT) == "FATAL"; g5 <- chr(a$AETOXGR) == "5"; sd <- chr(a$AESDTH) == "Y"
  dfl <- chr(st$DM$DTHFL)[match(chr(a$USUBJID), chr(st$DM$USUBJID))]
  b <- which(fat != g5 | fat != sd | (fat & dfl %in% c("", NA)))
  fnd("SDTM-AE-002", "AE", a$USUBJID[b], paste("AESEQ", a$AESEQ[b]), paste(a$AEOUT[b], "grade", a$AETOXGR[b], "AESDTH", a$AESDTH[b]),
      "Fatal outcome, grade 5, death criterion and DM death flag disagree")
})
reg("SDTM-AE-003", "AE", function(st, p) {
  a <- st$AE; cr <- intersect(c("AESDTH", "AESHOSP", "AESLIFE", "AESDISAB", "AESCONG", "AESMIE"), names(a))
  any_cr <- Reduce(`|`, lapply(cr, function(v) chr(a[[v]]) == "Y"))
  b <- which((chr(a$AESER) == "Y") != any_cr)
  fnd("SDTM-AE-003", "AE", a$USUBJID[b], paste("AESEQ", a$AESEQ[b]), paste("AESER", a$AESER[b]), "Seriousness flag inconsistent with criteria")
})
reg("SDTM-AE-004", "AE", function(st, p) {
  a <- st$AE; b <- which(chr(a$AEDECOD) == "" | chr(a$AEBODSYS) == "")
  fnd("SDTM-AE-004", "AE", a$USUBJID[b], paste("AESEQ", a$AESEQ[b]), a$AETERM[b], "AE not coded (AEDECOD/AEBODSYS missing)")
})
reg("SDTM-TU-001", "TU", function(st, p) {
  t <- st$TU; k <- paste(t$USUBJID, chr(t$TUEVAL), chr(t$TULNKID))
  b <- which(duplicated(k))
  fnd("SDTM-TU-001", "TU", t$USUBJID[b], paste("TUSEQ", t$TUSEQ[b]), t$TULNKID[b], "Duplicated lesion link identifier")
})
reg("SDTM-TU-002", "TU", function(st, p) {
  t <- st$TU; bl <- chr(t$VISIT) == "BASELINE"; r <- chr(t$TUORRES)
  b <- which((r %in% c("TARGET", "NON-TARGET") & !bl) | (r == "NEW" & bl))
  fnd("SDTM-TU-002", "TU", t$USUBJID[b], paste(t$VISIT[b], t$TULNKID[b]), r[b], "Lesion category identified at the wrong visit")
})
reg("SDTM-TU-003", "TU", function(st, p) {
  t <- st$TU; t <- t[chr(t$TUORRES) == "TARGET" & chr(t$VISIT) == "BASELINE", ]; out <- list()
  for (u in unique(t$USUBJID)) {
    x <- t[t$USUBJID == u, ]; po <- table(chr(x$TULOC))
    if (nrow(x) > 5 || any(po > 2))
      out[[length(out) + 1]] <- fnd("SDTM-TU-003", "TU", u, "TARGET", paste(names(po), po, collapse = ", "), "More than 5 targets or more than 2 per organ")
  }
  do.call(rbind, out)
})
reg("SDTM-TR-001", c("TR", "TU"), function(st, p) {
  r <- st$TR; k <- paste(r$USUBJID, chr(r$TRLNKID)); tk <- paste(st$TU$USUBJID, chr(st$TU$TULNKID))
  b <- which(chr(r$TRLNKID) != "" & !(k %in% tk))
  fnd("SDTM-TR-001", "TR", r$USUBJID[b], paste("TRSEQ", r$TRSEQ[b]), r$TRLNKID[b], "TRLNKID not found in TU")
})
reg("SDTM-TR-002", "TR", function(st, p) {
  r <- st$TR; dia <- chr(r$TRTESTCD) %in% c("LDIAM", "LPERP", "SUMDIAM")
  nd <- chr(r$TRSTAT) == "NOT DONE"
  b <- which(dia & ((chr(r$TRSTRESU) != "mm" & !nd) | (is.na(num(r$TRSTRESN)) & !nd) | (nd & chr(r$TRREASND) == "")))
  fnd("SDTM-TR-002", "TR", r$USUBJID[b], paste("TRSEQ", r$TRSEQ[b]), paste(r$TRSTRESN[b], chr(r$TRSTRESU)[b], chr(r$TRSTAT)[b]),
      "Diameter units not mm, result missing, or not done without reason")
})
reg("SDTM-TR-003", c("TR", "TU"), function(st, p) {
  r <- st$TR; tg <- paste(st$TU$USUBJID, st$TU$TULNKID)[chr(st$TU$TUORRES) == "TARGET"]
  les <- r[chr(r$TRTESTCD) %in% c("LDIAM", "LPERP") & paste(r$USUBJID, r$TRLNKID) %in% tg, ]
  sm <- tapply(num(les$TRSTRESN), paste(les$USUBJID, les$VISITNUM), sum)
  s <- r[chr(r$TRTESTCD) == "SUMDIAM", ]; ex <- sm[paste(s$USUBJID, s$VISITNUM)]
  b <- which(is.na(ex) | is.na(num(s$TRSTRESN)) | num(s$TRSTRESN) != ex)
  fnd("SDTM-TR-003", "TR", s$USUBJID[b], s$VISIT[b], paste0(s$TRSTRESN[b], " (lesions sum ", ex[b], ")"), "SUMDIAM differs from sum of target diameters")
})
reg("SDTM-TR-004", "TR", function(st, p) {
  r <- st$TR; r <- r[chr(r$VISIT) == "BASELINE" & chr(r$TRTESTCD) %in% c("LDIAM", "LPERP"), ]
  v <- num(r$TRSTRESN); b <- which((chr(r$TRTESTCD) == "LDIAM" & v < 10) | (chr(r$TRTESTCD) == "LPERP" & v < 15))
  fnd("SDTM-TR-004", "TR", r$USUBJID[b], paste(r$TRLNKID[b], r$TRTESTCD[b]), paste(v[b], "mm"), "Baseline target lesion not measurable")
})
rs_wide <- function(rs) {
  k <- paste(rs$USUBJID, rs$VISITNUM)
  g <- function(tc) { x <- rs[chr(rs$RSTESTCD) == tc, ]; chr(x$RSSTRESC)[match(unique(k), paste(x$USUBJID, x$VISITNUM))] }
  u <- rs[!duplicated(k), ]
  data.frame(USUBJID = chr(u$USUBJID), VISITNUM = num(u$VISITNUM), VISIT = chr(u$VISIT), TRG = g("TRGRESP"),
             NTRG = g("NTRGRESP"), NEWL = g("NEWLPROG"), OVR = g("OVRLRESP"), stringsAsFactors = FALSE)
}
reg("SDTM-RS-001", "RS", function(st, p) {
  w <- rs_wide(st$RS); ex <- recist_overall(w$TRG, w$NTRG, w$NEWL)
  b <- which(!is.na(w$OVR) & w$OVR != ex)
  fnd("SDTM-RS-001", "RS", w$USUBJID[b], w$VISIT[b], paste0(w$OVR[b], " (Table 1 gives ", ex[b], ")"), "Overall response inconsistent with RECIST 1.1 Table 1")
})
target_expected_sdtm <- function(st) {
  r <- st$TR; tu <- st$TU
  tg <- tu[chr(tu$TUORRES) == "TARGET", ]; tg$nodal <- chr(tg$TULOC) == "LYMPH NODE"
  r <- r[chr(r$TRTESTCD) %in% c("LDIAM", "LPERP") & paste(r$USUBJID, r$TRLNKID) %in% paste(tg$USUBJID, tg$TULNKID), ]
  out <- list()
  for (u in unique(r$USUBJID)) {
    x <- r[r$USUBJID == u, ]; nd <- tg$nodal[match(paste(u, x$TRLNKID), paste(tg$USUBJID, tg$TULNKID))]
    bv <- min(num(x$VISITNUM))
    rs <- recist_target_series(num(x$VISITNUM), chr(x$TRLNKID), num(x$TRSTRESN), nd, bv)
    if (nrow(rs)) out[[length(out) + 1]] <- data.frame(USUBJID = u, VISITNUM = rs$visit, exp = rs$trg)
  }
  do.call(rbind, out)
}
reg("SDTM-RS-002", c("RS", "TR", "TU"), function(st, p) {
  w <- rs_wide(st$RS); e <- target_expected_sdtm(st)
  ex <- e$exp[match(paste(w$USUBJID, w$VISITNUM), paste(e$USUBJID, e$VISITNUM))]
  b <- which(!is.na(ex) & w$TRG != ex)
  fnd("SDTM-RS-002", "RS", w$USUBJID[b], w$VISIT[b], paste0(w$TRG[b], " (TR diameters give ", ex[b], ")"), "Target response inconsistent with TR diameters")
})
reg("SDTM-RS-003", c("RS", "TR"), function(st, p) {
  rs <- st$RS; tr <- st$TR
  mx <- tapply(dt(tr$TRDTC), paste(tr$USUBJID, tr$VISITNUM), function(z) max(z, na.rm = TRUE))
  k <- paste(rs$USUBJID, rs$VISITNUM); m <- as.Date(as.numeric(mx[k]), origin = "1970-01-01")
  b <- which(is.na(m) | dt(rs$RSDTC) != m)
  fnd("SDTM-RS-003", "RS", rs$USUBJID[b], paste(rs$VISIT[b], rs$RSTESTCD[b]), paste0(rs$RSDTC[b], " (TR: ", iso(m[b]), ")"),
      "No tumour results at this visit or response date differs from scan date")
})
reg("SDTM-DS-001", c("DS", "DM"), function(st, p) {
  ds <- st$DS; dm <- st$DM
  de <- ds[chr(ds$DSDECOD) == "DEATH", ]
  dd <- chr(de$DSSTDTC)[match(chr(dm$USUBJID), chr(de$USUBJID))]; dd[is.na(dd)] <- ""
  b <- which(dd != chr(dm$DTHDTC))
  out <- list(fnd("SDTM-DS-001", "DS", dm$USUBJID[b], "DEATH", paste0("DS ", dd[b], " / DM ", chr(dm$DTHDTC)[b]), "Death date in DS differs from DM"))
  ev <- ds[chr(ds$DSCAT) == "DISPOSITION EVENT", ]; k <- paste(ev$USUBJID, chr(ev$EPOCH))
  b2 <- which(duplicated(k))
  out[[2]] <- fnd("SDTM-DS-001", "DS", ev$USUBJID[b2], paste("EPOCH", ev$EPOCH[b2]), ev$DSDECOD[b2], "More than one disposition event in an epoch")
  do.call(rbind, out)
})

# ---- ADaM --------------------------------------------------------------------
reg("ADAM-GEN-001", c("ADSL", "DM"), function(st, p) {
  a <- chr(st$ADSL$USUBJID); d <- chr(st$DM$USUBJID)
  out <- list(fnd("ADAM-GEN-001", "ADSL", unique(a[duplicated(a)]), "USUBJID", "duplicated", "More than one ADSL record for subject"),
              fnd("ADAM-GEN-001", "ADSL", setdiff(d, a), "USUBJID", "in DM only", "Subject in DM missing from ADSL"),
              fnd("ADAM-GEN-001", "ADSL", setdiff(a, d), "USUBJID", "in ADSL only", "Subject in ADSL not in DM"))
  do.call(rbind, out)
})
reg("ADAM-GEN-002", character(), function(st, p) {
  out <- lapply(intersect(c("ADRS", "ADTTE"), names(st)), function(ds) {
    d <- st[[ds]]; pp <- tapply(chr(d$PARAM), chr(d$PARAMCD), function(z) length(unique(z)))
    b <- names(pp)[pp > 1]
    fnd("ADAM-GEN-002", ds, rep("(dataset)", length(b)), paste("PARAMCD", b),
        vapply(b, function(x) paste(unique(chr(d$PARAM[chr(d$PARAMCD) == x])), collapse = " | "), ""), "PARAMCD maps to more than one PARAM")
  })
  do.call(rbind, out)
})
reg("ADAM-GEN-003", "ADSL", function(st, p) {
  s <- st$ADSL; out <- list()
  for (ds in intersect(c("ADRS", "ADTTE", "ADAE"), names(st))) {
    d <- st[[ds]]; m <- match(chr(d$USUBJID), chr(s$USUBJID))
    for (v in intersect(c("TRT01A", "TRTSDT", "TRTEDT", "SAFFL"), names(d))) {
      a <- if (grepl("DT$", v)) iso(dt(d[[v]])) else chr(d[[v]])
      e <- if (grepl("DT$", v)) iso(dt(s[[v]]))[m] else chr(s[[v]])[m]
      b <- which(!is.na(m) & a != e)
      out[[length(out) + 1]] <- fnd("ADAM-GEN-003", ds, d$USUBJID[b], v, paste0(a[b], " (ADSL ", e[b], ")"), "Copied ADSL variable differs from ADSL")
    }
  }
  do.call(rbind, out)
})
reg("ADAM-GEN-004", character(), function(st, p) {
  out <- list()
  chk <- function(ds, v, ok) if (ds %in% names(st) && v %in% names(st[[ds]])) {
    x <- chr(st[[ds]][[v]]); b <- which(!(x %in% ok))
    out[[length(out) + 1]] <<- fnd("ADAM-GEN-004", ds, st[[ds]]$USUBJID[b], v, x[b], "Invalid flag value")
  }
  chk("ADSL", "ITTFL", c("Y", "N")); chk("ADSL", "SAFFL", c("Y", "N")); chk("ADSL", "DTHFL", c("Y", ""))
  chk("ADRS", "ANL01FL", c("Y", "")); chk("ADAE", "TRTEMFL", c("Y", ""))
  do.call(rbind, out)
})
reg("ADAM-GEN-005", c("ADRS", "RS"), function(st, p) {
  a <- st$ADRS; a <- a[chr(a$PARAMCD) == "OVR", ]; r <- st$RS[chr(st$RS$RSTESTCD) == "OVRLRESP", ]
  b <- which(chr(a$SRCDOM) != "RS" | !(paste(a$USUBJID, num(a$SRCSEQ)) %in% paste(r$USUBJID, num(r$RSSEQ))))
  fnd("ADAM-GEN-005", "ADRS", a$USUBJID[b], paste(a$AVISIT[b]), paste(a$SRCDOM[b], a$SRCSEQ[b]), "Source pointer does not resolve to RS OVRLRESP")
})
ex_range <- function(st) {
  e <- st$EX; u <- chr(e$USUBJID)
  list(s = as.Date(tapply(dt(e$EXSTDTC), u, function(z) min(z, na.rm = TRUE)), origin = "1970-01-01"),
       e = as.Date(tapply(dt(e$EXENDTC), u, function(z) max(z, na.rm = TRUE)), origin = "1970-01-01"),
       t = tapply(chr(e$EXTRT), u, function(z) paste(sort(unique(z)), collapse = "+")))
}
reg("ADAM-ADSL-001", c("ADSL", "EX"), function(st, p) {
  s <- st$ADSL; r <- ex_range(st); u <- chr(s$USUBJID)
  es <- iso(r$s[u]); ee <- iso(r$e[u]); as_ <- iso(dt(s$TRTSDT)); ae <- iso(dt(s$TRTEDT))
  es[is.na(es)] <- ""; ee[is.na(ee)] <- ""
  b <- which(as_ != es | ae != ee)
  fnd("ADAM-ADSL-001", "ADSL", u[b], "TRTSDT/TRTEDT", paste0(as_[b], " / ", ae[b], " (EX ", es[b], " / ", ee[b], ")"), "Treatment dates differ from EX")
})
reg("ADAM-ADSL-002", c("ADSL", "EX", "DS"), function(st, p) {
  s <- st$ADSL; u <- chr(s$USUBJID)
  dosed <- u %in% chr(st$EX$USUBJID); rand <- u %in% chr(st$DS$USUBJID[chr(st$DS$DSDECOD) == "RANDOMIZED"])
  b <- which((chr(s$SAFFL) == "Y") != dosed | (chr(s$ITTFL) == "Y") != rand)
  fnd("ADAM-ADSL-002", "ADSL", u[b], "SAFFL/ITTFL", paste(s$SAFFL[b], s$ITTFL[b], sep = " / "), "Population flag inconsistent with dosing or randomisation")
})
reg("ADAM-ADSL-003", c("ADSL", "DM"), function(st, p) {
  s <- st$ADSL; m <- match(chr(s$USUBJID), chr(st$DM$USUBJID))
  dfl <- chr(st$DM$DTHFL)[m]; ddt <- chr(st$DM$DTHDTC)[m]
  b <- which(!is.na(m) & (chr(s$DTHFL) != dfl | iso(dt(s$DTHDT)) != ddt))
  fnd("ADAM-ADSL-003", "ADSL", s$USUBJID[b], "DTHFL/DTHDT", paste0(chr(s$DTHFL)[b], " ", iso(dt(s$DTHDT))[b], " (DM ", dfl[b], " ", ddt[b], ")"),
      "Death flag or date differs from DM")
})
reg("ADAM-ADSL-004", c("ADSL", "EX"), function(st, p) {
  s <- st$ADSL; r <- ex_range(st); e <- toupper(chr(r$t[chr(s$USUBJID)]))
  b <- which(toupper(chr(s$TRT01A)) != e)
  fnd("ADAM-ADSL-004", "ADSL", s$USUBJID[b], "TRT01A", paste0(s$TRT01A[b], " (EX ", e[b], ")"), "Actual treatment differs from EX")
})
reg("ADAM-ADRS-001", c("ADRS", "RS"), function(st, p) {
  a <- st$ADRS; a <- a[chr(a$PARAMCD) == "OVR", ]; w <- rs_wide(st$RS)
  e <- w$OVR[match(paste(a$USUBJID, num(a$AVISITN)), paste(w$USUBJID, w$VISITNUM))]
  b <- which(is.na(e) | chr(a$AVALC) != e)
  fnd("ADAM-ADRS-001", "ADRS", a$USUBJID[b], a$AVISIT[b], paste0(a$AVALC[b], " (RS ", e[b], ")"), "Time-point response differs from RS")
})
bor_expected <- function(st, p) {
  w <- rs_wide(st$RS); dts <- tapply(dt(st$RS$RSDTC), paste(st$RS$USUBJID, st$RS$VISITNUM), function(z) z[1])
  w$date <- as.Date(as.numeric(dts[paste(w$USUBJID, w$VISITNUM)]), origin = "1970-01-01")
  dm <- st$DM; u <- chr(dm$USUBJID)
  res <- lapply(u, function(s) {
    x <- w[w$USUBJID == s, ]; b <- bor_derive(x$date, x$OVR, dt(dm$RFXSTDTC[dm$USUBJID == s]), p)
    data.frame(USUBJID = s, bor = b$bor, adt = b$adt,
               first_resp = if (any(x$OVR %in% c("CR", "PR"))) min(x$date[x$OVR %in% c("CR", "PR")]) else as.Date(NA))
  })
  do.call(rbind, res)
}
reg("ADAM-ADRS-002", c("ADRS", "RS", "DM"), function(st, p) {
  a <- st$ADRS; a <- a[chr(a$PARAMCD) == "BOR", ]; e <- bor_expected(st, p)
  ex <- e$bor[match(chr(a$USUBJID), e$USUBJID)]
  b <- which(!is.na(ex) & chr(a$AVALC) != ex)
  fnd("ADAM-ADRS-002", "ADRS", a$USUBJID[b], "BOR", paste0(a$AVALC[b], " (derived ", ex[b], ")"), "Best overall response differs from SAP derivation")
})
reg("ADAM-ADRS-003", c("ADRS", "ADSL"), function(st, p) {
  itt <- chr(st$ADSL$USUBJID[chr(st$ADSL$ITTFL) == "Y"])
  n <- table(factor(chr(st$ADRS$USUBJID[chr(st$ADRS$PARAMCD) == "BOR"]), levels = unique(itt)))
  b <- names(n)[n != 1]
  fnd("ADAM-ADRS-003", "ADRS", b, "BOR", paste(n[b], "records"), "ITT subject does not have exactly one BOR record")
})
reg("ADAM-ADTTE-001", "ADTTE", function(st, p) {
  a <- st$ADTTE; ex <- as.numeric(dt(a$ADT) - dt(a$STARTDT)) + 1; av <- num(a$AVAL)
  b <- which(is.na(av) | is.na(ex) | av != ex | av <= 0)
  fnd("ADAM-ADTTE-001", "ADTTE", a$USUBJID[b], a$PARAMCD[b], paste0(av[b], " (expected ", ex[b], ")"), "AVAL not equal to ADT - STARTDT + 1")
})
tte_expected <- function(st, p) {
  dm <- st$DM; w <- rs_wide(st$RS)
  dts <- tapply(dt(st$RS$RSDTC), paste(st$RS$USUBJID, st$RS$VISITNUM), function(z) z[1])
  w$date <- as.Date(as.numeric(dts[paste(w$USUBJID, w$VISITNUM)]), origin = "1970-01-01")
  rd <- st$DS[chr(st$DS$DSDECOD) == "RANDOMIZED", ]
  res <- lapply(chr(dm$USUBJID), function(s) {
    x <- w[w$USUBJID == s, ]; rand <- dt(rd$DSSTDTC[chr(rd$USUBJID) == s])[1]
    death <- dt(dm$DTHDTC[dm$USUBJID == s])
    pf <- pfs_derive(rand, x$date, x$OVR, death, p)
    lst <- if ("ADSL" %in% names(st)) dt(st$ADSL$LSTALVDT[chr(st$ADSL$USUBJID) == s])[1] else as.Date(NA)
    data.frame(USUBJID = s, PARAMCD = c("PFS", "OS"), adt = c(pf$adt, if (!is.na(death)) death else lst),
               cnsr = c(pf$cnsr, if (!is.na(death)) 0 else 1), missed = c(pf$missed, FALSE))
  })
  do.call(rbind, res)
}
reg("ADAM-ADTTE-002", c("ADTTE", "RS", "DM", "DS"), function(st, p) {
  a <- st$ADTTE; a <- a[chr(a$PARAMCD) %in% c("PFS", "OS"), ]; e <- tte_expected(st, p)
  m <- match(paste(a$USUBJID, a$PARAMCD), paste(e$USUBJID, e$PARAMCD))
  cn <- num(a$CNSR); ad <- dt(a$ADT)
  miss_case <- e$missed[m] & cn == 0           # reported by ADAM-ADTTE-003
  b <- which(!is.na(m) & !miss_case & (cn != e$cnsr[m] | is.na(ad) | ad != e$adt[m]))
  fnd("ADAM-ADTTE-002", "ADTTE", a$USUBJID[b], a$PARAMCD[b],
      paste0("CNSR ", cn[b], " ", iso(ad[b]), " (derived CNSR ", e$cnsr[m][b], " ", iso(e$adt[m][b]), ")"),
      "Event/censoring status or date differs from SAP derivation")
})
reg("ADAM-ADTTE-003", c("ADTTE", "RS", "DM", "DS"), function(st, p) {
  a <- st$ADTTE; a <- a[chr(a$PARAMCD) == "PFS", ]; e <- tte_expected(st, p); e <- e[e$PARAMCD == "PFS", ]
  m <- match(chr(a$USUBJID), e$USUBJID)
  b <- which(!is.na(m) & e$missed[m] & num(a$CNSR) == 0)
  fnd("ADAM-ADTTE-003", "ADTTE", a$USUBJID[b], "PFS", paste0("event ", iso(dt(a$ADT))[b], "; censor at ", iso(e$adt[m][b])),
      "Event after two or more missed assessments counted as PFS event")
})
reg("ADAM-ADTTE-004", "ADTTE", function(st, p) {
  a <- st$ADTTE; pf <- a[chr(a$PARAMCD) == "PFS", ]; os <- a[chr(a$PARAMCD) == "OS", ]
  m <- match(chr(os$USUBJID), chr(pf$USUBJID))
  b <- which(!is.na(m) & num(os$AVAL) < num(pf$AVAL)[m])
  fnd("ADAM-ADTTE-004", "ADTTE", os$USUBJID[b], "OS vs PFS", paste0("OS ", os$AVAL[b], " < PFS ", pf$AVAL[m][b]), "Overall survival shorter than PFS")
})
reg("ADAM-ADTTE-005", c("ADTTE", "RS", "DM"), function(st, p) {
  a <- st$ADTTE; d <- a[chr(a$PARAMCD) == "DOR", ]; e <- bor_expected(st, p)
  resp <- e$USUBJID[e$bor %in% c("CR", "PR")]
  m <- match(chr(d$USUBJID), e$USUBJID)
  b1 <- which(!(chr(d$USUBJID) %in% resp) | dt(d$STARTDT) != e$first_resp[m])
  b2 <- setdiff(resp, chr(d$USUBJID))
  do.call(rbind, list(
    fnd("ADAM-ADTTE-005", "ADTTE", d$USUBJID[b1], "DOR", paste0("STARTDT ", iso(dt(d$STARTDT))[b1], "; BOR ", e$bor[m][b1]),
        "DOR record for a non-responder or start not at first response"),
    fnd("ADAM-ADTTE-005", "ADTTE", b2, "DOR", "absent", "Confirmed responder without DOR record")))
})
reg("ADAM-ADTTE-006", c("ADTTE", "ADSL"), function(st, p) {
  a <- st$ADTTE; dco <- dt(st$ADSL$DCUTDT)[match(chr(a$USUBJID), chr(st$ADSL$USUBJID))]
  b <- which(!is.na(dco) & dt(a$ADT) > dco)
  fnd("ADAM-ADTTE-006", "ADTTE", a$USUBJID[b], a$PARAMCD[b], iso(dt(a$ADT))[b], "Analysis date after data cut-off")
})
reg("ADAM-ADAE-001", c("ADAE", "ADSL"), function(st, p) {
  a <- st$ADAE; m <- match(chr(a$USUBJID), chr(st$ADSL$USUBJID))
  ts <- dt(st$ADSL$TRTSDT)[m]; te <- dt(st$ADSL$TRTEDT)[m]; as_ <- dt(a$ASTDT)
  ex <- ifelse(!is.na(as_) & !is.na(ts) & as_ >= ts & as_ <= te + p$teae_days, "Y", "")
  b <- which(chr(a$TRTEMFL) != ex)
  fnd("ADAM-ADAE-001", "ADAE", a$USUBJID[b], paste("AESEQ", a$AESEQ[b]), paste0("'", chr(a$TRTEMFL)[b], "' (derived '", ex[b], "')"),
      "Treatment-emergent flag differs from derivation")
})
reg("ADAM-ADAE-002", c("ADAE", "AE"), function(st, p) {
  k1 <- paste(st$AE$USUBJID, num(st$AE$AESEQ)); k2 <- paste(st$ADAE$USUBJID, num(st$ADAE$AESEQ))
  mi <- setdiff(k1, k2); ex <- setdiff(k2, k1); du <- unique(k2[duplicated(k2)])
  sp <- function(k) sub(" .*$", "", k)
  do.call(rbind, list(fnd("ADAM-ADAE-002", "ADAE", sp(mi), mi, "in AE only", "AE record missing from ADAE"),
                      fnd("ADAM-ADAE-002", "ADAE", sp(ex), ex, "in ADAE only", "ADAE record not in AE"),
                      fnd("ADAM-ADAE-002", "ADAE", sp(du), du, "duplicated", "AE record repeated in ADAE")))
})
reg("ADAM-ADAE-003", c("ADAE", "AE"), function(st, p) {
  a <- st$ADAE; src <- chr(st$AE$AESTDTC)[match(paste(a$USUBJID, num(a$AESEQ)), paste(st$AE$USUBJID, num(st$AE$AESEQ)))]
  part <- !is.na(src) & nchar(src) < 10 & src != ""
  b <- which((part & chr(a$ASTDTF) == "") | (!part & chr(a$ASTDTF) != ""))
  fnd("ADAM-ADAE-003", "ADAE", a$USUBJID[b], paste("AESEQ", a$AESEQ[b]), paste0("AESTDTC ", src[b], "; ASTDTF '", chr(a$ASTDTF)[b], "'"),
      "Imputation flag inconsistent with partial start date")
})

# Run every automated check; report checks that could not run and why
run_onc_checks <- function(st, p = ONC_PAR) {
  stat <- list(); res <- list()
  for (id in CHECK_SPEC$Check_ID) {
    sp <- CHECK_SPEC[CHECK_SPEC$Check_ID == id, ]
    if (sp$Automation != "Automated") {
      stat[[id]] <- data.frame(Check_ID = id, Run_status = "Manual review (not automated)", Findings = NA_integer_); next
    }
    ck <- CHECKS[[id]]; miss <- setdiff(ck$needs, names(st))
    if (length(miss)) {
      stat[[id]] <- data.frame(Check_ID = id, Run_status = paste("Not run: sheet absent -", paste(miss, collapse = ", ")),
                               Findings = NA_integer_); next
    }
    r <- tryCatch(ck$fun(st, p), error = function(e) structure(conditionMessage(e), class = "err"))
    if (inherits(r, "err")) {
      stat[[id]] <- data.frame(Check_ID = id, Run_status = paste("Not run: required variable missing or unreadable -", r), Findings = NA_integer_)
    } else {
      n <- if (is.null(r)) 0L else nrow(r)
      stat[[id]] <- data.frame(Check_ID = id, Run_status = if (n) "Run - findings" else "Run - passed", Findings = n)
      if (n) res[[id]] <- r
    }
  }
  f <- if (length(res)) do.call(rbind, res) else
    data.frame(check_id = character(), dataset = character(), usubjid = character(), record = character(),
               value = character(), message = character())
  rownames(f) <- NULL
  # Raw findings carry SUBJID; express all as USUBJID
  rawi <- grepl("^RAW", f$check_id) & !grepl(paste0("^", ONC_PAR$study), f$usubjid)
  f$usubjid[rawi] <- raw_sid(st, f$usubjid[rawi])
  sp <- CHECK_SPEC[match(f$check_id, CHECK_SPEC$Check_ID), ]
  f <- cbind(finding_id = sprintf("F%04d", seq_len(nrow(f))), level = sp$Level, f, severity = sp$Severity)
  status <- do.call(rbind, stat); rownames(status) <- NULL
  list(findings = f, status = merge(CHECK_SPEC[, c("Check_ID", "Level", "Dataset", "Check", "Severity")], status, by = "Check_ID", sort = FALSE))
}

# -----------------------------------------------------------------------------
# 5. Simulated RECIST 1.1 oncology study
# -----------------------------------------------------------------------------
AE_DICT <- data.frame(
  term   = c("Fatigue", "Nausea", "Diarrhoea", "Skin rash", "Anaemia", "Low neutrophils", "Pneumonitis", "Tingling in feet"),
  decod  = c("Fatigue", "Nausea", "Diarrhoea", "Rash", "Anaemia", "Neutrophil count decreased", "Pneumonitis", "Peripheral sensory neuropathy"),
  bodsys = c("General disorders and administration site conditions", "Gastrointestinal disorders", "Gastrointestinal disorders",
             "Skin and subcutaneous tissue disorders", "Blood and lymphatic system disorders", "Investigations",
             "Respiratory, thoracic and mediastinal disorders", "Nervous system disorders"),
  stringsAsFactors = FALSE)

simulate_onc_study <- function(n = 120, seed = 2026, p = ONC_PAR) {
  set.seed(seed)
  STUDY <- p$study; dco <- p$dco
  organs <- c("LUNG", "LIVER", "LYMPH NODE", "ADRENAL GLAND", "PERITONEUM", "KIDNEY")
  types <- c("CR", "PR", "SD", "PD_early", "PD_late", "NEW", "NTPD", "DEATH_NO_PD", "AE_DEATH")
  mult <- list(CR = c(.6, .25, 0, 0, 0, 0), PR = c(.75, .6, .55, .55, .6, .72), SD = c(.95, .9, .92, 1, 1.03, 1.05),
               PD_early = 1.4, PD_late = c(.9, 1, 1.35), NEW = c(.95, .95, 1, 1, 1, 1), NTPD = c(.95, .97, 1, 1, 1, 1),
               DEATH_NO_PD = c(.95, .95), AE_DEATH = rep(.92, 6))

  site <- sample(c("101", "102", "103"), n, TRUE)
  subjid <- sprintf("%s-%03d", site, seq_len(n))
  usub <- paste(STUDY, subjid, sep = "-")
  rand <- as.Date("2025-01-15") + sort(sample(0:300, n, TRUE))
  icd <- rand - sample(3:20, n, TRUE)
  brth <- icd - round(sample(38:79, n, TRUE) * 365.25) - sample(10:300, n, TRUE)
  arm <- sample(rep(c("Drug X", "Standard of care"), length.out = n))
  type <- sample(types, n, TRUE, prob = c(.06, .22, .24, .1, .12, .08, .06, .06, .06))
  type[c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25)] <- c(types, "NEW", "NEW", "DEATH_NO_PD")
  treated <- rep(TRUE, n); treated[n] <- FALSE
  trts <- rand + sample(0:3, n, TRUE); trts[!treated] <- NA

  L <- list(); R <- list(); X <- list(); A <- list(); S <- list()
  for (i in seq_len(n)) {
    u <- usub[i]; ty <- type[i]
    # ---- baseline lesions
    nt <- sample(1:5, 1, prob = c(.2, .3, .25, .15, .1)); if (ty %in% c("PD_early", "PD_late")) nt <- max(nt, 2)
    org <- character()
    for (k in seq_len(nt)) { av <- organs[!organs %in% names(which(table(org) >= 2))]; org <- c(org, sample(av, 1)) }
    nodal <- org == "LYMPH NODE"
    base <- ifelse(nodal, sample(15:35, nt, TRUE), sample(12:55, nt, TRUE))
    ntl <- sample(1:2, 1); ntorg <- sample(organs, ntl, TRUE)
    bdate <- if (treated[i]) trts[i] - sample(3:25, 1) else rand[i] - sample(1:10, 1)
    lid <- sprintf("T%02d", seq_len(nt)); nlid <- sprintf("NT%02d", seq_len(ntl))
    L[[length(L) + 1]] <- rbind(
      data.frame(USUBJID = u, v = 0, date = bdate, lid = lid, ltype = "TARGET", organ = org, nodal = nodal, diam = base, nts = ""),
      data.frame(USUBJID = u, v = 0, date = bdate, lid = nlid, ltype = "NON-TARGET", organ = ntorg, nodal = ntorg == "LYMPH NODE", diam = NA, nts = "PRESENT"))
    death <- as.Date(NA); pd_date <- as.Date(NA); last_scan <- bdate
    ae5 <- as.Date(NA)
    if (ty == "AE_DEATH" && treated[i]) { ae5 <- trts[i] + sample(60:150, 1); death <- ae5 + 5 }
    if (treated[i]) {
      mm <- mult[[ty]]; nadir_hist <- list()
      vis_l <- list(); vdat <- list()
      for (v in seq_along(mm)) {
        sd <- trts[i] + p$scan_interval * v + sample(-5:5, 1)
        if (sd > dco || (!is.na(death) && sd >= death)) break
        if (ty == "DEATH_NO_PD" && v > 2) break
        d <- round(base * mm[v] * (1 + rnorm(nt, 0, .03)))
        d[nodal & mm[v] == 0] <- 8; d[!nodal & mm[v] == 0] <- 0; d <- pmax(d, 0)
        nts <- rep(if (ty == "CR" && v >= 3) "ABSENT" else if (ty == "NTPD" && v == 2) "UNEQUIVOCAL PROGRESSION" else "PRESENT", ntl)
        newl <- ty == "NEW" && v == 3
        rows <- rbind(
          data.frame(USUBJID = u, v = v, date = sd, lid = lid, ltype = "TARGET", organ = org, nodal = nodal, diam = d, nts = ""),
          data.frame(USUBJID = u, v = v, date = sd, lid = nlid, ltype = "NON-TARGET", organ = ntorg, nodal = ntorg == "LYMPH NODE", diam = NA, nts = nts))
        if (newl) rows <- rbind(rows, data.frame(USUBJID = u, v = v, date = sd, lid = "NEW01", ltype = "NEW", organ = "LIVER",
                                                 nodal = FALSE, diam = NA, nts = "PRESENT"))
        vis_l[[v]] <- rows
        # responses
        allt <- do.call(rbind, c(list(L[[length(L)]][L[[length(L)]]$ltype == "TARGET", ]), lapply(vis_l, function(z) z[z$ltype == "TARGET", ])))
        ts <- recist_target_series(allt$v, allt$lid, allt$diam, allt$nodal, 0)
        trg <- ts$trg[ts$visit == v]
        ntrg <- if (all(nts == "ABSENT")) "CR" else if (any(nts == "UNEQUIVOCAL PROGRESSION")) "PD" else "NON-CR/NON-PD"
        ov <- recist_overall(trg, ntrg, if (newl) "Y" else "N")
        R[[length(R) + 1]] <- data.frame(USUBJID = u, v = v, date = sd, trg = trg, ntrg = ntrg, newl = if (newl) "Y" else "N", ovr = ov)
        last_scan <- sd
        if (ov == "PD") { pd_date <- sd; break }
      }
      if (length(vis_l)) L[[length(L) + 1]] <- do.call(rbind, vis_l)
    }
    # ---- death after progression or without progression
    if (ty == "DEATH_NO_PD" && treated[i]) death <- last_scan + 160
    if (!is.na(pd_date) && runif(1) < .45) death <- pd_date + sample(30:250, 1)
    if (!is.na(death) && death > dco) death <- as.Date(NA)
    # ---- treatment end
    eot <- min(c(pd_date, death, if (!is.na(ae5)) ae5, dco + 1), na.rm = TRUE)
    # ---- adverse events
    na <- if (treated[i]) rpois(1, 2.2) else 0
    ae <- NULL
    if (na > 0) {
      hi <- as.numeric(min(eot, dco) - trts[i])
      st_ <- trts[i] + sample(-10:max(-9, hi), na, TRUE)
      g <- sample(1:4, na, TRUE, prob = c(.45, .3, .2, .05))
      tm <- sample(seq_len(nrow(AE_DICT) - 1), na, TRUE)
      ae <- data.frame(USUBJID = u, start = st_, end = st_ + sample(3:40, na, TRUE), grade = g, ti = tm)
    }
    if (!is.na(ae5)) ae <- rbind(ae, data.frame(USUBJID = u, start = ae5, end = death, grade = 5, ti = 7))
    if (!is.null(ae)) {
      ae <- ae[order(ae$start), ]
      lim <- if (!is.na(death)) death else dco
      ae <- ae[ae$start <= lim, ]
      ae$ongoing <- ae$end > lim & ae$grade < 5
      ae$end[ae$ongoing] <- NA
      ae$ser <- ifelse(ae$grade == 5, "Y", ifelse(ae$grade >= 3 & runif(nrow(ae)) < .4, "Y", "N"))
      ae$hosp <- ifelse(ae$ser == "Y" & ae$grade < 5, "Y", "N"); ae$sdth <- ifelse(ae$grade == 5, "Y", "N")
      ae$out <- ifelse(ae$grade == 5, "FATAL", ifelse(ae$ongoing, "NOT RECOVERED/NOT RESOLVED", "RECOVERED/RESOLVED"))
      ae$rel <- sample(c("RELATED", "NOT RELATED"), nrow(ae), TRUE)
      ae$partial <- FALSE
    }
    # ---- dosing
    ex <- NULL
    if (treated[i]) {
      cs <- trts[i] + 21 * (0:200); cs <- cs[cs < eot & cs <= dco]
      ex <- data.frame(USUBJID = u, cycle = seq_along(cs), date = cs, dose = p$dose_levels[[arm[i]]][1], adj = "N", reason = "")
      if (!is.null(ae) && nrow(ae)) {
        ae$action <- ifelse(ae$start < trts[i], "NOT APPLICABLE", ifelse(ae$grade == 5, "DRUG WITHDRAWN",
                     ifelse(ae$grade == 4, "DRUG INTERRUPTED", "DOSE NOT CHANGED")))
        g3 <- which(ae$grade == 3 & ae$start >= trts[i] & ae$start < max(cs))[1]
        if (!is.na(g3)) {
          ae$action[g3] <- "DOSE REDUCED"
          r <- ex$date > ae$start[g3]
          ex$dose[r] <- p$dose_levels[[arm[i]]][2]; ex$adj[r] <- "Y"; ex$reason[r] <- "ADVERSE EVENT"
        }
      }
    }
    lastcont <- if (!is.na(death)) death else if (!treated[i]) rand[i] + 5 else
      max(c(dco - sample(0:20, 1), last_scan, if (!is.null(ex)) max(ex$date), if (!is.null(ae) && nrow(ae)) max(ae$start)))
    lastcont <- min(lastcont, dco)
    eotreas <- if (!treated[i]) "WITHDRAWAL BY SUBJECT" else if (!is.na(pd_date) && (is.na(death) || pd_date <= death)) "PROGRESSIVE DISEASE" else
      if (!is.na(death)) "DEATH" else ""
    eotdt <- if (!treated[i]) rand[i] + 5 else if (eotreas == "PROGRESSIVE DISEASE") pd_date else if (eotreas == "DEATH") death else as.Date(NA)
    X[[i]] <- ex; A[[i]] <- ae
    S[[i]] <- data.frame(USUBJID = u, SUBJID = subjid[i], site = site[i], brth = brth[i], icd = icd[i], rand = rand[i],
                         arm = arm[i], treated = treated[i], trts = trts[i],
                         trte = if (!is.null(ex)) max(ex$date) else as.Date(NA), death = death, lastcont = lastcont,
                         eotreas = eotreas, eotdt = eotdt, type = ty,
                         sex = sample(c("M", "F"), 1), race = sample(c("WHITE", "ASIAN", "BLACK OR AFRICAN AMERICAN"), 1, prob = c(.7, .2, .1)),
                         ecog = sample(c("0", "1"), 1))
  }
  S <- do.call(rbind, S); L <- do.call(rbind, L); R <- do.call(rbind, R); X <- do.call(rbind, X)
  A <- do.call(rbind, A); A <- A[order(A$USUBJID, A$start), ]
  A$aenum <- ave(seq_len(nrow(A)), A$USUBJID, FUN = seq_along)
  # one partial AE start date (unknown day), well after first dose
  cand <- which(A$grade < 5 & A$start >= S$trts[match(A$USUBJID, S$USUBJID)] + 62)
  A$partial[cand[1]] <- TRUE

  # ---- raw (EDC export) ----------------------------------------------------
  sid <- function(u) S$SUBJID[match(u, S$USUBJID)]
  raw <- list()
  raw$RAW_DM <- data.frame(SUBJID = S$SUBJID, SITEID = S$site, BRTHDT = fmt_raw(S$brth), SEX = S$sex, RACE = S$race,
                           ICDT = fmt_raw(S$icd), RANDDT = fmt_raw(S$rand), ARM = S$arm, ECOG = S$ecog, MEASDIS = "Y")
  raw$RAW_EX <- data.frame(SUBJID = sid(X$USUBJID), CYCLE = X$cycle, EXSTDT = fmt_raw(X$date), EXENDT = fmt_raw(X$date),
                           DOSE = X$dose, DOSEU = "mg", DOSEADJ = X$adj, ADJREAS = X$reason)
  Lr <- L[order(L$USUBJID, L$v, L$lid), ]
  raw$RAW_TUMOR <- data.frame(SUBJID = sid(Lr$USUBJID), VISIT = ifelse(Lr$v == 0, "BASELINE", paste("WEEK", 8 * Lr$v)),
                              SCANDT = fmt_raw(Lr$date), LESIONID = Lr$lid, LESTYPE = Lr$ltype, ORGAN = Lr$organ,
                              NODAL = ifelse(Lr$nodal, "Y", "N"), METHOD = "CT", DIAM_MM = Lr$diam, NTSTATUS = Lr$nts)
  raw$RAW_RESP <- data.frame(SUBJID = sid(R$USUBJID), VISIT = paste("WEEK", 8 * R$v), RESPDT = fmt_raw(R$date),
                             TRGRESP = R$trg, NTRGRESP = R$ntrg, NEWLES = R$newl, OVRLRESP = R$ovr)
  ast <- ifelse(A$partial, sprintf("UN-%s-%s", MON[as.integer(format(A$start, "%m"))], format(A$start, "%Y")), fmt_raw(A$start))
  raw$RAW_AE <- data.frame(SUBJID = sid(A$USUBJID), AENUM = A$aenum, AETERM = AE_DICT$term[A$ti], AESTDT = ast,
                           AEENDT = fmt_raw(A$end), ONGOING = ifelse(A$ongoing, "Y", "N"), GRADE = A$grade, SERIOUS = A$ser,
                           HOSP = A$hosp, OUTCOME = A$out, ACTION = A$action, RELATED = A$rel)
  raw$RAW_DS <- data.frame(SUBJID = S$SUBJID, EOTREAS = S$eotreas, EOTDT = fmt_raw(S$eotdt), DTHDT = fmt_raw(S$death),
                           DTHCAUSE = ifelse(is.na(S$death), "", ifelse(S$type == "AE_DEATH", "ADVERSE EVENT", "DISEASE PROGRESSION")),
                           LASTCONTDT = fmt_raw(S$lastcont))

  # ---- SDTM -----------------------------------------------------------------
  ref <- S$trts; names(ref) <- S$USUBJID
  sd <- list()
  armcd <- function(a) ifelse(a == "Drug X", "DRUGX", "SOC")
  sd$DM <- data.frame(STUDYID = STUDY, DOMAIN = "DM", USUBJID = S$USUBJID, SUBJID = S$SUBJID,
                      RFSTDTC = iso(S$trts), RFENDTC = iso(S$trte), RFXSTDTC = iso(S$trts), RFXENDTC = iso(S$trte),
                      RFICDTC = iso(S$icd), DTHDTC = iso(S$death), DTHFL = ifelse(is.na(S$death), "", "Y"),
                      SITEID = S$site, BRTHDTC = iso(S$brth), AGE = floor(as.numeric(S$icd - S$brth) / 365.25), AGEU = "YEARS",
                      SEX = S$sex, RACE = S$race, ARMCD = armcd(S$arm), ARM = S$arm,
                      ACTARMCD = ifelse(S$treated, armcd(S$arm), "NOTTRT"), ACTARM = ifelse(S$treated, S$arm, "Not Treated"),
                      COUNTRY = "GBR")
  xt <- toupper(S$arm[match(X$USUBJID, S$USUBJID)])
  sd$EX <- data.frame(STUDYID = STUDY, DOMAIN = "EX", USUBJID = X$USUBJID, EXSEQ = ave(seq_len(nrow(X)), X$USUBJID, FUN = seq_along),
                      EXTRT = xt, EXDOSE = X$dose, EXDOSU = "mg", EXDOSFRM = "INJECTION", EXROUTE = "INTRAVENOUS",
                      EXADJ = ifelse(X$adj == "Y", "ADVERSE EVENT", ""), VISITNUM = X$cycle, VISIT = paste("CYCLE", X$cycle, "DAY 1"),
                      EXSTDTC = iso(X$date), EXENDTC = iso(X$date),
                      EXSTDY = study_day(X$date, ref[X$USUBJID]), EXENDY = study_day(X$date, ref[X$USUBJID]))
  aes <- ifelse(A$partial, format(A$start, "%Y-%m"), iso(A$start))
  sd$AE <- data.frame(STUDYID = STUDY, DOMAIN = "AE", USUBJID = A$USUBJID, AESEQ = A$aenum, AETERM = AE_DICT$term[A$ti],
                      AEDECOD = AE_DICT$decod[A$ti], AEBODSYS = AE_DICT$bodsys[A$ti], AETOXGR = as.character(A$grade),
                      AESER = A$ser, AESDTH = A$sdth, AESHOSP = A$hosp, AESLIFE = "N", AESDISAB = "N", AESCONG = "N", AESMIE = "N",
                      AEACN = A$action, AEREL = A$rel, AEOUT = A$out, AESTDTC = aes, AEENDTC = iso(A$end),
                      AESTDY = ifelse(A$partial, NA, study_day(A$start, ref[A$USUBJID])), AEENDY = study_day(A$end, ref[A$USUBJID]))
  vis <- function(v) ifelse(v == 0, "BASELINE", paste("WEEK", 8 * v))
  tu <- L[L$v == 0 | L$ltype == "NEW", ]; tu <- tu[order(tu$USUBJID, tu$v, tu$lid), ]
  sd$TU <- data.frame(STUDYID = STUDY, DOMAIN = "TU", USUBJID = tu$USUBJID, TUSEQ = ave(seq_len(nrow(tu)), tu$USUBJID, FUN = seq_along),
                      TULNKID = tu$lid, TUTESTCD = "TUMIDENT", TUTEST = "Tumor Identification", TUORRES = tu$ltype, TUSTRESC = tu$ltype,
                      TULOC = tu$organ, TUMETHOD = "CT", TUEVAL = "INVESTIGATOR", VISITNUM = tu$v + 1, VISIT = vis(tu$v),
                      TUDTC = iso(tu$date), TUDY = study_day(tu$date, ref[tu$USUBJID]))
  tg <- L[L$ltype == "TARGET", ]
  sm <- aggregate(diam ~ USUBJID + v + date, tg, sum)
  trr <- rbind(
    data.frame(USUBJID = tg$USUBJID, v = tg$v, date = tg$date, lid = tg$lid, tc = ifelse(tg$nodal, "LPERP", "LDIAM"),
               tt = ifelse(tg$nodal, "Longest Perpendicular", "Longest Diameter"), res = as.character(tg$diam), n = tg$diam, u = "mm"),
    data.frame(USUBJID = L$USUBJID[L$ltype != "TARGET"], v = L$v[L$ltype != "TARGET"], date = L$date[L$ltype != "TARGET"],
               lid = L$lid[L$ltype != "TARGET"], tc = "TUMSTATE", tt = "Tumor State", res = L$nts[L$ltype != "TARGET"], n = NA, u = ""),
    data.frame(USUBJID = sm$USUBJID, v = sm$v, date = sm$date, lid = "", tc = "SUMDIAM", tt = "Sum of Diameter",
               res = as.character(sm$diam), n = sm$diam, u = "mm"))
  trr <- trr[order(trr$USUBJID, trr$v, trr$tc == "SUMDIAM", trr$lid), ]
  sd$TR <- data.frame(STUDYID = STUDY, DOMAIN = "TR", USUBJID = trr$USUBJID, TRSEQ = ave(seq_len(nrow(trr)), trr$USUBJID, FUN = seq_along),
                      TRLNKID = trr$lid, TRTESTCD = trr$tc, TRTEST = trr$tt, TRORRES = trr$res, TRORRESU = trr$u,
                      TRSTRESC = trr$res, TRSTRESN = trr$n, TRSTRESU = trr$u, TRSTAT = "", TRREASND = "", TRMETHOD = "CT",
                      TREVAL = "INVESTIGATOR", VISITNUM = trr$v + 1, VISIT = vis(trr$v), TRDTC = iso(trr$date),
                      TRDY = study_day(trr$date, ref[trr$USUBJID]))
  rsl <- do.call(rbind, lapply(c("TRGRESP", "NTRGRESP", "NEWLPROG", "OVRLRESP"), function(tc) {
    v <- switch(tc, TRGRESP = R$trg, NTRGRESP = R$ntrg, NEWLPROG = R$newl, OVRLRESP = R$ovr)
    data.frame(USUBJID = R$USUBJID, v = R$v, date = R$date, tc = tc, res = v,
               tt = switch(tc, TRGRESP = "Target Response", NTRGRESP = "Non-target Response", NEWLPROG = "New Lesion Progression",
                           OVRLRESP = "Overall Response"))
  }))
  rsl <- rsl[order(rsl$USUBJID, rsl$v, match(rsl$tc, c("TRGRESP", "NTRGRESP", "NEWLPROG", "OVRLRESP"))), ]
  sd$RS <- data.frame(STUDYID = STUDY, DOMAIN = "RS", USUBJID = rsl$USUBJID, RSSEQ = ave(seq_len(nrow(rsl)), rsl$USUBJID, FUN = seq_along),
                      RSTESTCD = rsl$tc, RSTEST = rsl$tt, RSCAT = "RECIST 1.1", RSORRES = rsl$res, RSSTRESC = rsl$res,
                      RSEVAL = "INVESTIGATOR", VISITNUM = rsl$v + 1, VISIT = vis(rsl$v), RSDTC = iso(rsl$date),
                      RSDY = study_day(rsl$date, ref[rsl$USUBJID]))
  ds <- rbind(
    data.frame(USUBJID = S$USUBJID, term = "INFORMED CONSENT OBTAINED", decod = "INFORMED CONSENT OBTAINED", cat = "PROTOCOL MILESTONE", epoch = "SCREENING", d = S$icd),
    data.frame(USUBJID = S$USUBJID, term = "RANDOMIZED", decod = "RANDOMIZED", cat = "PROTOCOL MILESTONE", epoch = "SCREENING", d = S$rand),
    data.frame(USUBJID = S$USUBJID, term = S$eotreas, decod = S$eotreas, cat = "DISPOSITION EVENT", epoch = "TREATMENT", d = S$eotdt)[S$eotreas != "", ],
    data.frame(USUBJID = S$USUBJID, term = "DEATH", decod = "DEATH", cat = "DISPOSITION EVENT", epoch = "FOLLOW-UP", d = S$death)[
      !is.na(S$death) & S$eotreas != "DEATH", ])
  ds <- ds[order(ds$USUBJID, ds$d), ]
  sd$DS <- data.frame(STUDYID = STUDY, DOMAIN = "DS", USUBJID = ds$USUBJID, DSSEQ = ave(seq_len(nrow(ds)), ds$USUBJID, FUN = seq_along),
                      DSTERM = ds$term, DSDECOD = ds$decod, DSCAT = ds$cat, EPOCH = ds$epoch, DSSTDTC = iso(ds$d),
                      DSSTDY = study_day(ds$d, ref[ds$USUBJID]))

  # ---- ADaM -----------------------------------------------------------------
  ad <- list()
  ad$ADSL <- data.frame(STUDYID = STUDY, USUBJID = S$USUBJID, SUBJID = S$SUBJID, SITEID = S$site, AGE = sd$DM$AGE, AGEU = "YEARS",
                        SEX = S$sex, RACE = S$race, ARM = S$arm, ACTARM = sd$DM$ACTARM, TRT01P = S$arm,
                        TRT01A = ifelse(S$treated, S$arm, ""), RANDDT = S$rand, TRTSDT = S$trts, TRTEDT = S$trte,
                        ITTFL = "Y", SAFFL = ifelse(S$treated, "Y", "N"), DTHFL = ifelse(is.na(S$death), "", "Y"),
                        DTHDT = S$death, LSTALVDT = S$lastcont, DCUTDT = dco)
  adsl <- ad$ADSL
  rso <- sd$RS[sd$RS$RSTESTCD == "OVRLRESP", ]
  avn <- c(CR = 1, PR = 2, SD = 3, PD = 4, NE = 5)
  m <- match(rso$USUBJID, adsl$USUBJID)
  ovr <- data.frame(STUDYID = STUDY, USUBJID = rso$USUBJID, TRT01A = adsl$TRT01A[m], TRTSDT = adsl$TRTSDT[m],
                    PARAMCD = "OVR", PARAM = "Overall Response by Investigator", AVALC = rso$RSSTRESC,
                    AVAL = unname(avn[rso$RSSTRESC]), ADT = dt(rso$RSDTC), AVISIT = rso$VISIT, AVISITN = rso$VISITNUM,
                    ANL01FL = "Y", SRCDOM = "RS", SRCSEQ = rso$RSSEQ)
  stl <- c(sd, ad["ADSL"])
  be <- bor_expected(stl, p)
  mb <- match(be$USUBJID, adsl$USUBJID)
  bor <- data.frame(STUDYID = STUDY, USUBJID = be$USUBJID, TRT01A = adsl$TRT01A[mb], TRTSDT = adsl$TRTSDT[mb],
                    PARAMCD = "BOR", PARAM = "Best Confirmed Overall Response by Investigator", AVALC = be$bor,
                    AVAL = unname(avn[be$bor]), ADT = be$adt, AVISIT = "", AVISITN = NA, ANL01FL = "Y", SRCDOM = "ADRS", SRCSEQ = NA)
  ad$ADRS <- rbind(ovr, bor); ad$ADRS <- ad$ADRS[order(ad$ADRS$USUBJID, ad$ADRS$PARAMCD == "BOR", ad$ADRS$AVISITN), ]
  te <- tte_expected(stl, p)
  rnd <- adsl$RANDDT[match(te$USUBJID, adsl$USUBJID)]
  desc <- mapply(function(s, pc, c) {
    if (pc == "OS") return(if (c == 0) "Death" else "Last known alive")
    x <- rso[rso$USUBJID == s, ]; dth <- adsl$DTHDT[adsl$USUBJID == s]
    pfs_derive(adsl$RANDDT[adsl$USUBJID == s], dt(x$RSDTC), x$RSSTRESC, dth, p)$desc
  }, te$USUBJID, te$PARAMCD, te$cnsr)
  tte <- data.frame(USUBJID = te$USUBJID, PARAMCD = te$PARAMCD,
                    PARAM = ifelse(te$PARAMCD == "PFS", "Progression-free survival (days)", "Overall survival (days)"),
                    STARTDT = rnd, ADT = te$adt, CNSR = te$cnsr, EVNTDESC = ifelse(te$cnsr == 0, desc, ""),
                    CNSDTDSC = ifelse(te$cnsr == 1, desc, ""))
  rsp <- be[be$bor %in% c("CR", "PR"), ]
  pf <- te[te$PARAMCD == "PFS", ]; mp <- match(rsp$USUBJID, pf$USUBJID)
  if (nrow(rsp)) tte <- rbind(tte, data.frame(USUBJID = rsp$USUBJID, PARAMCD = "DOR", PARAM = "Duration of response (days)",
                                              STARTDT = rsp$first_resp, ADT = pf$adt[mp], CNSR = pf$cnsr[mp],
                                              EVNTDESC = tte$EVNTDESC[tte$PARAMCD == "PFS"][mp], CNSDTDSC = tte$CNSDTDSC[tte$PARAMCD == "PFS"][mp]))
  tte$AVAL <- as.numeric(tte$ADT - tte$STARTDT) + 1
  mt <- match(tte$USUBJID, adsl$USUBJID)
  ad$ADTTE <- data.frame(STUDYID = STUDY, USUBJID = tte$USUBJID, TRT01P = adsl$TRT01P[mt], TRT01A = adsl$TRT01A[mt],
                         PARAMCD = tte$PARAMCD, PARAM = tte$PARAM, STARTDT = tte$STARTDT, ADT = tte$ADT, AVAL = tte$AVAL,
                         CNSR = tte$CNSR, EVNTDESC = tte$EVNTDESC, CNSDTDSC = tte$CNSDTDSC, SRCDOM = "RS")
  ad$ADTTE <- ad$ADTTE[order(ad$ADTTE$USUBJID, match(ad$ADTTE$PARAMCD, c("PFS", "OS", "DOR"))), ]
  ae <- sd$AE; ma <- match(ae$USUBJID, adsl$USUBJID)
  astdt <- ifelse(nchar(ae$AESTDTC) == 7, paste0(ae$AESTDTC, "-01"), ae$AESTDTC)
  astdt <- dt(astdt)
  ad$ADAE <- data.frame(STUDYID = STUDY, USUBJID = ae$USUBJID, AESEQ = ae$AESEQ, TRT01A = adsl$TRT01A[ma], TRTSDT = adsl$TRTSDT[ma],
                        TRTEDT = adsl$TRTEDT[ma], SAFFL = adsl$SAFFL[ma], AETERM = ae$AETERM, AEDECOD = ae$AEDECOD,
                        AEBODSYS = ae$AEBODSYS, AETOXGR = ae$AETOXGR, AETOXGRN = as.numeric(ae$AETOXGR), AESER = ae$AESER,
                        AEOUT = ae$AEOUT, ASTDT = astdt, ASTDTF = ifelse(nchar(ae$AESTDTC) == 7, "D", ""),
                        AENDT = dt(ae$AEENDTC),
                        TRTEMFL = ifelse(astdt >= adsl$TRTSDT[ma] & astdt <= adsl$TRTEDT[ma] + p$teae_days, "Y", ""))
  ad$ADAE$TRTEMFL[is.na(ad$ADAE$TRTEMFL)] <- ""
  out <- c(raw, sd, ad)
  lapply(out, function(z) { rownames(z) <- NULL; z })
}

# -----------------------------------------------------------------------------
# 6. Seeded errors
# -----------------------------------------------------------------------------
# Each seed: primary check, level, description, candidate selector, modifier.
# Raw seeds use their own subject pool; SDTM and ADaM seeds share a pool so that
# cross-level comparisons are not disturbed by two errors on one subject.

sid_of <- function(u) sub(paste0("^", ONC_PAR$study, "-"), "", u)
rows_of <- function(d, u, col = "USUBJID") which(chr(d[[col]]) == u)

SEEDS <- list()
seed <- function(id, level, desc, select, apply) SEEDS[[length(SEEDS) + 1]] <<- list(check = id, level = level, desc = desc, select = select, apply = apply)
all_u <- function(st) chr(st$DM$USUBJID)
treated_u <- function(st) chr(st$DM$USUBJID[chr(st$DM$ACTARM) != "Not Treated"])

# ---- raw seeds
seed("RAW-001", "Raw", "Demography row entered twice", function(st) all_u(st),
     function(st, u) { i <- rows_of(st$RAW_DM, sid_of(u), "SUBJID"); st$RAW_DM <- rbind(st$RAW_DM, st$RAW_DM[i, ]); st })
seed("RAW-002", "Raw", "Consent date entered 3 days after randomisation", function(st) all_u(st),
     function(st, u) { i <- rows_of(st$RAW_DM, sid_of(u), "SUBJID"); st$RAW_DM$ICDT[i] <- fmt_raw(parse_raw(st$RAW_DM$RANDDT[i]) + 3); st })
seed("RAW-003", "Raw", "Birth year mistyped: age at consent 16", function(st) all_u(st),
     function(st, u) { i <- rows_of(st$RAW_DM, sid_of(u), "SUBJID"); st$RAW_DM$BRTHDT[i] <- fmt_raw(parse_raw(st$RAW_DM$ICDT[i]) - round(16.5 * 365.25)); st })
seed("RAW-004", "Raw", "ECOG 2 recorded for a randomised subject", function(st) all_u(st),
     function(st, u) { st$RAW_DM$ECOG[rows_of(st$RAW_DM, sid_of(u), "SUBJID")] <- "2"; st })
seed("RAW-005", "Raw", "Impossible dose end date (31-APR)", function(st) unique(paste(ONC_PAR$study, st$RAW_EX$SUBJID[num(st$RAW_EX$CYCLE) == 3], sep = "-")),
     function(st, u) { i <- which(st$RAW_EX$SUBJID == sid_of(u) & num(st$RAW_EX$CYCLE) == 2)
       st$RAW_EX$EXENDT[i] <- paste0("31-APR-", substr(st$RAW_EX$EXENDT[i], 8, 11)); st })
seed("RAW-006", "Raw", "First dose dated 2 days before randomisation", function(st) treated_u(st),
     function(st, u) { s <- sid_of(u); i <- which(st$RAW_EX$SUBJID == s & num(st$RAW_EX$CYCLE) == 1)
       d <- fmt_raw(parse_raw(st$RAW_DM$RANDDT[st$RAW_DM$SUBJID == s]) - 2); st$RAW_EX$EXSTDT[i] <- d; st$RAW_EX$EXENDT[i] <- d; st })
seed("RAW-007", "Raw", "Dose of 175 mg (not a protocol dose level)", function(st) unique(paste(ONC_PAR$study, st$RAW_EX$SUBJID[num(st$RAW_EX$CYCLE) == 2], sep = "-")),
     function(st, u) { st$RAW_EX$DOSE[which(st$RAW_EX$SUBJID == sid_of(u) & num(st$RAW_EX$CYCLE) == 2)] <- 175; st })
seed("RAW-008", "Raw", "Cycle 3 entered as cycle 2", function(st) unique(paste(ONC_PAR$study, st$RAW_EX$SUBJID[num(st$RAW_EX$CYCLE) == 4], sep = "-")),
     function(st, u) { st$RAW_EX$CYCLE[which(st$RAW_EX$SUBJID == sid_of(u) & num(st$RAW_EX$CYCLE) == 3)] <- 2; st })
seed("RAW-009", "Raw", "Baseline scan 40 days before first dose", function(st) treated_u(st),
     function(st, u) { s <- sid_of(u); fd <- min(parse_raw(st$RAW_EX$EXSTDT[st$RAW_EX$SUBJID == s]))
       st$RAW_TUMOR$SCANDT[st$RAW_TUMOR$SUBJID == s & st$RAW_TUMOR$VISIT == "BASELINE"] <- fmt_raw(fd - 40); st })
three_nonnodal <- function(t, stype = "LESTYPE", sid = "SUBJID", vis = "VISIT", nod = function(x) chr(x$NODAL) == "N") {
  b <- t[chr(t[[vis]]) == "BASELINE" & chr(t[[stype]]) == "TARGET", ]
  b <- b[nod(b), ]; n <- table(b[[sid]]); names(n)[n >= 3]
}
seed("RAW-010", "Raw", "Three target lesions recorded in the same organ", function(st) paste(ONC_PAR$study, three_nonnodal(st$RAW_TUMOR), sep = "-"),
     function(st, u) { s <- sid_of(u); t <- st$RAW_TUMOR
       ids <- head(unique(t$LESIONID[t$SUBJID == s & t$VISIT == "BASELINE" & t$LESTYPE == "TARGET" & t$NODAL == "N"]), 3)
       st$RAW_TUMOR$ORGAN[t$SUBJID == s & t$LESIONID %in% ids] <- "LIVER"; st })
two_nonnodal <- function(t) { b <- t[t$VISIT == "BASELINE" & t$LESTYPE == "TARGET" & t$NODAL == "N" & num(t$DIAM_MM) >= 13, ]
  n <- table(b$SUBJID); names(n)[n >= 2] }
seed("RAW-011", "Raw", "Baseline target lesion of 8 mm (not measurable); sum of diameters unchanged", function(st) paste(ONC_PAR$study, two_nonnodal(st$RAW_TUMOR), sep = "-"),
     function(st, u) { s <- sid_of(u); t <- st$RAW_TUMOR
       i <- which(t$SUBJID == s & t$VISIT == "BASELINE" & t$LESTYPE == "TARGET" & t$NODAL == "N" & num(t$DIAM_MM) >= 13)[1:2]
       d <- num(t$DIAM_MM[i[1]]); st$RAW_TUMOR$DIAM_MM[i[1]] <- 8; st$RAW_TUMOR$DIAM_MM[i[2]] <- num(t$DIAM_MM[i[2]]) + d - 8; st })
last_visit_subj <- function(st) { r <- st$RAW_RESP; unique(paste(ONC_PAR$study, r$SUBJID[r$VISIT == "WEEK 16"], sep = "-")) }
seed("RAW-012", "Raw", "Target lesion measurement left blank at the last assessment", last_visit_subj,
     function(st, u) { s <- sid_of(u); t <- st$RAW_TUMOR; lv <- tail(sort(num(sub("WEEK ", "", t$VISIT[t$SUBJID == s & t$VISIT != "BASELINE"]))), 1)
       i <- which(t$SUBJID == s & t$VISIT == paste("WEEK", lv) & t$LESTYPE == "TARGET")[1]; st$RAW_TUMOR$DIAM_MM[i] <- NA; st })
seed("RAW-013", "Raw", "Follow-up target lesion T09 not identified at baseline", last_visit_subj,
     function(st, u) { s <- sid_of(u); t <- st$RAW_TUMOR; i <- which(t$SUBJID == s & t$VISIT == "WEEK 8" & t$LESTYPE == "TARGET")[1]
       r <- t[i, ]; r$LESIONID <- "T09"; r$DIAM_MM <- 12; st$RAW_TUMOR <- rbind(t, r); st })
seed("RAW-014", "Raw", "Lesion imaged by MRI at week 8 (CT at baseline)", last_visit_subj,
     function(st, u) { s <- sid_of(u); t <- st$RAW_TUMOR; i <- which(t$SUBJID == s & t$VISIT == "WEEK 8")[1]; st$RAW_TUMOR$METHOD[i] <- "MRI"; st })
seed("RAW-015", "Raw", "Week 16 scan performed 30 days late", last_visit_subj,
     function(st, u) { s <- sid_of(u); i <- which(st$RAW_TUMOR$SUBJID == s & st$RAW_TUMOR$VISIT == "WEEK 16")
       st$RAW_TUMOR$SCANDT[i] <- fmt_raw(parse_raw(st$RAW_TUMOR$SCANDT[i]) + 30); st })
sd_visit <- function(st) { r <- st$RAW_RESP; unique(paste(ONC_PAR$study, r$SUBJID[r$OVRLRESP == "SD" & r$NTRGRESP == "NON-CR/NON-PD" & r$NEWLES == "N"], sep = "-")) }
seed("RAW-016", "Raw", "Overall response PR entered where components give SD", sd_visit,
     function(st, u) { i <- which(st$RAW_RESP$SUBJID == sid_of(u) & st$RAW_RESP$OVRLRESP == "SD")[1]; st$RAW_RESP$OVRLRESP[i] <- "PR"; st })
seed("RAW-017", "Raw", "Target response and overall response PR where diameters give SD", sd_visit,
     function(st, u) { i <- which(st$RAW_RESP$SUBJID == sid_of(u) & st$RAW_RESP$OVRLRESP == "SD" & st$RAW_RESP$TRGRESP == "SD")[1]
       st$RAW_RESP$TRGRESP[i] <- "PR"; st$RAW_RESP$OVRLRESP[i] <- "PR"; st })
ae_subj <- function(st, cond) unique(paste(ONC_PAR$study, st$RAW_AE$SUBJID[cond(st$RAW_AE)], sep = "-"))
seed("RAW-018", "Raw", "AE marked ongoing although an end date is recorded", function(st) ae_subj(st, function(a) a$ONGOING == "N"),
     function(st, u) { i <- which(st$RAW_AE$SUBJID == sid_of(u) & st$RAW_AE$ONGOING == "N")[1]; st$RAW_AE$ONGOING[i] <- "Y"; st })
seed("RAW-019", "Raw", "Grade 4 AE entered as grade 5 without fatal outcome or death", function(st) ae_subj(st, function(a) a$GRADE == 4),
     function(st, u) { i <- which(st$RAW_AE$SUBJID == sid_of(u) & st$RAW_AE$GRADE == 4)[1]; st$RAW_AE$GRADE[i] <- 5; st })
seed("RAW-020", "Raw", "Action 'drug withdrawn' although dosing continued", function(st) {
       a <- st$RAW_AE; e <- st$RAW_EX
       ok <- vapply(seq_len(nrow(a)), function(i) { s <- parse_raw(a$AESTDT[i]); !is.na(s) && a$GRADE[i] < 5 &&
         any(parse_raw(e$EXSTDT[e$SUBJID == a$SUBJID[i]]) > s) }, logical(1))
       unique(paste(ONC_PAR$study, a$SUBJID[ok], sep = "-")) },
     function(st, u) { s <- sid_of(u); a <- st$RAW_AE; e <- st$RAW_EX
       i <- which(vapply(seq_len(nrow(a)), function(k) a$SUBJID[k] == s && !is.na(parse_raw(a$AESTDT[k])) &&
         any(parse_raw(e$EXSTDT[e$SUBJID == s]) > parse_raw(a$AESTDT[k])), logical(1)))[1]
       st$RAW_AE$ACTION[i] <- "DRUG WITHDRAWN"; st })
seed("RAW-021", "Raw", "Death date entered 10 days before the last dose", function(st) {
       d <- st$RAW_DS; unique(paste(ONC_PAR$study, d$SUBJID[d$DTHDT != "" & d$SUBJID %in% st$RAW_EX$SUBJID], sep = "-")) },
     function(st, u) { s <- sid_of(u); le <- max(parse_raw(st$RAW_EX$EXSTDT[st$RAW_EX$SUBJID == s]))
       st$RAW_DS$DTHDT[st$RAW_DS$SUBJID == s] <- fmt_raw(le - 10); st })

# ---- SDTM seeds
ae_u <- function(st, cond) unique(chr(st$AE$USUBJID[cond(st$AE)]))
ex_u <- function(st, k) { n <- table(chr(st$EX$USUBJID)); names(n)[n >= k] }
seed("SDTM-GEN-001", "SDTM", "STUDYID blank on an EX record", function(st) ex_u(st, 2),
     function(st, u) { st$EX$STUDYID[rows_of(st$EX, u)[2]] <- ""; st })
seed("SDTM-GEN-002", "SDTM", "DOMAIN = 'EA' on an AE record", function(st) ae_u(st, function(a) TRUE),
     function(st, u) { st$AE$DOMAIN[rows_of(st$AE, u)[1]] <- "EA"; st })
seed("SDTM-GEN-003", "SDTM", "AE record carries a USUBJID that is not in DM", function(st) ae_u(st, function(a) a$AEOUT != "FATAL"),
     function(st, u) { i <- which(chr(st$AE$USUBJID) == u & st$AE$AEOUT != "FATAL")[1]; st$AE$USUBJID[i] <- paste0(ONC_PAR$study, "-999-999"); st })
seed("SDTM-GEN-004", "SDTM", "Second AE given the same AESEQ as the first", function(st) { n <- table(chr(st$AE$USUBJID)); names(n)[n >= 2] },
     function(st, u) { i <- rows_of(st$AE, u); st$AE$AESEQ[i[2]] <- st$AE$AESEQ[i[1]]; st })
seed("SDTM-GEN-005", "SDTM", "EXSTDTC written as DD/MM/YYYY on a mid-treatment record", function(st) ex_u(st, 3),
     function(st, u) { i <- rows_of(st$EX, u)[2]; st$EX$EXSTDTC[i] <- format(dt(st$EX$EXSTDTC[i]), "%d/%m/%Y"); st })
seed("SDTM-GEN-006", "SDTM", "AESTDY off by one", function(st) ae_u(st, function(a) nchar(a$AESTDTC) == 10),
     function(st, u) { i <- which(chr(st$AE$USUBJID) == u & nchar(st$AE$AESTDTC) == 10)[1]; st$AE$AESTDY[i] <- num(st$AE$AESTDY[i]) + 1; st })
seed("SDTM-GEN-007", "SDTM", "SEX = 'Male' (not CDISC CT)", function(st) all_u(st),
     function(st, u) { i <- rows_of(st$DM, u); st$DM$SEX[i] <- ifelse(st$DM$SEX[i] == "M", "Male", "Female"); st })
seed("SDTM-DM-001", "SDTM", "RFXENDTC 7 days after last exposure", function(st) treated_u(st),
     function(st, u) { i <- rows_of(st$DM, u); st$DM$RFXENDTC[i] <- iso(dt(st$DM$RFXENDTC[i]) + 7); st })
seed("SDTM-DM-002", "SDTM", "DTHFL = Y for a living subject (no death date)", function(st) chr(st$DM$USUBJID[chr(st$DM$DTHFL) == "" & chr(st$DM$ACTARM) != "Not Treated"]),
     function(st, u) { st$DM$DTHFL[rows_of(st$DM, u)] <- "Y"; st })
seed("SDTM-DM-003", "SDTM", "ACTARM swapped to the other treatment", function(st) treated_u(st),
     function(st, u) { i <- rows_of(st$DM, u); sw <- st$DM$ACTARM[i] == "Drug X"
       st$DM$ACTARM[i] <- ifelse(sw, "Standard of care", "Drug X"); st$DM$ACTARMCD[i] <- ifelse(sw, "SOC", "DRUGX"); st })
seed("SDTM-EX-001", "SDTM", "Exposure end date overlaps the next cycle", function(st) ex_u(st, 3),
     function(st, u) { i <- rows_of(st$EX, u); st$EX$EXENDTC[i[2]] <- iso(dt(st$EX$EXSTDTC[i[3]]) + 2)
       st$EX$EXENDY[i[2]] <- study_day(dt(st$EX$EXENDTC[i[2]]), dt(st$DM$RFSTDTC[st$DM$USUBJID == u])); st })
seed("SDTM-EX-002", "SDTM", "EXDOSU blank", function(st) ex_u(st, 2),
     function(st, u) { st$EX$EXDOSU[rows_of(st$EX, u)[2]] <- ""; st })
seed("SDTM-AE-001", "SDTM", "AE end date 3 days before start", function(st) ae_u(st, function(a) nchar(a$AESTDTC) == 10 & a$AEENDTC != ""),
     function(st, u) { i <- which(chr(st$AE$USUBJID) == u & nchar(st$AE$AESTDTC) == 10 & st$AE$AEENDTC != "")[1]
       st$AE$AEENDTC[i] <- iso(dt(st$AE$AESTDTC[i]) - 3); st$AE$AEENDY[i] <- study_day(dt(st$AE$AEENDTC[i]), dt(st$DM$RFSTDTC[st$DM$USUBJID == u])); st })
seed("SDTM-AE-002", "SDTM", "Grade 3 AE with outcome FATAL", function(st) ae_u(st, function(a) a$AETOXGR == "3"),
     function(st, u) { i <- which(chr(st$AE$USUBJID) == u & st$AE$AETOXGR == "3")[1]; st$AE$AEOUT[i] <- "FATAL"; st })
seed("SDTM-AE-003", "SDTM", "AESER = Y with no seriousness criterion", function(st) ae_u(st, function(a) a$AESER == "N"),
     function(st, u) { i <- which(chr(st$AE$USUBJID) == u & st$AE$AESER == "N")[1]; st$AE$AESER[i] <- "Y"; st })
seed("SDTM-AE-004", "SDTM", "AEDECOD blank (uncoded)", function(st) ae_u(st, function(a) TRUE),
     function(st, u) { st$AE$AEDECOD[rows_of(st$AE, u)[1]] <- ""; st })
new_u <- function(st) unique(chr(st$TU$USUBJID[st$TU$TUORRES == "NEW"]))
seed("SDTM-TU-001", "SDTM", "New lesion identification record duplicated", new_u,
     function(st, u) { i <- which(chr(st$TU$USUBJID) == u & st$TU$TUORRES == "NEW")[1]; r <- st$TU[i, ]
       r$TUSEQ <- max(num(st$TU$TUSEQ[rows_of(st$TU, u)])) + 1; st$TU <- rbind(st$TU, r); st })
seed("SDTM-TU-002", "SDTM", "New lesion identified as TARGET at week 24", new_u,
     function(st, u) { i <- which(chr(st$TU$USUBJID) == u & st$TU$TUORRES == "NEW")[1]; st$TU$TUORRES[i] <- "TARGET"; st$TU$TUSTRESC[i] <- "TARGET"; st })
seed("SDTM-TU-003", "SDTM", "Three target lesions mapped to the same TULOC",
     function(st) three_nonnodal(st$TU, "TUORRES", "USUBJID", "VISIT", function(x) chr(x$TULOC) != "LYMPH NODE"),
     function(st, u) { t <- st$TU; ids <- head(t$TULNKID[chr(t$USUBJID) == u & t$TUORRES == "TARGET" & t$TULOC != "LYMPH NODE"], 3)
       st$TU$TULOC[chr(t$USUBJID) == u & t$TULNKID %in% ids] <- "LIVER"; st })
seed("SDTM-TR-001", "SDTM", "Non-target result linked to NT09 (not in TU)", function(st) unique(chr(st$TR$USUBJID[st$TR$TRTESTCD == "TUMSTATE" & st$TR$VISIT == "WEEK 8"])),
     function(st, u) { i <- which(chr(st$TR$USUBJID) == u & st$TR$TRTESTCD == "TUMSTATE" & st$TR$VISIT == "WEEK 8")[1]; st$TR$TRLNKID[i] <- "NT09"; st })
seed("SDTM-TR-002", "SDTM", "Diameter recorded with unit 'cm'", function(st) unique(chr(st$TR$USUBJID[st$TR$TRTESTCD == "LDIAM" & st$TR$VISIT == "WEEK 8"])),
     function(st, u) { i <- which(chr(st$TR$USUBJID) == u & st$TR$TRTESTCD == "LDIAM" & st$TR$VISIT == "WEEK 8")[1]
       st$TR$TRSTRESU[i] <- "cm"; st$TR$TRORRESU[i] <- "cm"; st })
seed("SDTM-TR-003", "SDTM", "SUMDIAM 5 mm higher than the lesion total", function(st) unique(chr(st$TR$USUBJID[st$TR$TRTESTCD == "SUMDIAM" & st$TR$VISIT == "WEEK 8"])),
     function(st, u) { i <- which(chr(st$TR$USUBJID) == u & st$TR$TRTESTCD == "SUMDIAM" & st$TR$VISIT == "WEEK 8")[1]
       v <- num(st$TR$TRSTRESN[i]) + 5; st$TR$TRSTRESN[i] <- v; st$TR$TRSTRESC[i] <- st$TR$TRORRES[i] <- as.character(v); st })
seed("SDTM-TR-004", "SDTM", "Baseline LDIAM 8 mm; sum of diameters unchanged", function(st) {
       r <- st$TR[st$TR$VISIT == "BASELINE" & st$TR$TRTESTCD == "LDIAM" & num(st$TR$TRSTRESN) >= 13, ]; n <- table(chr(r$USUBJID)); names(n)[n >= 2] },
     function(st, u) { r <- st$TR; i <- which(chr(r$USUBJID) == u & r$VISIT == "BASELINE" & r$TRTESTCD == "LDIAM" & num(r$TRSTRESN) >= 13)[1:2]
       d <- num(r$TRSTRESN[i[1]]); nv <- c(8, num(r$TRSTRESN[i[2]]) + d - 8)
       st$TR$TRSTRESN[i] <- nv; st$TR$TRSTRESC[i] <- st$TR$TRORRES[i] <- as.character(nv); st })
rs_sd_u <- function(st) { w <- rs_wide(st$RS); unique(w$USUBJID[w$OVR == "SD" & w$TRG == "SD" & w$NTRG == "NON-CR/NON-PD" & w$NEWL == "N"]) }
seed("SDTM-RS-001", "SDTM", "OVRLRESP = PR where Table 1 gives SD", rs_sd_u,
     function(st, u) { w <- rs_wide(st$RS); v <- w$VISITNUM[w$USUBJID == u & w$OVR == "SD"][1]
       i <- which(chr(st$RS$USUBJID) == u & num(st$RS$VISITNUM) == v & st$RS$RSTESTCD == "OVRLRESP"); st$RS$RSORRES[i] <- st$RS$RSSTRESC[i] <- "PR"; st })
seed("SDTM-RS-002", "SDTM", "TRGRESP and OVRLRESP = PR where diameters give SD", rs_sd_u,
     function(st, u) { w <- rs_wide(st$RS); v <- w$VISITNUM[w$USUBJID == u & w$OVR == "SD"][1]
       i <- which(chr(st$RS$USUBJID) == u & num(st$RS$VISITNUM) == v & st$RS$RSTESTCD %in% c("TRGRESP", "OVRLRESP"))
       st$RS$RSORRES[i] <- st$RS$RSSTRESC[i] <- "PR"; st })
seed("SDTM-RS-003", "SDTM", "Response date 10 days after the scan date", function(st) unique(chr(st$RS$USUBJID)),
     function(st, u) { i <- which(chr(st$RS$USUBJID) == u & num(st$RS$VISITNUM) == min(num(st$RS$VISITNUM[chr(st$RS$USUBJID) == u])))
       st$RS$RSDTC[i] <- iso(dt(st$RS$RSDTC[i]) + 10); st$RS$RSDY[i] <- num(st$RS$RSDY[i]) + 10; st })
seed("SDTM-DS-001", "SDTM", "DS death date 3 days later than DM.DTHDTC", function(st) unique(chr(st$DS$USUBJID[st$DS$DSDECOD == "DEATH"])),
     function(st, u) { i <- which(chr(st$DS$USUBJID) == u & st$DS$DSDECOD == "DEATH"); st$DS$DSSTDTC[i] <- iso(dt(st$DS$DSSTDTC[i]) + 3)
       st$DS$DSSTDY[i] <- num(st$DS$DSSTDY[i]) + 3; st })

# ---- ADaM seeds
no_ae_treated <- function(st) setdiff(treated_u(st), chr(st$AE$USUBJID))
seed("ADAM-GEN-001", "ADaM", "ADSL record duplicated", function(st) all_u(st),
     function(st, u) { st$ADSL <- rbind(st$ADSL, st$ADSL[rows_of(st$ADSL, u), ]); st })
seed("ADAM-GEN-002", "ADaM", "One OS record labelled 'Overall survival (months)'", function(st) all_u(st),
     function(st, u) { i <- which(chr(st$ADTTE$USUBJID) == u & st$ADTTE$PARAMCD == "OS"); st$ADTTE$PARAM[i] <- "Overall survival (months)"; st })
seed("ADAM-GEN-003", "ADaM", "TRT01A on one ADRS record differs from ADSL", function(st) unique(chr(st$ADRS$USUBJID[st$ADRS$PARAMCD == "OVR"])),
     function(st, u) { i <- which(chr(st$ADRS$USUBJID) == u & st$ADRS$PARAMCD == "OVR")[1]
       st$ADRS$TRT01A[i] <- ifelse(st$ADRS$TRT01A[i] == "Drug X", "Standard of care", "Drug X"); st })
seed("ADAM-GEN-004", "ADaM", "ANL01FL = '1' instead of 'Y'", function(st) unique(chr(st$ADRS$USUBJID[st$ADRS$PARAMCD == "OVR"])),
     function(st, u) { st$ADRS$ANL01FL[which(chr(st$ADRS$USUBJID) == u & st$ADRS$PARAMCD == "OVR")[1]] <- "1"; st })
seed("ADAM-GEN-005", "ADaM", "SRCSEQ points to a non-existent RS record", function(st) unique(chr(st$ADRS$USUBJID[st$ADRS$PARAMCD == "OVR"])),
     function(st, u) { st$ADRS$SRCSEQ[which(chr(st$ADRS$USUBJID) == u & st$ADRS$PARAMCD == "OVR")[1]] <- 999; st })
seed("ADAM-ADSL-001", "ADaM", "TRTEDT 5 days after last exposure", no_ae_treated,
     function(st, u) { i <- rows_of(st$ADSL, u); st$ADSL$TRTEDT[i] <- dt(st$ADSL$TRTEDT[i]) + 5; st })
seed("ADAM-ADSL-002", "ADaM", "SAFFL = N for a dosed subject", no_ae_treated,
     function(st, u) { st$ADSL$SAFFL[rows_of(st$ADSL, u)] <- "N"; st })
seed("ADAM-ADSL-003", "ADaM", "DTHDT 4 days later than DM.DTHDTC", function(st) chr(st$DM$USUBJID[chr(st$DM$DTHDTC) != ""]),
     function(st, u) { i <- rows_of(st$ADSL, u); st$ADSL$DTHDT[i] <- dt(st$ADSL$DTHDT[i]) + 4; st })
seed("ADAM-ADSL-004", "ADaM", "TRT01A swapped to the other treatment in ADSL", no_ae_treated,
     function(st, u) { i <- rows_of(st$ADSL, u); st$ADSL$TRT01A[i] <- ifelse(st$ADSL$TRT01A[i] == "Drug X", "Standard of care", "Drug X"); st })
adrs_sd_u <- function(st) unique(chr(st$ADRS$USUBJID[st$ADRS$PARAMCD == "OVR" & st$ADRS$AVALC == "SD"]))
seed("ADAM-ADRS-001", "ADaM", "ADRS time-point response PR where RS gives SD", adrs_sd_u,
     function(st, u) { i <- which(chr(st$ADRS$USUBJID) == u & st$ADRS$PARAMCD == "OVR" & st$ADRS$AVALC == "SD")[1]
       st$ADRS$AVALC[i] <- "PR"; st$ADRS$AVAL[i] <- 2; st })
seed("ADAM-ADRS-002", "ADaM", "BOR = PR for a subject whose derived BOR is SD", function(st) unique(chr(st$ADRS$USUBJID[st$ADRS$PARAMCD == "BOR" & st$ADRS$AVALC == "SD"])),
     function(st, u) { i <- which(chr(st$ADRS$USUBJID) == u & st$ADRS$PARAMCD == "BOR"); st$ADRS$AVALC[i] <- "PR"; st$ADRS$AVAL[i] <- 2; st })
seed("ADAM-ADRS-003", "ADaM", "BOR record duplicated", function(st) unique(chr(st$ADRS$USUBJID[st$ADRS$PARAMCD == "BOR"])),
     function(st, u) { st$ADRS <- rbind(st$ADRS, st$ADRS[which(chr(st$ADRS$USUBJID) == u & st$ADRS$PARAMCD == "BOR"), ]); st })
seed("ADAM-ADTTE-001", "ADaM", "PFS AVAL one day longer than ADT - STARTDT + 1", function(st) all_u(st),
     function(st, u) { i <- which(chr(st$ADTTE$USUBJID) == u & st$ADTTE$PARAMCD == "PFS"); st$ADTTE$AVAL[i] <- num(st$ADTTE$AVAL[i]) + 1; st })
seed("ADAM-ADTTE-002", "ADaM", "PFS progression event recorded as censored", function(st) unique(chr(st$ADTTE$USUBJID[st$ADTTE$PARAMCD == "PFS" & st$ADTTE$CNSR == 0 & st$ADTTE$EVNTDESC == "Disease progression"])),
     function(st, u) { i <- which(chr(st$ADTTE$USUBJID) == u & st$ADTTE$PARAMCD == "PFS")
       st$ADTTE$CNSR[i] <- 1; st$ADTTE$CNSDTDSC[i] <- st$ADTTE$EVNTDESC[i]; st$ADTTE$EVNTDESC[i] <- ""; st })
seed("ADAM-ADTTE-003", "ADaM", "Death after two or more missed assessments counted as a PFS event", function(st) unique(chr(st$ADTTE$USUBJID[st$ADTTE$PARAMCD == "PFS" & st$ADTTE$CNSDTDSC == "Event after two or more missed assessments"])),
     function(st, u) { i <- which(chr(st$ADTTE$USUBJID) == u & st$ADTTE$PARAMCD == "PFS"); dd <- dt(st$ADSL$DTHDT[rows_of(st$ADSL, u)])
       st$ADTTE$ADT[i] <- dd; st$ADTTE$AVAL[i] <- as.numeric(dd - dt(st$ADTTE$STARTDT[i])) + 1; st$ADTTE$CNSR[i] <- 0
       st$ADTTE$EVNTDESC[i] <- "Death"; st$ADTTE$CNSDTDSC[i] <- ""; st })
seed("ADAM-ADTTE-004", "ADaM", "OS censoring date set 10 days before the PFS date", function(st) {
       a <- st$ADTTE; os <- a[a$PARAMCD == "OS" & a$CNSR == 1, ]; pf <- a[a$PARAMCD == "PFS", ]
       m <- match(os$USUBJID, pf$USUBJID); chr(os$USUBJID[num(pf$AVAL[m]) > 20]) },
     function(st, u) { i <- which(chr(st$ADTTE$USUBJID) == u & st$ADTTE$PARAMCD == "OS"); j <- which(chr(st$ADTTE$USUBJID) == u & st$ADTTE$PARAMCD == "PFS")
       st$ADTTE$ADT[i] <- dt(st$ADTTE$ADT[j]) - 10; st$ADTTE$AVAL[i] <- as.numeric(dt(st$ADTTE$ADT[i]) - dt(st$ADTTE$STARTDT[i])) + 1; st })
seed("ADAM-ADTTE-005", "ADaM", "DOR record created for a subject with BOR SD", function(st) unique(chr(st$ADRS$USUBJID[st$ADRS$PARAMCD == "BOR" & st$ADRS$AVALC == "SD"])),
     function(st, u) { j <- which(chr(st$ADTTE$USUBJID) == u & st$ADTTE$PARAMCD == "PFS"); r <- st$ADTTE[j, ]
       r$PARAMCD <- "DOR"; r$PARAM <- "Duration of response (days)"
       r$STARTDT <- dt(st$ADRS$ADT[which(chr(st$ADRS$USUBJID) == u & st$ADRS$PARAMCD == "BOR")])
       r$AVAL <- as.numeric(dt(r$ADT) - dt(r$STARTDT)) + 1; st$ADTTE <- rbind(st$ADTTE, r); st })
seed("ADAM-ADTTE-006", "ADaM", "OS censoring date 15 days after the data cut-off", function(st) chr(st$ADTTE$USUBJID[st$ADTTE$PARAMCD == "OS" & st$ADTTE$CNSR == 1]),
     function(st, u) { i <- which(chr(st$ADTTE$USUBJID) == u & st$ADTTE$PARAMCD == "OS")
       st$ADTTE$ADT[i] <- ONC_PAR$dco + 15; st$ADTTE$AVAL[i] <- as.numeric(dt(st$ADTTE$ADT[i]) - dt(st$ADTTE$STARTDT[i])) + 1; st })
seed("ADAM-ADAE-001", "ADaM", "Pre-treatment AE flagged treatment-emergent", function(st) unique(chr(st$ADAE$USUBJID[st$ADAE$TRTEMFL == "" & !is.na(st$ADAE$ASTDT) & dt(st$ADAE$ASTDT) < dt(st$ADAE$TRTSDT)])),
     function(st, u) { i <- which(chr(st$ADAE$USUBJID) == u & st$ADAE$TRTEMFL == "" & dt(st$ADAE$ASTDT) < dt(st$ADAE$TRTSDT))[1]; st$ADAE$TRTEMFL[i] <- "Y"; st })
seed("ADAM-ADAE-002", "ADaM", "One AE record dropped from ADAE", function(st) unique(chr(st$ADAE$USUBJID)),
     function(st, u) { st$ADAE <- st$ADAE[-rows_of(st$ADAE, u)[1], ]; st })
seed("ADAM-ADAE-003", "ADaM", "Imputed start date without ASTDTF", function(st) unique(chr(st$ADAE$USUBJID[st$ADAE$ASTDTF == "D"])),
     function(st, u) { st$ADAE$ASTDTF[which(chr(st$ADAE$USUBJID) == u & st$ADAE$ASTDTF == "D")] <- ""; st })

# Apply seeds: pick subjects, record the findings each seed produces on its own,
# then apply all seeds together.
apply_seeds <- function(clean, p = ONC_PAR, expect = TRUE) {
  used <- list(Raw = character(), Other = character())
  if (expect) { base_f <- run_onc_checks(clean, p)$findings; base_k <- paste(base_f$check_id, base_f$usubjid) }
  key <- list(); st_all <- clean
  cands <- lapply(SEEDS, function(s) s$select(clean))
  pick <- character(length(SEEDS))
  for (k in order(lengths(cands))) {           # scarcest candidates assigned first
    pool <- if (SEEDS[[k]]$level == "Raw") "Raw" else "Other"
    cand <- setdiff(cands[[k]], used[[pool]])
    if (!length(cand)) stop("No candidate subject for seed ", SEEDS[[k]]$check)
    pick[k] <- cand[1]; used[[pool]] <- c(used[[pool]], cand[1])
  }
  for (k in seq_along(SEEDS)) {
    s <- SEEDS[[k]]; u <- pick[k]
    if (expect) {
      one <- run_onc_checks(s$apply(clean, u), p)$findings
      fk <- setdiff(unique(paste(one$check_id, one$usubjid)), base_k)
    } else fk <- paste(s$check, u)
    exp_checks <- unique(sub(" .*$", "", fk))
    key[[k]] <- data.frame(Seed_ID = sprintf("S%02d", k), Level = s$level, USUBJID = u, Error_seeded = s$desc,
                           Target_check = s$check,
                           Checks_expected_to_flag = paste(exp_checks, collapse = ", "),
                           Expected_keys = paste(fk, collapse = "|"), stringsAsFactors = FALSE)
    st_all <- s$apply(st_all, u)
  }
  list(study = st_all, key = do.call(rbind, key))
}

# -----------------------------------------------------------------------------
# 7. Excel writers (Arial, light grey headers, black and white)
# -----------------------------------------------------------------------------
xl_styles <- function() list(
  head = createStyle(fontName = "Arial", fontSize = 10, textDecoration = "bold", fgFill = "#D9D9D9",
                     border = "TopBottomLeftRight", borderColour = "#808080", wrapText = TRUE, valign = "center"),
  body = createStyle(fontName = "Arial", fontSize = 10, valign = "top"),
  wrap = createStyle(fontName = "Arial", fontSize = 10, wrapText = TRUE, valign = "top"),
  date = createStyle(fontName = "Arial", fontSize = 10, numFmt = "yyyy-mm-dd", valign = "top"),
  title = createStyle(fontName = "Arial", fontSize = 13, textDecoration = "bold"),
  bold = createStyle(fontName = "Arial", fontSize = 10, textDecoration = "bold"))

xl_sheet <- function(wb, name, df, widths = NULL, wrap_cols = NULL, startRow = 1) {
  S <- xl_styles(); addWorksheet(wb, name, gridLines = TRUE)
  writeData(wb, name, df, startRow = startRow, headerStyle = S$head, keepNA = FALSE)
  if (nrow(df)) {
    rr <- startRow + seq_len(nrow(df))
    addStyle(wb, name, S$body, rows = rr, cols = seq_along(df), gridExpand = TRUE)
    for (j in which(vapply(df, inherits, logical(1), "Date"))) addStyle(wb, name, S$date, rows = rr, cols = j, gridExpand = TRUE)
    for (j in match(wrap_cols, names(df), nomatch = 0)) if (j) addStyle(wb, name, S$wrap, rows = rr, cols = j, gridExpand = TRUE)
  }
  w <- widths %||% vapply(names(df), function(v) min(60, max(nchar(v), if (nrow(df)) max(nchar(chr(head(df[[v]], 300)))) else 0) + 2), numeric(1))
  setColWidths(wb, name, cols = seq_along(df), widths = w)
  freezePane(wb, name, firstActiveRow = startRow + 1)
  addFilter(wb, name, rows = startRow, cols = seq_along(df))
  invisible(wb)
}

xl_readme <- function(wb, name, title, lines) {
  S <- xl_styles(); addWorksheet(wb, name, gridLines = FALSE)
  writeData(wb, name, title, startRow = 1); addStyle(wb, name, S$title, rows = 1, cols = 1)
  writeData(wb, name, data.frame(x = lines), startRow = 3, colNames = FALSE)
  addStyle(wb, name, S$wrap, rows = 3:(2 + length(lines)), cols = 1, gridExpand = TRUE)
  bold_rows <- 2 + which(grepl("^[A-Z][A-Za-z /()-]+$", lines) & nchar(lines) < 40)
  if (length(bold_rows)) addStyle(wb, name, S$bold, rows = bold_rows, cols = 1, gridExpand = TRUE)
  setColWidths(wb, name, cols = 1, widths = 120)
}

TRACE <- data.frame(
  Endpoint = c("Objective response / best overall response", "Progression-free survival", "Overall survival",
               "Duration of response", "Treatment-emergent adverse events", "Exposure"),
  Raw_source = c("RAW_TUMOR (lesions, diameters, method), RAW_RESP (investigator responses)",
                 "RAW_RESP, RAW_TUMOR (scan dates), RAW_DS (death), subsequent therapy form",
                 "RAW_DS (death date, last contact)", "RAW_RESP, RAW_DS",
                 "RAW_AE (term, dates, grade, seriousness, outcome, action)", "RAW_EX (cycles, dates, dose, adjustments)"),
  SDTM = c("TU (TUORRES, TULOC), TR (LDIAM, LPERP, SUMDIAM, TUMSTATE), RS (TRGRESP, NTRGRESP, NEWLPROG, OVRLRESP)",
           "RS (OVRLRESP, RSDTC), DM (DTHDTC), DS (RANDOMIZED)", "DM (DTHDTC), DS (DEATH)", "RS, DM",
           "AE (AEDECOD, AEBODSYS, AETOXGR, AESER, AEOUT, AESTDTC)", "EX (EXTRT, EXDOSE, EXSTDTC, EXENDTC), DM (RFXSTDTC, RFXENDTC)"),
  ADaM = c("ADRS: PARAMCD = OVR, BOR", "ADTTE: PARAMCD = PFS", "ADTTE: PARAMCD = OS", "ADTTE: PARAMCD = DOR",
           "ADAE: TRTEMFL, AETOXGRN, ASTDT, ASTDTF", "ADSL: TRTSDT, TRTEDT, TRT01A, SAFFL"),
  Key_derivation = c("Confirmed CR/PR (>= 28 days apart); SD >= 42 days from first dose; assessments after PD excluded",
                     "Event = first PD or death; censor at last adequate assessment; event after two or more missed assessments (gap > 126 days) censored",
                     "Event = death; censor at last known alive date", "From first CR/PR to PD or death; responders only",
                     "TRTSDT <= ASTDT <= TRTEDT + 30 days; partial dates imputed to the first of the month and flagged",
                     "First and last exposure dates; actual treatment from EXTRT"),
  Checks = c("RAW-010 to RAW-017, SDTM-TU-001 to SDTM-RS-003, ADAM-ADRS-001 to ADAM-ADRS-003",
             "RAW-015, RAW-021, RAW-022, SDTM-RS-003, SDTM-DS-001, ADAM-ADTTE-001 to ADAM-ADTTE-003, ADAM-ADTTE-006",
             "RAW-021, SDTM-DM-002, SDTM-DS-001, ADAM-ADSL-003, ADAM-ADTTE-002, ADAM-ADTTE-004",
             "ADAM-ADTTE-005", "RAW-018 to RAW-020, SDTM-AE-001 to SDTM-AE-004, ADAM-ADAE-001 to ADAM-ADAE-003",
             "RAW-006 to RAW-008, SDTM-DM-001, SDTM-DM-003, SDTM-EX-001, SDTM-EX-002, ADAM-ADSL-001, ADAM-ADSL-004"),
  stringsAsFactors = FALSE)

CT_TABLE <- data.frame(
  Codelist = c("SEX", "NY", "OUT (AEOUT)", "ACN (AEACN)", "AETOXGR", "TUORRES", "RSTESTCD", "TRGRESP / OVRLRESP",
               "NTRGRESP", "NEWLPROG", "TRTESTCD", "Units"),
  Values = c(paste(CT$SEX, collapse = ", "), "Y, N", paste(CT$OUT, collapse = ", "), paste(CT$ACN, collapse = ", "),
             "1, 2, 3, 4, 5 (CTCAE grade)", paste(CT$TUORRES, collapse = ", "), "TRGRESP, NTRGRESP, NEWLPROG, OVRLRESP",
             "CR, PR, SD, PD, NE", "CR, NON-CR/NON-PD, PD, NE", "Y, N", "LDIAM, LPERP, SUMDIAM, TUMSTATE", "mg (EXDOSU); mm (TRSTRESU)"),
  stringsAsFactors = FALSE)

write_spec_xlsx <- function(path) {
  wb <- createWorkbook(); modifyBaseFont(wb, fontName = "Arial", fontSize = 10)
  xl_readme(wb, "README", "CDISC oncology data check specification", c(
    "Purpose",
    "Checks applied in sequence to raw EDC/CRF data, SDTM datasets and ADaM datasets for a randomised oncology trial with RECIST 1.1 tumour assessment, CTCAE-graded adverse events and PFS, OS, objective response and duration of response endpoints.",
    "",
    "Standards",
    "SDTMIG v3.4 and ADaMIG v1.3 (versions required by FDA for current submissions); CDISC Controlled Terminology; RECIST 1.1; NCI CTCAE in the protocol-specified version (v5.0 or v6.0); FDA guidance on clinical trial endpoints for cancer drugs (2018) for PFS censoring.",
    "",
    "Sheets",
    "Raw_Checks, SDTM_Checks, ADaM_Checks: one row per check with identifier, dataset, category, check, logic, severity, reference and automation status.",
    "Traceability: lineage of each endpoint from raw form to SDTM to ADaM, with the checks that protect it.",
    "Parameters: protocol and SAP values used by the checks (windows, confirmation interval, SD duration, TEAE window, data cut-off).",
    "Controlled_Terms: codelist values the CT conformance checks accept.",
    "",
    "Severity",
    "Critical: affects eligibility, safety reporting or a primary/key secondary endpoint. Major: affects a derivation, a listing or submission conformance. Minor: affects schedule adherence or presentation.",
    "",
    "Automation",
    "Automated: implemented in cdisc_oncology_checks.R and run by the Data Cleaning Dashboard (CDISC oncology tab). Manual review: needs medical, radiology or submission-package review outside the data.",
    "",
    "Order of work",
    "Raw checks are resolved through data queries before SDTM mapping; SDTM checks are resolved before ADaM derivation; ADaM checks confirm that analysis datasets reproduce the SAP derivations from SDTM."))
  cols <- c("Check_ID", "Dataset", "Category", "Check", "Logic", "Severity", "Reference", "Automation")
  w <- c(14, 18, 18, 55, 70, 10, 45, 16)
  for (lv in c("Raw", "SDTM", "ADaM"))
    xl_sheet(wb, paste0(lv, "_Checks"), CHECK_SPEC[CHECK_SPEC$Level == lv, cols], widths = w, wrap_cols = c("Check", "Logic", "Reference"))
  xl_sheet(wb, "Traceability", TRACE, widths = c(30, 45, 55, 30, 60, 50), wrap_cols = names(TRACE))
  par_df <- data.frame(Parameter = c("Data cut-off", "Tumour assessment interval", "Assessment window", "Baseline imaging window",
                                     "Confirmation interval (CR/PR)", "Minimum SD duration", "Two or more missed assessments",
                                     "TEAE window after last dose", "Drug X dose levels", "Standard of care dose levels"),
                       Value = c(format(ONC_PAR$dco), "56 days (every 8 weeks)", "+/- 7 days", "28 days before first dose",
                                 ">= 28 days", ">= 42 days from first dose", "gap > 126 days (2 x 56 + 14)", "30 days",
                                 "200 mg; 150 mg (reduced)", "100 mg; 75 mg (reduced)"),
                       Source = c("SAP", "Protocol", "Protocol", "Protocol; RECIST 1.1", "RECIST 1.1; SAP", "RECIST 1.1; SAP",
                                  "FDA endpoints guidance (2018); SAP", "SAP", "Protocol", "Protocol"))
  xl_sheet(wb, "Parameters", par_df, widths = c(34, 34, 34))
  xl_sheet(wb, "Controlled_Terms", CT_TABLE, widths = c(22, 110), wrap_cols = "Values")
  saveWorkbook(wb, path, overwrite = TRUE)
}

STUDY_SHEETS <- c("RAW_DM", "RAW_EX", "RAW_TUMOR", "RAW_RESP", "RAW_AE", "RAW_DS",
                  "DM", "EX", "AE", "TU", "TR", "RS", "DS", "ADSL", "ADRS", "ADTTE", "ADAE")

write_study_xlsx <- function(study, key, path) {
  wb <- createWorkbook(); modifyBaseFont(wb, fontName = "Arial", fontSize = 10)
  xl_readme(wb, "README", paste("Simulated oncology study", ONC_PAR$study, "- raw, SDTM and ADaM data with seeded errors"), c(
    "Design",
    sprintf("Randomised 1:1 trial of Drug X versus standard of care in advanced solid tumours; %d subjects; RECIST 1.1 tumour assessment every 8 weeks from first dose; 21-day treatment cycles; data cut-off %s.",
            nrow(study$RAW_DM) - sum(duplicated(study$RAW_DM$SUBJID)), format(ONC_PAR$dco)),
    "",
    "Sheets",
    "RAW_DM, RAW_EX, RAW_TUMOR, RAW_RESP, RAW_AE, RAW_DS: EDC export (dates as DD-MMM-YYYY).",
    "DM, EX, AE, TU, TR, RS, DS: SDTM (SDTMIG v3.4 structure, ISO 8601 dates).",
    "ADSL, ADRS, ADTTE, ADAE: ADaM (ADaMIG v1.3 structure, numeric dates).",
    "Seeded_Errors: every error placed in the data, the subject, the check it targets and every check expected to flag it.",
    "",
    "How the errors were placed",
    "The data were first generated without errors and every automated check was run to confirm zero findings. Errors were then placed one level at a time: raw errors in the RAW_ sheets only, SDTM errors in SDTM only, ADaM errors in ADaM only, so each layer can be checked on its own. Raw and SDTM/ADaM errors use separate subjects.",
    "",
    "Use",
    "Upload this workbook on the CDISC oncology tab of the Data Cleaning Dashboard, or run run_onc_checks() in R, and compare the findings with Seeded_Errors. Sheet and variable names are the interface: a workbook from another study runs the checks whose sheets are present."))
  for (sh in STUDY_SHEETS) xl_sheet(wb, sh, study[[sh]])
  xl_sheet(wb, "Seeded_Errors", key[, setdiff(names(key), "Expected_keys")], widths = c(9, 8, 18, 60, 18, 45),
           wrap_cols = c("Error_seeded", "Checks_expected_to_flag"))
  saveWorkbook(wb, path, overwrite = TRUE)
}

write_findings_xlsx <- function(res, key, path) {
  wb <- createWorkbook(); modifyBaseFont(wb, fontName = "Arial", fontSize = 10); S <- xl_styles()
  f <- res$findings; stt <- res$status
  nf <- nrow(f)
  xl_readme(wb, "README", "CDISC oncology check findings", c(
    "Content",
    "Summary: one row per check with run status, number of findings (counted from the Findings sheet by formula), subjects affected and result.",
    "Findings: one row per finding with level, check, dataset, subject, record, value and message.",
    "Seeded_vs_Detected (simulated study only): each seeded error, the checks expected to flag it and whether every expected finding was raised.",
    "",
    "Cells to complete during review",
    "Findings sheet, columns Query_status (Open, Answered, Closed), Resolution and Reviewer. Example entry: Query_status = Closed; Resolution = Source shows consent 12-MAR-2025, EDC corrected; Reviewer = initials and date.",
    "",
    "Checks not run",
    "Manual review checks are listed in Summary with status 'Manual review (not automated)'. Automated checks whose sheets are absent from a workbook are listed as 'Not run' with the missing sheet named."))
  sm <- stt[, c("Check_ID", "Level", "Dataset", "Check", "Severity", "Run_status")]
  sm$Findings <- NA; sm$Subjects <- vapply(sm$Check_ID, function(id) length(unique(f$usubjid[f$check_id == id])), integer(1))
  sm$Result <- NA
  xl_sheet(wb, "Summary", sm, widths = c(16, 7, 18, 60, 10, 30, 10, 10, 12), wrap_cols = "Check")
  n <- nrow(sm); rr <- 2:(n + 1)
  writeFormula(wb, "Summary", x = sprintf('IF(LEFT(F%d,3)="Run",COUNTIF(Findings!$C$2:$C$%d,A%d),"")', rr, max(nf + 1, 2), rr), startCol = 7, startRow = 2)
  writeFormula(wb, "Summary", x = sprintf('IF(LEFT(F%d,3)="Run",IF(G%d=0,"Pass","Fail"),"Not run")', rr, rr), startCol = 9, startRow = 2)
  addStyle(wb, "Summary", S$body, rows = rr, cols = c(7, 9), gridExpand = TRUE, stack = TRUE)
  tot <- n + 3
  writeData(wb, "Summary", data.frame(a = c("Total findings", "Checks failed", "Checks passed", "Checks not run or manual")),
            startRow = tot, startCol = 1, colNames = FALSE)
  writeFormula(wb, "Summary", x = c(sprintf("SUM(G2:G%d)", n + 1), sprintf('COUNTIF(I2:I%d,"Fail")', n + 1),
                                    sprintf('COUNTIF(I2:I%d,"Pass")', n + 1), sprintf('COUNTIF(I2:I%d,"Not run")', n + 1)),
               startCol = 7, startRow = tot)
  addStyle(wb, "Summary", S$bold, rows = tot:(tot + 3), cols = c(1, 7), gridExpand = TRUE)
  fo <- f[, c("finding_id", "level", "check_id", "severity", "dataset", "usubjid", "record", "value", "message")]
  names(fo) <- c("Finding_ID", "Level", "Check_ID", "Severity", "Dataset", "USUBJID", "Record", "Value", "Message")
  fo$Query_status <- rep("", nrow(fo)); fo$Resolution <- rep("", nrow(fo)); fo$Reviewer <- rep("", nrow(fo))
  xl_sheet(wb, "Findings", fo, widths = c(10, 7, 16, 9, 10, 18, 26, 40, 50, 13, 40, 14), wrap_cols = c("Value", "Message"))
  if (is.null(key) || !nrow(key)) { saveWorkbook(wb, path, overwrite = TRUE); return(invisible(path)) }
  fk <- paste(f$check_id, f$usubjid)
  det <- key
  det$Detected <- vapply(strsplit(det$Expected_keys, "\\|"), function(k) if (all(k %in% fk)) "Yes" else "No", "")
  det$Findings_raised <- vapply(strsplit(det$Expected_keys, "\\|"), function(k) sum(fk %in% k), integer(1))
  xl_sheet(wb, "Seeded_vs_Detected", det[, c("Seed_ID", "Level", "USUBJID", "Error_seeded", "Target_check", "Checks_expected_to_flag", "Detected", "Findings_raised")],
           widths = c(9, 8, 18, 55, 18, 40, 10, 10), wrap_cols = c("Error_seeded", "Checks_expected_to_flag"))
  saveWorkbook(wb, path, overwrite = TRUE)
}

# Read a study workbook: sheets named as in STUDY_SHEETS; other sheets ignored
read_study_xlsx <- function(path) {
  sh <- intersect(excel_sheets(path), STUDY_SHEETS)
  out <- lapply(sh, function(s) {
    d <- as.data.frame(read_excel(path, sheet = s, col_types = "text"), stringsAsFactors = FALSE)
    for (v in names(d)) {
      x <- d[[v]]
      if (grepl("^ADSL|^ADRS|^ADTTE|^ADAE", s) && grepl("DT$", v) && v != "ASTDTF") {
        nx <- num(x); d[[v]] <- if (all(is.na(x) | !is.na(nx))) as.Date(nx, origin = "1899-12-30") else dt(x)
      } else if (all(is.na(x) | !is.na(num(x))) && !grepl("DTC$|^(SUBJID|SITEID|ECOG|AETOXGR|GRADE)$", v) && !grepl("^RAW", s) | v %in% c("DIAM_MM", "DOSE", "CYCLE", "AENUM")) {
        d[[v]] <- num(x)
      } else d[[v]] <- chr(x)
    }
    d
  })
  names(out) <- sh
  out
}

# Build all three workbooks in one call
build_onc_workbooks <- function(dir = ".", n = 120, seed = 2026, p = ONC_PAR) {
  clean <- simulate_onc_study(n, seed, p)
  sd <- apply_seeds(clean, p)
  res <- run_onc_checks(sd$study, p)
  f <- file.path(dir, c("CDISC_Oncology_Check_Specification.xlsx", "CDISC_Oncology_Simulated_Study.xlsx",
                        "CDISC_Oncology_Check_Findings.xlsx"))
  write_spec_xlsx(f[1]); write_study_xlsx(sd$study, sd$key, f[2]); write_findings_xlsx(res, sd$key, f[3])
  invisible(list(files = f, study = sd$study, key = sd$key, result = res))
}

# -----------------------------------------------------------------------------
# 8. Variable metadata and per-layer files (raw EDC, SDTM, ADaM)
# -----------------------------------------------------------------------------
md <- function(ds, var, label, origin, comment = "") data.frame(Dataset = ds, Variable = var, Label = label, Origin = origin,
                                                               Derivation_or_comment = comment, stringsAsFactors = FALSE)
ID <- function(ds) md(ds, c("STUDYID", "DOMAIN", "USUBJID"), c("Study Identifier", "Domain Abbreviation", "Unique Subject Identifier"),
                     c("Protocol", "Assigned", "Derived"), c("", "", "STUDYID-SITEID-subject number"))
ADID <- function(ds) md(ds, c("STUDYID", "USUBJID"), c("Study Identifier", "Unique Subject Identifier"), "Predecessor", c("DM.STUDYID", "DM.USUBJID"))

VAR_META <- rbind(
  md("RAW_DM", c("SUBJID", "SITEID", "BRTHDT", "SEX", "RACE", "ICDT", "RANDDT", "ARM", "ECOG", "MEASDIS"),
     c("Subject number", "Site number", "Date of birth", "Sex", "Race", "Date informed consent signed", "Date of randomisation",
       "Randomised treatment", "ECOG performance status at screening", "Measurable disease per RECIST 1.1 at baseline"), "CRF",
     c("Site-subject", "", "DD-MMM-YYYY", "M / F", "", "DD-MMM-YYYY", "DD-MMM-YYYY (IxRS)", "From IxRS", "0-4", "Y / N")),
  md("RAW_EX", c("SUBJID", "CYCLE", "EXSTDT", "EXENDT", "DOSE", "DOSEU", "DOSEADJ", "ADJREAS"),
     c("Subject number", "Cycle number", "Infusion start date", "Infusion end date", "Dose administered", "Dose unit",
       "Dose adjusted", "Reason for dose adjustment"), "CRF", c("", "21-day cycles", "DD-MMM-YYYY", "DD-MMM-YYYY", "", "mg", "Y / N", "")),
  md("RAW_TUMOR", c("SUBJID", "VISIT", "SCANDT", "LESIONID", "LESTYPE", "ORGAN", "NODAL", "METHOD", "DIAM_MM", "NTSTATUS"),
     c("Subject number", "Visit", "Scan date", "Lesion identifier", "Lesion type", "Organ / site", "Lymph node lesion",
       "Imaging method", "Diameter (mm)", "Non-target / new lesion status"), "CRF",
     c("", "BASELINE, WEEK 8, WEEK 16, ...", "DD-MMM-YYYY", "T01-T05 target, NT01-NT02 non-target, NEW01 new",
       "TARGET / NON-TARGET / NEW", "", "Y / N", "CT / MRI", "Longest diameter; short axis for nodal lesions; target lesions only",
       "PRESENT / ABSENT / UNEQUIVOCAL PROGRESSION")),
  md("RAW_RESP", c("SUBJID", "VISIT", "RESPDT", "TRGRESP", "NTRGRESP", "NEWLES", "OVRLRESP"),
     c("Subject number", "Visit", "Response assessment date", "Target lesion response", "Non-target lesion response",
       "New lesion present", "Overall response (investigator)"), "CRF",
     c("", "", "DD-MMM-YYYY", "CR / PR / SD / PD / NE", "CR / NON-CR/NON-PD / PD / NE", "Y / N", "CR / PR / SD / PD / NE")),
  md("RAW_AE", c("SUBJID", "AENUM", "AETERM", "AESTDT", "AEENDT", "ONGOING", "GRADE", "SERIOUS", "HOSP", "OUTCOME", "ACTION", "RELATED"),
     c("Subject number", "AE number", "Adverse event (verbatim)", "Start date", "End date", "Ongoing", "CTCAE grade", "Serious",
       "Required or prolonged hospitalisation", "Outcome", "Action taken with study treatment", "Related to study treatment"), "CRF",
     c("", "Sequential within subject", "", "DD-MMM-YYYY; UN-MMM-YYYY if day unknown", "DD-MMM-YYYY", "Y / N", "1-5", "Y / N", "Y / N", "", "", "")),
  md("RAW_DS", c("SUBJID", "EOTREAS", "EOTDT", "DTHDT", "DTHCAUSE", "LASTCONTDT"),
     c("Subject number", "Primary reason for end of treatment", "End of treatment date", "Date of death", "Primary cause of death",
       "Date of last contact"), "CRF", c("", "Blank while on treatment", "DD-MMM-YYYY", "DD-MMM-YYYY", "", "DD-MMM-YYYY")),
  rbind(ID("DM"), md("DM", c("SUBJID", "RFSTDTC", "RFENDTC", "RFXSTDTC", "RFXENDTC", "RFICDTC", "DTHDTC", "DTHFL", "SITEID", "BRTHDTC",
                            "AGE", "AGEU", "SEX", "RACE", "ARMCD", "ARM", "ACTARMCD", "ACTARM", "COUNTRY"),
     c("Subject Identifier for the Study", "Subject Reference Start Date/Time", "Subject Reference End Date/Time",
       "Date/Time of First Study Treatment", "Date/Time of Last Study Treatment", "Date/Time of Informed Consent",
       "Date/Time of Death", "Subject Death Flag", "Study Site Identifier", "Date/Time of Birth", "Age", "Age Units", "Sex", "Race",
       "Planned Arm Code", "Description of Planned Arm", "Actual Arm Code", "Description of Actual Arm", "Country"),
     c("CRF", "Derived", "Derived", "Derived", "Derived", "CRF", "CRF", "Derived", "CRF", "CRF", "Derived", "Assigned", "CRF", "CRF",
       "Assigned", "CRF", "Derived", "Derived", "Assigned"),
     c("", "First EXSTDTC", "Last EXENDTC", "First EXSTDTC", "Last EXENDTC", "", "", "Y when DTHDTC populated", "", "",
       "Integer years at informed consent", "", "", "", "", "", "From EXTRT; NOTTRT when no exposure", "", "ISO 3166 alpha-3"))),
  rbind(ID("EX"), md("EX", c("EXSEQ", "EXTRT", "EXDOSE", "EXDOSU", "EXDOSFRM", "EXROUTE", "EXADJ", "VISITNUM", "VISIT", "EXSTDTC", "EXENDTC", "EXSTDY", "EXENDY"),
     c("Sequence Number", "Name of Treatment", "Dose", "Dose Units", "Dose Form", "Route of Administration", "Reason for Dose Adjustment",
       "Visit Number", "Visit Name", "Start Date/Time of Treatment", "End Date/Time of Treatment", "Study Day of Start of Treatment",
       "Study Day of End of Treatment"),
     c("Derived", "CRF", "CRF", "CRF", "Assigned", "Assigned", "CRF", "Derived", "Derived", "CRF", "CRF", "Derived", "Derived"),
     c("", "", "", "", "", "", "", "Cycle number", "CYCLE n DAY 1", "", "", "Date - RFSTDTC (+1 on or after)", "Date - RFSTDTC (+1 on or after)"))),
  rbind(ID("AE"), md("AE", c("AESEQ", "AETERM", "AEDECOD", "AEBODSYS", "AETOXGR", "AESER", "AESDTH", "AESHOSP", "AESLIFE", "AESDISAB",
                            "AESCONG", "AESMIE", "AEACN", "AEREL", "AEOUT", "AESTDTC", "AEENDTC", "AESTDY", "AEENDY"),
     c("Sequence Number", "Reported Term for the Adverse Event", "Dictionary-Derived Term", "Body System or Organ Class",
       "Standard Toxicity Grade", "Serious Event", "Results in Death", "Requires or Prolongs Hospitalization", "Is Life Threatening",
       "Persist or Signif Disability/Incapacity", "Congenital Anomaly or Birth Defect", "Other Medically Important Serious Event",
       "Action Taken with Study Treatment", "Causality", "Outcome of Adverse Event", "Start Date/Time of Adverse Event",
       "End Date/Time of Adverse Event", "Study Day of Start of Adverse Event", "Study Day of End of Adverse Event"),
     c("Derived", "CRF", "Assigned", "Assigned", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "Derived", "Derived"),
     c("AENUM", "", "MedDRA PT", "MedDRA primary SOC", "CTCAE grade (protocol version)", "", "", "", "", "", "", "", "", "", "",
       "Partial (YYYY-MM) when day unknown", "", "Blank for partial dates", ""))),
  rbind(ID("TU"), md("TU", c("TUSEQ", "TULNKID", "TUTESTCD", "TUTEST", "TUORRES", "TUSTRESC", "TULOC", "TUMETHOD", "TUEVAL", "VISITNUM", "VISIT", "TUDTC", "TUDY"),
     c("Sequence Number", "Link ID", "Tumor Identification Short Name", "Tumor Identification Test Name", "Tumor Identification Result",
       "Tumor Identification Result Std. Format", "Location of the Tumor", "Method of Identification", "Evaluator", "Visit Number",
       "Visit Name", "Date/Time of Tumor Identification", "Study Day of Tumor Identification"),
     c("Derived", "CRF", "Assigned", "Assigned", "CRF", "Derived", "CRF", "CRF", "Assigned", "Derived", "CRF", "CRF", "Derived"),
     c("", "Lesion identifier; links TU to TR", "TUMIDENT", "", "TARGET / NON-TARGET / NEW", "", "", "", "INVESTIGATOR", "1 = baseline", "", "", ""))),
  rbind(ID("TR"), md("TR", c("TRSEQ", "TRLNKID", "TRTESTCD", "TRTEST", "TRORRES", "TRORRESU", "TRSTRESC", "TRSTRESN", "TRSTRESU", "TRSTAT",
                            "TRREASND", "TRMETHOD", "TREVAL", "VISITNUM", "VISIT", "TRDTC", "TRDY"),
     c("Sequence Number", "Link ID", "Tumor/Lesion Assessment Short Name", "Tumor/Lesion Assessment Test Name", "Result or Finding in Original Units",
       "Original Units", "Character Result/Finding in Std Format", "Numeric Result/Finding in Standard Units", "Standard Units",
       "Completion Status", "Reason Not Done", "Method Used to Identify the Tumor", "Evaluator", "Visit Number", "Visit Name",
       "Date/Time of Tumor Measurement", "Study Day of Tumor Measurement"),
     c("Derived", "CRF", "Assigned", "Assigned", "CRF", "CRF", "Derived", "Derived", "Derived", "CRF", "CRF", "CRF", "Assigned", "Derived", "CRF", "CRF", "Derived"),
     c("", "Blank for SUMDIAM", "LDIAM, LPERP, TUMSTATE, SUMDIAM", "", "", "", "", "", "mm", "", "", "", "INVESTIGATOR", "", "", "",
       "SUMDIAM = sum of target lesion LDIAM/LPERP at the visit"))),
  rbind(ID("RS"), md("RS", c("RSSEQ", "RSTESTCD", "RSTEST", "RSCAT", "RSORRES", "RSSTRESC", "RSEVAL", "VISITNUM", "VISIT", "RSDTC", "RSDY"),
     c("Sequence Number", "Assessment Short Name", "Assessment Name", "Category for Assessment", "Result or Finding in Original Units",
       "Character Result/Finding in Std Format", "Evaluator", "Visit Number", "Visit Name", "Date/Time of Assessment", "Study Day of Assessment"),
     c("Derived", "Assigned", "Assigned", "Assigned", "CRF", "Derived", "Assigned", "Derived", "CRF", "CRF", "Derived"),
     c("", "TRGRESP, NTRGRESP, NEWLPROG, OVRLRESP", "", "RECIST 1.1", "", "", "INVESTIGATOR", "", "", "Scan date of the time point", ""))),
  rbind(ID("DS"), md("DS", c("DSSEQ", "DSTERM", "DSDECOD", "DSCAT", "EPOCH", "DSSTDTC", "DSSTDY"),
     c("Sequence Number", "Reported Term for the Disposition Event", "Standardized Disposition Term", "Category for Disposition Event",
       "Epoch", "Start Date/Time of Disposition Event", "Study Day of Start of Disposition Event"),
     c("Derived", "CRF", "Assigned", "Assigned", "Assigned", "CRF", "Derived"),
     c("", "", "NCOMPLT / protocol milestone codelists", "PROTOCOL MILESTONE / DISPOSITION EVENT", "SCREENING / TREATMENT / FOLLOW-UP", "", ""))),
  rbind(ADID("ADSL"), md("ADSL", c("SUBJID", "SITEID", "AGE", "AGEU", "SEX", "RACE", "ARM", "ACTARM", "TRT01P", "TRT01A", "RANDDT", "TRTSDT",
                                  "TRTEDT", "ITTFL", "SAFFL", "DTHFL", "DTHDT", "LSTALVDT", "DCUTDT"),
     c("Subject Identifier for the Study", "Study Site Identifier", "Age", "Age Units", "Sex", "Race", "Description of Planned Arm",
       "Description of Actual Arm", "Planned Treatment for Period 01", "Actual Treatment for Period 01", "Date of Randomization",
       "Date of First Exposure to Treatment", "Date of Last Exposure to Treatment", "Intent-To-Treat Population Flag",
       "Safety Population Flag", "Subject Death Flag", "Date of Death", "Date Last Known Alive", "Date of Data Cut-off"),
     c(rep("Predecessor", 8), "Derived", "Derived", "Derived", "Derived", "Derived", "Derived", "Derived", "Predecessor", "Derived", "Derived", "Assigned"),
     c("DM.SUBJID", "DM.SITEID", "DM.AGE", "DM.AGEU", "DM.SEX", "DM.RACE", "DM.ARM", "DM.ACTARM", "= ARM", "= ACTARM; blank if not treated",
       "DS.DSSTDTC where DSDECOD = RANDOMIZED", "min(EX.EXSTDTC)", "max(EX.EXENDTC)", "Y if randomised", "Y if any EX record",
       "DM.DTHFL", "DM.DTHDTC", "Death date, or last contact date", "SAP data cut-off"))),
  rbind(ADID("ADRS"), md("ADRS", c("TRT01A", "TRTSDT", "PARAMCD", "PARAM", "AVALC", "AVAL", "ADT", "AVISIT", "AVISITN", "ANL01FL", "SRCDOM", "SRCSEQ"),
     c("Actual Treatment for Period 01", "Date of First Exposure to Treatment", "Parameter Code", "Parameter", "Analysis Value (C)",
       "Analysis Value", "Analysis Date", "Analysis Visit", "Analysis Visit (N)", "Analysis Flag 01", "Source Data", "Source Sequence Number"),
     c("Predecessor", "Predecessor", "Assigned", "Assigned", "Derived", "Derived", "Derived", "Derived", "Derived", "Derived", "Assigned", "Predecessor"),
     c("ADSL.TRT01A", "ADSL.TRTSDT", "OVR (time point), BOR (best confirmed)", "",
       "OVR: RS.OVRLRESP. BOR: confirmed CR/PR (second CR/PR >= 28 days later); else SD if CR/PR/SD >= 42 days from first dose; else PD; else NE; assessments after PD excluded",
       "1 = CR, 2 = PR, 3 = SD, 4 = PD, 5 = NE", "OVR: RS.RSDTC. BOR: date of first assessment establishing the response", "", "", "", "", "RS.RSSEQ for OVR"))),
  rbind(ADID("ADTTE"), md("ADTTE", c("TRT01P", "TRT01A", "PARAMCD", "PARAM", "STARTDT", "ADT", "AVAL", "CNSR", "EVNTDESC", "CNSDTDSC", "SRCDOM"),
     c("Planned Treatment for Period 01", "Actual Treatment for Period 01", "Parameter Code", "Parameter", "Time to Event Origin Date for Subject",
       "Analysis Date", "Analysis Value", "Censor", "Event or Censoring Description", "Censor Date Description", "Source Data"),
     c("Predecessor", "Predecessor", "Assigned", "Assigned", "Derived", "Derived", "Derived", "Derived", "Derived", "Derived", "Assigned"),
     c("ADSL.TRT01P", "ADSL.TRT01A", "PFS, OS, DOR", "", "PFS/OS: ADSL.RANDDT. DOR: first CR/PR date",
       "PFS: first PD or death; censored at last adequate assessment, or randomisation if none; event after a gap > 126 days censored. OS: death, else last known alive",
       "ADT - STARTDT + 1 (days)", "0 = event, 1 = censored", "", "", ""))),
  rbind(ADID("ADAE"), md("ADAE", c("AESEQ", "TRT01A", "TRTSDT", "TRTEDT", "SAFFL", "AETERM", "AEDECOD", "AEBODSYS", "AETOXGR", "AETOXGRN", "AESER",
                                  "AEOUT", "ASTDT", "ASTDTF", "AENDT", "TRTEMFL"),
     c("Sequence Number", "Actual Treatment for Period 01", "Date of First Exposure to Treatment", "Date of Last Exposure to Treatment",
       "Safety Population Flag", "Reported Term for the Adverse Event", "Dictionary-Derived Term", "Body System or Organ Class",
       "Standard Toxicity Grade", "Standard Toxicity Grade (N)", "Serious Event", "Outcome of Adverse Event", "Analysis Start Date",
       "Analysis Start Date Imputation Flag", "Analysis End Date", "Treatment Emergent Analysis Flag"),
     c("Predecessor", "Predecessor", "Predecessor", "Predecessor", "Predecessor", "Predecessor", "Predecessor", "Predecessor", "Predecessor",
       "Derived", "Predecessor", "Predecessor", "Derived", "Derived", "Derived", "Derived"),
     c("AE.AESEQ", "ADSL.TRT01A", "ADSL.TRTSDT", "ADSL.TRTEDT", "ADSL.SAFFL", "", "", "", "", "Numeric AETOXGR", "", "",
       "AE.AESTDTC; partial YYYY-MM imputed to the first of the month", "D when the day was imputed", "AE.AEENDTC",
       "Y if TRTSDT <= ASTDT <= TRTEDT + 30")))
)

DS_DESC <- data.frame(
  Dataset = c("RAW_DM", "RAW_EX", "RAW_TUMOR", "RAW_RESP", "RAW_AE", "RAW_DS", "DM", "EX", "AE", "TU", "TR", "RS", "DS",
              "ADSL", "ADRS", "ADTTE", "ADAE"),
  Layer = c(rep("Raw", 6), rep("SDTM", 7), rep("ADaM", 4)),
  Description = c("Demography, consent, randomisation, eligibility", "Study drug administration", "Tumour assessment (RECIST 1.1 lesions)",
                  "Tumour response (investigator)", "Adverse events", "End of treatment, death and last contact",
                  "Demographics", "Exposure", "Adverse Events", "Tumor/Lesion Identification", "Tumor/Lesion Results",
                  "Disease Response and Clin Classification", "Disposition",
                  "Subject-Level Analysis Dataset", "Response Analysis Dataset", "Time-to-Event Analysis Dataset",
                  "Adverse Events Analysis Dataset"),
  Class = c(rep("EDC form", 6), "Special Purpose", "Interventions", "Events", "Findings", "Findings", "Findings", "Events",
            "ADSL", "BDS", "BDS (time to event)", "OCCDS"),
  Structure = c("One record per subject", "One record per subject per cycle", "One record per lesion per scan",
                "One record per subject per assessment", "One record per adverse event", "One record per subject",
                "One record per subject", "One record per constant-dosing interval", "One record per adverse event",
                "One record per identified lesion per evaluator", "One record per lesion measurement per visit per evaluator",
                "One record per response assessment per visit per evaluator", "One record per disposition event",
                "One record per subject", "One record per subject per parameter per analysis visit", "One record per subject per parameter",
                "One record per adverse event"),
  Keys = c("SUBJID", "SUBJID, CYCLE", "SUBJID, VISIT, LESIONID", "SUBJID, VISIT", "SUBJID, AENUM", "SUBJID",
           "STUDYID, USUBJID", "STUDYID, USUBJID, EXTRT, EXSTDTC", "STUDYID, USUBJID, AEDECOD, AESTDTC", "STUDYID, USUBJID, TUEVAL, TULNKID",
           "STUDYID, USUBJID, TREVAL, TRLNKID, TRTESTCD, VISITNUM", "STUDYID, USUBJID, RSEVAL, RSTESTCD, VISITNUM", "STUDYID, USUBJID, DSDECOD, DSSTDTC",
           "USUBJID", "USUBJID, PARAMCD, AVISITN", "USUBJID, PARAMCD", "USUBJID, AESEQ"),
  stringsAsFactors = FALSE)

write_layer_xlsx <- function(study, layer, path) {
  wb <- createWorkbook(); modifyBaseFont(wb, fontName = "Arial", fontSize = 10)
  S <- xl_styles(); dss <- DS_DESC[DS_DESC$Layer == layer, ]
  cont <- data.frame(Dataset = dss$Dataset, Description = dss$Description, Class = dss$Class, Structure = dss$Structure,
                     Keys = dss$Keys, Records = vapply(dss$Dataset, function(d) nrow(study[[d]]), integer(1)),
                     Subjects = vapply(dss$Dataset, function(d) length(unique(study[[d]][[if (layer == "Raw") "SUBJID" else "USUBJID"]])), integer(1)),
                     Variables = vapply(dss$Dataset, function(d) ncol(study[[d]]), integer(1)))
  intro <- switch(layer,
    Raw = "EDC export as received from the clinical database: one sheet per CRF form, dates as DD-MMM-YYYY, CRF field names.",
    SDTM = "SDTM tabulation datasets (SDTMIG v3.4 structure, CDISC CT values, ISO 8601 dates), mapped from the raw EDC export.",
    ADaM = "ADaM analysis datasets (ADaMIG v1.3 structure, numeric dates shown as yyyy-mm-dd), derived from SDTM.")
  addWorksheet(wb, "Contents", gridLines = FALSE)
  writeData(wb, "Contents", paste(ONC_PAR$study, "-", layer, "datasets"), startRow = 1); addStyle(wb, "Contents", S$title, rows = 1, cols = 1)
  writeData(wb, "Contents", c(intro, sprintf("Randomised 1:1 trial of Drug X versus standard of care; RECIST 1.1 assessment every 8 weeks; data cut-off %s; simulated data without errors.",
                                             format(ONC_PAR$dco))), startRow = 2)
  writeData(wb, "Contents", cont, startRow = 5, headerStyle = S$head)
  addStyle(wb, "Contents", S$wrap, rows = 6:(5 + nrow(cont)), cols = 1:8, gridExpand = TRUE)
  setColWidths(wb, "Contents", 1:8, c(12, 38, 18, 42, 44, 9, 9, 10))
  xl_sheet(wb, "Variable_Metadata", VAR_META[VAR_META$Dataset %in% dss$Dataset, ], widths = c(12, 12, 40, 12, 70),
           wrap_cols = c("Label", "Derivation_or_comment"))
  for (d in dss$Dataset) xl_sheet(wb, d, study[[d]])
  saveWorkbook(wb, path, overwrite = TRUE)
}

write_layer_xpt <- function(study, layer, dir) {
  dss <- DS_DESC$Dataset[DS_DESC$Layer == layer]
  files <- character()
  for (d in dss) {
    x <- study[[d]]
    for (v in names(x)) {
      if (inherits(x[[v]], "Date")) x[[v]] <- as.numeric(x[[v]] - as.Date("1960-01-01"))   # SAS date
      if (is.logical(x[[v]])) x[[v]] <- as.character(x[[v]])
      lab <- VAR_META$Label[VAR_META$Dataset == d & VAR_META$Variable == v]
      attr(x[[v]], "label") <- if (length(lab)) substr(lab, 1, 40) else v
      if (grepl("DT$", v) && grepl("^AD", d) && v != "ASTDTF") attr(x[[v]], "format.sas") <- "DATE9"
    }
    f <- file.path(dir, paste0(tolower(d), ".xpt"))
    haven::write_xpt(x, f, version = 5, name = d, label = substr(DS_DESC$Description[DS_DESC$Dataset == d], 1, 40))
    files <- c(files, f)
  }
  files
}

# Clean raw, SDTM and ADaM files (Excel per layer; SAS XPT v5 for SDTM and ADaM)
build_layer_files <- function(dir = ".", n = 120, seed = 2026, p = ONC_PAR) {
  st <- simulate_onc_study(n, seed, p)
  write_layer_xlsx(st, "Raw", file.path(dir, "ONC001_Raw_EDC.xlsx"))
  write_layer_xlsx(st, "SDTM", file.path(dir, "ONC001_SDTM.xlsx"))
  write_layer_xlsx(st, "ADaM", file.path(dir, "ONC001_ADaM.xlsx"))
  for (l in c("SDTM", "ADaM")) { xd <- file.path(dir, paste0("xpt_", tolower(l))); dir.create(xd, showWarnings = FALSE); write_layer_xpt(st, l, xd) }
  invisible(st)
}

# Read any mix of study workbooks (.xlsx; sheets named as datasets) and SAS transport files
# (.xpt; file name = dataset name). Later files replace earlier ones for the same dataset.
read_onc_files <- function(paths, names = basename(paths)) {
  st <- list()
  for (k in seq_along(paths)) {
    ext <- tolower(tools::file_ext(names[k]))
    if (ext %in% c("xlsx", "xls")) {
      x <- read_study_xlsx(paths[k]); st[names(x)] <- x
    } else if (ext == "xpt" && requireNamespace("haven", quietly = TRUE)) {
      nm <- toupper(tools::file_path_sans_ext(names[k]))
      if (!nm %in% STUDY_SHEETS) next
      x <- as.data.frame(haven::zap_formats(haven::zap_labels(haven::read_xpt(paths[k]))), stringsAsFactors = FALSE)
      for (v in names(x)) { attr(x[[v]], "label") <- NULL; if (is.character(x[[v]])) x[[v]] <- chr(x[[v]]) }
      st[[nm]] <- x
    }
  }
  st[intersect(STUDY_SHEETS, names(st))]
}

# A subject that shows the most of the data model: confirmed responder with several scans and AEs
featured_subject <- function(st) {
  if (!all(c("ADRS", "AE", "RS") %in% names(st))) return(chr(st[[intersect(c("DM", "ADSL"), names(st))[1]]]$USUBJID)[1])
  b <- st$ADRS[chr(st$ADRS$PARAMCD) == "BOR", ]
  nv <- table(chr(st$RS$USUBJID[chr(st$RS$RSTESTCD) == "OVRLRESP"]))
  c1 <- chr(b$USUBJID[chr(b$AVALC) == "PR"]); c1 <- c1[c1 %in% names(nv)[nv >= 5] & c1 %in% chr(st$AE$USUBJID)]
  if (length(c1)) c1[1] else chr(b$USUBJID)[1]
}
