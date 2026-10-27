# ONC-001 Data Cleaning Dashboard

R Shiny dashboard for clinical trial data cleaning with CDISC oncology logic checks across raw EDC, SDTM and ADaM data.

Author: Atanu Bhattacharjee

## What it does

**Generic data cleaning** (simulated trial data or any uploaded CSV)

- Raw data checks: every problem cell in the data as received (blanks, missing-value codes, whitespace, spelling variants, decimal commas, non-numeric entries, invalid or non-ISO dates, range-rule violations, ID format, duplicate IDs, exact duplicate records)
- Cleaning pipeline with an audit log of every changed value and a query listing of every rule violation
- Missingness, category harmonisation, editable range / unit / consistency rules, before-and-after distributions, outlier screening

**CDISC oncology** (simulated RECIST 1.1 trial ONC-001: Drug X versus standard of care, 120 subjects)

- Study data: 6 raw EDC forms, 7 SDTM domains (DM, EX, AE, TU, TR, RS, DS) and 4 ADaM datasets (ADSL, ADRS, ADTTE, ADAE) with variable labels, origin and derivation
- Logic checks: 78 checks (69 automated, 9 manual review) from raw through SDTM (SDTMIG v3.4) to ADaM (ADaMIG v1.3), with protocol and SAP parameters that can be changed in the app
- Patient journey: one subject from raw tumour form to SDTM TR/RS to ADaM ADRS/ADTTE, with tumour burden against RECIST 1.1 thresholds, dosing and adverse event timeline, and that subject's check findings
- Uploads: layer workbooks, a combined study workbook, or SAS transport files (one `.xpt` per dataset)
- Downloads: findings, check specification and study data as Excel

## Repository contents

| File | Purpose |
|---|---|
| `app.R` | Shiny app |
| `cdisc_oncology_checks.R` | Check specification, check engine, RECIST 1.1 and endpoint derivations, ONC-001 simulator, Excel writers |
| `data/ONC001_Raw_EDC.xlsx`, `data/ONC001_SDTM.xlsx`, `data/ONC001_ADaM.xlsx` | ONC-001 datasets loaded at start-up (simulated, no real patient data) |
| `renv.lock` | Package versions the app was tested with, used by Posit Connect Cloud |

## Run locally

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "openxlsx", "readxl", "haven"))
shiny::runApp("path/to/onc001-data-cleaning-dashboard")
```

## Deploy on Posit Connect Cloud

1. Sign in at connect.posit.cloud with the GitHub account that holds this repository.
2. Choose **Publish**, then **Shiny**.
3. Select this repository, branch `main`, primary file `app.R`.
4. Publish. Connect Cloud installs the packages listed in `renv.lock`.

## Standards and references

SDTMIG v3.4; ADaMIG v1.3; CDISC Controlled Terminology; RECIST 1.1 (Eisenhauer et al., Eur J Cancer 2009;45:228-247); NCI CTCAE (protocol-specified version); FDA guidance on clinical trial endpoints for the approval of cancer drugs and biologics (2018).
