# =============================================================================
#  Data Cleaning Dashboard (R Shiny)
#  Author: Atanu Bhattacharjee
#
#  Opens on a raw data screen: every problem cell in the data as received is
#  listed before any change is made. Cleaning then runs on a simulated clinical
#  trial dataset that carries realistic data errors, or on any uploaded CSV.
#  Every change is written to an audit log and
#  every rule violation to a query listing; cleaned data, log and queries can
#  be downloaded.
#
#  The CDISC oncology tab runs raw, SDTM and ADaM checks for a RECIST 1.1
#  study from cdisc_oncology_checks.R (keep it in the same folder).
#
#  Packages: shiny, bslib, DT, ggplot2 (+ openxlsx, readxl for the CDISC tab)
#  Run:      shiny::runApp("path/to/onc001-data-cleaning-dashboard")
#            or open app.R in RStudio and press Run App
#  Deploy:   Posit Connect Cloud from GitHub (primary file app.R; packages from renv.lock)
# =============================================================================

# ---- Packages: stop with a clear instruction rather than a cryptic error ----------
local({
  need <- c("shiny", "bslib", "DT", "ggplot2", "openxlsx", "readxl")
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss))
    stop("The dashboard needs these packages, which are not installed: ", paste(miss, collapse = ", "),
         "\nInstall them with:\n  install.packages(c(", paste0('"', miss, '"', collapse = ", "), "))\n",
         call. = FALSE)
  if (utils::packageVersion("bslib") < "0.6.0")
    stop("bslib ", utils::packageVersion("bslib"), " is installed; the dashboard needs 0.6.0 or later.\n",
         "Update it with: install.packages(\"bslib\")", call. = FALSE)
  if (utils::packageVersion("shiny") < "1.7.4")
    stop("shiny ", utils::packageVersion("shiny"), " is installed; the dashboard needs 1.7.4 or later.\n",
         "Update it with: install.packages(\"shiny\")", call. = FALSE)
})

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(ggplot2)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

PAL <- c(raw = "#D1495B", clean = "#2E86AB", flag = "#E07A1F", ok = "#7A8B99")

# CDISC oncology raw / SDTM / ADaM checks (cdisc_oncology_checks.R in the same folder)
# Folder of this app: the working directory when started by runApp / Run App, or the
# folder of the file when it is run with source() from somewhere else
SOURCED_FROM <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL)
APP_DIR <- local({
  cands <- unique(c(getwd(), if (!is.null(SOURCED_FROM)) dirname(SOURCED_FROM)))
  hit <- cands[file.exists(file.path(cands, "cdisc_oncology_checks.R"))]
  if (length(hit)) hit[1] else cands[length(cands)]
})
ONC_ENGINE <- file.path(APP_DIR, "cdisc_oncology_checks.R")
HAS_ONC <- file.exists(ONC_ENGINE)
if (HAS_ONC) source(ONC_ENGINE, local = TRUE)

# -----------------------------------------------------------------------------
# 1. Simulated trial data with deliberate errors
# -----------------------------------------------------------------------------
simulate_trial <- function(n = 300, seed = 2026) {
  set.seed(seed)
  sex    <- sample(c("Male", "Female"), n, TRUE)
  height <- round(ifelse(sex == "Male", rnorm(n, 176, 7), rnorm(n, 163, 6)), 1)
  weight <- round(rnorm(n, 28, 4.5) * (height / 100)^2, 1)
  sbp    <- round(rnorm(n, 138, 17))
  dbp    <- round(0.55 * sbp + rnorm(n, 8, 7))
  rand   <- as.Date("2025-01-06") + sort(sample(0:400, n, TRUE))
  visit  <- rand + round(rnorm(n, 90, 7))
  died   <- rbinom(n, 1, 0.05)
  ddate  <- rep("", n)
  ddate[died == 1] <- as.character(visit[died == 1] + sample(5:200, sum(died), TRUE))

  d <- data.frame(
    subject_id = sprintf("DUN-%03d", seq_len(n)),
    site       = sample(c("Dundee", "Glasgow", "Aberdeen", "Edinburgh"), n, TRUE,
                        c(.35, .25, .20, .20)),
    arm        = sample(c("Treatment", "Placebo"), n, TRUE),
    sex        = sex,
    age        = as.character(round(rnorm(n, 58, 11))),
    height_cm  = as.character(height),
    weight_kg  = as.character(weight),
    sbp        = as.character(sbp),
    dbp        = as.character(dbp),
    hba1c      = as.character(round(rnorm(n, 58, 12))),
    egfr       = as.character(round(pmin(pmax(rnorm(n, 78, 20), 8), 130))),
    smoking    = sample(c("Never", "Former", "Current"), n, TRUE, c(.50, .33, .17)),
    rand_date  = as.character(rand),
    visit_date = as.character(visit),
    died       = as.character(died),
    death_date = ddate,
    consent    = "Y",
    stringsAsFactors = FALSE
  )

  k   <- function(m) sample.int(n, max(1, round(m * n / 300)))
  put <- function(col, m, vals) {
    i <- k(m)
    d[[col]][i] <<- if (is.function(vals)) vals(d[[col]][i]) else sample(vals, length(i), TRUE)
  }

  # identifiers
  put("subject_id", 6, function(x) tolower(sub("-", "", x)))
  put("subject_id", 5, function(x) paste0(" ", x, " "))
  # inconsistent category coding
  put("site", 12, c("dundee", "DUNDEE", "Glasgow ", "Abdn", "edinburgh"))
  put("arm", 10, c("Trt", "PBO", "placebo", "treatment"))
  put("sex", 12, c("M", "F", "male", "f", "U"))
  put("smoking", 15, c("ex", "Ex-smoker", "never", "CURRENT", ""))
  put("consent", 3, "y")
  i <- k(2); d$consent[i] <- "N"
  # impossible values, missing codes, wrong units, decimal commas
  put("age", 4, c("-2", "150", "999", ""))
  put("age", 4, "-99")
  put("height_cm", 5, function(x) as.character(round(as.numeric(x) / 100, 2)))
  put("height_cm", 3, "-99")
  put("weight_kg", 4, function(x) as.character(round(as.numeric(x) * 2.2046, 1)))
  put("weight_kg", 5, function(x) sub("\\.", ",", x))
  put("weight_kg", 3, c("-99", ""))
  put("sbp", 3, "999")
  i <- k(4); tmp <- d$sbp[i]; d$sbp[i] <- d$dbp[i]; d$dbp[i] <- tmp
  put("hba1c", 6, function(x) as.character(round(as.numeric(x) / 10.929 + 2.15, 1)))
  put("hba1c", 4, c("NA", "N/A", ""))
  put("egfr", 3, function(x) paste0("-", x))
  put("egfr", 2, c(".", "pending"))
  # date problems
  i <- k(4); d$visit_date[i] <- as.character(rand[i] - sample(3:20, length(i), TRUE))
  put("rand_date", 10, function(x) format(as.Date(x), "%d/%m/%Y"))
  put("rand_date", 2, c("2025-02-30", "2025-13-01"))
  i <- sample(which(died == 0), 3); d$death_date[i] <- as.character(visit[i] + 30)
  # duplicate IDs with conflicting data, and exact duplicate rows
  i <- sample.int(n, 3); j <- sample(setdiff(seq_len(n), i), 3)
  d$subject_id[j] <- d$subject_id[i]
  dupi <- sample.int(n, 6)
  d <- rbind(d, d[dupi, ])
  d <- d[order(c(seq_len(n), dupi + 0.5)), ]
  rownames(d) <- NULL
  d
}

SIM_RANGES <- data.frame(
  variable = c("age", "height_cm", "weight_kg", "sbp", "dbp", "hba1c", "egfr"),
  min      = c(18, 120, 30, 70, 40, 20, 0),
  max      = c(100, 220, 250, 250, 150, 200, 200),
  action   = "Set missing",
  stringsAsFactors = FALSE
)

SIM_UNITS <- paste(
  "height_cm ; x < 3 ; x * 100",
  "weight_kg ; x / (height_cm / 100)^2 > 50 ; round(x / 2.2046, 1)",
  "hba1c ; x < 20 ; round(10.929 * (x - 2.15))",
  sep = "\n")

SIM_LOGIC <- paste(
  "Visit on or after randomisation: visit_date >= rand_date",
  "Diastolic below systolic: dbp < sbp",
  "Death date only when died = 1: is.na(death_date) | died == 1",
  "Consent recorded as Yes: consent == 'Yes'",
  sep = "\n")

# -----------------------------------------------------------------------------
# 2. Helpers
# -----------------------------------------------------------------------------
DATE_FORMATS <- c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y", "%Y/%m/%d", "%d %b %Y")

parse_dates <- function(x) {
  out <- as.Date(rep(NA_character_, length(x)))
  for (f in DATE_FORMATS) {
    todo <- is.na(out) & !is.na(x)
    if (!any(todo)) break
    out[todo] <- suppressWarnings(as.Date(x[todo], format = f))
  }
  out
}

is_date_col <- function(name, x) {
  nm <- !is.na(x)
  if (!any(nm)) return(grepl("date", name, ignore.case = TRUE))
  if (grepl("date", name, ignore.case = TRUE)) return(TRUE)
  mean(!is.na(parse_dates(x[nm]))) >= 0.8 && !all(grepl("^-?[0-9.,]+$", x[nm]))
}

as_num <- function(x) suppressWarnings(as.numeric(ifelse(grepl("^-?[0-9]+,[0-9]+$", x),
                                                          sub(",", ".", x), x)))

mostly_numeric <- function(x) {
  nm <- !is.na(x) & trimws(x) != ""
  sum(nm) > 0 && mean(!is.na(as_num(trimws(x[nm])))) >= 0.9
}

log_diff <- function(old, new, rows, ids, var, step, reason) {
  o <- as.character(old); nn <- as.character(new)
  ch <- !(is.na(o) & is.na(nn)) & (is.na(o) | is.na(nn) | o != nn)
  if (!any(ch)) return(NULL)
  data.frame(row = rows[ch], subject_id = ids[ch], variable = var, step = step,
             old_value = o[ch], new_value = nn[ch], reason = reason,
             stringsAsFactors = FALSE)
}

make_query <- function(rows, ids, var, check, value, message) {
  if (length(rows) == 0) return(NULL)
  data.frame(row = rows, subject_id = ids, variable = var, check = check,
             value = as.character(value), message = message, stringsAsFactors = FALSE)
}

parse_rule_lines <- function(txt, sep) {
  lines <- trimws(strsplit(txt %||% "", "\n")[[1]])
  lines[nzchar(lines) & !startsWith(lines, "#")]
}

# Suggested harmonised level for each raw spelling
SYNONYMS <- c(m = "Male", male = "Male", f = "Female", female = "Female",
              trt = "Treatment", treatment = "Treatment", active = "Treatment",
              pbo = "Placebo", placebo = "Placebo",
              never = "Never", ex = "Former", exsmoker = "Former", former = "Former",
              current = "Current", y = "Yes", yes = "Yes", n = "No", no = "No",
              abdn = "Aberdeen", u = "", unknown = "", unk = "")

suggest_levels <- function(vals, counts) {
  key <- tolower(gsub("[^A-Za-z0-9]", "", vals))
  out <- character(length(vals))
  for (kk in unique(key)) {
    g <- which(key == kk)
    if (kk %in% names(SYNONYMS)) { out[g] <- SYNONYMS[[kk]]; next }
    canon <- vals[g][which.max(counts[g])]
    if (nchar(canon) > 3 && (canon == toupper(canon) || canon == tolower(canon)))
      canon <- tools::toTitleCase(tolower(canon))
    out[g] <- canon
  }
  out
}

categorical_vars <- function(raw, id_col, codes, max_levels = 30) {
  vars <- setdiff(names(raw), c(id_col, ".row"))
  keep <- vapply(vars, function(v) {
    x <- trimws(raw[[v]])
    x <- x[!is.na(x) & x != "" & !(toupper(x) %in% toupper(codes))]
    length(x) > 0 && !mostly_numeric(x) && !is_date_col(v, x) &&
      length(unique(x)) <= max_levels
  }, logical(1))
  vars[keep]
}

default_map <- function(raw, id_col, codes) {
  cv <- categorical_vars(raw, id_col, codes)
  if (length(cv) == 0)
    return(data.frame(variable = character(), raw_value = character(), n = integer(),
                      clean_value = character(), stringsAsFactors = FALSE))
  do.call(rbind, lapply(cv, function(v) {
    x <- trimws(raw[[v]])
    x <- x[!is.na(x) & x != "" & !(toupper(x) %in% toupper(codes))]
    tb <- sort(table(x), decreasing = TRUE)
    vals <- names(tb); cnt <- as.integer(tb)
    data.frame(variable = v, raw_value = vals, n = cnt,
               clean_value = suggest_levels(vals, cnt), stringsAsFactors = FALSE)
  }))
}

# -----------------------------------------------------------------------------
# 3a. Raw data screening (nothing is changed; every problem cell is listed)
# -----------------------------------------------------------------------------
ISSUE_ORDER <- c("Blank cell", "Missing-value code", "Extra whitespace", "Spelling/coding variant",
                 "Decimal comma", "Non-numeric entry", "Non-ISO date format", "Invalid date",
                 "Outside range rule", "ID format variant", "Duplicate ID", "Exact duplicate record")

raw_screen <- function(raw, id_col = NULL, codes = character(), id_pad = 3, ranges = NULL) {
  vars <- names(raw); out <- list()
  ids <- if (!is.null(id_col) && id_col %in% vars) trimws(raw[[id_col]]) else as.character(seq_len(nrow(raw)))
  add <- function(i, v, issue, val) if (length(i))
    out[[length(out) + 1]] <<- data.frame(row = i, subject_id = ids[i], variable = v, issue = issue,
                                          raw_value = as.character(val), stringsAsFactors = FALSE)
  cats <- categorical_vars(raw, id_col, codes)
  for (v in vars) {
    x <- raw[[v]]; x[is.na(x)] <- ""
    tx <- trimws(x)
    blank <- tx == ""; code <- !blank & toupper(tx) %in% toupper(codes)
    add(which(blank), v, "Blank cell", x[blank])
    add(which(code), v, "Missing-value code", x[code])
    ws <- !blank & x != trimws(gsub("[[:space:]]+", " ", x))
    add(which(ws), v, "Extra whitespace", paste0("'", x[ws], "'"))
    ok <- !blank & !code
    if (!is.null(id_col) && identical(v, id_col)) {
      fmt <- ok & !grepl(sprintf("^[A-Z]+-[0-9]{%d}$", as.integer(id_pad)), x)
      add(which(fmt), v, "ID format variant", x[fmt])
      pat <- "^([A-Z]+)[-_ ]?0*([0-9]+)$"; u <- toupper(tx)
      std <- ifelse(grepl(pat, u), paste(sub(pat, "\\1", u), as.integer(sub(pat, "\\2", u))), u)
      key <- do.call(paste, c(lapply(raw[setdiff(vars, id_col)], function(z) trimws(z)), sep = "\r"))
      grp <- split(seq_along(std), std)
      for (g in grp[lengths(grp) > 1]) if (length(unique(key[g])) > 1)
        add(g, v, "Duplicate ID", x[g])
      next
    }
    if (v %in% cats) {
      vals <- unique(tx[ok]); cnt <- as.integer(table(factor(tx[ok], levels = vals)))
      sug <- suggest_levels(vals, cnt)
      # canonical spelling = most frequent raw spelling of each harmonised level
      canon <- tapply(seq_along(vals), sug, function(i) vals[i][which.max(cnt[i])])
      bad <- vals[sug == "" | vals != canon[sug]]
      i <- which(ok & tx %in% bad); add(i, v, "Spelling/coding variant", tx[i])
      next
    }
    if (!any(ok)) next
    if (is_date_col(v, tx[ok])) {
      p <- parse_dates(tx)
      inv <- ok & is.na(p); add(which(inv), v, "Invalid date", x[inv])
      non <- ok & !is.na(p) & tx != format(p, "%Y-%m-%d"); add(which(non), v, "Non-ISO date format", x[non])
    } else if (mostly_numeric(tx[ok])) {
      cm <- ok & grepl("^-?[0-9]+,[0-9]+$", tx); add(which(cm), v, "Decimal comma", x[cm])
      num <- as_num(tx)
      nn <- ok & is.na(num); add(which(nn), v, "Non-numeric entry", x[nn])
      if (!is.null(ranges) && v %in% ranges$variable) {
        r <- ranges[ranges$variable == v, ][1, ]
        lo <- suppressWarnings(as.numeric(r$min)); hi <- suppressWarnings(as.numeric(r$max))
        oor <- ok & !is.na(num) & ((!is.na(lo) & num < lo) | (!is.na(hi) & num > hi))
        add(which(oor), v, "Outside range rule", x[oor])
      }
    }
  }
  key <- do.call(paste, c(lapply(raw, function(z) ifelse(is.na(z), "<NA>", z)), sep = "\r"))
  dup <- which(duplicated(key))
  add(dup, "(whole record)", "Exact duplicate record",
      paste("identical to row", match(key[dup], key)))
  res <- if (length(out)) do.call(rbind, out) else
    data.frame(row = integer(), subject_id = character(), variable = character(),
               issue = character(), raw_value = character())
  res$issue <- factor(res$issue, levels = ISSUE_ORDER)
  res <- res[order(res$row, res$variable, res$issue), ]
  res$issue <- as.character(res$issue); rownames(res) <- NULL
  res
}

# -----------------------------------------------------------------------------
# 3b. Cleaning pipeline
# -----------------------------------------------------------------------------
STEPS <- c("Trim whitespace" = "trim", "Standardise IDs" = "id",
           "Convert missing codes" = "missing", "Harmonise categories" = "recode",
           "Convert types (numbers, dates)" = "types", "Unit corrections" = "units",
           "Range checks" = "range", "Remove exact duplicates" = "dedupe",
           "Consistency checks" = "logic")

run_pipeline <- function(raw, cfg) {
  d <- raw
  d$.row <- seq_len(nrow(d))
  vars <- setdiff(names(d), ".row")
  logs <- list(); qs <- list(); errs <- character(); types <- setNames(rep("text", length(vars)), vars)
  idc <- if (!is.null(cfg$id_col) && cfg$id_col %in% vars) cfg$id_col else NULL
  ids <- function() if (is.null(idc)) as.character(d$.row) else as.character(d[[idc]])
  add_log <- function(x) if (!is.null(x)) logs[[length(logs) + 1]] <<- x
  add_q   <- function(x) if (!is.null(x)) qs[[length(qs) + 1]] <<- x
  on <- function(s) s %in% cfg$steps
  codes <- trimws(strsplit(cfg$codes %||% "", ",")[[1]]); codes <- codes[nzchar(codes)]

  # Step 1: whitespace
  if (on("trim")) for (v in vars) if (is.character(d[[v]])) {
    new <- trimws(gsub("[[:space:]]+", " ", d[[v]]))
    add_log(log_diff(d[[v]], new, d$.row, ids(), v, "1 Trim whitespace",
                     "Leading, trailing or repeated spaces removed"))
    d[[v]] <- new
  }

  # Step 2: identifier format PREFIX-000
  if (on("id") && !is.null(idc)) {
    x <- toupper(trimws(d[[idc]])); new <- x
    pat <- "^([A-Z]+)[-_ ]?0*([0-9]+)$"; ok <- !is.na(x) & grepl(pat, x)
    new[ok] <- sprintf("%s-%0*d", sub(pat, "\\1", x[ok]), as.integer(cfg$id_pad %||% 3),
                       as.integer(sub(pat, "\\2", x[ok])))
    old <- d[[idc]]; d[[idc]] <- new
    add_log(log_diff(old, new, d$.row, ids(), idc, "2 Standardise IDs",
                     "Identifier reformatted to PREFIX-number"))
    bad <- which(!is.na(new) & !grepl("^[A-Z]+-[0-9]+$", new))
    add_q(make_query(d$.row[bad], ids()[bad], idc, "ID format", new[bad],
                     "Identifier does not follow the PREFIX-number pattern"))
  }

  # Step 3: missing-value codes
  if (on("missing")) for (v in vars) if (is.character(d[[v]])) {
    x <- d[[v]]
    hit <- !is.na(x) & (toupper(trimws(x)) %in% toupper(codes) | trimws(x) == "")
    if (any(hit)) {
      new <- x; new[hit] <- NA
      coded <- hit & trimws(x) != ""
      add_log(log_diff(x[coded], new[coded], d$.row[coded], ids()[coded], v, "3 Missing codes",
                       "Missing-value code set to NA"))
      d[[v]] <- new
    }
  }

  # Step 4: harmonise categorical spellings
  if (on("recode") && !is.null(cfg$map) && nrow(cfg$map) > 0) {
    for (v in intersect(unique(cfg$map$variable), vars)) {
      mm <- cfg$map[cfg$map$variable == v, ]
      x <- d[[v]]; idx <- match(trimws(x), mm$raw_value); has <- !is.na(idx) & !is.na(x)
      new <- x; cv <- mm$clean_value[idx[has]]; cv[is.na(cv) | trimws(cv) == ""] <- NA
      new[has] <- cv
      add_log(log_diff(x, new, d$.row, ids(), v, "4 Harmonise categories",
                       "Spelling/coding mapped to harmonised level"))
      d[[v]] <- new
      types[v] <- "category"
    }
  }

  # Step 5: types
  if (on("types")) for (v in setdiff(vars, idc)) {
    x <- d[[v]]; if (!is.character(x)) next
    nm <- !is.na(x)
    if (types[v] != "category" && is_date_col(v, x[nm])) {
      p <- parse_dates(x)
      ref <- nm & !is.na(p) & x != format(p, "%Y-%m-%d")
      add_log(log_diff(x[ref], format(p[ref], "%Y-%m-%d"), d$.row[ref], ids()[ref], v,
                       "5 Convert types", "Date standardised to ISO 8601 (day-first input assumed)"))
      fail <- nm & is.na(p)
      add_log(log_diff(x[fail], rep(NA, sum(fail)), d$.row[fail], ids()[fail], v,
                       "5 Convert types", "Invalid or unparseable date set to NA"))
      add_q(make_query(d$.row[fail], ids()[fail], v, "Invalid date", x[fail],
                       "Date could not be parsed; source value needed"))
      d[[v]] <- p; types[v] <- "date"
    } else if (types[v] != "category" && mostly_numeric(x)) {
      num <- as_num(x)
      comma <- nm & grepl("^-?[0-9]+,[0-9]+$", x)
      add_log(log_diff(x[comma], num[comma], d$.row[comma], ids()[comma], v,
                       "5 Convert types", "Decimal comma converted to decimal point"))
      fail <- nm & is.na(num)
      add_log(log_diff(x[fail], rep(NA, sum(fail)), d$.row[fail], ids()[fail], v,
                       "5 Convert types", "Non-numeric entry in numeric field set to NA"))
      add_q(make_query(d$.row[fail], ids()[fail], v, "Non-numeric entry", x[fail],
                       "Text found in a numeric field; source value needed"))
      d[[v]] <- num; types[v] <- "numeric"
    } else if (types[v] == "text" && length(unique(x[nm])) <= 30) {
      types[v] <- "category"
    }
  }

  # Step 6: unit corrections   "variable ; condition ; transformation"
  if (on("units")) for (ln in parse_rule_lines(cfg$units)) {
    p <- trimws(strsplit(ln, ";")[[1]])
    if (length(p) < 3 || !(p[1] %in% vars) || !is.numeric(d[[p[1]]])) {
      errs <- c(errs, paste0("Unit rule skipped (variable missing or not numeric): ", ln)); next
    }
    res <- tryCatch({
      env <- c(list(x = d[[p[1]]]), as.list(d))
      cond <- as.logical(eval(parse(text = p[2]), env)); cond[is.na(cond)] <- FALSE
      tr <- eval(parse(text = p[3]), env)
      new <- d[[p[1]]]; new[cond] <- tr[cond]
      add_log(log_diff(d[[p[1]]], new, d$.row, ids(), p[1], "6 Unit corrections",
                       paste0("Unit correction where ", p[2], " -> ", p[3])))
      d[[p[1]]] <- new; TRUE
    }, error = function(e) conditionMessage(e))
    if (!isTRUE(res)) errs <- c(errs, paste0("Unit rule error in '", ln, "': ", res))
  }

  # Step 7: plausible ranges
  if (on("range") && !is.null(cfg$ranges)) for (r in seq_len(nrow(cfg$ranges))) {
    v <- cfg$ranges$variable[r]
    if (!(v %in% vars) || !is.numeric(d[[v]])) next
    lo <- suppressWarnings(as.numeric(cfg$ranges$min[r])); hi <- suppressWarnings(as.numeric(cfg$ranges$max[r]))
    x <- d[[v]]
    bad <- !is.na(x) & ((!is.na(lo) & x < lo) | (!is.na(hi) & x > hi))
    if (!any(bad)) next
    b <- which(bad)
    add_q(make_query(d$.row[b], ids()[b], v, "Range check", x[b],
                     sprintf("Outside plausible range [%s, %s]", lo, hi)))
    if (identical(cfg$ranges$action[r], "Set missing")) {
      new <- x; new[bad] <- NA
      add_log(log_diff(x, new, d$.row, ids(), v, "7 Range checks",
                       sprintf("Outside plausible range [%s, %s] set to NA", lo, hi)))
      d[[v]] <- new
    }
  }

  # Step 8: duplicates
  if (on("dedupe")) {
    key <- do.call(paste, c(lapply(d[vars], function(z) ifelse(is.na(z), "<NA>", as.character(z))), sep = "\r"))
    dup <- duplicated(key)
    if (any(dup)) {
      first <- d$.row[match(key[dup], key)]
      add_log(data.frame(row = d$.row[dup], subject_id = ids()[dup], variable = "(whole record)",
                         step = "8 Remove duplicates", old_value = "record present",
                         new_value = "record removed",
                         reason = paste("Exact duplicate of row", first), stringsAsFactors = FALSE))
      d <- d[!dup, , drop = FALSE]
    }
  }
  if (!is.null(idc)) {
    idv <- d[[idc]]; dd <- !is.na(idv) & (duplicated(idv) | duplicated(idv, fromLast = TRUE))
    if (any(dd)) add_q(make_query(d$.row[dd], idv[dd], idc, "Duplicate ID", idv[dd],
                                  "Same identifier on records with different data"))
  }

  # Step 9: consistency rules   "label: expression that must be TRUE"
  if (on("logic")) for (ln in parse_rule_lines(cfg$logic)) {
    pos <- regexpr(":", ln, fixed = TRUE)
    if (pos < 1) { errs <- c(errs, paste0("Consistency rule needs 'label: expression': ", ln)); next }
    lab <- trimws(substr(ln, 1, pos - 1)); ex <- trimws(substring(ln, pos + 1))
    res <- tryCatch({
      e <- parse(text = ex)
      ok <- as.logical(eval(e, as.list(d)))
      if (length(ok) == 1) ok <- rep(ok, nrow(d))
      fail <- which(!is.na(ok) & !ok)
      used <- intersect(all.vars(e), vars)
      val <- if (length(used)) do.call(paste, c(lapply(used, function(u)
        paste0(u, "=", as.character(d[[u]][fail]))), sep = "; ")) else rep("", length(fail))
      add_q(make_query(d$.row[fail], ids()[fail], paste(used, collapse = ", "), lab, val,
                       paste("Fails:", ex)))
      TRUE
    }, error = function(e) conditionMessage(e))
    if (!isTRUE(res)) errs <- c(errs, paste0("Consistency rule error in '", ln, "': ", res))
  }

  log <- if (length(logs)) do.call(rbind, logs) else
    data.frame(row = integer(), subject_id = character(), variable = character(), step = character(),
               old_value = character(), new_value = character(), reason = character())
  q <- if (length(qs)) do.call(rbind, qs) else
    data.frame(row = integer(), subject_id = character(), variable = character(), check = character(),
               value = character(), message = character())
  q <- q[q$row %in% d$.row, , drop = FALSE]
  if (nrow(q)) {
    q <- q[order(q$row, q$check), ]
    q <- cbind(query_id = sprintf("Q%04d", seq_len(nrow(q))), q, status = "Open")
  }
  rownames(log) <- NULL; rownames(q) <- NULL
  list(data = d, log = log, queries = q, errors = errs, types = types)
}

variable_summary <- function(d, types) {
  vars <- setdiff(names(d), ".row")
  do.call(rbind, lapply(vars, function(v) {
    x <- d[[v]]; n <- length(x); miss <- sum(is.na(x))
    smry <- if (is.numeric(x) && any(!is.na(x))) {
      q <- quantile(x, c(0, .25, .5, .75, 1), na.rm = TRUE)
      sprintf("mean %.1f (SD %.1f); median %.1f [IQR %.1f to %.1f]; range %.1f to %.1f",
              mean(x, na.rm = TRUE), sd(x, na.rm = TRUE), q[3], q[2], q[4], q[1], q[5])
    } else if (inherits(x, "Date") && any(!is.na(x))) {
      paste(format(min(x, na.rm = TRUE)), "to", format(max(x, na.rm = TRUE)))
    } else if (any(!is.na(x))) {
      tb <- sort(table(x), decreasing = TRUE)
      s <- paste0(names(tb)[1:min(5, length(tb))], " (", tb[1:min(5, length(tb))], ")", collapse = ", ")
      if (length(tb) > 5) paste0(s, ", ...") else s
    } else ""
    data.frame(variable = v,
               type = if (is.numeric(x)) "numeric" else if (inherits(x, "Date")) "date" else unname(types[v] %||% "text"),
               n = n - miss, missing = miss, pct_missing = round(100 * miss / max(n, 1), 1),
               distinct = length(unique(x[!is.na(x)])), summary = smry,
               stringsAsFactors = FALSE)
  }))
}

outlier_flags <- function(x, method, k) {
  if (sum(!is.na(x)) < 3) return(list(flag = rep(FALSE, length(x)), lo = NA, hi = NA))
  if (method == "iqr") {
    q <- quantile(x, c(.25, .75), na.rm = TRUE); i <- diff(q)
    lo <- q[1] - k * i; hi <- q[2] + k * i
  } else if (method == "z") {
    m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE); lo <- m - k * s; hi <- m + k * s
  } else {
    m <- median(x, na.rm = TRUE); s <- mad(x, constant = 1, na.rm = TRUE)
    lo <- m - k * s / 0.6745; hi <- m + k * s / 0.6745
  }
  list(flag = !is.na(x) & (x < lo | x > hi), lo = unname(lo), hi = unname(hi))
}

theme_dash <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.position = "top",
          plot.title.position = "plot", axis.title = element_text(colour = "#3d4852"))
}

# CDISC oncology pages (shown only when cdisc_oncology_checks.R sits beside this app)
ONC_NAV <- if (HAS_ONC) {
  nav_menu("CDISC oncology",
    nav_panel("Study data", value = "onc_data",
      layout_columns(col_widths = c(3, 9),
        card(card_header("ONC-001 datasets"),
             radioButtons("onc_src", NULL,
                          c("ONC-001 files (clean)" = "files", "ONC-001 with seeded errors" = "seeded",
                            "Upload files (.xlsx or .xpt)" = "upload")),
             conditionalPanel("input.onc_src == 'upload'",
               fileInput("onc_files", NULL, multiple = TRUE, accept = c(".xlsx", ".xpt")),
               p(class = "small text-muted", "Layer workbooks (ONC001_Raw_EDC, ONC001_SDTM, ONC001_ADaM), a combined study",
                 "workbook, or one .xpt per dataset (dm.xpt, adsl.xpt, ...). Sheets and files are matched by dataset name.")),
             uiOutput("onc_src_note"),
             div(class = "onc-inv", tableOutput("onc_inventory")),
             downloadButton("onc_dl_layers", "Layer workbooks (.zip)", class = "btn-outline-primary btn-sm w-100"),
             downloadButton("onc_dl_study", "Study data in one workbook (.xlsx)", class = "btn-outline-primary btn-sm w-100")),
        card(card_header(textOutput("onc_ds_head")),
             layout_columns(col_widths = c(3, 4, 5),
               selectInput("onc_layer", "Layer", c("Raw EDC" = "Raw", "SDTM" = "SDTM", "ADaM" = "ADaM")),
               selectInput("onc_ds", "Dataset", choices = NULL),
               selectInput("onc_subj", "Subject", choices = NULL)),
             uiOutput("onc_ds_facts"),
             navset_card_tab(
               nav_panel("Records", DTOutput("onc_ds_tbl")),
               nav_panel("Variables", DTOutput("onc_ds_meta"))))
      )
    ),
    nav_panel("Logic checks", value = "onc_checks",
      layout_columns(col_widths = c(3, 9),
        card(card_header("Protocol and SAP parameters"),
             textInput("onc_dco", "Data cut-off (YYYY-MM-DD)", value = "2026-06-30"),
             numericInput("onc_conf", "CR/PR confirmation interval (days)", 28, min = 1),
             numericInput("onc_sd", "Minimum SD duration from first dose (days)", 42, min = 1),
             numericInput("onc_gap", "Missed-assessment gap for PFS censoring (days)", 126, min = 1),
             numericInput("onc_teae", "TEAE window after last dose (days)", 30, min = 0),
             numericInput("onc_win", "Scan window around schedule (days)", 7, min = 0),
             numericInput("onc_blw", "Baseline imaging window (days)", 28, min = 1),
             p(class = "small text-muted", "Checks re-run on the selected study data whenever a value changes."),
             downloadButton("onc_dl_find", "Findings (.xlsx)", class = "btn-primary btn-sm w-100"),
             downloadButton("onc_dl_spec", "Check specification (.xlsx)", class = "btn-outline-primary btn-sm w-100")),
        div(
          uiOutput("onc_msg"),
          layout_column_wrap(width = 1/5, fill = FALSE,
            value_box("Checks run", textOutput("onc_vb_run"), theme = "light"),
            value_box("Checks with findings", textOutput("onc_vb_fail"), theme = "light"),
            value_box("Not run / manual review", textOutput("onc_vb_nr"), theme = "light"),
            value_box("Findings", textOutput("onc_vb_f"), theme = "light"),
            value_box("Critical findings", textOutput("onc_vb_c"), theme = "light")),
          card(card_header("Findings by check across raw, SDTM and ADaM"), plotOutput("onc_plot", height = "auto")),
          card(card_header("Check detail"),
               selectInput("onc_check", NULL, choices = NULL, width = "100%"),
               uiOutput("onc_check_spec"),
               DTOutput("onc_check_find"),
               p(class = "small text-muted", "Select a finding to open that subject on the Patient journey page.")),
          navset_card_tab(
            nav_panel("Check status", DTOutput("onc_status")),
            nav_panel("All findings", DTOutput("onc_findings")),
            nav_panel("Seeded errors vs detected", DTOutput("onc_seeded")),
            nav_panel("Specification", DTOutput("onc_spec")))
        )
      )
    ),
    nav_panel("Patient journey", value = "onc_journey",
      layout_columns(col_widths = c(3, 9),
        card(card_header("Subject"),
             selectInput("onc_ju", NULL, choices = NULL),
             uiOutput("onc_ju_facts")),
        div(
          card(card_header("Logic check findings for this subject"), DTOutput("onc_ju_find")),
          card(card_header("Tumour burden: SDTM TR sum of diameters, labelled with SDTM RS overall response"),
               plotOutput("onc_ju_tb", height = 330)),
          card(card_header("Dosing (SDTM EX) and adverse events (ADaM ADAE) on the study-day axis"),
               plotOutput("onc_ju_tl", height = 280)),
          card(card_header("Raw EDC: tumour assessment form (lesion by visit)"), DTOutput("onc_ju_raw")),
          layout_columns(col_widths = c(6, 6),
            card(card_header("SDTM: RS time-point responses"), DTOutput("onc_ju_rs")),
            card(card_header("ADaM: ADRS and ADTTE"), DTOutput("onc_ju_adrs"), tableOutput("onc_ju_adtte")))
        )
      )
    )
  )
} else {
  nav_panel("CDISC oncology",
    card(card_header("CDISC oncology checks unavailable"),
         p("cdisc_oncology_checks.R was not found in ", code(APP_DIR), "."),
         p("Keep app.R, cdisc_oncology_checks.R and the three ONC001 workbooks together in one folder, then restart the app.")))
}

# -----------------------------------------------------------------------------
# 4. User interface
# -----------------------------------------------------------------------------
dt_opts <- list(pageLength = 15, scrollX = TRUE, dom = "frtip")

ui <- page_navbar(
  id = "nav",
  title = "Data Cleaning Dashboard",
  theme = bs_theme(version = 5, bg = "#FFFFFF", fg = "#1F2933", primary = "#2E86AB",
                   secondary = "#7A8B99", "navbar-bg" = "#F2F6FA"),
  fillable = FALSE,
  header = tags$head(tags$style(HTML("
    table.dataTable td, table.dataTable th { white-space: nowrap; font-size: 12.5px; }
    table.dataTable thead th { vertical-align: bottom; }
    .onc-inv table { font-size: 12.5px; }"))),
  sidebar = sidebar(
    id = "side",
    width = 310,
    radioButtons("source", "Data source",
                 c("Simulated trial data" = "sim", "Upload CSV" = "upload")),
    conditionalPanel(
      "input.source == 'sim'",
      numericInput("n", "Number of participants", 300, min = 50, max = 5000, step = 50),
      numericInput("seed", "Random seed", 2026, min = 1)
    ),
    conditionalPanel(
      "input.source == 'upload'",
      fileInput("file", "CSV file", accept = c(".csv", "text/csv"))
    ),
    selectInput("id_col", "Participant ID column", choices = NULL),
    numericInput("id_pad", "ID digits (zero-padding)", 3, min = 1, max = 8),
    textInput("codes", "Missing-value codes (comma separated)",
              "-99, -999, 999, 9999, NA, N/A, ., unknown, not known"),
    checkboxInput("blank_issue", "Raw checks: count blank cells as issues", TRUE),
    checkboxGroupInput("steps", "Cleaning steps applied", STEPS, selected = STEPS),
    downloadButton("dl_raw_issues", "Raw data issue listing (.csv)", class = "btn-primary btn-sm w-100"),
    downloadButton("dl_clean", "Cleaned data (.csv)", class = "btn-outline-primary btn-sm w-100"),
    downloadButton("dl_log", "Audit log (.csv)", class = "btn-outline-primary btn-sm w-100"),
    downloadButton("dl_q", "Query listing (.csv)", class = "btn-outline-primary btn-sm w-100"),
    downloadButton("dl_dict", "Variable summary (.csv)", class = "btn-outline-primary btn-sm w-100")
  ),

  nav_panel("1. Raw data checks",
    layout_column_wrap(width = 1/5, fill = FALSE,
      value_box("Records x variables", textOutput("rb_dim"), theme = "light"),
      value_box("Cells with an issue", textOutput("rb_cells"), theme = "light"),
      value_box("Records with an issue", textOutput("rb_rows"), theme = "light"),
      value_box("Exact duplicate records", textOutput("rb_dups"), theme = "light"),
      value_box("IDs on conflicting records", textOutput("rb_dupid"), theme = "light")
    ),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Issues by variable and type (data as received, nothing changed)"),
           plotOutput("raw_matrix", height = 440)),
      card(card_header("Issue summary"),
           p(class = "small text-muted",
             "Click a row to list the affected records below. Range screening uses the rules on the Rules tab."),
           DTOutput("raw_issue_sum"))
    ),
    card(card_header(textOutput("raw_detail_head", inline = TRUE)), DTOutput("raw_issue_detail")),
    card(card_header("Raw data as received (cells with an issue shaded)"), DTOutput("raw_view"))
  ),

  nav_panel("2. Cleaning overview",
    layout_column_wrap(width = 1/5, fill = FALSE,
      value_box("Records received", textOutput("vb_raw"), theme = "light"),
      value_box("Records after cleaning", textOutput("vb_clean"), theme = "light"),
      value_box("Values changed", textOutput("vb_changes"), theme = "light"),
      value_box("Open queries", textOutput("vb_queries"), theme = "light"),
      value_box("Missing cells", textOutput("vb_missing"), theme = "light")
    ),
    layout_columns(col_widths = c(7, 5),
      card(card_header("Variable summary (cleaned data)"), DTOutput("var_summary")),
      card(card_header("Changes and queries by cleaning step"),
           plotOutput("step_plot", height = 330),
           plotOutput("query_plot", height = 280))
    )
  ),

  nav_panel("Missingness",
    layout_columns(col_widths = c(5, 7),
      card(card_header("Missing values by variable: as received vs after cleaning"),
           plotOutput("miss_dumbbell", height = 520),
           p(class = "text-muted small",
             "As received counts only empty cells. After cleaning adds missing-value codes,",
             "unparseable entries and out-of-range values set to missing.")),
      card(card_header("Missingness pattern (cleaned data)"),
           plotOutput("miss_pattern", height = 520),
           p(class = "text-muted small",
             "Records sorted by number of missing values; up to 1,000 records shown."))
    )
  ),

  nav_panel("Categories",
    layout_columns(col_widths = c(6, 6),
      card(card_header("Level mapping (edit the clean_value column)"),
           p(class = "small text-muted",
             "Double-click a clean_value cell to edit. Leave it blank to set that spelling to missing."),
           DTOutput("map_tbl"),
           actionButton("map_reset", "Reset to suggested mapping", class = "btn-sm btn-outline-secondary")),
      card(card_header("Raw spellings merged into each harmonised level"),
           selectInput("cat_var", NULL, choices = NULL),
           plotOutput("cat_plot", height = 420))
    )
  ),

  nav_panel("Rules",
    layout_columns(col_widths = c(5, 7),
      card(card_header("Plausible ranges (editable)"),
           p(class = "small text-muted",
             "Values outside [min, max] always raise a query. Action 'Set missing' also removes",
             "the value; 'Query only' keeps it. Double-click to edit."),
           DTOutput("range_tbl"),
           layout_columns(col_widths = c(6, 6),
             textInput("new_range_var", NULL, placeholder = "variable name"),
             actionButton("add_range", "Add range rule", class = "btn-sm btn-outline-secondary"))),
      card(card_header("Unit corrections and consistency checks"),
           textAreaInput("units", "Unit corrections - one per line: variable ; condition ; transformation  (x = the variable)",
                         SIM_UNITS, rows = 4, width = "100%"),
           textAreaInput("logic", "Consistency checks - one per line: label: R expression that must be TRUE",
                         SIM_LOGIC, rows = 6, width = "100%"),
           uiOutput("rule_errors"))
    )
  ),

  nav_panel("Before / after",
    layout_columns(col_widths = c(7, 5),
      card(card_header("Distribution as received vs after cleaning"),
           selectInput("ba_var", NULL, choices = NULL),
           plotOutput("ba_plot", height = 420)),
      card(card_header("Changes to this variable"), DTOutput("ba_tbl"))
    )
  ),

  nav_panel("Outliers",
    layout_columns(col_widths = c(3, 9),
      card(card_header("Settings"),
           selectInput("out_var", "Variable", choices = NULL),
           selectInput("out_grp", "Within groups of", choices = NULL),
           radioButtons("out_method", "Method",
                        c("Tukey IQR fences" = "iqr", "Standardised z-score" = "z",
                          "Robust (median / MAD) z-score" = "mad")),
           numericInput("out_k", "Threshold k", 1.5, min = 0.5, step = 0.5),
           withMathJax(uiOutput("out_eq"))),
      card(card_header("Flagged values"),
           layout_columns(col_widths = c(6, 6),
             plotOutput("out_strip", height = 380),
             plotOutput("out_ecdf", height = 380)),
           DTOutput("out_tbl"))
    )
  ),

  nav_panel("Audit log",
    card(card_header("Every value changed by the cleaning pipeline"), DTOutput("log_tbl"))),

  nav_panel("Queries",
    card(card_header("Data queries for source verification"), DTOutput("q_tbl"))),

  nav_panel("Data",
    navset_card_tab(
      nav_panel("Cleaned", DTOutput("clean_tbl")),
      nav_panel("As received", DTOutput("raw_tbl"))
    )
  ),

  ONC_NAV,

  nav_panel("Methods",
    card(card_header("What each step does"), withMathJax(uiOutput("methods")))
  )
)

# -----------------------------------------------------------------------------
# 5. Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  raw_data <- reactive({
    if (input$source == "sim") {
      req(input$n, input$seed)
      simulate_trial(max(50, min(5000, input$n)), input$seed)
    } else {
      req(input$file)
      read.csv(input$file$datapath, colClasses = "character", na.strings = character(0),
               check.names = TRUE, stringsAsFactors = FALSE)
    }
  })

  codes_vec <- reactive({ x <- trimws(strsplit(input$codes %||% "", ",")[[1]]); x[nzchar(x)] })

  map_rv <- reactiveVal(NULL);    map_ver <- reactiveVal(0)
  ranges_rv <- reactiveVal(NULL); range_ver <- reactiveVal(0)

  observeEvent(raw_data(), {
    d <- raw_data()
    guess <- names(d)[grepl("(^|_)id$|subject|patient|participant", names(d), ignore.case = TRUE)][1]
    if (is.na(guess)) guess <- names(d)[1]
    updateSelectInput(session, "id_col", choices = c("(none)", names(d)), selected = guess)
    map_rv(default_map(d, guess, codes_vec())); map_ver(map_ver() + 1)
    if (input$source == "sim") {
      ranges_rv(SIM_RANGES)
      updateTextAreaInput(session, "units", value = SIM_UNITS)
      updateTextAreaInput(session, "logic", value = SIM_LOGIC)
    } else {
      updateTextAreaInput(session, "units", value = "")
      updateTextAreaInput(session, "logic", value = "")
      pre <- run_pipeline(d, list(id_col = guess, steps = c("trim", "missing", "types"),
                                  codes = input$codes))$data
      nv <- names(pre)[vapply(pre, is.numeric, logical(1)) & names(pre) != ".row"]
      ranges_rv(if (length(nv)) data.frame(
        variable = nv,
        min = vapply(nv, function(v) signif(min(pre[[v]], na.rm = TRUE), 4), numeric(1)),
        max = vapply(nv, function(v) signif(max(pre[[v]], na.rm = TRUE), 4), numeric(1)),
        action = "Query only", stringsAsFactors = FALSE, row.names = NULL) else NULL)
    }
    range_ver(range_ver() + 1)
  })

  observeEvent(input$map_reset, {
    map_rv(default_map(raw_data(), input$id_col, codes_vec())); map_ver(map_ver() + 1)
  })
  observeEvent(input$map_tbl_cell_edit, {
    map_rv(editData(map_rv(), input$map_tbl_cell_edit, rownames = FALSE))
  })
  observeEvent(input$range_tbl_cell_edit, {
    ranges_rv(editData(ranges_rv(), input$range_tbl_cell_edit, rownames = FALSE))
  })
  observeEvent(input$add_range, {
    v <- trimws(input$new_range_var); req(nzchar(v))
    r <- ranges_rv() %||% data.frame(variable = character(), min = numeric(), max = numeric(),
                                    action = character())
    ranges_rv(rbind(r, data.frame(variable = v, min = NA_real_, max = NA_real_,
                                  action = "Query only", stringsAsFactors = FALSE)))
    range_ver(range_ver() + 1)
    updateTextInput(session, "new_range_var", value = "")
  })

  cfg <- reactive(list(
    id_col = if (identical(input$id_col, "(none)")) NULL else input$id_col,
    id_pad = input$id_pad, steps = input$steps, codes = input$codes,
    map = map_rv(), ranges = ranges_rv(), units = input$units, logic = input$logic))

  res <- reactive({ req(raw_data()); run_pipeline(raw_data(), cfg()) })
  clean <- reactive(res()$data)
  vsum <- reactive(variable_summary(clean(), res()$types))

  observe({
    d <- clean()
    nv <- names(d)[vapply(d, is.numeric, logical(1)) & names(d) != ".row"]
    cv <- names(d)[vapply(d, function(z) is.character(z) && length(unique(z)) <= 30, logical(1))]
    sel <- function(id, ch, pre = NULL) updateSelectInput(session, id, choices = c(pre, ch),
                                                          selected = if (isTRUE(isolate(input[[id]]) %in% c(pre, ch))) isolate(input[[id]]) else c(pre, ch)[1])
    sel("ba_var", nv); sel("out_var", nv); sel("out_grp", setdiff(cv, cfg()$id_col), "(all records)")
    sel("cat_var", unique(map_rv()$variable))
  })

  # ---- raw data checks -----------------------------------------------------
  raw_iss <- reactive({
    req(raw_data())
    ri <- raw_screen(raw_data(), cfg()$id_col, codes_vec(), input$id_pad %||% 3, ranges_rv())
    if (isFALSE(input$blank_issue)) ri <- ri[ri$issue != "Blank cell", ]
    ri
  })
  raw_sum <- reactive({
    ri <- raw_iss()
    if (!nrow(ri)) return(data.frame(variable = character(), issue = character(), cells = integer(),
                                     records = integer(), examples = character()))
    s <- aggregate(row ~ variable + issue, ri, length); names(s)[3] <- "cells"
    s$records <- aggregate(row ~ variable + issue, ri, function(z) length(unique(z)))$row
    s$examples <- vapply(seq_len(nrow(s)), function(i) {
      v <- unique(ri$raw_value[ri$variable == s$variable[i] & ri$issue == s$issue[i]])
      v[v == ""] <- "(blank)"
      paste0(paste(head(v, 4), collapse = ", "), if (length(v) > 4) ", ..." else "")
    }, character(1))
    s[order(match(s$issue, ISSUE_ORDER), -s$cells), ]
  })

  output$rb_dim   <- renderText(sprintf("%s x %s", format(nrow(raw_data()), big.mark = ","), ncol(raw_data())))
  output$rb_cells <- renderText({
    ri <- raw_iss(); ri <- ri[ri$variable != "(whole record)", ]
    n <- nrow(unique(ri[c("row", "variable")]))
    sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / (nrow(raw_data()) * ncol(raw_data())))
  })
  output$rb_rows  <- renderText({
    n <- length(unique(raw_iss()$row)); sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / nrow(raw_data()))
  })
  output$rb_dups  <- renderText(sum(raw_iss()$issue == "Exact duplicate record"))
  output$rb_dupid <- renderText(length(unique(raw_iss()$raw_value[raw_iss()$issue == "Duplicate ID"])))

  output$raw_matrix <- renderPlot({
    s <- raw_sum(); validate(need(nrow(s) > 0, "No issues found in the raw data."))
    s$issue <- factor(s$issue, levels = ISSUE_ORDER)
    s$variable <- factor(s$variable, levels = rev(unique(c(names(raw_data()), "(whole record)"))))
    ggplot(s, aes(issue, variable)) +
      geom_point(aes(size = cells, colour = issue), alpha = .85) +
      geom_text(aes(label = cells), size = 3, colour = "#1F2933", vjust = -1.3) +
      scale_size_area(max_size = 11, guide = "none") +
      scale_colour_manual(values = setNames(hcl.colors(length(ISSUE_ORDER), "Dark 3"), ISSUE_ORDER),
                          guide = "none") +
      scale_x_discrete(drop = TRUE) +
      labs(x = NULL, y = NULL, subtitle = "Point size and label = number of cells") + theme_dash() +
      theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.grid.major = element_line(colour = "#EEF2F5"))
  })

  output$raw_issue_sum <- renderDT(datatable(raw_sum(), rownames = FALSE, selection = "single",
                                             class = "compact stripe", options = list(pageLength = 10, dom = "tip")))

  raw_detail <- reactive({
    ri <- raw_iss(); s <- raw_sum(); sel <- input$raw_issue_sum_rows_selected
    if (length(sel)) ri <- ri[ri$variable == s$variable[sel] & ri$issue == s$issue[sel], ]
    ri
  })
  output$raw_detail_head <- renderText({
    s <- raw_sum(); sel <- input$raw_issue_sum_rows_selected
    if (length(sel)) sprintf("Affected records: %s - %s", s$variable[sel], s$issue[sel])
    else "All raw data issues, cell by cell"
  })
  output$raw_issue_detail <- renderDT({
    d <- raw_detail(); d$raw_value[d$raw_value == ""] <- "(blank)"
    datatable(d, rownames = FALSE, class = "compact stripe",
              options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
  })

  output$raw_view <- renderDT({
    r <- raw_data(); ri <- raw_iss()
    tab <- datatable(cbind(row = seq_len(nrow(r)), r), rownames = FALSE,
                     class = "compact", options = dt_opts)
    for (v in intersect(unique(ri$variable), names(r))) {
      vals <- unique(r[[v]][unique(ri$row[ri$variable == v])])
      vals <- vals[!is.na(vals)]
      if (length(vals)) tab <- formatStyle(tab, v, backgroundColor = styleEqual(vals, rep("#FBE3E1", length(vals))))
    }
    dup <- ri$row[ri$issue == "Exact duplicate record"]
    if (length(dup)) tab <- formatStyle(tab, "row", backgroundColor = styleEqual(dup, rep("#FFE9C7", length(dup))))
    tab
  })

  # ---- overview ------------------------------------------------------------
  output$vb_raw     <- renderText(format(nrow(raw_data()), big.mark = ","))
  output$vb_clean   <- renderText(format(nrow(clean()), big.mark = ","))
  output$vb_changes <- renderText(format(nrow(res()$log), big.mark = ","))
  output$vb_queries <- renderText(format(nrow(res()$queries), big.mark = ","))
  output$vb_missing <- renderText({
    d <- clean()[setdiff(names(clean()), ".row")]
    sprintf("%.1f%%", 100 * mean(is.na(as.matrix(d))))
  })

  output$var_summary <- renderDT(datatable(vsum(), rownames = FALSE, options = dt_opts,
                                           class = "compact stripe"))

  output$step_plot <- renderPlot({
    lg <- res()$log; validate(need(nrow(lg) > 0, "No values changed."))
    s <- as.data.frame(table(step = lg$step)); s <- s[s$Freq > 0, ]
    ggplot(s, aes(Freq, reorder(step, -as.integer(substr(step, 1, 1))))) +
      geom_segment(aes(x = 0, xend = Freq, yend = reorder(step, -as.integer(substr(step, 1, 1)))),
                   colour = PAL["ok"], linewidth = 1) +
      geom_point(colour = PAL["clean"], size = 4) +
      geom_text(aes(label = Freq), hjust = -0.6, size = 3.8) +
      scale_x_continuous(expand = expansion(mult = c(0, .15))) +
      labs(x = "Values changed", y = NULL, title = "Audit log entries") + theme_dash()
  })

  output$query_plot <- renderPlot({
    q <- res()$queries; validate(need(nrow(q) > 0, "No open queries."))
    s <- as.data.frame(table(check = q$check)); s <- s[s$Freq > 0, ]
    ggplot(s, aes(Freq, reorder(check, Freq))) +
      geom_segment(aes(x = 0, xend = Freq, yend = reorder(check, Freq)), colour = PAL["ok"], linewidth = 1) +
      geom_point(colour = PAL["flag"], size = 4) +
      geom_text(aes(label = Freq), hjust = -0.6, size = 3.8) +
      scale_x_continuous(expand = expansion(mult = c(0, .15))) +
      labs(x = "Queries", y = NULL, title = "Queries by check") + theme_dash()
  })

  # ---- missingness ---------------------------------------------------------
  output$miss_dumbbell <- renderPlot({
    r <- raw_data(); d <- clean()
    vars <- intersect(names(r), names(d))
    m <- data.frame(variable = vars,
                    raw = vapply(vars, function(v) 100 * mean(is.na(r[[v]]) | trimws(r[[v]]) == ""), numeric(1)),
                    clean = vapply(vars, function(v) 100 * mean(is.na(d[[v]])), numeric(1)))
    m$variable <- factor(m$variable, levels = m$variable[order(m$clean)])
    ggplot(m) +
      geom_segment(aes(x = raw, xend = clean, y = variable, yend = variable), colour = "grey70", linewidth = 1.2) +
      geom_point(aes(raw, variable, colour = "As received"), size = 3.5) +
      geom_point(aes(clean, variable, colour = "After cleaning"), size = 3.5) +
      scale_colour_manual(NULL, values = c("As received" = unname(PAL["raw"]),
                                           "After cleaning" = unname(PAL["clean"]))) +
      labs(x = "Missing (%)", y = NULL) + theme_dash()
  })

  output$miss_pattern <- renderPlot({
    d <- clean()[setdiff(names(clean()), ".row")]
    if (nrow(d) > 1000) d <- d[sort(sample.int(nrow(d), 1000)), ]
    mm <- is.na(as.matrix(d)); validate(need(any(mm), "No missing values after cleaning."))
    ord <- order(rowSums(mm), decreasing = TRUE)
    long <- data.frame(record = rep(seq_len(nrow(mm)), ncol(mm)),
                       variable = factor(rep(colnames(mm), each = nrow(mm)),
                                         levels = colnames(mm)[order(colSums(mm), decreasing = TRUE)]),
                       missing = as.vector(mm[ord, , drop = FALSE]))
    ggplot(long, aes(variable, record, fill = missing)) + geom_raster() +
      scale_fill_manual(NULL, values = c(`FALSE` = "#E8EEF3", `TRUE` = unname(PAL["raw"])),
                        labels = c("Observed", "Missing")) +
      scale_y_reverse(expand = c(0, 0)) +
      labs(x = NULL, y = "Record (sorted)") + theme_dash() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
  })

  # ---- categories ----------------------------------------------------------
  output$map_tbl <- renderDT({
    map_ver()
    datatable(isolate(map_rv()), rownames = FALSE, class = "compact stripe",
              editable = list(target = "cell", disable = list(columns = 0:2)),
              options = list(pageLength = 12, dom = "tip"))
  })

  output$cat_plot <- renderPlot({
    m <- map_rv(); req(input$cat_var); m <- m[m$variable == input$cat_var, ]
    validate(need(nrow(m) > 0, "No levels."))
    m$level <- ifelse(is.na(m$clean_value) | m$clean_value == "", "(set to missing)", m$clean_value)
    m$raw_value <- factor(m$raw_value, levels = m$raw_value[order(m$level, m$n)])
    ggplot(m, aes(n, raw_value, colour = level)) +
      geom_segment(aes(x = 0, xend = n, yend = raw_value), linewidth = 1) +
      geom_point(size = 4) + geom_text(aes(label = n), hjust = -0.7, size = 3.6, colour = "#3d4852") +
      scale_colour_brewer(NULL, palette = "Set2") +
      scale_x_continuous(expand = expansion(mult = c(0, .12))) +
      labs(x = "Records", y = "Raw spelling") + theme_dash()
  })

  # ---- rules ---------------------------------------------------------------
  output$range_tbl <- renderDT({
    range_ver()
    datatable(isolate(ranges_rv()), rownames = FALSE, class = "compact stripe",
              editable = list(target = "cell", disable = list(columns = 0)),
              options = list(pageLength = 15, dom = "t"))
  })
  output$rule_errors <- renderUI({
    e <- res()$errors
    if (length(e)) div(class = "alert alert-warning small", HTML(paste(htmltools::htmlEscape(e), collapse = "<br>")))
  })

  # ---- before / after ------------------------------------------------------
  output$ba_plot <- renderPlot({
    v <- input$ba_var; req(v, v %in% names(raw_data()))
    r <- as_num(trimws(raw_data()[[v]])); cl <- clean()[[v]]
    df <- rbind(data.frame(version = "As received", value = r),
                data.frame(version = "After cleaning", value = cl))
    df <- df[!is.na(df$value), ]
    df$version <- factor(df$version, levels = c("As received", "After cleaning"))
    ggplot(df, aes(value, colour = version)) + stat_ecdf(linewidth = 1.1, pad = FALSE) +
      geom_rug(data = df[df$version == "As received", ], sides = "b", alpha = .4) +
      scale_colour_manual(NULL, values = c("As received" = unname(PAL["raw"]),
                                           "After cleaning" = unname(PAL["clean"]))) +
      labs(x = v, y = "Cumulative proportion",
           subtitle = "Empirical CDF; rug marks show values as received") + theme_dash()
  })
  output$ba_tbl <- renderDT({
    lg <- res()$log; lg <- lg[lg$variable == input$ba_var, c("row", "subject_id", "old_value", "new_value", "reason")]
    datatable(lg, rownames = FALSE, class = "compact stripe", options = list(pageLength = 12, dom = "tip"))
  })

  # ---- outliers ------------------------------------------------------------
  observeEvent(input$out_method, {
    updateNumericInput(session, "out_k", value = switch(input$out_method, iqr = 1.5, z = 3, mad = 3.5))
  })

  out_res <- reactive({
    v <- input$out_var; req(v, v %in% names(clean()))
    d <- clean(); x <- d[[v]]
    g <- if (is.null(input$out_grp) || input$out_grp == "(all records)" || !(input$out_grp %in% names(d)))
      rep("All records", nrow(d)) else ifelse(is.na(d[[input$out_grp]]), "(missing)", d[[input$out_grp]])
    flag <- rep(FALSE, length(x)); lo <- hi <- NA
    for (gg in unique(g)) {
      i <- which(g == gg); o <- outlier_flags(x[i], input$out_method, input$out_k)
      flag[i] <- o$flag; if (length(unique(g)) == 1) { lo <- o$lo; hi <- o$hi }
    }
    idc <- cfg()$id_col
    data.frame(row = d$.row, subject_id = if (!is.null(idc) && idc %in% names(d)) d[[idc]] else d$.row,
               group = g, value = x, flagged = flag)[!is.na(x), ] -> out
    list(df = out, lo = lo, hi = hi)
  })

  output$out_eq <- renderUI({
    k <- input$out_k
    eq <- switch(input$out_method,
      iqr = sprintf("$$x_i < Q_1 - %s\\,(Q_3 - Q_1)\\;\\text{or}\\;x_i > Q_3 + %s\\,(Q_3 - Q_1)$$", k, k),
      z   = sprintf("$$z_i = \\frac{x_i - \\bar{x}}{s},\\quad |z_i| > %s$$", k),
      mad = sprintf("$$M_i = \\frac{0.6745\\,(x_i - \\tilde{x})}{\\operatorname{median}_j|x_j - \\tilde{x}|},\\quad |M_i| > %s$$", k))
    withMathJax(div(class = "small", p("Value flagged when"), eq,
                    p(class = "text-muted", "Computed within each group when a grouping variable is chosen.")))
  })

  output$out_strip <- renderPlot({
    o <- out_res(); df <- o$df; validate(need(nrow(df) > 0, "No values."))
    set.seed(1)
    ggplot(df, aes(value, group, colour = flagged)) +
      geom_jitter(height = 0.18, width = 0, size = 2, alpha = .75) +
      { if (!is.na(o$lo)) geom_vline(xintercept = c(o$lo, o$hi), linetype = 2, colour = PAL["flag"]) } +
      scale_colour_manual(NULL, values = c(`FALSE` = unname(PAL["clean"]), `TRUE` = unname(PAL["flag"])),
                          labels = c("Within limits", "Flagged")) +
      labs(x = input$out_var, y = NULL) + theme_dash()
  })
  output$out_ecdf <- renderPlot({
    o <- out_res(); df <- o$df; validate(need(nrow(df) > 0, "No values."))
    ggplot(df, aes(value, colour = group)) + stat_ecdf(linewidth = 1, pad = FALSE) +
      geom_point(data = df[df$flagged, ], aes(value, 1.02), shape = 25, fill = PAL["flag"],
                 colour = PAL["flag"], size = 2.5, inherit.aes = FALSE) +
      { if (!is.na(o$lo)) geom_vline(xintercept = c(o$lo, o$hi), linetype = 2, colour = PAL["flag"]) } +
      scale_colour_brewer(NULL, palette = "Set1") +
      labs(x = input$out_var, y = "Cumulative proportion", subtitle = "Triangles mark flagged values") +
      theme_dash()
  })
  output$out_tbl <- renderDT({
    df <- out_res()$df; df <- df[df$flagged, c("row", "subject_id", "group", "value")]
    datatable(df, rownames = FALSE, class = "compact stripe", options = list(pageLength = 8, dom = "tip"))
  })

  # ---- log, queries, data --------------------------------------------------
  output$log_tbl <- renderDT(datatable(res()$log, rownames = FALSE,
                                       class = "compact stripe", options = dt_opts))
  output$q_tbl <- renderDT(datatable(res()$queries, rownames = FALSE,
                                     class = "compact stripe", options = dt_opts))
  output$clean_tbl <- renderDT({
    d <- clean(); d <- d[c(".row", setdiff(names(d), ".row"))]
    datatable(d, rownames = FALSE, class = "compact stripe", options = dt_opts)
  })
  output$raw_tbl <- renderDT(datatable(raw_data(), class = "compact stripe",
                                       options = dt_opts))

  # ---- methods -------------------------------------------------------------
  output$methods <- renderUI(withMathJax(tagList(
    h6("Raw data checks (tab 1)"),
    p("The data as received are screened cell by cell before any change is made. Every cell is",
      "classed against the issue types: blank cell, missing-value code, extra whitespace,",
      "spelling/coding variant (a raw spelling that differs from its harmonised level), decimal comma,",
      "non-numeric entry in a numeric field, non-ISO or invalid date, value outside a range rule,",
      "identifier not in PREFIX-number format, identifier shared by non-identical records, and exact",
      "duplicate record. The screen is the record of the raw data state; the cleaning steps below act on it."),
    h6("Cleaning steps"),
    tags$ol(
      tags$li(tags$b("Trim whitespace."), " Leading, trailing and repeated spaces removed from all text fields."),
      tags$li(tags$b("Standardise IDs."), " Identifiers upper-cased and rewritten as PREFIX-number with fixed zero-padding (e.g. ' dun12' becomes DUN-012)."),
      tags$li(tags$b("Missing-value codes."), " Blank cells and the listed codes become NA."),
      tags$li(tags$b("Harmonise categories."), " Each raw spelling is mapped to one level through the editable mapping table."),
      tags$li(tags$b("Convert types."), " A field is numeric when at least 90% of its non-missing entries are numbers (decimal commas accepted); text left in it becomes NA and raises a query. Date fields are parsed from ISO, day-first slash, dash and dot formats; impossible dates become NA and raise a query."),
      tags$li(tags$b("Unit corrections."), " Rules of the form variable ; condition ; transformation. In the simulated data: height recorded in metres \\((x<3)\\) is multiplied by 100; weight recorded in pounds is detected by \\(\\text{BMI}=x/(h/100)^2>50\\) and converted by \\(x/2.2046\\); HbA1c recorded in % (DCCT) is converted to mmol/mol (IFCC) by \\(10.929\\,(x-2.15)\\)."),
      tags$li(tags$b("Range checks."), " Values outside \\([\\min,\\max]\\) raise a query and, with action 'Set missing', are set to NA."),
      tags$li(tags$b("Duplicates."), " Records identical on every field after steps 1 to 7 are removed (first kept); identifiers shared by non-identical records raise a query."),
      tags$li(tags$b("Consistency checks."), " Each rule is an R expression evaluated row-wise; records where it is FALSE raise a query. Records where it cannot be evaluated (missing inputs) are not flagged.")
    ),
    h6("Outlier screening"),
    p("$$\\text{Tukey: } x_i \\notin [Q_1 - k\\,\\mathrm{IQR},\\; Q_3 + k\\,\\mathrm{IQR}],\\qquad
        \\text{z-score: } |x_i - \\bar{x}|/s > k,\\qquad
        \\text{robust: } 0.6745\\,|x_i - \\tilde{x}|/\\mathrm{MAD} > k$$"),
    p("Outliers are screened on the cleaned data and are listed for review; they are not changed automatically."),
    h6("CDISC oncology checks"),
    p("Raw (EDC), SDTM (SDTMIG v3.4) and ADaM (ADaMIG v1.3) checks for a RECIST 1.1 oncology trial, run from",
      "cdisc_oncology_checks.R. Target response: PD when \\(S_t \\ge 1.2\\,S_{\\text{nadir}}\\) and \\(S_t - S_{\\text{nadir}} \\ge 5\\) mm;",
      "PR when \\(S_t \\le 0.7\\,S_0\\); CR when non-nodal diameters are 0 and nodal short axes are below 10 mm; otherwise SD.",
      "Best overall response requires CR/PR confirmed at least the confirmation interval later and SD at least the minimum duration from first dose.",
      "Time to event: \\(\\text{AVAL} = \\text{ADT} - \\text{STARTDT} + 1\\); PFS events after a gap longer than the missed-assessment limit",
      "are censored at the last adequate assessment; TEAE when \\(\\text{TRTSDT} \\le \\text{ASTDT} \\le \\text{TRTEDT} + w\\).")
  )))

  if (HAS_ONC) {
  # ---- CDISC oncology: data ----------------------------------------------------
  onc_files_here <- function() {
    f <- c("ONC001_Raw_EDC.xlsx", "ONC001_SDTM.xlsx", "ONC001_ADaM.xlsx")
    for (d in file.path(APP_DIR, c(".", "data", "layers"))) if (all(file.exists(file.path(d, f)))) return(normalizePath(file.path(d, f)))
    NULL
  }
  onc_base <- reactive({
    validate(need(HAS_ONC, "cdisc_oncology_checks.R must be in the same folder as this app for the CDISC oncology pages."))
    f <- onc_files_here()
    if (!is.null(f)) list(st = read_onc_files(f), note = paste("Loaded from", paste(basename(f), collapse = ", ")))
    else list(st = simulate_onc_study(120, 2026), note = "ONC-001 files not found beside the app, so the identical simulated study (seed 2026) was generated.")
  })
  onc_data <- reactive({
    src <- input$onc_src %||% "files"
    if (src == "upload") {
      validate(need(input$onc_files, "Upload one or more .xlsx or .xpt files on the Study data page."))
      st <- read_onc_files(input$onc_files$datapath, input$onc_files$name)
      validate(need(length(st) > 0, "No uploaded sheet or file carries a recognised raw, SDTM or ADaM dataset name."))
      list(st = st, key = NULL, note = paste("Uploaded:", paste(input$onc_files$name, collapse = ", ")))
    } else {
      b <- onc_base()
      if (src == "files") list(st = b$st, key = NULL, note = b$note) else {
        sdd <- apply_seeds(b$st, ONC_PAR, expect = FALSE)
        list(st = sdd$study, key = sdd$key, note = paste(b$note, "One error per automated check was then seeded (see Logic checks)."))
      }
    }
  })
  onc_par <- debounce(reactive({
    p <- ONC_PAR
    dco <- suppressWarnings(as.Date(input$onc_dco %||% "", format = "%Y-%m-%d")); if (length(dco) && !is.na(dco)) p$dco <- dco; p$confirm_days <- input$onc_conf %||% p$confirm_days
    p$sd_min_days <- input$onc_sd %||% p$sd_min_days; p$missed_gap <- input$onc_gap %||% p$missed_gap
    p$teae_days <- input$onc_teae %||% p$teae_days; p$scan_window <- input$onc_win %||% p$scan_window
    p$baseline_days <- input$onc_blw %||% p$baseline_days
    p
  }), 800)
  onc_res <- reactive({
    d <- onc_data()
    withProgress(message = "Running logic checks", value = .5, run_onc_checks(d$st, onc_par()))
  })
  onc_subjects <- reactive({
    st <- onc_data()$st
    if ("DM" %in% names(st)) chr(st$DM$USUBJID) else if ("ADSL" %in% names(st)) chr(st$ADSL$USUBJID) else
      if ("RAW_DM" %in% names(st)) paste(ONC_PAR$study, chr(st$RAW_DM$SUBJID), sep = "-") else character()
  })
  # Seeded error -> finding key. Errors that create a foreign subject or a dataset-level
  # finding are matched on the target check alone.
  onc_keys <- function(d, r) {
    f <- r$findings; ids <- onc_subjects(); k <- d$key
    vapply(seq_len(nrow(k)), function(i) {
      own <- paste(k$Target_check[i], k$USUBJID[i])
      oth <- f$check_id == k$Target_check[i] & !(f$usubjid %in% ids)
      if (own %in% paste(f$check_id, f$usubjid) || !any(oth)) own else paste(f$check_id[oth], f$usubjid[oth])[1]
    }, "")
  }

  output$onc_src_note <- renderUI(p(class = "small text-muted", onc_data()$note))
  output$onc_inventory <- renderTable({
    st <- onc_data()$st
    data.frame(Layer = DS_DESC$Layer, Dataset = DS_DESC$Dataset,
               Records = vapply(DS_DESC$Dataset, function(d) if (d %in% names(st)) format(nrow(st[[d]]), big.mark = ",") else "not supplied", ""),
               check.names = FALSE)
  }, striped = TRUE, spacing = "xs", width = "100%")

  observe({
    st <- onc_data()$st; lay <- input$onc_layer %||% "Raw"
    ch <- DS_DESC$Dataset[DS_DESC$Layer == lay & DS_DESC$Dataset %in% names(st)]
    ch <- setNames(ch, paste(ch, "-", DS_DESC$Description[match(ch, DS_DESC$Dataset)])[seq_along(ch)])
    cur <- isolate(input$onc_ds)
    updateSelectInput(session, "onc_ds", choices = ch, selected = if (isTRUE(cur %in% ch)) cur else ch[1])
  })
  observe({
    u <- onc_subjects(); cur <- isolate(input$onc_subj)
    updateSelectInput(session, "onc_subj", choices = c("All subjects" = "", u), selected = if (isTRUE(cur %in% u)) cur else "")
  })
  observe({
    u <- onc_subjects(); st <- onc_data()$st
    cur <- isolate(input$onc_ju)
    updateSelectInput(session, "onc_ju", choices = u, selected = if (isTRUE(cur %in% u)) cur else featured_subject(st))
  })

  onc_cur <- reactive({
    st <- onc_data()$st; d <- input$onc_ds
    validate(need(isTRUE(d %in% names(st)) && DS_DESC$Layer[match(d, DS_DESC$Dataset)] == input$onc_layer,
                  "This layer has no datasets in the selected study data."))
    list(name = d, x = st[[d]])
  })
  output$onc_ds_head <- renderText({ d <- input$onc_ds %||% ""; paste(d, "-", DS_DESC$Description[match(d, DS_DESC$Dataset)]) })
  output$onc_ds_facts <- renderUI({
    c <- onc_cur(); dd <- DS_DESC[DS_DESC$Dataset == c$name, ]
    idv <- if (dd$Layer == "Raw") "SUBJID" else "USUBJID"
    div(class = "small text-muted d-flex flex-wrap gap-3 mb-2",
        span("Class: ", strong(dd$Class)), span("Structure: ", strong(dd$Structure)), span("Keys: ", strong(dd$Keys)),
        span(strong(format(nrow(c$x), big.mark = ",")), " records"),
        span(strong(length(unique(c$x[[idv]]))), " subjects"), span(strong(ncol(c$x)), " variables"))
  })
  lab_header <- function(d, cols) {
    labs <- VAR_META$Label[match(paste(d, cols), paste(VAR_META$Dataset, VAR_META$Variable))]; labs[is.na(labs)] <- ""
    htmltools::withTags(table(class = "display", thead(tr(lapply(seq_along(cols), function(j)
      th(HTML(sprintf('%s<br><span style="font-weight:400;font-size:11px;opacity:.7">%s</span>', cols[j], htmltools::htmlEscape(labs[j])))))))))
  }
  output$onc_ds_tbl <- renderDT({
    c <- onc_cur(); x <- c$x; s <- input$onc_subj
    if (isTRUE(nzchar(s))) {
      if (DS_DESC$Layer[DS_DESC$Dataset == c$name] == "Raw") x <- x[chr(x$SUBJID) == sid_of(s), , drop = FALSE] else x <- x[chr(x$USUBJID) == s, , drop = FALSE]
    }
    datatable(x, rownames = FALSE, container = lab_header(c$name, names(x)), class = "compact stripe",
              options = list(pageLength = 20, scrollX = TRUE, dom = "frtip"))
  })
  output$onc_ds_meta <- renderDT({
    c <- onc_cur()
    m <- VAR_META[VAR_META$Dataset == c$name, c("Variable", "Label", "Origin", "Derivation_or_comment")]
    extra <- setdiff(names(c$x), m$Variable)
    if (length(extra)) m <- rbind(m, data.frame(Variable = extra, Label = "", Origin = "Not in ONC-001 metadata", Derivation_or_comment = ""))
    m <- m[m$Variable %in% names(c$x), ]
    m$Missing <- vapply(m$Variable, function(v) sum(is.na(c$x[[v]]) | chr(c$x[[v]]) == ""), integer(1))
    datatable(m, rownames = FALSE, class = "compact stripe", options = list(pageLength = 30, dom = "t"))
  })

  # ---- CDISC oncology: logic checks ----------------------------------------------
  output$onc_msg <- renderUI({
    s <- onc_res()$status; nr <- s[grepl("^Not run", s$Run_status), ]
    div(class = "small text-muted mb-2", onc_data()$note,
        if (nrow(nr)) div(class = "alert alert-light small mt-2", strong("Checks not run: "),
                          paste(sprintf("%s (%s)", nr$Check_ID, sub("^Not run: ", "", nr$Run_status)), collapse = "; ")))
  })
  output$onc_vb_run  <- renderText(sum(grepl("^Run", onc_res()$status$Run_status)))
  output$onc_vb_fail <- renderText(sum(onc_res()$status$Run_status == "Run - findings"))
  output$onc_vb_nr   <- renderText(sum(!grepl("^Run", onc_res()$status$Run_status)))
  output$onc_vb_f    <- renderText(nrow(onc_res()$findings))
  output$onc_vb_c    <- renderText(sum(onc_res()$findings$severity == "Critical"))

  output$onc_plot <- renderPlot(height = function() 140 + 15 * max(1, length(unique(onc_res()$findings$check_id))), {
    f <- onc_res()$findings; validate(need(nrow(f) > 0, "No findings: every automated check passed on these datasets."))
    s <- aggregate(finding_id ~ check_id + level + severity, f, length); names(s)[4] <- "n"
    s$level <- factor(s$level, levels = c("Raw", "SDTM", "ADaM"))
    s$check_id <- factor(s$check_id, levels = rev(CHECK_SPEC$Check_ID[CHECK_SPEC$Check_ID %in% s$check_id]))
    ggplot(s, aes(n, check_id, colour = level)) +
      geom_segment(aes(x = 0, xend = n, yend = check_id), linewidth = .9) +
      geom_point(aes(shape = severity), size = 3.2) +
      scale_colour_manual(NULL, values = c(Raw = unname(PAL["raw"]), SDTM = unname(PAL["clean"]), ADaM = "#5B8C5A")) +
      scale_shape_manual(NULL, values = c(Critical = 16, Major = 17, Minor = 15)) +
      scale_x_continuous(breaks = function(l) unique(floor(pretty(l))), expand = expansion(mult = c(0, .08))) +
      labs(x = "Findings", y = NULL) + theme_dash() + theme(axis.text.y = element_text(size = 9))
  })

  observe({
    s <- onc_res()$status; s <- s[s$Check_ID %in% CHECK_SPEC$Check_ID[CHECK_SPEC$Automation == "Automated"], ]
    lab <- paste0(s$Check_ID, " - ", s$Check, " (", ifelse(is.na(s$Findings), "not run", paste(s$Findings, "findings")), ")")
    ch <- setNames(s$Check_ID, lab)
    first <- s$Check_ID[which(s$Findings > 0)[1]]; if (is.na(first)) first <- s$Check_ID[1]
    cur <- isolate(input$onc_check)
    updateSelectInput(session, "onc_check", choices = ch, selected = if (isTRUE(cur %in% ch)) cur else first)
  })
  output$onc_check_spec <- renderUI({
    req(input$onc_check); sp <- CHECK_SPEC[CHECK_SPEC$Check_ID == input$onc_check, ]
    st <- onc_res()$status; rs <- st$Run_status[st$Check_ID == input$onc_check]
    div(class = "small mb-2",
        div(strong("Dataset: "), sp$Dataset, " | ", strong("Category: "), sp$Category, " | ", strong("Severity: "), sp$Severity, " | ", strong("Status: "), rs),
        div(strong("Logic: "), code(sp$Logic)), div(strong("Reference: "), sp$Reference))
  })
  onc_check_rows <- reactive({ f <- onc_res()$findings; req(input$onc_check); f[f$check_id == input$onc_check, ] })
  output$onc_check_find <- renderDT({
    f <- onc_check_rows()
    datatable(f[, c("usubjid", "dataset", "record", "value", "message")], rownames = FALSE, selection = "single",
              class = "compact stripe", options = list(pageLength = 8, dom = "tip", scrollX = TRUE))
  })
  go_journey <- function(u) {
    if (isTRUE(u %in% onc_subjects())) { updateSelectInput(session, "onc_ju", selected = u); nav_select("nav", "onc_journey") }
  }
  observeEvent(input$onc_check_find_rows_selected, go_journey(onc_check_rows()$usubjid[input$onc_check_find_rows_selected]))
  observeEvent(input$onc_findings_rows_selected, go_journey(onc_res()$findings$usubjid[input$onc_findings_rows_selected]))

  output$onc_status <- renderDT(datatable(onc_res()$status, rownames = FALSE, class = "compact stripe",
                                          options = list(pageLength = 15, scrollX = TRUE, dom = "frtip")))
  output$onc_findings <- renderDT(datatable(onc_res()$findings, rownames = FALSE, selection = "single",
                                            class = "compact stripe", options = list(pageLength = 15, scrollX = TRUE, dom = "frtip")))
  output$onc_seeded <- renderDT({
    d <- onc_data(); validate(need(!is.null(d$key), "Seeded errors exist only for 'ONC-001 with seeded errors'."))
    k <- d$key; k$Detected <- ifelse(onc_keys(d, onc_res()) %in% paste(onc_res()$findings$check_id, onc_res()$findings$usubjid), "Yes", "No")
    datatable(k[, c("Seed_ID", "Level", "USUBJID", "Error_seeded", "Target_check", "Detected")], rownames = FALSE,
              class = "compact stripe", options = list(pageLength = 15))
  })
  output$onc_spec <- renderDT(datatable(CHECK_SPEC, rownames = FALSE, class = "compact stripe",
                                        options = list(pageLength = 15, scrollX = TRUE, dom = "frtip")))

  # ---- CDISC oncology: patient journey ---------------------------------------------
  ju_st <- reactive(onc_data()$st)
  ju_ref <- reactive({
    st <- ju_st(); u <- input$onc_ju; req(u)
    a <- if ("ADSL" %in% names(st)) st$ADSL[chr(st$ADSL$USUBJID) == u, ] else NULL
    dm <- if ("DM" %in% names(st)) st$DM[chr(st$DM$USUBJID) == u, ] else NULL
    ref <- if (!is.null(dm) && nrow(dm) && chr(dm$RFSTDTC) != "") dt(dm$RFSTDTC) else if (!is.null(a) && nrow(a)) dt(a$TRTSDT) else as.Date(NA)
    list(u = u, a = a, dm = dm, ref = ref)
  })
  output$onc_ju_facts <- renderUI({
    j <- ju_ref(); st <- ju_st(); a <- j$a; f <- onc_res()$findings
    bor <- if ("ADRS" %in% names(st)) chr(st$ADRS$AVALC[chr(st$ADRS$USUBJID) == j$u & chr(st$ADRS$PARAMCD) == "BOR"])[1] else NA
    tte <- function(pc) { if (!"ADTTE" %in% names(st)) return("-"); x <- st$ADTTE[chr(st$ADTTE$USUBJID) == j$u & chr(st$ADTTE$PARAMCD) == pc, ]
      if (!nrow(x)) return("-"); paste0(x$AVAL[1], " days (", if (num(x$CNSR[1]) == 0) chr(x$EVNTDESC[1]) else paste("censored:", chr(x$CNSDTDSC[1])), ")") }
    row <- function(k, v) tags$tr(tags$th(k, class = "fw-normal text-muted pe-3"), tags$td(v))
    tags$table(class = "table table-sm small",
      row("Arm (actual)", if (!is.null(a) && nrow(a)) chr(a$TRT01A) else chr(j$dm$ACTARM)),
      row("Age / sex", if (!is.null(a) && nrow(a)) paste(a$AGE, "years,", chr(a$SEX)) else ""),
      row("Randomised", if (!is.null(a) && nrow(a)) iso(dt(a$RANDDT)) else ""),
      row("First dose", iso(j$ref)),
      row("Best confirmed response", bor %||% "-"),
      row("PFS", tte("PFS")), row("OS", tte("OS")), row("DoR", tte("DOR")),
      row("Death", if (!is.null(a) && nrow(a) && !is.na(dt(a$DTHDT))) iso(dt(a$DTHDT)) else "Alive at cut-off"),
      row("Check findings", sum(f$usubjid == j$u)))
  })
  output$onc_ju_find <- renderDT({
    f <- onc_res()$findings; f <- f[f$usubjid == input$onc_ju, c("check_id", "level", "severity", "dataset", "record", "value", "message")]
    datatable(f, rownames = FALSE, class = "compact stripe",
              options = list(pageLength = 5, dom = "tip", scrollX = TRUE, language = list(emptyTable = "No logic-check findings for this subject.")))
  })
  output$onc_ju_tb <- renderPlot({
    j <- ju_ref(); st <- ju_st()
    validate(need("TR" %in% names(st), "TR not supplied: tumour burden needs the SDTM TR domain."))
    tr <- st$TR[chr(st$TR$USUBJID) == j$u & chr(st$TR$TRTESTCD) == "SUMDIAM", ]
    validate(need(nrow(tr) > 0, "No tumour measurements for this subject."))
    tr <- tr[order(num(tr$VISITNUM)), ]
    d <- data.frame(day = study_day(dt(tr$TRDTC), j$ref), v = num(tr$TRSTRESN), vn = num(tr$VISITNUM), vis = chr(tr$VISIT))
    if (all(is.na(d$day))) d$day <- as.numeric(dt(tr$TRDTC) - min(dt(tr$TRDTC)))
    ovr <- if ("RS" %in% names(st)) { r <- st$RS[chr(st$RS$USUBJID) == j$u & chr(st$RS$RSTESTCD) == "OVRLRESP", ]; chr(r$RSSTRESC)[match(d$vn, num(r$VISITNUM))] } else NA
    d$lab <- paste0(d$v, " mm", ifelse(d$vn == min(d$vn), " · baseline", ifelse(is.na(ovr), "", paste0(" · ", ovr))))
    base <- d$v[1]; nad <- cummin(d$v); pd <- c(NA, pmax(1.2 * head(nad, -1), head(nad, -1) + 5))
    ggplot(d, aes(day, v)) +
      geom_hline(yintercept = 0.7 * base, linetype = 2, colour = "#2B8C8C") +
      annotate("text", x = min(d$day), y = 0.7 * base, label = "PR: 70% of baseline", vjust = -0.5, hjust = 0, size = 3.3, colour = "#2B8C8C") +
      geom_line(data = data.frame(day = d$day, y = pd)[-1, ], aes(day, y), linetype = 2, colour = "#C0392B") +
      geom_point(data = data.frame(day = d$day, y = pd)[-1, ], aes(day, y), colour = "#C0392B", shape = 3, size = 2) +
      geom_line(colour = PAL["clean"], linewidth = 1.1) + geom_point(colour = PAL["clean"], size = 3, fill = "white", shape = 21, stroke = 1.5) +
      geom_text(aes(label = lab), vjust = -1.1, size = 3.4) +
      scale_x_continuous(expand = expansion(mult = .1)) +
      scale_y_continuous(limits = c(0, max(d$v, pd, na.rm = TRUE) * 1.2), expand = expansion(mult = c(0, .02))) +
      labs(x = "Study day (day 1 = first dose)", y = "Sum of diameters (mm)",
           subtitle = "Dashed red: PD threshold, 120% of nadir and at least +5 mm") + theme_dash()
  })
  output$onc_ju_tl <- renderPlot({
    j <- ju_ref(); st <- ju_st(); u <- j$u
    ex <- if ("EX" %in% names(st)) st$EX[chr(st$EX$USUBJID) == u, ] else NULL
    ae <- if ("ADAE" %in% names(st)) st$ADAE[chr(st$ADAE$USUBJID) == u, ] else NULL
    validate(need(!is.null(ex) || !is.null(ae), "EX and ADAE not supplied."))
    pts <- if (!is.null(ex) && nrow(ex)) data.frame(day = study_day(dt(ex$EXSTDTC), j$ref), y = "Dosing",
                                                  red = chr(ex$EXADJ) != "", dose = num(ex$EXDOSE)) else NULL
    seg <- NULL
    if (!is.null(ae) && nrow(ae)) {
      end <- dt(ae$AENDT); lst <- if (!is.null(j$a) && nrow(j$a)) dt(j$a$LSTALVDT) else ONC_PAR$dco
      end2 <- end; end2[is.na(end2)] <- lst
      seg <- data.frame(s = study_day(dt(ae$ASTDT), j$ref), e = study_day(end2, j$ref),
                        y = paste0(chr(ae$AEDECOD), " (", ae$AESEQ, ")"), g = factor(num(ae$AETOXGRN), levels = 1:5),
                        te = chr(ae$TRTEMFL) == "Y", ongoing = is.na(end), imp = chr(ae$ASTDTF) != "")
      seg$e <- pmax(seg$e, seg$s + 1)
    }
    validate(need(!is.null(pts) || !is.null(seg), "No dosing or adverse events for this subject."))
    lv <- c(rev(unique(seg$y)), "Dosing")
    p <- ggplot() + theme_dash() + labs(x = "Study day (day 1 = first dose)", y = NULL)
    if (!is.null(seg)) p <- p + geom_segment(data = seg, aes(x = s, xend = e, y = factor(y, lv), yend = factor(y, lv), colour = g, alpha = te),
                                            linewidth = 6, lineend = "butt") +
      geom_text(data = seg[seg$ongoing | seg$imp, ], aes(x = e, y = factor(y, lv), label = ifelse(ongoing, "ongoing", "start day imputed")),
                hjust = -0.1, size = 3) +
      scale_colour_manual("CTCAE grade", values = c(`1` = "#E8C547", `2` = "#E39B3A", `3` = "#D4652A", `4` = "#B23A2E", `5` = "#5E1A1A"), drop = FALSE) +
      scale_alpha_manual("Treatment-emergent", values = c(`TRUE` = 1, `FALSE` = .3), labels = c(`TRUE` = "Yes", `FALSE` = "No"))
    if (!is.null(pts)) p <- p + geom_point(data = pts, aes(day, factor(y, lv), shape = red), size = 3, colour = "#1F2933") +
      scale_shape_manual("Dose", values = c(`FALSE` = 15, `TRUE` = 0), labels = c(`FALSE` = "Full", `TRUE` = "Reduced"))
    if (!is.null(j$a) && nrow(j$a) && !is.na(dt(j$a$DTHDT)))
      p <- p + geom_vline(xintercept = study_day(dt(j$a$DTHDT), j$ref), colour = "#C0392B", linewidth = 1) +
        annotate("text", x = study_day(dt(j$a$DTHDT), j$ref), y = 0.6, label = "Death", hjust = 1.1, colour = "#C0392B", size = 3.4)
    p + theme(legend.position = "top", legend.box = "horizontal")
  })
  output$onc_ju_raw <- renderDT({
    j <- ju_ref(); st <- ju_st(); validate(need("RAW_TUMOR" %in% names(st), "RAW_TUMOR not supplied."))
    t <- st$RAW_TUMOR[chr(st$RAW_TUMOR$SUBJID) == sid_of(j$u), ]
    validate(need(nrow(t) > 0, "No tumour assessment form for this subject."))
    vis <- unique(chr(t$VISIT)); les <- unique(chr(t$LESIONID))
    out <- data.frame(Lesion = les, Type = chr(t$LESTYPE)[match(les, t$LESIONID)],
                      Organ = paste0(tolower(chr(t$ORGAN)[match(les, t$LESIONID)]), ifelse(chr(t$NODAL)[match(les, t$LESIONID)] == "Y", " (node)", "")))
    for (v in vis) out[[paste0(v, "\n", chr(t$SCANDT)[match(v, t$VISIT)])]] <- vapply(les, function(l) {
      r <- t[chr(t$VISIT) == v & chr(t$LESIONID) == l, ]; if (!nrow(r)) "" else if (!is.na(num(r$DIAM_MM[1]))) as.character(num(r$DIAM_MM[1])) else chr(r$NTSTATUS[1]) }, "")
    datatable(out, rownames = FALSE, class = "compact stripe", options = list(dom = "t", scrollX = TRUE, ordering = FALSE, pageLength = 20))
  })
  output$onc_ju_rs <- renderDT({
    j <- ju_ref(); st <- ju_st(); validate(need("RS" %in% names(st), "RS not supplied."))
    w <- rs_wide(st$RS[chr(st$RS$USUBJID) == j$u, ])
    r <- st$RS[chr(st$RS$USUBJID) == j$u & chr(st$RS$RSTESTCD) == "OVRLRESP", ]
    w$RSDTC <- chr(r$RSDTC)[match(w$VISITNUM, num(r$VISITNUM))]
    datatable(w[, c("VISIT", "RSDTC", "TRG", "NTRG", "NEWL", "OVR")], rownames = FALSE, class = "compact stripe",
              colnames = c("VISIT", "RSDTC", "TRGRESP", "NTRGRESP", "NEWLPROG", "OVRLRESP"),
              options = list(dom = "t", ordering = FALSE, pageLength = 20, language = list(emptyTable = "No response assessments.")))
  })
  output$onc_ju_adrs <- renderDT({
    j <- ju_ref(); st <- ju_st(); validate(need("ADRS" %in% names(st), "ADRS not supplied."))
    a <- st$ADRS[chr(st$ADRS$USUBJID) == j$u, c("PARAMCD", "AVISIT", "AVALC", "AVAL", "ADT", "ANL01FL")]
    formatStyle(datatable(a, rownames = FALSE, class = "compact stripe", options = list(dom = "t", ordering = FALSE, pageLength = 20)),"PARAMCD", target = "row", fontWeight = styleEqual("BOR", "bold"))
  })
  output$onc_ju_adtte <- renderTable({
    j <- ju_ref(); st <- ju_st(); validate(need("ADTTE" %in% names(st), "ADTTE not supplied."))
    a <- st$ADTTE[chr(st$ADTTE$USUBJID) == j$u, c("PARAMCD", "STARTDT", "ADT", "AVAL", "CNSR", "EVNTDESC", "CNSDTDSC")]
    a$STARTDT <- iso(dt(a$STARTDT)); a$ADT <- iso(dt(a$ADT)); a
  }, striped = TRUE, spacing = "xs", width = "100%", digits = 0)

  # ---- CDISC oncology: downloads -------------------------------------------------
  output$onc_dl_find <- downloadHandler(function() paste0("CDISC_Oncology_Check_Findings_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    function(f) { d <- onc_data(); r <- onc_res(); k <- d$key
      if (!is.null(k)) k$Expected_keys <- onc_keys(d, r)
      write_findings_xlsx(r, k, f) })
  output$onc_dl_spec <- downloadHandler("CDISC_Oncology_Check_Specification.xlsx", function(f) write_spec_xlsx(f))
  output$onc_dl_study <- downloadHandler(function() paste0("ONC001_Study_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    function(f) { d <- onc_data(); k <- d$key %||% data.frame(Seed_ID = character(), Level = character(), USUBJID = character(),
                  Error_seeded = character(), Target_check = character(), Checks_expected_to_flag = character(), Expected_keys = character())
      st <- d$st; for (s in setdiff(STUDY_SHEETS, names(st))) st[[s]] <- data.frame(note = "Dataset not supplied")
      write_study_xlsx(st, k, f) })
  output$onc_dl_layers <- downloadHandler("ONC001_layer_workbooks.zip", function(f) {
    d <- onc_data(); st <- d$st; tmp <- tempfile(); dir.create(tmp)
    for (s in setdiff(STUDY_SHEETS, names(st))) st[[s]] <- data.frame(note = "Dataset not supplied")
    files <- file.path(tmp, c("ONC001_Raw_EDC.xlsx", "ONC001_SDTM.xlsx", "ONC001_ADaM.xlsx"))
    write_layer_xlsx(st, "Raw", files[1]); write_layer_xlsx(st, "SDTM", files[2]); write_layer_xlsx(st, "ADaM", files[3])
    utils::zip(f, files, flags = "-j9q")
  })

  }

  # ---- downloads -----------------------------------------------------------
  stamp <- function(x) paste0(x, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
  output$dl_raw_issues <- downloadHandler(stamp("raw_data_issues"), function(f)
    write.csv(raw_iss(), f, row.names = FALSE, na = ""))
  output$dl_clean <- downloadHandler(stamp("cleaned_data"), function(f) {
    d <- clean(); write.csv(d[setdiff(names(d), ".row")], f, row.names = FALSE, na = "")
  })
  output$dl_log  <- downloadHandler(stamp("audit_log"), function(f) write.csv(res()$log, f, row.names = FALSE, na = ""))
  output$dl_q    <- downloadHandler(stamp("query_listing"), function(f) write.csv(res()$queries, f, row.names = FALSE, na = ""))
  output$dl_dict <- downloadHandler(stamp("variable_summary"), function(f) write.csv(vsum(), f, row.names = FALSE, na = ""))
}

app <- shinyApp(ui, server)
# Sourcing this file by hand (RStudio "Source") would only build the app object; open it instead.
# runApp() and RStudio's Run App load the file themselves and use the object below.
if (!is.null(SOURCED_FROM) && interactive()) runApp(app, launch.browser = TRUE) else app
