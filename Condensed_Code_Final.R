# =============================================================================
# THE MERITOCRACY MYTH: MASTER ANALYSIS SCRIPT
# =============================================================================
# Author: Alfredo Munoz, edited for this project by Pranav Bommareddy 
# Version: 1.1
# 
# This master script sources all analysis modules in the correct order.
# Run this script to regenerate all tables, figures, and statistics.
#
# STRUCTURE:
#   00_setup.R          - Environment, packages, utility functions
#   01_data_import.R    - Load and validate data files
#   02_transformations.R - Clean and transform data
#   03_primitives.R     - Analysis helper functions
#   04_core_analyses.R  - Run all analyses, store results
#   05_tables.R         - Generate Tables 1-7
#   06_figures.R        - Generate Figures 1-8
#   07_reporting.R      - Statistical reporting and export
#
# =============================================================================
# =============================================================================
# SECTION 0: ENVIRONMENT SETUP
# =============================================================================

rm(list = ls())
set.seed(12345)

# ─────────────────────────────────────────────────────────────────────────────
# 0.1 Load Packages
# ─────────────────────────────────────────────────────────────────────────────

required_packages <- c(
  
  # Data manipulation
  
  "tidyverse", "readr", "janitor", "lubridate", "stringr",
  # Visualization
  "scales", "paletteer", "patchwork", "RColorBrewer", 
  "ggalluvial", "ggrepel", "cowplot", "ggpattern", "ragg",
  # Document export
  "officer", "flextable"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# ─────────────────────────────────────────────────────────────────────────────
# 0.2 Directory Configuration
# ─────────────────────────────────────────────────────────────────────────────

BASE_DIR   <- "~/Downloads/dataverse_files 5"
CODE_DIR   <- file.path(BASE_DIR, "Code")
DATA_DIR   <- file.path(BASE_DIR, "Data")
OUTPUT_DIR <- file.path(BASE_DIR, "Outputs")


for (d in c(CODE_DIR, DATA_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

dir.exists(BASE_DIR)
list.files(BASE_DIR)

# ─────────────────────────────────────────────────────────────────────────────
# 0.3 Global Constants
# ─────────────────────────────────────────────────────────────────────────────

# CIP codes for orchestral performance degrees
ORCHESTRAL_CIP_CODES <- c("50.0903", "50.0907", "50.0911", "50.0914", "50.0915", "50.0916")

# Analysis time windows
IPEDS_START_YEAR <- 2011
IPEDS_END_YEAR   <- 2024
POOLED_WINDOW_START <- 2017
POOLED_WINDOW_END   <- 2024

# Top 8 orchestras (by budget/prestige)
TOP8_ORCHESTRAS <- c(
  "Boston Symphony Orchestra",
  "Chicago Symphony Orchestra",
  "The Cleveland Orchestra",
  "Los Angeles Philharmonic",
  "New York Philharmonic",
  "The Philadelphia Orchestra",
  "San Francisco Symphony",
  "National Symphony Orchestra"
)

# Professional benchmarks (League of American Orchestras, 2023)
LAO_2023_BENCHMARKS <- list(
  White    = 79.1,
  Hispanic = 4.8,
  Black    = 2.4,
  Asian    = 11.0,
  URM      = 7.5
)

# U.S. Population benchmarks (U.S. Census Bureau Vintage 2024 estimates)
US_POP_2024 <- list(
  White    = 57.5,
  Hispanic = 20.0,
  Black    = 13.7,
  Asian    = 6.7,
  URM      = 35.4
)

# ─────────────────────────────────────────────────────────────────────────────
# 0.4 JCE Formatting Standards
# ─────────────────────────────────────────────────────────────────────────────

# Page dimensions (mm)
JCE_WIDTH_DOUBLE    <- 174
JCE_WIDTH_SINGLE    <- 84
JCE_WIDTH_LANDSCAPE <- 270
JCE_MAX_HEIGHT      <- 234

mm_to_inches <- function(mm) mm / 25.4

# Color palette
JCE_PALETTE <- c(
  
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
  "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", 
  "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"
)

# ggplot2 theme
theme_jce <- theme_minimal(base_size = 11) +
  theme(
    text = element_text(family = "Helvetica"),
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

theme_set(theme_jce)

# ─────────────────────────────────────────────────────────────────────────────
# 0.5 Export Utility Functions
# ─────────────────────────────────────────────────────────────────────────────

#' Save table in JCE format (Word + CSV)
#' @param table_data Data frame to export
#' @param table_number Integer table number
#' @param table_title Character title
#' @param file_name Character base filename (no extension)
#' @param notes Character vector of table notes
#' @param digits Integer decimal places for numeric columns
save_jce_table <- function(table_data,
                           table_number,
                           table_title,
                           file_name,
                           notes = NULL,
                           digits = 2) {
  
  
  stopifnot(is.data.frame(table_data))
  
  # Save CSV
  csv_path <- file.path(OUTPUT_DIR, paste0(file_name, ".csv"))
  readr::write_csv(table_data, csv_path)
  
  # Prepare display table
  display_tbl <- table_data
  num_cols <- vapply(display_tbl, is.numeric, logical(1))
  if (any(num_cols)) {
    display_tbl[, num_cols] <- lapply(display_tbl[, num_cols, drop = FALSE], round, digits = digits)
  }
  
  # Create flextable
  ft <- flextable::flextable(display_tbl) %>%
    flextable::theme_booktabs() %>%
    flextable::fontsize(size = 10, part = "all") %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(align = "left", j = 1, part = "body") %>%
    flextable::line_spacing(space = 1, part = "all") %>%
    flextable::autofit() %>%
    flextable::set_table_properties(width = 1, layout = "autofit")
  
  # Build Word document
  docx_path <- file.path(OUTPUT_DIR, "Tables.docx")
  caption_text <- paste0("Table ", table_number, ". ", table_title)
  
  caption_fp <- officer::fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")
  notes_fp <- officer::fp_text(font.size = 10, font.family = "Times New Roman")
  
  doc <- if (file.exists(docx_path)) {
    officer::read_docx(docx_path)
  } else {
    officer::read_docx()
  }
  
  doc <- doc %>%
    officer::body_add_fpar(officer::fpar(officer::ftext(caption_text, prop = caption_fp))) %>%
    officer::body_add_par("", style = "Normal") %>%
    flextable::body_add_flextable(ft) %>%
    officer::body_add_par("", style = "Normal")
  
  if (!is.null(notes) && length(notes) > 0) {
    note_text <- paste(notes, collapse = " ")
    doc <- doc %>%
      officer::body_add_fpar(officer::fpar(officer::ftext(paste0("Notes: ", note_text), prop = notes_fp))) %>%
      officer::body_add_par("", style = "Normal")
  }
  
  print(doc, target = docx_path)
  cat(sprintf("✓ Table %d saved: %s\n", table_number, file_name))
  
  invisible(list(csv = csv_path, docx = docx_path))
}

#' Save figure in JCE format (EPS)
#' @param plot_obj ggplot object
#' @param filename_stub Character base filename (no extension)
#' @param width_mm Numeric width in millimeters
#' @param height_mm Numeric height in millimeters
save_jce_figure <- function(plot_obj,
                            filename_stub,
                            width_mm = JCE_WIDTH_DOUBLE,
                            height_mm = JCE_MAX_HEIGHT * 0.6) {
  
  stopifnot(inherits(plot_obj, "ggplot"))
  
  ggplot2::ggsave(
    filename = file.path(OUTPUT_DIR, paste0(filename_stub, ".eps")),
    plot = plot_obj,
    width = mm_to_inches(width_mm),
    height = mm_to_inches(height_mm),
    units = "in",
    device = "eps",
    bg = "white"
  )
  
  cat(sprintf("✓ Figure saved: %s.eps\n", filename_stub))
  invisible(file.path(OUTPUT_DIR, paste0(filename_stub, ".eps")))
}

cat("\n")
cat(strrep("═", 70), "\n")
cat("  ENVIRONMENT SETUP COMPLETE\n")
cat(strrep("═", 70), "\n")
cat(sprintf("  Output directory: %s\n", OUTPUT_DIR))
cat("\n")

# =============================================================================
# SECTION 1: DATA IMPORT & VALIDATION
# =============================================================================

cat("\n")
cat(strrep("═", 70), "\n")
cat("  SECTION 1: DATA IMPORT & VALIDATION\n")
cat(strrep("═", 70), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 1.1 Define File Paths
# ─────────────────────────────────────────────────────────────────────────────

DATA_FILES <- list(
  musicians  = file.path(DATA_DIR, "musician_data.csv"),
  orchestras = file.path(DATA_DIR, "orchestra_data.csv"),
  ipeds      = file.path(DATA_DIR, "ipeds_data.csv")
)

# Validate all files exist
missing_files <- names(DATA_FILES)[!file.exists(unlist(DATA_FILES))]
if (length(missing_files) > 0) {
  stop(sprintf("Missing data files: %s", paste(missing_files, collapse = ", ")))
}

# ─────────────────────────────────────────────────────────────────────────────
# 1.2 Load Raw Data
# ─────────────────────────────────────────────────────────────────────────────

cat("\nLoading data files...\n")

raw_musicians <- readr::read_csv(DATA_FILES$musicians, show_col_types = FALSE) %>%
  janitor::clean_names()

raw_orchestras <- readr::read_csv(DATA_FILES$orchestras, show_col_types = FALSE) %>%
  janitor::clean_names()

raw_ipeds <- readr::read_csv(DATA_FILES$ipeds, show_col_types = FALSE) %>%
  janitor::clean_names()

cat(sprintf("  ✓ Musicians:  %s rows\n", format(nrow(raw_musicians), big.mark = ",")))
cat(sprintf("  ✓ Orchestras: %s rows\n", format(nrow(raw_orchestras), big.mark = ",")))
cat(sprintf("  ✓ IPEDS:      %s rows\n", format(nrow(raw_ipeds), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# 1.3 Validate Data Structure
# ─────────────────────────────────────────────────────────────────────────────

# Check required columns exist
required_cols <- list(
  musicians = c("m_id", "orchestra"),
  orchestras = c("orchestra", "group"),
  ipeds = c("unitid", "instnm", "data_year", "ctotalt", "cip_code_clean")
)

for (dataset_name in names(required_cols)) {
  dataset <- get(paste0("raw_", dataset_name))
  missing_cols <- setdiff(required_cols[[dataset_name]], names(dataset))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in %s: %s", dataset_name, paste(missing_cols, collapse = ", ")))
  }
}

cat("\n  ✓ All required columns present\n")

########
# My code starts here#
## Section 2: Data Transformations

# Transform IPEDS Data

# Filter to orchestral performance CIP codes
music_degrees <- raw_ipeds |>
  filter(cip_code_clean %in% ORCHESTRAL_CIP_CODES) |>
  mutate(
    # Standardize types
    unitid    = as.character(unitid),
    data_year = as.integer(data_year),
    
    # Numeric counts
    ctotalt = as.numeric(ctotalt),
    ctotalw = as.numeric(ctotalw),
    chispt  = as.numeric(chispt),
    cbkaat  = as.numeric(cbkaat),
    caiant  = as.numeric(caiant),
    cnhpit  = as.numeric(cnhpit),
    casiat  = as.numeric(casiat),
    cwhitt  = as.numeric(cwhitt),
    c2mort  = as.numeric(c2mort),
    cnralt  = as.numeric(cnralt),
    awlevel = as.integer(awlevel),
    
    # Derive degree level from IPEDS award level codes
    # 3=Associate's, 5=Bachelor's, 7=Master's, 9/10/11/17/18/19=Doctoral
    # 1/2/4/6/8/20/21=Professional Studies/Certificates
    degree_level = case_when(
      awlevel == 3 ~ "Associate's",
      awlevel == 5 ~ "Bachelor's",
      awlevel == 7 ~ "Master's",
      awlevel %in% c(9, 10, 11, 17, 18, 19) ~ "Doctoral",
      awlevel %in% c(1, 2, 4, 6, 8, 20, 21) ~ "Professional Studies",
      TRUE ~ "Other"
    ),
    
    # Calculate new demographic counts
    ## urm are Hispanic, Black, American Indian, Native American, Native Hawaiian and Pacific Islander
    ## non-urm are White and Asian
    urm_count = chispt + cbkaat + caiant + cnhpit,
    non_urm_count = ctotalt - urm_count,
    
    # Calculate percentages (with zero-division protection)
    urm_pct      = if_else(ctotalt > 0, urm_count / ctotalt * 100, 0),
    hispanic_pct = if_else(ctotalt > 0, chispt / ctotalt * 100, 0),
    black_pct    = if_else(ctotalt > 0, cbkaat / ctotalt * 100, 0),
    asian_pct    = if_else(ctotalt > 0, casiat / ctotalt * 100, 0),
    white_pct    = if_else(ctotalt > 0, cwhitt / ctotalt * 100, 0),
    nra_pct      = if_else(ctotalt > 0, cnralt / ctotalt * 100, 0)
  )

# Create institution lookup table to refer back to 
ipeds_institutions <- music_degrees |>
  distinct(unitid, instnm) |>
  group_by(unitid) |>
  summarise(instnm = first(instnm), .groups = "drop")


### Transform LOA Orchestra Data

# create new variable for group_num
orchestra_directory = raw_orchestras |>
  mutate(
    orchestra = str_squish(orchestra),
    group = if ("group" %in% names(.)) str_squish(group) else NA_character_,
    group_num = suppressWarnings(as.integer(group))
  ) |>
  distinct(orchestra, group, group_num)

# PB # Identify and sort orchestras by group according to LOA budget guidelines
group1_orchestras <- orchestra_directory |>
  filter(!is.na(group_num) & group_num == 1) |>
  pull(orchestra) |>
  unique()

group2_orchestras <- orchestra_directory %>%
  filter(!is.na(group_num) & group_num == 2) %>%
  pull(orchestra) %>%
  unique()

group3_orchestras <- orchestra_directory %>%
  filter(!is.na(group_num) & group_num == 3) %>%
  pull(orchestra) %>%
  unique()

group123_orchestras <- orchestra_directory %>%
  filter(!is.na(group_num) & group_num %in% c(1, 2, 3)) %>%
  pull(orchestra) %>%
  unique()
print(group123_orchestras)

### Transform Musician Data

# Identify the unitid column name
unitid_col <- case_when(
  "ipeds_unitid" %in% names(raw_musicians) ~ "ipeds_unitid",
  "unitid" %in% names(raw_musicians) ~ "unitid",
  TRUE ~ NA_character_
)
## og munoz code
if (is.na(unitid_col)) {
  stop("Musician data must contain 'ipeds_unitid' or 'unitid' column")
}

#
musicians <- raw_musicians %>%
  mutate(
    m_id = as.character(m_id),
    orchestra = str_squish(orchestra),
    institution = if ("institution" %in% names(.)) str_squish(institution) else NA_character_,
    institution_type = if ("institution_type" %in% names(.)) str_squish(institution_type) else NA_character_,
    degree_year = if ("degree_year" %in% names(.)) as.integer(degree_year) else NA_integer_,
    
    # Standardize institution key
    ipeds_unitid = suppressWarnings(as.integer(.data[[unitid_col]])),
    institution_key = as.character(ipeds_unitid),
    
    # Flag international training
    is_international = (institution_type == "International")
  )

# Filter to musicians with valid IPEDS institution links
musicians_linked <- musicians %>%
  filter(!is.na(institution_key) & institution_key != "NA")

cat(sprintf("  ✓ Total musician-degree records: %s\n", format(nrow(musicians), big.mark = ",")))
cat(sprintf("  ✓ Records linked to IPEDS: %s\n", format(nrow(musicians_linked), big.mark = ",")))
cat(sprintf("  ✓ Unique musicians (linked): %s\n", 
            format(n_distinct(musicians_linked$m_id), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# 2.4 Calculate Summary Statistics
# ─────────────────────────────────────────────────────────────────────────────

cat("\nCalculating summary statistics...\n")

# Total orchestral degrees
TOTAL_DEGREES <- sum(music_degrees$ctotalt, na.rm = TRUE)

# Institution counts
N_INSTITUTIONS_ALL <- n_distinct(raw_ipeds$unitid[!is.na(raw_ipeds$unitid)])
N_INSTITUTIONS_ORCHESTRAL <- n_distinct(music_degrees$unitid)

# Musician counts
N_ORCHESTRAS <- n_distinct(orchestra_directory$orchestra)
N_MUSICIANS_ALL <- n_distinct(raw_musicians$m_id[!is.na(raw_musicians$m_id)])
N_MUSICIANS_LINKED <- n_distinct(musicians_linked$m_id)

# =============================================================================
# SECTION 3: ANALYSIS PRIMITIVES
# =============================================================================
# This section defines reusable functions for calculating metrics.
# These are "primitives" - building blocks used throughout the analysis.
# Original code from Munoz 
# =============================================================================

cat("\n")
cat(strrep("═", 70), "\n")
cat("  SECTION 3: ANALYSIS PRIMITIVES\n")
cat(strrep("═", 70), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 3.1 Concentration Metrics
# ─────────────────────────────────────────────────────────────────────────────

#' Calculate placement counts and shares by institution
#' @param edu_df Musician education data frame
#' @param orchestra_filter Optional vector of orchestra names to filter
#' @return Data frame with institution_key, n_musicians, share, cum_share, rank
calc_placement_counts <- function(edu_df, orchestra_filter = NULL) {
  
  x <- edu_df
  if (!is.null(orchestra_filter)) {
    x <- x %>% filter(orchestra %in% orchestra_filter)
  }
  
  x %>%
    filter(!is.na(m_id), !is.na(institution_key), institution_key != "NA") %>%
    distinct(m_id, institution_key) %>%
    count(institution_key, name = "n_musicians", sort = TRUE) %>%
    mutate(
      total_known = sum(n_musicians),
      share = n_musicians / total_known,
      cum_share = cumsum(share),
      rank = row_number()
    )
}

#' Get K-coverage set (minimum schools to cover X% of placements)
#' @param counts_df Output from calc_placement_counts()
#' @param target Coverage target (0-1)
#' @return Character vector of institution_keys
get_k_coverage_set <- function(counts_df, target = 0.50) {
  idx <- which(counts_df$cum_share >= target)[1]
  if (is.na(idx)) idx <- nrow(counts_df)
  counts_df %>% 
    slice_head(n = idx) %>% 
    pull(institution_key) %>% 
    unique()
}

#' Calculate coverage of a specific set of institutions
#' @param counts_df Output from calc_placement_counts()
#' @param institution_keys Character vector of institution_keys
#' @return Numeric coverage (0-1)
calc_set_coverage <- function(counts_df, institution_keys) {
  if (length(institution_keys) == 0) return(0)
  counts_df %>%
    filter(institution_key %in% institution_keys) %>%
    summarise(coverage = sum(share, na.rm = TRUE)) %>%
    pull(coverage)
}

#' Calculate Gini coefficient from Lorenz curve data
#' @param shares Numeric vector of shares (should sum to 1)
#' @return Numeric Gini coefficient (0-1)
calc_gini <- function(shares) {
  shares_sorted <- sort(shares)
  n <- length(shares_sorted)
  cum_shares <- cumsum(shares_sorted)
  
  # Add origin point
  x <- c(0, seq_len(n) / n)
  y <- c(0, cum_shares)
  
  # Calculate area under Lorenz curve using trapezoidal rule
  area_under <- sum((y[-1] + y[-length(y)]) * diff(x) / 2)
  
  # Gini = (area under line of equality - area under Lorenz) / area under line of equality
  (0.5 - area_under) / 0.5
}

#' Calculate Herfindahl-Hirschman Index
#' @param shares Numeric vector of shares (should sum to 1)
#' @return Numeric HHI (0-10000 scale)
calc_hhi <- function(shares) {
  sum(shares^2) * 10000
}

# ─────────────────────────────────────────────────────────────────────────────
# 3.2 Demographic Metrics
# ─────────────────────────────────────────────────────────────────────────────

#' Calculate demographic statistics for a filtered IPEDS dataset
#' @param df Filtered music_degrees data frame
#' @return Named list with Total, URM, NRA counts and percentages
calc_demographic_stats <- function(df) {
  df %>%
    summarise(
      Total_Degrees = sum(ctotalt, na.rm = TRUE),
      URM_Degrees   = sum(urm_count, na.rm = TRUE),
      NRA_Degrees   = sum(cnralt, na.rm = TRUE),
      White_Degrees = sum(cwhitt, na.rm = TRUE),
      Hispanic_Degrees = sum(chispt, na.rm = TRUE),
      Black_Degrees = sum(cbkaat, na.rm = TRUE),
      Asian_Degrees = sum(casiat, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      URM_Pct = if_else(Total_Degrees > 0, URM_Degrees / Total_Degrees * 100, NA_real_),
      NRA_Pct = if_else(Total_Degrees > 0, NRA_Degrees / Total_Degrees * 100, NA_real_),
      White_Pct = if_else(Total_Degrees > 0, White_Degrees / Total_Degrees * 100, NA_real_),
      Hispanic_Pct = if_else(Total_Degrees > 0, Hispanic_Degrees / Total_Degrees * 100, NA_real_),
      Black_Pct = if_else(Total_Degrees > 0, Black_Degrees / Total_Degrees * 100, NA_real_),
      Asian_Pct = if_else(Total_Degrees > 0, Asian_Degrees / Total_Degrees * 100, NA_real_)
    )
}

#' Calculate Wilson score confidence interval for a proportion
#' @param x Number of successes
#' @param n Sample size
#' @param z Z-score for confidence level (default 1.96 for 95%)
#' @return Named vector with lower and upper bounds (as percentages)
calc_wilson_ci <- function(x, n, z = 1.96) {
  p <- x / n
  denom <- 1 + (z^2 / n)
  center <- (p + (z^2 / (2 * n))) / denom
  half <- (z / denom) * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2)))
  c(lower = (center - half) * 100, upper = (center + half) * 100)
}

# ─────────────────────────────────────────────────────────────────────────────
# 3.3 Statistical Tests
# ─────────────────────────────────────────────────────────────────────────────

#' Perform chi-square analysis on 2x2 contingency table
#' @param table_2x2 Matrix with rows = groups, cols = outcomes
#' @return List with test results, risk ratio, confidence intervals
analyze_contingency <- function(table_2x2) {
  
  N <- sum(table_2x2)
  
  # Chi-square test (without Yates correction)
  chi_test <- suppressWarnings(chisq.test(table_2x2, correct = FALSE))
  
  # Phi coefficient
  phi <- sqrt(chi_test$statistic / N)
  
  # Minimum expected count
  min_expected <- min(chi_test$expected)
  
  # Risk ratio and CI
  risk_row1 <- table_2x2[1, 1] / sum(table_2x2[1, ])
  risk_row2 <- table_2x2[2, 1] / sum(table_2x2[2, ])
  risk_ratio <- risk_row1 / risk_row2
  
  # Standard error of log(RR)
  se_log_rr <- sqrt(
    (1 / table_2x2[1, 1]) - (1 / sum(table_2x2[1, ])) +
      (1 / table_2x2[2, 1]) - (1 / sum(table_2x2[2, ]))
  )
  
  ci_low <- exp(log(risk_ratio) - 1.96 * se_log_rr)
  ci_high <- exp(log(risk_ratio) + 1.96 * se_log_rr)
  
  list(
    chi_sq = unname(chi_test$statistic),
    df = chi_test$parameter,
    p_value = chi_test$p.value,
    phi = unname(phi),
    min_expected = min_expected,
    risk_ratio = risk_ratio,
    rr_ci_low = ci_low,
    rr_ci_high = ci_high,
    N = N
  )
}

#' Bootstrap BCa confidence interval for risk ratio
#' @param table_2x2 2x2 contingency table matrix
#' @param n_boot Number of bootstrap iterations
#' @param alpha Significance level
#' @return List with observed RR, percentile CI, BCa CI, and parameters
bootstrap_risk_ratio_bca <- function(table_2x2, n_boot = 10000, alpha = 0.05) {
  
  a <- table_2x2[1, 1]  # Row 1, Col 1
  b <- table_2x2[1, 2]  # Row 1, Col 2
  c <- table_2x2[2, 1]  # Row 2, Col 1
  d <- table_2x2[2, 2]  # Row 2, Col 2
  
  n_row1 <- a + b
  n_row2 <- c + d
  
  p_row1 <- a / n_row1
  p_row2 <- c / n_row2
  rr_obs <- p_row1 / p_row2
  
  # Generate bootstrap distribution
  rr_boot <- numeric(n_boot)
  for (i in seq_len(n_boot)) {
    boot_a <- rbinom(1, n_row1, p_row1)
    boot_c <- rbinom(1, n_row2, p_row2)
    
    p1_boot <- boot_a / n_row1
    p2_boot <- boot_c / n_row2
    
    rr_boot[i] <- if (p2_boot > 0) p1_boot / p2_boot else NA
  }
  
  rr_boot <- rr_boot[!is.na(rr_boot)]
  B <- length(rr_boot)
  
  # Bias correction (z0)
  z0 <- qnorm(mean(rr_boot < rr_obs))
  
  # Acceleration via jackknife approximation
  n_jack <- min(1000, n_row1 + n_row2)
  jack_rr <- numeric(n_jack)
  
  for (j in seq_len(n_jack)) {
    if (runif(1) < p_row1) {
      ja <- a - 1; jn1 <- n_row1 - 1
    } else {
      ja <- a; jn1 <- n_row1 - 1
    }
    if (runif(1) < p_row2) {
      jc <- c - 1; jn2 <- n_row2 - 1
    } else {
      jc <- c; jn2 <- n_row2 - 1
    }
    
    jp1 <- ja / jn1
    jp2 <- jc / jn2
    jack_rr[j] <- if (jp2 > 0 && jp1 >= 0) jp1 / jp2 else NA
  }
  
  jack_rr <- jack_rr[!is.na(jack_rr)]
  jack_mean <- mean(jack_rr)
  jack_diff <- jack_mean - jack_rr
  
  num <- sum(jack_diff^3)
  denom <- 6 * (sum(jack_diff^2))^1.5
  acc <- if (denom != 0) num / denom else 0
  
  # BCa adjusted percentiles
  z_lo <- qnorm(alpha / 2)
  z_hi <- qnorm(1 - alpha / 2)
  
  alpha1 <- pnorm(z0 + (z0 + z_lo) / (1 - acc * (z0 + z_lo)))
  alpha2 <- pnorm(z0 + (z0 + z_hi) / (1 - acc * (z0 + z_hi)))
  
  alpha1 <- max(0.0001, min(0.9999, alpha1))
  alpha2 <- max(0.0001, min(0.9999, alpha2))
  
  rr_sorted <- sort(rr_boot)
  
  list(
    observed_rr = rr_obs,
    ci_percentile = quantile(rr_boot, c(alpha / 2, 1 - alpha / 2)),
    ci_bca = c(lower = unname(quantile(rr_sorted, alpha1)),
               upper = unname(quantile(rr_sorted, alpha2))),
    z0 = z0,
    acceleration = acc,
    n_boot = B
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# 3.4 Attrition Metrics
# ─────────────────────────────────────────────────────────────────────────────

#' Calculate pipeline attrition rate
#' @param pipeline_pct Percentage in pipeline (graduates)
#' @param profession_pct Percentage in profession
#' @return Attrition rate as percentage
calc_attrition <- function(pipeline_pct, profession_pct) {
  (pipeline_pct - profession_pct) / pipeline_pct * 100
}

# ─────────────────────────────────────────────────────────────────────────────
# 3.5 International Training Metrics
# ─────────────────────────────────────────────────────────────────────────────

#' Map country names to geographic regions
#' @param country Character country name
#' @return Character region name or NA
map_country_to_region <- function(country) {
  ctry <- str_squish(as.character(country))
  ctry <- na_if(ctry, "")
  
  europe <- c("Romania", "Moldova", "Bulgaria", "Russia", "Belgium", "Lithuania",
              "Netherlands", "United Kingdom", "Ukraine", "Poland", "Germany",
              "Portugal", "France", "Italy", "Switzerland", "Czech Republic",
              "Iceland", "Finland", "Sweden", "Armenia", "Denmark", "Greece",
              "Norway", "Albania", "Hungary", "Austria", "Serbia", "Ireland",
              "Spain", "Scotland")
  
  asia <- c("South Korea", "Japan", "China", "Hong Kong (China)", "Taiwan",
            "Singapore", "Turkey", "Uzbekistan", "Israel")
  
  north_america_non_us <- c("Canada", "Mexico")
  
  latin_america <- c("Colombia", "Cuba", "Brazil", "Puerto Rico (USA territory)",
                     "Jamaica", "Trinidad and Tobago", "Costa Rica", "Chile", "Venezuela")
  
  oceania <- c("Australia", "New Zealand")
  africa <- c("South Africa")
  
  case_when(
    ctry %in% europe ~ "Europe",
    ctry %in% asia ~ "Asia",
    ctry %in% north_america_non_us ~ "North America (non-U.S.)",
    ctry %in% latin_america ~ "Latin America/Caribbean",
    ctry %in% oceania ~ "Oceania",
    ctry %in% africa ~ "Sub-Saharan Africa",
    TRUE ~ NA_character_
  )
}

######
## This core analyses contains, mostly, my own code, but is based in the Munoz code.

#Calculate Placement Concentrations by Tier


## use the primitive function to filter musician presence by orchestra type to arrange by total counts of musicians
## shows how many musicians from each school is a member of an orchestra in the respective group
counts_all = calc_placement_counts(musicians_linked, orchestra_filter = NULL)
counts_g1 = calc_placement_counts(musicians_linked, orchestra_filter = group1_orchestras)
counts_top8 = calc_placement_counts(musicians_linked, orchestra_filter = TOP8_ORCHESTRAS)
counts_g2 = calc_placement_counts(musicians_linked, orchestra_filter = group2_orchestras)
counts_g3 = calc_placement_counts(musicians_linked, orchestra_filter = group3_orchestras)
counts_g123 = calc_placement_counts(musicians_linked, orchestra_filter = group123_orchestras)



##Empirically Identify Elite Feeder Institutions

# Get K50 sets for each tier
k50_all   <- get_k_coverage_set(counts_all, 0.50)
k50_g1    <- get_k_coverage_set(counts_g1, 0.50)
k50_top8  <- get_k_coverage_set(counts_top8, 0.50)
k50_g2 = get_k_coverage_set(counts_g2, 0.50)
k50_g3 = get_k_coverage_set(counts_g3, 0.50)
k75_g1 = get_k_coverage_set(counts_g1, 0.75)
k50_g123 = get_k_coverage_set(counts_g123, 0.50)
print(k50_g123)


# Find institutions present in at least 2 K50 sets (cross-tier consistency)
## my code loosens the restrictions to consider LOA groups 2 and 3 which are also considered "large budget orchestras" according to LOA
all_k50_institutions <- unique(c(k50_all, k50_g1, k50_g2, k50_g3, k50_top8))
ipeds_unitids <- unique(music_degrees$unitid)

PB_presence2 <- tibble(institution_key = all_k50_institutions) %>%
  mutate(
    in_all = institution_key %in% k50_all,
    in_g3 = institution_key %in% k50_g3,
    in_g2 = institution_key %in% k50_g2,
    in_g1 = institution_key %in% k50_g1,
    in_top8 = institution_key %in% k50_top8,
    tiers_present = in_all + in_g3 + in_g2 + in_g1 + in_top8 
  )

PB_candidate_keys = PB_presence2 |>
  filter(tiers_present >= 2, institution_key %in% ipeds_unitids) |>
  pull(institution_key)

# Greedy algorithm to find minimal set achieving ≥50% coverage in groups 1 through 3 and LOA Top 8
## algorithm keeps adding a school until it reaches >=50% coverage in each group.
## arranged by total contribution across all groups
PB_candidate_contrib <- tibble(institution_key = PB_candidate_keys) |>
  left_join(ipeds_institutions, by = c("institution_key" = "unitid")) |>
  mutate(top8_share = map_dbl(institution_key, ~calc_set_coverage(counts_top8, .x)),
         g1_share   = map_dbl(institution_key, ~calc_set_coverage(counts_g1, .x)),
         g3_share = map_dbl(institution_key, ~calc_set_coverage(counts_g3, .x)),
         g2_share = map_dbl(institution_key, ~calc_set_coverage(counts_g2, .x)),
         total_contrib = top8_share + g1_share + g2_share + g3_share) |>
  arrange(desc(total_contrib))

# Greedy selection
PB_selected <- character(0)
for (i in seq_len(nrow(PB_candidate_contrib))) {
  PB_selected <- c(PB_selected, PB_candidate_contrib$institution_key[i])
  if (calc_set_coverage(counts_top8, selected) >= 0.50 &&
      calc_set_coverage(counts_g1, selected) >= 0.50 &&
      calc_set_coverage(counts_g2, selected) >= 0.50 &&
      calc_set_coverage(counts_g3, selected) >= 0.50) {
    break
  }
}

PB_ELITE_FEEDER_UNITIDS <- PB_selected



## Build elite feeder lookup table by:
#initializing a tibble containing only the unitids elite feeders
#then merge unitids with ipeds_institution data
#arrange elite feeder by rank, LOA group 1 is the primary sort

PB_elite_feeders <- tibble(unitid = PB_ELITE_FEEDER_UNITIDS) |>
  left_join(ipeds_institutions, by = "unitid") |>
  left_join(counts_all %>% select(institution_key, rank_all = rank), 
            by = c("unitid" = "institution_key")) |>
  left_join(counts_g1 %>% select(institution_key, rank_g1 = rank),
            by = c("unitid" = "institution_key"))|>
  left_join(counts_top8 %>% select(institution_key, rank_top8 = rank),
            by = c("unitid" = "institution_key")) |>
  left_join(counts_g2 %>% select(institution_key, rank_g2 = rank),
            by = c("unitid" = "institution_key")) |>
  left_join(counts_g3 %>% select(institution_key, rank_g3 = rank),
            by = c("unitid" = "institution_key")) |>
  arrange(rank_g1, rank_g2, rank_g3, rank_top8, rank_all)

# Calculate and store k-coverage metrics
COVERAGE_ALL   <- calc_set_coverage(counts_all, ELITE_FEEDER_UNITIDS)
COVERAGE_G1    <- calc_set_coverage(counts_g1, ELITE_FEEDER_UNITIDS)
COVERAGE_TOP8  <- calc_set_coverage(counts_top8, ELITE_FEEDER_UNITIDS)

for (i in seq_len(nrow(elite_feeders))) {
  cat(sprintf("  %d. %s\n", i, elite_feeders$instnm[i]))
}


## Calculate K-Coverage Statistics

k_coverage_stats <- tibble(
  k_threshold = c(0.25, 0.50, 0.75)
) |>
  mutate(
    k_set = map(k_threshold, ~get_k_coverage_set(counts_all, .x)),
    n_schools = map_int(k_set, length),
    coverage_all = map_dbl(k_set, ~calc_set_coverage(counts_all, .x) * 100),
    coverage_top8 = map_dbl(k_set, ~calc_set_coverage(counts_top8, .x) * 100),
    coverage_123 = map_dbl(k_set, ~calc_set_coverage(counts_g123, .x) * 100)
  ) |>
  select(-k_set)

# ─────────────────────────────────────────────────────────────────────────────
# Calculate Concentration Metrics (Muñoz og code)
# ─────────────────────────────────────────────────────────────────────────────

cat("\nCalculating concentration metrics...\n")

GINI_COEFFICIENT <- calc_gini(counts_all$share)

HHI_ALL   <- calc_hhi(counts_all$share)
HHI_G1    <- calc_hhi(counts_g1$share)
HHI_TOP8  <- calc_hhi(counts_top8$share)
HHI_FOLD_INCREASE <- HHI_TOP8 / HHI_ALL

# Gradient ratios
GRADIENT_RATIO_ALL   <- 1.00
GRADIENT_RATIO_G1    <- (COVERAGE_G1 * 100) / (COVERAGE_ALL * 100)
GRADIENT_RATIO_TOP8  <- (COVERAGE_TOP8 * 100) / (COVERAGE_ALL * 100)

cat(sprintf("  ✓ Gini coefficient: %.3f\n", GINI_COEFFICIENT))
cat(sprintf("  ✓ HHI: All=%.0f, G1=%.0f, Top8=%.0f (%.1f-fold)\n", 
            HHI_ALL, HHI_G1, HHI_TOP8, HHI_FOLD_INCREASE))
cat(sprintf("  ✓ Gradient ratios: All=%.2f, G1=%.2f, Top8=%.2f\n",
            GRADIENT_RATIO_ALL, GRADIENT_RATIO_G1, GRADIENT_RATIO_TOP8))


## Calculate Elite vs Non-Elite Demographics

# filtering elite
stats_elite <- music_degrees %>%
  filter(unitid %in% PB_ELITE_FEEDER_UNITIDS) %>%
  calc_demographic_stats()

# filtering non-elite
stats_nonelite <- music_degrees %>%
  filter(!unitid %in% PB_ELITE_FEEDER_UNITIDS) %>%
  calc_demographic_stats()

URM_GAP_PP <- stats_nonelite$URM_Pct - stats_elite$URM_Pct

ELITE_DEGREE_SHARE <- stats_elite$Total_Degrees / TOTAL_DEGREES * 100

NRA_ALL_PCT <- music_degrees %>%
  filter(data_year >= 2011, data_year <= 2024) %>%
  summarise(
    total_deg = sum(ctotalt, na.rm = TRUE),
    nra_deg   = sum(cnralt, na.rm = TRUE),
    pct       = 100 * nra_deg / total_deg
  ) %>%
  pull(pct)

# ─────────────────────────────────────────────────────────────────────────────
# Build Contingency Table and Statistical Tests (Muñoz og code)
# ─────────────────────────────────────────────────────────────────────────────

cat("\nPerforming contingency analysis...\n")

# Build 2x2 table: Elite/Non-Elite × URM/Non-URM
contingency_2x2 <- matrix(
  c(stats_elite$URM_Degrees, 
    stats_elite$Total_Degrees - stats_elite$URM_Degrees,
    stats_nonelite$URM_Degrees, 
    stats_nonelite$Total_Degrees - stats_nonelite$URM_Degrees),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("Elite Feeders", "Non-Feeders"), c("URM", "Non-URM"))
)

# Run analyses
contingency_results <- analyze_contingency(contingency_2x2)
bootstrap_results <- bootstrap_risk_ratio_bca(contingency_2x2, n_boot = 10000)



## Calculate Demographic Trends Over Time

# Annual trends: changes in racial demographics using summarise
annual_trends <- music_degrees %>%
  group_by(data_year) %>%
  summarise(
    Total = sum(ctotalt, na.rm = TRUE),
    White_Pct = sum(cwhitt, na.rm = TRUE) / Total * 100,
    URM_Pct = sum(urm_count, na.rm = TRUE) / Total * 100,
    Hispanic_Pct = sum(chispt, na.rm = TRUE) / Total * 100,
    Black_Pct = sum(cbkaat, na.rm = TRUE) / Total * 100,
    Asian_Pct = sum(casiat, na.rm = TRUE) / Total * 100
  )

# annual trends: changes in demographics in elite feeder schools
elite_annual_trends <- music_degrees %>%
  filter(unitid %in% PB_ELITE_FEEDER_UNITIDS) %>%
  group_by(data_year) %>%
  summarise(
    Total = sum(ctotalt, na.rm = TRUE),
    Elite_URM_Pct = sum(urm_count, na.rm = TRUE) / Total * 100
  )

# Extract key years for figure
WHITE_2011 <- annual_trends$White_Pct[annual_trends$data_year == 2011]
WHITE_2024 <- annual_trends$White_Pct[annual_trends$data_year == 2024]
WHITE_CHANGE <- WHITE_2024 - WHITE_2011

URM_2011 <- annual_trends$URM_Pct[annual_trends$data_year == 2011]
URM_2024 <- annual_trends$URM_Pct[annual_trends$data_year == 2024]
URM_CHANGE <- URM_2024 - URM_2011

ELITE_URM_2011 <- elite_annual_trends$Elite_URM_Pct[elite_annual_trends$data_year == 2011]
ELITE_URM_2024 <- elite_annual_trends$Elite_URM_Pct[elite_annual_trends$data_year == 2024]

# Calculate gap widening
gap_trends <- annual_trends %>%
  left_join(elite_annual_trends %>% select(data_year, Elite_URM_Pct), by = "data_year") %>%
  mutate(Gap_PP = URM_Pct - Elite_URM_Pct)

GAP_2011 <- gap_trends$Gap_PP[gap_trends$data_year == 2011]
GAP_2024 <- gap_trends$Gap_PP[gap_trends$data_year == 2024]



## Calculate International Training Statistics
# Filtering musicians with ANY international training
musicians_with_intl <- musicians %>%
  filter(is_international == TRUE) %>%
  distinct(m_id)

# Musicians with ONLY international training (no U.S. degrees)
musicians_with_us <- musicians %>%
  filter(is_international == FALSE) %>%
  distinct(m_id) %>%
  pull(m_id)

intl_only_musicians <- musicians_with_intl %>%
  filter(!m_id %in% musicians_with_us)

# store values
N_INTL_TRAINED <- nrow(musicians_with_intl)
N_INTL_ONLY <- nrow(intl_only_musicians)
PCT_INTL_OF_SAMPLE <- N_INTL_TRAINED / N_MUSICIANS_ALL * 100
PCT_INTL_ONLY_OF_INTL <- N_INTL_ONLY / N_INTL_TRAINED * 100



## Calculate Pipeline Attrition (Pooled 2017-2024)
# graduate demographics pooled over dataset timeline
pooled_grads <- music_degrees |>
  filter(data_year >= POOLED_WINDOW_START, data_year <= POOLED_WINDOW_END) |>
  calc_demographic_stats()

# elite feeder school graduate demographics pooled over dataset timeline
pooled_elite_grads <- music_degrees |>
  filter(data_year >= POOLED_WINDOW_START, data_year <= POOLED_WINDOW_END,
         unitid %in% PB_ELITE_FEEDER_UNITIDS) |>
  calc_demographic_stats()

# Calculate and store attrition rates for table
BLACK_ATTRITION <- calc_attrition(pooled_grads$Black_Pct, LAO_2023_BENCHMARKS$Black)
HISPANIC_ATTRITION <- calc_attrition(pooled_grads$Hispanic_Pct, LAO_2023_BENCHMARKS$Hispanic)
####

# =============================================================================
# SECTION 5: TABLES 1-4
# Original code by Munoz, edited and adapted by Bommareddy
# =============================================================================

# Initialize table notes collection for statistical reporting
table_notes <- list()

# =============================================================================
# TABLE 1: Institutional Concentration and K-Coverage
# =============================================================================

## k-coverage table with cumulative coverage in groups 1-3
PB_table1_data <- k_coverage_stats |>
  transmute(
    `K-Coverage` = paste0("K", k_threshold * 100),
    Schools = n_schools,
    `Cumulative Placements %` = round(coverage_all, 1),
    `Group 1-3 Representation %` = round(coverage_123, 1)
  )

# notes for table 1
PB_table1_notes <- c(
  "K-coverage indicates the minimum number of schools required to account for X% of placements (based on musician biographies).",
  "Group 1-3 Representation % shows the share of Group 1-3 orchestra musicians trained at schools in each K-coverage set.",
  sprintf("Data based on musicians with linkable school records (n = %s).",
          format(N_MUSICIANS_LINKED, big.mark = ","))
)

# save to outputs folder
save_jce_table(
  PB_table1_data, 1,
  "Institutional Concentration and K-Coverage",
  "PB_Table1_Institutional_Concentration",
  PB_table1_notes,
  digits = 1
)

# =============================================================================
# TABLE 2: Elite Feeder Institutions and Output Characteristics
# =============================================================================


PB_elite_inst_stats <- music_degrees |>
  filter(unitid %in% PB_ELITE_FEEDER_UNITIDS) |>
  group_by(unitid, instnm) |>
  calc_demographic_stats() |>
  ungroup() |>
  left_join(counts_all %>% select(institution_key, rank), 
            by = c("unitid" = "institution_key")) |>
  mutate(National_Pct = Total_Degrees / TOTAL_DEGREES * 100)

PB_table2_data <- elite_inst_stats |>
  transmute(
    Institution = instnm,
    `Total Degrees` = round(Total_Degrees, 0),
    `% of National Total` = round(National_Pct, 1),
    `% URM` = round(URM_Pct, 2),
    `% NRA` = round(NRA_Pct, 2),
    Rank = round(rank, 0)
  ) |>
  arrange(is.na(Rank), Rank, desc(`Total Degrees`)) |>
  add_row(
    Institution = "Elite 12 Total",
    `Total Degrees` = round(stats_elite$Total_Degrees, 0),
    `% of National Total` = round(ELITE_DEGREE_SHARE, 1),
    `% URM` = round(stats_elite$URM_Pct, 2),
    `% NRA` = round(stats_elite$NRA_Pct, 2),
    Rank = NA_real_
  ) |>
  add_row(
    Institution = "All Other Schools",
    `Total Degrees` = round(stats_nonelite$Total_Degrees, 0),
    `% of National Total` = round(100 - ELITE_DEGREE_SHARE, 1),
    `% URM` = round(stats_nonelite$URM_Pct, 2),
    `% NRA` = round(stats_nonelite$NRA_Pct, 2),
    Rank = NA_real_
  )

PB_table2_notes <- c(
  "Elite feeders identified through demand-side algorithm (see Section 3.3).",
  "URM = Underrepresented Minority (Black, Hispanic, Indigenous, Pacific Islander).",
  "NRA = Nonresident Alien (international students).",
  "Rank based on share of placements across all orchestras in sample.",
  "Percentages for Elite 12 Total and All Other Schools are weighted averages."
)

save_jce_table(PB_table2_data, 2, "Elite Feeder Institutions and Output Characteristics",
               "PB_Table2_Elite_Feeders", PB_table2_notes, digits = 2)

# =============================================================================
# TABLE 3: Contingency Table - Elite Feeder Status and URM Status (Munoz og code)
# =============================================================================

cat("\nCreating Table 4...\n")

PB_table4_data = as.data.frame.matrix(contingency_2x2) |>
  tibble::rownames_to_column("Institution Type") |>
  mutate(Total = URM + `Non-URM`) |>
  select(`Institution Type`, URM, `Non-URM`, Total)

PB_table4_notes = c(
  sprintf("χ²(1, N = %s) = %.2f, p < .001.",
          format(contingency_results$N, big.mark = ","), contingency_results$chi_sq),
  sprintf("φ = %.2f; minimum expected count = %.2f.",
          contingency_results$phi, contingency_results$min_expected),
  sprintf("Risk Ratio = %.2f (95%% CI [%.2f, %.2f]).",
          contingency_results$risk_ratio, 
          contingency_results$rr_ci_low, contingency_results$rr_ci_high),
  sprintf("Bootstrap validation (BCa, 10,000 iterations): 95%% CI [%.2f, %.2f].",
          bootstrap_results$ci_bca["lower"], bootstrap_results$ci_bca["upper"]),
  "URM = Underrepresented Minority (Black, Hispanic, Indigenous, Pacific Islander).",
  "Elite Feeder = seven institutions identified through demand-side algorithm."
)

save_jce_table(PB_table4_data, 4, "Contingency Table: Elite Feeder Status and URM Status",
               "PB_Table4_Contingency", table4_notes, digits = 0)

# =============================================================================
# TABLE 4: Dual Pipeline Analysis (Pooled 2017-2024) 
# =============================================================================

## shows attrition rates across demogrpahics: from training to profession

PB_table6_data <- tibble(
  `Racial/Ethnic Group` = c("Hispanic", "Black", "Native American", "Asian", "White", "All URM"),
  `All Grads (%)` = c(
    pooled_grads$Hispanic_Pct, pooled_grads$Black_Pct,
    music_degrees |> 
      filter(data_year >= POOLED_WINDOW_START, data_year <= POOLED_WINDOW_END) |>
      summarise(pct = sum(caiant, na.rm = TRUE) / sum(ctotalt, na.rm = TRUE) * 100) %>% pull(pct),
    pooled_grads$Asian_Pct, pooled_grads$White_Pct, pooled_grads$URM_Pct
  ),
  `Elite Grads (%)` = c(
    pooled_elite_grads$Hispanic_Pct, pooled_elite_grads$Black_Pct,
    music_degrees |>
      filter(data_year >= POOLED_WINDOW_START, data_year <= POOLED_WINDOW_END,
             unitid %in% PB_ELITE_FEEDER_UNITIDS) |>
      summarise(pct = sum(caiant, na.rm = TRUE) / sum(ctotalt, na.rm = TRUE) * 100) |> pull(pct),
    pooled_elite_grads$Asian_Pct, pooled_elite_grads$White_Pct, pooled_elite_grads$URM_Pct
  ),
  `Profession (%)` = c(4.8, 2.4, 0.1, 11.0, 79.1, 7.5),
  `U.S. Population (%)` = c(19.5, 13.6, 1.3, 6.3, 59.3, 34.4)
) |>
  mutate(
    `Attrition Rate (%)` = if_else(`All Grads (%)` > 0,
                                   (`All Grads (%)` - `Profession (%)`) / `All Grads (%)` * 100,
                                   NA_real_),
    `Gap to Population (pp)` = `Profession (%)` - `U.S. Population (%)`
  ) |>
  mutate(across(where(is.numeric), ~round(.x, 1)))

PB_table6_notes <- c(
  sprintf("All Grads = Orchestral performance graduates, pooled %d–%d (IPEDS).",
          POOLED_WINDOW_START, POOLED_WINDOW_END),
  sprintf("Elite Grads = Graduates from seven elite feeder institutions, pooled %d–%d.",
          POOLED_WINDOW_START, POOLED_WINDOW_END),
  "Profession = Orchestral musicians (League of American Orchestras, 2023).",
  "Attrition Rate = [(All Grads − Profession) / All Grads] × 100.",
  "URM = Hispanic/Latino, Black, American Indian/Alaska Native, Native Hawaiian/Pacific Islander.",
  "Pooled window aligns with active audition pool and Figures 7–8."
)

save_jce_table(PB_table6_data, 6, "Dual Pipeline Analysis: Training vs. Profession Demographics",
               "PB_Table6_Dual_Pipeline", table6_notes, digits = 1)

####
# =============================================================================
# SECTION 6: FIGURES 1-8 
## Original code by Munoz, edited and adapted by Bommareddy
# =============================================================================


# Initialize figure captions collection
figure_captions <- list()

# =============================================================================
# FIGURE 1: Demographic Shifts in Orchestral Training 2011-2024
# =============================================================================

## Shows how racial demographics have changed in the schools
#this code uses primitive functions pre-made by Munoz.

PB_fig1_data <- annual_trends |>
  select(data_year, White_Pct, URM_Pct, Hispanic_Pct, Black_Pct) |>
  pivot_longer(-data_year, names_to = "Group", values_to = "Percentage") |>
  mutate(Group = factor(Group, 
                        levels = c("White_Pct", "URM_Pct", "Hispanic_Pct", "Black_Pct"),
                        labels = c("White", "URM Total", "Hispanic", "Black")))

PB_fig1 <- ggplot(PB_fig1_data, aes(x = data_year, y = Percentage, 
                                    color = Group, linetype = Group, shape = Group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("White" = JCE_PALETTE[1], "URM Total" = JCE_PALETTE[2],
                                "Hispanic" = JCE_PALETTE[3], "Black" = JCE_PALETTE[4])) +
  scale_linetype_manual(values = c("White" = "solid", "URM Total" = "longdash",
                                   "Hispanic" = "dashed", "Black" = "dotted")) +
  scale_shape_manual(values = c("White" = 16, "URM Total" = 17, "Hispanic" = 15, "Black" = 18)) +
  scale_x_continuous(breaks = seq(2011, 2024, 1), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 70)) +
  labs(x = "Academic Year", y = "Percentage of Students", color = NULL, linetype = NULL, shape = NULL) +
  theme_jce +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.spacing.x = unit(15, "pt")) +
  guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))

figure_captions$fig1 <- sprintf(
  "**Fig. 1** Demographic shifts in orchestral training 2011-2024. Notes: White: %.1f%% to %.1f%% (%+.1f pp); URM: %.1f%% to %.1f%% (%+.1f pp). Data from IPEDS Completions Survey 2011-2024.",
  WHITE_2011, WHITE_2024, WHITE_CHANGE, URM_2011, URM_2024, URM_CHANGE
)

save_jce_figure(PB_fig1, "PB_Fig1_Demographic_Shifts")


# =============================================================================
# FIGURE 2: Racial Bottleneck
# =============================================================================

# shows how elite schools act as a bottlenecking filter against access to professional orchestras

elite_ci <- calc_wilson_ci(stats_elite$URM_Degrees, stats_elite$Total_Degrees)
nonelite_ci <- calc_wilson_ci(stats_nonelite$URM_Degrees, stats_nonelite$Total_Degrees)

PB_fig3_data <- tibble(
  Institution_Type = factor(c("Elite Feeders", "Non-Feeders"), levels = c("Elite Feeders", "Non-Feeders")),
  URM_Percentage = c(stats_elite$URM_Pct, stats_nonelite$URM_Pct),
  URM_Count = c(stats_elite$URM_Degrees, stats_nonelite$URM_Degrees),
  Total_Count = c(stats_elite$Total_Degrees, stats_nonelite$Total_Degrees),
  CI_Lower = c(elite_ci["lower"], nonelite_ci["lower"]),
  CI_Upper = c(elite_ci["upper"], nonelite_ci["upper"])
) |>
  mutate(Label_Pct = sprintf("%.1f%%", URM_Percentage),
         Label_Detail = sprintf("%s URM\n%s total", format(URM_Count, big.mark = ","), format(Total_Count, big.mark = ",")))

PB_fig3 <- ggplot(fig3_data, aes(x = Institution_Type, y = URM_Percentage)) +
  geom_col_pattern(aes(fill = Institution_Type, pattern = Institution_Type),
                   width = 0.52, color = "black", linewidth = 0.5,
                   pattern_fill = "black", pattern_colour = "black",
                   pattern_density = 0.1, pattern_spacing = 0.03) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.14, linewidth = 0.6) +
  geom_text(aes(label = Label_Pct), vjust = -1.5, size = 4.2, fontface = "bold") +
  geom_text(aes(label = Label_Detail, y = pmax(URM_Percentage * 0.55, 1.5)),
            color = "white", size = 3.15, fontface = "bold", lineheight = 0.95) +
  scale_fill_manual(values = c("Elite Feeders" = JCE_PALETTE[1], "Non-Feeders" = JCE_PALETTE[2]), guide = "none") +
  scale_pattern_manual(values = c("Elite Feeders" = "stripe", "Non-Feeders" = "crosshatch"), guide = "none") +
  scale_y_continuous(name = "URM Percentage (%)", labels = percent_format(scale = 1),
                     limits = c(0, max(fig3_data$CI_Upper) * 1.15), expand = expansion(mult = c(0, 0.03))) +
  labs(x = NULL) +
  theme_jce +
  theme(axis.text.x = element_text(size = 11, face = "bold"), panel.grid.major.x = element_blank())

figure_captions$PB_fig3 <- sprintf(
  "**Fig. 3** Racial bottleneck in access to elite training. Elite: %.2f%% URM; Non-elite: %.2f%% URM; Gap: %.2f pp. Risk ratio = %.2f, 95%% CI [%.2f, %.2f]. χ²(1, N = %s) = %.2f, p < .001.",
  stats_elite$URM_Pct, stats_nonelite$URM_Pct, URM_GAP_PP,
  contingency_results$risk_ratio, contingency_results$rr_ci_low, contingency_results$rr_ci_high,
  format(contingency_results$N, big.mark = ","), contingency_results$chi_sq
)

save_jce_figure(PB_fig3, "PB_Fig3_Racial_Bottleneck")

# =============================================================================
# FIGURE 3: Diverging Pathways
# =============================================================================

cat("\nCreating Figure 4...\n")

fig3_data <- gap_trends |>
  select(data_year, National_URM_Pct = URM_Pct, Elite_URM_Pct, Gap_PP) |>
  filter(!is.na(Elite_URM_Pct))

fig3_long <- fig3_data |>
  pivot_longer(c(National_URM_Pct, Elite_URM_Pct), names_to = "Group", values_to = "URM_Pct") |>
  mutate(Group = factor(Group, levels = c("National_URM_Pct", "Elite_URM_Pct"),
                        labels = c("All Orchestral Graduates", "Elite Feeder Graduates")))

mid_year <- fig4_data$data_year[ceiling(nrow(fig4_data) / 2)]
mid_y <- fig4_data |>
  filter(data_year == mid_year) |>
  summarise(y = (National_URM_Pct + Elite_URM_Pct) / 2) |>
  pull(y)
gap_label <- sprintf("Gap: %.1f pp to %.1f pp\nWidened by %.1f pp", GAP_2011, GAP_2024, GAP_2024 - GAP_2011)

fig3 <- ggplot(fig4_long, aes(x = data_year, y = URM_Pct, color = Group, linetype = Group, shape = Group)) +
  geom_ribbon(data = fig4_data, aes(x = data_year, 
                                    ymin = pmin(National_URM_Pct, Elite_URM_Pct), ymax = pmax(National_URM_Pct, Elite_URM_Pct)),
              inherit.aes = FALSE, fill = "gray90") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  annotate("text", x = mid_year, y = mid_y, label = gap_label, hjust = 0.5, vjust = 0.5, size = 3.2, fontface = "bold", lineheight = 0.95) +
  scale_color_manual(name = NULL, values = c("All Orchestral Graduates" = JCE_PALETTE[1], "Elite Feeder Graduates" = JCE_PALETTE[2])) +
  scale_linetype_manual(name = NULL, values = c("All Orchestral Graduates" = "solid", "Elite Feeder Graduates" = "dashed")) +
  scale_shape_manual(name = NULL, values = c("All Orchestral Graduates" = 16, "Elite Feeder Graduates" = 17)) +
  scale_x_continuous(name = "Academic Year", breaks = seq(2011, 2024, 1)) +
  scale_y_continuous(name = "URM Percentage (%)", labels = percent_format(scale = 1)) +
  theme_jce +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))

figure_captions$fig3 <- sprintf(
  "**Fig. 3** Diverging pathways: Gap widened from %.1f pp (2011) to %.1f pp (2024).",
  GAP_2011, GAP_2024
)

save_jce_figure(fig3, "Fig3_Diverging_Pathways")



# =============================================================================
# FIGURE 3: Demographic Trade-Off
# =============================================================================

#addresses intersectionality question; how do gender and race effect the filter.
cat("\nCreating Figure 6...\n")

pop_stats <- institution_demographics %>%
  summarise(avg_urm = mean(urm_pct), avg_women = mean(women_pct))

fig6 <- ggplot() +
  geom_point(data = institution_demographics %>% filter(elite_status == "Non-Elite"),
             aes(x = urm_pct, y = women_pct, shape = "Non-Elite"),
             color = "gray60", fill = "white", size = 2, stroke = 0.6) +
  geom_point(data = institution_demographics %>% filter(elite_status == "Elite"),
             aes(x = urm_pct, y = women_pct, shape = "Elite"),
             color = "black", fill = JCE_PALETTE[1], size = 4, stroke = 0.8) +
  geom_smooth(data = institution_demographics, aes(x = urm_pct, y = women_pct),
              method = "lm", formula = y ~ x, color = "black", se = FALSE, linewidth = 1.2) +
  geom_hline(yintercept = pop_stats$avg_women, linetype = "dotted", color = "gray40", linewidth = 0.8) +
  geom_vline(xintercept = pop_stats$avg_urm, linetype = "dotted", color = "gray40", linewidth = 0.8) +
  scale_shape_manual(name = NULL, values = c("Non-Elite" = 21, "Elite" = 24),
                     labels = c("Non-Elite Institutions", "Elite Feeder Institutions")) +
  scale_x_continuous(limits = c(-2, 102), breaks = seq(0, 100, 20)) +
  scale_y_continuous(limits = c(-2, 102), breaks = seq(0, 100, 20)) +
  labs(x = "URM Student Percentage (%)", y = "Women Student Percentage (%)") +
  theme_jce +
  theme(legend.position = "bottom", panel.grid.major = element_line(color = "gray92"))

figure_captions$fig6 <- sprintf(
  "**Fig. 6** Demographic trade-off between URM and women percentages. Notes: Institutions with 50+ degrees (n = %s). Dashed lines show averages. r = %.2f, p < .001. Data from IPEDS 2011-2024.",
  format(CORRELATION_N, big.mark = ","), CORRELATION_R
)

save_jce_figure(fig6, "Fig6_Demographic_Tradeoff")

# =============================================================================
# FIGURE 4: Stepwise Pipeline
# =============================================================================

## Shows the filtering effect in a stepwise graph

fig5_data <- tibble(
  Stage = factor(c(rep(sprintf("All Graduates\n(%d-%d)", POOLED_WINDOW_START, POOLED_WINDOW_END), 4),
                   rep("Elite Feeder\nGraduates", 4),
                   rep("Professional\nMusicians", 4)),
                 levels = c(sprintf("All Graduates\n(%d-%d)", POOLED_WINDOW_START, POOLED_WINDOW_END),
                            "Elite Feeder\nGraduates", "Professional\nMusicians")),
  Group = factor(rep(c("Black", "Hispanic", "White", "Asian"), 3),
                 levels = c("Black", "Hispanic", "White", "Asian")),
  Percentage = c(
    pooled_grads$Black_Pct, pooled_grads$Hispanic_Pct, pooled_grads$White_Pct, pooled_grads$Asian_Pct,
    pooled_elite_grads$Black_Pct, pooled_elite_grads$Hispanic_Pct, pooled_elite_grads$White_Pct, pooled_elite_grads$Asian_Pct,
    LAO_2023_BENCHMARKS$Black, LAO_2023_BENCHMARKS$Hispanic, LAO_2023_BENCHMARKS$White, LAO_2023_BENCHMARKS$Asian
  )
) |>
  mutate(Label = sprintf("%.1f%%", Percentage))

y_max <- max(fig7_data$Percentage) * 1.15

fig5 <- ggplot(fig7_data, aes(x = Stage, y = Percentage, fill = Group, pattern = Group)) +
  geom_col_pattern(position = position_dodge(width = 0.78), width = 0.62,
                   color = "black", linewidth = 0.5,
                   pattern_fill = "black", pattern_colour = "black",
                   pattern_density = 0.1, pattern_spacing = 0.02) +
  geom_text(aes(label = Label), position = position_dodge(width = 0.78), vjust = -0.35, size = 3.2, fontface = "bold") +
  annotate("text", x = 1, y = y_max * 0.95, label = "SUPPLY", size = 3.6, fontface = "bold", color = "gray35") +
  annotate("text", x = 2, y = y_max * 0.95, label = "FILTER", size = 3.6, fontface = "bold", color = "gray35") +
  annotate("text", x = 3, y = y_max * 0.95, label = "OUTCOME", size = 3.6, fontface = "bold", color = "gray35") +
  scale_fill_manual(name = NULL, values = c("Black" = JCE_PALETTE[1], "Hispanic" = JCE_PALETTE[2],
                                            "White" = JCE_PALETTE[3], "Asian" = JCE_PALETTE[4])) +
  scale_pattern_manual(name = NULL, values = c("Black" = "stripe", "Hispanic" = "crosshatch",
                                               "White" = "none", "Asian" = "circle")) +
  scale_y_continuous(name = "Representation (%)", limits = c(0, y_max), breaks = seq(0, 80, 20)) +
  labs(x = NULL) +
  theme_jce +
  theme(legend.position = "bottom", axis.text.x = element_text(size = 10, face = "bold"), panel.grid.major.x = element_blank())

figure_captions$fig7 <- sprintf(
  "**Fig. 7** Stepwise representation across the pipeline. Notes: Graduate data from IPEDS %d-%d; Professional benchmarks from League of American Orchestras (2023).",
  POOLED_WINDOW_START, POOLED_WINDOW_END
)

save_jce_figure(fig5, "Fig5_Stepwise_Pipeline")

# =============================================================================
# FIGURE 6: Complete Pipeline
# =============================================================================

## The highlight of the paper: shows just how effectively elite schools act as a constrictor

cat("\nCreating Figure 8...\n")

other_all = 100 - (pooled_grads$White_Pct + pooled_grads$Hispanic_Pct + 
                     pooled_grads$Black_Pct + pooled_grads$Asian_Pct)
other_elite = 100 - (pooled_elite_grads$White_Pct + pooled_elite_grads$Hispanic_Pct +
                       pooled_elite_grads$Black_Pct + pooled_elite_grads$Asian_Pct)

fig6_data = tibble(
  Stage = factor(c("U.S. Adult Population", "All Graduates", "Elite Graduates", "Professional Musicians"),
                 levels = c("U.S. Adult Population", "All Graduates", "Elite Graduates", "Professional Musicians")),
  White = c(US_POP_2024$White, pooled_grads$White_Pct, pooled_elite_grads$White_Pct, LAO_2023_BENCHMARKS$White),
  Hispanic = c(US_POP_2024$Hispanic, pooled_grads$Hispanic_Pct, pooled_elite_grads$Hispanic_Pct, LAO_2023_BENCHMARKS$Hispanic),
  Black = c(US_POP_2024$Black, pooled_grads$Black_Pct, pooled_elite_grads$Black_Pct, LAO_2023_BENCHMARKS$Black),
  Asian = c(US_POP_2024$Asian, pooled_grads$Asian_Pct, pooled_elite_grads$Asian_Pct, LAO_2023_BENCHMARKS$Asian),
  Other = c(100 - 59.3 - 19.5 - 13.6 - 6.3, other_all, other_elite, 100 - 79.1 - 4.8 - 2.4 - 11.0)
)

fig6_long = fig8_data |>
  pivot_longer(-Stage, names_to = "Group", values_to = "Percentage") |>
  mutate(Group = factor(Group, levels = c("Asian", "Black", "Hispanic", "Other", "White")),
         Label = if_else(Percentage >= 2, sprintf("%.1f%%", Percentage), ""))

stage_labels = tibble(
  Stage = fig8_data$Stage,
  label = c("U.S. ADULT\nPOPULATION", "ALL\nGRADUATES", "ELITE\nGRADUATES", "PROFESSIONAL\nMUSICIANS")
)

fig6 <- ggplot(fig8_long, aes(x = Stage, y = Percentage, fill = Group, pattern = Group)) +
  geom_col_pattern(width = 0.62, linewidth = 0.5, color = "blue",
                   pattern_fill = "blue", pattern_colour = "blue",
                   pattern_density = 0.08, pattern_spacing = 0.02) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white", size = 3, fontface = "bold") +
  scale_fill_manual(name = NULL, values = c("Asian" = "#984EA3", "Black" = "#4DAF4A", "Hispanic" = "#377EB8",
                                            "Other" = "gray70", "White" = "#E41A1C"),
                    labels = c("Asian", "Black", "Hispanic", "Other*", "White")) +
  scale_pattern_manual(name = NULL, values = c("Asian" = "wave", "Black" = "circle", "Hispanic" = "crosshatch",
                                               "Other" = "stripe", "White" = "none"),
                       labels = c("Asian", "Black", "Hispanic", "Other*", "White")) +
  scale_y_continuous(name = "Percentage (%)", limits = c(-8, 100), breaks = seq(0, 100, 20)) +
  labs(x = NULL) +
  theme_jce +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(), legend.position = "bottom", plot.margin = margin(10, 10, 20, 10)) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE), pattern = guide_legend(nrow = 1, reverse = TRUE)) +
  coord_cartesian(clip = "off") +
  geom_text(data = stage_labels, aes(x = Stage, y = -6, label = label),
            inherit.aes = FALSE, hjust = 0.5, size = 3.1, fontface = "bold", color = "gray30", lineheight = 0.9)

figure_captions$fig8 = sprintf(
  "**Fig. 8** Complete pipeline from population to profession. Notes: Graduate composition pooled %d-%d; U.S. Population: 2024 Census estimates; Professional: LAO (2023); 'Other' includes Native American, Pacific Islander, Two or more races, NRA.",
  POOLED_WINDOW_START, POOLED_WINDOW_END
)

save_jce_figure(fig6, "PB_Fig6_Complete_Pipeline")



