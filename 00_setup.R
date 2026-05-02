# =============================================================================
# THE MERITOCRACY MYTH: ORCHESTRAL LABOR MARKET ANALYSIS
# =============================================================================
# Author: Alfredo Munoz
# Version: 1.0
# Last Updated: 2026-01-26
#
# SCRIPT STRUCTURE:
# ─────────────────────────────────────────────────────────────────────────────
#   SECTION 0: Environment Setup
#   SECTION 1: Data Import & Validation
#   SECTION 2: Data Transformations
#   SECTION 3: Analysis Primitives (reusable metric functions)
#   SECTION 4: Core Analyses (apply primitives to generate results)
#   SECTION 5: Tables 1-7
#   SECTION 6: Figures 1-8
#   SECTION 7: Statistical Reporting (APA-formatted output)
#   SECTION 8: Export Outputs
#
# OUTPUTS:
#   - Tables 1-7 (.docx, .csv)
#   - Figures 1-8 (.eps)
#   - Figure_Captions.docx
#   - Manuscript_Statistics_Reference.docx
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
