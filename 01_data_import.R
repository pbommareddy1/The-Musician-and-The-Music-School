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


