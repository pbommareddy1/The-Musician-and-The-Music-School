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
rm(list = ls())

# Set working directory to script location
# (Uncomment and modify)
# setwd("/Downloads/Research/Alfredo Munoz Meritocracy Myth/Code")

cat("\n")
cat(strrep("═", 70), "\n")
cat("  THE MERITOCRACY MYTH: ORCHESTRAL LABOR MARKET ANALYSIS\n")
cat("  Version 3.0 | ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("═", 70), "\n")

# Source all modules
source("00_setup.R")
source("01_data_import.R")
source("02_transformations.R")
source("03_primitives.R")
source("04_core_analyses.R")
source("05_tables.R")
source("06_figures.R")
source("07_reporting.R")

cat("\n  All modules executed successfully.\n\n")
