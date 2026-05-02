# =============================================================================
# SECTION 3: ANALYSIS PRIMITIVES
# =============================================================================
# This section defines reusable functions for calculating metrics.
# These are "primitives" - building blocks used throughout the analysis.
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

cat("\n  ✓ Analysis primitives defined\n")
cat("    - Concentration: calc_placement_counts, get_k_coverage_set, calc_set_coverage, calc_gini, calc_hhi\n")
cat("    - Demographics: calc_demographic_stats, calc_wilson_ci\n")
cat("    - Statistics: analyze_contingency, bootstrap_risk_ratio_bca\n")
cat("    - Attrition: calc_attrition\n")
cat("    - International: map_country_to_region\n")
