# =============================================================================
# SECTION 4: CORE ANALYSES
# =============================================================================
# This section applies the primitives to generate all analytical results.
# Results are stored in clearly named objects for use in tables/figures.
# =============================================================================

cat("\n")
cat(strrep("═", 70), "\n")
cat("  SECTION 4: CORE ANALYSES\n")
cat(strrep("═", 70), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 4.1 Calculate Placement Concentrations by Tier
# ─────────────────────────────────────────────────────────────────────────────

cat("\nCalculating placement concentrations by orchestra tier...\n")

counts_all = calc_placement_counts(musicians_linked, orchestra_filter = NULL)
counts_g1 = calc_placement_counts(musicians_linked, orchestra_filter = group1_orchestras)
counts_top8 = calc_placement_counts(musicians_linked, orchestra_filter = TOP8_ORCHESTRAS)

cat(sprintf("  ✓ All orchestras: %d institutions with placements\n", nrow(counts_all)))
cat(sprintf("  ✓ LOA Group 1:    %d institutions\n", nrow(counts_g1)))
cat(sprintf("  ✓ Top 8:          %d institutions\n", nrow(counts_top8)))

# ─────────────────────────────────────────────────────────────────────────────
# 4.2 Identify Elite Feeder Institutions
# ─────────────────────────────────────────────────────────────────────────────

cat("\nIdentifying elite feeder institutions...\n")

# Get K50 sets for each tier
k50_all   <- get_k_coverage_set(counts_all, 0.50)
k50_g1    <- get_k_coverage_set(counts_g1, 0.50)
k50_top8  <- get_k_coverage_set(counts_top8, 0.50)


# Find institutions present in at least 2 K50 sets (cross-tier consistency)
all_k50_institutions <- unique(c(k50_all, k50_g1, k50_top8))
ipeds_unitids <- unique(music_degrees$unitid)

presence <- tibble(institution_key = all_k50_institutions) %>%
  mutate(
    in_all   = institution_key %in% k50_all,
    in_g1    = institution_key %in% k50_g1,
    in_top8  = institution_key %in% k50_top8,
    tiers_present = in_all + in_g1 + in_top8
  )

candidate_keys <- presence %>%
  filter(tiers_present >= 2, institution_key %in% ipeds_unitids) %>%
  pull(institution_key)

# Greedy algorithm to find minimal set achieving ≥50% coverage in Top 8 and Group 1
candidate_contrib <- tibble(institution_key = candidate_keys) %>%
  left_join(ipeds_institutions, by = c("institution_key" = "unitid")) %>%
  mutate(
    top8_share = map_dbl(institution_key, ~calc_set_coverage(counts_top8, .x)),
    g1_share   = map_dbl(institution_key, ~calc_set_coverage(counts_g1, .x)),
    total_contrib = top8_share + g1_share
  ) %>%
  arrange(desc(total_contrib))

# Greedy selection
selected <- character(0)
for (i in seq_len(nrow(candidate_contrib))) {
  selected <- c(selected, candidate_contrib$institution_key[i])
  if (calc_set_coverage(counts_top8, selected) >= 0.50 &&
      calc_set_coverage(counts_g1, selected) >= 0.50) {
    break
  }
}

ELITE_FEEDER_UNITIDS <- selected

# Build elite feeder lookup table
elite_feeders <- tibble(unitid = ELITE_FEEDER_UNITIDS) %>%
  left_join(ipeds_institutions, by = "unitid") %>%
  left_join(counts_all %>% select(institution_key, rank_all = rank), 
            by = c("unitid" = "institution_key")) %>%
  left_join(counts_g1 %>% select(institution_key, rank_g1 = rank),
            by = c("unitid" = "institution_key")) %>%
  left_join(counts_top8 %>% select(institution_key, rank_top8 = rank),
            by = c("unitid" = "institution_key")) %>%
  arrange(rank_top8, rank_g1, rank_all)

# Calculate coverage metrics
COVERAGE_ALL   <- calc_set_coverage(counts_all, ELITE_FEEDER_UNITIDS)
COVERAGE_G1    <- calc_set_coverage(counts_g1, ELITE_FEEDER_UNITIDS)
COVERAGE_TOP8  <- calc_set_coverage(counts_top8, ELITE_FEEDER_UNITIDS)

cat(sprintf("\n  ELITE FEEDER IDENTIFICATION RESULTS:\n"))
cat(sprintf("  ─────────────────────────────────────────\n"))
cat(sprintf("  Number identified: %d\n", length(ELITE_FEEDER_UNITIDS)))
cat(sprintf("  Coverage - All:    %.1f%%\n", COVERAGE_ALL * 100))
cat(sprintf("  Coverage - G1:     %.1f%%\n", COVERAGE_G1 * 100))
cat(sprintf("  Coverage - Top 8:  %.1f%%\n", COVERAGE_TOP8 * 100))
cat(sprintf("  ─────────────────────────────────────────\n"))
for (i in seq_len(nrow(elite_feeders))) {
  cat(sprintf("  %d. %s\n", i, elite_feeders$instnm[i]))
}

# Identify Juilliard for special analyses
JUILLIARD_UNITID <- elite_feeders %>%
  filter(str_detect(instnm, "Juilliard")) %>%
  pull(unitid) %>%
  first()

ELITE6_UNITIDS <- setdiff(ELITE_FEEDER_UNITIDS, JUILLIARD_UNITID)

# ─────────────────────────────────────────────────────────────────────────────
# 4.3 Calculate K-Coverage Statistics
# ─────────────────────────────────────────────────────────────────────────────

cat("\nCalculating K-coverage statistics...\n")

k_coverage_stats <- tibble(
  k_threshold = c(0.25, 0.50, 0.75)
) %>%
  mutate(
    k_set = map(k_threshold, ~get_k_coverage_set(counts_all, .x)),
    n_schools = map_int(k_set, length),
    coverage_all = map_dbl(k_set, ~calc_set_coverage(counts_all, .x) * 100),
    coverage_top8 = map_dbl(k_set, ~calc_set_coverage(counts_top8, .x) * 100)
  ) %>%
  select(-k_set)

# Store individual K values for easy reference
K25_SCHOOLS <- k_coverage_stats$n_schools[1]
K50_SCHOOLS <- k_coverage_stats$n_schools[2]
K75_SCHOOLS <- k_coverage_stats$n_schools[3]

cat(sprintf("  ✓ K25: %d schools (%.1f%% coverage)\n", K25_SCHOOLS, k_coverage_stats$coverage_all[1]))
cat(sprintf("  ✓ K50: %d schools (%.1f%% coverage)\n", K50_SCHOOLS, k_coverage_stats$coverage_all[2]))
cat(sprintf("  ✓ K75: %d schools (%.1f%% coverage)\n", K75_SCHOOLS, k_coverage_stats$coverage_all[3]))

# ─────────────────────────────────────────────────────────────────────────────
# 4.4 Calculate Concentration Metrics
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

# ─────────────────────────────────────────────────────────────────────────────
# 4.5 Calculate Elite vs Non-Elite Demographics
# ─────────────────────────────────────────────────────────────────────────────

cat("\nCalculating elite vs non-elite demographics...\n")

stats_elite <- music_degrees %>%
  filter(unitid %in% ELITE_FEEDER_UNITIDS) %>%
  calc_demographic_stats()

stats_nonelite <- music_degrees %>%
  filter(!unitid %in% ELITE_FEEDER_UNITIDS) %>%
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

cat(sprintf("  ✓ Elite degrees:    %s (%.1f%% of total)\n", 
            format(stats_elite$Total_Degrees, big.mark = ","), ELITE_DEGREE_SHARE))
cat(sprintf("  ✓ Elite URM:        %.2f%%\n", stats_elite$URM_Pct))
cat(sprintf("  ✓ Non-Elite URM:    %.2f%%\n", stats_nonelite$URM_Pct))
cat(sprintf("  ✓ URM Gap:          %.2f pp\n", URM_GAP_PP))
cat(sprintf("  ✓ NRA (all degrees): %.1f%%\n", NRA_ALL_PCT))

# ─────────────────────────────────────────────────────────────────────────────
# 4.6 Build Contingency Table and Statistical Tests
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

cat(sprintf("  ✓ χ²(1, N=%s) = %.2f, p < .001\n", 
            format(contingency_results$N, big.mark = ","), contingency_results$chi_sq))
cat(sprintf("  ✓ φ = %.2f\n", contingency_results$phi))
cat(sprintf("  ✓ Min expected count: %.2f\n", contingency_results$min_expected))
cat(sprintf("  ✓ Risk Ratio = %.2f, 95%% CI [%.2f, %.2f]\n",
            contingency_results$risk_ratio, 
            contingency_results$rr_ci_low, contingency_results$rr_ci_high))
cat(sprintf("  ✓ Bootstrap BCa CI: [%.2f, %.2f]\n",
            bootstrap_results$ci_bca["lower"], bootstrap_results$ci_bca["upper"]))

# ─────────────────────────────────────────────────────────────────────────────
# 4.7 Calculate Demographic Trends Over Time
# ─────────────────────────────────────────────────────────────────────────────

cat("\nCalculating demographic trends...\n")

# Annual trends
annual_trends <- music_degrees %>%
  group_by(data_year) %>%
  summarise(
    Total = sum(ctotalt, na.rm = TRUE),
    White_Pct = sum(cwhitt, na.rm = TRUE) / Total * 100,
    URM_Pct = sum(urm_count, na.rm = TRUE) / Total * 100,
    Hispanic_Pct = sum(chispt, na.rm = TRUE) / Total * 100,
    Black_Pct = sum(cbkaat, na.rm = TRUE) / Total * 100,
    Asian_Pct = sum(casiat, na.rm = TRUE) / Total * 100,
    .groups = "drop"
  )

# Elite annual trends
elite_annual_trends <- music_degrees %>%
  filter(unitid %in% ELITE_FEEDER_UNITIDS) %>%
  group_by(data_year) %>%
  summarise(
    Total = sum(ctotalt, na.rm = TRUE),
    Elite_URM_Pct = sum(urm_count, na.rm = TRUE) / Total * 100,
    .groups = "drop"
  )

# Extract key years for reporting
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

cat(sprintf("  ✓ White: %.1f%% → %.1f%% (%.1f pp)\n", WHITE_2011, WHITE_2024, WHITE_CHANGE))
cat(sprintf("  ✓ URM: %.1f%% → %.1f%% (+%.1f pp)\n", URM_2011, URM_2024, URM_CHANGE))
cat(sprintf("  ✓ Elite URM: %.1f%% → %.1f%%\n", ELITE_URM_2011, ELITE_URM_2024))
cat(sprintf("  ✓ Gap widening: %.1f pp → %.1f pp\n", GAP_2011, GAP_2024))

# ─────────────────────────────────────────────────────────────────────────────
# 4.8 Calculate International Training Statistics
# ─────────────────────────────────────────────────────────────────────────────

cat("\nCalculating international training statistics...\n")

# Flag musicians with international training
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

N_INTL_TRAINED <- nrow(musicians_with_intl)
N_INTL_ONLY <- nrow(intl_only_musicians)
PCT_INTL_OF_SAMPLE <- N_INTL_TRAINED / N_MUSICIANS_ALL * 100
PCT_INTL_ONLY_OF_INTL <- N_INTL_ONLY / N_INTL_TRAINED * 100

cat(sprintf("  ✓ Internationally trained: %d (%.1f%% of sample)\n", 
            N_INTL_TRAINED, PCT_INTL_OF_SAMPLE))
cat(sprintf("  ✓ International-only: %d (%.1f%% of intl trained)\n",
            N_INTL_ONLY, PCT_INTL_ONLY_OF_INTL))

# ─────────────────────────────────────────────────────────────────────────────
# 4.9 Calculate Demographic Trade-Off Correlation
# ─────────────────────────────────────────────────────────────────────────────

cat("\nCalculating demographic trade-off correlation...\n")

institution_demographics <- raw_ipeds %>%
  mutate(
    unitid = as.character(unitid),
    ctotalt = as.numeric(ctotalt),
    ctotalw = as.numeric(ctotalw),
    chispt = as.numeric(chispt),
    cbkaat = as.numeric(cbkaat),
    caiant = as.numeric(caiant),
    cnhpit = as.numeric(cnhpit)
  ) %>%
  filter(!is.na(unitid)) %>%
  group_by(unitid) %>%
  summarise(
    total_degrees = sum(ctotalt, na.rm = TRUE),
    urm_pct = if_else(total_degrees > 0,
                      sum(chispt + cbkaat + caiant + cnhpit, na.rm = TRUE) / total_degrees * 100,
                      NA_real_),
    women_pct = if_else(total_degrees > 0,
                        sum(ctotalw, na.rm = TRUE) / total_degrees * 100,
                        NA_real_),
    .groups = "drop"
  ) %>%
  filter(
    total_degrees >= 50,
    !is.na(urm_pct), !is.na(women_pct),
    is.finite(urm_pct), is.finite(women_pct)
  ) %>%
  mutate(elite_status = if_else(unitid %in% ELITE_FEEDER_UNITIDS, "Elite", "Non-Elite"))

cor_test_result <- cor.test(
  institution_demographics$urm_pct, 
  institution_demographics$women_pct
)

CORRELATION_R <- cor_test_result$estimate
CORRELATION_CI_LOW <- cor_test_result$conf.int[1]
CORRELATION_CI_HIGH <- cor_test_result$conf.int[2]
CORRELATION_N <- nrow(institution_demographics)

cat(sprintf("  ✓ r = %.2f, 95%% CI [%.2f, %.2f], n = %d\n",
            CORRELATION_R, CORRELATION_CI_LOW, CORRELATION_CI_HIGH, CORRELATION_N))

# ─────────────────────────────────────────────────────────────────────────────
# 4.10 Calculate Pipeline Attrition (Pooled 2017-2024)
# ─────────────────────────────────────────────────────────────────────────────

cat("\nCalculating pipeline attrition...\n")

# Pooled graduate demographics
pooled_grads <- music_degrees %>%
  filter(data_year >= POOLED_WINDOW_START, data_year <= POOLED_WINDOW_END) %>%
  calc_demographic_stats()

pooled_elite_grads <- music_degrees %>%
  filter(data_year >= POOLED_WINDOW_START, data_year <= POOLED_WINDOW_END,
         unitid %in% ELITE_FEEDER_UNITIDS) %>%
  calc_demographic_stats()

# Calculate attrition rates
BLACK_ATTRITION <- calc_attrition(pooled_grads$Black_Pct, LAO_2023_BENCHMARKS$Black)
HISPANIC_ATTRITION <- calc_attrition(pooled_grads$Hispanic_Pct, LAO_2023_BENCHMARKS$Hispanic)

cat(sprintf("  ✓ Black attrition: %.1f%%\n", BLACK_ATTRITION))
cat(sprintf("  ✓ Hispanic attrition: %.1f%%\n", HISPANIC_ATTRITION))

# ─────────────────────────────────────────────────────────────────────────────
# 4.11 Julliard vs Elite 6 Analysis
# ─────────────────────────────────────────────────────────────────────────────

cat("\nCalculating Juilliard vs Elite 6 shares...\n")

JUILLIARD_SHARE_ALL   <- calc_set_coverage(counts_all, JUILLIARD_UNITID) * 100
JUILLIARD_SHARE_G1    <- calc_set_coverage(counts_g1, JUILLIARD_UNITID) * 100
JUILLIARD_SHARE_TOP8  <- calc_set_coverage(counts_top8, JUILLIARD_UNITID) * 100

ELITE6_SHARE_ALL   <- calc_set_coverage(counts_all, ELITE6_UNITIDS) * 100
ELITE6_SHARE_G1    <- calc_set_coverage(counts_g1, ELITE6_UNITIDS) * 100
ELITE6_SHARE_TOP8  <- calc_set_coverage(counts_top8, ELITE6_UNITIDS) * 100

cat(sprintf("  ✓ Juilliard (Top 8): %.1f%%\n", JUILLIARD_SHARE_TOP8))
cat(sprintf("  ✓ Elite 6 (Top 8): %.1f%% (computed directly)\n", ELITE6_SHARE_TOP8))

cat("\n")
cat(strrep("═", 70), "\n")
cat("  CORE ANALYSES COMPLETE\n")
cat(strrep("═", 70), "\n")
