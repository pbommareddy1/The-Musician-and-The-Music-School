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




