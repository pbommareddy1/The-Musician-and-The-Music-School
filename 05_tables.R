# =============================================================================
# SECTION 5: TABLES 1-7
# =============================================================================

cat("\n")
cat(strrep("═", 70), "\n")
cat("  SECTION 5: TABLES\n")
cat(strrep("═", 70), "\n")

# Initialize table notes collection for statistical reporting
table_notes <- list()

# =============================================================================
# TABLE 1: Institutional Concentration and K-Coverage
# =============================================================================

cat("\nCreating Table 1...\n")

table1_data <- k_coverage_stats %>%
  transmute(
    `K-Coverage` = paste0("K", k_threshold * 100),
    Schools = n_schools,
    `Cumulative Placements %` = round(coverage_all, 1),
    `Top 8 Representation %` = round(coverage_top8, 1)
  )

table1_notes <- c(
  "K-coverage indicates the minimum number of schools required to account for X% of placements (based on musician biographies).",
  "Top 8 Representation % shows the share of Top 8 orchestra musicians trained at schools in each K-coverage set.",
  sprintf("Data based on musicians with linkable school records (n = %s).",
          format(N_MUSICIANS_LINKED, big.mark = ","))
)

save_jce_table(
  table1_data, 1,
  "Institutional Concentration and K-Coverage",
  "Table1_Institutional_Concentration",
  table1_notes,
  digits = 1
)

# =============================================================================
# TABLE 2: Elite Feeder Institutions and Output Characteristics
# =============================================================================

cat("\nCreating Table 2...\n")

elite_inst_stats <- music_degrees %>%
  filter(unitid %in% ELITE_FEEDER_UNITIDS) %>%
  group_by(unitid, instnm) %>%
  calc_demographic_stats() %>%
  ungroup() %>%
  left_join(counts_all %>% select(institution_key, rank), 
            by = c("unitid" = "institution_key")) %>%
  mutate(National_Pct = Total_Degrees / TOTAL_DEGREES * 100)

table2_data <- elite_inst_stats %>%
  transmute(
    Institution = instnm,
    `Total Degrees` = round(Total_Degrees, 0),
    `% of National Total` = round(National_Pct, 1),
    `% URM` = round(URM_Pct, 2),
    `% NRA` = round(NRA_Pct, 2),
    Rank = round(rank, 0)
  ) %>%
  arrange(is.na(Rank), Rank, desc(`Total Degrees`)) %>%
  add_row(
    Institution = "Elite 7 Total",
    `Total Degrees` = round(stats_elite$Total_Degrees, 0),
    `% of National Total` = round(ELITE_DEGREE_SHARE, 1),
    `% URM` = round(stats_elite$URM_Pct, 2),
    `% NRA` = round(stats_elite$NRA_Pct, 2),
    Rank = NA_real_
  ) %>%
  add_row(
    Institution = "All Other Schools",
    `Total Degrees` = round(stats_nonelite$Total_Degrees, 0),
    `% of National Total` = round(100 - ELITE_DEGREE_SHARE, 1),
    `% URM` = round(stats_nonelite$URM_Pct, 2),
    `% NRA` = round(stats_nonelite$NRA_Pct, 2),
    Rank = NA_real_
  )

table2_notes <- c(
  "Elite feeders identified through demand-side algorithm (see Section 3.3).",
  "URM = Underrepresented Minority (Black, Hispanic, Indigenous, Pacific Islander).",
  "NRA = Nonresident Alien (international students).",
  "Rank based on share of placements across all orchestras in sample.",
  "Percentages for Elite 7 Total and All Other Schools are weighted averages."
)

save_jce_table(table2_data, 2, "Elite Feeder Institutions and Output Characteristics",
               "Table2_Elite_Feeders", table2_notes, digits = 2)

# =============================================================================
# TABLE 3: Scale and Structure of Orchestral Training Pipeline
# =============================================================================

cat("\nCreating Table 3...\n")

calc_period_stats <- function(start_yr, end_yr) {
  music_degrees %>%
    filter(data_year >= start_yr, data_year <= end_yr) %>%
    summarise(
      Period = paste0(start_yr, "-", end_yr),
      `Total Degrees` = sum(ctotalt, na.rm = TRUE),
      `% Ass.` = sum(ctotalt[degree_level == "Associate's"], na.rm = TRUE) / `Total Degrees` * 100,
      `% Bacc.` = sum(ctotalt[degree_level == "Bachelor's"], na.rm = TRUE) / `Total Degrees` * 100,
      `% Mas.` = sum(ctotalt[degree_level == "Master's"], na.rm = TRUE) / `Total Degrees` * 100,
      `% Doc.` = sum(ctotalt[degree_level == "Doctoral"], na.rm = TRUE) / `Total Degrees` * 100,
      `% Prof.` = sum(ctotalt[degree_level == "Professional Studies"], na.rm = TRUE) / `Total Degrees` * 100,
      `% White` = sum(cwhitt, na.rm = TRUE) / `Total Degrees` * 100,
      `% URM` = sum(urm_count, na.rm = TRUE) / `Total Degrees` * 100,
      `% Hispanic` = sum(chispt, na.rm = TRUE) / `Total Degrees` * 100,
      `% Black` = sum(cbkaat, na.rm = TRUE) / `Total Degrees` * 100
    )
}

table3_data <- bind_rows(
  calc_period_stats(2011, 2014),
  calc_period_stats(2015, 2018),
  calc_period_stats(2019, 2022),
  calc_period_stats(2023, 2024),
  calc_period_stats(2011, 2024)
) %>%
  mutate(across(where(is.numeric) & !matches("Total"), ~round(.x, 1)))

table3_notes <- c(
  "Data from IPEDS Completion Survey (2011-2024).",
  "URM = Underrepresented Minority (Black, Hispanic, Indigenous, Pacific Islander).",
  "Associate's (awlevel=3); Bachelor's (awlevel=5); Master's (awlevel=7).",
  "Doctoral includes PhD, DMA, and other doctoral degrees (awlevel=9,10,11,17,18,19).",
  "Professional Studies includes certificates and diplomas (awlevel=1,2,4,6,8,20,21)."
)

save_jce_table(table3_data, 3, "Scale and Structure of Orchestral Training Pipeline (2011-2024)",
               "Table3_Scale_Structure", table3_notes, digits = 1)

# =============================================================================
# TABLE 4: Contingency Table - Elite Feeder Status and URM Status
# =============================================================================

cat("\nCreating Table 4...\n")

table4_data <- as.data.frame.matrix(contingency_2x2) %>%
  tibble::rownames_to_column("Institution Type") %>%
  mutate(Total = URM + `Non-URM`) %>%
  select(`Institution Type`, URM, `Non-URM`, Total)

table4_notes <- c(
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

save_jce_table(table4_data, 4, "Contingency Table: Elite Feeder Status and URM Status",
               "Table4_Contingency", table4_notes, digits = 0)

# =============================================================================
# TABLE 5: Prestige Gradient and Tournament Market Effects
# =============================================================================

cat("\nCreating Table 5...\n")

# Calculate tier statistics
calc_tier_row <- function(tier_name, counts_df, orchestra_filter = NULL) {
  
  # Get total musicians for international %
  if (is.null(orchestra_filter)) {
    tier_musicians <- musicians
  } else {
    tier_musicians <- musicians %>% filter(orchestra %in% orchestra_filter)
  }
  
  n_total <- n_distinct(tier_musicians$m_id)
  n_intl <- tier_musicians %>% filter(is_international == TRUE) %>% distinct(m_id) %>% nrow()
  
  tibble(
    `Orchestra Tier` = tier_name,
    `% Total Elite 7` = round(calc_set_coverage(counts_df, ELITE_FEEDER_UNITIDS) * 100, 1),
    `% from Elite 6` = round(calc_set_coverage(counts_df, ELITE6_UNITIDS) * 100, 1),
    `% from Top Feeder` = round(calc_set_coverage(counts_df, JUILLIARD_UNITID) * 100, 1),
    `% International` = round(n_intl / n_total * 100, 1),
    HHI = round(calc_hhi(counts_df$share), 0)
  )
}

table5_data <- bind_rows(
  calc_tier_row("All Sample Orchestras", counts_all, NULL),
  calc_tier_row("LOA Group 1", counts_g1, group1_orchestras),
  calc_tier_row("Top 8 Orchestras", counts_top8, TOP8_ORCHESTRAS)
) %>%
  mutate(`Gradient Ratio` = round(`% Total Elite 7` / first(`% Total Elite 7`), 2))

table5_notes <- c(
  "Elite 7 = Seven elite feeder institutions; Elite 6 = Elite excluding Juilliard.",
  "Top Feeder = The Juilliard School.",
  "HHI = Herfindahl-Hirschman Index, calculated as Σ(s_i)² × 10,000.",
  "Gradient Ratio = % Total Elite 7 relative to All Sample Orchestras.",
  sprintf("Data based on musician biographies (n = %s).", format(N_MUSICIANS_LINKED, big.mark = ","))
)

save_jce_table(table5_data, 5, "Prestige Gradient and Tournament Market Effects",
               "Table5_Prestige_Gradient", table5_notes, digits = 1)

# =============================================================================
# TABLE 6: Dual Pipeline Analysis (Pooled 2017-2024)
# =============================================================================

cat("\nCreating Table 6...\n")

table6_data <- tibble(
  `Racial/Ethnic Group` = c("Hispanic", "Black", "Native American", "Asian", "White", "All URM"),
  `All Grads (%)` = c(
    pooled_grads$Hispanic_Pct, pooled_grads$Black_Pct,
    music_degrees %>% 
      filter(data_year >= POOLED_WINDOW_START, data_year <= POOLED_WINDOW_END) %>%
      summarise(pct = sum(caiant, na.rm = TRUE) / sum(ctotalt, na.rm = TRUE) * 100) %>% pull(pct),
    pooled_grads$Asian_Pct, pooled_grads$White_Pct, pooled_grads$URM_Pct
  ),
  `Elite Grads (%)` = c(
    pooled_elite_grads$Hispanic_Pct, pooled_elite_grads$Black_Pct,
    music_degrees %>%
      filter(data_year >= POOLED_WINDOW_START, data_year <= POOLED_WINDOW_END,
             unitid %in% ELITE_FEEDER_UNITIDS) %>%
      summarise(pct = sum(caiant, na.rm = TRUE) / sum(ctotalt, na.rm = TRUE) * 100) %>% pull(pct),
    pooled_elite_grads$Asian_Pct, pooled_elite_grads$White_Pct, pooled_elite_grads$URM_Pct
  ),
  `Profession (%)` = c(4.8, 2.4, 0.1, 11.0, 79.1, 7.5),
  `U.S. Population (%)` = c(19.5, 13.6, 1.3, 6.3, 59.3, 34.4)
) %>%
  mutate(
    `Attrition Rate (%)` = if_else(`All Grads (%)` > 0,
                                   (`All Grads (%)` - `Profession (%)`) / `All Grads (%)` * 100,
                                   NA_real_),
    `Gap to Population (pp)` = `Profession (%)` - `U.S. Population (%)`
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 1)))

table6_notes <- c(
  sprintf("All Grads = Orchestral performance graduates, pooled %d–%d (IPEDS).",
          POOLED_WINDOW_START, POOLED_WINDOW_END),
  sprintf("Elite Grads = Graduates from seven elite feeder institutions, pooled %d–%d.",
          POOLED_WINDOW_START, POOLED_WINDOW_END),
  "Profession = Orchestral musicians (League of American Orchestras, 2023).",
  "Attrition Rate = [(All Grads − Profession) / All Grads] × 100.",
  "URM = Hispanic/Latino, Black, American Indian/Alaska Native, Native Hawaiian/Pacific Islander.",
  "Pooled window aligns with active audition pool and Figures 7–8."
)

save_jce_table(table6_data, 6, "Dual Pipeline Analysis: Training vs. Profession Demographics",
               "Table6_Dual_Pipeline", table6_notes, digits = 1)

# =============================================================================
# TABLE 7: International Training Composition
# =============================================================================

cat("\nCreating Table 7...\n")

# Prepare international musician data with regions
prep_intl_data <- function(orchestra_filter = NULL) {
  x <- musicians
  if (!is.null(orchestra_filter)) {
    x <- x %>% filter(orchestra %in% orchestra_filter)
  }
  
  x %>%
    filter(is_international == TRUE, !is.na(m_id)) %>%
    mutate(country_clean = str_squish(as.character(country))) %>%
    filter(!is.na(country_clean), country_clean != "") %>%
    count(m_id, country_clean, name = "n_country") %>%
    arrange(m_id, desc(n_country), country_clean) %>%
    group_by(m_id) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    mutate(region = map_country_to_region(country_clean)) %>%
    select(m_id, country = country_clean, region)
}

intl_all   <- prep_intl_data(NULL)
intl_g1    <- prep_intl_data(group1_orchestras)
intl_top8  <- prep_intl_data(TOP8_ORCHESTRAS)

# Region shares
region_order <- c("Europe", "Asia", "North America (non-U.S.)", "Latin America/Caribbean",
                  "Sub-Saharan Africa", "Oceania")

calc_shares <- function(df, var) {
  total <- nrow(df)
  if (total == 0) return(tibble(Item = character(), pct = numeric()))
  df %>%
    count({{ var }}, name = "n") %>%
    mutate(pct = n / total * 100) %>%
    rename(Item = {{ var }}) %>%
    select(Item, pct)
}

table7_regions <- tibble(Item = region_order) %>%
  left_join(calc_shares(intl_all, region), by = "Item") %>% rename(`All Orchestras (%)` = pct) %>%
  left_join(calc_shares(intl_g1, region), by = "Item") %>% rename(`LOA Group 1 (%)` = pct) %>%
  left_join(calc_shares(intl_top8, region), by = "Item") %>% rename(`Top 8 (%)` = pct) %>%
  mutate(across(where(is.numeric), ~coalesce(.x, 0)),
         `Top 8 - All (pp)` = `Top 8 (%)` - `All Orchestras (%)`,
         Category = "Region") %>%
  select(Category, `Region / Country` = Item, everything())

# Top 10 countries
top_countries <- calc_shares(intl_all, country) %>%
  arrange(desc(pct)) %>%
  slice_head(n = 10) %>%
  pull(Item)

table7_countries <- tibble(Item = top_countries) %>%
  left_join(calc_shares(intl_all, country), by = "Item") %>% rename(`All Orchestras (%)` = pct) %>%
  left_join(calc_shares(intl_g1, country), by = "Item") %>% rename(`LOA Group 1 (%)` = pct) %>%
  left_join(calc_shares(intl_top8, country), by = "Item") %>% rename(`Top 8 (%)` = pct) %>%
  mutate(across(where(is.numeric), ~coalesce(.x, 0)),
         `Top 8 - All (pp)` = `Top 8 (%)` - `All Orchestras (%)`,
         Category = "Country") %>%
  select(Category, `Region / Country` = Item, everything())

table7_data <- bind_rows(table7_regions, table7_countries) %>%
  mutate(across(where(is.numeric), ~round(.x, 1)))

table7_notes <- c(
  "Denominator is distinct musicians (m_id). Each musician counted once.",
  "International training defined as any non-U.S. credential.",
  sprintf("All Orchestras: %d internationally trained musicians (%.1f%% of sample).",
          N_INTL_TRAINED, PCT_INTL_OF_SAMPLE)
)

save_jce_table(table7_data, 7, "International Training Composition: Regional and Country Sources",
               "Table7_International_Composition", table7_notes, digits = 1)

cat("\n  ✓ All 7 tables created\n")
