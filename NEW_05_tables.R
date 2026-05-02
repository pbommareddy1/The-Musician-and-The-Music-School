# =============================================================================
# SECTION 5: TABLES 1-4
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

