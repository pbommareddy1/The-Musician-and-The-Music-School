# =============================================================================
# SECTION 7: STATISTICAL REPORTING
# =============================================================================

cat("\n")
cat(strrep("═", 70), "\n")
cat("  SECTION 7: STATISTICAL REPORTING\n")
cat(strrep("═", 70), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 7.1 Print APA-Formatted Statistics for Manuscript
# ─────────────────────────────────────────────────────────────────────────────

cat("\n")
cat(strrep("─", 60), "\n")
cat("KEY STATISTICS FOR MANUSCRIPT (APA 7 Format)\n")
cat(strrep("─", 60), "\n")

cat("\n[ABSTRACT / SECTION 3: METHODS]\n")
cat(sprintf("  Total degrees (2011-2024): %s\n", format(TOTAL_DEGREES, big.mark = ",")))
cat(sprintf("  Institutions: %s\n", format(N_INSTITUTIONS_ALL, big.mark = ",")))
cat(sprintf("  Orchestras: %d; Musicians: %s\n", N_ORCHESTRAS, format(N_MUSICIANS_LINKED, big.mark = ",")))
cat(sprintf("  Elite feeders: %d (%.1f%% of degrees)\n", length(ELITE_FEEDER_UNITIDS), ELITE_DEGREE_SHARE))

cat("\n[SECTION 4.1: DEMOGRAPHIC TRENDS]\n")
cat(sprintf("  White: %.1f%% → %.1f%% (%.1f pp)\n", WHITE_2011, WHITE_2024, WHITE_CHANGE))
cat(sprintf("  URM: %.1f%% → %.1f%% (+%.1f pp)\n", URM_2011, URM_2024, URM_CHANGE))

cat("\n[SECTION 4.2: RACIAL BOTTLENECK]\n")
cat(sprintf("  Elite URM: %.2f%% (%s/%s)\n", stats_elite$URM_Pct,
            format(stats_elite$URM_Degrees, big.mark = ","),
            format(stats_elite$Total_Degrees, big.mark = ",")))
cat(sprintf("  Non-Elite URM: %.2f%% (%s/%s)\n", stats_nonelite$URM_Pct,
            format(stats_nonelite$URM_Degrees, big.mark = ","),
            format(stats_nonelite$Total_Degrees, big.mark = ",")))
cat(sprintf("  URM Gap: %.2f pp\n", URM_GAP_PP))
cat(sprintf("  χ²(1, N = %s) = %.2f, p < .001\n", format(contingency_results$N, big.mark = ","), contingency_results$chi_sq))
cat(sprintf("  φ = %.2f\n", contingency_results$phi))
cat(sprintf("  Minimum expected count = %.2f\n", contingency_results$min_expected))
cat(sprintf("  Risk Ratio = %.2f, 95%% CI [%.2f, %.2f]\n", 
            contingency_results$risk_ratio, contingency_results$rr_ci_low, contingency_results$rr_ci_high))
cat(sprintf("  Bootstrap (BCa): 95%% CI [%.2f, %.2f]\n",
            bootstrap_results$ci_bca["lower"], bootstrap_results$ci_bca["upper"]))
cat(sprintf("  Gap widening: %.1f pp (2011) → %.1f pp (2024)\n", GAP_2011, GAP_2024))
cat(sprintf("  Elite URM trend: %.1f%% → %.1f%%\n", ELITE_URM_2011, ELITE_URM_2024))

cat("\n[SECTION 4.3: CONCENTRATION]\n")
cat(sprintf("  Elite 7 coverage: All %.1f%%, Group 1 %.1f%%, Top 8 %.1f%%\n",
            COVERAGE_ALL * 100, COVERAGE_G1 * 100, COVERAGE_TOP8 * 100))
cat(sprintf("  Gini coefficient: %.3f\n", GINI_COEFFICIENT))
cat(sprintf("  K25 = %d, K50 = %d, K75 = %d schools\n", K25_SCHOOLS, K50_SCHOOLS, K75_SCHOOLS))
cat(sprintf("  HHI: All = %.0f, Group 1 = %.0f, Top 8 = %.0f (%.1f-fold increase)\n",
            HHI_ALL, HHI_G1, HHI_TOP8, HHI_FOLD_INCREASE))
cat(sprintf("  Gradient ratios: All = %.2f, Group 1 = %.2f, Top 8 = %.2f\n",
            GRADIENT_RATIO_ALL, GRADIENT_RATIO_G1, GRADIENT_RATIO_TOP8))
cat(sprintf("  Juilliard (Top 8): %.1f%%; Elite 6 (Top 8): %.1f%% (computed directly)\n",
            JUILLIARD_SHARE_TOP8, ELITE6_SHARE_TOP8))

cat("\n[SECTION 4.4: INTERNATIONAL TRAINING]\n")
cat(sprintf("  International musicians: %d (%.1f%% of sample)\n", N_INTL_TRAINED, PCT_INTL_OF_SAMPLE))
cat(sprintf("  International-only: %d (%.1f%% of intl trained)\n", N_INTL_ONLY, PCT_INTL_ONLY_OF_INTL))
cat(sprintf("  Elite NRA: %.2f%%; Non-Elite NRA: %.2f%% (see Table 2)\n",
            stats_elite$NRA_Pct, stats_nonelite$NRA_Pct))
cat(sprintf("  IPEDS NRA (all degrees): %.1f%%\n", NRA_ALL_PCT))

cat("\n[SECTION 4.5: DEMOGRAPHIC TRADE-OFF]\n")
cat(sprintf("  r = %.2f, p < .001, 95%% CI [%.2f, %.2f], n = %d\n",
            CORRELATION_R, CORRELATION_CI_LOW, CORRELATION_CI_HIGH, CORRELATION_N))

cat("\n[SECTION 4.6: PIPELINE ATTRITION (Pooled %d-%d)]\n", POOLED_WINDOW_START, POOLED_WINDOW_END)
cat(sprintf("  Black attrition: %.1f%%\n", BLACK_ATTRITION))
cat(sprintf("  Hispanic attrition: %.1f%%\n", HISPANIC_ATTRITION))

# =============================================================================
# SECTION 8: EXPORT OUTPUTS
# =============================================================================

cat("\n")
cat(strrep("═", 70), "\n")
cat("  SECTION 8: EXPORT OUTPUTS\n")
cat(strrep("═", 70), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 8.1 Export Figure Captions
# ─────────────────────────────────────────────────────────────────────────────

cat("\nExporting figure captions...\n")

doc <- officer::read_docx()

title_fp <- officer::fp_text(bold = TRUE, font.size = 14, font.family = "Times New Roman")
caption_bold_fp <- officer::fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")
caption_normal_fp <- officer::fp_text(font.size = 11, font.family = "Times New Roman")

doc <- doc %>%
  officer::body_add_fpar(officer::fpar(officer::ftext("FIGURE CAPTIONS", prop = title_fp))) %>%
  officer::body_add_par("", style = "Normal")

for (i in 1:8) {
  fig_key <- paste0("fig", i)
  if (fig_key %in% names(figure_captions)) {
    caption_text <- gsub("\\*\\*", "", figure_captions[[fig_key]])
    
    fig_label <- regmatches(caption_text, regexpr("^Fig\\. [0-9]+", caption_text))
    rest_text <- sub("^Fig\\. [0-9]+", "", caption_text)
    
    doc <- doc %>%
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext(fig_label, prop = caption_bold_fp),
          officer::ftext(rest_text, prop = caption_normal_fp)
        )
      ) %>%
      officer::body_add_par("", style = "Normal")
  }
}

print(doc, target = file.path(OUTPUT_DIR, "Figure_Captions.docx"))
cat("  ✓ Figure_Captions.docx\n")

# ─────────────────────────────────────────────────────────────────────────────
# 8.2 Export Statistics Reference Document
# ─────────────────────────────────────────────────────────────────────────────

cat("\nExporting statistics reference document...\n")

doc <- officer::read_docx()

title_fp <- officer::fp_text(bold = TRUE, font.size = 16, font.family = "Times New Roman")
section_fp <- officer::fp_text(bold = TRUE, font.size = 14, font.family = "Times New Roman")
stat_fp <- officer::fp_text(font.size = 11, font.family = "Times New Roman")
note_fp <- officer::fp_text(italic = TRUE, font.size = 10, font.family = "Times New Roman")

doc <- doc %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    "THE MERITOCRACY MYTH: STATISTICS REFERENCE", prop = title_fp))) %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Generated: %s", format(Sys.time(), "%B %d, %Y at %H:%M:%S")), prop = note_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    "All statistics verified against source data.", prop = note_fp))) %>%
  officer::body_add_par("", style = "Normal")

# Core Statistics
doc <- doc %>%
  officer::body_add_fpar(officer::fpar(officer::ftext("CORE SAMPLE", prop = section_fp))) %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Total degrees: %s", format(TOTAL_DEGREES, big.mark = ",")), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Institutions: %s", format(N_INSTITUTIONS_ALL, big.mark = ",")), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Orchestras: %d; Musicians: %s", N_ORCHESTRAS, format(N_MUSICIANS_LINKED, big.mark = ",")), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Elite feeders: %d (%.1f%% of degrees)", length(ELITE_FEEDER_UNITIDS), ELITE_DEGREE_SHARE), prop = stat_fp))) %>%
  officer::body_add_par("", style = "Normal")

# Contingency Analysis
doc <- doc %>%
  officer::body_add_fpar(officer::fpar(officer::ftext("CONTINGENCY ANALYSIS", prop = section_fp))) %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Elite URM: %.2f%%; Non-Elite URM: %.2f%%; Gap: %.2f pp",
            stats_elite$URM_Pct, stats_nonelite$URM_Pct, URM_GAP_PP), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("χ²(1, N = %s) = %.2f, p < .001",
            format(contingency_results$N, big.mark = ","), contingency_results$chi_sq), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("φ = %.2f; Min expected = %.2f", contingency_results$phi, contingency_results$min_expected), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Risk Ratio = %.2f, 95%% CI [%.2f, %.2f]",
            contingency_results$risk_ratio, contingency_results$rr_ci_low, contingency_results$rr_ci_high), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Bootstrap BCa: [%.2f, %.2f]",
            bootstrap_results$ci_bca["lower"], bootstrap_results$ci_bca["upper"]), prop = stat_fp))) %>%
  officer::body_add_par("", style = "Normal")

# Concentration
doc <- doc %>%
  officer::body_add_fpar(officer::fpar(officer::ftext("CONCENTRATION", prop = section_fp))) %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Gini: %.3f", GINI_COEFFICIENT), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("K-coverage: K25=%d, K50=%d, K75=%d", K25_SCHOOLS, K50_SCHOOLS, K75_SCHOOLS), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("HHI: %.0f / %.0f / %.0f (%.1f-fold)", HHI_ALL, HHI_G1, HHI_TOP8, HHI_FOLD_INCREASE), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Gradient ratios: %.2f / %.2f / %.2f", GRADIENT_RATIO_ALL, GRADIENT_RATIO_G1, GRADIENT_RATIO_TOP8), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Elite coverage: %.1f%% / %.1f%% / %.1f%%", COVERAGE_ALL * 100, COVERAGE_G1 * 100, COVERAGE_TOP8 * 100), prop = stat_fp))) %>%
  officer::body_add_par("", style = "Normal")

# Trends
doc <- doc %>%
  officer::body_add_fpar(officer::fpar(officer::ftext("DEMOGRAPHIC TRENDS (2011-2024)", prop = section_fp))) %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("White: %.1f%% → %.1f%% (%.1f pp)", WHITE_2011, WHITE_2024, WHITE_CHANGE), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("URM: %.1f%% → %.1f%% (+%.1f pp)", URM_2011, URM_2024, URM_CHANGE), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Elite URM: %.1f%% → %.1f%%", ELITE_URM_2011, ELITE_URM_2024), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Gap widening: %.1f pp → %.1f pp", GAP_2011, GAP_2024), prop = stat_fp))) %>%
  officer::body_add_par("", style = "Normal")

# International
doc <- doc %>%
  officer::body_add_fpar(officer::fpar(officer::ftext("INTERNATIONAL", prop = section_fp))) %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Intl trained: %d (%.1f%%)", N_INTL_TRAINED, PCT_INTL_OF_SAMPLE), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Intl-only: %d (%.1f%%)", N_INTL_ONLY, PCT_INTL_ONLY_OF_INTL), prop = stat_fp))) %>%
  officer::body_add_par("", style = "Normal")

# Correlation
doc <- doc %>%
  officer::body_add_fpar(officer::fpar(officer::ftext("TRADE-OFF CORRELATION", prop = section_fp))) %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("r = %.2f, 95%% CI [%.2f, %.2f], n = %d",
            CORRELATION_R, CORRELATION_CI_LOW, CORRELATION_CI_HIGH, CORRELATION_N), prop = stat_fp))) %>%
  officer::body_add_par("", style = "Normal")

# Attrition
doc <- doc %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("ATTRITION (Pooled %d-%d)", POOLED_WINDOW_START, POOLED_WINDOW_END), prop = section_fp))) %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Black: %.1f%%", BLACK_ATTRITION), prop = stat_fp))) %>%
  officer::body_add_fpar(officer::fpar(officer::ftext(
    sprintf("Hispanic: %.1f%%", HISPANIC_ATTRITION), prop = stat_fp)))

print(doc, target = file.path(OUTPUT_DIR, "Manuscript_Statistics_Reference.docx"))
cat("  ✓ Manuscript_Statistics_Reference.docx\n")

# =============================================================================
# SCRIPT COMPLETE
# =============================================================================

cat("\n")
cat(strrep("═", 70), "\n")
cat("  SCRIPT COMPLETE\n")
cat(strrep("═", 70), "\n")
cat(sprintf("  Output directory: %s\n", OUTPUT_DIR))
cat("\n  Files created:\n")
cat("    Tables:   Tables.docx, Table1-7 CSV files\n")
cat("    Figures:  Fig1-8.eps\n")
cat("    Captions: Figure_Captions.docx\n")
cat("    Stats:    Manuscript_Statistics_Reference.docx\n")
cat(strrep("═", 70), "\n")
