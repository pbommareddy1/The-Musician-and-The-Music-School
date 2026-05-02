# =============================================================================
# SECTION 6: FIGURES 1-8
# =============================================================================

cat("\n")
cat(strrep("═", 70), "\n")
cat("  SECTION 6: FIGURES\n")
cat(strrep("═", 70), "\n")

# Initialize figure captions collection
figure_captions <- list()

# =============================================================================
# FIGURE 1: Demographic Shifts in Orchestral Training 2011-2024
# =============================================================================

cat("\nCreating Figure 1...\n")

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

cat("\nCreating Figure 3...\n")

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

fig4_data <- gap_trends %>%
  select(data_year, National_URM_Pct = URM_Pct, Elite_URM_Pct, Gap_PP) %>%
  filter(!is.na(Elite_URM_Pct))

fig4_long <- fig4_data %>%
  pivot_longer(c(National_URM_Pct, Elite_URM_Pct), names_to = "Group", values_to = "URM_Pct") %>%
  mutate(Group = factor(Group, levels = c("National_URM_Pct", "Elite_URM_Pct"),
                        labels = c("All Orchestral Graduates", "Elite Feeder Graduates")))

mid_year <- fig4_data$data_year[ceiling(nrow(fig4_data) / 2)]
mid_y <- fig4_data %>% filter(data_year == mid_year) %>%
  summarise(y = (National_URM_Pct + Elite_URM_Pct) / 2) %>% pull(y)
gap_label <- sprintf("Gap: %.1f pp to %.1f pp\nWidened by %.1f pp", GAP_2011, GAP_2024, GAP_2024 - GAP_2011)

fig4 <- ggplot(fig4_long, aes(x = data_year, y = URM_Pct, color = Group, linetype = Group, shape = Group)) +
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

figure_captions$fig4 <- sprintf(
  "**Fig. 4** Diverging pathways: Gap widened from %.1f pp (2011) to %.1f pp (2024).",
  GAP_2011, GAP_2024
)

save_jce_figure(fig4, "Fig4_Diverging_Pathways")



# =============================================================================
# FIGURE 4: Demographic Trade-Off
# =============================================================================

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
# FIGURE 5: Stepwise Pipeline
# =============================================================================

cat("\nCreating Figure 7...\n")

fig7_data <- tibble(
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

fig7 <- ggplot(fig7_data, aes(x = Stage, y = Percentage, fill = Group, pattern = Group)) +
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

save_jce_figure(fig7, "Fig7_Stepwise_Pipeline")

# =============================================================================
# FIGURE 6: Complete Pipeline
# =============================================================================

cat("\nCreating Figure 8...\n")

other_all = 100 - (pooled_grads$White_Pct + pooled_grads$Hispanic_Pct + 
                    pooled_grads$Black_Pct + pooled_grads$Asian_Pct)
other_elite = 100 - (pooled_elite_grads$White_Pct + pooled_elite_grads$Hispanic_Pct +
                      pooled_elite_grads$Black_Pct + pooled_elite_grads$Asian_Pct)

fig8_data = tibble(
  Stage = factor(c("U.S. Adult Population", "All Graduates", "Elite Graduates", "Professional Musicians"),
                 levels = c("U.S. Adult Population", "All Graduates", "Elite Graduates", "Professional Musicians")),
  White = c(US_POP_2024$White, pooled_grads$White_Pct, pooled_elite_grads$White_Pct, LAO_2023_BENCHMARKS$White),
  Hispanic = c(US_POP_2024$Hispanic, pooled_grads$Hispanic_Pct, pooled_elite_grads$Hispanic_Pct, LAO_2023_BENCHMARKS$Hispanic),
  Black = c(US_POP_2024$Black, pooled_grads$Black_Pct, pooled_elite_grads$Black_Pct, LAO_2023_BENCHMARKS$Black),
  Asian = c(US_POP_2024$Asian, pooled_grads$Asian_Pct, pooled_elite_grads$Asian_Pct, LAO_2023_BENCHMARKS$Asian),
  Other = c(100 - 59.3 - 19.5 - 13.6 - 6.3, other_all, other_elite, 100 - 79.1 - 4.8 - 2.4 - 11.0)
)

fig8_long = fig8_data |>
  pivot_longer(-Stage, names_to = "Group", values_to = "Percentage") |>
  mutate(Group = factor(Group, levels = c("Asian", "Black", "Hispanic", "Other", "White")),
         Label = if_else(Percentage >= 2, sprintf("%.1f%%", Percentage), ""))

stage_labels = tibble(
  Stage = fig8_data$Stage,
  label = c("U.S. ADULT\nPOPULATION", "ALL\nGRADUATES", "ELITE\nGRADUATES", "PROFESSIONAL\nMUSICIANS")
)

fig8 <- ggplot(fig8_long, aes(x = Stage, y = Percentage, fill = Group, pattern = Group)) +
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

save_jce_figure(fig8, "PB_Fig8_Complete_Pipeline")

cat("\n  ✓ All 8 figures created\n")
