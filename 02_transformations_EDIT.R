

### Transform IPEDS Data

# Filter to orchestral performance CIP codes
music_degrees <- raw_ipeds |>
  filter(cip_code_clean %in% ORCHESTRAL_CIP_CODES) |>
  mutate(
    # Standardize types
    unitid    = as.character(unitid),
    data_year = as.integer(data_year),
    
    # Numeric counts
    ctotalt = as.numeric(ctotalt),
    ctotalw = as.numeric(ctotalw),
    chispt  = as.numeric(chispt),
    cbkaat  = as.numeric(cbkaat),
    caiant  = as.numeric(caiant),
    cnhpit  = as.numeric(cnhpit),
    casiat  = as.numeric(casiat),
    cwhitt  = as.numeric(cwhitt),
    c2mort  = as.numeric(c2mort),
    cnralt  = as.numeric(cnralt),
    awlevel = as.integer(awlevel),
    
    # Derive degree level from IPEDS award level codes
    # 3=Associate's, 5=Bachelor's, 7=Master's, 9/10/11/17/18/19=Doctoral
    # 1/2/4/6/8/20/21=Professional Studies/Certificates
    degree_level = case_when(
      awlevel == 3 ~ "Associate's",
      awlevel == 5 ~ "Bachelor's",
      awlevel == 7 ~ "Master's",
      awlevel %in% c(9, 10, 11, 17, 18, 19) ~ "Doctoral",
      awlevel %in% c(1, 2, 4, 6, 8, 20, 21) ~ "Professional Studies",
      TRUE ~ "Other"
    ),
    
    # Calculate new demographic counts
    ## urm are Hispanic, Black, American Indian, Native American, Native Hawaiian and Pacific Islander
    ## non-urm are White and Asian
    urm_count = chispt + cbkaat + caiant + cnhpit,
    non_urm_count = ctotalt - urm_count,
    
    # Calculate percentages (with zero-division protection)
    urm_pct      = if_else(ctotalt > 0, urm_count / ctotalt * 100, 0),
    hispanic_pct = if_else(ctotalt > 0, chispt / ctotalt * 100, 0),
    black_pct    = if_else(ctotalt > 0, cbkaat / ctotalt * 100, 0),
    asian_pct    = if_else(ctotalt > 0, casiat / ctotalt * 100, 0),
    white_pct    = if_else(ctotalt > 0, cwhitt / ctotalt * 100, 0),
    nra_pct      = if_else(ctotalt > 0, cnralt / ctotalt * 100, 0)
  )

# Create institution lookup table to refer back to 
ipeds_institutions <- music_degrees |>
  distinct(unitid, instnm) |>
  group_by(unitid) |>
  summarise(instnm = first(instnm), .groups = "drop")


### Transform LOA Orchestra Data

# create new variable for group_num
orchestra_directory = raw_orchestras |>
  mutate(
    orchestra = str_squish(orchestra),
    group = if ("group" %in% names(.)) str_squish(group) else NA_character_,
    group_num = suppressWarnings(as.integer(group))
  ) |>
  distinct(orchestra, group, group_num)

# PB # Identify and sort orchestras by group according to LOA budget guidelines
group1_orchestras <- orchestra_directory |>
  filter(!is.na(group_num) & group_num == 1) |>
  pull(orchestra) |>
  unique()

group2_orchestras <- orchestra_directory %>%
  filter(!is.na(group_num) & group_num == 2) %>%
  pull(orchestra) %>%
  unique()

group3_orchestras <- orchestra_directory %>%
  filter(!is.na(group_num) & group_num == 3) %>%
  pull(orchestra) %>%
  unique()

group123_orchestras <- orchestra_directory %>%
  filter(!is.na(group_num) & group_num %in% c(1, 2, 3)) %>%
  pull(orchestra) %>%
  unique()
print(group123_orchestras)

### Transform Musician Data

# Identify the unitid column name
unitid_col <- case_when(
  "ipeds_unitid" %in% names(raw_musicians) ~ "ipeds_unitid",
  "unitid" %in% names(raw_musicians) ~ "unitid",
  TRUE ~ NA_character_
)
 ## og munoz code
if (is.na(unitid_col)) {
  stop("Musician data must contain 'ipeds_unitid' or 'unitid' column")
}

#
musicians <- raw_musicians %>%
  mutate(
    m_id = as.character(m_id),
    orchestra = str_squish(orchestra),
    institution = if ("institution" %in% names(.)) str_squish(institution) else NA_character_,
    institution_type = if ("institution_type" %in% names(.)) str_squish(institution_type) else NA_character_,
    degree_year = if ("degree_year" %in% names(.)) as.integer(degree_year) else NA_integer_,
    
    # Standardize institution key
    ipeds_unitid = suppressWarnings(as.integer(.data[[unitid_col]])),
    institution_key = as.character(ipeds_unitid),
    
    # Flag international training
    is_international = (institution_type == "International")
  )

# Filter to musicians with valid IPEDS institution links
musicians_linked <- musicians %>%
  filter(!is.na(institution_key) & institution_key != "NA")

cat(sprintf("  ✓ Total musician-degree records: %s\n", format(nrow(musicians), big.mark = ",")))
cat(sprintf("  ✓ Records linked to IPEDS: %s\n", format(nrow(musicians_linked), big.mark = ",")))
cat(sprintf("  ✓ Unique musicians (linked): %s\n", 
            format(n_distinct(musicians_linked$m_id), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# 2.4 Calculate Summary Statistics
# ─────────────────────────────────────────────────────────────────────────────

cat("\nCalculating summary statistics...\n")

# Total orchestral degrees
TOTAL_DEGREES <- sum(music_degrees$ctotalt, na.rm = TRUE)

# Institution counts
N_INSTITUTIONS_ALL <- n_distinct(raw_ipeds$unitid[!is.na(raw_ipeds$unitid)])
N_INSTITUTIONS_ORCHESTRAL <- n_distinct(music_degrees$unitid)

# Musician counts
N_ORCHESTRAS <- n_distinct(orchestra_directory$orchestra)
N_MUSICIANS_ALL <- n_distinct(raw_musicians$m_id[!is.na(raw_musicians$m_id)])
N_MUSICIANS_LINKED <- n_distinct(musicians_linked$m_id)

cat(sprintf("\n  SUMMARY:\n"))
cat(sprintf("  ─────────────────────────────────────────\n"))
cat(sprintf("  Total degrees (2011-2024):    %s\n", format(TOTAL_DEGREES, big.mark = ",")))
cat(sprintf("  Institutions (all music):     %s\n", format(N_INSTITUTIONS_ALL, big.mark = ",")))
cat(sprintf("  Institutions (orchestral):    %s\n", format(N_INSTITUTIONS_ORCHESTRAL, big.mark = ",")))
cat(sprintf("  Orchestras in sample:         %d\n", N_ORCHESTRAS))
cat(sprintf("  Musicians (with school data): %s\n", format(N_MUSICIANS_LINKED, big.mark = ",")))
cat(sprintf("  ─────────────────────────────────────────\n"))
