# ============================================================
# 02_clean_data.R
# Texas Chronic Disease Risk Factor Analysis
# ============================================================

library(tidyverse)
library(janitor)

# Add code below
# ============================================================
# 02_build_texas_health_panel.R
# Texas Chronic Disease Risk Factor Analysis
# Build Master Texas County Health Panel
# ============================================================

library(tidyverse)
library(janitor)
library(readr)
library(stringr)

# ------------------------------------------------------------
# 1. Set project directories
# ------------------------------------------------------------

project_dir <- "C:/Users/vonne/OneDrive/Desktop/HYDROCODEX/texas_chronic_disease_analysis"

raw_dir <- file.path(project_dir, "data_raw")
clean_dir <- file.path(project_dir, "data_clean")
processed_dir <- file.path(project_dir, "data_processed")

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. Load CDC PLACES dataset
# ------------------------------------------------------------

places <- read_csv(
  file.path(
    raw_dir,
    "PLACES__Local_Data_for_Better_Health,_County_Data,_2025_release_20260513.csv"
  ),
  show_col_types = FALSE
) |> 
  clean_names()

glimpse(places)

# ------------------------------------------------------------
# 3. Filter to Texas counties
# ------------------------------------------------------------

tx_places <- places |> 
  filter(state_abbr == "TX") |> 
  mutate(
    county_fips = str_pad(as.character(location_id), width = 5, pad = "0"),
    county = location_name
  )

# ------------------------------------------------------------
# 4. Build key health measure dataset
# ------------------------------------------------------------

tx_places_wide <- tx_places |> 
  mutate(
    measure_clean = case_when(
      str_detect(str_to_lower(measure), "diabetes") ~ "diabetes",
      str_detect(str_to_lower(measure), "obesity") ~ "obesity",
      str_detect(str_to_lower(measure), "smoking") ~ "smoking",
      str_detect(str_to_lower(measure), "physical inactivity") ~ "physical_inactivity",
      str_detect(str_to_lower(measure), "blood pressure") ~ "high_blood_pressure",
      str_detect(str_to_lower(measure), "coronary heart disease") ~ "coronary_heart_disease",
      str_detect(str_to_lower(measure), "kidney disease") ~ "chronic_kidney_disease",
      str_detect(str_to_lower(measure), "health insurance") ~ "uninsured",
      TRUE ~ NA_character_
    )
  ) |> 
  filter(!is.na(measure_clean)) |> 
  group_by(
    county_fips,
    county,
    total_population,
    total_pop18plus,
    measure_clean
  ) |> 
  summarise(
    data_value = mean(data_value, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  pivot_wider(
    names_from = measure_clean,
    values_from = data_value
  )

# ------------------------------------------------------------
# 5. Save cleaned PLACES dataset
# ------------------------------------------------------------

write_csv(
  tx_places_wide,
  file.path(clean_dir, "tx_places_chronic_disease_wide.csv")
)

glimpse(tx_places_wide)

# ------------------------------------------------------------
# 6. Load SVI county dataset
# ------------------------------------------------------------

svi <- read_csv(
  file.path(raw_dir, "SVI_2022_US_county.csv"),
  show_col_types = FALSE
) |> 
  clean_names()

glimpse(svi)

# ------------------------------------------------------------
# 7. Filter SVI to Texas
# ------------------------------------------------------------

tx_svi <- svi |> 
  filter(st_abbr == "TX") |> 
  mutate(
    county_fips = str_pad(as.character(fips), width = 5, pad = "0")
  ) |> 
  select(
    county_fips,
    svi_county = county,
    svi_location = location,
    
    # Overall SVI
    rpl_themes,
    
    # Theme rankings
    rpl_theme1,
    rpl_theme2,
    rpl_theme3,
    rpl_theme4,
    
    # Key socioeconomic vulnerability variables
    ep_pov150,
    ep_unemp,
    ep_nohsdp,
    ep_uninsur,
    ep_age65,
    ep_disabl,
    ep_minrty,
    ep_limeng,
    ep_noveh,
    ep_groupq
  )

glimpse(tx_svi)

# ------------------------------------------------------------
# 8. Save cleaned SVI dataset
# ------------------------------------------------------------

write_csv(
  tx_svi,
  file.path(clean_dir, "tx_svi_selected.csv")
)

# ------------------------------------------------------------
# 9. Merge PLACES + SVI
# ------------------------------------------------------------

tx_health_panel <- tx_places_wide |> 
  left_join(tx_svi, by = "county_fips")

# ------------------------------------------------------------
# 10. Merge diagnostics
# ------------------------------------------------------------

merge_check <- tx_health_panel |> 
  summarise(
    counties = n(),
    missing_svi = sum(is.na(rpl_themes)),
    missing_diabetes = sum(is.na(diabetes)),
    missing_obesity = sum(is.na(obesity)),
    missing_smoking = sum(is.na(smoking)),
    missing_uninsured = sum(is.na(uninsured))
  )

print(merge_check)

# ------------------------------------------------------------
# 11. Save final health panel
# ------------------------------------------------------------

write_csv(
  tx_health_panel,
  file.path(processed_dir, "tx_health_panel_places_svi.csv")
)

# ------------------------------------------------------------
# 12. Load mortality trend datasets
# ------------------------------------------------------------

diabetes_death <- read_csv(
  file.path(raw_dir, "Diabetes death bar chart.csv"),
  show_col_types = FALSE
) |> 
  clean_names() |> 
  rename(
    diabetes_mortality_rate = mortality_rates
  )

kidney_death <- read_csv(
  file.path(raw_dir, "Kidney disease death bar chart.csv"),
  show_col_types = FALSE
) |> 
  clean_names() |> 
  rename(
    kidney_mortality_rate = mortality_rates
  )

# ------------------------------------------------------------
# 13. Merge mortality trend datasets
# ------------------------------------------------------------

mortality_trends <- diabetes_death |> 
  full_join(kidney_death, by = "year")

# ------------------------------------------------------------
# 14. Save mortality trends
# ------------------------------------------------------------

write_csv(
  mortality_trends,
  file.path(clean_dir, "tx_mortality_trends.csv")
)

# ------------------------------------------------------------
# 15. Summary statistics
# ------------------------------------------------------------

summary_table <- tx_health_panel |> 
  summarise(
    counties = n(),
    avg_diabetes = mean(diabetes, na.rm = TRUE),
    avg_obesity = mean(obesity, na.rm = TRUE),
    avg_smoking = mean(smoking, na.rm = TRUE),
    avg_high_blood_pressure = mean(high_blood_pressure, na.rm = TRUE),
    avg_uninsured = mean(uninsured, na.rm = TRUE),
    avg_coronary_heart_disease = mean(coronary_heart_disease, na.rm = TRUE),
    avg_svi = mean(rpl_themes, na.rm = TRUE)
  )

print(summary_table)

write_csv(
  summary_table,
  file.path(processed_dir, "summary_table.csv")
)

# ------------------------------------------------------------
# 16. Top diabetes prevalence counties
# ------------------------------------------------------------

top_diabetes_counties <- tx_health_panel |> 
  arrange(desc(diabetes)) |> 
  select(
    county,
    county_fips,
    diabetes,
    obesity,
    high_blood_pressure,
    coronary_heart_disease,
    smoking,
    uninsured,
    rpl_themes
  ) |> 
  slice_head(n = 15)

print(top_diabetes_counties)

write_csv(
  top_diabetes_counties,
  file.path(processed_dir, "top_diabetes_counties.csv")
)

# ------------------------------------------------------------
# 17. Completion message
# ------------------------------------------------------------

message("Texas health panel successfully built.")
message("Saved to:")
message("data_processed/tx_health_panel_places_svi.csv")