# ============================================================
# 03_eda.R
# Texas Chronic Disease Risk Factor Analysis
# ============================================================

library(tidyverse)
library(janitor)

# Add code below
# ============================================================
# 03_eda_visualizations.R
# Texas Chronic Disease Risk Factor Analysis
# EDA and Portfolio Visuals
# ============================================================

library(tidyverse)
library(janitor)
library(readr)
library(ggplot2)
library(scales)

project_dir <- "C:/Users/vonne/OneDrive/Desktop/HYDROCODEX/texas_chronic_disease_analysis"

processed_dir <- file.path(project_dir, "data_processed")
figures_dir <- file.path(project_dir, "figures")
tables_dir <- file.path(project_dir, "tables")

dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

tx_health_panel <- read_csv(
  file.path(processed_dir, "tx_health_panel_places_svi.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 1. Summary statistics
# ------------------------------------------------------------

summary_stats <- tx_health_panel |> 
  summarise(
    counties = n(),
    avg_diabetes = mean(diabetes, na.rm = TRUE),
    avg_obesity = mean(obesity, na.rm = TRUE),
    avg_smoking = mean(smoking, na.rm = TRUE),
    avg_high_blood_pressure = mean(high_blood_pressure, na.rm = TRUE),
    avg_uninsured = mean(uninsured, na.rm = TRUE),
    avg_svi = mean(rpl_themes, na.rm = TRUE)
  )

write_csv(summary_stats, file.path(tables_dir, "summary_stats.csv"))

print(summary_stats)

# ------------------------------------------------------------
# 2. Top 15 counties by diabetes prevalence
# ------------------------------------------------------------

top_diabetes <- tx_health_panel |> 
  arrange(desc(diabetes)) |> 
  slice_head(n = 15) |> 
  mutate(county = fct_reorder(county, diabetes))

p_top_diabetes <- ggplot(top_diabetes, aes(x = county, y = diabetes)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 Texas Counties by Diabetes Prevalence",
    x = NULL,
    y = "Diabetes prevalence (%)",
    caption = "Source: CDC PLACES 2025 release; SVI 2022"
  ) +
  theme_minimal(base_size = 13)

print(p_top_diabetes)

ggsave(
  file.path(figures_dir, "top_15_diabetes_counties.png"),
  p_top_diabetes,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 3. Obesity vs diabetes scatterplot
# ------------------------------------------------------------

p_obesity_diabetes <- ggplot(
  tx_health_panel,
  aes(x = obesity, y = diabetes)
) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Obesity and Diabetes Prevalence Across Texas Counties",
    x = "Obesity prevalence (%)",
    y = "Diabetes prevalence (%)",
    caption = "Source: CDC PLACES 2025 release"
  ) +
  theme_minimal(base_size = 13)

print(p_obesity_diabetes)

ggsave(
  file.path(figures_dir, "obesity_vs_diabetes.png"),
  p_obesity_diabetes,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 4. Uninsured vs diabetes scatterplot
# ------------------------------------------------------------

p_uninsured_diabetes <- ggplot(
  tx_health_panel,
  aes(x = uninsured, y = diabetes)
) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Uninsurance and Diabetes Prevalence Across Texas Counties",
    x = "Uninsured adults (%)",
    y = "Diabetes prevalence (%)",
    caption = "Source: CDC PLACES 2025 release"
  ) +
  theme_minimal(base_size = 13)

print(p_uninsured_diabetes)

ggsave(
  file.path(figures_dir, "uninsured_vs_diabetes.png"),
  p_uninsured_diabetes,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 5. SVI vs diabetes scatterplot
# ------------------------------------------------------------

p_svi_diabetes <- ggplot(
  tx_health_panel,
  aes(x = rpl_themes, y = diabetes)
) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Social Vulnerability and Diabetes Prevalence Across Texas Counties",
    x = "Overall Social Vulnerability Index percentile",
    y = "Diabetes prevalence (%)",
    caption = "Source: CDC PLACES 2025 release; CDC/ATSDR SVI 2022"
  ) +
  theme_minimal(base_size = 13)

print(p_svi_diabetes)

ggsave(
  file.path(figures_dir, "svi_vs_diabetes.png"),
  p_svi_diabetes,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 6. Correlation table
# ------------------------------------------------------------

cor_data <- tx_health_panel |> 
  select(
    diabetes,
    obesity,
    smoking,
    high_blood_pressure,
    coronary_heart_disease,
    uninsured,
    rpl_themes,
    ep_pov150,
    ep_unemp,
    ep_nohsdp,
    ep_uninsur
  )

cor_table <- cor(cor_data, use = "pairwise.complete.obs")

print(round(cor_table, 2))

write_csv(
  as.data.frame(round(cor_table, 2)) |> 
    rownames_to_column("variable"),
  file.path(tables_dir, "correlation_table.csv")
)

# ------------------------------------------------------------
# 7. Simple regression model
# ------------------------------------------------------------

m_diabetes <- lm(
  diabetes ~ obesity + smoking + uninsured + rpl_themes,
  data = tx_health_panel
)

summary(m_diabetes)

capture.output(
  summary(m_diabetes),
  file = file.path(tables_dir, "diabetes_regression_model.txt")
)

message("EDA visuals and tables saved successfully.")













