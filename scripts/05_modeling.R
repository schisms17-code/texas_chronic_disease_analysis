# ============================================================
# statistical_modeling.R
# Texas Chronic Disease Risk Factor Analysis
# Statistical Modeling
# ============================================================

library(tidyverse)
library(readr)
library(janitor)
library(modelsummary)
library(broom)

# ------------------------------------------------------------
# 1. Project directories
# ------------------------------------------------------------

project_dir <- "C:/Users/vonne/OneDrive/Desktop/HYDROCODEX/texas_chronic_disease_analysis"

processed_dir <- file.path(project_dir, "data_processed")
tables_dir <- file.path(project_dir, "tables")
figures_dir <- file.path(project_dir, "figures")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. Load master health panel
# ------------------------------------------------------------

tx_health_panel <- read_csv(
  file.path(
    processed_dir,
    "tx_health_panel_places_svi.csv"
  ),
  show_col_types = FALSE
)

glimpse(tx_health_panel)

# ------------------------------------------------------------
# 3. Correlation matrix
# ------------------------------------------------------------

cor_vars <- tx_health_panel |> 
  select(
    diabetes,
    obesity,
    smoking,
    uninsured,
    high_blood_pressure,
    coronary_heart_disease,
    rpl_themes,
    ep_pov150,
    ep_unemp,
    ep_nohsdp
  )

cor_matrix <- cor(
  cor_vars,
  use = "pairwise.complete.obs"
)

print(round(cor_matrix, 2))

write_csv(
  as.data.frame(round(cor_matrix, 2)) |> 
    rownames_to_column("variable"),
  file.path(
    tables_dir,
    "correlation_matrix.csv"
  )
)

# ------------------------------------------------------------
# 4. Regression Models
# ------------------------------------------------------------

# Model 1
m1 <- lm(
  diabetes ~ obesity,
  data = tx_health_panel
)

# Model 2
m2 <- lm(
  diabetes ~ obesity + smoking + uninsured,
  data = tx_health_panel
)

# Model 3
m3 <- lm(
  diabetes ~ obesity + smoking + uninsured + rpl_themes,
  data = tx_health_panel
)

# Model 4
m4 <- lm(
  diabetes ~ obesity +
    smoking +
    uninsured +
    ep_pov150 +
    ep_unemp +
    ep_nohsdp,
  data = tx_health_panel
)

# ------------------------------------------------------------
# 5. Print model summaries
# ------------------------------------------------------------

summary(m1)
summary(m2)
summary(m3)
summary(m4)

# ------------------------------------------------------------
# 6. Export regression comparison table
# ------------------------------------------------------------

modelsummary(
  list(
    "Model 1" = m1,
    "Model 2" = m2,
    "Model 3" = m3,
    "Model 4" = m4
  ),
  output = file.path(
    tables_dir,
    "diabetes_regression_models.html"
  )
)

# ------------------------------------------------------------
# 7. Export tidy Model 4 coefficients
# ------------------------------------------------------------

tidy_m4 <- tidy(m4)

print(tidy_m4)

write_csv(
  tidy_m4,
  file.path(
    tables_dir,
    "model4_coefficients.csv"
  )
)

# ------------------------------------------------------------
# 8. Create modeling dataset
# Removes missing observations before prediction
# ------------------------------------------------------------

model_data <- tx_health_panel |> 
  select(
    county,
    county_fips,
    diabetes,
    obesity,
    smoking,
    uninsured,
    high_blood_pressure,
    coronary_heart_disease,
    rpl_themes,
    ep_pov150,
    ep_unemp,
    ep_nohsdp
  ) |> 
  drop_na(
    diabetes,
    obesity,
    smoking,
    uninsured,
    ep_pov150,
    ep_unemp,
    ep_nohsdp
  )

# ------------------------------------------------------------
# 9. Generate predictions
# ------------------------------------------------------------

model_data$predicted_diabetes <- predict(
  m4,
  newdata = model_data
)

# ------------------------------------------------------------
# 10. Residuals
# ------------------------------------------------------------

model_data$residual <- 
  model_data$diabetes - model_data$predicted_diabetes

glimpse(model_data)

write_csv(
  model_data,
  file.path(
    tables_dir,
    "diabetes_model_data_with_predictions.csv"
  )
)

# ------------------------------------------------------------
# 11. Predicted vs observed plot
# ------------------------------------------------------------

p_predicted <- ggplot(
  model_data,
  aes(
    x = predicted_diabetes,
    y = diabetes
  )
) +
  geom_point(alpha = 0.75) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Predicted vs Observed Diabetes Prevalence",
    x = "Predicted diabetes prevalence (%)",
    y = "Observed diabetes prevalence (%)",
    caption = "Source: CDC PLACES 2025 release; CDC/ATSDR SVI 2022"
  ) +
  theme_minimal(base_size = 13)

print(p_predicted)

ggsave(
  filename = file.path(
    figures_dir,
    "predicted_vs_observed_diabetes.png"
  ),
  plot = p_predicted,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 12. Counties with largest residuals
# ------------------------------------------------------------

high_residual_counties <- model_data |> 
  arrange(desc(abs(residual))) |> 
  select(
    county,
    county_fips,
    diabetes,
    predicted_diabetes,
    residual,
    obesity,
    uninsured,
    rpl_themes
  ) |> 
  slice_head(n = 15)

print(high_residual_counties)

write_csv(
  high_residual_counties,
  file.path(
    tables_dir,
    "high_residual_counties.csv"
  )
)

# ------------------------------------------------------------
# 13. Model fit summary
# ------------------------------------------------------------

model_fit <- tibble(
  model = c(
    "Model 1",
    "Model 2",
    "Model 3",
    "Model 4"
  ),
  r_squared = c(
    summary(m1)$r.squared,
    summary(m2)$r.squared,
    summary(m3)$r.squared,
    summary(m4)$r.squared
  ),
  adj_r_squared = c(
    summary(m1)$adj.r.squared,
    summary(m2)$adj.r.squared,
    summary(m3)$adj.r.squared,
    summary(m4)$adj.r.squared
  )
)

print(model_fit)

write_csv(
  model_fit,
  file.path(
    tables_dir,
    "model_fit_summary.csv"
  )
)

# ------------------------------------------------------------
# 14. Completion message
# ------------------------------------------------------------

message("Statistical modeling completed successfully.")
message("Outputs saved to:")
message("tables/")
message("figures/")