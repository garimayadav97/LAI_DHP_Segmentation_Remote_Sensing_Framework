# 📦 Load packages
library(readxl)
library(dplyr)
library(Metrics)
library(writexl)
setwd('./research/dhp_analysis/playground/')


# Load data
df <- read_excel("regression_analysis_345_rings.xlsx", sheet = "Sheet1")

# Target and predictors
target <- "LICOR"
predictors <- setdiff(names(df), target)

# Ensure output directory exists
dir.create("lai_regression_results", showWarnings = FALSE)

#️ Storage
results_list <- list()
metrics_list <- list()

# Loop through each predictor
for (pred in predictors) {
  cat("\n Processing:", pred, "\n")
  
  sub_df <- df %>%
    select(Actual = !!sym(target), Predicted = !!sym(pred)) %>%
    filter(!is.na(Actual), !is.na(Predicted))
  
  # Initial regression for residual filtering
  lm_model <- lm(Actual ~ Predicted, data = sub_df)
  sub_df$Residual <- abs(sub_df$Actual - sub_df$Predicted)
  threshold <- quantile(sub_df$Residual, 0.70, na.rm = TRUE)
  sub_df <- sub_df %>% filter(Residual <= threshold)
  
  # Final regression model
  final_model <- lm(Actual ~ Predicted, data = sub_df)
  sub_df$Fitted <- predict(final_model)
  
  # Metrics
  rmse_val <- RMSE(sub_df$Fitted, sub_df$Actual)
  r_val <- cor(sub_df$Actual, sub_df$Fitted)
  r2_val <- summary(final_model)$r.squared
  bias_val <- mean(sub_df$Fitted - sub_df$Actual)
  p_val <- summary(final_model)$coefficients[2, 4]
  
  # Save results to Excel sheet
  sheet_name <- gsub("-", "_", pred)
  results_list[[sheet_name]] <- sub_df
  
  # Save metrics
  metrics_list[[pred]] <- data.frame(
    Predictor = pred,
    RMSE = round(rmse_val, 2),
    R = round(r_val, 2),
    R2 = round(r2_val, 2),
    Bias = round(bias_val, 2),
    P_value = signif(p_val, 2),
    N = nrow(sub_df)
  )
}

# Extra comparison: LAI-RING5 vs LAI-CNN
cat("\n Extra comparison: LAI-RING5 vs LAI-CNN\n")

# Extract and clean data
extra_df <- df %>%
  select(Actual = `CE_RING_5`, Predicted = `CNN_RING_5`) %>%
  filter(!is.na(Actual), !is.na(Predicted))

# Residual-based filtering (70% threshold like main loop)
lm_model_extra <- lm(Actual ~ Predicted, data = extra_df)
extra_df$Residual <- abs(extra_df$Actual - extra_df$Predicted)
threshold_extra <- quantile(extra_df$Residual, 0.70, na.rm = TRUE)
extra_df <- extra_df %>% filter(Residual <= threshold_extra)

# Final model
final_model_extra <- lm(Actual ~ Predicted, data = extra_df)
extra_df$Fitted <- predict(final_model_extra)

# Metrics
rmse_val <- RMSE(extra_df$Fitted, extra_df$Actual)
r_val <- cor(extra_df$Actual, extra_df$Fitted)
r2_val <- summary(final_model_extra)$r.squared
bias_val <- mean(extra_df$Fitted - extra_df$Actual)
p_val <- summary(final_model_extra)$coefficients[2, 4]

# Save sheet
results_list[["LAI_RING5_vs_LAI_CNN"]] <- extra_df

metrics_df <- bind_rows(metrics_list)


# Add to metrics
metrics_df <- rbind(metrics_df, data.frame(
  Predictor = "LAI_RING5_vs_LAI_CNN",
  RMSE = round(rmse_val, 2),
  R = round(r_val, 2),
  R2 = round(r2_val, 2),
  Bias = round(bias_val, 2),
  P_value = signif(p_val, 2),
  N = nrow(extra_df)
))


# Combine metrics
results_list[["Summary_Metrics"]] <- metrics_df

# 💾 Save all results in one Excel file
write_xlsx(results_list, path = "LAI_LICOR_Comparisons4.xlsx")

cat("\n✅ All results saved to: LAI_LICOR_Comparisons4.xlsx\n")
