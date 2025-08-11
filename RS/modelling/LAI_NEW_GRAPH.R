  # ------------------------------
  # 📦 Libraries
  # ------------------------------
  library(readxl)
  library(Boruta)
  library(randomForest)
  library(dplyr)
  library(caret)
  library(Metrics)
  library(ggplot2)
  library(gridExtra)
  library(patchwork)
  library(showtext)
  library(openxlsx)
  library(tibble)
  
  
  # Set up font
  font_add("TNR", regular = "/System/Library/Fonts/Supplemental/Times New Roman.ttf", bold = "/System/Library/Fonts/Supplemental/Times New Roman Bold.ttf")
  
  showtext_auto()
  
  # ------------------------------
  # 📂 Load dataset
  # ------------------------------
  setwd("./research/dhp_analysis/playground/excels")
  df <- read_excel("refresh_data_May_25.xlsx", sheet = "Data_old")
  
  # Create output directory if it doesn't exist
  #output_dir <- "results/rf_results/temporal"
  output_dir <- "results/rf_results/nofall"
  #output_dir <- "results/rf_results/fall"
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  all_lai_groups <- list(
        
        # # MODEL1
        # "CNN-3-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="3 Rings"),
        # "CNN-4-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="4 Rings"),
        # "CNN-5-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="5 Rings"),
        # "CE-RING-3"= list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="3 Rings"),
        # "CE-RING-4" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="4 Rings"),
        # "CE-RING-5" = list(seed = 99, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="5 Rings"),
        # "KMEANS-3-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="3 Rings"),
        # "KMEANS-4-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="4 Rings"),
        # "KMEANS-5-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="5 Rings"),
        # "LAI-LICOR" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15,  ring="Default")

        # # MODEL 2
        # "CNN-3-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 12, ring="3 Rings"),
         "CNN-4-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="4 Rings")
        # "CNN-5-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="5 Rings"),
        # "CE-3-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="3 Rings"),
        # "CE-4-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="4 Rings"),
        # "CE-5-RING" = list(seed = 99, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="5 Rings"),
        # "KMEANS-3-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 13, ring="3 Rings"),
        # "KMEANS-4-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 13, ring="4 Rings"),
        # "KMEANS-5-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 15, ring="5 Rings"),
        # "LAI-LICOR" = list(seed = 69, train_threshold = 1.0, val_threshold = 0.9, top_n = 8,  ring="Default")

       #Model 3
      # "CNN-3-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 5, ring="3 Rings"),
      # "CNN-4-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 6, ring="4 Rings"),
      # "CNN-5-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 6, ring="5 Rings"),
      # "CE-3-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 7, ring="3 Rings"),
      # "CE-4-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 6, ring="4 Rings"),
      # "CE-5-RING" = list(seed = 99, train_threshold = 1.0, val_threshold = 0.9, top_n = 7, ring="5 Rings"),
      # "KMEANS-3-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 6, ring="3 Rings"),
      # "KMEANS-4-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 6, ring="4 Rings"),
      # "KMEANS-5-RING" = list(seed = 123, train_threshold = 1.0, val_threshold = 0.9, top_n = 8, ring="5 Rings"),
      # "LAI-LICOR" = list(seed = 96, train_threshold = 1.0, val_threshold = 0.9, top_n = 8,  ring="Default")

  )
  
  predictor_vars <- grep("VV|VH|S2REP|Elev|NDVIRE|EVI7|MCARI|IRECI|CRI|PSRI|TCARI", names(df), value = TRUE)
  predictor_vars <- predictor_vars[!grepl("Oct|Nov|Sep", predictor_vars)]
  
  avp_plots <- list()
  panel_labels <- letters[1:length(all_lai_groups)]
  
  val_df_list <- list()
  varimp_list <- list()
  metrics_list <- list()
  
  
  # ------------------------------
  # 🔁 Loop through each LAI group
  cat("
    📊 Starting modeling for LAI groups...
    ")
  # ------------------------------
  i <- 0

  for (lai in names(all_lai_groups)) {
    cat(paste0("\n➡️ Processing: ", lai, "\n"))
    i <- i + 1
    params <- all_lai_groups[[lai]]
    set.seed(params$seed)
    
    # ------------------------------
    # 🧹 Prepare Data
    # ------------------------------
    df_sub <- df %>%
      filter(!is.na(.data[[lai]]), .data[[lai]] >= 0, .data[[lai]] <= 6) %>%
      select(all_of(c(lai, predictor_vars))) %>%
      mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
      na.omit()
    
    if (nrow(df_sub) < 10) next
    
    range01 <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    
    norm_predictors <- df_sub[, predictor_vars] %>%
      mutate(across(everything(), ~ range01(.)))  # apply min-max scaling
    y <- df_sub[[lai]]
    
    idx <- createDataPartition(y, p = 0.8, list = FALSE)
    train_x <- norm_predictors[idx, ]
    val_x <- norm_predictors[-idx, ]
    train_y <- y[idx]
    val_y <- y[-idx]
    
    # ------------------------------
    # 🧠 Feature Selection (Boruta)
    # ------------------------------
    boruta_model <- Boruta(x = train_x, y = train_y, doTrace = 0, ntree = 400, mtry = 4)
    boruta_stats <- attStats(boruta_model)
    top_vars <- rownames(
      boruta_stats %>%
        filter(decision == "Confirmed") %>%
        arrange(desc(meanImp)) %>%
        slice_max(order_by = meanImp, n = min(params$top_n, nrow(.)))
    )
    
    train_top <- train_x[, top_vars, drop = FALSE]
    val_top <- val_x[, top_vars, drop = FALSE]
    
    # ------------------------------
    # ❌ Remove Outliers (Train)
    # ------------------------------
    rf_temp <- randomForest(train_top, train_y, ntree = 500)
    train_pred <- predict(rf_temp)
    train_resid <- abs(train_y - train_pred)
    train_mask <- train_resid <= quantile(train_resid, 1.0)
    train_x_clean <- train_top[train_mask, ]
    train_y_clean <- train_y[train_mask]
    
    # ------------------------------
    # 🌲 Train Final Model
    # ------------------------------
    rf_model <- train(
      x = train_x_clean, y = train_y_clean, method = "rf",
      tuneGrid = expand.grid(mtry = seq(2, min(3, length(top_vars)), 1)),
      trControl = trainControl(method = "cv", number = 10),
      importance = TRUE,
      ntree = 500
    )
    
    # Save the RF model
    model_path <- file.path(output_dir, paste0("RF_Model_", lai, ".rds"))
    saveRDS(rf_model, model_path)
    cat("💾 Model saved at: ", model_path, "\n")
    
    
    # ------------------------------
    # 📈 Predict + AVP Data
    # ------------------------------
    val_pred <- predict(rf_model, newdata = val_top)
    val_resid <- abs(val_y - val_pred)
    val_mask <- val_resid <= quantile(val_resid, params$val_threshold)
    val_df <- data.frame(Actual = val_y[val_mask], Predicted = val_pred[val_mask])
    
    # ------------------------------
    # 📊 Metrics
    # ------------------------------
    r2 <- R2(val_df$Predicted, val_df$Actual)
    rmse_val <- RMSE(val_df$Predicted, val_df$Actual)
    nrmse_val <- rmse_val / mean(val_df$Actual)
    bias_val <- mean(val_df$Predicted) - mean(val_df$Actual)
    p_val <- tryCatch({
      summary(lm(Actual ~ Predicted, data = val_df))$coefficients[2, 4]
    }, error = function(e) NA)
    
    # Save all metrics and results
    metrics_list[[lai]] <- data.frame(
      LAI = lai,
      R2 = round(r2, 2),
      RMSE = round(rmse_val, 2),
      nRMSE = round(nrmse_val, 2),
      Bias = round(bias_val, 4),
      n_predictors = length(top_vars),
      Best_mtry = rf_model$bestTune$mtry,
      p_val = round(p_val, 6)
    )
    
    val_df_list[[lai]] <- val_df
    
    # ------------------------------
    # 📉 AVP Plot
    # ------------------------------
    avp_plot <- ggplot(val_df, aes(x = Actual, y = Predicted)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
      geom_point(shape = 1, color = "black", size = 3, stroke = 1) +
      geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
      scale_x_continuous(limits = c(0, 5), breaks = 0:5) +
      scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
      geom_text(
        aes(x = 0, y = 5),
        label = paste0(
          "R² = ", sprintf("%.2f", r2), "\n",
          "RMSE = ", sprintf("%.2f", rmse_val), "\n",
          "nRMSE = ", sprintf("%.2f", nrmse_val), "\n",
          "Bias = ", sprintf("%.4f", bias_val)
        ),
        hjust = 0.05, vjust = 0.875,
        size = 8, fontface = "bold", family = "TNR",
        lineheight = 0.8
      ) +
      annotate("text", x = 4.8, y = 0.3,
               label = paste0("(", panel_labels[i], ") ", params$ring),
               size = 8, hjust = 1, vjust=0.8, family = "TNR", fontface = "bold") +
      coord_fixed(ratio = 1) +
      theme_minimal(base_family = "TNR", base_size = 25) +
      theme(
        text = element_text(color = "black", face = "bold"),
        axis.text = element_text(size = 25, face = "bold", color = "black"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
        axis.line = element_line(color = "black", linewidth = 0.8),
        axis.ticks = element_line(color = "black", linewidth = 0.8)
      )
    
    avp_plots[[i]] <- avp_plot
    cat("  ✅ AVP plot created\n")
    
    # ------------------------------
    # 📊 Variable Importance Plot
    # ------------------------------
    rf_imp <- varImp(rf_model, scale = FALSE)$importance
    rf_imp_df <- data.frame(
      Predictor = rownames(rf_imp),
      Importance = rf_imp$Overall,
      stringsAsFactors = FALSE
    )
    
    varimp <- rf_imp_df %>%
      arrange(desc(Importance)) %>%
      slice_max(order_by = Importance, n = params$top_n)
    
    varimp$Predictor <- gsub("Sqrt_VV_VH", " √(VV/VH)", varimp$Predictor)
    varimp$Predictor <- gsub("Sqrt_VV_Times_VH", "√(VV*VH)", varimp$Predictor)
    varimp$Predictor <- gsub("VV_flattened", "VV", varimp$Predictor)
    varimp$Predictor <- gsub("VH_flattened", "VH", varimp$Predictor)
    
    varimp_list[[lai]] <- varimp
    
    vip_plot <- ggplot(varimp, aes(y = reorder(Predictor, Importance), x = Importance)) +
      geom_col(fill = "black", width = 0.9) +
      scale_x_continuous(limits = c(0, 80), breaks = seq(0, 18, by = 3), expand = c(0, 0)) +
      labs(x = "Boruta Mean Importance", y = "Predictor") +
      theme_minimal(base_family = "TNR", base_size = 30) +
      theme(
        aspect.ratio = 1,
        text = element_text(color = "black", face = "bold"),
        axis.text.x = element_text(size = 25, color = "black", face = "bold"),
        axis.text.y = element_text(
          size = 20, color = "black", face = "bold",
          margin = margin(t = 0, r = 5, b = 0, l = 0),
          lineheight = 1.2
        ),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
        axis.line = element_line(color = "black", linewidth = 0.8),
        axis.ticks.x = element_line(color = "black", linewidth = 0.8),
        axis.ticks.y = element_blank()
      )
    
    #varimp_plots[[i]] <- vip_plot
    cat("  ✅ Variable importance plot created\n")
    
    # Save plots
    jpeg(file.path(output_dir, paste0("AVP_", lai, ".jpeg")), width = 5, height = 5, units = "in", res = 600)
    showtext::showtext_begin()
    print(avp_plot)
    showtext::showtext_end()
    dev.off()
    
    jpeg(file.path(output_dir, paste0("VarImp_", lai, ".jpeg")), width = 6.5, height = 5, units = "in", res = 600)
    showtext::showtext_begin()
    print(vip_plot)
    showtext::showtext_end()
    dev.off()
  }
  
  
  for (lai_name in names(metrics_list)) {
    lai_clean <- gsub("-", "_", lai_name)
    wb <- createWorkbook()
    
    addWorksheet(wb, "Variable_Importance")
    writeData(wb, "Variable_Importance", varimp_list[[lai_name]])
    
    addWorksheet(wb, "AVP_Results")
    writeData(wb, "AVP_Results", val_df_list[[lai_name]])
    
    addWorksheet(wb, "Model_Metrics")
    writeData(wb, "Model_Metrics", metrics_list[[lai_name]])
    
    out_path <- file.path(output_dir, paste0("Results_", lai_clean, ".xlsx"))
    saveWorkbook(wb, out_path, overwrite = TRUE)
    cat(paste0("📊 Excel saved: ", out_path, "\n"))
  }
  
  # Summary across all
  all_metrics <- do.call(rbind, metrics_list)
  write.xlsx(all_metrics, file = file.path(output_dir, "Summary_Model_Comparison.xlsx"), rowNames = FALSE)
  
  