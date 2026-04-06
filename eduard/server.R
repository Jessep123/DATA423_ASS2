# server.R - DATA423 Assignment 2 - Eduard Bradley

server <- function(input, output, session) {
  
  # ==========================================================================
  # MISSING VALUE STANDARDIZATION FUNCTION
  # ==========================================================================
  
  standardize_missing <- function(data, treat_neg99 = TRUE, treat_dash = TRUE, treat_na_string = TRUE) {
    result <- data
    for(col in names(result)) {
      vals <- as.character(result[[col]])
      was_numeric <- col %in% numeric_cols
      if(treat_neg99) vals[vals == "-99"] <- NA
      if(treat_dash) vals[vals == "--"] <- NA
      if(treat_na_string) vals[vals == "NA"] <- NA
      if(was_numeric) {
        result[[col]] <- suppressWarnings(as.numeric(vals))
      } else {
        result[[col]] <- vals
      }
    }
    return(result)
  }
  
  # ==========================================================================
  # OUTLIER DETECTION HELPER FUNCTIONS
  # ==========================================================================
  
  prepare_outlier_data <- function(data, include_code = FALSE) {
    result <- data
    for(col in names(result)) {
      if(col %in% numeric_cols && is.character(result[[col]])) {
        result[[col]] <- suppressWarnings(as.numeric(result[[col]]))
      }
    }
    numeric_cols_only <- names(result)[sapply(result, is.numeric)]
    if(!include_code) {
      numeric_cols_only <- numeric_cols_only[!numeric_cols_only %in% c("CODE", "OBS_TYPE")]
    }
    if(length(numeric_cols_only) < 2) return(NULL)
    result <- result[, numeric_cols_only, drop = FALSE]
    result <- na.omit(result)
    if(nrow(result) < 5) return(NULL)
    return(result)
  }
  
  detect_mahalanobis_outliers <- function(data, threshold_prob = 0.999) {
    processed <- prepare_outlier_data(data)
    if(is.null(processed)) return(NULL)
    Covar <- cov(processed)
    Means <- colMeans(processed)
    if(rcond(Covar) < 1e-10) Covar <- Covar + diag(ncol(processed)) * 1e-6
    md2 <- mahalanobis(x = processed, center = Means, cov = Covar)
    names(md2) <- rownames(processed)
    df <- ncol(processed)
    threshold <- qchisq(p = threshold_prob, df = df)
    is_outlier <- md2 > threshold
    return(list(scores = md2, threshold = threshold, is_outlier = is_outlier,
                outlier_indices = which(is_outlier), outlier_names = names(md2)[is_outlier],
                df = df, n_obs = length(md2)))
  }
  
  detect_cooks_outliers <- function(data, method = "4mean") {
    processed <- prepare_outlier_data(data, include_code = FALSE)
    if(is.null(processed)) return(NULL)
    if(!"DEATH_RATE" %in% names(processed)) return(NULL)
    predictor_vars <- names(processed)[names(processed) != "DEATH_RATE"]
    if(length(predictor_vars) == 0) return(NULL)
    formula <- as.formula(paste("DEATH_RATE ~", paste(predictor_vars, collapse = " + ")))
    lmod <- lm(formula, data = processed)
    dc <- cooks.distance(lmod)
    names(dc) <- rownames(processed)
    if(method == "4mean") {
      threshold <- 4 * mean(dc, na.rm = TRUE)
    } else if(method == "4n") {
      threshold <- 4 / length(dc)
    } else {
      threshold <- quantile(dc, 0.99, na.rm = TRUE)
    }
    is_outlier <- dc > threshold
    return(list(scores = dc, threshold = threshold, is_outlier = is_outlier,
                outlier_indices = which(is_outlier), outlier_names = names(dc)[is_outlier],
                method = method, n_obs = length(dc)))
  }
  
  detect_lof_outliers <- function(data, minPts = 5, threshold = 2) {
    processed <- prepare_outlier_data(data)
    if(is.null(processed)) return(NULL)
    if(!requireNamespace("dbscan", quietly = TRUE)) return(NULL)
    mat <- as.matrix(processed)
    lof_scores <- dbscan::lof(mat, minPts = minPts)
    names(lof_scores) <- rownames(processed)
    is_outlier <- lof_scores > threshold
    return(list(scores = lof_scores, threshold = threshold, is_outlier = is_outlier,
                outlier_indices = which(is_outlier), outlier_names = names(lof_scores)[is_outlier],
                minPts = minPts, n_obs = length(lof_scores)))
  }
  
  detect_svm_outliers <- function(data, nu = 0.05, kernel = "linear", scale = FALSE) {
    processed <- prepare_outlier_data(data)
    if(is.null(processed)) return(NULL)
    if(!requireNamespace("e1071", quietly = TRUE)) return(NULL)
    mat <- as.matrix(processed)
    model <- e1071::svm(mat, y = NULL, type = 'one-classification', 
                        nu = nu, scale = scale, kernel = kernel)
    predictions <- predict(model, mat)
    is_outlier <- !predictions
    decision_values <- attr(predictions, "decision.values")
    if(is.null(decision_values)) {
      scores <- ifelse(is_outlier, 1, 0)
    } else {
      scores <- -decision_values[, 1]
    }
    names(scores) <- rownames(processed)
    names(is_outlier) <- rownames(processed)
    return(list(scores = scores, threshold = nu, is_outlier = is_outlier,
                outlier_indices = which(is_outlier), outlier_names = names(is_outlier)[is_outlier],
                kernel = kernel, nu = nu, n_obs = length(is_outlier)))
  }
  
  detect_rf_outliers <- function(data, iqr_multiplier = 2) {
    processed <- prepare_outlier_data(data, include_code = FALSE)
    if(is.null(processed)) return(NULL)
    if(!"DEATH_RATE" %in% names(processed)) return(NULL)
    if(!requireNamespace("randomForest", quietly = TRUE)) return(NULL)
    predictor_vars <- names(processed)[names(processed) != "DEATH_RATE"]
    if(length(predictor_vars) == 0) return(NULL)
    formula <- as.formula(paste("DEATH_RATE ~", paste(predictor_vars, collapse = " + ")))
    rf_model <- randomForest::randomForest(formula, data = processed, ntree = 500, importance = FALSE)
    predictions <- predict(rf_model, newdata = processed)
    residuals <- processed$DEATH_RATE - predictions
    names(residuals) <- rownames(processed)
    box_stats <- boxplot.stats(residuals, coef = iqr_multiplier)
    lower_bound <- box_stats$stats[1]
    upper_bound <- box_stats$stats[5]
    is_outlier <- residuals < lower_bound | residuals > upper_bound
    return(list(scores = residuals, threshold_lower = lower_bound, threshold_upper = upper_bound,
                is_outlier = is_outlier, outlier_indices = which(is_outlier),
                outlier_names = names(residuals)[is_outlier], iqr_multiplier = iqr_multiplier,
                n_obs = length(residuals), model = rf_model))
  }
  
  detect_iforest_outliers <- function(data, threshold = 0.6) {
    processed <- prepare_outlier_data(data)
    if(is.null(processed)) return(NULL)
    if(!requireNamespace("isotree", quietly = TRUE)) return(NULL)
    itree <- isotree::isolation.forest(processed, seed = 123)
    scores <- predict(itree, newdata = processed)
    names(scores) <- rownames(processed)
    is_outlier <- scores > threshold
    return(list(scores = scores, threshold = threshold, is_outlier = is_outlier,
                outlier_indices = which(is_outlier), outlier_names = names(scores)[is_outlier],
                n_obs = length(scores)))
  }
  
  create_outlier_consensus <- function(results_list, data) {
    codes <- data$CODE
    if(is.null(codes)) {
      codes <- rownames(data)
      if(is.null(codes)) codes <- 1:nrow(data)
    }
    all_obs <- rownames(prepare_outlier_data(data))
    if(is.null(all_obs)) all_obs <- 1:nrow(data)
    consensus_df <- data.frame(Observation = all_obs, Code = codes[1:length(all_obs)], stringsAsFactors = FALSE)
    method_names <- c()
    for(method_name in names(results_list)) {
      if(!is.null(results_list[[method_name]])) {
        method_names <- c(method_names, method_name)
        flags <- rep(0, nrow(consensus_df))
        outlier_names <- results_list[[method_name]]$outlier_names
        flags[consensus_df$Observation %in% outlier_names] <- 1
        consensus_df[[method_name]] <- flags
      }
    }
    if(length(method_names) > 0) {
      consensus_df$Total_Flags <- rowSums(consensus_df[, method_names, drop = FALSE])
    } else {
      consensus_df$Total_Flags <- 0
    }
    return(list(data = consensus_df, methods = method_names))
  }
  
  # ==========================================================================
  # REACTIVE VALUES
  # ==========================================================================
  
  reactive_data <- reactiveVal(df)
  recipe_object <- reactiveVal(NULL)
  recipe_processed_data <- reactiveVal(NULL)
  model_results <- reactiveVal(NULL)
  train_test_split <- reactiveVal(NULL)
  missing_rpart_model <- reactiveVal(NULL)
  outlier_results <- reactiveVal(list())
  processed_data_working <- reactiveVal(standardize_missing(df))
  recipe_objects <- reactiveVal(list())
  recipe_names <- reactiveVal(character(0))
  current_recipe_prepped <- reactiveVal(NULL)
  recipe_comparison_results <- reactiveVal(data.frame())
  
  # ==========================================================================
  # SIDEBAR TOGGLE OBSERVERS
  # ==========================================================================
  
  sidebar_state <- reactiveValues(
    heatmap = TRUE, distribution = TRUE, boxplot = TRUE, correlation = TRUE,
    scatter = TRUE, mosaic = TRUE, ggpairs = TRUE, tabplot = TRUE, rising = TRUE, datatable = TRUE
  )
  
  observe({
    for(panel in names(sidebar_state)) sidebar_state[[panel]] <- TRUE
  })
  
  observeEvent(input$toggle_heatmap_sidebar, {
    if(sidebar_state$heatmap) {
      shinyjs::addClass(id = "heatmap_wrapper", class = "sidebar-hidden")
      sidebar_state$heatmap <- FALSE
      updateActionButton(session, "toggle_heatmap_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Show Filters'))
    } else {
      shinyjs::removeClass(id = "heatmap_wrapper", class = "sidebar-hidden")
      sidebar_state$heatmap <- TRUE
      updateActionButton(session, "toggle_heatmap_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'))
    }
  })
  
  observeEvent(input$toggle_distribution_sidebar, {
    if(sidebar_state$distribution) {
      shinyjs::addClass(id = "distribution_wrapper", class = "sidebar-hidden")
      sidebar_state$distribution <- FALSE
      updateActionButton(session, "toggle_distribution_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Show Filters'))
    } else {
      shinyjs::removeClass(id = "distribution_wrapper", class = "sidebar-hidden")
      sidebar_state$distribution <- TRUE
      updateActionButton(session, "toggle_distribution_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'))
    }
  })
  
  observeEvent(input$toggle_boxplot_sidebar, {
    if(sidebar_state$boxplot) {
      shinyjs::addClass(id = "boxplot_wrapper", class = "sidebar-hidden")
      sidebar_state$boxplot <- FALSE
      updateActionButton(session, "toggle_boxplot_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Show Filters'))
    } else {
      shinyjs::removeClass(id = "boxplot_wrapper", class = "sidebar-hidden")
      sidebar_state$boxplot <- TRUE
      updateActionButton(session, "toggle_boxplot_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'))
    }
  })
  
  observeEvent(input$toggle_correlation_sidebar, {
    if(sidebar_state$correlation) {
      runjs('document.getElementById("correlation_wrapper").classList.add("sidebar-hidden")')
      sidebar_state$correlation <- FALSE
      updateActionButton(session, "toggle_correlation_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Show Filters'))
    } else {
      runjs('document.getElementById("correlation_wrapper").classList.remove("sidebar-hidden")')
      sidebar_state$correlation <- TRUE
      updateActionButton(session, "toggle_correlation_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'))
    }
  })
  
  observeEvent(input$toggle_scatter_sidebar, {
    if(sidebar_state$scatter) {
      shinyjs::addClass(id = "scatter_wrapper", class = "sidebar-hidden")
      sidebar_state$scatter <- FALSE
      updateActionButton(session, "toggle_scatter_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Show Filters'))
    } else {
      shinyjs::removeClass(id = "scatter_wrapper", class = "sidebar-hidden")
      sidebar_state$scatter <- TRUE
      updateActionButton(session, "toggle_scatter_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'))
    }
  })
  
  observeEvent(input$toggle_mosaic_sidebar, {
    if(sidebar_state$mosaic) {
      shinyjs::addClass(id = "mosaic_wrapper", class = "sidebar-hidden")
      sidebar_state$mosaic <- FALSE
      updateActionButton(session, "toggle_mosaic_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Show Filters'))
    } else {
      shinyjs::removeClass(id = "mosaic_wrapper", class = "sidebar-hidden")
      sidebar_state$mosaic <- TRUE
      updateActionButton(session, "toggle_mosaic_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'))
    }
  })
  
  observeEvent(input$toggle_ggpairs_sidebar, {
    if(sidebar_state$ggpairs) {
      shinyjs::addClass(id = "ggpairs_wrapper", class = "sidebar-hidden")
      sidebar_state$ggpairs <- FALSE
      updateActionButton(session, "toggle_ggpairs_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Show Filters'))
    } else {
      shinyjs::removeClass(id = "ggpairs_wrapper", class = "sidebar-hidden")
      sidebar_state$ggpairs <- TRUE
      updateActionButton(session, "toggle_ggpairs_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'))
    }
  })
  
  observeEvent(input$toggle_tabplot_sidebar, {
    if(sidebar_state$tabplot) {
      shinyjs::addClass(id = "tabplot_wrapper", class = "sidebar-hidden")
      sidebar_state$tabplot <- FALSE
      updateActionButton(session, "toggle_tabplot_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Show Filters'))
    } else {
      shinyjs::removeClass(id = "tabplot_wrapper", class = "sidebar-hidden")
      sidebar_state$tabplot <- TRUE
      updateActionButton(session, "toggle_tabplot_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'))
    }
  })
  
  observeEvent(input$toggle_rising_sidebar, {
    if(sidebar_state$rising) {
      shinyjs::addClass(id = "rising_wrapper", class = "sidebar-hidden")
      sidebar_state$rising <- FALSE
      updateActionButton(session, "toggle_rising_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Show Filters'))
    } else {
      shinyjs::removeClass(id = "rising_wrapper", class = "sidebar-hidden")
      sidebar_state$rising <- TRUE
      updateActionButton(session, "toggle_rising_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'))
    }
  })
  
  observeEvent(input$toggle_datatable_sidebar, {
    if(sidebar_state$datatable) {
      shinyjs::addClass(id = "datatable_wrapper", class = "sidebar-hidden")
      sidebar_state$datatable <- FALSE
      updateActionButton(session, "toggle_datatable_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Show Filters'))
    } else {
      shinyjs::removeClass(id = "datatable_wrapper", class = "sidebar-hidden")
      sidebar_state$datatable <- TRUE
      updateActionButton(session, "toggle_datatable_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'))
    }
  })
  
  # ==========================================================================
  # TAB 1: SUMMARY & EDA
  # ==========================================================================
  
  # Summary outputs
  output$summary_row_count <- renderText({ format(nrow(df), big.mark = ",") })
  output$summary_col_count <- renderText({ ncol(df) })
  output$summary_total_cells <- renderText({ format(nrow(df) * ncol(df), big.mark = ",") })
  
  output$summary_non_na_values <- renderText({
    total_cells <- nrow(df) * ncol(df)
    total_missing <- sum(sapply(df, function(x) sum(grepl("^-99$|^--$|^NA$", as.character(x)))))
    format(total_cells - total_missing, big.mark = ",")
  })
  
  output$summary_complete_cases <- renderUI({
    complete_rows <- sum(apply(df, 1, function(row) !any(grepl("^-99$|^--$|^NA$", as.character(row)))))
    pct <- round(100 * complete_rows / nrow(df), 1)
    div(h5(complete_rows, style = "margin: 2px; font-weight: bold;"),
        p(paste("Complete Rows (", pct, "%)", sep = ""), style = "font-size: 11px; margin: 0;"))
  })
  
  output$summary_incomplete_cases <- renderUI({
    complete_rows <- sum(apply(df, 1, function(row) !any(grepl("^-99$|^--$|^NA$", as.character(row)))))
    incomplete_rows <- nrow(df) - complete_rows
    pct <- round(100 * incomplete_rows / nrow(df), 1)
    div(h5(incomplete_rows, style = "margin: 2px; font-weight: bold;"),
        p(paste("Rows with Missing (", pct, "%)", sep = ""), style = "font-size: 11px; margin: 0;"))
  })
  
  output$summary_missing_cells <- renderUI({
    total_cells <- nrow(df) * ncol(df)
    total_missing <- sum(sapply(df, function(x) sum(grepl("^-99$|^--$|^NA$", as.character(x)))))
    pct <- round(100 * total_missing / total_cells, 1)
    div(h5(paste(pct, "%", sep = ""), style = "margin: 2px; font-weight: bold;"),
        p("Missing Cells", style = "font-size: 11px; margin: 0;"))
  })
  
  output$summary_data_completeness <- renderUI({
    total_cells <- nrow(df) * ncol(df)
    total_missing <- sum(sapply(df, function(x) sum(grepl("^-99$|^--$|^NA$", as.character(x)))))
    complete_pct <- round(100 * (total_cells - total_missing) / total_cells, 1)
    div(h5(paste(complete_pct, "%", sep = ""), style = "margin: 2px; font-weight: bold;"),
        p("Data Complete", style = "font-size: 11px; margin: 0;"))
  })
  
  output$summary_numeric_count <- renderUI({ div(h5(length(numeric_cols), style = "margin: 2px; font-weight: bold;"), p("Numeric Vars", style = "font-size: 11px; margin: 0;")) })
  output$summary_categorical_count <- renderUI({ div(h5(length(categorical_cols), style = "margin: 2px; font-weight: bold;"), p("Categorical Vars", style = "font-size: 11px; margin: 0;")) })
  
  output$summary_missing_types <- renderUI({
    total_neg99 <- sum(sapply(df, function(x) sum(grepl("^-99$", as.character(x)), na.rm = TRUE)))
    total_dash <- sum(sapply(df, function(x) sum(grepl("^--$", as.character(x)), na.rm = TRUE)))
    total_na_string <- sum(sapply(df, function(x) sum(grepl("^NA$", as.character(x)), na.rm = TRUE)))
    div(h5(paste(total_neg99, "/", total_dash, "/", total_na_string, sep = " "), style = "margin: 2px; font-weight: bold;"),
        p("-99 / -- / 'NA'", style = "font-size: 11px; margin: 0;"))
  })
  
  output$summary_numeric_table <- renderDT({
    summary_list <- lapply(numeric_cols, function(col) {
      vals <- df[[col]]
      char_vals <- as.character(vals)
      char_vals[grepl("^-99$|^--$|^NA$", char_vals)] <- NA
      num_vals <- suppressWarnings(as.numeric(char_vals))
      neg99_count <- sum(grepl("^-99$", as.character(vals)), na.rm = TRUE)
      dash_count <- sum(grepl("^--$", as.character(vals)), na.rm = TRUE)
      na_string_count <- sum(grepl("^NA$", as.character(vals)), na.rm = TRUE)
      total_missing <- neg99_count + dash_count + na_string_count
      if(sum(!is.na(num_vals)) > 0) {
        data.frame(Variable = gsub("_", " ", col), Min = round(min(num_vals, na.rm = TRUE), 2),
                   Q1 = round(quantile(num_vals, 0.25, na.rm = TRUE), 2),
                   Median = round(median(num_vals, na.rm = TRUE), 2),
                   Mean = round(mean(num_vals, na.rm = TRUE), 2),
                   Q3 = round(quantile(num_vals, 0.75, na.rm = TRUE), 2),
                   Max = round(max(num_vals, na.rm = TRUE), 2), SD = round(sd(num_vals, na.rm = TRUE), 2),
                   N = sum(!is.na(num_vals)), Missing = total_missing,
                   Missing_Pct = paste0(round(100 * total_missing / length(vals), 1), "%"),
                   Neg99 = neg99_count, Dash = dash_count, NA_String = na_string_count, stringsAsFactors = FALSE)
      } else {
        data.frame(Variable = gsub("_", " ", col), Min = NA, Q1 = NA, Median = NA, Mean = NA,
                   Q3 = NA, Max = NA, SD = NA, N = 0, Missing = total_missing,
                   Missing_Pct = "100%", Neg99 = neg99_count, Dash = dash_count,
                   NA_String = na_string_count, stringsAsFactors = FALSE)
      }
    })
    summary_df <- do.call(rbind, summary_list)
    colnames(summary_df) <- c("Variable", "Min", "Q1", "Median", "Mean", "Q3", "Max", "SD",
                              "N", "Missing", "Missing %", "-99", "--", "NA String")
    datatable(summary_df, options = list(paging = FALSE, searching = FALSE, info = FALSE, scrollX = TRUE, dom = 't'),
              rownames = FALSE) %>% formatRound(columns = c("Min", "Q1", "Median", "Mean", "Q3", "Max", "SD"), digits = 2)
  })
  
  output$summary_categorical_table <- renderDT({
    summary_list <- lapply(categorical_cols, function(col) {
      vals <- df[[col]]
      char_vals <- as.character(vals)
      val_counts <- table(vals, useNA = "no")
      n_unique <- length(val_counts)
      if(length(val_counts) > 0) {
        most_common <- names(sort(val_counts, decreasing = TRUE))[1]
        most_common_count <- max(val_counts)
        most_common_pct <- paste0(round(100 * most_common_count / length(vals), 1), "%")
      } else {
        most_common <- NA; most_common_count <- 0; most_common_pct <- "0%"
      }
      neg99_count <- sum(grepl("^-99$", char_vals), na.rm = TRUE)
      dash_count <- sum(grepl("^--$", char_vals), na.rm = TRUE)
      na_string_count <- sum(grepl("^NA$", char_vals), na.rm = TRUE)
      total_missing <- neg99_count + dash_count + na_string_count
      data.frame(Variable = gsub("_", " ", col), Unique_Values = n_unique,
                 Most_Common = as.character(most_common), Most_Common_Count = most_common_count,
                 Most_Common_Pct = most_common_pct, Total_Obs = length(vals),
                 Missing_Total = total_missing, Missing_Pct = paste0(round(100 * total_missing / length(vals), 1), "%"),
                 Neg99 = neg99_count, Dash = dash_count, NA_String = na_string_count, stringsAsFactors = FALSE)
    })
    summary_df <- do.call(rbind, summary_list)
    colnames(summary_df) <- c("Variable", "Unique Values", "Most Common", "Most Common Count",
                              "Most Common %", "Total Obs", "Missing Total", "Missing %",
                              "-99", "--", "NA String")
    datatable(summary_df, options = list(paging = FALSE, searching = FALSE, info = FALSE, scrollX = TRUE, dom = 't'), rownames = FALSE)
  })
  
  output$summary_missing_table <- renderDT({
    missing_df <- data.frame(Variable = gsub("_", " ", names(df)), Total_Obs = nrow(df),
                             Neg99 = sapply(df, function(x) sum(grepl("^-99$", as.character(x)), na.rm = TRUE)),
                             Dash = sapply(df, function(x) sum(grepl("^--$", as.character(x)), na.rm = TRUE)),
                             NA_String = sapply(df, function(x) sum(grepl("^NA$", as.character(x)), na.rm = TRUE)), stringsAsFactors = FALSE)
    missing_df$Total_Missing <- missing_df$Neg99 + missing_df$Dash + missing_df$NA_String
    missing_df$Missing_Pct <- paste0(round(100 * missing_df$Total_Missing / nrow(df), 1), "%")
    missing_df$Type <- ifelse(names(df) %in% numeric_cols, "Numeric", "Categorical")
    missing_df <- missing_df[, c("Variable", "Type", "Total_Obs", "Total_Missing", "Missing_Pct", "Neg99", "Dash", "NA_String")]
    colnames(missing_df) <- c("Variable", "Type", "Total Obs", "Missing Total", "Missing %", "-99", "--", "NA String")
    datatable(missing_df, options = list(paging = FALSE, searching = FALSE, info = FALSE, scrollX = TRUE, dom = 't'), rownames = FALSE)
  })
  
  output$summary_dfsummary <- renderUI({
    tryCatch({
      if(!requireNamespace("summarytools", quietly = TRUE)) {
        return(HTML("<div class='alert alert-warning'><strong>Package not installed:</strong> 'summarytools' is required.</div>"))
      }
      df_temp <- df
      for(col in names(df_temp)) {
        vals <- as.character(df_temp[[col]])
        vals[grepl("^-99$|^--$|^NA$", vals)] <- NA
        if(col %in% numeric_cols) {
          df_temp[[col]] <- suppressWarnings(as.numeric(vals))
        } else {
          df_temp[[col]] <- vals
        }
      }
      summary_df <- summarytools::dfSummary(df_temp, graph.col = FALSE, valid.col = TRUE, silent = TRUE,
                                            style = "grid", plain.ascii = FALSE, headings = FALSE, method = 'render', footnote = NA)
      html_output <- capture.output(summarytools::view(summary_df, method = 'render', bootstrap.css = FALSE, silent = TRUE))
      HTML(paste(html_output, collapse = "\n"))
    }, error = function(e) {
      HTML(paste0("<div class='alert alert-danger'><strong>Error:</strong> ", e$message, "</div>"))
    })
  })
  
  # ==========================================================================
  # BOXPLOT ANALYSIS
  # ==========================================================================
  
  observe({
    updateSliderInput(session, "iqr_boxplot", value = 1.5)
    updateCheckboxInput(session, "boxplot_center", value = TRUE)
    updateCheckboxInput(session, "boxplot_scale", value = TRUE)
    updatePickerInput(session, "boxplot_numeric_vars", selected = numeric_cols)
    updatePickerInput(session, "boxplot_cat_vars_group", selected = "None")
    updatePickerInput(session, "boxplot_filter_vars", selected = NULL)
    updateCheckboxInput(session, "include_null_boxplot", value = FALSE)
  })
  
  apply_categorical_filters <- function(data, cat_vars, filter_prefix, include_null) {
    if(is.null(cat_vars) || length(cat_vars) == 0) return(data)
    for(var in cat_vars) {
      sel <- input[[paste0(filter_prefix, var)]]
      if(!is.null(sel) && length(sel) > 0) {
        if("NA" %in% sel) {
          sel_real <- setdiff(sel, "NA")
          if(include_null) {
            data <- data[data[[var]] %in% sel_real | is.na(data[[var]]), ]
          } else {
            data <- data[data[[var]] %in% sel_real, ]
          }
        } else {
          data <- data[data[[var]] %in% sel, ]
          if(!include_null) data <- data[!is.na(data[[var]]), ]
        }
      }
    }
    return(data)
  }
  
  apply_center_scale <- function(data) {
    if(!input$boxplot_center && !input$boxplot_scale) return(data)
    
    data %>%
      dplyr::group_by(variable) %>%
      dplyr::mutate(
        value = if(input$boxplot_center && input$boxplot_scale) {
          scale(value)
        } else if(input$boxplot_center) {
          value - mean(value, na.rm = TRUE)
        } else if(input$boxplot_scale) {
          value / sd(value, na.rm = TRUE)
        } else {
          value
        }
      ) %>%
      dplyr::ungroup()
  }
  
  output$boxplot_cat_filters <- renderUI({
    req(input$boxplot_filter_vars)
    req(length(input$boxplot_filter_vars) > 0)
    filter_list <- lapply(input$boxplot_filter_vars, function(var) {
      vals <- df[[var]]
      choices <- as.character(unique(vals[!is.na(vals)]))
      if(any(is.na(vals))) choices <- c(choices, "NA")
      choices <- sort(choices)
      default_selected <- if(input$include_null_boxplot) choices else choices[choices != "NA"]
      div(class = "well", style = "padding: 10px; margin-bottom: 15px; background-color: #f9f9f9;",
          h5(var, style = "font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px; color: #2C3E50;"),
          div(style = "max-height: 200px; overflow-y: auto;",
              checkboxGroupInput(inputId = paste0("boxplot_filter_", var), label = NULL,
                                 choices = choices, selected = default_selected, inline = FALSE)))
    })
    do.call(tagList, filter_list)
  })
  
  filtered_boxplot_data <- reactive({
    data <- df
    
    # Apply categorical filters if selected
    if(!is.null(input$boxplot_filter_vars) && length(input$boxplot_filter_vars) > 0) {
      for(var in input$boxplot_filter_vars) {
        sel <- input[[paste0("boxplot_filter_", var)]]
        if(!is.null(sel) && length(sel) > 0) {
          if("NA" %in% sel) {
            sel_real <- setdiff(sel, "NA")
            if(input$include_null_boxplot) {
              data <- data[data[[var]] %in% sel_real | is.na(data[[var]]), ]
            } else {
              data <- data[data[[var]] %in% sel_real, ]
            }
          } else {
            data <- data[data[[var]] %in% sel, ]
            if(!input$include_null_boxplot) {
              data <- data[!is.na(data[[var]]), ]
            }
          }
        }
      }
    }
    
    return(data)
  })
  
  # Add this MISSING boxplot_data function
  boxplot_data <- reactive({
    data <- filtered_boxplot_data()
    selected_vars <- input$boxplot_numeric_vars
    req(length(selected_vars) > 0)
    
    plot_data_list <- list()
    for(var in selected_vars) {
      if(var %in% names(data)) {
        vals <- data[[var]]
        if(is.character(vals)) {
          vals[vals %in% c("-99", "--", "NA")] <- NA
          vals <- suppressWarnings(as.numeric(vals))
        }
        
        temp_df <- data.frame(
          value = vals,
          variable = var,
          code = data$CODE,
          stringsAsFactors = FALSE
        )
        
        if(!is.null(input$boxplot_cat_vars_group) && input$boxplot_cat_vars_group != "None") {
          group_var <- input$boxplot_cat_vars_group
          if(group_var %in% names(data)) {
            temp_df$group <- as.character(data[[group_var]])
            temp_df$group[is.na(temp_df$group)] <- "MISSING"
          } else {
            temp_df$group <- "All"
          }
        } else {
          temp_df$group <- "All"
        }
        
        plot_data_list[[var]] <- temp_df
      }
    }
    
    result <- do.call(rbind, plot_data_list)
    result <- result[!is.na(result$value), ]
    result
  })
  
  add_outlier_flags <- function(data, iqr_multiplier) {
    if("group" %in% names(data)) {
      data %>% dplyr::group_by(variable, group) %>%
        dplyr::mutate(Q1 = quantile(value, 0.25, na.rm = TRUE), Q3 = quantile(value, 0.75, na.rm = TRUE),
                      IQR = Q3 - Q1, lower_bound = Q1 - iqr_multiplier * IQR,
                      upper_bound = Q3 + iqr_multiplier * IQR,
                      is_outlier = value < lower_bound | value > upper_bound) %>% dplyr::ungroup()
    } else {
      data %>% dplyr::group_by(variable) %>%
        dplyr::mutate(Q1 = quantile(value, 0.25, na.rm = TRUE), Q3 = quantile(value, 0.75, na.rm = TRUE),
                      IQR = Q3 - Q1, lower_bound = Q1 - iqr_multiplier * IQR,
                      upper_bound = Q3 + iqr_multiplier * IQR,
                      is_outlier = value < lower_bound | value > upper_bound) %>% dplyr::ungroup()
    }
  }
  
  output$boxplot_plot <- renderPlotly({
    req(input$boxplot_numeric_vars, length(input$boxplot_numeric_vars) > 0)
    
    df_plot <- boxplot_data()
    req(nrow(df_plot) > 0)
    
    # Apply centering/scaling
    df_plot <- apply_center_scale(df_plot)
    
    iqr_mult <- input$iqr_boxplot
    outlier_data <- add_outlier_flags(df_plot, iqr_mult)
    
    non_outliers <- outlier_data %>% dplyr::filter(!is_outlier)
    outliers <- outlier_data %>% dplyr::filter(is_outlier)
    
    transform_text <- dplyr::case_when(
      input$boxplot_center && input$boxplot_scale ~ "(Centered & Scaled)",
      input$boxplot_center ~ "(Centered)",
      input$boxplot_scale ~ "(Scaled)",
      TRUE ~ ""
    )
    null_text <- if(input$include_null_boxplot) " | NULL values included" else " | NULL values excluded"
    
    # Check if we have meaningful grouping
    has_grouping <- !is.null(input$boxplot_cat_vars_group) && 
      input$boxplot_cat_vars_group != "None" &&
      length(unique(outlier_data$group)) > 1
    
    if(has_grouping) {
      n_groups <- length(unique(outlier_data$group))
      
      # Create the base plot
      p <- ggplot2::ggplot() +
        # Boxplots with dodged position - this separates them by group
        ggplot2::geom_boxplot(data = non_outliers, 
                              ggplot2::aes(x = variable, y = value, fill = group),
                              outlier.shape = NA, 
                              na.rm = TRUE,
                              position = ggplot2::position_dodge(width = 0.8)) +
        # Outlier points with dodged position
        ggplot2::geom_point(data = outliers, 
                            ggplot2::aes(x = variable, y = value, color = group,
                                         text = paste("Variable:", variable, 
                                                      "<br>Group:", group,
                                                      "<br>Code:", code,
                                                      "<br>Value:", round(value, 2),
                                                      "<br>Type: OUTLIER")),
                            position = ggplot2::position_dodge(width = 0.8),
                            size = 2.5, 
                            alpha = 0.8, 
                            na.rm = TRUE) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste("Boxplot Analysis | IQR Multiplier:", iqr_mult, transform_text,
                        "| Grouped by:", input$boxplot_cat_vars_group),
          subtitle = paste(sum(outliers$is_outlier, na.rm = TRUE), 
                           "outliers detected across all groups", null_text),
          y = ifelse(input$boxplot_center | input$boxplot_scale, "Standardized Value", "Value"),
          x = "Variable",
          fill = input$boxplot_cat_vars_group,
          color = input$boxplot_cat_vars_group
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray50", size = 11),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          legend.position = "bottom"
        )
      
      # Calculate appropriate height
      height_val <- max(600, 400 + 100 * n_groups)
      
      ggplotly(p, tooltip = "text", height = height_val) %>% 
        plotly::layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
      
    } else {
      # No grouping - original plot
      p <- ggplot2::ggplot() +
        ggplot2::geom_boxplot(data = non_outliers, 
                              ggplot2::aes(x = variable, y = value),
                              fill = "#ADD8E6", 
                              color = "darkblue", 
                              outlier.shape = NA, 
                              na.rm = TRUE) +
        ggplot2::geom_point(data = outliers, 
                            ggplot2::aes(x = variable, y = value,
                                         text = paste("Variable:", variable, 
                                                      "<br>Code:", code, 
                                                      "<br>Value:", round(value, 2), 
                                                      "<br>Type: OUTLIER")),
                            color = "red", 
                            size = 2.5, 
                            alpha = 0.8, 
                            na.rm = TRUE) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste("Boxplot Analysis | IQR Multiplier:", iqr_mult, transform_text),
          subtitle = paste(sum(outliers$is_outlier, na.rm = TRUE), "outliers detected", null_text),
          y = ifelse(input$boxplot_center | input$boxplot_scale, "Standardized Value", "Value"),
          x = "Variable"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray50", size = 11),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
      
      ggplotly(p, tooltip = "text", height = 600) %>% 
        plotly::layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
    }
  })
  
  output$boxplot_stats <- renderPrint({
    req(input$boxplot_numeric_vars, length(input$boxplot_numeric_vars) > 0)
    data <- filtered_boxplot_data()
    valid_numeric <- input$boxplot_numeric_vars[input$boxplot_numeric_vars %in% names(data)]
    if(length(valid_numeric) == 0) { cat("No valid numeric variables selected"); return() }
    cat("Boxplot Analysis Summary\n========================\n\n")
    cat("Variables plotted:", paste(valid_numeric, collapse = ", "), "\n")
    if(!is.null(input$boxplot_cat_vars_group) && input$boxplot_cat_vars_group != "None") {
      cat("Grouped by:", input$boxplot_cat_vars_group, "\n")
      cat("Number of groups:", length(unique(data[[input$boxplot_cat_vars_group]])), "\n")
    }
    cat("\nIQR Multiplier:", input$iqr_boxplot, "\n")
    if(input$boxplot_center) cat("Data Centered: Yes\n")
    if(input$boxplot_scale) cat("Data Scaled: Yes\n")
    cat("\nOutlier Statistics:\n-------------------\n")
    for(var in valid_numeric) {
      vals <- data[[var]]
      if(is.character(vals)) { vals[vals %in% c("-99", "--", "NA")] <- NA; vals <- suppressWarnings(as.numeric(vals)) }
      vals <- vals[!is.na(vals)]
      if(length(vals) > 0) {
        Q1 <- quantile(vals, 0.25); Q3 <- quantile(vals, 0.75); IQR <- Q3 - Q1
        lower <- Q1 - input$iqr_boxplot * IQR; upper <- Q3 + input$iqr_boxplot * IQR
        n_outliers <- sum(vals < lower | vals > upper)
        cat("  ", var, ": ", n_outliers, " outliers (", round(100 * n_outliers / length(vals), 1), "% of data)\n", sep = "")
      } else { cat("  ", var, ": No valid data\n", sep = "") }
    }
    cat("\nFiltering:\n-----------\n")
    cat("  Observations shown:", nrow(data), "of", nrow(df), "\n")
    if(!is.null(input$boxplot_filter_vars) && length(input$boxplot_filter_vars) > 0) {
      cat("  Categorical filters applied:", paste(input$boxplot_filter_vars, collapse = ", "), "\n")
    }
    cat("  NULL values:", if(input$include_null_boxplot) "included" else "excluded", "\n")
  })
  
  observeEvent(input$reset_boxplot, {
    updateSliderInput(session, "iqr_boxplot", value = 1.5)
    updateCheckboxInput(session, "boxplot_center", value = FALSE)
    updateCheckboxInput(session, "boxplot_scale", value = FALSE)
    updatePickerInput(session, "boxplot_numeric_vars", selected = numeric_cols[1:3])
    updatePickerInput(session, "boxplot_cat_vars_group", selected = "None")
    updatePickerInput(session, "boxplot_filter_vars", selected = NULL)
    updateCheckboxInput(session, "include_null_boxplot", value = FALSE)
    showNotification("Boxplot settings reset to defaults", type = "default")
  })
  
  # ==========================================================================
  # CORRELATION ANALYSIS
  # ==========================================================================
  
  observe({
    updateSelectInput(session, "corr_method", selected = "pearson")
    updateSelectInput(session, "corr_order", selected = "AOE")
    updateSelectInput(session, "hclust_method", selected = "complete")
    updateCheckboxInput(session, "corr_abs", value = FALSE)
    updateCheckboxInput(session, "corr_show_values", value = TRUE)
    updateSelectInput(session, "corr_digits", selected = 2)
    updatePickerInput(session, "corr_numeric_vars", selected = numeric_cols)
    updatePickerInput(session, "corr_categorical_vars", selected = NULL)
    updateCheckboxGroupInput(session, "corr_mv_types", selected = c("neg99", "dash", "na_string"))
    updateCheckboxInput(session, "include_null_correlation", value = FALSE)
  })
  
  apply_corr_filters <- function(data, cat_vars, filter_prefix, include_null) {
    if(is.null(cat_vars) || length(cat_vars) == 0) return(data)
    for(var in cat_vars) {
      sel <- input[[paste0(filter_prefix, var)]]
      if(!is.null(sel) && length(sel) > 0) {
        if("NA" %in% sel) {
          sel_real <- setdiff(sel, "NA")
          if(include_null) {
            data <- data[data[[var]] %in% sel_real | is.na(data[[var]]), ]
          } else {
            data <- data[data[[var]] %in% sel_real, ]
          }
        } else {
          data <- data[data[[var]] %in% sel, ]
          if(!include_null) data <- data[!is.na(data[[var]]), ]
        }
      }
    }
    return(data)
  }
  
  output$corr_categorical_filters <- renderUI({
    req(input$corr_categorical_vars, length(input$corr_categorical_vars) > 0)
    filter_list <- lapply(input$corr_categorical_vars, function(var) {
      vals <- df[[var]]
      choices <- as.character(unique(vals[!is.na(vals)]))
      if(any(is.na(vals))) choices <- c(choices, "NA")
      choices <- sort(choices)
      default_selected <- if(input$include_null_correlation) choices else choices[choices != "NA"]
      div(class = "well", style = "padding: 10px; margin-bottom: 15px; background-color: #f9f9f9;",
          h5(var, style = "font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px; color: #2C3E50;"),
          div(style = "max-height: 200px; overflow-y: auto;",
              checkboxGroupInput(inputId = paste0("corr_filter_", var), label = NULL,
                                 choices = choices, selected = default_selected, inline = FALSE)))
    })
    do.call(tagList, filter_list)
  })
  
  output$corr_null_explanation <- renderUI({
    if(input$include_null_correlation) {
      div(style = "color: #0c5460; background-color: #d1ecf1; padding: 8px; border-radius: 4px; margin: 10px 0;",
          icon("info-circle"), HTML("<strong>Note:</strong> NULLs in categorical variables are included in filtering, but any row with a NULL in a <strong>numeric</strong> variable will be automatically removed for correlation calculation."))
    } else {
      div(style = "color: #155724; background-color: #d4edda; padding: 8px; border-radius: 4px; margin: 10px 0;",
          icon("check-circle"), HTML("<strong>Note:</strong> NULLs are excluded from categorical filtering. Any remaining NULLs in numeric variables will be automatically removed for correlation calculation."))
    }
  })
  
  corr_filtered_data <- reactive({
    data <- df
    if(!is.null(input$corr_categorical_vars) && length(input$corr_categorical_vars) > 0) {
      data <- apply_corr_filters(data, input$corr_categorical_vars, "corr_filter_", input$include_null_correlation)
    }
    data
  })
  
  output$correlation_plot <- renderPlotly({
    req(input$corr_numeric_vars, length(input$corr_numeric_vars) >= 2)
    num_vars <- length(input$corr_numeric_vars)
    data <- corr_filtered_data()
    num_data <- data[, input$corr_numeric_vars, drop = FALSE]
    for(col in names(num_data)) {
      if(is.character(num_data[[col]])) {
        treat_neg99 <- !is.null(input$corr_mv_types) && "neg99" %in% input$corr_mv_types
        treat_dash <- !is.null(input$corr_mv_types) && "dash" %in% input$corr_mv_types
        treat_na_string <- !is.null(input$corr_mv_types) && "na_string" %in% input$corr_mv_types
        temp_vals <- num_data[[col]]
        if(treat_neg99) temp_vals[temp_vals == "-99"] <- NA
        if(treat_dash) temp_vals[temp_vals == "--"] <- NA
        if(treat_na_string) temp_vals[temp_vals == "NA"] <- NA
        num_data[[col]] <- suppressWarnings(as.numeric(temp_vals))
      }
    }
    num_data <- na.omit(num_data)
    if(nrow(num_data) < 3) {
      return(plot_ly() %>% add_annotations(text = paste("Not enough complete observations. Found:", nrow(num_data), "complete cases"),
                                           x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)) %>%
               layout(title = "Correlation Heat Map"))
    }
    valid_cols <- sapply(num_data, function(col) sd(col, na.rm = TRUE) > 0)
    if(sum(valid_cols) < 2) {
      return(plot_ly() %>% add_annotations(text = "Need at least 2 variables with non-zero variance",
                                           x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)) %>%
               layout(title = "Correlation Heat Map"))
    }
    num_data <- num_data[, valid_cols, drop = FALSE]
    corr_matrix <- cor(num_data, method = input$corr_method, use = "pairwise.complete.obs")
    if(input$corr_abs) corr_matrix <- abs(corr_matrix)
    
    if(input$corr_order == "AOE") {
      if(requireNamespace("corrplot", quietly = TRUE)) {
        order_idx <- corrplot::corrMatOrder(corr_matrix, order = "AOE")
        corr_matrix <- corr_matrix[order_idx, order_idx]
      } else {
        hc <- hclust(as.dist(1 - abs(corr_matrix)), method = "complete")
        corr_matrix <- corr_matrix[hc$order, hc$order]
      }
    } else if(input$corr_order == "FPC") {
      if(requireNamespace("corrplot", quietly = TRUE)) {
        order_idx <- corrplot::corrMatOrder(corr_matrix, order = "FPC")
        corr_matrix <- corr_matrix[order_idx, order_idx]
      } else {
        hc <- hclust(as.dist(1 - abs(corr_matrix)), method = "complete")
        corr_matrix <- corr_matrix[hc$order, hc$order]
      }
    } else if(input$corr_order == "hclust") {
      hc <- hclust(as.dist(1 - abs(corr_matrix)), method = input$hclust_method)
      corr_matrix <- corr_matrix[hc$order, hc$order]
    } else if(input$corr_order == "alphabet") {
      order_idx <- order(colnames(corr_matrix))
      corr_matrix <- corr_matrix[order_idx, order_idx]
    }
    
    method_display <- switch(input$corr_method, "pearson" = "Pearson", "spearman" = "Spearman", "kendall" = "Kendall")
    order_display <- switch(input$corr_order, "original" = "Original", "AOE" = "Angular Order of Eigenvectors",
                            "FPC" = "First Principal Component", "hclust" = paste("Hierarchical Clustering (", input$hclust_method, ")", sep = ""),
                            "alphabet" = "Alphabetical")
    abs_display <- if(input$corr_abs) " | Absolute Values" else ""
    plot_title <- paste0("Correlation Heat Map<br>Method: ", method_display, " | Order: ", order_display, abs_display,
                         "<br>Complete Cases: ", nrow(num_data))
    
    colorscale <- if(input$corr_abs) {
      list(list(0.0, "white"), list(0.25, "#ffcccc"), list(0.5, "#ff9999"), list(0.75, "#ff4d4d"), list(1.0, "red"))
    } else {
      list(list(0, "blue"), list(0.5, "white"), list(1, "red"))
    }
    
    p <- plot_ly(z = corr_matrix, x = colnames(corr_matrix), y = colnames(corr_matrix), type = "heatmap",
                 colorscale = colorscale, zmin = if(input$corr_abs) 0 else -1, zmax = 1,
                 zmid = if(input$corr_abs) 0.5 else 0,
                 hovertemplate = "<b>%{x}</b> vs <b>%{y}</b><br>Correlation: %{z:.3f}<extra></extra>",
                 showscale = TRUE, colorbar = list(title = "Correlation", titleside = "right", tickformat = ".2f",
                                                   tickvals = seq(-1, 1, by = 0.5), ticktext = c("-1.0", "-0.5", "0.0", "0.5", "1.0"))) %>%
      layout(title = list(text = plot_title, font = list(size = 12), y = 0.95),
             xaxis = list(title = "", tickangle = -45, tickfont = list(size = ifelse(num_vars > 15, 8, ifelse(num_vars > 10, 9, 10))), side = "bottom"),
             yaxis = list(title = "", tickfont = list(size = ifelse(num_vars > 15, 8, ifelse(num_vars > 10, 9, 10))), autorange = "reversed"),
             margin = list(b = 120, t = 100, l = 120, r = 80))
    
    if(input$corr_show_values) {
      annotations_list <- list()
      n <- ncol(corr_matrix)
      for(i in 1:n) {
        for(j in 1:n) {
          font_size <- ifelse(n > 20, 8, ifelse(n > 12, 9, 10))
          corr_val <- corr_matrix[i, j]
          text_color <- ifelse(abs(corr_val) > 0.7, "white", "black")
          annotations_list <- append(annotations_list, list(list(x = colnames(corr_matrix)[j], y = colnames(corr_matrix)[i],
                                                                 text = sprintf(paste0("%.", input$corr_digits, "f"), corr_val),
                                                                 showarrow = FALSE, font = list(size = font_size, color = text_color),
                                                                 xanchor = "center", yanchor = "middle")))
        }
      }
      p <- p %>% layout(annotations = annotations_list)
    }
    return(p)
  })
  
  output$correlation_stats <- renderPrint({
    req(input$corr_numeric_vars, length(input$corr_numeric_vars) >= 2)
    data <- corr_filtered_data()
    valid_numeric <- input$corr_numeric_vars[input$corr_numeric_vars %in% names(data)]
    cat("Correlation Analysis Summary\n============================\n\n")
    cat("Method:", input$corr_method, "\n")
    cat("Variables:", length(valid_numeric), "selected\n")
    cat("  -", paste(valid_numeric, collapse = "\n  - "), "\n\n")
    num_data <- data[, valid_numeric, drop = FALSE]
    for(col in names(num_data)) {
      if(is.character(num_data[[col]])) {
        treat_neg99 <- !is.null(input$corr_mv_types) && "neg99" %in% input$corr_mv_types
        treat_dash <- !is.null(input$corr_mv_types) && "dash" %in% input$corr_mv_types
        treat_na_string <- !is.null(input$corr_mv_types) && "na_string" %in% input$corr_mv_types
        temp_vals <- num_data[[col]]
        if(treat_neg99) temp_vals[temp_vals == "-99"] <- NA
        if(treat_dash) temp_vals[temp_vals == "--"] <- NA
        if(treat_na_string) temp_vals[temp_vals == "NA"] <- NA
        num_data[[col]] <- suppressWarnings(as.numeric(temp_vals))
      }
    }
    complete_cases <- sum(complete.cases(num_data))
    cat("Complete cases for correlation:", complete_cases, "\n")
    cat("Total observations after filtering:", nrow(data), "\n")
    if(complete_cases > 0) {
      corr_matrix <- cor(num_data, method = input$corr_method, use = "pairwise.complete.obs")
      corr_matrix[lower.tri(corr_matrix, diag = TRUE)] <- NA
      corr_flat <- as.vector(corr_matrix)
      names(corr_flat) <- paste(rep(rownames(corr_matrix), ncol(corr_matrix)), rep(colnames(corr_matrix), each = nrow(corr_matrix)), sep = " vs ")
      corr_flat <- corr_flat[!is.na(corr_flat)]
      if(length(corr_flat) > 0) {
        top_corr <- sort(abs(corr_flat), decreasing = TRUE)[1:min(5, length(corr_flat))]
        cat("\nStrongest Correlations:\n-----------------------\n")
        for(i in seq_along(top_corr)) {
          corr_name <- names(top_corr)[i]
          corr_val <- corr_flat[names(corr_flat) == corr_name][1]
          cat("  ", corr_name, ": ", round(corr_val, 3), "\n", sep = "")
        }
      }
    }
    cat("\nMissing Value Handling:\n-----------------------\n")
    if(!is.null(input$corr_mv_types)) {
      if("neg99" %in% input$corr_mv_types) cat("  -99 values treated as missing\n")
      if("dash" %in% input$corr_mv_types) cat("  -- values treated as missing\n")
      if("na_string" %in% input$corr_mv_types) cat("  'NA' strings treated as missing\n")
    }
    cat("  NULL values in categorical filters:", if(input$include_null_correlation) "included" else "excluded", "\n")
  })
  
  output$corr_obs_count <- renderText({
    data <- corr_filtered_data()
    if(is.null(input$corr_numeric_vars) || length(input$corr_numeric_vars) == 0) return("No numeric variables selected")
    valid_numeric <- input$corr_numeric_vars[input$corr_numeric_vars %in% names(data)]
    if(length(valid_numeric) < 2) return("Need at least 2 numeric variables for correlation")
    num_data <- data[, valid_numeric, drop = FALSE]
    for(col in names(num_data)) {
      if(is.character(num_data[[col]])) {
        treat_neg99 <- !is.null(input$corr_mv_types) && "neg99" %in% input$corr_mv_types
        treat_dash <- !is.null(input$corr_mv_types) && "dash" %in% input$corr_mv_types
        treat_na_string <- !is.null(input$corr_mv_types) && "na_string" %in% input$corr_mv_types
        temp_vals <- num_data[[col]]
        if(treat_neg99) temp_vals[temp_vals == "-99"] <- NA
        if(treat_dash) temp_vals[temp_vals == "--"] <- NA
        if(treat_na_string) temp_vals[temp_vals == "NA"] <- NA
        num_data[[col]] <- suppressWarnings(as.numeric(temp_vals))
      }
    }
    complete_cases <- sum(complete.cases(num_data))
    rows_with_na <- nrow(num_data) - complete_cases
    null_status <- if(input$include_null_correlation) "Categorical NULLs included" else "Categorical NULLs excluded"
    paste("Showing", nrow(data), "of", nrow(df), "observations |", null_status, "|",
          "Complete cases for correlation:", complete_cases, if(rows_with_na > 0) paste("| Rows with NAs:", rows_with_na) else "")
  })
  
  observeEvent(input$reset_correlation, {
    updateSelectInput(session, "corr_method", selected = "pearson")
    updateSelectInput(session, "corr_order", selected = "AOE")
    updateSelectInput(session, "hclust_method", selected = "complete")
    updateCheckboxInput(session, "corr_abs", value = FALSE)
    updateCheckboxInput(session, "corr_show_values", value = TRUE)
    updateSelectInput(session, "corr_digits", selected = 2)
    updatePickerInput(session, "corr_numeric_vars", selected = numeric_cols)
    updatePickerInput(session, "corr_categorical_vars", selected = NULL)
    updateCheckboxGroupInput(session, "corr_mv_types", selected = c("neg99", "dash", "na_string"))
    updateCheckboxInput(session, "include_null_correlation", value = FALSE)
    showNotification("Correlation settings reset to defaults", type = "default")
  })
  
  # ==========================================================================
  # RISING VALUES
  # ==========================================================================
  
  observe({
    updatePickerInput(session, "rv_numeric_vars", selected = numeric_cols)
    updatePickerInput(session, "rv_categorical_vars", selected = NULL)
    updateMaterialSwitch(session, "rv_standardise", value = TRUE)
    updateMaterialSwitch(session, "rv_include_null", value = TRUE)
    updateCheckboxGroupInput(session, "rv_mv_types", selected = c("neg99", "dash", "na_string"))
  })
  
  output$rv_categorical_filters <- renderUI({
    req(input$rv_categorical_vars, length(input$rv_categorical_vars) > 0)
    filter_list <- lapply(input$rv_categorical_vars, function(var) {
      vals <- df[[var]]
      choices <- as.character(unique(vals[!is.na(vals)]))
      if(any(is.na(vals))) choices <- c(choices, "NA")
      choices <- sort(choices)
      div(class = "well", style = "padding: 10px; margin-bottom: 15px; background-color: #f9f9f9;",
          h5(var, style = "font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px; color: #2C3E50;"),
          div(style = "max-height: 200px; overflow-y: auto;",
              checkboxGroupInput(inputId = paste0("rv_filter_", var), label = NULL,
                                 choices = choices, selected = if(input$rv_include_null) choices else choices[choices != "NA"], inline = FALSE)))
    })
    do.call(tagList, filter_list)
  })
  
  observeEvent(input$rv_include_null, {
    req(input$rv_categorical_vars)
    for(var in input$rv_categorical_vars) {
      vals <- df[[var]]
      choices <- as.character(unique(vals[!is.na(vals)]))
      if(any(is.na(vals))) choices <- c(choices, "NA")
      choices <- sort(choices)
      if(input$rv_include_null) {
        updateCheckboxGroupInput(session, paste0("rv_filter_", var), selected = choices)
      } else {
        updateCheckboxGroupInput(session, paste0("rv_filter_", var), selected = choices[choices != "NA"])
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  get_rv_include_null <- reactive({ if(is.null(input$rv_include_null)) return(FALSE); return(input$rv_include_null) })
  
  rv_filtered_data <- reactive({
    req(input$rv_numeric_vars, length(input$rv_numeric_vars) > 0)
    data <- df
    include_null <- get_rv_include_null()
    treat_neg99 <- !is.null(input$rv_mv_types) && "neg99" %in% input$rv_mv_types
    treat_dash <- !is.null(input$rv_mv_types) && "dash" %in% input$rv_mv_types
    treat_na_string <- !is.null(input$rv_mv_types) && "na_string" %in% input$rv_mv_types
    for(col in names(data)) {
      if(col %in% numeric_cols && is.character(data[[col]])) {
        temp_vals <- data[[col]]
        if(treat_neg99) temp_vals[temp_vals == "-99"] <- NA
        if(treat_dash) temp_vals[temp_vals == "--"] <- NA
        if(treat_na_string) temp_vals[temp_vals == "NA"] <- NA
        data[[col]] <- suppressWarnings(as.numeric(temp_vals))
      }
    }
    if(!is.null(input$rv_categorical_vars) && length(input$rv_categorical_vars) > 0) {
      for(var in input$rv_categorical_vars) {
        selected_vals <- input[[paste0("rv_filter_", var)]]
        if(!is.null(selected_vals) && length(selected_vals) > 0) {
          if("NA" %in% selected_vals) {
            selected_vals_real <- setdiff(selected_vals, "NA")
            if(length(selected_vals_real) > 0) {
              data <- data[data[[var]] %in% selected_vals_real | is.na(data[[var]]), ]
            } else {
              data <- data[is.na(data[[var]]), ]
            }
          } else {
            data <- data[data[[var]] %in% selected_vals, ]
            if(!include_null) data <- data[!is.na(data[[var]]), ]
          }
        }
      }
    }
    data[, input$rv_numeric_vars, drop = FALSE]
  })
  
  output$rising_plot <- renderPlotly({
    data <- rv_filtered_data()
    req(ncol(data) > 0)
    data <- data[, sapply(data, is.numeric), drop = FALSE]
    req(ncol(data) > 0)
    include_null <- get_rv_include_null()
    null_status <- if(include_null) "NULLs included" else "NULLs excluded"
    if(ncol(data) == 1) {
      var_name <- names(data)[1]
      sorted_vals <- sort(na.omit(data[,1]))
      if(length(sorted_vals) < 2) {
        return(plot_ly() %>% add_annotations(text = "Insufficient data<br>Need at least 2 non-NA values", x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)) %>%
                 layout(title = "Rising Value Chart"))
      }
      if(input$rv_standardise) {
        plot_vals <- scale(sorted_vals); ylab_text <- "Standardised Values (Z-score)"; title_suffix <- "(Standardised)"
      } else {
        plot_vals <- sorted_vals; ylab_text <- "Raw Values"; title_suffix <- "(Raw)"
      }
      x_vals <- seq(0, 100, length.out = length(plot_vals))
      plot_ly(x = x_vals, y = as.vector(plot_vals), type = "scatter", mode = "lines+markers",
              line = list(color = "blue", width = 2), marker = list(color = "blue", size = 4),
              name = var_name, hovertemplate = paste("<b>", var_name, "</b><br>Percentile: %{x:.1f}<br>Value: %{y:.3f}<extra></extra>")) %>%
        layout(title = list(text = paste("Rising Value Chart -", var_name, title_suffix)),
               xaxis = list(title = "Percentile", gridcolor = "lightgray"), yaxis = list(title = ylab_text, gridcolor = "lightgray"),
               plot_bgcolor = "white", annotations = list(list(x = 0.5, y = -0.12, text = paste(null_status, "| Non-NA values:", length(sorted_vals)),
                                                               showarrow = FALSE, xref = "paper", yref = "paper", font = list(size = 10))))
    } else {
      data <- data[rowSums(is.na(data)) < ncol(data), ]
      req(nrow(data) > 1)
      sorted_list <- lapply(data, function(col) sort(na.omit(col)))
      min_len <- min(sapply(sorted_list, length))
      sorted_trimmed <- do.call(cbind, lapply(sorted_list, function(x) x[1:min_len]))
      colnames(sorted_trimmed) <- names(data)
      if(input$rv_standardise) {
        plot_data <- scale(sorted_trimmed); ylab_text <- "Standardised Values (Z-score)"; title_suffix <- "(Standardised)"
      } else {
        plot_data <- sorted_trimmed; ylab_text <- "Raw Values"; title_suffix <- "(Raw)"
      }
      x_vals <- seq(0, 100, length.out = nrow(plot_data))
      p <- plot_ly()
      for(i in 1:ncol(plot_data)) {
        p <- add_trace(p, x = x_vals, y = plot_data[, i], type = "scatter", mode = "lines",
                       line = list(width = 1.5), name = colnames(plot_data)[i],
                       hovertemplate = paste("<b>", colnames(plot_data)[i], "</b><br>Percentile: %{x:.1f}<br>Value: %{y:.3f}<extra></extra>"))
      }
      p %>% layout(title = list(text = paste("Rising Value Chart", title_suffix)), xaxis = list(title = "Percentile", gridcolor = "lightgray"),
                   yaxis = list(title = ylab_text, gridcolor = "lightgray"), hovermode = "x unified", showlegend = TRUE,
                   legend = list(x = 1.02, y = 0.5, xanchor = "left"), plot_bgcolor = "white",
                   annotations = list(list(x = 0.5, y = -0.12, text = paste(null_status, "| Observations per variable:", min_len),
                                           showarrow = FALSE, xref = "paper", yref = "paper", font = list(size = 10))))
    }
  })
  
  output$rising_obs_count <- renderPrint({
    data <- rv_filtered_data()
    include_null <- get_rv_include_null()
    null_status <- if(include_null) "NULLs included" else "NULLs excluded"
    cat("Rising Values Summary\n=====================\n\n")
    cat("Variables selected:", ncol(data), "\n")
    cat("Variables with valid data:", sum(sapply(data, function(x) sum(!is.na(x)) > 0)), "\n")
    cat("NULL handling:", null_status, "\n\nVariable details:\n")
    for(var in names(data)) { cat("  ", var, ":", sum(!is.na(data[[var]])), "valid values\n") }
  })
  
  observeEvent(input$reset_rising, {
    updatePickerInput(session, "rv_numeric_vars", selected = numeric_cols)
    updatePickerInput(session, "rv_categorical_vars", selected = NULL)
    updateMaterialSwitch(session, "rv_standardise", value = TRUE)
    updateMaterialSwitch(session, "rv_include_null", value = TRUE)
    updateCheckboxGroupInput(session, "rv_mv_types", selected = c("neg99", "dash", "na_string"))
    showNotification("Rising values settings reset to defaults", type = "default")
  })
  
  # ==========================================================================
  # MISSING VALUES HEATMAP
  # ==========================================================================
  
  observe({
    updateCheckboxGroupInput(session, "mv_types", selected = c("neg99", "dash", "na_string"))
    updateSliderInput(session, "col_missing_threshold", value = 100)
    updateSliderInput(session, "row_missing_threshold", value = 15)
    updateRadioButtons(session, "heatmap_order", selected = "original")
  })
  
  original_var_order <- c("CODE", "GOVERN_TYPE", "POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", 
                          "AGE50_PROPTN", "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", 
                          "VAX_RATE", "HEALTHCARE_BASIS", "HEALTHCARE_COST", "DEATH_RATE", "OBS_TYPE")
  
  heatmap_data <- reactive({
    selected_cat <- if(!is.null(input$heatmap_cat_vars)) input$heatmap_cat_vars else character(0)
    selected_num <- if(!is.null(input$heatmap_numeric_vars)) input$heatmap_numeric_vars else character(0)
    selected_vars <- c(selected_num, selected_cat)
    selected_vars <- selected_vars[!selected_vars %in% c("CODE", "OBS_TYPE")]
    req(length(selected_vars) > 0)
    code_column <- df$CODE
    plot_df <- df[, selected_vars, drop = FALSE]
    col_threshold <- if(!is.null(input$col_missing_threshold)) input$col_missing_threshold else 100
    row_threshold <- if(!is.null(input$row_missing_threshold)) input$row_missing_threshold else 5
    col_missing_pct <- apply(plot_df, 2, function(x) { vals <- as.character(x); sum(vals %in% c("-99", "--", "NA") | is.na(vals)) / length(vals) * 100 })
    plot_df <- plot_df[, col_missing_pct <= col_threshold, drop = FALSE]
    row_missing_count <- apply(plot_df, 1, function(x) { vals <- as.character(x); sum(vals %in% c("-99", "--", "NA") | is.na(vals)) })
    keep_rows <- row_missing_count <= row_threshold
    plot_df <- plot_df[keep_rows, , drop = FALSE]
    code_column <- code_column[keep_rows]
    var_order <- original_var_order[original_var_order %in% names(plot_df)]
    plot_df <- plot_df[, var_order, drop = FALSE]
    show_neg99 <- !is.null(input$mv_types) && "neg99" %in% input$mv_types
    show_dash <- !is.null(input$mv_types) && "dash" %in% input$mv_types
    show_na_string <- !is.null(input$mv_types) && "na_string" %in% input$mv_types
    color_matrix <- matrix("#ADD8E6", nrow = nrow(plot_df), ncol = ncol(plot_df))
    colnames(color_matrix) <- names(plot_df)
    has_neg99 <- FALSE; has_dash <- FALSE; has_na_string <- FALSE; has_r_na <- FALSE
    for(i in 1:nrow(plot_df)) {
      for(j in 1:ncol(plot_df)) {
        val <- plot_df[i, j]
        if(is.na(val)) { has_r_na <- TRUE; color_matrix[i, j] <- "#808080" }
        else if(val == "-99") { has_neg99 <- TRUE; color_matrix[i, j] <- "#FF69B4" }
        else if(val == "--") { has_dash <- TRUE; color_matrix[i, j] <- "#32CD32" }
        else if(val == "NA") { has_na_string <- TRUE; color_matrix[i, j] <- "#FF0000" }
      }
    }
    if(!show_neg99) { for(i in 1:nrow(plot_df)) { for(j in 1:ncol(plot_df)) { if(plot_df[i, j] == "-99") color_matrix[i, j] <- "#ADD8E6" } } }
    if(!show_dash) { for(i in 1:nrow(plot_df)) { for(j in 1:ncol(plot_df)) { if(plot_df[i, j] == "--") color_matrix[i, j] <- "#ADD8E6" } } }
    if(!show_na_string) { for(i in 1:nrow(plot_df)) { for(j in 1:ncol(plot_df)) { if(plot_df[i, j] == "NA") color_matrix[i, j] <- "#ADD8E6" } } }
    missing_pcts <- sapply(names(plot_df), function(col) { vals <- as.character(plot_df[[col]]); round(100 * sum(vals %in% c("-99", "--", "NA") | is.na(vals)) / nrow(plot_df), 1) })
    if(!is.null(input$heatmap_order) && input$heatmap_order == "desc") {
      order_idx <- order(missing_pcts, decreasing = TRUE)
      color_matrix <- color_matrix[, order_idx, drop = FALSE]
      missing_pcts <- missing_pcts[order_idx]
    }
    var_labels <- paste0(colnames(color_matrix), " (", missing_pcts, "%)")
    list(color_matrix = color_matrix, var_labels = var_labels, n_vars = ncol(color_matrix), n_obs = nrow(color_matrix),
         plot_df = plot_df, code_column = code_column, has_neg99 = has_neg99, has_dash = has_dash,
         has_na_string = has_na_string, has_r_na = has_r_na, missing_pcts = missing_pcts,
         show_neg99 = show_neg99, show_dash = show_dash, show_na_string = show_na_string)
  })
  
  output$heatmap_plot <- renderPlotly({
    heatmap_info <- heatmap_data()
    req(heatmap_info$n_vars > 0, heatmap_info$n_obs > 0)
    color_matrix <- heatmap_info$color_matrix; var_labels <- heatmap_info$var_labels
    n_vars <- heatmap_info$n_vars; n_obs <- heatmap_info$n_obs
    plot_df <- heatmap_info$plot_df; code_column <- heatmap_info$code_column
    if(n_obs > 100) { tickvals <- seq(1, n_obs, by = 20); ticktext <- seq(1, n_obs, by = 20)
    } else if(n_obs > 50) { tickvals <- seq(1, n_obs, by = 10); ticktext <- seq(1, n_obs, by = 10)
    } else if(n_obs > 20) { tickvals <- seq(1, n_obs, by = 5); ticktext <- seq(1, n_obs, by = 5)
    } else { tickvals <- 1:n_obs; ticktext <- 1:n_obs }
    legend_colors <- list(Present = "#ADD8E6", Neg99 = "#FF69B4", Dash = "#32CD32", NA_String = "#FF0000", R_NA = "#808080")
    legend_items <- 1
    if(heatmap_info$has_neg99) legend_items <- legend_items + 1
    if(heatmap_info$has_dash) legend_items <- legend_items + 1
    if(heatmap_info$has_na_string) legend_items <- legend_items + 1
    if(heatmap_info$has_r_na) legend_items <- legend_items + 1
    total_width <- 0.8; start_x <- 0.15; spacing <- total_width / legend_items
    key_annotations <- list(list(x = 0.5, y = 1.12, text = "Missingness Visualisation:", showarrow = FALSE,
                                 xref = "paper", yref = "paper", font = list(size = 16, color = "#2C3E50", weight = "bold")))
    current_x <- start_x
    key_annotations <- append(key_annotations, list(list(x = current_x, y = 1.06, text = "Present", showarrow = FALSE,
                                                         xref = "paper", yref = "paper", font = list(color = legend_colors$Present, size = 12, weight = "bold"))))
    current_x <- current_x + spacing
    if(heatmap_info$has_neg99) {
      key_annotations <- append(key_annotations, list(list(x = current_x, y = 1.06, text = "-99", showarrow = FALSE,
                                                           xref = "paper", yref = "paper", font = list(color = legend_colors$Neg99, size = 12, weight = "bold"))))
      current_x <- current_x + spacing
    }
    if(heatmap_info$has_dash) {
      key_annotations <- append(key_annotations, list(list(x = current_x, y = 1.06, text = "--", showarrow = FALSE,
                                                           xref = "paper", yref = "paper", font = list(color = legend_colors$Dash, size = 12, weight = "bold"))))
      current_x <- current_x + spacing
    }
    if(heatmap_info$has_na_string) {
      key_annotations <- append(key_annotations, list(list(x = current_x, y = 1.06, text = "NA string", showarrow = FALSE,
                                                           xref = "paper", yref = "paper", font = list(color = legend_colors$NA_String, size = 12, weight = "bold"))))
      current_x <- current_x + spacing
    }
    if(heatmap_info$has_r_na) {
      key_annotations <- append(key_annotations, list(list(x = current_x, y = 1.06, text = "R NA", showarrow = FALSE,
                                                           xref = "paper", yref = "paper", font = list(color = legend_colors$R_NA, size = 12, weight = "bold"))))
    }
    hover_text <- matrix("", nrow = n_obs, ncol = n_vars)
    for(i in 1:n_obs) {
      for(j in 1:n_vars) {
        val <- plot_df[i, j]; var_name <- colnames(plot_df)[j]; code_val <- code_column[i]
        if(is.na(val)) hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: R NA")
        else if(val == "-99") hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: -99")
        else if(val == "--") hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: --")
        else if(val == "NA") hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: NA string")
        else hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: Present")
      }
    }
    unique_colors <- unique(as.vector(color_matrix))
    color_scale <- list()
    for(i in seq_along(unique_colors)) color_scale[[i]] <- list((i-1)/(length(unique_colors)-1), unique_colors[i])
    color_to_num <- setNames(seq_along(unique_colors), unique_colors)
    numeric_matrix <- matrix(color_to_num[color_matrix], nrow = n_obs, ncol = n_vars)
    p <- plot_ly(z = numeric_matrix, x = var_labels, y = 1:n_obs, type = "heatmap", colorscale = color_scale,
                 text = hover_text, hoverinfo = "text", showscale = FALSE) %>%
      layout(annotations = key_annotations, xaxis = list(title = "Variables", tickangle = -45,
                                                         tickfont = list(size = ifelse(n_vars > 15, 8, ifelse(n_vars > 10, 9, 11))), titlefont = list(size = 14), side = "bottom"),
             yaxis = list(title = "Observation Index", tickmode = "array", tickvals = tickvals, ticktext = ticktext,
                          titlefont = list(size = 14), autorange = "reversed"),
             margin = list(b = ifelse(n_vars > 15, 150, ifelse(n_vars > 10, 130, 120)), l = 80, t = 120, r = 50),
             title = list(text = paste("Missing Values Heatmap -", n_obs, "Observations,", n_vars, "Variables"), font = list(size = 16), y = 0.98))
    return(p)
  })
  
  output$heatmap_summary <- renderPrint({
    heatmap_info <- heatmap_data()
    req(!is.null(heatmap_info))
    plot_df <- heatmap_info$plot_df; n_obs <- heatmap_info$n_obs; n_vars <- heatmap_info$n_vars; missing_pcts <- heatmap_info$missing_pcts
    cat("===============================================================================\n")
    cat("                    MISSING VALUES HEATMAP SUMMARY\n")
    cat("===============================================================================\n\n")
    cat("OVERALL STATISTICS\n-------------------------------------------------------------------------------\n")
    cat("  Total observations:", n_obs, "\n"); cat("  Total variables:", n_vars, "\n"); cat("  Total cells:", n_obs * n_vars, "\n\n")
    neg99_count <- sum(sapply(plot_df, function(x) sum(x == "-99", na.rm = TRUE)))
    dash_count <- sum(sapply(plot_df, function(x) sum(x == "--", na.rm = TRUE)))
    na_string_count <- sum(sapply(plot_df, function(x) sum(x == "NA", na.rm = TRUE)))
    r_na_count <- sum(sapply(plot_df, function(x) sum(is.na(x))))
    total_missing <- neg99_count + dash_count + na_string_count + r_na_count
    cat("MISSING VALUES BY TYPE\n-------------------------------------------------------------------------------\n")
    cat("  -99 values:     ", sprintf("%6d", neg99_count), " (", sprintf("%5.1f", 100 * neg99_count / (n_obs * n_vars)), "%)\n")
    cat("  -- values:      ", sprintf("%6d", dash_count), " (", sprintf("%5.1f", 100 * dash_count / (n_obs * n_vars)), "%)\n")
    cat("  NA strings:     ", sprintf("%6d", na_string_count), " (", sprintf("%5.1f", 100 * na_string_count / (n_obs * n_vars)), "%)\n")
    cat("  R NA:           ", sprintf("%6d", r_na_count), " (", sprintf("%5.1f", 100 * r_na_count / (n_obs * n_vars)), "%)\n")
    cat("  TOTAL MISSING:  ", sprintf("%6d", total_missing), " (", sprintf("%5.1f", 100 * total_missing / (n_obs * n_vars)), "%)\n\n")
    cat("COLUMN STATISTICS (Variables with highest missing %)\n-------------------------------------------------------------------------------\n")
    col_stats <- data.frame(Variable = names(missing_pcts), Missing_Pct = missing_pcts)
    col_stats <- col_stats[order(-col_stats$Missing_Pct), ]
    top_cols <- head(col_stats, min(5, nrow(col_stats)))
    for(i in 1:nrow(top_cols)) cat("  ", sprintf("%-20s", top_cols$Variable[i]), ": ", sprintf("%5.1f", top_cols$Missing_Pct[i]), "%\n")
    cat("\nAPPLIED THRESHOLDS\n-------------------------------------------------------------------------------\n")
    cat("  Column missing percentage threshold:", input$col_missing_threshold, "%\n")
    cat("  Row missing count threshold:", input$row_missing_threshold, "\n")
  })
  
  observeEvent(input$reset_heatmap, {
    updateCheckboxGroupInput(session, "mv_types", selected = c("neg99", "dash", "na_string"))
    updateSliderInput(session, "col_missing_threshold", value = 100)
    updateSliderInput(session, "row_missing_threshold", value = 5)
    updatePickerInput(session, "heatmap_cat_vars", selected = c("GOVERN_TYPE", "HEALTHCARE_BASIS", "CODE", "OBS_TYPE"))
    updatePickerInput(session, "heatmap_numeric_vars", selected = numeric_cols)
    updateRadioButtons(session, "heatmap_order", selected = "original")
    showNotification("Heatmap settings reset to defaults", type = "default")
  })
  
  # ==========================================================================
  # DISTRIBUTION PLOTS
  # ==========================================================================
  
  observe({
    updateSliderInput(session, "dist_bins", value = 30)
    updateSelectInput(session, "dist_var", selected = "VAX_RATE")
    updateCheckboxGroupInput(session, "dist_mv_types", selected = c("neg99", "dash", "na_string"))
  })
  
  output$distribution_plot <- renderPlotly({
    var <- input$dist_var
    req(var %in% names(df))
    vals <- df[[var]]; codes <- df$CODE; is_numeric_var <- var %in% numeric_cols
    if(is_numeric_var) {
      vals_char <- as.character(vals)
      treat_neg99 <- !is.null(input$dist_mv_types) && "neg99" %in% input$dist_mv_types
      treat_dash <- !is.null(input$dist_mv_types) && "dash" %in% input$dist_mv_types
      treat_na_string <- !is.null(input$dist_mv_types) && "na_string" %in% input$dist_mv_types
      if(treat_neg99) vals_char[vals_char == "-99"] <- NA
      if(treat_dash) vals_char[vals_char == "--"] <- NA
      if(treat_na_string) vals_char[vals_char == "NA"] <- NA
      num_vals <- suppressWarnings(as.numeric(vals_char))
      plot_df <- data.frame(value = num_vals, code = codes, stringsAsFactors = FALSE)
      plot_df <- plot_df[!is.na(plot_df$value), ]
      req(nrow(plot_df) > 0)
      p <- ggplot(plot_df, aes(x = value)) +
        geom_histogram(bins = input$dist_bins, fill = "#ADD8E6", color = "white", alpha = 0.7,
                       aes(text = paste0("Range: ", round(after_stat(xmin), 2), " to ", round(after_stat(xmax), 2),
                                         "<br>Count: ", after_stat(count),
                                         "<br>Percentage: ", round(after_stat(count / sum(count) * 100), 1), "%"))) +
        geom_rug(aes(x = value, text = paste0("CODE: ", code, "<br>Value: ", round(value, 2))),
                 alpha = 0.5, color = "#4d41ff", size = 0.5, sides = "b") +
        labs(title = paste("Distribution of", var, "(", input$dist_bins, "bins)"), x = var, y = "Count") +
        theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      ggplotly(p, tooltip = "text")
    } else {
      vals_char <- as.character(vals)
      treat_neg99 <- !is.null(input$dist_mv_types) && "neg99" %in% input$dist_mv_types
      treat_dash <- !is.null(input$dist_mv_types) && "dash" %in% input$dist_mv_types
      treat_na_string <- !is.null(input$dist_mv_types) && "na_string" %in% input$dist_mv_types
      clean_vals <- vals_char
      if(treat_neg99) clean_vals[clean_vals == "-99"] <- NA
      if(treat_dash) clean_vals[clean_vals == "--"] <- NA
      if(treat_na_string) clean_vals[clean_vals == "NA"] <- NA
      freq_table <- as.data.frame(table(clean_vals, useNA = "ifany"))
      names(freq_table) <- c("category", "count")
      freq_table$category <- as.character(freq_table$category)
      freq_table$category[is.na(freq_table$category)] <- "Missing"
      freq_table$percentage <- round(100 * freq_table$count / sum(freq_table$count), 1)
      freq_table$tooltip <- paste0("Category: ", freq_table$category, "<br>Count: ", freq_table$count,
                                   "<br>Percentage: ", freq_table$percentage, "%")
      p <- ggplot(freq_table, aes(x = category, y = count, fill = category, text = tooltip)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        labs(title = paste("Bar Plot of", var), x = var, y = "Count") +
        theme_minimal() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
        coord_flip()
      ggplotly(p, tooltip = "text")
    }
  })
  
  output$distribution_stats <- renderPrint({
    var <- input$dist_var
    req(var %in% names(df))
    vals <- df[[var]]; is_numeric_var <- var %in% numeric_cols
    treat_neg99 <- !is.null(input$dist_mv_types) && "neg99" %in% input$dist_mv_types
    treat_dash <- !is.null(input$dist_mv_types) && "dash" %in% input$dist_mv_types
    treat_na_string <- !is.null(input$dist_mv_types) && "na_string" %in% input$dist_mv_types
    if(is_numeric_var) {
      vals_char <- as.character(vals)
      if(treat_neg99) vals_char[vals_char == "-99"] <- NA
      if(treat_dash) vals_char[vals_char == "--"] <- NA
      if(treat_na_string) vals_char[vals_char == "NA"] <- NA
      num_vals <- suppressWarnings(as.numeric(vals_char))
      valid_vals <- num_vals[!is.na(num_vals)]
      cat("Distribution Statistics for", var, "\n================================\n\n")
      if(length(valid_vals) > 0) {
        cat("Valid observations:", length(valid_vals), "\n")
        cat("Minimum:", round(min(valid_vals), 2), "\n")
        cat("Median:", round(median(valid_vals), 2), "\n")
        cat("Mean:", round(mean(valid_vals), 2), "\n")
        cat("Maximum:", round(max(valid_vals), 2), "\n")
        cat("Standard Deviation:", round(sd(valid_vals), 2), "\n")
      } else cat("No valid numeric values found.\n")
    } else {
      vals_char <- as.character(vals)
      if(treat_neg99) vals_char[vals_char == "-99"] <- NA
      if(treat_dash) vals_char[vals_char == "--"] <- NA
      if(treat_na_string) vals_char[vals_char == "NA"] <- NA
      freq_table <- table(vals_char, useNA = "no")
      cat("Frequency Table for", var, "\n========================\n\n")
      for(i in seq_along(freq_table)) {
        pct <- round(100 * freq_table[i] / length(vals), 1)
        cat(names(freq_table)[i], ":", freq_table[i], "(", pct, "%)\n")
      }
    }
  })
  
  observeEvent(input$reset_distribution, {
    updateSliderInput(session, "dist_bins", value = 30)
    updateSelectInput(session, "dist_var", selected = "VAX_RATE")
    updateCheckboxGroupInput(session, "dist_mv_types", selected = c("neg99", "dash", "na_string"))
    showNotification("Distribution settings reset to defaults", type = "default")
  })
  
  # ==========================================================================
  # SCATTER PLOT
  # ==========================================================================
  
  observe({
    updateSelectInput(session, "scatter_x", selected = "GDP")
    updateSelectInput(session, "scatter_y", selected = "DEATH_RATE")
    updateSelectInput(session, "scatter_color", selected = "None")
    updateCheckboxGroupInput(session, "scatter_mv_types", selected = c("neg99", "dash", "na_string"))
  })
  
  output$scatter_plot <- renderPlotly({
    req(input$scatter_x, input$scatter_y)
    x_var <- input$scatter_x; y_var <- input$scatter_y; color_var <- input$scatter_color
    x_vals <- df[[x_var]]; y_vals <- df[[y_var]]; codes <- df$CODE
    treat_neg99 <- !is.null(input$scatter_mv_types) && "neg99" %in% input$scatter_mv_types
    treat_dash <- !is.null(input$scatter_mv_types) && "dash" %in% input$scatter_mv_types
    treat_na_string <- !is.null(input$scatter_mv_types) && "na_string" %in% input$scatter_mv_types
    if(is.character(x_vals)) {
      if(treat_neg99) x_vals[x_vals == "-99"] <- NA
      if(treat_dash) x_vals[x_vals == "--"] <- NA
      if(treat_na_string) x_vals[x_vals == "NA"] <- NA
      x_vals <- suppressWarnings(as.numeric(x_vals))
    }
    if(is.character(y_vals)) {
      if(treat_neg99) y_vals[y_vals == "-99"] <- NA
      if(treat_dash) y_vals[y_vals == "--"] <- NA
      if(treat_na_string) y_vals[y_vals == "NA"] <- NA
      y_vals <- suppressWarnings(as.numeric(y_vals))
    }
    plot_df <- data.frame(x = x_vals, y = y_vals, code = codes)
    if(color_var != "None") plot_df$color <- df[[color_var]]
    plot_df <- na.omit(plot_df)
    if(color_var != "None") {
      p <- ggplot(plot_df, aes(x = x, y = y, color = color, text = paste("Code:", code, "<br>", x_var, ":", round(x, 2),
                                                                         "<br>", y_var, ":", round(y, 2), "<br>", color_var, ":", color))) +
        geom_point(alpha = 0.6, size = 2)
    } else {
      p <- ggplot(plot_df, aes(x = x, y = y, text = paste("Code:", code, "<br>", x_var, ":", round(x, 2),
                                                          "<br>", y_var, ":", round(y, 2)))) +
        geom_point(alpha = 0.6, size = 2, color = "#48b1d4")
    }
    p <- p + labs(title = paste(y_var, "vs", x_var), x = x_var, y = y_var) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p, tooltip = "text")
  })
  
  output$scatter_correlation <- renderPrint({
    x_var <- input$scatter_x; y_var <- input$scatter_y
    x_vals <- df[[x_var]]; y_vals <- df[[y_var]]
    treat_neg99 <- !is.null(input$scatter_mv_types) && "neg99" %in% input$scatter_mv_types
    treat_dash <- !is.null(input$scatter_mv_types) && "dash" %in% input$scatter_mv_types
    treat_na_string <- !is.null(input$scatter_mv_types) && "na_string" %in% input$scatter_mv_types
    if(is.character(x_vals)) {
      if(treat_neg99) x_vals[x_vals == "-99"] <- NA
      if(treat_dash) x_vals[x_vals == "--"] <- NA
      if(treat_na_string) x_vals[x_vals == "NA"] <- NA
      x_vals <- suppressWarnings(as.numeric(x_vals))
    }
    if(is.character(y_vals)) {
      if(treat_neg99) y_vals[y_vals == "-99"] <- NA
      if(treat_dash) y_vals[y_vals == "--"] <- NA
      if(treat_na_string) y_vals[y_vals == "NA"] <- NA
      y_vals <- suppressWarnings(as.numeric(y_vals))
    }
    complete_idx <- complete.cases(x_vals, y_vals)
    cat("Scatter Plot Summary\n====================\n\n")
    cat("X-axis:", x_var, "\nY-axis:", y_var, "\n")
    if(input$scatter_color != "None") cat("Colored by:", input$scatter_color, "\n")
    cat("\nData Summary:\n  Complete cases:", sum(complete_idx), "\n  Missing/Excluded:", length(x_vals) - sum(complete_idx), "\n")
    if(sum(complete_idx) >= 3) {
      cor_val <- cor(x_vals[complete_idx], y_vals[complete_idx], use = "complete.obs")
      cat("\nCorrelation:\n  Pearson correlation:", round(cor_val, 4), "\n  R-squared:", round(cor_val^2, 4), "\n")
    }
  })
  
  observeEvent(input$reset_scatter, {
    updateSelectInput(session, "scatter_x", selected = "GDP")
    updateSelectInput(session, "scatter_y", selected = "DEATH_RATE")
    updateSelectInput(session, "scatter_color", selected = "None")
    updateCheckboxGroupInput(session, "scatter_mv_types", selected = c("neg99", "dash", "na_string"))
    showNotification("Scatter plot settings reset to defaults", type = "default")
  })
  
  # ==========================================================================
  # GGPAIRS PLOT
  # ==========================================================================
  
  observe({
    updatePickerInput(session, "ggpairs_numeric", selected = numeric_cols[1:4])
    updateSelectInput(session, "ggpairs_color", selected = "None")
    updateCheckboxGroupInput(session, "ggpairs_mv_types", selected = c("neg99", "dash", "na_string"))
  })
  
  observeEvent(input$deselect_numeric, {
    updatePickerInput(session, "ggpairs_numeric", selected = character(0))
  })
  
  output$ggpairs_total_count <- renderUI({
    num_count <- length(input$ggpairs_numeric); total <- num_count
    if(total > 11) div(style = "color: #dc3545; font-weight: bold; margin: 5px 0;", icon("exclamation-triangle"), paste("Total:", total, "of 11 variables selected (too many)"))
    else if(total == 0) div(style = "color: #6c757d; margin: 5px 0;", icon("info-circle"), "No variables selected")
    else div(style = "color: #28a745; margin: 5px 0;", icon("check-circle"), paste("Total:", total, "of 11 variables selected"))
  })
  
  ggpairs_selected_vars <- reactive({ input$ggpairs_numeric })
  
  ggpairs_filtered_data <- reactive({
    data <- df
    treat_neg99 <- !is.null(input$ggpairs_mv_types) && "neg99" %in% input$ggpairs_mv_types
    treat_dash <- !is.null(input$ggpairs_mv_types) && "dash" %in% input$ggpairs_mv_types
    treat_na_string <- !is.null(input$ggpairs_mv_types) && "na_string" %in% input$ggpairs_mv_types
    for(col in names(data)) {
      if(col %in% numeric_cols && is.character(data[[col]])) {
        temp_vals <- data[[col]]
        if(treat_neg99) temp_vals[temp_vals == "-99"] <- NA
        if(treat_dash) temp_vals[temp_vals == "--"] <- NA
        if(treat_na_string) temp_vals[temp_vals == "NA"] <- NA
        data[[col]] <- suppressWarnings(as.numeric(temp_vals))
      }
    }
    data
  })
  
  output$ggpairs_plot <- renderPlotly({
    vars <- ggpairs_selected_vars()
    if(length(vars) < 2) {
      return(plot_ly() %>% add_annotations(text = paste("Please select at least 2 numeric variables.\nCurrently selected:", length(vars)),
                                           x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14, color = "gray")) %>%
               layout(title = "GGpairs Plot"))
    }
    if(length(vars) > 11) {
      return(plot_ly() %>% add_annotations(text = paste("Too many variables selected. Maximum is 11.\nCurrently selected:", length(vars)),
                                           x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14, color = "gray")) %>%
               layout(title = "GGpairs Plot"))
    }
    data <- ggpairs_filtered_data()
    color_var <- if(input$ggpairs_color == "None") NULL else input$ggpairs_color
    plot_data <- data[, vars, drop = FALSE]
    complete_idx <- complete.cases(plot_data[, vars])
    plot_data <- plot_data[complete_idx, ]
    if(nrow(plot_data) < 3) {
      return(plot_ly() %>% add_annotations(text = paste("Insufficient complete observations.\nFound:", nrow(plot_data), "complete cases\nNeed at least 3"),
                                           x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14, color = "gray")) %>%
               layout(title = "GGpairs Plot"))
    }
    code_vector <- as.character(data$CODE)
    code_vector <- code_vector[complete_idx]
    code_vector[is.na(code_vector)] <- "Unknown"
    if(!is.null(color_var)) plot_data$color <- as.factor(data[[color_var]][complete_idx])
    lower_fn <- function(data, mapping, ...) {
      x_var <- rlang::as_name(mapping$x); y_var <- rlang::as_name(mapping$y)
      data$CODE_local <- code_vector[seq_len(nrow(data))]
      data$tooltip_text <- paste0("CODE: ", data$CODE_local, "<br>", x_var, ": ", round(data[[x_var]], 2),
                                  "<br>", y_var, ": ", round(data[[y_var]], 2))
      ggplot(data = data, mapping = mapping) + geom_point(aes(text = tooltip_text), alpha = 0.5, size = 1.5)
    }
    if(!is.null(color_var)) {
      ggp <- ggpairs(plot_data, columns = which(names(plot_data) %in% vars), mapping = aes(color = color),
                     title = paste("GGpairs Plot - Colored by", color_var), progress = FALSE,
                     upper = list(continuous = wrap("cor", size = 3)), lower = list(continuous = wrap(lower_fn)),
                     diag = list(continuous = wrap("densityDiag", alpha = 0.5)))
    } else {
      ggp <- ggpairs(plot_data, columns = which(names(plot_data) %in% vars), title = "GGpairs Plot", progress = FALSE,
                     upper = list(continuous = wrap("cor", size = 3)), lower = list(continuous = wrap(lower_fn)),
                     diag = list(continuous = wrap("densityDiag", alpha = 0.5)))
    }
    ggp <- ggp + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                       strip.background = element_rect(fill = "lightgray"), strip.text = element_text(face = "bold", size = 10))
    ggplotly(ggp, height = 800, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white", font = list(size = 10)),
                                                             margin = list(t = 80, l = 50, r = 50, b = 50))
  })
  
  output$ggpairs_obs_count <- renderPrint({
    data <- ggpairs_filtered_data(); vars <- ggpairs_selected_vars()
    if(length(vars) >= 2) {
      plot_data <- data[, vars, drop = FALSE]
      complete_cases <- sum(complete.cases(plot_data))
      cat("GGpairs Plot Summary\n====================\n\n")
      cat("Variables:", paste(vars, collapse = ", "), "\n")
      cat("Complete cases:", complete_cases, "\n")
      cat("Removed:", nrow(plot_data) - complete_cases, "\n")
    } else cat("Select at least 2 variables.\n")
  })
  
  observeEvent(input$reset_ggpairs, {
    updatePickerInput(session, "ggpairs_numeric", selected = numeric_cols[1:4])
    updateSelectInput(session, "ggpairs_color", selected = "None")
    updateCheckboxGroupInput(session, "ggpairs_mv_types", selected = c("neg99", "dash", "na_string"))
    showNotification("GGpairs settings reset", type = "default")
  })
  
  # ==========================================================================
  # TABPLOT
  # ==========================================================================
  
  observe({
    updateSliderInput(session, "tabplot_nbins", value = 100)
    updateRadioButtons(session, "tabplot_code_order", selected = "asc")
    updateMaterialSwitch(session, "tabplot_showNA", value = TRUE)
    updateMaterialSwitch(session, "tabplot_include_null", value = TRUE)
    updatePickerInput(session, "tabplot_numeric_vars", selected = numeric_cols[1:3])
    updatePickerInput(session, "tabplot_categorical_vars", selected = categorical_cols[categorical_cols != "CODE"][1:2])
    updateCheckboxGroupInput(session, "tabplot_mv_types", selected = c("neg99", "dash", "na_string"))
  })
  
  output$tabplot_categorical_filters <- renderUI({
    req(input$tabplot_categorical_vars, length(input$tabplot_categorical_vars) > 0)
    filter_list <- lapply(input$tabplot_categorical_vars, function(var) {
      vals <- df[[var]]; char_vals <- as.character(vals); all_vals <- unique(char_vals); choices <- sort(all_vals)
      div(class = "well", style = "padding: 10px; margin-bottom: 15px; background-color: #f9f9f9;",
          h5(var, style = "font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px; color: #2C3E50;"),
          div(style = "max-height: 200px; overflow-y: auto;",
              checkboxGroupInput(inputId = paste0("tabplot_filter_", var), label = NULL, choices = choices, selected = choices, inline = FALSE)))
    })
    do.call(tagList, filter_list)
  })
  
  tabplot_filtered_data <- reactive({
    req(input$tabplot_numeric_vars, length(input$tabplot_numeric_vars) > 0)
    data <- df
    treat_neg99 <- !is.null(input$tabplot_mv_types) && "neg99" %in% input$tabplot_mv_types
    treat_dash <- !is.null(input$tabplot_mv_types) && "dash" %in% input$tabplot_mv_types
    treat_na_string <- !is.null(input$tabplot_mv_types) && "na_string" %in% input$tabplot_mv_types
    for(col in names(data)) {
      if(col %in% numeric_cols && is.character(data[[col]])) {
        temp_vals <- data[[col]]
        if(treat_neg99) temp_vals[temp_vals == "-99"] <- NA
        if(treat_dash) temp_vals[temp_vals == "--"] <- NA
        if(treat_na_string) temp_vals[temp_vals == "NA"] <- NA
        data[[col]] <- suppressWarnings(as.numeric(temp_vals))
      }
    }
    for(col in names(data)) {
      if(col %in% categorical_cols && is.character(data[[col]])) {
        temp_vals <- as.character(data[[col]])
        if(treat_dash) temp_vals[temp_vals == "--"] <- "MISSING_DASH"
        if(treat_na_string) temp_vals[temp_vals == "NA"] <- "MISSING_NA_STRING"
        if(treat_neg99) temp_vals[temp_vals == "-99"] <- "MISSING_NEG99"
        temp_vals[is.na(temp_vals)] <- "MISSING_R_NA"
        data[[col]] <- temp_vals
      }
    }
    if(!is.null(input$tabplot_categorical_vars) && length(input$tabplot_categorical_vars) > 0) {
      for(var in input$tabplot_categorical_vars) {
        selected_vals <- input[[paste0("tabplot_filter_", var)]]
        if(!is.null(selected_vals) && length(selected_vals) > 0) {
          selected_vals_converted <- selected_vals
          if("--" %in% selected_vals && treat_dash) selected_vals_converted <- c(selected_vals_converted[selected_vals_converted != "--"], "MISSING_DASH")
          if("NA" %in% selected_vals && treat_na_string) selected_vals_converted <- c(selected_vals_converted[selected_vals_converted != "NA"], "MISSING_NA_STRING")
          if("-99" %in% selected_vals && treat_neg99) selected_vals_converted <- c(selected_vals_converted[selected_vals_converted != "-99"], "MISSING_NEG99")
          data <- data[data[[var]] %in% selected_vals_converted, ]
        }
      }
    }
    selected_vars <- c("CODE", input$tabplot_numeric_vars, input$tabplot_categorical_vars)
    selected_vars <- unique(selected_vars)
    if(length(selected_vars) == 0) return(NULL)
    result <- data[, selected_vars, drop = FALSE]
    if(!input$tabplot_include_null) {
      for(col in intersect(names(result), categorical_cols)) result <- result[result[[col]] != "MISSING_R_NA", ]
      numeric_cols_selected <- intersect(names(result), numeric_cols)
      if(length(numeric_cols_selected) > 0) {
        na_rows <- apply(result[, numeric_cols_selected, drop = FALSE], 1, function(x) any(is.na(x)))
        result <- result[!na_rows, ]
      }
    }
    result
  })
  
  output$tabplot_has_data <- reactive({ !is.null(tabplot_filtered_data()) && nrow(tabplot_filtered_data()) > 0 })
  outputOptions(output, "tabplot_has_data", suspendWhenHidden = FALSE)
  
  output$tabplot_plot <- renderPlot({
    if(!requireNamespace("tabplot", quietly = TRUE)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "tabplot package not installed")
      text(1, 1, "Please install tabplot package", cex = 1.2); return()
    }
    data <- tabplot_filtered_data()
    if(is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No Data Available")
      text(1, 1, "Please select at least one variable", cex = 1.2); return()
    }
    data_plot <- as.data.frame(data)
    for(col in names(data_plot)) {
      if(is.character(data_plot[[col]])) {
        unique_vals <- unique(data_plot[[col]])
        labels <- unique_vals
        labels[labels == "MISSING_DASH"] <- "--"
        labels[labels == "MISSING_NA_STRING"] <- "NA"
        labels[labels == "MISSING_NEG99"] <- "-99"
        labels[labels == "MISSING_R_NA"] <- "MISSING"
        data_plot[[col]] <- factor(data_plot[[col]], levels = unique_vals, labels = labels)
      }
    }
    sort_col <- "CODE"; decreasing <- (input$tabplot_code_order == "desc")
    tryCatch({
      tabplot::tableplot(data_plot, sortCol = sort_col, decreasing = decreasing, nBins = input$tabplot_nbins,
                         showNA = if(input$tabplot_showNA) "ifany" else "no", plot = TRUE)
    }, error = function(e) {
      tryCatch({ tabplot::tableplot(data_plot, nBins = input$tabplot_nbins, plot = TRUE) },
               error = function(e2) { plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Error Creating Tabplot")
                 text(1, 1, paste("Error:", substr(e2$message, 1, 40)), cex = 0.8) })
    })
  })
  
  output$tabplot_obs_count <- renderPrint({
    data <- tabplot_filtered_data()
    if(is.null(data) || nrow(data) == 0) { cat("No data available\nPlease select at least one variable"); return() }
    total_rows <- nrow(data)
    cat("Tableplot Summary\n=================\n\n")
    cat("Total observations:", total_rows, "of", nrow(df), "\n")
    cat("Variables plotted:", ncol(data), "\n")
    cat("  - CODE (always included): Yes\n")
    cat("  - Numeric variables:", sum(sapply(data, is.numeric)), "\n")
    cat("  - Categorical variables:", sum(sapply(data, is.factor) | sapply(data, is.character)), "\n\n")
    cat("Plot Settings:\n")
    cat("  CODE order:", if(input$tabplot_code_order == "asc") "Ascending (A→Z)" else "Descending (Z→A)", "\n")
    cat("  Number of bins:", input$tabplot_nbins, "\n")
    cat("  Show NA:", if(input$tabplot_showNA) "Yes" else "No", "\n")
    cat("  Include NULL values:", if(input$tabplot_include_null) "Yes" else "No", "\n\n")
    cat("Missing Value Handling:\n")
    cat("  -- values:", if("dash" %in% input$tabplot_mv_types) "Converted to '--' in plot" else "Treated as regular values", "\n")
    cat("  'NA' strings:", if("na_string" %in% input$tabplot_mv_types) "Converted to 'NA' in plot" else "Treated as regular values", "\n")
    cat("  -99 values:", if("neg99" %in% input$tabplot_mv_types) "Converted to '-99' in plot" else "Treated as regular values", "\n")
    cat("  R NA values:", if(input$tabplot_include_null) "Displayed as 'MISSING'" else "Excluded", "\n\n")
    cat("Variables:\n")
    for(var in names(data)) {
      var_type <- if(is.numeric(data[[var]])) "numeric" else "categorical"
      if(var_type == "numeric") {
        missing_count <- sum(is.na(data[[var]]))
        missing_pct <- round(100 * missing_count / nrow(data), 1)
        cat("  ", var, " (", var_type, "): ", missing_count, " missing (", missing_pct, "%)\n", sep = "")
      } else {
        vals_char <- as.character(data[[var]])
        missing_count <- sum(vals_char %in% c("--", "NA", "-99", "MISSING"))
        missing_pct <- round(100 * missing_count / nrow(data), 1)
        cat("  ", var, " (", var_type, "): ", missing_count, " missing (", missing_pct, "%)\n", sep = "")
        cat("    Categories: ", paste(sort(unique(vals_char)), collapse = ", "), "\n")
      }
    }
  })
  
  observeEvent(input$reset_tabplot, {
    updateSliderInput(session, "tabplot_nbins", value = 100)
    updateRadioButtons(session, "tabplot_code_order", selected = "asc")
    updateMaterialSwitch(session, "tabplot_showNA", value = TRUE)
    updateMaterialSwitch(session, "tabplot_include_null", value = TRUE)
    updatePickerInput(session, "tabplot_numeric_vars", selected = numeric_cols[1:3])
    updatePickerInput(session, "tabplot_categorical_vars", selected = categorical_cols[categorical_cols != "CODE"][1:2])
    updateCheckboxGroupInput(session, "tabplot_mv_types", selected = c("neg99", "dash", "na_string"))
    if(!is.null(input$tabplot_categorical_vars)) {
      for(var in input$tabplot_categorical_vars) {
        vals <- df[[var]]; char_vals <- as.character(vals); choices <- sort(unique(char_vals))
        updateCheckboxGroupInput(session, paste0("tabplot_filter_", var), selected = choices)
      }
    }
    showNotification("Tabplot settings reset to defaults", type = "default")
  })
  
  # ==========================================================================
  # MOSAIC PLOT
  # ==========================================================================
  
  observe({
    updateSelectInput(session, "mosaic_x", selected = "GOVERN_TYPE")
    updateSelectInput(session, "mosaic_y", selected = "HEALTHCARE_BASIS")
    updateSelectInput(session, "mosaic_z", selected = "None")
  })
  
  output$mosaic_plot <- renderPlot({
    req(input$mosaic_x, input$mosaic_y)
    if(input$mosaic_z == "None") {
      formula <- as.formula(paste("~", input$mosaic_x, "+", input$mosaic_y))
    } else {
      formula <- as.formula(paste("~", input$mosaic_x, "+", input$mosaic_y, "+", input$mosaic_z))
    }
    mosaic(formula, data = df, shade = TRUE, legend = TRUE, main = paste("Mosaic Plot:", input$mosaic_x, "vs", input$mosaic_y))
  })
  
  output$mosaic_stats <- renderPrint({
    tbl <- table(df[[input$mosaic_x]], df[[input$mosaic_y]], useNA = "ifany")
    cat("Contingency Table:\n"); print(tbl)
  })
  
  # ==========================================================================
  # RAW DATA
  # ==========================================================================
  
  observe({
    updateSliderInput(session, "dt_row_range", max = nrow(df), value = c(1, nrow(df)))
    updateSelectInput(session, "dt_page_length", selected = 25)
    updateTabsetPanel(session, "dt_column_tabs", selected = "All Columns")
    updatePickerInput(session, "dt_all_columns", selected = names(df))
    updatePickerInput(session, "dt_numeric_columns", selected = numeric_cols)
    updatePickerInput(session, "dt_categorical_columns", selected = categorical_cols)
  })
  
  observe({ updateSliderInput(session, "dt_row_range", max = nrow(df), value = c(1, nrow(df))) })
  observe({ updatePickerInput(session, "dt_all_columns", choices = names(df), selected = names(df)) })
  observe({ updatePickerInput(session, "dt_numeric_columns", choices = numeric_cols, selected = numeric_cols) })
  observe({ updatePickerInput(session, "dt_categorical_columns", choices = categorical_cols, selected = categorical_cols) })
  
  observeEvent(input$dt_all_select_all, { updatePickerInput(session, "dt_all_columns", selected = names(df)) })
  observeEvent(input$dt_all_deselect_all, { updatePickerInput(session, "dt_all_columns", selected = character(0)) })
  observeEvent(input$dt_num_select_all, { updatePickerInput(session, "dt_numeric_columns", selected = numeric_cols) })
  observeEvent(input$dt_num_deselect_all, { updatePickerInput(session, "dt_numeric_columns", selected = character(0)) })
  observeEvent(input$dt_cat_select_all, { updatePickerInput(session, "dt_categorical_columns", selected = categorical_cols) })
  observeEvent(input$dt_cat_deselect_all, { updatePickerInput(session, "dt_categorical_columns", selected = character(0)) })
  
  selected_columns <- reactive({
    active_tab <- input$dt_column_tabs
    if(active_tab == "All Columns") {
      if(is.null(input$dt_all_columns) || length(input$dt_all_columns) == 0) return(names(df))
      return(input$dt_all_columns)
    } else if(active_tab == "Numeric Columns") {
      selected_num <- if(is.null(input$dt_numeric_columns) || length(input$dt_numeric_columns) == 0) character(0) else input$dt_numeric_columns
      always_include <- c("CODE", "OBS_TYPE")
      return(unique(c(always_include, selected_num)))
    } else if(active_tab == "Categorical Columns") {
      selected_cat <- if(is.null(input$dt_categorical_columns) || length(input$dt_categorical_columns) == 0) character(0) else input$dt_categorical_columns
      always_include <- c("CODE", "OBS_TYPE")
      return(unique(c(always_include, selected_cat)))
    }
    return(names(df))
  })
  
  row_range <- reactive({ if(is.null(input$dt_row_range)) return(1:nrow(df)); input$dt_row_range[1]:input$dt_row_range[2] })
  
  output$dt_row_info <- renderText({
    start_row <- input$dt_row_range[1]; end_row <- input$dt_row_range[2]
    total_rows <- nrow(df); selected_rows <- length(row_range())
    paste("Showing rows", start_row, "to", end_row, "of", total_rows, "|", selected_rows, "rows selected")
  })
  
  output$raw_data_table <- renderDT({
    req(selected_columns())
    rows <- row_range()
    df_subset <- df[rows, selected_columns(), drop = FALSE]
    page_length <- if(input$dt_page_length == -1) nrow(df_subset) else as.numeric(input$dt_page_length)
    datatable(df_subset, extensions = "Buttons",
              options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             pageLength = page_length, lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
                             scrollX = TRUE, autoWidth = TRUE),
              filter = "top", class = 'cell-border stripe hover', rownames = FALSE)
  })
  
  observeEvent(input$reset_datatable, {
    updateSliderInput(session, "dt_row_range", value = c(1, nrow(df)))
    updateSelectInput(session, "dt_page_length", selected = 25)
    updateTabsetPanel(session, "dt_column_tabs", selected = "All Columns")
    updatePickerInput(session, "dt_all_columns", selected = names(df))
    updatePickerInput(session, "dt_numeric_columns", selected = numeric_cols)
    updatePickerInput(session, "dt_categorical_columns", selected = categorical_cols)
    showNotification("Data table settings reset to defaults", type = "default")
  })
  
  # ==========================================================================
  # TAB 2: DATA PROCESSING STRATEGY
  # ==========================================================================
  
  processing_log <- reactiveVal(character(0))
  
  add_to_log <- function(message) {
    current_log <- processing_log()
    timestamp <- format(Sys.time(), "%H:%M:%S")
    new_log <- c(current_log, paste0("[", timestamp, "] ", message))
    processing_log(new_log)
  }
  
  observe({
    if(length(processing_log()) == 0) add_to_log("Session started. Working with original dataset.")
  })
  
  # Missing Values Heatmap for Tab 2
  output$proc_missing_heatmap <- renderPlotly({
    selected_cat <- input$proc_heatmap_cat_vars
    selected_num <- input$proc_heatmap_numeric_vars
    selected_vars <- c(selected_num, selected_cat)
    selected_vars <- selected_vars[!selected_vars %in% c("CODE", "OBS_TYPE")]
    req(length(selected_vars) > 0)
    
    data <- processed_data_working()
    code_column <- data$CODE
    selected_vars <- intersect(selected_vars, names(data))
    req(length(selected_vars) > 0)
    
    plot_df <- data[, selected_vars, drop = FALSE]
    col_threshold <- input$proc_col_missing_threshold
    row_threshold <- input$proc_row_missing_threshold
    
    # Calculate missing percentages
    col_missing_pct <- apply(plot_df, 2, function(x) {
      vals <- as.character(x)
      sum(vals %in% c("-99", "--", "NA") | is.na(vals)) / length(vals) * 100
    })
    
    # Filter columns
    plot_df <- plot_df[, col_missing_pct <= col_threshold, drop = FALSE]
    code_column <- code_column[col_missing_pct <= col_threshold]
    
    # Filter rows
    row_missing_count <- apply(plot_df, 1, function(x) {
      vals <- as.character(x)
      sum(vals %in% c("-99", "--", "NA") | is.na(vals))
    })
    keep_rows <- row_missing_count <= row_threshold
    plot_df <- plot_df[keep_rows, , drop = FALSE]
    code_column <- code_column[keep_rows]
    
    n_obs <- nrow(plot_df)
    n_vars <- ncol(plot_df)
    
    if(n_obs == 0 || n_vars == 0) {
      return(plot_ly() %>% add_annotations(text = "No data after applying filters", x = 0.5, y = 0.5))
    }
    
    # Create color matrix
    color_matrix <- matrix("#ADD8E6", nrow = n_obs, ncol = n_vars)
    colnames(color_matrix) <- names(plot_df)
    
    show_neg99 <- "neg99" %in% input$proc_mv_types
    show_dash <- "dash" %in% input$proc_mv_types
    show_na_string <- "na_string" %in% input$proc_mv_types
    
    for(i in 1:n_obs) {
      for(j in 1:n_vars) {
        val <- plot_df[i, j]
        if(is.na(val)) {
          color_matrix[i, j] <- "#808080"  # R NA
        } else if(val == "-99") {
          if(show_neg99) color_matrix[i, j] <- "#FF69B4"
        } else if(val == "--") {
          if(show_dash) color_matrix[i, j] <- "#32CD32"
        } else if(val == "NA") {
          if(show_na_string) color_matrix[i, j] <- "#FF0000"
        }
      }
    }
    
    # Order columns if requested
    if(input$proc_heatmap_order == "desc") {
      missing_pcts <- sapply(names(plot_df), function(col) {
        vals <- as.character(plot_df[[col]])
        round(100 * sum(vals %in% c("-99", "--", "NA") | is.na(vals)) / n_obs, 1)
      })
      order_idx <- order(missing_pcts, decreasing = TRUE)
      color_matrix <- color_matrix[, order_idx, drop = FALSE]
    }
    
    # Create hover text
    hover_text <- matrix("", nrow = n_obs, ncol = n_vars)
    for(i in 1:n_obs) {
      for(j in 1:n_vars) {
        val <- plot_df[i, j]
        var_name <- colnames(plot_df)[j]
        code_val <- code_column[i]
        if(is.na(val)) {
          hover_text[i, j] <- paste0("Row: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: R NA")
        } else if(val == "-99") {
          hover_text[i, j] <- paste0("Row: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: -99")
        } else if(val == "--") {
          hover_text[i, j] <- paste0("Row: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: --")
        } else if(val == "NA") {
          hover_text[i, j] <- paste0("Row: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: NA string")
        } else {
          hover_text[i, j] <- paste0("Row: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: Present")
        }
      }
    }
    
    # Create color scale
    unique_colors <- unique(as.vector(color_matrix))
    color_scale <- list()
    for(i in seq_along(unique_colors)) {
      color_scale[[i]] <- list((i-1)/(length(unique_colors)-1), unique_colors[i])
    }
    color_to_num <- setNames(seq_along(unique_colors), unique_colors)
    numeric_matrix <- matrix(color_to_num[color_matrix], nrow = n_obs, ncol = n_vars)
    
    # Create the plot
    plot_ly(z = numeric_matrix, 
            x = colnames(color_matrix), 
            y = 1:n_obs, 
            type = "heatmap", 
            colorscale = color_scale,
            text = hover_text, 
            hoverinfo = "text", 
            showscale = FALSE) %>%
      layout(
        title = paste("Missing Values Heatmap -", n_obs, "Observations,", n_vars, "Variables"),
        xaxis = list(title = "Variables", tickangle = -45, tickfont = list(size = 10)),
        yaxis = list(title = "Observation Index", autorange = "reversed"),
        margin = list(b = 100, t = 80, l = 80, r = 50)
      )
  })
  
  # ==========================================================================
  # UPSET PLOT FUNCTIONS - STANDARD NANIAR FORMAT
  # ==========================================================================
  
  # Prepare missing data for upset plot - CONVERT ALL TO PROPER NA
  prepare_missing_data <- reactive({
    selected_cat <- input$upset_cat_vars
    selected_num <- input$upset_numeric_vars
    selected_vars <- c(selected_cat, selected_num)
    req(length(selected_vars) > 0)
    
    data <- processed_data_working()
    df_missing <- data[, selected_vars, drop = FALSE]
    
    # Convert ALL missing value indicators to proper NA
    for(col in names(df_missing)) {
      vals <- as.character(df_missing[[col]])
      
      # Convert all types of missing to NA
      vals[vals == "-99"] <- NA
      vals[vals == "--"] <- NA
      vals[vals == "NA"] <- NA
      
      if(col %in% numeric_cols) {
        df_missing[[col]] <- suppressWarnings(as.numeric(vals))
      } else {
        df_missing[[col]] <- vals
      }
    }
    return(df_missing)
  })
  
  filtered_missing_data <- reactive({
    df_missing <- prepare_missing_data()
    missing_count <- rowSums(is.na(df_missing))
    min_missing <- input$upset_missing_count[1]
    max_missing <- input$upset_missing_count[2]
    rows_to_keep <- missing_count >= min_missing & missing_count <= max_missing
    df_missing[rows_to_keep, , drop = FALSE]
  })
  
  # UPSET PLOT USING NANIAR - STANDARD FORMAT
  output$upset_plot <- renderPlot({
    req(input$upset_cat_vars, input$upset_numeric_vars)
    
    # Get the filtered data
    df_upset <- filtered_missing_data()
    req(nrow(df_upset) > 0, ncol(df_upset) > 0)
    
    # Check if there are any missing values
    if(sum(is.na(df_upset)) == 0) {
      plot.new()
      text(0.5, 0.5, "No missing values found in selected variables", cex = 1.2)
      return()
    }
    
    # Get columns that actually have missing values
    missing_cols <- names(which(colSums(is.na(df_upset)) > 0))
    
    if(length(missing_cols) < 2) {
      plot.new()
      text(0.5, 0.5, paste("Need at least 2 variables with missing values.\nFound:", length(missing_cols)), cex = 1.2)
      return()
    }
    
    # Subset to only columns with missing values
    upset_df <- df_upset[, missing_cols, drop = FALSE]
    
    # Convert to data frame
    upset_df <- as.data.frame(upset_df)
    
    # Create the upset plot using naniar
    p <- naniar::gg_miss_upset(
      upset_df,
      nintersects = input$upset_n_combinations,
      nsets = min(length(missing_cols), input$upset_nsets),
      order.by = "freq"
    ) +
      labs(title = paste("Missing Value Combinations\n(", nrow(upset_df), " observations, ", ncol(upset_df), " variables)", sep = "")) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
      )
    
    print(p)
  })
  
  # Pattern selector UI
  output$pattern_selector <- renderUI({
    df_upset <- filtered_missing_data()
    req(nrow(df_upset) > 0)
    
    # Get columns with missing values
    missing_cols <- names(which(colSums(is.na(df_upset)) > 0))
    
    if(length(missing_cols) == 0) {
      selectInput("selected_pattern", "Choose a missingness pattern to view:",
                  choices = c("No missing values in selected variables"))
    } else {
      # Create pattern strings
      upset_matrix <- as.data.frame(is.na(df_upset[, missing_cols, drop = FALSE]))
      pattern_strings <- apply(upset_matrix, 1, function(row) {
        if(any(row)) {
          paste(names(which(row)), collapse = " + ")
        } else {
          "Complete Row (No Missing)"
        }
      })
      pattern_counts <- sort(table(pattern_strings), decreasing = TRUE)
      selectInput("selected_pattern", "Choose a missingness pattern to view:", 
                  choices = names(pattern_counts))
    }
  })
  
  # Pattern description output
  output$pattern_description <- renderUI({
    req(input$selected_pattern)
    df_upset <- filtered_missing_data()
    req(nrow(df_upset) > 0)
    
    missing_cols <- names(which(colSums(is.na(df_upset)) > 0))
    
    if(length(missing_cols) == 0) {
      return(div(class = "alert alert-info", "No missing values in selected variables"))
    }
    
    upset_matrix <- as.data.frame(is.na(df_upset[, missing_cols, drop = FALSE]))
    pattern_strings <- apply(upset_matrix, 1, function(row) {
      if(any(row)) {
        paste(names(which(row)), collapse = " + ")
      } else {
        "Complete Row (No Missing)"
      }
    })
    
    selected_rows <- which(pattern_strings == input$selected_pattern)
    pattern_count <- length(selected_rows)
    
    if(input$selected_pattern == "Complete Row (No Missing)") {
      missing_vars <- "None"
    } else {
      missing_vars <- strsplit(input$selected_pattern, " \\+ ")[[1]]
    }
    
    div(class = "alert alert-info",
        tags$strong("Pattern:"), tags$br(), input$selected_pattern, tags$br(), tags$br(),
        tags$strong("Variables with Missing Values:"), tags$br(), 
        if(length(missing_vars) > 0) paste(missing_vars, collapse = ", ") else "None", 
        tags$br(), tags$br(),
        tags$strong("Number of Observations:"), tags$br(), pattern_count, tags$br(), tags$br(),
        tags$strong("Percentage of Filtered Data:"), tags$br(), 
        paste0(round(100 * pattern_count / nrow(df_upset), 1), "%")
    )
  })
  
  # Pattern data table output
  output$pattern_data_table <- renderDT({
    req(input$selected_pattern)
    req(input$selected_pattern != "No missing values in selected variables")
    
    df_upset <- filtered_missing_data()
    req(nrow(df_upset) > 0)
    
    missing_cols <- names(which(colSums(is.na(df_upset)) > 0))
    
    if(length(missing_cols) == 0) {
      return(datatable(data.frame(Message = "No missing values in selected variables")))
    }
    
    upset_matrix <- as.data.frame(is.na(df_upset[, missing_cols, drop = FALSE]))
    pattern_strings <- apply(upset_matrix, 1, function(row) {
      if(any(row)) {
        paste(names(which(row)), collapse = " + ")
      } else {
        "Complete Row (No Missing)"
      }
    })
    
    selected_rows <- which(pattern_strings == input$selected_pattern)
    req(length(selected_rows) > 0)
    
    # Get the original data indices
    df_missing <- prepare_missing_data()
    missing_count <- rowSums(is.na(df_missing))
    min_missing <- input$upset_missing_count[1]
    max_missing <- input$upset_missing_count[2]
    filtered_indices <- which(missing_count >= min_missing & missing_count <= max_missing)
    original_selected_rows <- filtered_indices[selected_rows]
    
    pattern_data <- df[original_selected_rows, , drop = FALSE]
    n_rows <- if(input$upset_n_rows == "All") nrow(pattern_data) else min(as.numeric(input$upset_n_rows), nrow(pattern_data))
    display_data <- pattern_data[1:n_rows, , drop = FALSE]
    
    datatable(display_data, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE) %>%
      formatStyle(names(display_data),
                  backgroundColor = JS("function(value) {
                  if(value === '-99') return '#FFB6C1';
                  if(value === '--') return '#FFD700';
                  if(value === 'NA') return '#FF6347';
                  return null;
                }"))
  })
  
  
  
  
  # RPART Missingness Prediction
  observeEvent(input$run_missing_rpart, {
    req(input$missing_target_var)
    req(input$missing_predictors)
    req(length(input$missing_predictors) > 0)
    data <- processed_data_working()
    model_data <- create_missingness_data(data, input$missing_target_var, input$missing_predictors, input$missing_rpart_types)
    if(is.null(model_data) || nrow(model_data) == 0) {
      showNotification("No valid data after processing. Check your variable selections.", type = "error")
      return()
    }
    class_counts <- table(model_data$target_missing)
    if(length(class_counts) < 2) {
      showNotification(paste("Only one class present:", names(class_counts)[1]), type = "error")
      return()
    }
    if(min(class_counts) < 5) {
      showNotification(paste("Very imbalanced: Only", min(class_counts), 
                             "observations in minority class. Tree may be unstable."), 
                       type = "warning", duration = 5)
    }
    formula <- as.formula(paste("target_missing ~", paste(input$missing_predictors, collapse = "+")))
    tree_model <- rpart::rpart(formula, data = model_data, method = "class", model = TRUE,
                               control = rpart::rpart.control(cp = input$missing_rpart_cp,
                                                              minsplit = input$missing_rpart_minsplit,
                                                              minbucket = max(1, floor(input$missing_rpart_minsplit / 3))))
    missing_rpart_model(tree_model)
    total_missing <- sum(model_data$target_missing == "Missing")
    total_present <- sum(model_data$target_missing == "Present")
    total_obs <- nrow(model_data)
    showNotification(paste("Tree built! Missing rate:", round(100 * total_missing / total_obs, 1), 
                           "% (", total_missing, " missing, ", total_present, " present)"), type = "message", duration = 5)
  })
  
  create_missingness_data <- function(data, target_var, predictor_vars, missing_types) {
    valid_predictors <- predictor_vars[predictor_vars %in% names(data)]
    valid_predictors <- valid_predictors[valid_predictors != target_var]
    if(length(valid_predictors) == 0) {
      showNotification("No valid predictor variables found. Please select different predictors.", type = "error")
      return(NULL)
    }
    
    # Create a copy of the data
    result_data <- data[, c(target_var, valid_predictors), drop = FALSE]
    
    # Convert target variable to check for missing values
    target_vals <- as.character(result_data[[target_var]])
    target_missing <- rep(FALSE, nrow(result_data))
    
    if("neg99" %in% missing_types) target_missing <- target_missing | (target_vals == "-99")
    if("dash" %in% missing_types) target_missing <- target_missing | (target_vals == "--")
    if("na_string" %in% missing_types) target_missing <- target_missing | (target_vals == "NA")
    if("r_na" %in% missing_types) target_missing <- target_missing | is.na(target_vals)
    
    result_data$target_missing <- factor(target_missing, levels = c(FALSE, TRUE), labels = c("Present", "Missing"))
    result_data[[target_var]] <- NULL
    
    # Convert predictor variables to proper format
    for(col in names(result_data)) {
      if(col != "target_missing") {
        # Convert to character first
        col_vals <- as.character(result_data[[col]])
        
        # Convert missing indicators to NA
        col_vals[col_vals == "-99"] <- NA
        col_vals[col_vals == "--"] <- NA
        col_vals[col_vals == "NA"] <- NA
        
        if(col %in% numeric_cols) {
          # For numeric columns, convert to numeric
          result_data[[col]] <- suppressWarnings(as.numeric(col_vals))
        } else {
          # For categorical columns, keep as factor
          result_data[[col]] <- as.factor(col_vals)
        }
      }
    }
    
    # Remove rows with NA in predictors
    predictor_cols <- setdiff(names(result_data), "target_missing")
    complete_rows <- complete.cases(result_data[, predictor_cols, drop = FALSE])
    result_data <- result_data[complete_rows, , drop = FALSE]
    
    if(nrow(result_data) == 0) {
      showNotification("No complete cases found. Try different predictors or handle missing values first.", type = "error")
      return(NULL)
    }
    
    # Check if we have both classes
    if(length(unique(result_data$target_missing)) < 2) {
      showNotification("Target variable has only one class. Cannot build tree.", type = "error")
      return(NULL)
    }
    
    return(result_data)
  }
  
  output$missing_rpart_tree <- renderPlot({
    req(missing_rpart_model())
    model <- missing_rpart_model()
    if(nrow(model$frame) <= 1) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No splits found - missingness cannot be predicted\nTry different parameters or variables", cex = 1.2)
      return()
    }
    par(mar = c(1, 1, 3, 1))
    rpart.plot::prp(model, type = 2, extra = 104, fallen.leaves = TRUE,
                    main = paste("What Predicts Missing Values in", input$missing_target_var, "?"),
                    branch.type = 5, shadow.col = "gray90",
                    box.palette = list("Present" = "lightblue", "Missing" = "lightcoral"),
                    cex = 0.8, under = TRUE, varlen = 0, faclen = 0, digits = 2, roundint = FALSE,
                    yes.text = "Missing", no.text = "Present")
  }, height = 500, width = 800, res = 100)
  
  output$missing_rpart_importance <- renderPlotly({
    req(missing_rpart_model())
    model <- missing_rpart_model()
    importance <- model$variable.importance
    if(is.null(importance) || length(importance) == 0) {
      return(plot_ly() %>% add_annotations(text = "No variable importance available", x = 0.5, y = 0.5))
    }
    imp_df <- data.frame(Variable = names(importance), Importance = importance) %>%
      arrange(Importance) %>% mutate(Variable = factor(Variable, levels = Variable))
    p <- ggplot(imp_df, aes(x = Importance, y = Variable)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      labs(title = paste("What Predicts Missingness in", input$missing_target_var, "?"),
           x = "Importance Score", y = "") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p)
  })
  
  output$missing_pattern_summary <- renderPrint({
    req(input$missing_target_var)
    data <- processed_data_working()
    target_vals <- as.character(data[[input$missing_target_var]])
    missing_counts <- list()
    if("neg99" %in% input$missing_rpart_types) missing_counts$neg99 <- sum(target_vals == "-99", na.rm = TRUE)
    if("dash" %in% input$missing_rpart_types) missing_counts$dash <- sum(target_vals == "--", na.rm = TRUE)
    if("na_string" %in% input$missing_rpart_types) missing_counts$na_string <- sum(target_vals == "NA", na.rm = TRUE)
    if("r_na" %in% input$missing_rpart_types) missing_counts$r_na <- sum(is.na(target_vals), na.rm = TRUE)
    total_missing <- sum(unlist(missing_counts))
    total_obs <- nrow(data)
    cat("=== Missingness Analysis for:", input$missing_target_var, "===\n\n")
    cat("Total Observations:", total_obs, "\n")
    cat("Total Missing:", total_missing, "(", round(100 * total_missing / total_obs, 1), "%)\n\n")
    cat("Missing by Type:\n")
    for(type in names(missing_counts)) {
      if(missing_counts[[type]] > 0) cat("  ", type, ":", missing_counts[[type]], "\n")
    }
  })
  
  output$missing_tree_rules <- renderPrint({
    req(missing_rpart_model())
    model <- missing_rpart_model()
    frame <- model$frame
    leaves <- which(frame$var == "<leaf")
    if(length(leaves) == 0) {
      cat("No rules generated")
      return()
    }
    rules <- rpart::path.rpart(model, leaves, print.it = FALSE)
    cat("=== Decision Tree Rules ===\n\n")
    for(i in seq_along(rules)) {
      leaf_name <- names(rules)[i]
      node_info <- frame[leaf_name, ]
      rule_path <- rules[[i]]
      rule_text <- paste(rule_path[-1], collapse = " AND ")
      prob_missing <- node_info$yval2[1,2] / (node_info$yval2[1,1] + node_info$yval2[1,2])
      cat("Rule", i, ":\n")
      cat("  IF", rule_text, "\n")
      cat("  THEN Missing probability:", round(100 * prob_missing, 1), "%\n")
      cat("  Observations:", node_info$n, "\n\n")
    }
  })
  
  # Missing Variable Correlation
  output$proc_missing_corr_plot <- renderPlotly({
    data <- processed_data_working()
    df_shadow <- is.na(data) + 0
    missing_cols <- colSums(df_shadow) > 0
    df_shadow <- df_shadow[, missing_cols, drop = FALSE]
    if(ncol(df_shadow) < 2) {
      return(plot_ly() %>% add_annotations(text = "Need at least 2 variables with missing values", 
                                           x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)))
    }
    corr_method <- input$proc_missing_corr_method
    corr_matrix <- cor(df_shadow, method = corr_method, use = "pairwise.complete.obs")
    if(input$proc_missing_corr_abs) corr_matrix <- abs(corr_matrix)
    order_method <- input$proc_missing_corr_order
    if(!is.null(order_method)) {
      if(order_method == "AOE") {
        if(requireNamespace("corrplot", quietly = TRUE)) {
          order_idx <- corrplot::corrMatOrder(corr_matrix, order = "AOE")
          corr_matrix <- corr_matrix[order_idx, order_idx]
        }
      } else if(order_method == "FPC") {
        if(requireNamespace("corrplot", quietly = TRUE)) {
          order_idx <- corrplot::corrMatOrder(corr_matrix, order = "FPC")
          corr_matrix <- corr_matrix[order_idx, order_idx]
        }
      } else if(order_method == "hclust") {
        hc <- hclust(as.dist(1 - corr_matrix), method = "complete")
        corr_matrix <- corr_matrix[hc$order, hc$order]
      } else if(order_method == "alphabet") {
        order_idx <- order(colnames(corr_matrix))
        corr_matrix <- corr_matrix[order_idx, order_idx]
      }
    }
    if(input$proc_missing_corr_abs) {
      colorscale <- list(list(0, "white"), list(0.5, "#ffcccc"), list(1, "red"))
      zmin <- 0
    } else {
      colorscale <- list(list(0, "blue"), list(0.5, "white"), list(1, "red"))
      zmin <- -1
    }
    p <- plot_ly(z = corr_matrix, x = colnames(corr_matrix), y = colnames(corr_matrix), type = "heatmap",
                 colorscale = colorscale, zmin = zmin, zmax = 1,
                 hovertemplate = "<b>%{x}</b> vs <b>%{y}</b><br>Correlation: %{z:.3f}<extra></extra>",
                 showscale = TRUE, colorbar = list(title = "Correlation", titleside = "right"))
    if(input$proc_missing_corr_display) {
      annotations_list <- list()
      n <- ncol(corr_matrix)
      for(i in 1:n) {
        for(j in 1:n) {
          corr_val <- corr_matrix[i, j]
          text_color <- ifelse(abs(corr_val) > 0.7, "white", "black")
          annotations_list <- append(annotations_list, list(
            list(x = colnames(corr_matrix)[j], y = colnames(corr_matrix)[i],
                 text = sprintf("%.2f", corr_val), showarrow = FALSE,
                 font = list(size = 10, color = text_color), xanchor = "center", yanchor = "middle")
          ))
        }
      }
      p <- p %>% layout(annotations = annotations_list)
    }
    p %>% layout(title = paste("Missing Variable Correlation -", 
                               switch(corr_method, pearson = "Pearson", spearman = "Spearman", kendall = "Kendall")),
                 xaxis = list(title = "", tickangle = -45, tickfont = list(size = 10)),
                 yaxis = list(title = "", tickfont = list(size = 10), autorange = "reversed"),
                 margin = list(b = 100, t = 80, l = 120, r = 50))
  })
  
  # ==========================================================================
  # DATA PROCESSING CONTROLS
  # ==========================================================================
  
  observeEvent(input$proc_remove_col_btn, {
    req(input$proc_remove_column)
    protected_cols <- c("CODE", "OBS_TYPE", "DEATH_RATE")
    if(input$proc_remove_column %in% protected_cols) {
      showNotification(paste("Cannot remove protected column:", input$proc_remove_column), type = "error", duration = 5)
      return()
    }
    current_data <- processed_data_working()
    if(input$proc_remove_column %in% names(current_data)) {
      new_data <- current_data[, !names(current_data) %in% input$proc_remove_column, drop = FALSE]
      processed_data_working(new_data)
      add_to_log(paste("Removed column:", input$proc_remove_column))
      showNotification(paste("Removed column:", input$proc_remove_column), type = "default")
    }
  })
  
  observeEvent(input$proc_create_shadow_btn, {
    req(input$proc_create_shadow)
    current_data <- processed_data_working()
    col_name <- input$proc_create_shadow
    if(col_name %in% names(current_data)) {
      shadow_name <- paste0(col_name, "_MISSING")
      current_data[[shadow_name]] <- as.numeric(is.na(current_data[[col_name]]))
      processed_data_working(current_data)
      add_to_log(paste("Created shadow variable:", shadow_name))
      showNotification(paste("Created shadow variable:", shadow_name), type = "success")
    }
  })
  
  observeEvent(input$proc_transform_btn, {
    req(input$proc_transform_column)
    current_data <- processed_data_working()
    if(input$proc_transform_column %in% names(current_data)) {
      col_data <- current_data[[input$proc_transform_column]]
      if(is.numeric(col_data)) {
        method <- input$proc_transform_method
        original_vals <- col_data[!is.na(col_data)]
        if(method == "log") {
          if(min(original_vals, na.rm = TRUE) > 0) {
            current_data[[input$proc_transform_column]] <- log(col_data)
            add_to_log(paste("Applied log transformation to:", input$proc_transform_column))
            showNotification(paste("Transformed column:", input$proc_transform_column), type = "default")
          } else {
            showNotification("Log transformation requires positive values", type = "error")
            return()
          }
        } else if(method == "sqrt") {
          if(min(original_vals, na.rm = TRUE) >= 0) {
            current_data[[input$proc_transform_column]] <- sqrt(col_data)
            add_to_log(paste("Applied square root transformation to:", input$proc_transform_column))
            showNotification(paste("Transformed column:", input$proc_transform_column), type = "default")
          } else {
            showNotification("Square root requires non-negative values", type = "error")
            return()
          }
        } else if(method == "square") {
          current_data[[input$proc_transform_column]] <- col_data^2
          add_to_log(paste("Applied square transformation to:", input$proc_transform_column))
          showNotification(paste("Transformed column:", input$proc_transform_column), type = "default")
        } else if(method == "boxcox") {
          if(requireNamespace("MASS", quietly = TRUE) && min(original_vals, na.rm = TRUE) > 0) {
            bc <- MASS::boxcox(col_data ~ 1, plotit = FALSE)
            lambda <- bc$x[which.max(bc$y)]
            if(abs(lambda) < 0.01) {
              current_data[[input$proc_transform_column]] <- log(col_data)
            } else {
              current_data[[input$proc_transform_column]] <- (col_data^lambda - 1) / lambda
            }
            add_to_log(paste("Applied Box-Cox transformation (lambda =", round(lambda, 3), ") to:", input$proc_transform_column))
            showNotification(paste("Transformed column:", input$proc_transform_column), type = "default")
          } else {
            showNotification("Box-Cox requires positive values", type = "error")
            return()
          }
        } else if(method == "yeojohnson") {
          if(requireNamespace("bestNormalize", quietly = TRUE)) {
            yj <- bestNormalize::yeojohnson(col_data)
            current_data[[input$proc_transform_column]] <- predict(yj, col_data)
            add_to_log(paste("Applied Yeo-Johnson transformation (lambda =", round(yj$lambda, 3), ") to:", input$proc_transform_column))
            showNotification(paste("Transformed column:", input$proc_transform_column), type = "default")
          } else {
            showNotification("bestNormalize package required for Yeo-Johnson", type = "error")
            return()
          }
        }
        processed_data_working(current_data)
      } else {
        showNotification("Selected column is not numeric", type = "error")
      }
    }
  })
  
  observeEvent(input$proc_impute_btn, {
    req(input$proc_impute_column)
    current_data <- standardize_missing(processed_data_working())
    if(input$proc_impute_column %in% names(current_data)) {
      col_data <- current_data[[input$proc_impute_column]]
      na_count <- sum(is.na(col_data))
      if(na_count > 0) {
        method <- input$proc_impute_method
        if(method == "median" && is.numeric(col_data)) {
          impute_val <- median(col_data, na.rm = TRUE)
          current_data[[input$proc_impute_column]][is.na(col_data)] <- impute_val
          add_to_log(paste("Imputed", na_count, "missing values in", input$proc_impute_column, "with median =", round(impute_val, 3)))
          showNotification(paste("Imputed", na_count, "missing values in:", input$proc_impute_column), type = "default")
        } else if(method == "mean" && is.numeric(col_data)) {
          impute_val <- mean(col_data, na.rm = TRUE)
          current_data[[input$proc_impute_column]][is.na(col_data)] <- impute_val
          add_to_log(paste("Imputed", na_count, "missing values in", input$proc_impute_column, "with mean =", round(impute_val, 3)))
          showNotification(paste("Imputed", na_count, "missing values in:", input$proc_impute_column), type = "default")
        } else if(method == "mode") {
          tbl <- table(col_data)
          impute_val <- names(tbl)[which.max(tbl)]
          current_data[[input$proc_impute_column]][is.na(col_data)] <- impute_val
          add_to_log(paste("Imputed", na_count, "missing values in", input$proc_impute_column, "with mode =", impute_val))
          showNotification(paste("Imputed", na_count, "missing values in:", input$proc_impute_column), type = "default")
        } else if(method == "constant") {
          impute_val <- input$proc_impute_value
          current_data[[input$proc_impute_column]][is.na(col_data)] <- impute_val
          add_to_log(paste("Imputed", na_count, "missing values in", input$proc_impute_column, "with constant =", impute_val))
          showNotification(paste("Imputed", na_count, "missing values in:", input$proc_impute_column), type = "default")
        } else {
          showNotification("Imputation method not compatible with this column type", type = "error")
          return()
        }
        processed_data_working(current_data)
      } else {
        showNotification(paste("No missing values found in column:", input$proc_impute_column), type = "default")
      }
    }
  })
  
  impute_categorical <- function(data, column, method, constant_value = NULL) {
    if(!column %in% names(data)) {
      return(list(data = data, message = paste("Column", column, "not found")))
    }
    col_data <- data[[column]]
    na_count <- sum(is.na(col_data))
    if(na_count == 0) {
      return(list(data = data, message = paste("No missing values in column:", column), imputed = 0))
    }
    if(method == "mode") {
      tbl <- table(col_data)
      impute_val <- names(tbl)[which.max(tbl)]
      data[[column]][is.na(col_data)] <- impute_val
      return(list(data = data, message = paste("Imputed", na_count, "missing values with mode =", impute_val), imputed = na_count))
    } else if(method == "new_category") {
      data[[column]] <- as.character(data[[column]])
      data[[column]][is.na(data[[column]])] <- "Missing"
      data[[column]] <- as.factor(data[[column]])
      return(list(data = data, message = paste("Imputed", na_count, "missing values with new category 'Missing'"), imputed = na_count))
    } else if(method == "constant") {
      impute_val <- constant_value
      data[[column]][is.na(col_data)] <- impute_val
      return(list(data = data, message = paste("Imputed", na_count, "missing values with constant =", impute_val), imputed = na_count))
    }
    return(list(data = data, message = "Unknown method", imputed = 0))
  }
  
  observeEvent(input$proc_impute_cat_btn, {
    req(input$proc_impute_cat_column)
    current_data <- standardize_missing(processed_data_working())
    column <- input$proc_impute_cat_column
    method <- input$proc_impute_cat_method
    constant_value <- if(method == "constant") input$proc_impute_cat_value else NULL
    if(!column %in% names(current_data)) {
      showNotification(paste("Column", column, "not found in current data"), type = "error")
      return()
    }
    if(column %in% numeric_cols) {
      showNotification(paste(column, "is a numeric column. Use numeric imputation instead."), type = "warning")
      return()
    }
    result <- impute_categorical(current_data, column, method, constant_value)
    if(result$imputed > 0) {
      processed_data_working(result$data)
      add_to_log(result$message)
      showNotification(result$message, type = "default")
    } else {
      showNotification(result$message, type = "default")
    }
  })
  
  observeEvent(input$proc_remove_rows_btn, {
    current_data <- processed_data_working()
    row_missing <- apply(current_data, 1, function(x) sum(is.na(x)))
    rows_to_keep <- row_missing <= input$proc_max_missing_per_row
    rows_removed <- sum(!rows_to_keep)
    if(rows_removed > 0) {
      new_data <- current_data[rows_to_keep, , drop = FALSE]
      processed_data_working(new_data)
      add_to_log(paste("Removed", rows_removed, "rows with >=", input$proc_max_missing_per_row, "missing values"))
      showNotification(paste("Removed", rows_removed, "rows"), type = "default")
    } else {
      showNotification("No rows exceeded the missing value threshold", type = "default")
    }
  })
  
  observeEvent(input$proc_apply_all, {
    data <- standardize_missing(df)
    add_to_log("Starting with standardized data (converted -99, --, 'NA' to NA)")
    if(!is.null(input$proc_remove_column) && input$proc_remove_column != "") {
      if(input$proc_remove_column %in% names(data)) {
        data <- data[, !names(data) %in% input$proc_remove_column, drop = FALSE]
        add_to_log(paste("Removed column:", input$proc_remove_column))
      }
    }
    if(!is.null(input$proc_transform_column) && input$proc_transform_column != "" && 
       input$proc_transform_column %in% names(data) && is.numeric(data[[input$proc_transform_column]])) {
      if(min(data[[input$proc_transform_column]], na.rm = TRUE) > 0) {
        data[[input$proc_transform_column]] <- log(data[[input$proc_transform_column]])
        add_to_log(paste("Applied log transformation to:", input$proc_transform_column))
      }
    }
    if(!is.null(input$proc_impute_column) && input$proc_impute_column != "" && 
       input$proc_impute_column %in% names(data) && any(is.na(data[[input$proc_impute_column]]))) {
      if(is.numeric(data[[input$proc_impute_column]])) {
        impute_val <- median(data[[input$proc_impute_column]], na.rm = TRUE)
        na_count <- sum(is.na(data[[input$proc_impute_column]]))
        data[[input$proc_impute_column]][is.na(data[[input$proc_impute_column]])] <- impute_val
        add_to_log(paste("Imputed", na_count, "missing values in numeric column:", input$proc_impute_column, "with median =", round(impute_val, 3)))
      }
    }
    if(!is.null(input$proc_impute_cat_column) && input$proc_impute_cat_column != "" && 
       input$proc_impute_cat_column %in% names(data)) {
      cat_col <- input$proc_impute_cat_column
      cat_method <- input$proc_impute_cat_method
      cat_constant <- if(cat_method == "constant") input$proc_impute_cat_value else NULL
      if(any(is.na(data[[cat_col]]))) {
        result <- impute_categorical(data, cat_col, cat_method, cat_constant)
        if(result$imputed > 0) data <- result$data
        add_to_log(result$message)
      }
    }
    row_missing <- apply(data, 1, function(x) sum(is.na(x)))
    rows_to_keep <- row_missing <= input$proc_max_missing_per_row
    rows_removed <- sum(!rows_to_keep)
    if(rows_removed > 0) {
      data <- data[rows_to_keep, , drop = FALSE]
      add_to_log(paste("Removed", rows_removed, "rows with >=", input$proc_max_missing_per_row, "missing values"))
    }
    processed_data_working(data)
    add_to_log("--- Applied all processing steps ---")
    showNotification("All processing steps applied!", type = "default")
  })
  
  output$proc_data_summary <- renderPrint({
    data <- processed_data_working()
    total_missing <- sum(is.na(data))
    total_cells <- nrow(data) * ncol(data)
    missing_pct <- round(100 * total_missing / total_cells, 2)
    cat("Current Data Summary\n")
    cat("====================\n\n")
    cat("Observations:", nrow(data), "of", nrow(df), "\n")
    cat("Variables:", ncol(data), "of", ncol(df), "\n")
    cat("Missing values (R NA):", total_missing, "\n")
    cat("Missing %:", missing_pct, "%\n\n")
    cat("Variables with missing values:\n")
    missing_by_var <- colSums(is.na(data))
    missing_by_var <- missing_by_var[missing_by_var > 0]
    if(length(missing_by_var) > 0) {
      for(i in seq_along(missing_by_var)) {
        pct <- round(100 * missing_by_var[i] / nrow(data), 1)
        cat("  ", names(missing_by_var)[i], ":", missing_by_var[i], "missing (", pct, "%)\n")
      }
    } else {
      cat("  No missing values in any variable!\n")
    }
    cat("\nVariables removed:\n")
    removed_vars <- setdiff(names(df), names(data))
    if(length(removed_vars) > 0) {
      for(var in removed_vars) cat("  - ", var, "\n")
    } else {
      cat("  None\n")
    }
  })
  
  output$proc_log <- renderPrint({
    log_entries <- processing_log()
    if(length(log_entries) == 0) {
      cat("No processing actions yet.\nUse the controls above to modify the data.")
    } else {
      cat(paste(log_entries, collapse = "\n"))
    }
  })
  
  output$proc_data_preview <- renderDT({
    data <- processed_data_working()
    datatable(head(data, 20), options = list(scrollX = TRUE), rownames = FALSE)
  })
  
  output$proc_download_data <- downloadHandler(
    filename = function() { paste("processed_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(processed_data_working(), file, row.names = FALSE) }
  )
  
  # ==========================================================================
  # OUTLIER DETECTION SUITE
  # ==========================================================================
  
  outlier_results <- reactiveVal(list())
  
  # Reactive outlier analysis that runs when parameters change
  outlier_analysis_reactive <- reactive({
    params <- list(
      mahalanobis_threshold = input$outlier_mahalanobis_threshold,
      cooks_method = input$outlier_cooks_method,
      lof_minpts = input$outlier_lof_minpts,
      lof_threshold = input$outlier_lof_threshold,
      svm_nu = input$outlier_svm_nu,
      svm_kernel = input$outlier_svm_kernel,
      rf_iqr = input$outlier_rf_iqr,
      if_threshold = input$outlier_if_threshold
    )
    
    data <- processed_data_working()
    
    results <- list()
    
    results$Mahalanobis <- detect_mahalanobis_outliers(data, params$mahalanobis_threshold)
    results$Cooks <- detect_cooks_outliers(data, params$cooks_method)
    results$LOF <- detect_lof_outliers(data, params$lof_minpts, params$lof_threshold)
    results$SVM <- detect_svm_outliers(data, params$svm_nu, params$svm_kernel)
    results$RF_Residuals <- detect_rf_outliers(data, params$rf_iqr)
    results$IForest <- detect_iforest_outliers(data, params$if_threshold)
    
    outlier_results(results)
    results
  })
  
  # Create consensus data reactively
  outlier_consensus <- reactive({
    results <- outlier_analysis_reactive()
    data <- processed_data_working()
    create_outlier_consensus(results, data)
  })
  
  # Combined View Output
  output$outlier_combined_plot <- renderPlotly({
    consensus <- outlier_consensus()
    req(consensus$data)
    
    plot_df <- consensus$data[consensus$data$Total_Flags > 0, ]
    if(nrow(plot_df) == 0) {
      return(plot_ly() %>% add_annotations(
        text = "No outliers detected by any method with current settings",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)))
    }
    
    heatmap_data <- as.matrix(plot_df[, consensus$methods, drop = FALSE])
    rownames(heatmap_data) <- plot_df$Observation
    
    plot_ly(z = heatmap_data, x = consensus$methods, y = rownames(heatmap_data), 
            type = "heatmap", colorscale = list(c(0, "lightgray"), c(1, "red")), 
            showscale = FALSE,
            hovertemplate = "Method: %{x}<br>Observation: %{y}<br>Flagged: %{z}<extra></extra>") %>%
      layout(title = "Outlier Detection Consensus Heatmap",
             xaxis = list(title = "Detection Method", tickangle = -45),
             yaxis = list(title = "Observation"),
             height = max(400, nrow(heatmap_data) * 20))
  })
  
  output$outlier_consensus_table <- renderDT({
    consensus <- outlier_consensus()
    req(consensus$data)
    
    display_df <- consensus$data[consensus$data$Total_Flags >= 2, ]
    if(nrow(display_df) == 0) {
      return(datatable(data.frame(Message = "No observations flagged by 2 or more methods with current settings")))
    }
    
    datatable(display_df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle("Total_Flags", 
                  backgroundColor = styleInterval(c(1, 2, 3), c("#fff3cd", "#ffeaa7", "#fdcb6e")))
  })
  
  # Mahalanobis Distance Plot
  output$outlier_mahalanobis_plot <- renderPlotly({
    res <- outlier_analysis_reactive()$Mahalanobis
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "Insufficient data", x = 0.5, y = 0.5))
    
    plot_df <- data.frame(md2 = res$scores, id = (1:length(res$scores))/length(res$scores),
                          label = ifelse(res$is_outlier, names(res$scores), NA),
                          Observations = ifelse(res$is_outlier, "outlier", "non-outlier"))
    
    p <- ggplot(plot_df, aes(y = md2, x = id, color = Observations, label = label)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_text(aes(label = label), hjust = -0.1, vjust = 0.5, size = 3, 
                check_overlap = TRUE, na.rm = TRUE) +
      scale_color_manual(values = c("non-outlier" = "blue", "outlier" = "red")) +
      labs(y = "Mahalanobis Distance Squared", x = "Complete Observations (percentile)",
           title = paste("Mahalanobis Distance Outliers (df =", res$df, ")")) +
      geom_hline(yintercept = res$threshold, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p, tooltip = c("y", "label")) %>% 
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Cook's Distance Plot
  output$outlier_cooks_plot <- renderPlotly({
    res <- outlier_analysis_reactive()$Cooks
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "Run analysis first (requires DEATH_RATE)", x = 0.5, y = 0.5))
    
    plot_df <- data.frame(dc = res$scores, id = (1:length(res$scores))/length(res$scores),
                          label = ifelse(res$is_outlier, names(res$scores), NA),
                          Observations = ifelse(res$is_outlier, "outlier", "non-outlier"))
    
    p <- ggplot(plot_df, aes(y = dc, x = id, color = Observations, label = label)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_text(aes(label = label), hjust = -0.1, vjust = 0.5, size = 3, check_overlap = TRUE, na.rm = TRUE) +
      scale_color_manual(values = c("non-outlier" = "blue", "outlier" = "red")) +
      labs(y = "Cook's Distance", x = "Complete Observations (percentile)",
           title = paste("Cook's Distance Outliers (method:", res$method, ")")) +
      geom_hline(yintercept = res$threshold, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p, tooltip = c("y", "label"))
  })
  
  # Local Outlier Factor Plot
  output$outlier_lof_plot <- renderPlotly({
    res <- outlier_analysis_reactive()$LOF
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "Run analysis first", x = 0.5, y = 0.5))
    
    plot_df <- data.frame(lof = res$scores, id = (1:length(res$scores))/length(res$scores),
                          label = ifelse(res$is_outlier, names(res$scores), NA),
                          Observations = ifelse(res$is_outlier, "outlier", "non-outlier"))
    
    p <- ggplot(plot_df, aes(y = lof, x = id, color = Observations, label = label)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_text(aes(label = label), hjust = -0.1, vjust = 0.5, size = 3, check_overlap = TRUE, na.rm = TRUE) +
      scale_color_manual(values = c("non-outlier" = "blue", "outlier" = "red")) +
      labs(y = "Local Outlier Factor", x = "Complete Observations (percentile)",
           title = paste("LOF Outliers (minPts =", res$minPts, ", threshold =", res$threshold, ")")) +
      geom_hline(yintercept = res$threshold, color = "black", linetype = "dashed") +
      scale_y_continuous(limits = c(0, NA)) +
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p, tooltip = c("y", "label"))
  })
  
  # One-Class SVM Plot
  output$outlier_svm_plot <- renderPlotly({
    res <- outlier_analysis_reactive()$SVM
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "Run analysis first", x = 0.5, y = 0.5))
    
    plot_df <- data.frame(score = res$scores, id = (1:length(res$scores))/length(res$scores),
                          label = ifelse(res$is_outlier, names(res$scores), NA),
                          Observations = ifelse(res$is_outlier, "outlier", "non-outlier"))
    
    p <- ggplot(plot_df, aes(y = score, x = id, color = Observations, label = label)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_text(aes(label = label), hjust = -0.1, vjust = 0.5, size = 3, check_overlap = TRUE, na.rm = TRUE) +
      scale_color_manual(values = c("non-outlier" = "blue", "outlier" = "red")) +
      labs(y = "SVM Decision Value", x = "Complete Observations (percentile)",
           title = paste("One-Class SVM Outliers (nu =", res$nu, ", kernel =", res$kernel, ")")) +
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p, tooltip = c("y", "label"))
  })
  
  # Random Forest Residuals Plot
  output$outlier_rf_plot <- renderPlotly({
    res <- outlier_analysis_reactive()$RF_Residuals
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "Run analysis first (requires DEATH_RATE)", x = 0.5, y = 0.5))
    
    plot_df <- data.frame(residual = res$scores, id = 1:length(res$scores),
                          label = ifelse(res$is_outlier, names(res$scores), NA),
                          Observations = ifelse(res$is_outlier, "outlier", "non-outlier"))
    
    fig <- plot_ly()
    fig <- fig %>% add_boxplot(y = ~residual, data = plot_df, name = "Residuals",
                               boxmean = "sd", jitter = 0.2, pointpos = -1.8,
                               marker = list(color = 'rgb(7,40,89)', size = 5),
                               line = list(color = 'rgb(7,40,89)', width = 1),
                               fillcolor = 'lightblue')
    
    outlier_df <- plot_df[plot_df$Observations == "outlier", ]
    if(nrow(outlier_df) > 0) {
      fig <- fig %>% add_trace(x = outlier_df$label, y = outlier_df$residual,
                               type = 'scatter', mode = 'markers+text',
                               text = outlier_df$label, textposition = 'top right',
                               textfont = list(size = 8), marker = list(color = 'red', size = 8),
                               name = 'Outliers',
                               hovertemplate = paste('Observation: %{text}<br>Residual: %{y:.3f}<extra></extra>'))
    }
    
    fig <- fig %>% layout(title = paste("RF Residual Outliers (IQR multiplier =", res$iqr_multiplier, ")"),
                          yaxis = list(title = "Random Forest Residuals", zeroline = TRUE),
                          xaxis = list(title = "", showticklabels = FALSE), showlegend = TRUE, height = 500)
    return(fig)
  })
  
  # Isolation Forest Plot
  output$outlier_if_plot <- renderPlotly({
    res <- outlier_analysis_reactive()$IForest
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "Run analysis first", x = 0.5, y = 0.5))
    
    plot_df <- data.frame(score = res$scores, id = (1:length(res$scores))/length(res$scores),
                          label = ifelse(res$is_outlier, names(res$scores), NA),
                          Observations = ifelse(res$is_outlier, "outlier", "non-outlier"))
    
    p <- ggplot(plot_df, aes(y = score, x = id, color = Observations, label = label)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_text(aes(label = label), hjust = -0.1, vjust = 0.5, size = 3, check_overlap = TRUE, na.rm = TRUE) +
      scale_color_manual(values = c("non-outlier" = "blue", "outlier" = "red")) +
      labs(y = "Isolation Forest Score", x = "Complete Observations (percentile)",
           title = paste("Isolation Forest Outliers (threshold =", res$threshold, ")")) +
      geom_hline(yintercept = res$threshold, color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p, tooltip = c("y", "label")) %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # ==========================================================================
  # TAB 3: RECIPE PIPELINE
  # ==========================================================================
  
  observeEvent(input$build_recipe, {
    tryCatch({
      data <- processed_data_working()
      if(is.character(data$DEATH_RATE)) {
        data$DEATH_RATE[data$DEATH_RATE %in% c("-99", "--", "NA")] <- NA
        data$DEATH_RATE <- suppressWarnings(as.numeric(data$DEATH_RATE))
      }
      data <- data[!is.na(data$DEATH_RATE), ]
      rec <- recipe(DEATH_RATE ~ ., data = data) %>%
        update_role(CODE, new_role = "id") %>% update_role(OBS_TYPE, new_role = "split")
      numeric_predictors <- names(data)[sapply(data, is.numeric)]
      numeric_predictors <- numeric_predictors[!numeric_predictors %in% c("DEATH_RATE", "CODE", "OBS_TYPE")]
      if(length(numeric_predictors) > 0) {
        if(input$recipe_impute_method != "none") {
          if(input$recipe_impute_method == "median") {
            rec <- rec %>% step_impute_median(all_of(numeric_predictors))
          } else if(input$recipe_impute_method == "mean") {
            rec <- rec %>% step_impute_mean(all_of(numeric_predictors))
          }
        }
        if(input$recipe_center_scale) {
          rec <- rec %>% step_center(all_of(numeric_predictors)) %>% step_scale(all_of(numeric_predictors))
        }
      }
      categorical_predictors <- names(data)[sapply(data, is.character)]
      categorical_predictors <- categorical_predictors[!categorical_predictors %in% c("CODE", "OBS_TYPE")]
      if(input$recipe_dummy && length(categorical_predictors) > 0) {
        rec <- rec %>% step_dummy(all_of(categorical_predictors))
      }
      recipe_object(rec)
      output$recipe_structure <- renderPrint({ cat("Recipe Structure:\n"); print(rec); cat("\nNumber of steps:", length(rec$steps), "\n") })
      showNotification("Recipe built successfully!", type = "success", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error building recipe:", e$message), type = "error", duration = 5)
    })
  })
  
  observeEvent(input$save_recipe, {
    req(recipe_object())
    recipe_name <- input$recipe_save_name
    if(is.null(recipe_name) || recipe_name == "") recipe_name <- paste0("Recipe_", length(recipe_names()) + 1)
    current_recipes <- recipe_objects()
    current_names <- recipe_names()
    recipe_config <- list(name = recipe_name, recipe = recipe_object(), timestamp = Sys.time(),
                          config = list(impute = input$recipe_impute_method, outlier = input$recipe_outlier_method,
                                        transform = input$recipe_transform, center_scale = input$recipe_center_scale,
                                        dummy = input$recipe_dummy, interactions = input$recipe_add_interactions,
                                        polynomial = input$recipe_add_polynomial, feature_selection = input$recipe_feature_selection))
    current_recipes[[recipe_name]] <- recipe_config
    recipe_objects(current_recipes)
    recipe_names(c(current_names, recipe_name))
    updateSelectInput(session, "compare_recipe_select", choices = recipe_names(), selected = NULL)
    showNotification(paste("Recipe saved as:", recipe_name), type = "success")
  })
  
  observeEvent(input$apply_recipe, {
    req(recipe_object())
    data <- processed_data_working()
    if(is.character(data$DEATH_RATE)) {
      data$DEATH_RATE[data$DEATH_RATE %in% c("-99", "--", "NA")] <- NA
      data$DEATH_RATE <- suppressWarnings(as.numeric(data$DEATH_RATE))
    }
    tryCatch({
      rec_prepped <- prep(recipe_object(), training = data, verbose = FALSE)
      current_recipe_prepped(rec_prepped)
      processed <- bake(rec_prepped, new_data = data)
      recipe_processed_data(processed)
      output$recipe_processed_data <- renderDT({
        preview_data <- processed[, !names(processed) %in% c("CODE", "OBS_TYPE"), drop = FALSE]
        datatable(head(preview_data, 20), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE) %>%
          formatRound(columns = which(sapply(preview_data, is.numeric)), digits = 3)
      })
      output$recipe_summary <- renderPrint({
        cat("Recipe Application Summary\n==========================\n\n")
        cat("Original data:\n  Rows:", nrow(data), "\n  Columns:", ncol(data), "\n  Missing values:", sum(is.na(data)), "\n\n")
        cat("Processed data:\n  Rows:", nrow(processed), "\n  Columns:", ncol(processed), "\n  Missing values:", sum(is.na(processed)), "\n\n")
        cat("Variables in processed data:\n")
        for(col in names(processed)) {
          if(col != "CODE" && col != "OBS_TYPE") cat("  -", col, ":", class(processed[[col]])[1], "\n")
        }
      })
      showNotification("Recipe applied successfully! You can now use this data for modeling.", type = "success", duration = 4)
    }, error = function(e) {
      showNotification(paste("Error applying recipe:", e$message), type = "error", duration = 5)
    })
  })
  
  observeEvent(input$run_recipe_comparison, {
    req(length(recipe_names()) >= 2)
    req(input$compare_recipe_select)
    selected_recipes <- input$compare_recipe_select
    data <- processed_data_working()
    if(is.character(data$DEATH_RATE)) {
      data$DEATH_RATE[data$DEATH_RATE %in% c("-99", "--", "NA")] <- NA
      data$DEATH_RATE <- suppressWarnings(as.numeric(data$DEATH_RATE))
    }
    comparison_list <- list()
    withProgress(message = 'Comparing recipes...', value = 0, {
      for(i in seq_along(selected_recipes)) {
        incProgress(1/length(selected_recipes), detail = paste("Processing:", selected_recipes[i]))
        recipe_config <- recipe_objects()[[selected_recipes[i]]]
        recipe_obj <- recipe_config$recipe
        tryCatch({
          rec_prepped <- prep(recipe_obj, training = data, verbose = FALSE)
          processed <- bake(rec_prepped, new_data = data)
          if("DEATH_RATE" %in% names(processed) && nrow(processed) > 10) {
            predictor_vars <- setdiff(names(processed), c("DEATH_RATE", "CODE", "OBS_TYPE"))
            predictor_vars <- predictor_vars[sapply(processed[, predictor_vars, drop = FALSE], function(x) sum(!is.na(x)) > 0)]
            if(length(predictor_vars) > 0) {
              X <- as.matrix(processed[, predictor_vars])
              y <- processed$DEATH_RATE
              complete_idx <- complete.cases(X, y)
              if(sum(complete_idx) > 20) {
                X <- X[complete_idx, ]; y <- y[complete_idx]
                if("OBS_TYPE" %in% names(processed)) {
                  train_idx <- processed$OBS_TYPE[complete_idx] == "Train"
                  if(sum(train_idx) == 0) train_idx <- sample(1:length(y), 0.7 * length(y))
                } else {
                  set.seed(123); train_idx <- sample(1:length(y), 0.7 * length(y))
                }
                X_train <- X[train_idx, ]; X_test <- X[!train_idx, ]
                y_train <- y[train_idx]; y_test <- y[!train_idx]
                cv_model <- cv.glmnet(X_train, y_train, alpha = 0.5, nfolds = min(5, nrow(X_train) - 1))
                final_model <- glmnet(X_train, y_train, alpha = 0.5, lambda = cv_model$lambda.min)
                train_pred <- predict(final_model, newx = X_train)
                test_pred <- predict(final_model, newx = X_test)
                comparison_list[[selected_recipes[i]]] <- data.frame(
                  Recipe = selected_recipes[i],
                  Train_RMSE = sqrt(mean((y_train - train_pred)^2)),
                  Test_RMSE = sqrt(mean((y_test - test_pred)^2)),
                  Train_R2 = 1 - sum((y_train - train_pred)^2) / sum((y_train - mean(y_train))^2),
                  Test_R2 = 1 - sum((y_test - test_pred)^2) / sum((y_test - mean(y_test))^2),
                  Variables = ncol(X), Observations = nrow(X), Timestamp = recipe_config$timestamp)
              }
            }
          }
        }, error = function(e) { cat("Error with recipe", selected_recipes[i], ":", e$message, "\n") })
      }
    })
    if(length(comparison_list) > 0) {
      comparison_df <- do.call(rbind, comparison_list)
      recipe_comparison_results(comparison_df)
      output$recipe_comparison_table <- renderDT({
        datatable(comparison_df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
          formatRound(columns = c("Train_RMSE", "Test_RMSE", "Train_R2", "Test_R2"), digits = 4)
      })
      output$recipe_comparison_plot <- renderPlotly({
        plot_df <- comparison_df
        p1 <- ggplot(plot_df, aes(x = Recipe, y = Test_RMSE, fill = Recipe)) +
          geom_bar(stat = "identity") + labs(title = "Test RMSE by Recipe", y = "RMSE") +
          theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p1, height = 400) %>% layout(showlegend = FALSE)
      })
      showNotification(paste("Compared", nrow(comparison_df), "recipes"), type = "success")
    } else {
      showNotification("Could not compare recipes. Check if data is valid.", type = "warning")
    }
  })
  
  observeEvent(input$reset_recipe, {
    recipe_object(NULL)
    recipe_processed_data(NULL)
    current_recipe_prepped(NULL)
    output$recipe_structure <- renderPrint({ cat("No recipe built") })
    output$recipe_processed_data <- renderDT({ datatable(data.frame(Message = "No recipe applied")) })
    output$recipe_summary <- renderPrint({ cat("No recipe applied") })
    output$recipe_comparison_table <- renderDT({ datatable(data.frame(Message = "No comparison run")) })
    showNotification("Recipe reset. Using original data for modeling.", type = "info")
  })
  
  # ==========================================================================
  # TAB 4: GLMNET MODELING
  # ==========================================================================
  
  model_data_source <- reactive({
    if(input$model_data_source == "recipe" && !is.null(recipe_processed_data())) {
      data <- recipe_processed_data()
    } else if(input$model_data_source == "processed") {
      data <- processed_data_working()
      data <- standardize_missing(data)
    } else {
      data <- standardize_missing(df)
    }
    return(data)
  })
  
  observeEvent(input$refresh_model_data, {
    showNotification("Refreshing processed data...", type = "default", duration = 2)
  })
  
  observe({
    if(input$model_data_source == "recipe" && !is.null(recipe_processed_data())) {
      data <- recipe_processed_data()
    } else if(input$model_data_source == "processed") {
      data <- processed_data_working()
      data <- standardize_missing(data)
    } else {
      data <- standardize_missing(df)
    }
    if("DEATH_RATE" %in% names(data) && is.character(data$DEATH_RATE)) {
      data$DEATH_RATE[data$DEATH_RATE %in% c("-99", "--", "NA")] <- NA
      data$DEATH_RATE <- suppressWarnings(as.numeric(data$DEATH_RATE))
    }
    data <- data[!is.na(data$DEATH_RATE), ]
    if("OBS_TYPE" %in% names(data)) {
      train_idx <- which(data$OBS_TYPE == "Train")
      test_idx <- which(data$OBS_TYPE == "Test")
      if(length(train_idx) == 0 || length(test_idx) == 0) {
        set.seed(123)
        train_idx <- sample(1:nrow(data), 0.7 * nrow(data))
        test_idx <- setdiff(1:nrow(data), train_idx)
        showNotification("OBS_TYPE column missing Train/Test values. Using random 70/30 split.", type = "warning")
      }
    } else {
      set.seed(123)
      train_idx <- sample(1:nrow(data), 0.7 * nrow(data))
      test_idx <- setdiff(1:nrow(data), train_idx)
    }
    train_test_split(list(train = data[train_idx, ], test = data[test_idx, ]))
  })
  
  observeEvent(input$train_model, {
    req(!is.null(train_test_split()))
    split <- train_test_split()
    req(!is.null(split))
    train_data <- split$train
    test_data <- split$test
    if(is.null(train_data) || nrow(train_data) == 0) {
      showNotification("Training data is empty. Please check your data source.", type = "error")
      return()
    }
    if(!"DEATH_RATE" %in% names(train_data)) {
      showNotification("DEATH_RATE column not found in data.", type = "error")
      return()
    }
    predictor_vars <- setdiff(names(train_data), c("DEATH_RATE", "CODE", "OBS_TYPE"))
    is_numeric <- sapply(train_data[, predictor_vars, drop = FALSE], is.numeric)
    is_numeric[is.na(is_numeric)] <- FALSE
    predictor_vars <- predictor_vars[is_numeric]
    if(length(predictor_vars) > 0) {
      not_all_na <- sapply(train_data[, predictor_vars, drop = FALSE], function(x) { result <- !all(is.na(x)); if(is.na(result)) result <- FALSE; return(result) })
      predictor_vars <- predictor_vars[not_all_na]
    }
    if(length(predictor_vars) == 0) {
      showNotification("No valid numeric predictors found. Check your data for NA values or select different data source.", type = "error")
      return()
    }
    X_train <- tryCatch({ as.matrix(train_data[, predictor_vars, drop = FALSE]) }, error = function(e) { showNotification(paste("Error creating predictor matrix:", e$message), type = "error"); return(NULL) })
    y_train <- train_data$DEATH_RATE
    if(is.null(X_train)) return()
    complete_train <- tryCatch({ complete.cases(X_train, y_train) }, error = function(e) { showNotification(paste("Error checking complete cases:", e$message), type = "error"); return(rep(FALSE, nrow(X_train))) })
    if(sum(complete_train, na.rm = TRUE) < 10) {
      showNotification(paste("Insufficient complete cases:", sum(complete_train, na.rm = TRUE), "Need at least 10."), type = "error")
      return()
    }
    X_train <- X_train[complete_train, , drop = FALSE]
    y_train <- y_train[complete_train]
    X_test <- tryCatch({ as.matrix(test_data[, predictor_vars, drop = FALSE]) }, error = function(e) { showNotification(paste("Error creating test matrix:", e$message), type = "warning"); return(NULL) })
    y_test <- test_data$DEATH_RATE
    if(!is.null(X_test)) {
      complete_test <- complete.cases(X_test, y_test)
      if(sum(complete_test, na.rm = TRUE) > 0) {
        X_test <- X_test[complete_test, , drop = FALSE]
        y_test <- y_test[complete_test]
      } else {
        X_test <- NULL; y_test <- NULL
      }
    }
    data_source_msg <- if(input$model_data_source == "processed") "PROCESSED DATA" else if(input$model_data_source == "recipe") "RECIPE DATA" else "ORIGINAL DATA"
    showNotification(paste("Training model on", data_source_msg, "-", nrow(X_train), "observations,", length(predictor_vars), "predictors"), type = "default", duration = 3)
    tryCatch({
      set.seed(input$cv_seed)
      cv_model <- cv.glmnet(X_train, y_train, alpha = as.numeric(input$glmnet_alpha), nfolds = min(input$cv_folds, nrow(X_train) - 1))
      lambda_opt <- if(is.null(input$glmnet_lambda) || is.na(input$glmnet_lambda) || input$glmnet_lambda == 0) { cv_model$lambda.min } else { input$glmnet_lambda }
      final_model <- glmnet(X_train, y_train, alpha = as.numeric(input$glmnet_alpha), lambda = lambda_opt)
      train_pred <- predict(final_model, newx = X_train)
      if(!is.null(X_test) && length(y_test) > 0) {
        test_pred <- predict(final_model, newx = X_test)
        test_rmse <- sqrt(mean((y_test - test_pred)^2))
        test_r2 <- 1 - sum((y_test - test_pred)^2) / sum((y_test - mean(y_test))^2)
      } else {
        test_pred <- NULL; test_rmse <- NA; test_r2 <- NA
        showNotification("No valid test data available. Using training data only.", type = "warning")
      }
      train_rmse <- sqrt(mean((y_train - train_pred)^2))
      train_r2 <- 1 - sum((y_train - train_pred)^2) / sum((y_train - mean(y_train))^2)
      model_results(list(model = final_model, cv_model = cv_model, train_pred = as.numeric(train_pred),
                         test_pred = if(!is.null(test_pred)) as.numeric(test_pred) else NULL,
                         y_train = y_train, y_test = y_test, lambda_opt = lambda_opt, lambda_min = cv_model$lambda.min,
                         lambda_1se = cv_model$lambda.1se, train_rmse = train_rmse, test_rmse = test_rmse,
                         train_r2 = train_r2, test_r2 = test_r2, predictors = predictor_vars, data_source = input$model_data_source))
      if(input$model_data_source == "processed") {
        showNotification(paste("Model trained on processed data! Test RMSE:", ifelse(is.na(test_rmse), "N/A", round(test_rmse, 4))), type = "success", duration = 5)
      } else if(input$model_data_source == "recipe") {
        showNotification(paste("Model trained on recipe data! Test RMSE:", ifelse(is.na(test_rmse), "N/A", round(test_rmse, 4))), type = "success", duration = 5)
      } else {
        showNotification(paste("Model trained on original data! Test RMSE:", ifelse(is.na(test_rmse), "N/A", round(test_rmse, 4))), type = "success", duration = 5)
      }
    }, error = function(e) { showNotification(paste("Model training error:", e$message), type = "error", duration = 8) })
  })
  
  output$model_performance <- renderPrint({
    res <- model_results()
    if(is.null(res)) { cat("No model trained yet.\n\nSelect data source and click 'Train GLMNET Model'"); return() }
    data_source_display <- switch(res$data_source, "original" = "ORIGINAL DATA", "processed" = "PROCESSED DATA (Tab 2)", "recipe" = "RECIPE DATA (Tab 3)", toupper(res$data_source))
    cat("GLMNET Model Performance\n========================\n\n")
    cat("Data Source:", data_source_display, "\n")
    cat("Predictors:", length(res$predictors), "\n")
    cat("Training observations:", length(res$y_train), "\n")
    if(!is.null(res$y_test) && length(res$y_test) > 0) cat("Test observations:", length(res$y_test), "\n") else cat("Test observations: None available\n")
    cat("\nTRAIN SET:\n  RMSE:", round(res$train_rmse, 4), "\n  R²:", round(res$train_r2, 4), "\n\n")
    cat("TEST SET:\n")
    if(!is.na(res$test_rmse)) cat("  RMSE:", round(res$test_rmse, 4), "\n  R²:", round(res$test_r2, 4), "\n") else cat("  No test results available\n")
  })
  
  output$model_hyperparameters <- renderPrint({
    res <- model_results()
    if(is.null(res)) { cat("No model trained yet."); return() }
    cat("Model Hyperparameters\n=====================\n\n")
    cat("Alpha (Elastic Net Mix):", input$glmnet_alpha, "\n")
    cat("Lambda (Regularization):", round(res$lambda_opt, 6), "\n")
    cat("Lambda.min (CV optimal):", round(res$lambda_min, 6), "\n")
    cat("Lambda.1se (CV 1-se rule):", round(res$lambda_1se, 6), "\n")
    cat("Cross-validation folds:", input$cv_folds, "\n")
    cat("Random seed:", input$cv_seed, "\n")
  })
  
  output$model_coefficients <- renderDT({
    res <- model_results()
    if(is.null(res)) return(datatable(data.frame(Message = "No model trained")))
    coef_matrix <- as.matrix(coef(res$model))
    coef_df <- data.frame(Variable = rownames(coef_matrix), Coefficient = as.numeric(coef_matrix[, 1]))
    coef_df <- coef_df[coef_df$Variable != "(Intercept)", ]
    coef_df <- coef_df[order(-abs(coef_df$Coefficient)), ]
    zero_count <- sum(coef_df$Coefficient == 0)
    nonzero_count <- nrow(coef_df) - zero_count
    datatable(coef_df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE,
              caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;',
                                                paste0(nonzero_count, " non-zero coefficients | ", zero_count, " zero coefficients"))) %>%
      formatRound("Coefficient", digits = 4) %>%
      formatStyle("Coefficient", background = styleColorBar(c(-1, 1), 'lightblue'),
                  backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
  })
  
  output$coef_plot <- renderPlotly({
    res <- model_results()
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "No model trained", x = 0.5, y = 0.5))
    coef_matrix <- as.matrix(coef(res$model))
    coef_df <- data.frame(Variable = rownames(coef_matrix), Coefficient = as.numeric(coef_matrix[, 1]))
    coef_df <- coef_df[coef_df$Variable != "(Intercept)", ]
    coef_df <- coef_df[order(coef_df$Coefficient), ]
    colors <- ifelse(coef_df$Coefficient > 0, "steelblue", "coral")
    plot_ly(coef_df, x = ~Coefficient, y = ~reorder(Variable, Coefficient), type = "bar", orientation = "h",
            marker = list(color = colors), text = ~paste(Variable, ": ", round(Coefficient, 4)), hoverinfo = "text") %>%
      layout(title = "Coefficient Estimates", xaxis = list(title = "Coefficient Value", gridcolor = "lightgray"),
             yaxis = list(title = "", gridcolor = "lightgray"), plot_bgcolor = "white", margin = list(l = 120))
  })
  
  output$predictions_plot <- renderPlotly({
    res <- model_results()
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "No model trained", x = 0.5, y = 0.5))
    split <- train_test_split()
    train_data <- split$train; test_data <- split$test
    train_codes <- as.character(train_data$CODE[complete.cases(train_data[, res$predictors])])
    test_codes <- as.character(test_data$CODE[complete.cases(test_data[, res$predictors])])
    if(length(train_codes) > length(res$train_pred)) train_codes <- train_codes[1:length(res$train_pred)]
    if(length(test_codes) > length(res$test_pred)) test_codes <- test_codes[1:length(res$test_pred)]
    plot_df <- data.frame(Actual = c(res$y_train, res$y_test), Predicted = c(res$train_pred, res$test_pred),
                          Set = rep(c("Train", "Test"), c(length(res$y_train), length(res$y_test))),
                          CODE = c(as.character(train_codes), as.character(test_codes)))
    p <- ggplot(plot_df, aes(x = Actual, y = Predicted, color = Set, 
                             text = paste("CODE:", CODE, "<br>Set:", Set, "<br>Actual:", round(Actual, 3), "<br>Predicted:", round(Predicted, 3)))) +
      geom_point(alpha = 0.6, size = 2) + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", size = 1) +
      labs(title = "Predictions vs Actual Values", x = "Actual Death Rate", y = "Predicted Death Rate") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white", font = list(size = 10)))
  })
  
  output$residual_plot <- renderPlotly({
    res <- model_results()
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "No model trained", x = 0.5, y = 0.5))
    split <- train_test_split()
    test_data <- split$test
    train_codes <- rownames(split$train)[complete.cases(split$train[, res$predictors])]
    test_codes <- test_data$CODE[complete.cases(test_data[, res$predictors])]
    if(length(train_codes) > length(res$train_pred)) train_codes <- train_codes[1:length(res$train_pred)]
    if(length(test_codes) > length(res$test_pred)) test_codes <- test_codes[1:length(res$test_pred)]
    train_residuals <- res$y_train - res$train_pred
    plot_df <- data.frame(Residuals = train_residuals, Set = "Train", Predicted = res$train_pred, CODE = as.character(train_codes))
    if(!is.null(res$test_pred) && length(res$test_pred) > 0) {
      test_residuals <- res$y_test - res$test_pred
      plot_df <- rbind(plot_df, data.frame(Residuals = test_residuals, Set = "Test", Predicted = res$test_pred, CODE = as.character(test_codes)))
    }
    p <- ggplot(plot_df, aes(x = Predicted, y = Residuals, color = Set, text = paste("CODE:", CODE, "<br>Predicted:", round(Predicted, 3), "<br>Residual:", round(Residuals, 3)))) +
      geom_point(alpha = 0.6, size = 2) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 1) +
      labs(title = "Residuals vs Fitted Values", x = "Predicted Death Rate", y = "Residuals") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white", font = list(size = 10)))
  })
  
  output$predictions_summary <- renderPrint({
    res <- model_results()
    if(is.null(res)) { cat("No model trained yet."); return() }
    cat("Predictions Summary\n==================\n\n")
    cat("TRAIN SET:\n  Min:", round(min(res$train_pred), 4), "\n  Max:", round(max(res$train_pred), 4),
        "\n  Mean:", round(mean(res$train_pred), 4), "\n  SD:", round(sd(res$train_pred), 4), "\n\n")
    if(!is.null(res$test_pred) && length(res$test_pred) > 0) {
      cat("TEST SET:\n  Min:", round(min(res$test_pred), 4), "\n  Max:", round(max(res$test_pred), 4),
          "\n  Mean:", round(mean(res$test_pred), 4), "\n  SD:", round(sd(res$test_pred), 4), "\n")
    } else { cat("TEST SET: No predictions available\n") }
  })
  
  output$residual_stats <- renderPrint({
    res <- model_results()
    if(is.null(res)) { cat("No model trained yet."); return() }
    train_residuals <- res$y_train - res$train_pred
    cat("Residual Analysis\n=================\n\n")
    cat("TRAIN SET:\n  Min:", round(min(train_residuals), 4), "\n  Max:", round(max(train_residuals), 4),
        "\n  Mean:", round(mean(train_residuals), 4), "\n  SD:", round(sd(train_residuals), 4),
        "\n  Q1:", round(quantile(train_residuals, 0.25), 4), "\n  Median:", round(median(train_residuals), 4),
        "\n  Q3:", round(quantile(train_residuals, 0.75), 4), "\n\n")
    if(!is.null(res$test_pred) && length(res$test_pred) > 0) {
      test_residuals <- res$y_test - res$test_pred
      cat("TEST SET:\n  Min:", round(min(test_residuals), 4), "\n  Max:", round(max(test_residuals), 4),
          "\n  Mean:", round(mean(test_residuals), 4), "\n  SD:", round(sd(test_residuals), 4),
          "\n  Q1:", round(quantile(test_residuals, 0.25), 4), "\n  Median:", round(median(test_residuals), 4),
          "\n  Q3:", round(quantile(test_residuals, 0.75), 4), "\n")
    }
  })
  
  output$residual_outliers <- renderDT({
    res <- model_results()
    if(is.null(res) || is.null(res$test_pred) || length(res$test_pred) == 0) {
      return(datatable(data.frame(Message = "No test predictions available. Train a model first.")))
    }
    residuals <- res$y_test - res$test_pred
    Q1 <- quantile(residuals, 0.25, na.rm = TRUE); Q3 <- quantile(residuals, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - input$residual_iqr * IQR; upper_bound <- Q3 + input$residual_iqr * IQR
    outlier_idx <- which(residuals < lower_bound | residuals > upper_bound)
    if(length(outlier_idx) == 0) {
      return(datatable(data.frame(Message = paste("No outliers detected with IQR multiplier =", input$residual_iqr))))
    }
    outlier_df <- data.frame(Observation = outlier_idx, Residual = round(residuals[outlier_idx], 4),
                             Predicted = round(res$test_pred[outlier_idx], 4), Actual = round(res$y_test[outlier_idx], 4),
                             Std_Residual = round(residuals[outlier_idx] / sd(residuals, na.rm = TRUE), 3))
    datatable(outlier_df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
              caption = paste("Total outliers:", length(outlier_idx), "(", round(100 * length(outlier_idx) / length(residuals), 1), "% of test data)")) %>%
      formatStyle("Residual", backgroundColor = styleInterval(c(lower_bound, upper_bound), c("white", "#ffcccc", "white")))
  })
  
  output$cv_plot <- renderPlotly({
    res <- model_results()
    if(is.null(res) || is.null(res$cv_model)) {
      return(plot_ly() %>% add_annotations(text = "No CV model available. Train a model first.", x = 0.5, y = 0.5))
    }
    cv_df <- data.frame(log_lambda = log(res$cv_model$lambda), cvm = res$cv_model$cvm,
                        cvup = res$cv_model$cvup, cvlo = res$cv_model$cvlo)
    p <- ggplot(cv_df, aes(x = log_lambda, y = cvm)) +
      geom_ribbon(aes(ymin = cvlo, ymax = cvup), alpha = 0.3, fill = "steelblue") +
      geom_line(color = "steelblue", size = 1) + geom_point(size = 1.5) +
      geom_vline(xintercept = log(res$lambda_min), linetype = "dashed", color = "red", size = 1) +
      geom_vline(xintercept = log(res$lambda_1se), linetype = "dotted", color = "blue", size = 1) +
      labs(title = "Cross-Validation Error", x = "log(Lambda)", y = "CV Error (MSE)") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white", font = list(size = 10)))
  })
  
  output$cv_summary <- renderPrint({
    res <- model_results()
    if(is.null(res) || is.null(res$cv_model)) { cat("No CV model available. Train a model first."); return() }
    cat("Cross-Validation Summary\n========================\n\n")
    cat("Number of folds:", input$cv_folds, "\n")
    cat("Lambda.min (minimum CV error):", round(res$lambda_min, 6), "\n")
    cat("Lambda.1se (1-standard error rule):", round(res$lambda_1se, 6), "\n")
    cat("Selected lambda:", round(res$lambda_opt, 6), "\n\n")
    cat("CV Error at lambda.min:", round(min(res$cv_model$cvm), 6), "\n")
    cat("CV Error at lambda.1se:", round(res$cv_model$cvm[which(res$cv_model$lambda == res$lambda_1se)], 6), "\n")
  })
  
  # ==========================================================================
  # TAB 5: REPORT SUMMARY
  # ==========================================================================
  
  output$report_data_desc <- renderText({ 
    paste("Dataset contains", nrow(df), "rows and", ncol(df), "columns. ",
          "The outcome variable is DEATH_RATE (projected death rate across ten years). ",
          "There are", length(numeric_cols), "numeric predictors and", length(categorical_cols), 
          "categorical variables including the state CODE and OBS_TYPE for train/test allocation.")
  })
  
  output$report_missing_strategy <- renderText({ 
    paste("Missing values are represented by '-99', '--', and 'NA' strings. ",
          "Users can remove columns exceeding", input$proc_col_missing_threshold, "% missing, ",
          "remove rows with more than", input$proc_max_missing_per_row, "missing values, ",
          "impute numeric missing values using median/mean/constant, ",
          "or impute categorical missing values using mode/new category/constant. ",
          "Shadow variables can also be created to track missingness patterns.")
  })
  
  output$report_outlier_strategy <- renderText({ 
    res <- model_results()
    if(!is.null(res) && !is.null(res$test_pred)) {
      residuals <- res$y_test - res$test_pred
      Q1 <- quantile(residuals, 0.25, na.rm = TRUE); Q3 <- quantile(residuals, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - input$residual_iqr * IQR; upper <- Q3 + input$residual_iqr * IQR
      n_outliers <- sum(residuals < lower | residuals > upper, na.rm = TRUE)
      outlier_pct <- round(100 * n_outliers / length(residuals), 1)
      paste("Outliers in residuals are identified using the IQR method with multiplier =", input$residual_iqr, ". ",
            "Currently", n_outliers, "test observations (", outlier_pct, "%) are flagged as residual outliers. ",
            "Users can adjust the IQR multiplier slider to change outlier sensitivity.")
    } else {
      paste("Outlier detection using IQR method with adjustable multiplier. Train a model first to see residual outlier analysis.")
    }
  })
  
  output$report_preprocess_strategy <- renderPrint({ 
    cat("Preprocessing Strategy:\n")
    cat("- Missing value imputation:", input$recipe_impute_method, "\n")
    cat("- Outlier treatment:", input$recipe_outlier_method, "\n")
    if(input$recipe_outlier_method != "none") cat("  Outlier percentile threshold:", input$recipe_outlier_percentile, "%\n")
    cat("- Transformation:", input$recipe_transform, "\n")
    cat("- Centering and scaling:", if(input$recipe_center_scale) "Yes" else "No", "\n")
    cat("- Dummy variables:", if(input$recipe_dummy) "Yes" else "No", "\n")
    cat("- Feature selection:", if(input$recipe_feature_selection) paste("Yes (top", input$recipe_select_n_features, "features)") else "No", "\n")
  })
  
  output$report_glmnet_explanation <- renderText({ 
    paste("GLMNET (Elastic Net) combines L1 (Lasso) and L2 (Ridge) regularization. ",
          "Alpha =", input$glmnet_alpha, "where 0 = pure Ridge, 1 = pure Lasso, and 0.5 = Elastic Net. ",
          "The model uses", input$cv_folds, "-fold cross-validation to select the optimal lambda parameter. ",
          "Lambda controls the strength of regularization - larger values increase penalization. ",
          "The model predicts DEATH_RATE using the selected numeric predictors after preprocessing.")
  })
  
  output$report_model_performance <- renderText({ 
    res <- model_results()
    if(is.null(res)) return("Train a model in Tab 4 to see performance metrics.")
    data_source <- switch(res$data_source, "original" = "Original Data", "processed" = "Processed Data (Tab 2)",
                          "recipe" = "Recipe Data (Tab 3)", "Unknown")
    paste("Model trained on:", data_source, ". Test RMSE =", round(res$test_rmse, 4), 
          ", Test R² =", round(res$test_r2, 4), ". The model uses", length(res$predictors),
          "predictors with alpha =", input$glmnet_alpha, "and lambda =", round(res$lambda_opt, 6), ".")
  })
  
  output$report_residual_summary <- renderText({ 
    res <- model_results()
    if(is.null(res) || is.null(res$test_pred)) return("Train a model first to see residual analysis.")
    residuals <- res$y_test - res$test_pred
    Q1 <- quantile(residuals, 0.25, na.rm = TRUE); Q3 <- quantile(residuals, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - input$residual_iqr * IQR; upper <- Q3 + input$residual_iqr * IQR
    n_outliers <- sum(residuals < lower | residuals > upper, na.rm = TRUE)
    outlier_pct <- round(100 * n_outliers / length(residuals), 1)
    paste("Test residuals analysis: mean =", round(mean(residuals), 4), ", sd =", round(sd(residuals), 4),
          ", min =", round(min(residuals), 4), ", max =", round(max(residuals), 4), ". ",
          "Using IQR multiplier =", input$residual_iqr, ", detected", n_outliers, "outliers (", outlier_pct, "% of test set).")
  })
  
} # End of server function