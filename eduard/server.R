server <- function(input, output, session) {
  
  # ========================================================================
  # ========================================================================
  # SECTION 1: HELPER FUNCTIONS
  # ========================================================================
  # ========================================================================
  
  # Define NULL coalescing operator
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
  
  # Safe NULL handler for missing value checks
  safe_in <- function(value, vector) {
    if(is.null(vector)) return(FALSE)
    return(value %in% vector)
  }
  
  # -------------------------------------------------------------------------
  # Center and Scale for Boxplot
  # -------------------------------------------------------------------------
  apply_center_scale <- function(data) {
    if(!input$boxplot_center && !input$boxplot_scale) return(data)
    
    data %>%
      dplyr::group_by(variable) %>%
      dplyr::mutate(
        value = if(input$boxplot_center && input$boxplot_scale) {
          as.numeric(scale(value))
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
  
  # Outlier Flag Addition for Boxplot
  add_outlier_flags <- function(data, iqr_multiplier) {
    if("group" %in% names(data)) {
      data %>% dplyr::group_by(variable, group) %>%
        dplyr::mutate(Q1 = quantile(value, 0.25, na.rm = TRUE), 
                      Q3 = quantile(value, 0.75, na.rm = TRUE),
                      IQR = Q3 - Q1, 
                      lower_bound = Q1 - iqr_multiplier * IQR,
                      upper_bound = Q3 + iqr_multiplier * IQR,
                      is_outlier = value < lower_bound | value > upper_bound) %>% 
        dplyr::ungroup()
    } else {
      data %>% dplyr::group_by(variable) %>%
        dplyr::mutate(Q1 = quantile(value, 0.25, na.rm = TRUE), 
                      Q3 = quantile(value, 0.75, na.rm = TRUE),
                      IQR = Q3 - Q1, 
                      lower_bound = Q1 - iqr_multiplier * IQR,
                      upper_bound = Q3 + iqr_multiplier * IQR,
                      is_outlier = value < lower_bound | value > upper_bound) %>% 
        dplyr::ungroup()
    }
  }
  
  # -------------------------------------------------------------------------
  # Missing Value Standardization
  # -------------------------------------------------------------------------
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
  
  # -------------------------------------------------------------------------
  # Remove outliers using IQR method
  # -------------------------------------------------------------------------
  remove_outliers_iqr <- function(data, vars, iqr_multiplier = 1.5) {
    result <- data
    outlier_rows <- rep(FALSE, nrow(data))
    
    for(var in vars) {
      if(var %in% names(data) && is.numeric(data[[var]])) {
        Q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
        Q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower <- Q1 - iqr_multiplier * IQR
        upper <- Q3 + iqr_multiplier * IQR
        outlier_rows <- outlier_rows | (data[[var]] < lower | data[[var]] > upper)
      }
    }
    
    result <- result[!outlier_rows, , drop = FALSE]
    removed <- sum(outlier_rows)
    return(list(data = result, removed = removed))
  }
  
  # -------------------------------------------------------------------------
  # Remove outliers using Mahalanobis distance
  # -------------------------------------------------------------------------
  remove_outliers_mahalanobis <- function(data, threshold_prob = 0.999) {
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    numeric_data <- numeric_data[, !names(numeric_data) %in% c("CODE", "OBS_TYPE"), drop = FALSE]
    
    if(ncol(numeric_data) < 2) {
      return(list(data = data, removed = 0, message = "Need at least 2 numeric variables"))
    }
    
    complete_idx <- complete.cases(numeric_data)
    if(sum(complete_idx) < 5) {
      return(list(data = data, removed = 0, message = "Insufficient complete cases"))
    }
    
    numeric_data <- numeric_data[complete_idx, ]
    
    Covar <- cov(numeric_data)
    Means <- colMeans(numeric_data)
    if(rcond(Covar) < 1e-10) Covar <- Covar + diag(ncol(numeric_data)) * 1e-6
    
    md2 <- mahalanobis(x = numeric_data, center = Means, cov = Covar)
    df <- ncol(numeric_data)
    threshold <- qchisq(p = threshold_prob, df = df)
    outlier_rows_md <- md2 > threshold
    
    outlier_indices <- which(complete_idx)[outlier_rows_md]
    outlier_flag <- rep(FALSE, nrow(data))
    outlier_flag[outlier_indices] <- TRUE
    
    result <- data[!outlier_flag, , drop = FALSE]
    removed <- sum(outlier_flag)
    
    return(list(data = result, removed = removed))
  }
  
  # -------------------------------------------------------------------------
  # Remove outliers using Cook's Distance
  # -------------------------------------------------------------------------
  remove_outliers_cooks <- function(data, method = "4mean") {
    if(!"DEATH_RATE" %in% names(data)) {
      return(list(data = data, removed = 0, message = "DEATH_RATE column required"))
    }
    
    predictor_vars <- names(data)[sapply(data, is.numeric)]
    predictor_vars <- predictor_vars[!predictor_vars %in% c("DEATH_RATE", "CODE", "OBS_TYPE")]
    
    if(length(predictor_vars) < 1) {
      return(list(data = data, removed = 0, message = "Need at least 1 predictor variable"))
    }
    
    model_data <- data[, c("DEATH_RATE", predictor_vars), drop = FALSE]
    complete_idx <- complete.cases(model_data)
    
    if(sum(complete_idx) < 10) {
      return(list(data = data, removed = 0, message = "Insufficient complete cases"))
    }
    
    model_data <- model_data[complete_idx, ]
    
    formula <- as.formula(paste("DEATH_RATE ~", paste(predictor_vars, collapse = " + ")))
    lmod <- lm(formula, data = model_data)
    dc <- cooks.distance(lmod)
    
    if(method == "4mean") {
      threshold <- 4 * mean(dc, na.rm = TRUE)
    } else if(method == "4n") {
      threshold <- 4 / length(dc)
    } else {
      threshold <- quantile(dc, 0.99, na.rm = TRUE)
    }
    
    outlier_rows_cooks <- dc > threshold
    outlier_indices <- which(complete_idx)[outlier_rows_cooks]
    outlier_flag <- rep(FALSE, nrow(data))
    outlier_flag[outlier_indices] <- TRUE
    
    result <- data[!outlier_flag, , drop = FALSE]
    removed <- sum(outlier_flag)
    
    return(list(data = result, removed = removed))
  }
  
  # -------------------------------------------------------------------------
  # Remove outliers using Local Outlier Factor (LOF)
  # -------------------------------------------------------------------------
  remove_outliers_lof <- function(data, minPts = 5, threshold = 2) {
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    numeric_data <- numeric_data[, !names(numeric_data) %in% c("CODE", "OBS_TYPE"), drop = FALSE]
    
    if(ncol(numeric_data) < 2) {
      return(list(data = data, removed = 0, message = "Need at least 2 numeric variables"))
    }
    
    complete_idx <- complete.cases(numeric_data)
    if(sum(complete_idx) < 10) {
      return(list(data = data, removed = 0, message = "Insufficient complete cases"))
    }
    
    numeric_data <- numeric_data[complete_idx, ]
    
    if(!requireNamespace("dbscan", quietly = TRUE)) {
      return(list(data = data, removed = 0, message = "dbscan package required"))
    }
    
    mat <- as.matrix(numeric_data)
    lof_scores <- dbscan::lof(mat, minPts = minPts)
    outlier_rows_lof <- lof_scores > threshold
    
    outlier_indices <- which(complete_idx)[outlier_rows_lof]
    outlier_flag <- rep(FALSE, nrow(data))
    outlier_flag[outlier_indices] <- TRUE
    
    result <- data[!outlier_flag, , drop = FALSE]
    removed <- sum(outlier_flag)
    
    return(list(data = result, removed = removed))
  }
  
  # -------------------------------------------------------------------------
  # Remove outliers using Isolation Forest
  # -------------------------------------------------------------------------
  remove_outliers_iforest <- function(data, threshold = 0.6) {
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    numeric_data <- numeric_data[, !names(numeric_data) %in% c("CODE", "OBS_TYPE"), drop = FALSE]
    
    if(ncol(numeric_data) < 2) {
      return(list(data = data, removed = 0, message = "Need at least 2 numeric variables"))
    }
    
    complete_idx <- complete.cases(numeric_data)
    if(sum(complete_idx) < 10) {
      return(list(data = data, removed = 0, message = "Insufficient complete cases"))
    }
    
    numeric_data <- numeric_data[complete_idx, ]
    
    if(!requireNamespace("isotree", quietly = TRUE)) {
      return(list(data = data, removed = 0, message = "isotree package required"))
    }
    
    itree <- isotree::isolation.forest(numeric_data, seed = 123)
    scores <- predict(itree, newdata = numeric_data)
    outlier_rows_if <- scores > threshold
    
    outlier_indices <- which(complete_idx)[outlier_rows_if]
    outlier_flag <- rep(FALSE, nrow(data))
    outlier_flag[outlier_indices] <- TRUE
    
    result <- data[!outlier_flag, , drop = FALSE]
    removed <- sum(outlier_flag)
    
    return(list(data = result, removed = removed))
  }
  
  # -------------------------------------------------------------------------
  # Variable ordering
  # -------------------------------------------------------------------------
  original_var_order <- c("CODE", "GOVERN_TYPE", "POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", 
                          "AGE50_PROPTN", "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", 
                          "VAX_RATE", "HEALTHCARE_BASIS", "HEALTHCARE_COST", "DEATH_RATE", "OBS_TYPE")
  
  numeric_cols <- c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", 
                    "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE", 
                    "HEALTHCARE_COST", "DEATH_RATE")
  
  categorical_cols <- c("CODE", "GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE")
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 2: REACTIVE VALUES & STORAGE
  # ========================================================================
  # ========================================================================
  
  reactive_data <- reactiveVal(df)
  model_results <- reactiveVal(NULL)
  train_test_split <- reactiveVal(NULL)
  missing_rpart_model <- reactiveVal(NULL)
  outlier_results <- reactiveVal(list())
  processed_data_working <- reactiveVal(standardize_missing(df))
  selected_model_dataset <- reactiveVal(NULL)
  
  saved_pipelines <- reactiveVal(list())
  saved_datasets <- reactiveVal(list())
  
  pipeline_steps <- reactiveVal(list())
  
  processing_log <- reactiveVal(character(0))
  
  # COMPLETE method display names
  method_display_names <- list(
    "remove_na_rows" = "Remove rows with NAs",
    "remove_na_cols" = "Remove columns with NAs",
    "remove_rows_by_index" = "Remove rows by index",
    "keep_rows_by_index" = "Keep rows by index",
    "remove_cols_by_name" = "Remove columns by name",
    "keep_cols_by_name" = "Keep columns by name",
    "remove_outliers_iqr" = "Remove outliers (IQR)",
    "remove_outliers_mahalanobis" = "Remove outliers (Mahalanobis)",
    "remove_outliers_cooks" = "Remove outliers (Cook's Distance)",
    "remove_outliers_lof" = "Remove outliers (LOF)",
    "remove_outliers_iforest" = "Remove outliers (Isolation Forest)",
    "impute_median" = "Impute with Median",
    "impute_mean" = "Impute with Mean",
    "impute_manual" = "Manual Imputation",
    "impute_knn" = "Impute with KNN",
    "impute_mice" = "Impute with MICE",
    "impute_rf" = "Impute with Random Forest",
    "log_transform" = "Log Transformation",
    "sqrt_transform" = "Square Root Transformation",
    "boxcox" = "Box-Cox Transformation",
    "yeojohnson" = "Yeo-Johnson Transformation",
    "scale" = "Scale Data (Z-score)",
    "center" = "Center Data",
    "one_hot_encode" = "One-Hot Encode Categorical Variables"
  )
  
  add_to_log <- function(message) {
    current_log <- processing_log()
    timestamp <- format(Sys.time(), "%H:%M:%S")
    new_log <- c(current_log, paste0("[", timestamp, "] ", message))
    processing_log(new_log)
  }
  
  observe({
    if(length(processing_log()) == 0) add_to_log("Session started. Working with original dataset.")
  })
  
  # -------------------------------------------------------------------------
  # Persistent Storage
  # -------------------------------------------------------------------------
  data_dir <- "saved_data"
  if(!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  
  save_all_data <- function() {
    tryCatch({
      datasets <- saved_datasets()
      pipelines <- saved_pipelines()
      save_list <- list(
        datasets = datasets,
        pipelines = pipelines,
        timestamp = as.character(Sys.time())
      )
      saveRDS(save_list, file.path(data_dir, "app_data_backup.rds"))
      cat("Data saved successfully\n")
    }, error = function(e) {
      cat("Save error:", e$message, "\n")
    })
  }
  
  load_all_data <- function() {
    backup_file <- file.path(data_dir, "app_data_backup.rds")
    if(file.exists(backup_file)) {
      tryCatch({
        save_list <- readRDS(backup_file)
        if(!is.null(save_list$datasets) && length(save_list$datasets) > 0) {
          saved_datasets(save_list$datasets)
        }
        if(!is.null(save_list$pipelines) && length(save_list$pipelines) > 0) {
          saved_pipelines(save_list$pipelines)
        }
        add_to_log(paste("Loaded", length(save_list$datasets %||% list()), 
                         "datasets and", length(save_list$pipelines %||% list()), 
                         "recipes from backup"))
        return(TRUE)
      }, error = function(e) {
        cat("Load error:", e$message, "\n")
        return(FALSE)
      })
    }
    return(FALSE)
  }
  
  auto_save_timer <- reactiveTimer(30000)
  observeEvent(auto_save_timer(), {
    if(length(saved_datasets()) > 0 || length(saved_pipelines()) > 0) {
      save_all_data()
    }
  })
  
  # Load saved data on startup
  observeEvent(TRUE, {
    isolate({ 
      load_all_data()
      Sys.sleep(0.1)
      datasets <- saved_datasets()
      if(length(datasets) > 0) {
        updatePickerInput(session, "model_dataset_select", 
                          choices = names(datasets),
                          selected = names(datasets)[length(names(datasets))])
      }
    })
  }, once = TRUE, ignoreNULL = FALSE)
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 3: MASTER FILTERED DATA (Data Processing Strategy Tab)
  # ========================================================================
  # ========================================================================
  
  master_filtered_data <- reactive({
    if(is.null(input$master_cat_vars) || is.null(input$master_numeric_vars)) {
      return(data.frame())
    }
    
    selected_cat <- input$master_cat_vars
    selected_num <- input$master_numeric_vars
    selected_vars <- unique(c(selected_cat, selected_num))
    
    if(length(selected_vars) == 0) {
      return(data.frame(CODE = character(), stringsAsFactors = FALSE))
    }
    
    data <- df
    selected_vars <- selected_vars[!selected_vars %in% c("CODE", "OBS_TYPE")]
    
    selected_vars <- selected_vars[selected_vars %in% names(data)]
    if(length(selected_vars) == 0) {
      return(data.frame(CODE = character(), stringsAsFactors = FALSE))
    }
    
    data <- data[, selected_vars, drop = FALSE]
    
    col_threshold <- input$master_col_missing_threshold %||% 100
    row_threshold <- input$master_row_missing_threshold %||% 15
    row_percent_threshold <- input$master_row_missing_percent %||% 100
    
    data <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
    
    col_missing_pct <- apply(data, 2, function(x) {
      sum(x %in% c("-99", "--", "NA") | is.na(x)) / length(x) * 100
    })
    
    if(length(col_missing_pct) > 0) {
      data <- data[, col_missing_pct <= col_threshold, drop = FALSE]
    }
    
    if(ncol(data) == 0) {
      return(data.frame(CODE = character(), stringsAsFactors = FALSE))
    }
    
    row_missing_count <- apply(data, 1, function(x) {
      sum(x %in% c("-99", "--", "NA") | is.na(x))
    })
    row_missing_percent <- row_missing_count / ncol(data) * 100
    
    keep_rows <- row_missing_count <= row_threshold & row_missing_percent <= row_percent_threshold
    data <- data[keep_rows, , drop = FALSE]
    
    if(input$master_order == "original") {
      ordered_cols <- original_var_order[original_var_order %in% names(data)]
      if(length(ordered_cols) > 0) {
        data <- data[, ordered_cols, drop = FALSE]
      }
    }
    
    if("CODE" %in% names(df) && !"CODE" %in% names(data)) {
      data$CODE <- df$CODE[keep_rows]
    }
    
    return(data)
  })
  
  master_filtered_data_standardized <- reactive({
    data <- master_filtered_data()
    
    if(is.null(data) || nrow(data) == 0) {
      return(data.frame())
    }
    
    for(col in names(data)) {
      vals <- as.character(data[[col]])
      was_numeric <- col %in% numeric_cols
      
      if(safe_in("neg99", input$master_mv_types)) vals[vals == "-99"] <- NA
      if(safe_in("dash", input$master_mv_types)) vals[vals == "--"] <- NA
      if(safe_in("na_string", input$master_mv_types)) vals[vals == "NA"] <- NA
      
      if(was_numeric) {
        data[[col]] <- suppressWarnings(as.numeric(vals))
      } else {
        data[[col]] <- vals
      }
    }
    
    return(data)
  })
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 4: SIDEBAR TOGGLE OBSERVERS
  # ========================================================================
  # ========================================================================
  
  sidebar_state <- reactiveValues(
    heatmap = TRUE, distribution = TRUE, boxplot = TRUE, correlation = TRUE,
    scatter = TRUE, mosaic = TRUE, ggpairs = TRUE, tabplot = TRUE, rising = TRUE, datatable = TRUE
  )
  
  toggle_sidebar <- function(wrapper_id, sidebar_state_name, session, button_id) {
    current_state <- sidebar_state[[sidebar_state_name]]
    if(current_state) {
      shinyjs::addClass(id = wrapper_id, class = "sidebar-hidden")
      sidebar_state[[sidebar_state_name]] <- FALSE
      updateActionButton(session, button_id, label = HTML('<i class="fa fa-sliders-h"></i> Show Filters'))
    } else {
      shinyjs::removeClass(id = wrapper_id, class = "sidebar-hidden")
      sidebar_state[[sidebar_state_name]] <- TRUE
      updateActionButton(session, button_id, label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'))
    }
  }
  
  observeEvent(input$toggle_heatmap_sidebar, { toggle_sidebar("heatmap_wrapper", "heatmap", session, "toggle_heatmap_sidebar") })
  observeEvent(input$toggle_distribution_sidebar, { toggle_sidebar("distribution_wrapper", "distribution", session, "toggle_distribution_sidebar") })
  observeEvent(input$toggle_boxplot_sidebar, { toggle_sidebar("boxplot_wrapper", "boxplot", session, "toggle_boxplot_sidebar") })
  observeEvent(input$toggle_correlation_sidebar, { toggle_sidebar("correlation_wrapper", "correlation", session, "toggle_correlation_sidebar") })
  observeEvent(input$toggle_scatter_sidebar, { toggle_sidebar("scatter_wrapper", "scatter", session, "toggle_scatter_sidebar") })
  observeEvent(input$toggle_mosaic_sidebar, { toggle_sidebar("mosaic_wrapper", "mosaic", session, "toggle_mosaic_sidebar") })
  observeEvent(input$toggle_ggpairs_sidebar, { toggle_sidebar("ggpairs_wrapper", "ggpairs", session, "toggle_ggpairs_sidebar") })
  observeEvent(input$toggle_tabplot_sidebar, { toggle_sidebar("tabplot_wrapper", "tabplot", session, "toggle_tabplot_sidebar") })
  observeEvent(input$toggle_rising_sidebar, { toggle_sidebar("rising_wrapper", "rising", session, "toggle_rising_sidebar") })
  observeEvent(input$toggle_datatable_sidebar, { toggle_sidebar("datatable_wrapper", "datatable", session, "toggle_datatable_sidebar") })
  
  master_sidebar_state <- reactiveValues(visible = TRUE)
  observeEvent(input$toggle_master_sidebar, {
    if(master_sidebar_state$visible) {
      shinyjs::addClass(id = "master_sidebar_wrapper", class = "sidebar-hidden")
      master_sidebar_state$visible <- FALSE
      updateActionButton(session, "toggle_master_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Show Master Filters'))
    } else {
      shinyjs::removeClass(id = "master_sidebar_wrapper", class = "sidebar-hidden")
      master_sidebar_state$visible <- TRUE
      updateActionButton(session, "toggle_master_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Hide Master Filters'))
    }
  })
  
  observeEvent(input$toggle_model_sidebar, {
    if(input$toggle_model_sidebar %% 2 == 1) {
      shinyjs::hide(id = "model_sidebar_panel")
      shinyjs::removeClass(id = "model_main_panel", class = "col-sm-9")
      shinyjs::addClass(id = "model_main_panel", class = "col-sm-12")
      updateActionButton(session, "toggle_model_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Show Settings Panel'))
    } else {
      shinyjs::show(id = "model_sidebar_panel")
      shinyjs::removeClass(id = "model_main_panel", class = "col-sm-12")
      shinyjs::addClass(id = "model_main_panel", class = "col-sm-9")
      updateActionButton(session, "toggle_model_sidebar", label = HTML('<i class="fa fa-sliders-h"></i> Hide Settings Panel'))
    }
  })
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 5: PROCESSING STEPS UI (Recipe Builder)
  # ========================================================================
  # ========================================================================
  
  output$processing_steps_ui <- renderUI({
    steps <- pipeline_steps()
    
    if(!is.list(steps) || length(steps) == 0) {
      return(div(class = "alert alert-info", 
                 icon("info-circle"), 
                 "No steps added yet. Click 'Add Step' to begin building your processing pipeline."))
    }
    
    current_data <- processed_data_working()
    
    # Get available columns for selection
    all_cols_available <- names(current_data)
    numeric_cols_available <- names(current_data)[sapply(current_data, is.numeric)]
    categorical_cols_available <- names(current_data)[!sapply(current_data, is.numeric)]
    
    # Remove protected columns from selection where appropriate
    numeric_cols_available <- numeric_cols_available[!numeric_cols_available %in% c("CODE", "OBS_TYPE")]
    
    tagList(lapply(seq_along(steps), function(idx) {
      step <- steps[[idx]]
      step_id <- step$id %||% idx
      additional_info <- step$additional_info
      
      div(class = "well", style = "padding: 10px; margin-bottom: 15px; background-color: #f9f9f9; border-left: 4px solid #2C3E50;",
          div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
              strong(paste("Step", idx), style = "color: #2C3E50;"),
              actionButton(paste0("remove_step_btn_", step_id), 
                           label = HTML('<i class="fa fa-trash"></i> Remove'), 
                           class = "btn-danger btn-xs",
                           onclick = paste0("Shiny.setInputValue('remove_step', ", step_id, ")"))
          ),
          
          selectInput(paste0("step_method_", step_id), "Processing Method",
                      choices = c("-- Select --" = "",
                                  "Row Operations" = "---",
                                  "Remove Rows with NAs" = "remove_na_rows",
                                  "Remove Rows by Index" = "remove_rows_by_index",
                                  "Keep Rows by Index" = "keep_rows_by_index",
                                  "Remove Outliers (IQR)" = "remove_outliers_iqr",
                                  "Remove Outliers (Mahalanobis)" = "remove_outliers_mahalanobis",
                                  "Remove Outliers (Cook's Distance)" = "remove_outliers_cooks",
                                  "Remove Outliers (LOF)" = "remove_outliers_lof",
                                  "Remove Outliers (Isolation Forest)" = "remove_outliers_iforest",
                                  "Column Operations" = "---",
                                  "Remove Columns with NAs" = "remove_na_cols",
                                  "Remove Columns by Name" = "remove_cols_by_name",
                                  "Keep Columns by Name" = "keep_cols_by_name",
                                  "Imputation" = "---",
                                  "Impute — Median" = "impute_median",
                                  "Impute — Mean" = "impute_mean",
                                  "Impute — Manual Value" = "impute_manual",
                                  "Impute — KNN" = "impute_knn",
                                  "Impute — MICE" = "impute_mice",
                                  "Impute — Random Forest" = "impute_rf",
                                  "Transformations" = "---",
                                  "Transform — Log" = "log_transform",
                                  "Transform — Square Root" = "sqrt_transform",
                                  "Transform — Box-Cox" = "boxcox",
                                  "Transform — Yeo-Johnson" = "yeojohnson",
                                  "Scale Data" = "scale",
                                  "Center Data" = "center",
                                  "Encoding" = "---",
                                  "One-Hot Encode" = "one_hot_encode"),
                      selected = step$method %||% ""),
          
          # Column selector for methods that need columns
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] != '' && 
                             input['step_method_", step_id, "'] != '---' &&
                             input['step_method_", step_id, "'] != 'remove_na_rows' && 
                             input['step_method_", step_id, "'] != 'remove_na_cols' &&
                             input['step_method_", step_id, "'] != 'remove_outliers_mahalanobis' &&
                             input['step_method_", step_id, "'] != 'remove_outliers_cooks' &&
                             input['step_method_", step_id, "'] != 'remove_outliers_lof' &&
                             input['step_method_", step_id, "'] != 'remove_outliers_iforest'"),
            pickerInput(paste0("step_cols_", step_id), "Select Variables",
                        choices = if(step$method %in% c("one_hot_encode")) categorical_cols_available else numeric_cols_available,
                        selected = step$cols %||% character(0),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                       `selected-text-format` = "count > 3"))
          ),
          
          # Remove NA rows threshold
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_na_rows'"),
            sliderInput(paste0("step_na_row_threshold_", step_id), "Max Missing Values per Row",
                        min = 0, max = 15, value = if(is.numeric(additional_info)) min(max(additional_info, 0), 15) else 3, step = 1)
          ),
          
          # Remove NA cols threshold
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_na_cols'"),
            sliderInput(paste0("step_na_col_threshold_", step_id), "Max Missing % per Column",
                        min = 0, max = 100, value = if(is.numeric(additional_info)) min(max(additional_info, 0), 100) else 50, step = 5, post = "%")
          ),
          
          # Manual imputation value
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'impute_manual'"),
            numericInput(paste0("step_manual_val_", step_id), "Imputation Value", 
                         value = if(is.numeric(additional_info)) additional_info else 0)
          ),
          
          # KNN imputation parameters
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'impute_knn'"),
            sliderInput(paste0("step_knn_k_", step_id), "Number of Neighbors (k):",
                        min = 1, max = 20, value = if(is.numeric(additional_info)) min(max(additional_info, 1), 20) else 5, step = 1)
          ),
          
          # MICE imputation parameters
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'impute_mice'"),
            sliderInput(paste0("step_mice_iter_", step_id), "Number of Iterations:",
                        min = 1, max = 20, value = if(is.numeric(additional_info)) min(max(additional_info, 1), 20) else 5, step = 1)
          ),
          
          # Random Forest imputation parameters
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'impute_rf'"),
            sliderInput(paste0("step_rf_ntree_", step_id), "Number of Trees:",
                        min = 50, max = 500, value = if(is.numeric(additional_info)) min(max(additional_info, 50), 500) else 100, step = 50)
          ),
          
          # Remove rows by index
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_rows_by_index'"),
            textAreaInput(paste0("step_row_indices_", step_id), "Row indices to remove (comma-separated, e.g., 1,5,10,25 or 1:10):",
                          value = "", rows = 2, placeholder = "1, 5, 10, 25, 50")
          ),
          
          # Keep rows by index
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'keep_rows_by_index'"),
            textAreaInput(paste0("step_row_keep_indices_", step_id), "Row indices to keep (comma-separated, e.g., 1,5,10,25 or 1:10):",
                          value = "", rows = 2, placeholder = "1, 5, 10, 25, 50")
          ),
          
          # Remove columns by name
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_cols_by_name'"),
            pickerInput(paste0("step_cols_remove_", step_id), "Select columns to remove:",
                        choices = all_cols_available,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE))
          ),
          
          # Keep columns by name
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'keep_cols_by_name'"),
            pickerInput(paste0("step_cols_keep_", step_id), "Select columns to keep:",
                        choices = all_cols_available,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE))
          ),
          
          # IQR outlier removal
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_outliers_iqr'"),
            pickerInput(paste0("step_outlier_vars_", step_id), "Select variables for outlier detection:",
                        choices = numeric_cols_available,
                        selected = numeric_cols_available[1:min(3, length(numeric_cols_available))],
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)),
            sliderInput(paste0("step_outlier_iqr_mult_", step_id), "IQR Multiplier:",
                        min = 1, max = 5, value = 1.5, step = 0.1)
          ),
          
          # Mahalanobis outlier removal
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_outliers_mahalanobis'"),
            sliderInput(paste0("step_outlier_mahalanobis_prob_", step_id), "Chi-Square Probability Threshold:",
                        min = 0.9, max = 0.9999, value = 0.999, step = 0.001)
          ),
          
          # Cook's Distance outlier removal
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_outliers_cooks'"),
            selectInput(paste0("step_outlier_cooks_method_", step_id), "Threshold Method:",
                        choices = c("4 * mean" = "4mean", "4/n" = "4n", "Quantile 0.99" = "quantile"),
                        selected = "4mean")
          ),
          
          # LOF outlier removal
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_outliers_lof'"),
            sliderInput(paste0("step_outlier_lof_minpts_", step_id), "minPts (neighbors):",
                        min = 3, max = 20, value = 5, step = 1),
            sliderInput(paste0("step_outlier_lof_threshold_", step_id), "LOF Threshold:",
                        min = 1, max = 5, value = 2, step = 0.1)
          ),
          
          # Isolation Forest outlier removal
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_outliers_iforest'"),
            sliderInput(paste0("step_outlier_if_threshold_", step_id), "Isolation Forest Threshold:",
                        min = 0.5, max = 0.95, value = 0.6, step = 0.01)
          ),
          
          # One-hot encoding options
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'one_hot_encode'"),
            div(class = "alert alert-info", style = "padding: 8px; margin-top: 5px; font-size: 11px;",
                icon("info-circle"),
                HTML("<strong>One-Hot Encoding:</strong> Creates binary columns for each category.")),
            checkboxInput(paste0("step_onehot_drop_first_", step_id), 
                          "Drop first category (avoid multicollinearity)", 
                          value = TRUE),
            checkboxInput(paste0("step_onehot_handle_na_", step_id), 
                          "Create 'Missing' category for NA values", 
                          value = TRUE)
          )
      )
    }))
  })
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 6: STEP MANAGEMENT
  # ========================================================================
  # ========================================================================
  
  # Add step
  observeEvent(input$add_processing_step, {
    current <- pipeline_steps()
    id <- if(length(current) == 0) 1 else max(sapply(current, function(x) x$id)) + 1
    
    new_step <- list(
      id = id,
      method = "",
      cols = character(0),
      additional_info = NULL
    )
    pipeline_steps(append(current, list(new_step)))
    add_to_log(paste("Added pipeline step", id))
  })
  
  # Remove step
  observeEvent(input$remove_step, {
    step_to_remove <- as.numeric(input$remove_step)
    current <- pipeline_steps()
    if(step_to_remove <= length(current) && step_to_remove > 0) {
      current <- current[-step_to_remove]
      # Reassign IDs sequentially
      for(i in seq_along(current)) {
        current[[i]]$id <- i
      }
      pipeline_steps(current)
      add_to_log(paste("Removed pipeline step", step_to_remove))
    }
  })
  
  # Update step parameters - COMPREHENSIVE VERSION
  observe({
    steps <- pipeline_steps()
    if(length(steps) == 0) return()
    
    updated <- FALSE
    
    for(i in seq_along(steps)) {
      step_id <- steps[[i]]$id
      
      # Update method
      method_input <- input[[paste0("step_method_", step_id)]]
      if(!is.null(method_input) && method_input != "" && method_input != "---") {
        if(steps[[i]]$method != method_input) {
          steps[[i]]$method <- method_input
          # Clear columns when method changes (except for specific methods)
          if(!method_input %in% c("remove_na_rows", "remove_na_cols", "remove_outliers_mahalanobis", 
                                  "remove_outliers_cooks", "remove_outliers_lof", "remove_outliers_iforest")) {
            steps[[i]]$cols <- character(0)
          }
          updated <- TRUE
        }
      }
      
      # Update columns for methods that use them
      cols_input <- input[[paste0("step_cols_", step_id)]]
      if(!is.null(cols_input) && !identical(steps[[i]]$cols, cols_input)) {
        steps[[i]]$cols <- cols_input
        updated <- TRUE
      }
      
      # Update method-specific parameters
      if(steps[[i]]$method == "remove_na_rows") {
        val <- input[[paste0("step_na_row_threshold_", step_id)]]
        if(!is.null(val) && !identical(steps[[i]]$additional_info, val)) {
          steps[[i]]$additional_info <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "remove_na_cols") {
        val <- input[[paste0("step_na_col_threshold_", step_id)]]
        if(!is.null(val) && !identical(steps[[i]]$additional_info, val)) {
          steps[[i]]$additional_info <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "impute_manual") {
        val <- input[[paste0("step_manual_val_", step_id)]]
        if(!is.null(val) && !identical(steps[[i]]$additional_info, val)) {
          steps[[i]]$additional_info <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "impute_knn") {
        val <- input[[paste0("step_knn_k_", step_id)]]
        if(!is.null(val) && !identical(steps[[i]]$additional_info, val)) {
          steps[[i]]$additional_info <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "impute_mice") {
        val <- input[[paste0("step_mice_iter_", step_id)]]
        if(!is.null(val) && !identical(steps[[i]]$additional_info, val)) {
          steps[[i]]$additional_info <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "impute_rf") {
        val <- input[[paste0("step_rf_ntree_", step_id)]]
        if(!is.null(val) && !identical(steps[[i]]$additional_info, val)) {
          steps[[i]]$additional_info <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "remove_rows_by_index") {
        val <- input[[paste0("step_row_indices_", step_id)]]
        if(!is.null(val) && val != "") {
          if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
          steps[[i]]$additional_info$indices <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "keep_rows_by_index") {
        val <- input[[paste0("step_row_keep_indices_", step_id)]]
        if(!is.null(val) && val != "") {
          if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
          steps[[i]]$additional_info$keep_indices <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "remove_cols_by_name") {
        val <- input[[paste0("step_cols_remove_", step_id)]]
        if(!is.null(val)) {
          if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
          steps[[i]]$additional_info$remove_cols <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "keep_cols_by_name") {
        val <- input[[paste0("step_cols_keep_", step_id)]]
        if(!is.null(val)) {
          if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
          steps[[i]]$additional_info$keep_cols <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "remove_outliers_iqr") {
        vars <- input[[paste0("step_outlier_vars_", step_id)]]
        mult <- input[[paste0("step_outlier_iqr_mult_", step_id)]]
        if(!is.null(vars) || !is.null(mult)) {
          if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
          if(!is.null(vars)) steps[[i]]$additional_info$outlier_vars <- vars
          if(!is.null(mult)) steps[[i]]$additional_info$iqr_multiplier <- mult
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "remove_outliers_mahalanobis") {
        val <- input[[paste0("step_outlier_mahalanobis_prob_", step_id)]]
        if(!is.null(val)) {
          if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
          steps[[i]]$additional_info$mahalanobis_prob <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "remove_outliers_cooks") {
        val <- input[[paste0("step_outlier_cooks_method_", step_id)]]
        if(!is.null(val)) {
          if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
          steps[[i]]$additional_info$cooks_method <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "remove_outliers_lof") {
        minPts <- input[[paste0("step_outlier_lof_minpts_", step_id)]]
        threshold <- input[[paste0("step_outlier_lof_threshold_", step_id)]]
        if(!is.null(minPts) || !is.null(threshold)) {
          if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
          if(!is.null(minPts)) steps[[i]]$additional_info$lof_minpts <- minPts
          if(!is.null(threshold)) steps[[i]]$additional_info$lof_threshold <- threshold
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "remove_outliers_iforest") {
        val <- input[[paste0("step_outlier_if_threshold_", step_id)]]
        if(!is.null(val)) {
          if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
          steps[[i]]$additional_info$if_threshold <- val
          updated <- TRUE
        }
      } else if(steps[[i]]$method == "one_hot_encode") {
        drop_first <- input[[paste0("step_onehot_drop_first_", step_id)]]
        handle_na <- input[[paste0("step_onehot_handle_na_", step_id)]]
        if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
        if(!identical(steps[[i]]$additional_info$drop_first, drop_first)) {
          steps[[i]]$additional_info$drop_first <- drop_first
          updated <- TRUE
        }
        if(!identical(steps[[i]]$additional_info$handle_na, handle_na)) {
          steps[[i]]$additional_info$handle_na <- handle_na
          updated <- TRUE
        }
      }
    }
    
    if(updated) pipeline_steps(steps)
  })
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 7: APPLY STEPS FUNCTION (with column validation & error handling)
  # ========================================================================
  # ========================================================================
  
  # Helper function to parse indices (comma-separated or range format)
  parse_indices <- function(indices_text, max_row) {
    if(is.null(indices_text) || indices_text == "") return(c())
    indices_parts <- unlist(strsplit(indices_text, ","))
    indices <- c()
    for(part in indices_parts) {
      part <- trimws(part)
      if(grepl(":", part)) {
        range_parts <- as.numeric(unlist(strsplit(part, ":")))
        if(length(range_parts) == 2 && !any(is.na(range_parts))) {
          indices <- c(indices, seq(range_parts[1], range_parts[2]))
        }
      } else {
        idx <- as.numeric(part)
        if(!is.na(idx)) indices <- c(indices, idx)
      }
    }
    indices <- unique(round(indices))
    indices <- indices[indices >= 1 & indices <= max_row]
    return(indices)
  }
  
  apply_steps_to_data <- function(data, steps) {
    # Safety check
    if(is.null(data) || nrow(data) == 0) {
      add_to_log("    ERROR: No data provided to apply_steps_to_data")
      return(data)
    }
    
    if(is.null(steps) || length(steps) == 0) {
      add_to_log("    No steps to apply")
      return(data)
    }
    
    for(step_idx in seq_along(steps)) {
      step <- steps[[step_idx]]
      method <- step$method
      if(is.null(method) || method == "") next
      
      add_to_log(paste("  Step", step_idx, ":", method_display_names[[method]] %||% method))
      
      # Get columns and validate they exist
      cols <- step$cols
      if(!is.null(cols) && length(cols) > 0 && !method %in% c("remove_na_rows", "remove_na_cols", 
                                                              "remove_outliers_mahalanobis", "remove_outliers_cooks",
                                                              "remove_outliers_lof", "remove_outliers_iforest")) {
        valid_cols <- cols[cols %in% names(data)]
        if(length(valid_cols) < length(cols)) {
          removed_cols <- setdiff(cols, valid_cols)
          add_to_log(paste("    Warning: Columns", paste(removed_cols, collapse=", "), 
                           "no longer exist in data - skipping for this step"))
        }
        cols <- valid_cols
        
        if(length(cols) == 0) {
          add_to_log(paste("    No valid columns for method", method, "- skipping step"))
          next
        }
      }
      
      tryCatch({
        # ===== ROW OPERATIONS =====
        if(method == "remove_na_rows") {
          threshold <- if(is.numeric(step$additional_info)) step$additional_info else 3
          row_na_count <- apply(data, 1, function(x) sum(is.na(x)))
          before <- nrow(data)
          data <- data[row_na_count <= threshold, , drop = FALSE]
          removed <- before - nrow(data)
          add_to_log(paste("    Removed", removed, "rows with >", threshold, "missing values"))
          
        } else if(method == "remove_na_cols") {
          threshold <- if(is.numeric(step$additional_info)) step$additional_info else 50
          before <- ncol(data)
          col_na_pct <- colMeans(is.na(data)) * 100
          cols_to_keep <- names(col_na_pct)[col_na_pct <= threshold]
          essential <- c("CODE", "OBS_TYPE", "DEATH_RATE")
          cols_to_keep <- unique(c(cols_to_keep, essential[essential %in% names(data)]))
          if(length(cols_to_keep) > 0) {
            data <- data[, cols_to_keep, drop = FALSE]
          }
          removed <- before - ncol(data)
          add_to_log(paste("    Removed", removed, "columns with >", threshold, "% missing values"))
          
        } else if(method == "remove_rows_by_index") {
          indices_text <- if(is.list(step$additional_info)) step$additional_info$indices %||% "" else ""
          if(indices_text != "") {
            indices <- parse_indices(indices_text, nrow(data))
            if(length(indices) > 0) {
              before <- nrow(data)
              data <- data[-indices, , drop = FALSE]
              add_to_log(paste("    Removed", before - nrow(data), "rows by index"))
            }
          }
          
        } else if(method == "keep_rows_by_index") {
          indices_text <- if(is.list(step$additional_info)) step$additional_info$keep_indices %||% "" else ""
          if(indices_text != "") {
            indices <- parse_indices(indices_text, nrow(data))
            if(length(indices) > 0) {
              before <- nrow(data)
              data <- data[indices, , drop = FALSE]
              add_to_log(paste("    Kept", nrow(data), "rows, removed", before - nrow(data), "rows"))
            }
          }
          
          # ===== IMPUTATION METHODS =====
        } else if(method == "impute_median" && length(cols) > 0) {
          for(col in cols) {
            if(col %in% names(data) && is.numeric(data[[col]])) {
              na_count <- sum(is.na(data[[col]]))
              if(na_count > 0) {
                data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
                add_to_log(paste("    Imputed", na_count, "values in", col, "with median"))
              }
            }
          }
          
        } else if(method == "impute_mean" && length(cols) > 0) {
          for(col in cols) {
            if(col %in% names(data) && is.numeric(data[[col]])) {
              na_count <- sum(is.na(data[[col]]))
              if(na_count > 0) {
                data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
                add_to_log(paste("    Imputed", na_count, "values in", col, "with mean"))
              }
            }
          }
          
        } else if(method == "impute_manual" && length(cols) > 0) {
          impute_val <- if(is.numeric(step$additional_info)) step$additional_info else 0
          for(col in cols) {
            if(col %in% names(data) && is.numeric(data[[col]])) {
              na_count <- sum(is.na(data[[col]]))
              if(na_count > 0) {
                data[[col]][is.na(data[[col]])] <- impute_val
                add_to_log(paste("    Imputed", na_count, "values in", col, "with", impute_val))
              }
            }
          }
          
        } else if(method == "impute_knn" && length(cols) > 0) {
          if(requireNamespace("VIM", quietly = TRUE)) {
            valid_cols <- cols[cols %in% names(data) & sapply(data[cols], is.numeric)]
            if(length(valid_cols) > 0) {
              k_val <- if(is.list(step$additional_info)) step$additional_info %||% 5 else step$additional_info %||% 5
              imputed_data <- VIM::kNN(data[, valid_cols, drop = FALSE], k = k_val, imp_var = FALSE)
              data[, valid_cols] <- imputed_data
              add_to_log(paste("    Performed KNN imputation on", length(valid_cols), "variables with k =", k_val))
            }
          } else {
            add_to_log("    KNN imputation requires VIM package - skipping")
          }
          
        } else if(method == "impute_mice" && length(cols) > 0) {
          if(requireNamespace("mice", quietly = TRUE)) {
            valid_cols <- cols[cols %in% names(data)]
            if(length(valid_cols) > 0) {
              iter_val <- if(is.list(step$additional_info)) step$additional_info %||% 5 else step$additional_info %||% 5
              mice_result <- mice::mice(data[, valid_cols, drop = FALSE], m = 1, maxit = iter_val, printFlag = FALSE)
              data[, valid_cols] <- mice::complete(mice_result)
              add_to_log(paste("    Performed MICE imputation on", length(valid_cols), "variables with", iter_val, "iterations"))
            }
          } else {
            add_to_log("    MICE imputation requires mice package - skipping")
          }
          
        } else if(method == "impute_rf" && length(cols) > 0) {
          if(requireNamespace("missForest", quietly = TRUE)) {
            valid_cols <- cols[cols %in% names(data)]
            if(length(valid_cols) > 0) {
              ntree_val <- if(is.list(step$additional_info)) step$additional_info %||% 100 else step$additional_info %||% 100
              rf_result <- missForest::missForest(data[, valid_cols, drop = FALSE], ntree = ntree_val, verbose = FALSE)
              data[, valid_cols] <- rf_result$ximp
              add_to_log(paste("    Performed Random Forest imputation on", length(valid_cols), "variables with", ntree_val, "trees"))
            }
          } else {
            add_to_log("    Random Forest imputation requires missForest package - skipping")
          }
          
          # ===== OUTLIER REMOVAL =====
        } else if(method == "remove_outliers_iqr") {
          outlier_vars <- if(is.list(step$additional_info)) step$additional_info$outlier_vars %||% character(0) else character(0)
          iqr_mult <- if(is.list(step$additional_info)) step$additional_info$iqr_multiplier %||% 1.5 else 1.5
          if(length(outlier_vars) > 0) {
            outlier_vars <- outlier_vars[outlier_vars %in% names(data)]
            if(length(outlier_vars) > 0) {
              result <- remove_outliers_iqr(data, outlier_vars, iqr_mult)
              data <- result$data
              add_to_log(paste("    Removed", result$removed, "outliers using IQR method"))
            }
          }
          
        } else if(method == "remove_outliers_mahalanobis") {
          prob <- if(is.list(step$additional_info)) step$additional_info$mahalanobis_prob %||% 0.999 else 0.999
          result <- remove_outliers_mahalanobis(data, prob)
          data <- result$data
          if(result$removed > 0) {
            add_to_log(paste("    Removed", result$removed, "outliers using Mahalanobis distance"))
          }
          
        } else if(method == "remove_outliers_cooks") {
          cooks_method <- if(is.list(step$additional_info)) step$additional_info$cooks_method %||% "4mean" else "4mean"
          result <- remove_outliers_cooks(data, cooks_method)
          data <- result$data
          if(result$removed > 0) {
            add_to_log(paste("    Removed", result$removed, "outliers using Cook's distance"))
          }
          
        } else if(method == "remove_outliers_lof") {
          minPts <- if(is.list(step$additional_info)) step$additional_info$lof_minpts %||% 5 else 5
          lof_threshold <- if(is.list(step$additional_info)) step$additional_info$lof_threshold %||% 2 else 2
          result <- remove_outliers_lof(data, minPts, lof_threshold)
          data <- result$data
          if(result$removed > 0) {
            add_to_log(paste("    Removed", result$removed, "outliers using Local Outlier Factor"))
          }
          
        } else if(method == "remove_outliers_iforest") {
          if_threshold <- if(is.list(step$additional_info)) step$additional_info$if_threshold %||% 0.6 else 0.6
          result <- remove_outliers_iforest(data, if_threshold)
          data <- result$data
          if(result$removed > 0) {
            add_to_log(paste("    Removed", result$removed, "outliers using Isolation Forest"))
          }
          
          # ===== COLUMN OPERATIONS =====
        } else if(method == "remove_cols_by_name") {
          cols_to_remove <- if(is.list(step$additional_info)) step$additional_info$remove_cols %||% character(0) else character(0)
          if(length(cols_to_remove) > 0) {
            protected_cols <- c("CODE", "OBS_TYPE", "DEATH_RATE")
            cols_to_remove <- cols_to_remove[!cols_to_remove %in% protected_cols]
            cols_to_remove <- cols_to_remove[cols_to_remove %in% names(data)]
            if(length(cols_to_remove) > 0) {
              before <- ncol(data)
              data <- data[, !names(data) %in% cols_to_remove, drop = FALSE]
              add_to_log(paste("    Removed", before - ncol(data), "columns by name"))
            }
          }
          
        } else if(method == "keep_cols_by_name") {
          cols_to_keep <- if(is.list(step$additional_info)) step$additional_info$keep_cols %||% character(0) else character(0)
          if(length(cols_to_keep) > 0) {
            always_keep <- c("CODE", "OBS_TYPE", "DEATH_RATE")
            cols_to_keep <- unique(c(cols_to_keep, always_keep[always_keep %in% names(data)]))
            cols_to_keep <- cols_to_keep[cols_to_keep %in% names(data)]
            if(length(cols_to_keep) > 0) {
              before <- ncol(data)
              data <- data[, names(data) %in% cols_to_keep, drop = FALSE]
              add_to_log(paste("    Kept", ncol(data), "columns, removed", before - ncol(data), "columns"))
            }
          }
          
          # ===== TRANSFORMATIONS =====
        } else if(method == "log_transform" && length(cols) > 0) {
          for(col in cols) {
            if(col %in% names(data) && is.numeric(data[[col]])) {
              min_val <- min(data[[col]], na.rm = TRUE)
              if(min_val <= 0) {
                data[[col]] <- log(data[[col]] + abs(min_val) + 1)
              } else {
                data[[col]] <- log(data[[col]])
              }
              add_to_log(paste("    Log transformed", col))
            }
          }
          
        } else if(method == "sqrt_transform" && length(cols) > 0) {
          for(col in cols) {
            if(col %in% names(data) && is.numeric(data[[col]])) {
              min_val <- min(data[[col]], na.rm = TRUE)
              if(min_val < 0) {
                data[[col]] <- sqrt(data[[col]] + abs(min_val) + 1)
              } else {
                data[[col]] <- sqrt(data[[col]])
              }
              add_to_log(paste("    Square root transformed", col))
            }
          }
          
        } else if(method == "scale" && length(cols) > 0) {
          for(col in cols) {
            if(col %in% names(data) && is.numeric(data[[col]])) {
              data[[col]] <- scale(data[[col]])
              add_to_log(paste("    Scaled", col))
            }
          }
          
        } else if(method == "center" && length(cols) > 0) {
          for(col in cols) {
            if(col %in% names(data) && is.numeric(data[[col]])) {
              data[[col]] <- data[[col]] - mean(data[[col]], na.rm = TRUE)
              add_to_log(paste("    Centered", col))
            }
          }
          
          # ===== ENCODING =====
        } else if(method == "one_hot_encode" && length(cols) > 0) {
          drop_first <- if(is.list(step$additional_info)) step$additional_info$drop_first %||% TRUE else TRUE
          handle_na <- if(is.list(step$additional_info)) step$additional_info$handle_na %||% TRUE else TRUE
          
          for(col in cols) {
            if(col %in% names(data) && !is.numeric(data[[col]])) {
              if(handle_na && any(is.na(data[[col]]))) {
                data[[col]][is.na(data[[col]])] <- "MISSING"
              }
              if(!is.factor(data[[col]])) {
                data[[col]] <- as.factor(data[[col]])
              }
              
              if(drop_first) {
                one_hot <- model.matrix(~ . - 1, data = data[, col, drop = FALSE])
              } else {
                one_hot <- model.matrix(~ ., data = data[, col, drop = FALSE])
                if("(Intercept)" %in% colnames(one_hot)) {
                  one_hot <- one_hot[, !colnames(one_hot) %in% "(Intercept)", drop = FALSE]
                }
              }
              
              colnames(one_hot) <- gsub(paste0("^", col), paste0(col, "_"), colnames(one_hot))
              colnames(one_hot) <- gsub(" ", "_", colnames(one_hot))
              colnames(one_hot) <- gsub("[^a-zA-Z0-9_]", "", colnames(one_hot))
              
              one_hot_df <- as.data.frame(one_hot)
              data[[col]] <- NULL
              data <- cbind(data, one_hot_df)
              add_to_log(paste("    One-hot encoded", col, "into", ncol(one_hot_df), "binary columns"))
            }
          }
        }
        
      }, error = function(e) {
        add_to_log(paste("    ERROR in step", step_idx, "(", method, "):", e$message))
      })
    }
    
    return(data)
  }
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 8: RECIPE ACTIONS
  # ========================================================================
  # ========================================================================
  
  # Apply Recipe button (current pipeline steps)
  observeEvent(input$process_data_btn, {
    steps <- pipeline_steps()
    if(length(steps) == 0) {
      showNotification("No steps in pipeline. Add steps first.", type = "warning")
      return()
    }
    
    add_to_log(paste("=== Starting pipeline processing with", length(steps), "steps ==="))
    
    # Use current working data
    data <- isolate(processed_data_working())
    
    # Validate data
    if(is.null(data) || nrow(data) == 0) {
      showNotification("No data available. Please check your data source.", type = "error")
      return()
    }
    
    original_rows <- nrow(data)
    original_cols <- ncol(data)
    
    # Apply steps with error handling
    data <- tryCatch({
      apply_steps_to_data(data, steps)
    }, error = function(e) {
      add_to_log(paste("ERROR applying recipe:", e$message))
      showNotification(paste("Error applying recipe:", e$message), type = "error", duration = 8)
      return(NULL)
    })
    
    if(is.null(data)) {
      return()
    }
    
    processed_data_working(data)
    add_to_log(paste("=== Pipeline complete: from", original_rows, "rows to", nrow(data), 
                     "rows, from", original_cols, "cols to", ncol(data), "cols ==="))
    showNotification(paste("✓ Pipeline complete!", nrow(data), "rows,", ncol(data), "columns"), 
                     type = "message", duration = 5)
  })
  
  # Reset pipeline (clear steps only, keep data)
  observeEvent(input$reset_pipeline_btn, {
    pipeline_steps(list())
    add_to_log("Reset pipeline - all steps cleared")
    showNotification("Pipeline steps cleared", type = "message")
  })
  
  # Clear all steps with confirmation
  observeEvent(input$clear_pipeline_btn, {
    showModal(modalDialog(
      title = "Clear All Steps", 
      "Are you sure you want to clear all steps from the current recipe?",
      footer = tagList(
        modalButton("Cancel"), 
        actionButton("confirm_clear_steps", "Clear All", class = "btn-danger")
      ), 
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_clear_steps, {
    pipeline_steps(list())
    updateTextInput(session, "pipeline_name", value = "")
    updateTextAreaInput(session, "pipeline_comments", value = "")
    add_to_log("Cleared all pipeline steps")
    showNotification("All steps cleared", type = "message")
    removeModal()
  })
  
  # Save recipe
  observeEvent(input$save_pipeline_btn, {
    pipeline_name <- trimws(input$pipeline_name)
    if(pipeline_name == "") {
      showNotification("Please enter a recipe name", type = "error")
      return()
    }
    
    steps <- pipeline_steps()
    if(!is.list(steps) || length(steps) == 0) {
      showNotification("No steps in recipe. Add steps first.", type = "error")
      return()
    }
    
    current <- saved_pipelines()
    original_name <- pipeline_name
    counter <- 1
    
    # Handle duplicate names
    while(pipeline_name %in% names(current)) {
      pipeline_name <- paste0(original_name, "_v", counter)
      counter <- counter + 1
    }
    
    if(pipeline_name != original_name) {
      showNotification(paste("Name already exists. Saved as:", pipeline_name), type = "warning")
    }
    
    # Save the recipe
    current[[pipeline_name]] <- list(
      steps = steps, 
      created = Sys.time(), 
      comments = trimws(input$pipeline_comments %||% "")
    )
    saved_pipelines(current)
    save_all_data()
    
    # Clear inputs
    updateTextInput(session, "pipeline_name", value = "")
    updateTextAreaInput(session, "pipeline_comments", value = "")
    
    showNotification(paste("Recipe saved as:", pipeline_name, "-", length(steps), "steps"), type = "message")
    add_to_log(paste("Saved recipe:", pipeline_name))
  })
  
  # Save As New Recipe
  observeEvent(input$save_as_pipeline_btn, {
    steps <- pipeline_steps()
    if(!is.list(steps) || length(steps) == 0) {
      showNotification("No steps in recipe. Add steps first.", type = "error")
      return()
    }
    
    base_name <- trimws(input$pipeline_name)
    if(base_name == "") {
      base_name <- paste0("Recipe_", format(Sys.time(), "%Y%m%d_%H%M"))
    }
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    new_name <- paste0(base_name, "_", timestamp)
    new_name <- gsub(" ", "_", new_name)
    new_name <- gsub("[^a-zA-Z0-9_]", "", new_name)
    
    current <- saved_pipelines()
    current[[new_name]] <- list(
      steps = steps, 
      created = Sys.time(), 
      comments = trimws(input$pipeline_comments %||% paste("Saved on", format(Sys.time(), "%Y-%m-%d %H:%M")))
    )
    saved_pipelines(current)
    save_all_data()
    
    # Clear inputs
    updateTextInput(session, "pipeline_name", value = "")
    updateTextAreaInput(session, "pipeline_comments", value = "")
    
    showNotification(paste("✓ Recipe saved as NEW:", new_name, "-", length(steps), "steps"), type = "message")
    add_to_log(paste("Saved new recipe:", new_name))
  })
  
  # Load recipe (load steps into pipeline without applying)
  observeEvent(input$load_pipeline_trigger, {
    req(input$load_pipeline_trigger)
    pipeline_name <- input$load_pipeline_trigger
    
    if(!pipeline_name %in% names(saved_pipelines())) {
      showNotification(paste("Recipe", pipeline_name, "not found"), type = "error")
      return()
    }
    
    saved_pipeline <- saved_pipelines()[[pipeline_name]]
    
    if(is.null(saved_pipeline$steps) || !is.list(saved_pipeline$steps)) {
      showNotification(paste("Recipe", pipeline_name, "has invalid step data"), type = "error")
      return()
    }
    
    loaded_steps <- saved_pipeline$steps
    
    if(length(loaded_steps) == 0) {
      showNotification(paste("Recipe", pipeline_name, "has no steps"), type = "warning")
      pipeline_steps(list())
      return()
    }
    
    # Reconstruct steps with proper IDs
    reconstructed_steps <- list()
    for(i in seq_along(loaded_steps)) {
      step <- loaded_steps[[i]]
      if(is.list(step)) {
        reconstructed_step <- list(
          id = i, 
          method = step$method %||% "", 
          cols = step$cols %||% character(0), 
          additional_info = step$additional_info %||% NULL
        )
        if(reconstructed_step$method != "") {
          reconstructed_steps[[i]] <- reconstructed_step
        }
      }
    }
    
    reconstructed_steps <- reconstructed_steps[!sapply(reconstructed_steps, is.null)]
    
    if(length(reconstructed_steps) == 0) {
      showNotification("No valid steps found in recipe", type = "error")
      return()
    }
    
    pipeline_steps(reconstructed_steps)
    updateTextInput(session, "pipeline_name", value = paste0(pipeline_name, "_modified"))
    
    comments_msg <- if(!is.null(saved_pipeline$comments) && saved_pipeline$comments != "") {
      paste0(" - Notes: ", saved_pipeline$comments)
    } else { "" }
    
    showNotification(paste("✓ Loaded recipe:", pipeline_name, "-", length(reconstructed_steps), "steps", comments_msg), 
                     type = "message", duration = 5)
    add_to_log(paste("Loaded saved recipe:", pipeline_name))
  })
  
  # Delete recipe
  observeEvent(input$delete_pipeline_trigger, {
    pipeline_name <- input$delete_pipeline_trigger
    
    if(!is.null(pipeline_name) && pipeline_name != "" && pipeline_name %in% names(saved_pipelines())) {
      showModal(modalDialog(
        title = "Delete Recipe", 
        paste("Are you sure you want to delete the recipe:", pipeline_name, "?"),
        footer = tagList(
          modalButton("Cancel"), 
          actionButton("confirm_delete_pipeline", "Delete", class = "btn-danger")
        ), 
        easyClose = TRUE
      ))
      
      pipeline_to_delete <- pipeline_name
      
      observeEvent(input$confirm_delete_pipeline, {
        current <- saved_pipelines()
        current[[pipeline_to_delete]] <- NULL
        saved_pipelines(current)
        save_all_data()
        showNotification(paste("Recipe", pipeline_to_delete, "deleted"), type = "message")
        add_to_log(paste("Deleted recipe:", pipeline_to_delete))
        removeModal()
      }, once = TRUE)
    }
  })
  
  # Apply Loaded Recipe button - APPLIES the loaded steps to ORIGINAL data
  observeEvent(input$apply_loaded_recipe_btn, {
    steps <- pipeline_steps()
    
    if(length(steps) == 0) {
      showNotification("No recipe loaded. Load a recipe first.", type = "warning")
      return()
    }
    
    # IMPORTANT: Reset to original data before applying
    data <- standardize_missing(df)
    
    if(is.null(data) || nrow(data) == 0) {
      showNotification("No data available. Please check your data source.", type = "error")
      return()
    }
    
    original_rows <- nrow(data)
    original_cols <- ncol(data)
    
    add_to_log(paste("=== Applying loaded recipe with", length(steps), "steps to ORIGINAL data ==="))
    
    data <- tryCatch({
      apply_steps_to_data(data, steps)
    }, error = function(e) {
      add_to_log(paste("ERROR applying recipe:", e$message))
      showNotification(paste("Error applying recipe:", e$message), type = "error", duration = 8)
      return(NULL)
    })
    
    if(is.null(data)) {
      return()
    }
    
    processed_data_working(data)
    
    add_to_log(paste("=== Recipe complete: from", original_rows, "rows to", nrow(data), 
                     "rows, from", original_cols, "cols to", ncol(data), "cols ==="))
    
    showNotification(paste("✓ Recipe applied successfully!", nrow(data), "rows,", ncol(data), "columns"), 
                     type = "message", duration = 5)
    
    # Auto-save processed data
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    ds_name <- paste0("Processed_", timestamp)
    
    current_datasets <- saved_datasets()
    current_datasets[[ds_name]] <- list(
      data = data, 
      name = ds_name, 
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      rows = nrow(data), 
      cols = ncol(data), 
      comments = paste("Processed using recipe:", if(!is.null(input$pipeline_name) && input$pipeline_name != "") input$pipeline_name else "Recipe Builder")
    )
    saved_datasets(current_datasets)
    save_all_data()
    
    updatePickerInput(session, "model_dataset_select", 
                      choices = names(saved_datasets()), 
                      selected = ds_name)
    
    add_to_log(paste("Auto-saved processed data as:", ds_name))
  })
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 9: UI OUTPUTS FOR RECIPE BUILDER
  # ========================================================================
  # ========================================================================
  
  output$saved_pipelines_list <- renderUI({
    pipelines <- saved_pipelines()
    if(length(pipelines) == 0) {
      return(div(class = "alert alert-info", "No saved recipes yet. Build one and save it above."))
    }
    tagList(lapply(names(pipelines), function(name) {
      pipeline <- pipelines[[name]]
      steps_count <- length(pipeline$steps)
      comments_html <- if(!is.null(pipeline$comments) && pipeline$comments != "") {
        tags$div(style = "font-size: 10px; color: #666; margin-top: 4px;", icon("comment"), " ", pipeline$comments)
      } else { NULL }
      created_time <- if(!is.null(pipeline$created)) format(pipeline$created, "%Y-%m-%d %H:%M") else "Unknown date"
      div(class = "well", style = "padding: 8px; margin-bottom: 8px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              div(strong(name, style = "color: #2C3E50;"), br(),
                  tags$small(paste(steps_count, "steps |", created_time)), comments_html),
              div(actionButton(paste0("load_pipeline_", gsub(" ", "_", name)), label = "Load", class = "btn-primary btn-xs",
                               style = "margin-right: 5px;", onclick = paste0("Shiny.setInputValue('load_pipeline_trigger', '", name, "', {priority: 'event'})")),
                  actionButton(paste0("delete_pipeline_", gsub(" ", "_", name)), label = "Delete", class = "btn-danger btn-xs",
                               onclick = paste0("Shiny.setInputValue('delete_pipeline_trigger', '", name, "', {priority: 'event'})"))))
      )
    }))
  })
  
  output$saved_datasets_list <- renderUI({
    datasets <- saved_datasets()
    if(length(datasets) == 0) {
      return(div(class = "alert alert-info", "No saved datasets yet. Process data and save one above."))
    }
    tagList(lapply(names(datasets), function(name) {
      ds <- datasets[[name]]
      comments_html <- if(!is.null(ds$comments) && ds$comments != "") {
        tags$div(style = "font-size: 10px; color: #666; margin-top: 4px;", icon("comment"), " ", ds$comments)
      } else { NULL }
      div(class = "well", style = "padding: 8px; margin-bottom: 8px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              div(strong(name, style = "color: #2C3E50;"), br(),
                  tags$small(paste(ds$rows, "rows,", ds$cols, "cols |", ds$timestamp)), comments_html),
              actionButton(paste0("load_dataset_", gsub(" ", "_", name)), 
                           label = "Load for Modeling", 
                           class = "btn-primary btn-xs",
                           onclick = paste0("Shiny.setInputValue('load_dataset_trigger', '", name, "', {priority: 'event'})")))
      )
    }))
  })
  
  output$pipeline_summary_ui <- renderUI({
    steps <- pipeline_steps()
    if(!is.list(steps) || length(steps) == 0) {
      return(p(icon("info-circle"), " No steps added yet. Click 'Add Step' to begin.", style = "color: grey;"))
    }
    step_elements <- list()
    for(i in seq_along(steps)) {
      step <- steps[[i]]
      method_name <- method_display_names[[step$method]] %||% (step$method %||% "Not selected")
      context <- ""
      if(!is.null(step$cols) && length(step$cols) > 0) {
        context <- paste0(" (", length(step$cols), " vars)")
      }
      if(step$method == "remove_na_rows" && !is.null(step$additional_info)) {
        context <- paste0(context, " | threshold = ", step$additional_info, " missing")
      }
      if(step$method == "remove_na_cols" && !is.null(step$additional_info)) {
        context <- paste0(context, " | threshold = ", step$additional_info, "%")
      }
      if(is.null(step$method) || step$method == "") {
        method_name <- "⚠️ No method selected"
      }
      step_elements <- append(step_elements, list(
        div(style = "display: flex; align-items: center; margin-bottom: 10px; padding: 8px; border-bottom: 1px solid #eee; background-color: #f8f9fa; border-radius: 4px;",
            div(style = "width: 35px; font-weight: bold; color: #2C3E50;", paste0(i, ".")),
            div(style = "flex: 1;", span(style = "font-weight: 500;", method_name), span(style = "color: #666; font-size: 12px; margin-left: 5px;", context)))
      ))
    }
    tagList(step_elements)
  })
  
  output$proc_data_summary_recipe <- renderPrint({
    data <- processed_data_working()
    total_missing <- sum(is.na(data))
    total_cells <- nrow(data) * ncol(data)
    missing_pct <- round(100 * total_missing / total_cells, 2)
    cat("Current Data Summary\n====================\n\n")
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
  })
  
  output$proc_data_preview_recipe <- renderDT({
    data <- processed_data_working()
    datatable(head(data, 20), options = list(scrollX = TRUE, pageLength = 10, dom = 't'), rownames = FALSE)
  })
  
  output$proc_log_recipe <- renderPrint({
    log_entries <- processing_log()
    if(length(log_entries) == 0) {
      cat("No processing actions yet.\nBuild a recipe and click 'Apply Recipe' to see results.")
    } else {
      recent_log <- tail(log_entries, 30)
      cat(paste(recent_log, collapse = "\n"))
    }
  })
  
  output$proc_download_data_recipe <- downloadHandler(
    filename = function() { paste("processed_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(processed_data_working(), file, row.names = FALSE) }
  )
  
  # Load dataset for modeling
  observeEvent(input$load_dataset_trigger, {
    ds_name <- input$load_dataset_trigger
    if(!is.null(ds_name) && ds_name != "" && ds_name %in% names(saved_datasets())) {
      saved_ds <- saved_datasets()[[ds_name]]
      if(is.list(saved_ds) && !is.null(saved_ds$data)) {
        processed_data_working(saved_ds$data)
        selected_model_dataset(ds_name)
        updateRadioButtons(session, "model_data_source", selected = "saved")
        updatePickerInput(session, "model_dataset_select", choices = names(saved_datasets()), selected = ds_name)
        comments_msg <- if(!is.null(saved_ds$comments) && saved_ds$comments != "") paste0(" - Notes: ", saved_ds$comments) else ""
        showNotification(paste("Loaded dataset:", ds_name, "(", saved_ds$rows, "rows,", saved_ds$cols, "cols)", comments_msg), type = "message")
        add_to_log(paste("Loaded saved dataset:", ds_name, comments_msg))
      } else {
        showNotification(paste("Dataset", ds_name, "has invalid structure"), type = "error")
      }
    }
  })
  
  
  # ============================================================================
  # ============================================================================
  # SECTION 10: TAB 1 - SUMMARY & EDA OUTPUTS
  # ============================================================================
  # ============================================================================
  
  # ----------------------------------------------------------------------------
  # Summary Statistics Outputs
  # ----------------------------------------------------------------------------
  
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
  
  # ----------------------------------------------------------------------------
  # Summary Tables
  # ----------------------------------------------------------------------------
  
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
  
  
  # ============================================================================
  # Boxplot Analysis
  # ============================================================================
  
  # UI for boxplot categorical filters
  output$boxplot_cat_filters_ui <- renderUI({
    req(input$boxplot_filter_vars)
    
    filter_ui_list <- list()
    
    for(var in input$boxplot_filter_vars) {
      if(var %in% names(df)) {
        vals <- as.character(df[[var]])
        unique_vals <- sort(unique(vals))
        
        if(any(is.na(vals))) {
          unique_vals <- c(unique_vals[!is.na(unique_vals)], "NA")
        }
        
        filter_ui_list <- append(filter_ui_list, list(
          div(style = "margin-top: 10px; margin-bottom: 10px;",
              h5(paste(var, ":"), style = "margin-bottom: 5px; font-size: 13px;"),
              checkboxGroupInput(
                inputId = paste0("boxplot_filter_", var),
                label = NULL,
                choices = unique_vals,
                selected = unique_vals,
                inline = FALSE
              )
          )
        ))
      }
    }
    
    if(length(filter_ui_list) == 0) {
      return(p("No categorical variables selected", style = "color: #666; font-size: 12px;"))
    }
    
    tagList(filter_ui_list)
  })
  
  filtered_boxplot_data <- reactive({
    data <- df
    
    if(!is.null(input$boxplot_filter_vars) && length(input$boxplot_filter_vars) > 0) {
      for(var in input$boxplot_filter_vars) {
        selected_vals <- input[[paste0("boxplot_filter_", var)]]
        
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
          }
        }
      }
    }
    
    return(data)
  })
  
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
  
  output$boxplot_plot <- renderPlotly({
    req(input$boxplot_numeric_vars, length(input$boxplot_numeric_vars) > 0)
    
    df_plot <- boxplot_data()
    req(nrow(df_plot) > 0)
    
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
    
    has_grouping <- !is.null(input$boxplot_cat_vars_group) && 
      input$boxplot_cat_vars_group != "None" &&
      length(unique(outlier_data$group)) > 1
    
    if(has_grouping) {
      n_groups <- length(unique(outlier_data$group))
      
      p <- ggplot2::ggplot() +
        ggplot2::geom_boxplot(data = non_outliers, 
                              ggplot2::aes(x = variable, y = value, fill = group),
                              outlier.shape = NA, na.rm = TRUE,
                              position = ggplot2::position_dodge(width = 0.8)) +
        ggplot2::geom_point(data = outliers, 
                            ggplot2::aes(x = variable, y = value, color = group,
                                         text = paste("Variable:", outliers$variable, 
                                                      "<br>Group:", outliers$group,
                                                      "<br>Code:", outliers$code,
                                                      "<br>Value:", round(outliers$value, 2),
                                                      "<br>Type: OUTLIER")),
                            position = ggplot2::position_dodge(width = 0.8),
                            size = 2.5, alpha = 0.8, na.rm = TRUE) +
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
      
      height_val <- max(600, 400 + 100 * n_groups)
      ggplotly(p, tooltip = "text", height = height_val) %>% 
        plotly::layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
      
    } else {
      p <- ggplot2::ggplot() +
        ggplot2::geom_boxplot(data = non_outliers, 
                              ggplot2::aes(x = variable, y = value),
                              fill = "#ADD8E6", color = "darkblue", 
                              outlier.shape = NA, na.rm = TRUE) +
        ggplot2::geom_point(data = outliers, 
                            ggplot2::aes(x = variable, y = value,
                                         text = paste("Variable:", variable, 
                                                      "<br>Code:", code, 
                                                      "<br>Value:", round(value, 2), 
                                                      "<br>Type: OUTLIER")),
                            color = "red", size = 2.5, alpha = 0.8, na.rm = TRUE) +
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
      for(var in input$boxplot_filter_vars) {
        selected_vals <- input[[paste0("boxplot_filter_", var)]]
        if(!is.null(selected_vals) && length(selected_vals) > 0) {
          cat("    -", var, ":", paste(selected_vals, collapse = ", "), "\n")
        }
      }
    }
    cat("  NULL values:", if(input$include_null_boxplot) "included" else "excluded", "\n")
  })
  
  observeEvent(input$reset_boxplot, {
    updateSliderInput(session, "iqr_boxplot", value = 1.5)
    updateCheckboxInput(session, "boxplot_center", value = TRUE)
    updateCheckboxInput(session, "boxplot_scale", value = TRUE)
    updatePickerInput(session, "boxplot_numeric_vars", selected = numeric_cols)
    updatePickerInput(session, "boxplot_cat_vars_group", selected = "None")
    updatePickerInput(session, "boxplot_filter_vars", selected = NULL)
    updateCheckboxInput(session, "include_null_boxplot", value = FALSE)
    showNotification("Boxplot settings reset to defaults", type = "default")
  })
  
  
  # ============================================================================
  # Correlation Analysis
  # ============================================================================
  
  # UI for correlation categorical filters
  output$corr_cat_filters_ui <- renderUI({
    req(input$corr_categorical_vars)
    
    filter_ui_list <- list()
    
    for(var in input$corr_categorical_vars) {
      if(var %in% names(df)) {
        vals <- as.character(df[[var]])
        unique_vals <- sort(unique(vals))
        
        if(any(is.na(vals))) {
          unique_vals <- c(unique_vals[!is.na(unique_vals)], "NA")
        }
        
        filter_ui_list <- append(filter_ui_list, list(
          div(style = "margin-top: 10px; margin-bottom: 10px;",
              h5(paste(var, ":"), style = "margin-bottom: 5px; font-size: 13px;"),
              checkboxGroupInput(
                inputId = paste0("corr_filter_", var),
                label = NULL,
                choices = unique_vals,
                selected = unique_vals,
                inline = FALSE
              ),
              checkboxInput(paste0("corr_include_null_", var), 
                            "Include NULL values", 
                            value = FALSE)
          )
        ))
      }
    }
    
    if(length(filter_ui_list) == 0) {
      return(p("No categorical variables selected", style = "color: #666; font-size: 12px;"))
    }
    
    tagList(filter_ui_list)
  })
  
  corr_filtered_data <- reactive({
    data <- df
    
    if(!is.null(input$corr_categorical_vars) && length(input$corr_categorical_vars) > 0) {
      for(var in input$corr_categorical_vars) {
        selected_vals <- input[[paste0("corr_filter_", var)]]
        include_null <- input[[paste0("corr_include_null_", var)]] %||% FALSE
        
        if(!is.null(selected_vals) && length(selected_vals) > 0) {
          if("NA" %in% selected_vals) {
            selected_vals_real <- setdiff(selected_vals, "NA")
            if(length(selected_vals_real) > 0) {
              if(include_null) {
                data <- data[data[[var]] %in% selected_vals_real | is.na(data[[var]]), ]
              } else {
                data <- data[data[[var]] %in% selected_vals_real, ]
              }
            } else {
              if(include_null) {
                data <- data[is.na(data[[var]]), ]
              } else {
                data <- data[!is.na(data[[var]]), ]
              }
            }
          } else {
            data <- data[data[[var]] %in% selected_vals, ]
            if(!include_null) {
              data <- data[!is.na(data[[var]]), ]
            }
          }
        }
      }
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
        treat_neg99 <- safe_in("neg99", input$corr_mv_types)
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
    cat("\nCategorical Filters Applied:\n----------------------------\n")
    if(!is.null(input$corr_categorical_vars) && length(input$corr_categorical_vars) > 0) {
      for(var in input$corr_categorical_vars) {
        selected_vals <- input[[paste0("corr_filter_", var)]]
        include_null <- input[[paste0("corr_include_null_", var)]] %||% FALSE
        if(!is.null(selected_vals) && length(selected_vals) > 0) {
          cat("  ", var, ":", paste(selected_vals, collapse = ", "))
          if(include_null) cat(" (NULLs included)\n") else cat("\n")
        }
      }
    } else {
      cat("  No categorical filters applied\n")
    }
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
    paste("Showing", nrow(data), "of", nrow(df), "observations |",
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
    showNotification("Correlation settings reset to defaults", type = "default")
  })
  
  
  # ============================================================================
  # Rising Values
  # ============================================================================
  
  # UI for rising values categorical filters (already exists as rv_categorical_filters)
  # This is the dynamic UI that appears when categorical variables are selected
  
  rv_filtered_data <- reactive({
    req(input$rv_numeric_vars, length(input$rv_numeric_vars) > 0)
    data <- df
    include_null <- if(is.null(input$rv_include_null)) FALSE else input$rv_include_null
    treat_neg99 <- safe_in("neg99", input$rv_mv_types)
    treat_dash <- !is.null(input$rv_mv_types) && "dash" %in% input$rv_mv_types
    treat_na_string <- !is.null(input$rv_mv_types) && "na_string" %in% input$rv_mv_types
    
    # Convert numeric columns
    for(col in names(data)) {
      if(col %in% numeric_cols && is.character(data[[col]])) {
        temp_vals <- data[[col]]
        if(treat_neg99) temp_vals[temp_vals == "-99"] <- NA
        if(treat_dash) temp_vals[temp_vals == "--"] <- NA
        if(treat_na_string) temp_vals[temp_vals == "NA"] <- NA
        data[[col]] <- suppressWarnings(as.numeric(temp_vals))
      }
    }
    
    # Apply categorical filters
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
  
  # Update checkboxes when categorical variables are selected
  observeEvent(input$rv_categorical_vars, {
    req(input$rv_categorical_vars)
    for(var in input$rv_categorical_vars) {
      vals <- df[[var]]
      choices <- as.character(unique(vals[!is.na(vals)]))
      if(any(is.na(vals))) choices <- c(choices, "NA")
      choices <- sort(choices)
      updateCheckboxGroupInput(session, paste0("rv_filter_", var), 
                               selected = choices)
    }
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  # UI for rising values categorical filters
  output$rv_categorical_filters <- renderUI({
    req(input$rv_categorical_vars)
    
    filter_ui_list <- list()
    
    for(var in input$rv_categorical_vars) {
      if(var %in% names(df)) {
        vals <- as.character(df[[var]])
        unique_vals <- sort(unique(vals))
        
        if(any(is.na(vals))) {
          unique_vals <- c(unique_vals[!is.na(unique_vals)], "NA")
        }
        
        filter_ui_list <- append(filter_ui_list, list(
          div(style = "margin-top: 10px; margin-bottom: 10px;",
              h5(paste(var, ":"), style = "margin-bottom: 5px; font-size: 13px;"),
              checkboxGroupInput(
                inputId = paste0("rv_filter_", var),
                label = NULL,
                choices = unique_vals,
                selected = unique_vals,
                inline = FALSE
              )
          )
        ))
      }
    }
    
    if(length(filter_ui_list) == 0) {
      return(p("No categorical variables selected", style = "color: #666; font-size: 12px;"))
    }
    
    tagList(filter_ui_list)
  })
  
  output$rising_plot <- renderPlotly({
    data <- rv_filtered_data()
    req(ncol(data) > 0)
    data <- data[, sapply(data, is.numeric), drop = FALSE]
    req(ncol(data) > 0)
    include_null <- if(is.null(input$rv_include_null)) FALSE else input$rv_include_null
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
    include_null <- if(is.null(input$rv_include_null)) FALSE else input$rv_include_null
    null_status <- if(include_null) "NULLs included" else "NULLs excluded"
    cat("Rising Values Summary\n=====================\n\n")
    cat("Variables selected:", ncol(data), "\n")
    cat("Variables with valid data:", sum(sapply(data, function(x) sum(!is.na(x)) > 0)), "\n")
    cat("NULL handling:", null_status, "\n\nVariable details:\n")
    for(var in names(data)) { cat("  ", var, ":", sum(!is.na(data[[var]])), "valid values\n") }
    cat("\nCategorical Filters Applied:\n----------------------------\n")
    if(!is.null(input$rv_categorical_vars) && length(input$rv_categorical_vars) > 0) {
      for(var in input$rv_categorical_vars) {
        selected_vals <- input[[paste0("rv_filter_", var)]]
        if(!is.null(selected_vals) && length(selected_vals) > 0) {
          cat("  ", var, ":", paste(selected_vals, collapse = ", "), "\n")
        }
      }
    } else {
      cat("  No categorical filters applied\n")
    }
  })
  
  observeEvent(input$reset_rising, {
    updatePickerInput(session, "rv_numeric_vars", selected = numeric_cols)
    updatePickerInput(session, "rv_categorical_vars", selected = NULL)
    updateMaterialSwitch(session, "rv_standardise", value = TRUE)
    updateMaterialSwitch(session, "rv_include_null", value = TRUE)
    updateCheckboxGroupInput(session, "rv_mv_types", selected = c("neg99", "dash", "na_string"))
    showNotification("Rising values settings reset to defaults", type = "default")
  })
  
  
  # ============================================================================
  # Missing Values Heatmap
  # ============================================================================
  
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
    
    col_missing_pct <- apply(plot_df, 2, function(x) { 
      vals <- as.character(x)
      sum(vals %in% c("-99", "--", "NA") | is.na(vals)) / length(vals) * 100 
    })
    
    plot_df <- plot_df[, col_missing_pct <= col_threshold, drop = FALSE]
    code_column <- code_column[col_missing_pct <= col_threshold]
    
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
      return(list(n_vars = 0, n_obs = 0))
    }
    
    var_order <- original_var_order[original_var_order %in% names(plot_df)]
    if(length(var_order) > 0) {
      plot_df <- plot_df[, var_order, drop = FALSE]
    }
    
    n_vars <- ncol(plot_df)
    
    show_neg99 <- safe_in("neg99", input$mv_types)
    show_dash <- !is.null(input$mv_types) && "dash" %in% input$mv_types
    show_na_string <- !is.null(input$mv_types) && "na_string" %in% input$mv_types
    
    color_matrix <- matrix("#ADD8E6", nrow = n_obs, ncol = n_vars)
    colnames(color_matrix) <- names(plot_df)
    has_neg99 <- FALSE
    has_dash <- FALSE
    has_na_string <- FALSE
    has_r_na <- FALSE
    
    for(i in 1:n_obs) {
      for(j in 1:n_vars) {
        val <- plot_df[i, j]
        if(is.na(val)) { 
          has_r_na <- TRUE
          color_matrix[i, j] <- "#808080" 
        } else if(val == "-99") { 
          has_neg99 <- TRUE
          color_matrix[i, j] <- "#FF69B4" 
        } else if(val == "--") { 
          has_dash <- TRUE
          color_matrix[i, j] <- "#32CD32" 
        } else if(val == "NA") { 
          has_na_string <- TRUE
          color_matrix[i, j] <- "#FF0000" 
        }
      }
    }
    
    if(!show_neg99) { 
      for(i in 1:n_obs) {
        for(j in 1:n_vars) {
          if(plot_df[i, j] == "-99") color_matrix[i, j] <- "#ADD8E6"
        }
      }
    }
    if(!show_dash) { 
      for(i in 1:n_obs) {
        for(j in 1:n_vars) {
          if(plot_df[i, j] == "--") color_matrix[i, j] <- "#ADD8E6"
        }
      }
    }
    if(!show_na_string) { 
      for(i in 1:n_obs) {
        for(j in 1:n_vars) {
          if(plot_df[i, j] == "NA") color_matrix[i, j] <- "#ADD8E6"
        }
      }
    }
    
    missing_pcts <- sapply(names(plot_df), function(col) { 
      vals <- as.character(plot_df[[col]])
      round(100 * sum(vals %in% c("-99", "--", "NA") | is.na(vals)) / n_obs, 1) 
    })
    
    if(!is.null(input$heatmap_order) && input$heatmap_order == "desc") {
      order_idx <- order(missing_pcts, decreasing = TRUE)
      color_matrix <- color_matrix[, order_idx, drop = FALSE]
      missing_pcts <- missing_pcts[order_idx]
      plot_df <- plot_df[, order_idx, drop = FALSE]
    }
    
    var_labels <- paste0(colnames(color_matrix), " (", missing_pcts, "%)")
    
    hover_text <- matrix("", nrow = n_obs, ncol = n_vars)
    for(i in 1:n_obs) {
      for(j in 1:n_vars) {
        val <- plot_df[i, j]
        var_name <- colnames(plot_df)[j]
        code_val <- code_column[i]
        if(is.na(val)) {
          hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: R NA")
        } else if(val == "-99") {
          hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: -99")
        } else if(val == "--") {
          hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: --")
        } else if(val == "NA") {
          hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: NA string")
        } else {
          hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: Present")
        }
      }
    }
    
    list(
      color_matrix = color_matrix,
      var_labels = var_labels,
      n_vars = ncol(color_matrix),
      n_obs = n_obs,
      plot_df = plot_df,
      code_column = code_column,
      has_neg99 = has_neg99,
      has_dash = has_dash,
      has_na_string = has_na_string,
      has_r_na = has_r_na,
      missing_pcts = missing_pcts,
      show_neg99 = show_neg99,
      show_dash = show_dash,
      show_na_string = show_na_string
    )
  })
  
  output$heatmap_plot <- renderPlotly({
    heatmap_info <- heatmap_data()
    
    if(heatmap_info$n_vars == 0 || heatmap_info$n_obs == 0) {
      return(plot_ly() %>% add_annotations(
        text = "No data available after applying filters.\nPlease adjust your variable selection or thresholds.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    color_matrix <- heatmap_info$color_matrix
    var_labels <- heatmap_info$var_labels
    n_vars <- heatmap_info$n_vars
    n_obs <- heatmap_info$n_obs
    plot_df <- heatmap_info$plot_df
    code_column <- heatmap_info$code_column
    has_neg99 <- heatmap_info$has_neg99
    has_dash <- heatmap_info$has_dash
    has_na_string <- heatmap_info$has_na_string
    has_r_na <- heatmap_info$has_r_na
    show_neg99 <- heatmap_info$show_neg99
    show_dash <- heatmap_info$show_dash
    show_na_string <- heatmap_info$show_na_string
    
    if(n_obs > 100) { 
      tickvals <- seq(1, n_obs, by = 20)
      ticktext <- seq(1, n_obs, by = 20)
    } else if(n_obs > 50) { 
      tickvals <- seq(1, n_obs, by = 10)
      ticktext <- seq(1, n_obs, by = 10)
    } else if(n_obs > 20) { 
      tickvals <- seq(1, n_obs, by = 5)
      ticktext <- seq(1, n_obs, by = 5)
    } else { 
      tickvals <- 1:n_obs
      ticktext <- 1:n_obs
    }
    
    legend_colors <- list(Present = "#ADD8E6", Neg99 = "#FF69B4", Dash = "#32CD32", NA_String = "#FF0000", R_NA = "#808080")
    legend_items <- 1
    if(has_neg99 && show_neg99) legend_items <- legend_items + 1
    if(has_dash && show_dash) legend_items <- legend_items + 1
    if(has_na_string && show_na_string) legend_items <- legend_items + 1
    if(has_r_na) legend_items <- legend_items + 1
    
    total_width <- 0.8
    start_x <- 0.15
    spacing <- total_width / legend_items
    
    key_annotations <- list(
      list(x = 0.5, y = 1.12, text = "Missingness Visualisation:", showarrow = FALSE,
           xref = "paper", yref = "paper", font = list(size = 16, color = "#2C3E50", weight = "bold"))
    )
    
    current_x <- start_x
    key_annotations <- append(key_annotations, list(
      list(x = current_x, y = 1.06, text = "Present", showarrow = FALSE,
           xref = "paper", yref = "paper", font = list(color = legend_colors$Present, size = 12, weight = "bold"))
    ))
    current_x <- current_x + spacing
    
    if(has_neg99 && show_neg99) {
      key_annotations <- append(key_annotations, list(
        list(x = current_x, y = 1.06, text = "-99", showarrow = FALSE,
             xref = "paper", yref = "paper", font = list(color = legend_colors$Neg99, size = 12, weight = "bold"))
      ))
      current_x <- current_x + spacing
    }
    if(has_dash && show_dash) {
      key_annotations <- append(key_annotations, list(
        list(x = current_x, y = 1.06, text = "--", showarrow = FALSE,
             xref = "paper", yref = "paper", font = list(color = legend_colors$Dash, size = 12, weight = "bold"))
      ))
      current_x <- current_x + spacing
    }
    if(has_na_string && show_na_string) {
      key_annotations <- append(key_annotations, list(
        list(x = current_x, y = 1.06, text = "NA string", showarrow = FALSE,
             xref = "paper", yref = "paper", font = list(color = legend_colors$NA_String, size = 12, weight = "bold"))
      ))
      current_x <- current_x + spacing
    }
    if(has_r_na) {
      key_annotations <- append(key_annotations, list(
        list(x = current_x, y = 1.06, text = "R NA", showarrow = FALSE,
             xref = "paper", yref = "paper", font = list(color = legend_colors$R_NA, size = 12, weight = "bold"))
      ))
    }
    
    hover_text <- matrix("", nrow = n_obs, ncol = n_vars)
    for(i in 1:n_obs) {
      for(j in 1:n_vars) {
        val <- plot_df[i, j]
        var_name <- colnames(plot_df)[j]
        code_val <- code_column[i]
        if(is.na(val)) {
          hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: R NA")
        } else if(val == "-99") {
          hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: -99")
        } else if(val == "--") {
          hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: --")
        } else if(val == "NA") {
          hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: NA string")
        } else {
          hover_text[i, j] <- paste0("Observation: ", i, "\nCode: ", code_val, "\nVariable: ", var_name, "\nStatus: Present")
        }
      }
    }
    
    unique_colors <- unique(as.vector(color_matrix))
    if(length(unique_colors) == 0) {
      unique_colors <- "#ADD8E6"
    }
    color_scale <- list()
    for(i in seq_along(unique_colors)) {
      color_scale[[i]] <- list((i-1)/(length(unique_colors)-1), unique_colors[i])
    }
    color_to_num <- setNames(seq_along(unique_colors), unique_colors)
    numeric_matrix <- matrix(color_to_num[color_matrix], nrow = n_obs, ncol = n_vars)
    
    p <- plot_ly(z = numeric_matrix, x = var_labels, y = 1:n_obs, type = "heatmap", 
                 colorscale = color_scale, text = hover_text, hoverinfo = "text", 
                 showscale = FALSE) %>%
      layout(
        annotations = key_annotations,
        xaxis = list(title = "Variables", tickangle = -45,
                     tickfont = list(size = ifelse(n_vars > 15, 8, ifelse(n_vars > 10, 9, 11))),
                     titlefont = list(size = 14), side = "bottom"),
        yaxis = list(title = "Observation Index", tickmode = "array",
                     tickvals = tickvals, ticktext = ticktext,
                     titlefont = list(size = 14), autorange = "reversed"),
        margin = list(b = ifelse(n_vars > 15, 150, ifelse(n_vars > 10, 130, 120)), 
                      l = 80, t = 120, r = 50),
        title = list(text = paste("Missing Values Heatmap -", n_obs, "Observations,", n_vars, "Variables"), 
                     font = list(size = 16), y = 0.98)
      )
    
    return(p)
  })
  
  output$heatmap_summary <- renderPrint({
    heatmap_info <- heatmap_data()
    req(!is.null(heatmap_info))
    if(heatmap_info$n_vars == 0 || heatmap_info$n_obs == 0) {
      cat("No data available after applying filters.\n")
      return()
    }
    
    plot_df <- heatmap_info$plot_df
    n_obs <- heatmap_info$n_obs
    n_vars <- heatmap_info$n_vars
    missing_pcts <- heatmap_info$missing_pcts
    
    cat("===============================================================================\n")
    cat("                    MISSING VALUES HEATMAP SUMMARY\n")
    cat("===============================================================================\n\n")
    cat("OVERALL STATISTICS\n-------------------------------------------------------------------------------\n")
    cat("  Total observations:", n_obs, "\n")
    cat("  Total variables:", n_vars, "\n")
    cat("  Total cells:", n_obs * n_vars, "\n\n")
    
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
    for(i in 1:nrow(top_cols)) {
      cat("  ", sprintf("%-20s", top_cols$Variable[i]), ": ", sprintf("%5.1f", top_cols$Missing_Pct[i]), "%\n")
    }
    
    cat("\nAPPLIED THRESHOLDS\n-------------------------------------------------------------------------------\n")
    cat("  Column missing percentage threshold:", input$col_missing_threshold, "%\n")
    cat("  Row missing count threshold:", input$row_missing_threshold, "\n")
  })
  
  observeEvent(input$reset_heatmap, {
    updateCheckboxGroupInput(session, "mv_types", selected = c("neg99", "dash", "na_string"))
    updateSliderInput(session, "col_missing_threshold", value = 100)
    updateSliderInput(session, "row_missing_threshold", value = 15)
    updatePickerInput(session, "heatmap_cat_vars", selected = c("GOVERN_TYPE", "HEALTHCARE_BASIS"))
    updatePickerInput(session, "heatmap_numeric_vars", selected = numeric_cols)
    updateRadioButtons(session, "heatmap_order", selected = "original")
    showNotification("Heatmap settings reset to defaults", type = "message")
  })
  
  
  # ============================================================================
  # Distribution Plots
  # ============================================================================
  
  output$distribution_plot <- renderPlotly({
    var <- input$dist_var
    req(var %in% names(df))
    vals <- df[[var]]; codes <- df$CODE; is_numeric_var <- var %in% numeric_cols
    if(is_numeric_var) {
      vals_char <- as.character(vals)
      treat_neg99 <- safe_in("neg99", input$dist_mv_types)
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
        geom_histogram(bins = input$dist_bins, fill = "#13D4D4", color = "white", alpha = 0.7,
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
    updateSelectInput(session, "dist_var", selected = "DEATH_RATE")
    updateCheckboxGroupInput(session, "dist_mv_types", selected = c("neg99", "dash", "na_string"))
    showNotification("Distribution settings reset to defaults", type = "default")
  })
  
  
  # ============================================================================
  # Scatter Plot
  # ============================================================================
  
  output$scatter_plot <- renderPlotly({
    req(input$scatter_x, input$scatter_y)
    x_var <- input$scatter_x; y_var <- input$scatter_y; color_var <- input$scatter_color
    x_vals <- df[[x_var]]; y_vals <- df[[y_var]]; codes <- df$CODE
    treat_neg99 <- safe_in("neg99", input$scatter_mv_types)
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
        geom_point(alpha = 0.6, size = 2, color = "#13D4D4")
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
  
  
  # ============================================================================
  # GGpairs Plot
  # ============================================================================
  
  ggpairs_selected_vars <- reactive({ input$ggpairs_numeric })
  
  ggpairs_filtered_data <- reactive({
    data <- df
    treat_neg99 <- safe_in("neg99", input$ggpairs_mv_types)
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
      if(!is.null(color_var) && "color" %in% names(mapping)) {
        data$tooltip_text <- paste0(data$tooltip_text, "<br>", color_var, ": ", data$color)
      }
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
  
  
  # ============================================================================
  # Tableplot
  # ============================================================================
  
  tabplot_filtered_data <- reactive({
    req(input$tabplot_numeric_vars, length(input$tabplot_numeric_vars) > 0)
    data <- df
    treat_neg99 <- safe_in("neg99", input$tabplot_mv_types)
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
    updatePickerInput(session, "tabplot_numeric_vars", selected = numeric_cols)
    updatePickerInput(session, "tabplot_categorical_vars", selected = categorical_cols[categorical_cols != "CODE"])
    updateCheckboxGroupInput(session, "tabplot_mv_types", selected = c("neg99", "dash", "na_string"))
    if(!is.null(input$tabplot_categorical_vars)) {
      for(var in input$tabplot_categorical_vars) {
        vals <- df[[var]]; char_vals <- as.character(vals); choices <- sort(unique(char_vals))
        updateCheckboxGroupInput(session, paste0("tabplot_filter_", var), selected = choices)
      }
    }
    showNotification("Tabplot settings reset to defaults", type = "default")
  })
  
  
  # ============================================================================
  # Mosaic Plot
  # ============================================================================
  
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
  
  
  # ============================================================================
  # Raw Data Table
  # ============================================================================
  
  observe({
    updateSliderInput(session, "dt_row_range", max = nrow(df), value = c(1, nrow(df)))
    updateSelectInput(session, "dt_page_length", selected = 25)
    updateTabsetPanel(session, "dt_column_tabs", selected = "All Columns")
    updatePickerInput(session, "dt_all_columns", 
                      choices = names(df),
                      selected = names(df))
    updatePickerInput(session, "dt_numeric_columns", 
                      choices = numeric_cols,
                      selected = numeric_cols)
    updatePickerInput(session, "dt_categorical_columns", 
                      choices = categorical_cols,
                      selected = categorical_cols)
  })
  
  selected_columns <- reactive({
    active_tab <- input$dt_column_tabs
    if(active_tab == "All Columns") {
      if(is.null(input$dt_all_columns) || length(input$dt_all_columns) == 0) {
        return(names(df))
      }
      return(input$dt_all_columns)
    } else if(active_tab == "Numeric Columns") {
      selected_num <- if(is.null(input$dt_numeric_columns) || length(input$dt_numeric_columns) == 0) {
        numeric_cols
      } else {
        input$dt_numeric_columns
      }
      always_include <- c("CODE", "OBS_TYPE")
      return(unique(c(always_include, selected_num)))
    } else if(active_tab == "Categorical Columns") {
      selected_cat <- if(is.null(input$dt_categorical_columns) || length(input$dt_categorical_columns) == 0) {
        categorical_cols
      } else {
        input$dt_categorical_columns
      }
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
  
  
  
  # ============================================================================
  # ============================================================================
  # SECTION 6: DATA PROCESSING STRATEGY TAB
  # ============================================================================
  # ============================================================================
  
  # ----------------------------------------------------------------------------
  # ============================================================================
  # MISSINGNESS HEATMAP OUTPUT 
  # ============================================================================
  # ----------------------------------------------------------------------------
  
  output$proc_missing_heatmap <- renderPlotly({
    # Safety check - only render if we're in the correct tab and have valid inputs
    if(is.null(input$master_cat_vars) || is.null(input$master_numeric_vars)) {
      return(plot_ly() %>% add_annotations(
        text = "Switch to the 'Data Processing Strategy' tab to view missingness analysis",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    plot_df <- master_filtered_data()
    
    # Validate data
    if(is.null(plot_df) || !is.data.frame(plot_df) || nrow(plot_df) == 0 || ncol(plot_df) == 0) {
      return(plot_ly() %>% add_annotations(
        text = "No data available after applying filters.\nPlease adjust your variable selection or thresholds.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    n_obs <- nrow(plot_df)
    n_vars <- ncol(plot_df)
    
    if(n_obs == 0 || n_vars == 0) {
      return(plot_ly() %>% add_annotations(
        text = "No data available after applying filters.\nPlease adjust your variable selection or thresholds.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    code_column <- plot_df$CODE
    if(is.null(code_column) || length(code_column) != n_obs) {
      row_indices <- as.numeric(rownames(plot_df))
      if(!any(is.na(row_indices)) && "CODE" %in% names(df)) {
        code_column <- df$CODE[row_indices]
      } else {
        code_column <- 1:n_obs
      }
    }
    
    show_neg99 <- "neg99" %in% input$master_mv_types
    show_dash <- "dash" %in% input$master_mv_types
    show_na_string <- "na_string" %in% input$master_mv_types
    
    color_matrix <- matrix("#ADD8E6", nrow = n_obs, ncol = n_vars)
    colnames(color_matrix) <- names(plot_df)
    has_neg99 <- FALSE
    has_dash <- FALSE
    has_na_string <- FALSE
    has_r_na <- FALSE
    
    for(i in 1:n_obs) {
      for(j in 1:n_vars) {
        val <- plot_df[i, j]
        if(!is.na(val) && val == "-99") {
          has_neg99 <- TRUE
          if(show_neg99) color_matrix[i, j] <- "#FF69B4"  # Hot pink for -99
        } else if(!is.na(val) && val == "--") {
          has_dash <- TRUE
          if(show_dash) color_matrix[i, j] <- "#32CD32"   # Lime green for --
        } else if(!is.na(val) && val == "NA") {
          has_na_string <- TRUE
          if(show_na_string) color_matrix[i, j] <- "#FF0000"  # Red for "NA" string
        } else if(is.na(val)) {
          has_r_na <- TRUE
          color_matrix[i, j] <- "#808080"  # Gray for R NA
        }
      }
    }
    
    if(input$master_order == "desc") {
      missing_pcts <- sapply(names(plot_df), function(col) {
        vals <- as.character(plot_df[[col]])
        round(100 * sum(vals %in% c("-99", "--", "NA") | is.na(vals)) / n_obs, 1)
      })
      order_idx <- order(missing_pcts, decreasing = TRUE)
      color_matrix <- color_matrix[, order_idx, drop = FALSE]
      plot_df <- plot_df[, order_idx, drop = FALSE]
    }
    
    missing_pcts <- sapply(names(plot_df), function(col) {
      vals <- as.character(plot_df[[col]])
      round(100 * sum(vals %in% c("-99", "--", "NA") | is.na(vals)) / n_obs, 1)
    })
    
    var_labels <- paste0(colnames(color_matrix), " (", missing_pcts, "%)")
    
    hover_text <- matrix("", nrow = n_obs, ncol = n_vars)
    for(i in 1:n_obs) {
      for(j in 1:n_vars) {
        val <- plot_df[i, j]
        var_name <- colnames(plot_df)[j]
        code_val <- if(length(code_column) >= i) code_column[i] else i
        if(!is.na(val) && val == "-99") {
          hover_text[i, j] <- paste0("CODE: ", code_val, "\nObservation: ", i, "\nVariable: ", var_name, "\nStatus: -99")
        } else if(!is.na(val) && val == "--") {
          hover_text[i, j] <- paste0("CODE: ", code_val, "\nObservation: ", i, "\nVariable: ", var_name, "\nStatus: --")
        } else if(!is.na(val) && val == "NA") {
          hover_text[i, j] <- paste0("CODE: ", code_val, "\nObservation: ", i, "\nVariable: ", var_name, "\nStatus: NA string")
        } else if(is.na(val)) {
          hover_text[i, j] <- paste0("CODE: ", code_val, "\nObservation: ", i, "\nVariable: ", var_name, "\nStatus: R NA")
        } else {
          hover_text[i, j] <- paste0("CODE: ", code_val, "\nObservation: ", i, "\nVariable: ", var_name, "\nStatus: Present")
        }
      }
    }
    
    if(n_obs > 100) { 
      tickvals <- seq(1, n_obs, by = 20)
      ticktext <- seq(1, n_obs, by = 20)
    } else if(n_obs > 50) { 
      tickvals <- seq(1, n_obs, by = 10)
      ticktext <- seq(1, n_obs, by = 10)
    } else if(n_obs > 20) { 
      tickvals <- seq(1, n_obs, by = 5)
      ticktext <- seq(1, n_obs, by = 5)
    } else { 
      tickvals <- 1:n_obs
      ticktext <- 1:n_obs
    }
    
    # Create color legend/key annotations
    legend_items <- 1  # Start with "Present"
    legend_colors <- list(Present = "#ADD8E6", Neg99 = "#FF69B4", Dash = "#32CD32", NA_String = "#FF0000", R_NA = "#808080")
    
    if(has_neg99 && show_neg99) legend_items <- legend_items + 1
    if(has_dash && show_dash) legend_items <- legend_items + 1
    if(has_na_string && show_na_string) legend_items <- legend_items + 1
    if(has_r_na) legend_items <- legend_items + 1
    
    total_width <- 0.8
    start_x <- 0.15
    spacing <- total_width / legend_items
    
    # Build the legend annotations
    key_annotations <- list(
      list(x = 0.5, y = 1.12, text = "Missingness Visualisation:", showarrow = FALSE,
           xref = "paper", yref = "paper", font = list(size = 16, color = "#2C3E50", weight = "bold"))
    )
    
    current_x <- start_x
    key_annotations <- append(key_annotations, list(
      list(x = current_x, y = 1.06, text = "Present", showarrow = FALSE,
           xref = "paper", yref = "paper", font = list(color = legend_colors$Present, size = 12, weight = "bold"))
    ))
    current_x <- current_x + spacing
    
    if(has_neg99 && show_neg99) {
      key_annotations <- append(key_annotations, list(
        list(x = current_x, y = 1.06, text = "-99", showarrow = FALSE,
             xref = "paper", yref = "paper", font = list(color = legend_colors$Neg99, size = 12, weight = "bold"))
      ))
      current_x <- current_x + spacing
    }
    if(has_dash && show_dash) {
      key_annotations <- append(key_annotations, list(
        list(x = current_x, y = 1.06, text = "--", showarrow = FALSE,
             xref = "paper", yref = "paper", font = list(color = legend_colors$Dash, size = 12, weight = "bold"))
      ))
      current_x <- current_x + spacing
    }
    if(has_na_string && show_na_string) {
      key_annotations <- append(key_annotations, list(
        list(x = current_x, y = 1.06, text = "NA string", showarrow = FALSE,
             xref = "paper", yref = "paper", font = list(color = legend_colors$NA_String, size = 12, weight = "bold"))
      ))
      current_x <- current_x + spacing
    }
    if(has_r_na) {
      key_annotations <- append(key_annotations, list(
        list(x = current_x, y = 1.06, text = "R NA", showarrow = FALSE,
             xref = "paper", yref = "paper", font = list(color = legend_colors$R_NA, size = 12, weight = "bold"))
      ))
    }
    
    # Create the heatmap with color scale
    unique_colors <- unique(as.vector(color_matrix))
    if(length(unique_colors) == 0) unique_colors <- "#ADD8E6"
    color_scale <- list()
    for(i in seq_along(unique_colors)) {
      color_scale[[i]] <- list((i-1)/(length(unique_colors)-1), unique_colors[i])
    }
    color_to_num <- setNames(seq_along(unique_colors), unique_colors)
    numeric_matrix <- matrix(color_to_num[color_matrix], nrow = n_obs, ncol = n_vars)
    
    plot_ly(z = numeric_matrix, x = var_labels, y = 1:n_obs, type = "heatmap", 
            colorscale = color_scale, text = hover_text, hoverinfo = "text", 
            showscale = FALSE) %>%
      layout(
        annotations = key_annotations,
        title = list(text = paste("Missing Values Heatmap -", n_obs, "Observations,", n_vars, "Variables"), 
                     font = list(size = 16), y = 0.98),
        xaxis = list(title = "Variables", tickangle = -45,
                     tickfont = list(size = ifelse(n_vars > 15, 8, ifelse(n_vars > 10, 9, 11)))),
        yaxis = list(title = "Observation Index", tickmode = "array",
                     tickvals = tickvals, ticktext = ticktext,
                     titlefont = list(size = 14), autorange = "reversed"),
        margin = list(b = ifelse(n_vars > 15, 150, ifelse(n_vars > 10, 130, 120)), 
                      l = 80, t = 120, r = 50)
      )
  })
  
  # ============================================================================
  # UPSET PLOT OUTPUTS
  # ============================================================================
  
  upset_filtered_data <- reactive({
    data <- master_filtered_data()
    
    # Return empty if no data
    if(is.null(data) || nrow(data) == 0) {
      return(data.frame())
    }
    
    result <- data
    for(col in names(result)) {
      vals <- as.character(result[[col]])
      vals[vals %in% c("-99", "--", "NA")] <- NA
      result[[col]] <- vals
    }
    return(result)
  })
  
  # Render Upset Plot
  # Render Upset Plot
  output$upset_plot <- renderPlot({
    df_filtered <- upset_filtered_data()
    
    # Check if data exists - using simple if statements (no validate with named arguments)
    if(is.null(df_filtered)) {
      plot.new()
      text(0.5, 0.5, "No data available. Please check your filters.", cex = 1.2)
      return()
    }
    
    if(!is.data.frame(df_filtered)) {
      plot.new()
      text(0.5, 0.5, "Data is not valid", cex = 1.2)
      return()
    }
    
    if(nrow(df_filtered) == 0) {
      plot.new()
      text(0.5, 0.5, "No rows available after filtering.\nPlease adjust your filters.", cex = 1.2)
      return()
    }
    
    if(ncol(df_filtered) == 0) {
      plot.new()
      text(0.5, 0.5, "No columns available after filtering.\nPlease select at least one variable.", cex = 1.2)
      return()
    }
    
    # Calculate missing values per column
    missing_counts <- colSums(is.na(df_filtered))
    missing_cols <- names(missing_counts[missing_counts > 0])
    
    # Remove CODE and OBS_TYPE if they exist
    missing_cols <- missing_cols[!missing_cols %in% c("CODE", "OBS_TYPE")]
    
    # Check if we have enough columns with missing values
    if(length(missing_cols) < 2) {
      plot.new()
      text(0.5, 0.5, paste("Need at least 2 variables with missing values.\nFound:", length(missing_cols)), cex = 1.2)
      return()
    }
    
    # Limit number of sets shown based on user input
    n_sets <- input$upset_nsets %||% 11
    missing_cols <- head(missing_cols, n_sets)
    
    # Create the upset data frame
    upset_df <- df_filtered %>%
      dplyr::select(dplyr::all_of(missing_cols)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.integer(is.na(.))))
    
    # Convert order.by to ComplexUpset format
    sort_by <- switch(input$upset_order,
                      "freq" = "cardinality",
                      "degree" = "degree",
                      "both" = "cardinality",
                      "cardinality"
    )
    
    # Number of intersections to show
    n_intersections <- input$upset_nintersects %||% 20
    
    # Create the upset plot with error handling
    tryCatch({
      # Create the base upset plot
      upset_plot <- ComplexUpset::upset(
        upset_df,
        intersect = missing_cols,
        base_annotations = list(
          'Intersection size' = ComplexUpset::intersection_size()
        ),
        sort_intersections_by = sort_by,
        n_intersections = n_intersections,
        width_ratio = 0.2,
        # Remove the angled text by setting minimal theme and removing axis text
        themes = ComplexUpset::upset_default_themes(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank()
        )
      ) +
        ggplot2::labs(
          title = "Missing Data Pattern (UpSet Plot)",
          subtitle = paste("Variables:", length(missing_cols), "| Observations:", nrow(upset_df), 
                           "| Showing top", n_intersections, "intersections")
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11),
          # Remove angled x-axis text completely
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          # Make sure legend is readable
          legend.position = "bottom"
        )
      
      # Print the plot
      print(upset_plot)
      
    }, error = function(e) {
      # If error, show message
      plot.new()
      text(0.5, 0.5, paste("Error creating Upset plot:\n", substr(e$message, 1, 100)), cex = 0.9, col = "red")
      return()
    })
  })
  
  output$pattern_selector <- renderUI({
    df_upset <- upset_filtered_data()
    req(nrow(df_upset) > 0)
    
    missing_cols <- names(which(sapply(df_upset, function(x) sum(is.na(x), na.rm = TRUE)) > 0))
    missing_cols <- missing_cols[!missing_cols %in% c("CODE", "OBS_TYPE")]
    
    if(length(missing_cols) == 0) {
      return(div(class = "alert alert-warning", "No missing values found in selected variables"))
    }
    
    upset_matrix <- as.data.frame(is.na(df_upset[, missing_cols, drop = FALSE]))
    pattern_strings <- apply(upset_matrix, 1, function(row) {
      if(any(row)) paste(names(which(row)), collapse = " + ") else "Complete Row (No Missing)"
    })
    pattern_counts <- sort(table(pattern_strings), decreasing = TRUE)
    
    selectInput("selected_pattern", "Select a missingness pattern to view details:", 
                choices = names(pattern_counts), 
                selected = names(pattern_counts)[1])
  })
  
  output$pattern_description <- renderUI({
    req(input$selected_pattern)
    df_upset <- upset_filtered_data()
    req(nrow(df_upset) > 0)
    
    missing_cols <- names(which(sapply(df_upset, function(x) sum(is.na(x), na.rm = TRUE)) > 0))
    missing_cols <- missing_cols[!missing_cols %in% c("CODE", "OBS_TYPE")]
    
    upset_matrix <- as.data.frame(is.na(df_upset[, missing_cols, drop = FALSE]))
    pattern_strings <- apply(upset_matrix, 1, function(row) {
      if(any(row)) paste(names(which(row)), collapse = " + ") else "Complete Row (No Missing)"
    })
    
    selected_rows <- which(pattern_strings == input$selected_pattern)
    pattern_count <- length(selected_rows)
    
    if(input$selected_pattern == "Complete Row (No Missing)") {
      missing_vars <- "None"
    } else {
      missing_vars <- strsplit(input$selected_pattern, " \\+ ")[[1]]
    }
    
    div(class = "alert alert-info",
        tags$strong("Selected Pattern:"), tags$br(), tags$code(input$selected_pattern), tags$br(), tags$br(),
        tags$strong("Variables Missing Together:"), tags$br(), 
        if(length(missing_vars) > 0) tags$ul(lapply(missing_vars, function(v) tags$li(v))) else "None", tags$br(),
        tags$strong("Number of Observations:"), tags$br(), tags$span(pattern_count, style = "font-size: 24px; font-weight: bold; color: #2C3E50;"), tags$br(),
        tags$strong("Percentage of Filtered Data:"), tags$br(), paste0(round(100 * pattern_count / nrow(df_upset), 1), "%")
    )
  })
  
  output$pattern_data_table <- renderDT({
    req(input$selected_pattern)
    req(input$selected_pattern != "No missing values in selected variables")
    
    df_upset <- upset_filtered_data()
    req(nrow(df_upset) > 0)
    
    missing_cols <- names(which(sapply(df_upset, function(x) sum(is.na(x), na.rm = TRUE)) > 0))
    missing_cols <- missing_cols[!missing_cols %in% c("CODE", "OBS_TYPE")]
    
    upset_matrix <- as.data.frame(is.na(df_upset[, missing_cols, drop = FALSE]))
    pattern_strings <- apply(upset_matrix, 1, function(row) {
      if(any(row)) paste(names(which(row)), collapse = " + ") else "Complete Row (No Missing)"
    })
    
    selected_rows <- which(pattern_strings == input$selected_pattern)
    req(length(selected_rows) > 0)
    
    original_df <- df
    row_indices <- as.numeric(rownames(df_upset)[selected_rows])
    pattern_data <- original_df[row_indices, , drop = FALSE]
    
    if(!"CODE" %in% names(pattern_data) && "CODE" %in% names(original_df)) {
      pattern_data$CODE <- original_df$CODE[row_indices]
    }
    if(!"OBS_TYPE" %in% names(pattern_data) && "OBS_TYPE" %in% names(original_df)) {
      pattern_data$OBS_TYPE <- original_df$OBS_TYPE[row_indices]
    }
    
    always_first <- c("CODE", "OBS_TYPE")
    cols_in_order <- c()
    for(col in always_first) {
      if(col %in% names(pattern_data)) {
        cols_in_order <- c(cols_in_order, col)
      }
    }
    other_cols <- setdiff(names(pattern_data), cols_in_order)
    cols_in_order <- c(cols_in_order, other_cols)
    
    pattern_data <- pattern_data[, cols_in_order, drop = FALSE]
    
    datatable(pattern_data, 
              options = list(scrollX = TRUE, pageLength = 10, lengthMenu = c(10, 25, 50, 100)), 
              rownames = FALSE,
              caption = paste("Showing", nrow(pattern_data), "observations with pattern:", input$selected_pattern)) %>%
      formatStyle(names(pattern_data),
                  backgroundColor = JS("function(value) {
                    if(value === '-99') return '#FFB6C1';
                    if(value === '--') return '#FFD700';
                    if(value === 'NA') return '#FF6347';
                    return null;
                  }"))
  })
  
  # ============================================================================
  # MISSINGNESS PREDICTION (RPART)
  # ============================================================================
  
  
  create_missingness_data <- function(data, target_var, predictor_vars, missing_types) {
    # Remove CODE from predictors if it's there
    predictor_vars <- predictor_vars[predictor_vars != "CODE"]
    
    valid_predictors <- predictor_vars[predictor_vars %in% names(data)]
    valid_predictors <- valid_predictors[valid_predictors != target_var]
    if(length(valid_predictors) == 0) {
      showNotification("No valid predictor variables found. Please select different predictors.", type = "error")
      return(NULL)
    }
    
    result_data <- data[, c(target_var, valid_predictors), drop = FALSE]
    
    target_vals <- as.character(result_data[[target_var]])
    target_missing <- rep(FALSE, nrow(result_data))
    
    if(!is.null(missing_types) && "neg99" %in% missing_types) target_missing <- target_missing | (target_vals == "-99")
    if("dash" %in% missing_types) target_missing <- target_missing | (target_vals == "--")
    if("na_string" %in% missing_types) target_missing <- target_missing | (target_vals == "NA")
    if("r_na" %in% missing_types) target_missing <- target_missing | is.na(target_vals)
    
    result_data$target_missing <- factor(target_missing, levels = c(FALSE, TRUE), labels = c("Present", "Missing"))
    result_data[[target_var]] <- NULL
    
    for(col in names(result_data)) {
      if(col != "target_missing") {
        col_vals <- as.character(result_data[[col]])
        col_vals[col_vals == "-99"] <- NA
        col_vals[col_vals == "--"] <- NA
        col_vals[col_vals == "NA"] <- NA
        
        if(col %in% numeric_cols) {
          result_data[[col]] <- suppressWarnings(as.numeric(col_vals))
        } else {
          result_data[[col]] <- as.factor(col_vals)
        }
      }
    }
    
    predictor_cols <- setdiff(names(result_data), "target_missing")
    complete_rows <- complete.cases(result_data[, predictor_cols, drop = FALSE])
    result_data <- result_data[complete_rows, , drop = FALSE]
    
    if(nrow(result_data) == 0) {
      showNotification("No complete cases found. Try different predictors or handle missing values first.", type = "error")
      return(NULL)
    }
    
    if(length(unique(result_data$target_missing)) < 2) {
      showNotification("Target variable has only one class. Cannot build tree.\nTry a different target variable or adjust missing value types.", type = "error")
      return(NULL)
    }
    
    return(result_data)
  }
  
  observeEvent(input$run_missing_rpart, {
    req(input$missing_target_var)
    req(input$missing_predictors)
    req(length(input$missing_predictors) > 0)
    
    showNotification("Building missingness tree...", type = "message", duration = 2)
    
    data <- master_filtered_data_standardized()
    
    model_data <- create_missingness_data(data, input$missing_target_var, input$missing_predictors, input$missing_rpart_types)
    
    if(is.null(model_data) || nrow(model_data) == 0) {
      showNotification("Failed to create model data. Check console for details.", type = "error", duration = 8)
      return()
    }
    
    formula <- as.formula(paste("target_missing ~", paste(setdiff(names(model_data), "target_missing"), collapse = "+")))
    
    tryCatch({
      tree_model <- rpart::rpart(formula, 
                                 data = model_data, 
                                 method = "class", 
                                 model = TRUE,
                                 control = rpart::rpart.control(
                                   cp = input$missing_rpart_cp,
                                   minsplit = input$missing_rpart_minsplit,
                                   minbucket = max(1, floor(input$missing_rpart_minsplit / 3))
                                 ))
      
      missing_rpart_model(tree_model)
      
      total_missing <- sum(model_data$target_missing == "Missing")
      total_present <- sum(model_data$target_missing == "Present")
      total_obs <- nrow(model_data)
      
      if(nrow(tree_model$frame) <= 1) {
        showNotification(paste("Tree built but no splits found. Missing rate:", 
                               round(100 * total_missing / total_obs, 1), "%"), 
                         type = "warning", duration = 5)
      } else {
        showNotification(paste("Tree built successfully! Missing rate:", 
                               round(100 * total_missing / total_obs, 1), 
                               "% (", total_missing, " missing, ", total_present, " present)"), 
                         type = "message", duration = 5)
      }
      
    }, error = function(e) {
      showNotification(paste("Error building tree:", e$message), type = "error", duration = 8)
    })
  })
  
  output$missing_rpart_tree <- renderPlot({
    req(missing_rpart_model())
    model <- missing_rpart_model()
    
    if(is.null(model$frame) || nrow(model$frame) <= 1) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No splits found - missingness cannot be predicted\nTry different parameters or variables", cex = 1.2)
      return()
    }
    
    if(model$method != "class") {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Model is not a classification tree", cex = 1.2)
      return()
    }
    
    par(mar = c(1, 1, 3, 1))
    
    rpart.plot::prp(model, 
                    type = 2, 
                    extra = 104, 
                    fallen.leaves = TRUE,
                    main = paste("What Predicts Missing Values in", input$missing_target_var, "?"),
                    branch.type = 5, 
                    shadow.col = "gray90",
                    box.palette = list("Present" = "lightblue", "Missing" = "lightcoral"),
                    cex = 0.8, 
                    under = TRUE, 
                    varlen = 0, 
                    faclen = 0, 
                    digits = 2, 
                    roundint = FALSE,
                    yes.text = "Missing", 
                    no.text = "Present")
  }, height = 500, width = 800, res = 100)
  
  output$missing_rpart_importance <- renderPlotly({
    req(missing_rpart_model())
    model <- missing_rpart_model()
    
    importance <- model$variable.importance
    
    if(is.null(importance) || length(importance) == 0) {
      return(plot_ly() %>% 
               add_annotations(text = "No variable importance available - tree may have no splits", 
                               x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)))
    }
    
    imp_df <- data.frame(Variable = names(importance), Importance = importance)
    imp_df <- imp_df[order(imp_df$Importance), ]
    imp_df$Variable <- factor(imp_df$Variable, levels = imp_df$Variable)
    
    p <- ggplot(imp_df, aes(x = Importance, y = Variable)) +
      geom_bar(stat = "identity", fill = "#ADD8E6", alpha = 0.8) +
      labs(title = paste("What Predicts Missingness in", input$missing_target_var, "?"),
           x = "Importance Score", y = "") +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p)
  })
  
  # ============================================================================
  # MISSING VARIABLE CORRELATION
  # ============================================================================
  
  output$proc_missing_corr_plot <- renderPlotly({
    data <- master_filtered_data_standardized()
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
  
  
  
  
  # ----------------------------------------------------------------------------
  # ============================================================================
  # OUTLIER DETECTION FUNCTIONS
  # ============================================================================
  # ----------------------------------------------------------------------------
  
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
    
    complete_rows <- complete.cases(result)
    result <- result[complete_rows, , drop = FALSE]
    
    if(nrow(result) < 5) return(NULL)
    
    if("CODE" %in% names(data) && !include_code) {
      rownames(result) <- data$CODE[complete_rows]
    } else {
      rownames(result) <- which(complete_rows)
    }
    
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
  
  # ============================================================================
  # Outlier Analysis Reactive
  # ============================================================================
  
  # Cache outlier results to prevent infinite recalculations
  outlier_analysis_reactive <- reactive({
    # Only recalculate when threshold inputs change
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
    
    data <- master_filtered_data_standardized()
    
    # Return empty list if no data
    if(is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
      return(list())
    }
    
    results <- list()
    
    # Only run each method if there's enough data
    if(nrow(data) >= 10 && ncol(data) >= 2) {
      results$Mahalanobis <- detect_mahalanobis_outliers(data, params$mahalanobis_threshold)
      results$Cooks <- detect_cooks_outliers(data, params$cooks_method)
      results$LOF <- detect_lof_outliers(data, params$lof_minpts, params$lof_threshold)
      results$SVM <- detect_svm_outliers(data, params$svm_nu, params$svm_kernel)
      results$RF_Residuals <- detect_rf_outliers(data, params$rf_iqr)
      results$IForest <- detect_iforest_outliers(data, params$if_threshold)
    }
    
    outlier_results(results)
    results
  })
  
  create_outlier_consensus <- function(results_list, data) {
    processed_data <- prepare_outlier_data(data)
    if(is.null(processed_data)) {
      return(list(data = data.frame(Code = character(), Total_Flags = integer()), methods = character()))
    }
    
    processed_rows <- rownames(processed_data)
    
    if("CODE" %in% names(data)) {
      row_indices <- as.numeric(processed_rows)
      if(!any(is.na(row_indices))) {
        code_values <- data$CODE[row_indices]
      } else {
        code_values <- data$CODE[match(processed_rows, rownames(data))]
      }
      if(any(is.na(code_values))) {
        code_values <- processed_rows
      }
    } else {
      code_values <- processed_rows
    }
    
    consensus_df <- data.frame(
      Code = as.character(code_values),
      stringsAsFactors = FALSE
    )
    
    consensus_df$RowIndex <- as.numeric(processed_rows)
    
    method_names <- c()
    
    for(method_name in names(results_list)) {
      if(!is.null(results_list[[method_name]])) {
        method_names <- c(method_names, method_name)
        flags <- rep(0, nrow(consensus_df))
        
        outlier_ids <- results_list[[method_name]]$outlier_names
        
        if(length(outlier_ids) > 0) {
          outlier_ids_char <- as.character(outlier_ids)
          
          for(i in 1:nrow(consensus_df)) {
            current_code <- consensus_df$Code[i]
            current_row <- as.character(consensus_df$RowIndex[i])
            
            if(current_code %in% outlier_ids_char || current_row %in% outlier_ids_char) {
              flags[i] <- 1
            }
          }
        }
        
        consensus_df[[method_name]] <- flags
      }
    }
    
    if(length(method_names) > 0) {
      consensus_df$Total_Flags <- rowSums(consensus_df[, method_names, drop = FALSE])
    } else {
      consensus_df$Total_Flags <- 0
    }
    
    consensus_df$RowIndex <- NULL
    consensus_df <- consensus_df[consensus_df$Total_Flags > 0, ]
    consensus_df <- consensus_df[order(consensus_df$Total_Flags, decreasing = TRUE), ]
    
    return(list(data = consensus_df, methods = method_names))
  }
  
  outlier_consensus <- reactive({
    results <- outlier_analysis_reactive()
    data <- master_filtered_data_standardized()
    create_outlier_consensus(results, data)
  })
  
  filtered_consensus <- reactive({
    consensus <- outlier_consensus()
    req(consensus$data, nrow(consensus$data) > 0)
    
    selected_methods <- c()
    if(input$show_mahalanobis) selected_methods <- c(selected_methods, "Mahalanobis")
    if(input$show_cooks) selected_methods <- c(selected_methods, "Cooks")
    if(input$show_lof) selected_methods <- c(selected_methods, "LOF")
    if(input$show_svm) selected_methods <- c(selected_methods, "SVM")
    if(input$show_rf) selected_methods <- c(selected_methods, "RF_Residuals")
    if(input$show_iforest) selected_methods <- c(selected_methods, "IForest")
    
    if(length(selected_methods) == 0) {
      return(list(data = data.frame(Code = character(), Total_Flags = integer()), methods = character()))
    }
    
    filtered_data <- consensus$data[, c("Code", selected_methods), drop = FALSE]
    filtered_data$Total_Flags <- rowSums(filtered_data[, selected_methods, drop = FALSE])
    filtered_data <- filtered_data[filtered_data$Total_Flags > 0, ]
    filtered_data <- filtered_data[order(filtered_data$Total_Flags, decreasing = TRUE), ]
    top_n <- min(input$outlier_display_count, nrow(filtered_data))
    filtered_data <- filtered_data[1:top_n, ]
    
    return(list(data = filtered_data, methods = selected_methods))
  })
  
  
  
  # ============================================================================
  # OUTLIER PLOT OUTPUTS - Add these after filtered_consensus()
  # ============================================================================
  
  # Mahalanobis Distance Plot
  output$outlier_mahalanobis_plot <- renderPlotly({
    results <- outlier_analysis_reactive()
    
    # Check if results exist and have Mahalanobis results
    if(is.null(results) || length(results) == 0 || is.null(results$Mahalanobis)) {
      return(plot_ly() %>% add_annotations(
        text = "Not enough data for Mahalanobis distance analysis.\nNeed at least 10 complete observations with 2+ numeric variables.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    maha_result <- results$Mahalanobis
    
    if(is.null(maha_result$scores) || length(maha_result$scores) == 0) {
      return(plot_ly() %>% add_annotations(
        text = "Mahalanobis distance calculation failed.\nCheck data quality.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    # Create data frame for plotting
    plot_df <- data.frame(
      Index = 1:length(maha_result$scores),
      Distance = maha_result$scores,
      IsOutlier = maha_result$is_outlier,
      Name = names(maha_result$scores)
    )
    
    # Create plot
    p <- ggplot(plot_df, aes(x = Index, y = Distance, color = IsOutlier,
                             text = paste("Observation:", Name, "<br>Distance:", round(Distance, 3),
                                          "<br>Outlier:", IsOutlier))) +
      geom_point(size = 2, alpha = 0.7) +
      geom_hline(yintercept = maha_result$threshold, linetype = "dashed", color = "red", alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "#13D4D4", "TRUE" = "#e74c3c")) +
      labs(title = "Mahalanobis Distance",
           subtitle = paste("Threshold (χ²):", round(maha_result$threshold, 3),
                            "| Outliers:", sum(maha_result$is_outlier)),
           x = "Observation Index", y = "Mahalanobis Distance") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Cook's Distance Plot
  output$outlier_cooks_plot <- renderPlotly({
    results <- outlier_analysis_reactive()
    
    if(is.null(results) || length(results) == 0 || is.null(results$Cooks)) {
      return(plot_ly() %>% add_annotations(
        text = "Not enough data for Cook's distance analysis.\nNeed DEATH_RATE and at least 1 predictor.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    cooks_result <- results$Cooks
    
    if(is.null(cooks_result$scores) || length(cooks_result$scores) == 0) {
      return(plot_ly() %>% add_annotations(
        text = "Cook's distance calculation failed.\nCheck that DEATH_RATE and predictors have valid data.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    plot_df <- data.frame(
      Index = 1:length(cooks_result$scores),
      Distance = cooks_result$scores,
      IsOutlier = cooks_result$is_outlier,
      Name = names(cooks_result$scores)
    )
    
    p <- ggplot(plot_df, aes(x = Index, y = Distance, color = IsOutlier,
                             text = paste("Observation:", Name, "<br>Distance:", round(Distance, 4),
                                          "<br>Outlier:", IsOutlier))) +
      geom_point(size = 2, alpha = 0.7) +
      geom_hline(yintercept = cooks_result$threshold, linetype = "dashed", color = "red", alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "#13D4D4", "TRUE" = "#e74c3c")) +
      labs(title = "Cook's Distance",
           subtitle = paste("Threshold:", round(cooks_result$threshold, 4),
                            "| Outliers:", sum(cooks_result$is_outlier)),
           x = "Observation Index", y = "Cook's Distance") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Local Outlier Factor (LOF) Plot
  output$outlier_lof_plot <- renderPlotly({
    results <- outlier_analysis_reactive()
    
    if(is.null(results) || length(results) == 0 || is.null(results$LOF)) {
      return(plot_ly() %>% add_annotations(
        text = "Not enough data for LOF analysis.\nNeed at least 10 complete observations with 2+ numeric variables.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    lof_result <- results$LOF
    
    if(is.null(lof_result$scores) || length(lof_result$scores) == 0) {
      return(plot_ly() %>% add_annotations(
        text = "LOF calculation failed.\nTry reducing minPts or checking data quality.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    plot_df <- data.frame(
      Index = 1:length(lof_result$scores),
      Score = lof_result$scores,
      IsOutlier = lof_result$is_outlier,
      Name = names(lof_result$scores)
    )
    
    p <- ggplot(plot_df, aes(x = Index, y = Score, color = IsOutlier,
                             text = paste("Observation:", Name, "<br>LOF Score:", round(Score, 3),
                                          "<br>Outlier:", IsOutlier))) +
      geom_point(size = 2, alpha = 0.7) +
      geom_hline(yintercept = lof_result$threshold, linetype = "dashed", color = "red", alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "#13D4D4", "TRUE" = "#e74c3c")) +
      labs(title = "Local Outlier Factor (LOF)",
           subtitle = paste("Threshold:", lof_result$threshold,
                            "| Outliers:", sum(lof_result$is_outlier),
                            "| minPts:", lof_result$minPts),
           x = "Observation Index", y = "LOF Score") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # One-Class SVM Plot
  output$outlier_svm_plot <- renderPlotly({
    results <- outlier_analysis_reactive()
    
    if(is.null(results) || length(results) == 0 || is.null(results$SVM)) {
      return(plot_ly() %>% add_annotations(
        text = "Not enough data for One-Class SVM analysis.\nNeed at least 10 complete observations with 2+ numeric variables.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    svm_result <- results$SVM
    
    if(is.null(svm_result$scores) || length(svm_result$scores) == 0) {
      return(plot_ly() %>% add_annotations(
        text = "SVM calculation failed.\nTry adjusting nu parameter or kernel.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    plot_df <- data.frame(
      Index = 1:length(svm_result$scores),
      Score = svm_result$scores,
      IsOutlier = svm_result$is_outlier,
      Name = names(svm_result$scores)
    )
    
    p <- ggplot(plot_df, aes(x = Index, y = Score, color = IsOutlier,
                             text = paste("Observation:", Name, "<br>Decision Value:", round(Score, 3),
                                          "<br>Outlier:", IsOutlier))) +
      geom_point(size = 2, alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "#13D4D4", "TRUE" = "#e74c3c")) +
      labs(title = "One-Class SVM",
           subtitle = paste("nu =", svm_result$nu, "| kernel =", svm_result$kernel,
                            "| Outliers:", sum(svm_result$is_outlier)),
           x = "Observation Index", y = "Decision Value") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Random Forest Residuals Plot
  output$outlier_rf_plot <- renderPlotly({
    results <- outlier_analysis_reactive()
    
    if(is.null(results) || length(results) == 0 || is.null(results$RF_Residuals)) {
      return(plot_ly() %>% add_annotations(
        text = "Not enough data for Random Forest analysis.\nNeed DEATH_RATE and at least 1 predictor.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    rf_result <- results$RF_Residuals
    
    if(is.null(rf_result$scores) || length(rf_result$scores) == 0) {
      return(plot_ly() %>% add_annotations(
        text = "Random Forest calculation failed.\nCheck that DEATH_RATE and predictors have valid data.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    plot_df <- data.frame(
      Index = 1:length(rf_result$scores),
      Residual = rf_result$scores,
      IsOutlier = rf_result$is_outlier,
      Name = names(rf_result$scores)
    )
    
    p <- ggplot(plot_df, aes(x = Index, y = Residual, color = IsOutlier,
                             text = paste("Observation:", Name, "<br>Residual:", round(Residual, 4),
                                          "<br>Outlier:", IsOutlier))) +
      geom_point(size = 2, alpha = 0.7) +
      geom_hline(yintercept = rf_result$threshold_lower, linetype = "dashed", color = "red", alpha = 0.7) +
      geom_hline(yintercept = rf_result$threshold_upper, linetype = "dashed", color = "red", alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "#13D4D4", "TRUE" = "#e74c3c")) +
      labs(title = "Random Forest Residuals",
           subtitle = paste("IQR Multiplier:", rf_result$iqr_multiplier,
                            "| Outliers:", sum(rf_result$is_outlier)),
           x = "Observation Index", y = "Residuals") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Isolation Forest Plot
  output$outlier_if_plot <- renderPlotly({
    results <- outlier_analysis_reactive()
    
    if(is.null(results) || length(results) == 0 || is.null(results$IForest)) {
      return(plot_ly() %>% add_annotations(
        text = "Not enough data for Isolation Forest analysis.\nNeed at least 10 complete observations with 2+ numeric variables.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    if_result <- results$IForest
    
    if(is.null(if_result$scores) || length(if_result$scores) == 0) {
      return(plot_ly() %>% add_annotations(
        text = "Isolation Forest calculation failed.\nTry adjusting threshold or checking data quality.",
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 14)
      ))
    }
    
    plot_df <- data.frame(
      Index = 1:length(if_result$scores),
      Score = if_result$scores,
      IsOutlier = if_result$is_outlier,
      Name = names(if_result$scores)
    )
    
    p <- ggplot(plot_df, aes(x = Index, y = Score, color = IsOutlier,
                             text = paste("Observation:", Name, "<br>Anomaly Score:", round(Score, 3),
                                          "<br>Outlier:", IsOutlier))) +
      geom_point(size = 2, alpha = 0.7) +
      geom_hline(yintercept = if_result$threshold, linetype = "dashed", color = "red", alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "#13D4D4", "TRUE" = "#e74c3c")) +
      labs(title = "Isolation Forest",
           subtitle = paste("Threshold:", if_result$threshold,
                            "| Outliers:", sum(if_result$is_outlier)),
           x = "Observation Index", y = "Anomaly Score") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  
  
  
  # ============================================================================
  # OUTLIER STATISTICS OUTPUTS WITH COLLAPSIBLE SECTIONS
  # ============================================================================
  
  # Mahalanobis Distance Statistics (Collapsible)
  output$outlier_mahalanobis_stats <- renderPrint({
    results <- outlier_analysis_reactive()
    if(is.null(results) || is.null(results$Mahalanobis)) {
      cat("Mahalanobis distance results not available.\n")
      return()
    }
    maha <- results$Mahalanobis
    cat("Mahalanobis Distance Statistics\n")
    cat("===============================\n\n")
    cat("Number of observations:", maha$n_obs, "\n")
    cat("Degrees of freedom:", maha$df, "\n")
    cat("Chi-square threshold:", round(maha$threshold, 4), "\n")
    cat("Outliers detected:", sum(maha$is_outlier), "\n")
    cat("Outlier percentage:", round(100 * sum(maha$is_outlier) / maha$n_obs, 2), "%\n\n")
    cat("Top outliers (highest distance):\n")
    top_outliers <- head(sort(maha$scores[maha$is_outlier], decreasing = TRUE), 10)
    if(length(top_outliers) > 0) {
      for(i in seq_along(top_outliers)) {
        cat("  ", names(top_outliers)[i], ":", round(top_outliers[i], 4), "\n")
      }
    }
  })
  
  # Cook's Distance Statistics (Collapsible)
  output$outlier_cooks_stats <- renderPrint({
    results <- outlier_analysis_reactive()
    if(is.null(results) || is.null(results$Cooks)) {
      cat("Cook's distance results not available.\n")
      return()
    }
    cooks <- results$Cooks
    cat("Cook's Distance Statistics\n")
    cat("==========================\n\n")
    cat("Number of observations:", cooks$n_obs, "\n")
    cat("Threshold method:", cooks$method, "\n")
    cat("Threshold value:", round(cooks$threshold, 6), "\n")
    cat("Outliers detected:", sum(cooks$is_outlier), "\n")
    cat("Outlier percentage:", round(100 * sum(cooks$is_outlier) / cooks$n_obs, 2), "%\n\n")
    cat("Top outliers (highest Cook's distance):\n")
    top_outliers <- head(sort(cooks$scores[cooks$is_outlier], decreasing = TRUE), 10)
    if(length(top_outliers) > 0) {
      for(i in seq_along(top_outliers)) {
        cat("  ", names(top_outliers)[i], ":", round(top_outliers[i], 6), "\n")
      }
    }
  })
  
  # Local Outlier Factor Statistics (Collapsible)
  output$outlier_lof_stats <- renderPrint({
    results <- outlier_analysis_reactive()
    if(is.null(results) || is.null(results$LOF)) {
      cat("LOF results not available.\n")
      return()
    }
    lof <- results$LOF
    cat("Local Outlier Factor Statistics\n")
    cat("===============================\n\n")
    cat("Number of observations:", lof$n_obs, "\n")
    cat("minPts (neighbors):", lof$minPts, "\n")
    cat("LOF threshold:", lof$threshold, "\n")
    cat("Outliers detected:", sum(lof$is_outlier), "\n")
    cat("Outlier percentage:", round(100 * sum(lof$is_outlier) / lof$n_obs, 2), "%\n\n")
    cat("Top outliers (highest LOF score):\n")
    top_outliers <- head(sort(lof$scores[lof$is_outlier], decreasing = TRUE), 10)
    if(length(top_outliers) > 0) {
      for(i in seq_along(top_outliers)) {
        cat("  ", names(top_outliers)[i], ":", round(top_outliers[i], 4), "\n")
      }
    }
  })
  
  # One-Class SVM Statistics (Collapsible)
  output$outlier_svm_stats <- renderPrint({
    results <- outlier_analysis_reactive()
    if(is.null(results) || is.null(results$SVM)) {
      cat("One-Class SVM results not available.\n")
      return()
    }
    svm <- results$SVM
    cat("One-Class SVM Statistics\n")
    cat("========================\n\n")
    cat("Number of observations:", svm$n_obs, "\n")
    cat("nu parameter:", svm$nu, "\n")
    cat("Kernel:", svm$kernel, "\n")
    cat("Outliers detected:", sum(svm$is_outlier), "\n")
    cat("Outlier percentage:", round(100 * sum(svm$is_outlier) / svm$n_obs, 2), "%\n")
  })
  
  # Random Forest Residuals Statistics (Collapsible)
  output$outlier_rf_stats <- renderPrint({
    results <- outlier_analysis_reactive()
    if(is.null(results) || is.null(results$RF_Residuals)) {
      cat("Random Forest results not available.\n")
      return()
    }
    rf <- results$RF_Residuals
    cat("Random Forest Residuals Statistics\n")
    cat("==================================\n\n")
    cat("Number of observations:", rf$n_obs, "\n")
    cat("IQR multiplier:", rf$iqr_multiplier, "\n")
    cat("Lower threshold:", round(rf$threshold_lower, 4), "\n")
    cat("Upper threshold:", round(rf$threshold_upper, 4), "\n")
    cat("Outliers detected:", sum(rf$is_outlier), "\n")
    cat("Outlier percentage:", round(100 * sum(rf$is_outlier) / rf$n_obs, 2), "%\n\n")
    cat("Top outliers (largest absolute residual):\n")
    residuals_sorted <- abs(rf$scores)
    names(residuals_sorted) <- names(rf$scores)
    top_outliers <- head(sort(residuals_sorted[rf$is_outlier], decreasing = TRUE), 10)
    if(length(top_outliers) > 0) {
      for(i in seq_along(top_outliers)) {
        cat("  ", names(top_outliers)[i], ":", round(rf$scores[names(top_outliers)[i]], 4), "\n")
      }
    }
  })
  
  # Isolation Forest Statistics (Collapsible)
  output$outlier_if_stats <- renderPrint({
    results <- outlier_analysis_reactive()
    if(is.null(results) || is.null(results$IForest)) {
      cat("Isolation Forest results not available.\n")
      return()
    }
    iforest <- results$IForest
    cat("Isolation Forest Statistics\n")
    cat("===========================\n\n")
    cat("Number of observations:", iforest$n_obs, "\n")
    cat("Anomaly score threshold:", iforest$threshold, "\n")
    cat("Outliers detected:", sum(iforest$is_outlier), "\n")
    cat("Outlier percentage:", round(100 * sum(iforest$is_outlier) / iforest$n_obs, 2), "%\n\n")
    cat("Top outliers (highest anomaly scores):\n")
    top_outliers <- head(sort(iforest$scores[iforest$is_outlier], decreasing = TRUE), 10)
    if(length(top_outliers) > 0) {
      for(i in seq_along(top_outliers)) {
        cat("  ", names(top_outliers)[i], ":", round(top_outliers[i], 4), "\n")
      }
    }
  })
  
  
  # Render the consensus table
  output$outlier_consensus_table <- renderDT({
    filtered <- filtered_consensus()
    req(filtered$data, nrow(filtered$data) > 0)
    
    display_df <- filtered$data
    
    col_order <- c("Code", "Total_Flags", filtered$methods)
    display_df <- display_df[, col_order]
    
    datatable(display_df, 
              options = list(
                pageLength = 25, 
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                columnDefs = list(
                  list(className = 'dt-center', targets = '_all')
                ),
                order = list(list(1, 'desc'))
              ),
              rownames = FALSE,
              extensions = 'Buttons',
              caption = htmltools::tags$caption(
                style = 'caption-side: bottom; text-align: center;',
                paste("States flagged by selected outlier detection methods (showing top", input$outlier_display_count, ")")
              )) %>%
      formatStyle("Total_Flags",
                  backgroundColor = styleInterval(
                    cuts = c(1, 2, 3, 4),
                    values = c("#fff3cd", "#ffeaa7", "#fdcb6e", "#e17055", "#c0392b")
                  )) %>%
      formatStyle(columns = filtered$methods,
                  backgroundColor = styleEqual(c(1), c("#e74c3c"))) %>%
      formatStyle(columns = "Code",
                  fontWeight = "bold")
  })
  
  
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 7: RECIPE BUILDER (Data Processing Controls - Tab 3)
  # ========================================================================
  # ========================================================================
  
  
  
  
  
  
  
  
  # -------------------------------------------------------------------------
  # Save and Load Recipes
  # -------------------------------------------------------------------------
  observeEvent(input$save_pipeline_btn, {
    pipeline_name <- trimws(input$pipeline_name)
    if(pipeline_name == "") {
      showNotification("Please enter a recipe name", type = "error")
      return()
    }
    steps <- pipeline_steps()
    if(!is.list(steps) || length(steps) == 0) {
      showNotification("No steps in recipe. Add steps first.", type = "error")
      return()
    }
    current <- saved_pipelines()
    original_name <- pipeline_name
    counter <- 1
    while(pipeline_name %in% names(current)) {
      pipeline_name <- paste0(original_name, "_v", counter)
      counter <- counter + 1
    }
    if(pipeline_name != original_name) {
      showNotification(paste("Name already exists. Saved as:", pipeline_name), type = "warning")
    }
    current[[pipeline_name]] <- list(steps = steps, created = Sys.time(), comments = trimws(input$pipeline_comments %||% ""))
    saved_pipelines(current)
    save_all_data()
    updateTextInput(session, "pipeline_name", value = "")
    updateTextAreaInput(session, "pipeline_comments", value = "")
    showNotification(paste("Recipe saved as:", pipeline_name, "-", length(steps), "steps"), type = "message")
    add_to_log(paste("Saved recipe:", pipeline_name))
  })
  
  observeEvent(input$save_as_pipeline_btn, {
    steps <- pipeline_steps()
    if(!is.list(steps) || length(steps) == 0) {
      showNotification("No steps in recipe. Add steps first.", type = "error")
      return()
    }
    base_name <- trimws(input$pipeline_name)
    if(base_name == "") {
      base_name <- paste0("Recipe_", format(Sys.time(), "%Y%m%d_%H%M"))
    }
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    new_name <- paste0(base_name, "_", timestamp)
    new_name <- gsub(" ", "_", new_name)
    new_name <- gsub("[^a-zA-Z0-9_]", "", new_name)
    current <- saved_pipelines()
    current[[new_name]] <- list(steps = steps, created = Sys.time(), comments = trimws(input$pipeline_comments %||% paste("Saved on", format(Sys.time(), "%Y-%m-%d %H:%M"))))
    saved_pipelines(current)
    save_all_data()
    updateTextInput(session, "pipeline_name", value = "")
    updateTextAreaInput(session, "pipeline_comments", value = "")
    showNotification(paste("✓ Recipe saved as NEW:", new_name, "-", length(steps), "steps"), type = "message")
    add_to_log(paste("Saved new recipe:", new_name))
  })
  
  observeEvent(input$load_pipeline_trigger, {
    req(input$load_pipeline_trigger)
    pipeline_name <- input$load_pipeline_trigger
    if(!pipeline_name %in% names(saved_pipelines())) {
      showNotification(paste("Recipe", pipeline_name, "not found"), type = "error")
      return()
    }
    saved_pipeline <- saved_pipelines()[[pipeline_name]]
    if(is.null(saved_pipeline$steps) || !is.list(saved_pipeline$steps)) {
      showNotification(paste("Recipe", pipeline_name, "has invalid step data"), type = "error")
      return()
    }
    loaded_steps <- saved_pipeline$steps
    if(length(loaded_steps) == 0) {
      showNotification(paste("Recipe", pipeline_name, "has no steps"), type = "warning")
      pipeline_steps(list())
      return()
    }
    reconstructed_steps <- list()
    for(i in seq_along(loaded_steps)) {
      step <- loaded_steps[[i]]
      if(is.list(step)) {
        reconstructed_step <- list(id = i, method = step$method %||% "", cols = step$cols %||% character(0), additional_info = step$additional_info %||% NULL)
        if(reconstructed_step$method != "") {
          reconstructed_steps[[i]] <- reconstructed_step
        }
      }
    }
    reconstructed_steps <- reconstructed_steps[!sapply(reconstructed_steps, is.null)]
    if(length(reconstructed_steps) == 0) {
      showNotification("No valid steps found in recipe", type = "error")
      return()
    }
    pipeline_steps(reconstructed_steps)
    updateTextInput(session, "pipeline_name", value = paste0(pipeline_name, "_modified"))
    comments_msg <- if(!is.null(saved_pipeline$comments) && saved_pipeline$comments != "") paste0(" - Notes: ", saved_pipeline$comments) else ""
    showNotification(paste("✓ Loaded recipe:", pipeline_name, "-", length(reconstructed_steps), "steps", comments_msg), type = "message", duration = 5)
    add_to_log(paste("Loaded saved recipe:", pipeline_name))
  })
  
  observeEvent(input$delete_pipeline_trigger, {
    pipeline_name <- input$delete_pipeline_trigger
    if(!is.null(pipeline_name) && pipeline_name != "" && pipeline_name %in% names(saved_pipelines())) {
      showModal(modalDialog(title = "Delete Recipe", paste("Are you sure you want to delete the recipe:", pipeline_name, "?"),
                            footer = tagList(modalButton("Cancel"), actionButton("confirm_delete_pipeline", "Delete", class = "btn-danger")), easyClose = TRUE))
      pipeline_to_delete <- pipeline_name
      observeEvent(input$confirm_delete_pipeline, {
        current <- saved_pipelines()
        current[[pipeline_to_delete]] <- NULL
        saved_pipelines(current)
        save_all_data()
        showNotification(paste("Recipe", pipeline_to_delete, "deleted"), type = "message")
        add_to_log(paste("Deleted recipe:", pipeline_to_delete))
        removeModal()
      }, once = TRUE)
    }
  })
  
  observeEvent(input$apply_loaded_recipe_btn, {
    steps <- pipeline_steps()
    if(length(steps) == 0) {
      showNotification("No recipe loaded. Load a recipe first.", type = "warning")
      return()
    }
    isolate({ processed_data_working(standardize_missing(df)) })
    data <- isolate(processed_data_working())
    original_rows <- nrow(data); original_cols <- ncol(data)
    add_to_log(paste("=== Applying loaded recipe with", length(steps), "steps ==="))
    data <- apply_steps_to_data(data, steps)
    processed_data_working(data)
    add_to_log(paste("=== Recipe complete: from", original_rows, "rows to", nrow(data), "rows, from", original_cols, "cols to", ncol(data), "cols ==="))
    showNotification(paste("✓ Recipe applied successfully!", nrow(data), "rows,", ncol(data), "columns"), type = "message", duration = 5)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    ds_name <- paste0("Processed_", timestamp)
    current <- saved_datasets()
    current[[ds_name]] <- list(data = data, name = ds_name, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                               rows = nrow(data), cols = ncol(data), comments = "Processed using recipe builder")
    saved_datasets(current)
    save_all_data()
    updatePickerInput(session, "model_dataset_select", choices = names(saved_datasets()), selected = ds_name)
    add_to_log(paste("Auto-saved processed data as:", ds_name))
  })
  
  # -------------------------------------------------------------------------
  # UI Outputs for Recipe Builder
  # -------------------------------------------------------------------------
  output$saved_pipelines_list <- renderUI({
    pipelines <- saved_pipelines()
    if(length(pipelines) == 0) {
      return(div(class = "alert alert-info", "No saved recipes yet. Build one and save it above."))
    }
    tagList(lapply(names(pipelines), function(name) {
      pipeline <- pipelines[[name]]
      steps_count <- length(pipeline$steps)
      comments_html <- if(!is.null(pipeline$comments) && pipeline$comments != "") {
        tags$div(style = "font-size: 10px; color: #666; margin-top: 4px;", icon("comment"), " ", pipeline$comments)
      } else { NULL }
      created_time <- if(!is.null(pipeline$created)) format(pipeline$created, "%Y-%m-%d %H:%M") else "Unknown date"
      div(class = "well", style = "padding: 8px; margin-bottom: 8px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              div(strong(name, style = "color: #2C3E50;"), br(),
                  tags$small(paste(steps_count, "steps |", created_time)), comments_html),
              div(actionButton(paste0("load_pipeline_", gsub(" ", "_", name)), label = "Load", class = "btn-primary btn-xs",
                               style = "margin-right: 5px;", onclick = paste0("Shiny.setInputValue('load_pipeline_trigger', '", name, "', {priority: 'event'})")),
                  actionButton(paste0("delete_pipeline_", gsub(" ", "_", name)), label = "Delete", class = "btn-danger btn-xs",
                               onclick = paste0("Shiny.setInputValue('delete_pipeline_trigger', '", name, "', {priority: 'event'})"))))
      )
    }))
  })
  
  output$saved_datasets_list <- renderUI({
    datasets <- saved_datasets()
    if(length(datasets) == 0) {
      return(div(class = "alert alert-info", "No saved datasets yet. Process data and save one above."))
    }
    tagList(lapply(names(datasets), function(name) {
      ds <- datasets[[name]]
      comments_html <- if(!is.null(ds$comments) && ds$comments != "") {
        tags$div(style = "font-size: 10px; color: #666; margin-top: 4px;", icon("comment"), " ", ds$comments)
      } else { NULL }
      div(class = "well", style = "padding: 8px; margin-bottom: 8px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              div(strong(name, style = "color: #2C3E50;"), br(),
                  tags$small(paste(ds$rows, "rows,", ds$cols, "cols |", ds$timestamp)), comments_html),
              actionButton("load_dataset_trigger", label = "Load for Modeling", class = "btn-primary btn-xs",
                           onclick = paste0("Shiny.setInputValue('load_dataset_trigger', '", name, "', {priority: 'event'})")))
      )
    }))
  })
  
  output$pipeline_summary_ui <- renderUI({
    steps <- pipeline_steps()
    if(!is.list(steps) || length(steps) == 0) {
      return(p(icon("info-circle"), " No steps added yet. Click 'Add Step' to begin.", style = "color: grey;"))
    }
    step_elements <- list()
    for(i in seq_along(steps)) {
      step <- steps[[i]]
      method_name <- method_display_names[[step$method]] %||% (step$method %||% "Not selected")
      context <- ""
      if(!is.null(step$cols) && length(step$cols) > 0) {
        context <- paste0(" (", length(step$cols), " vars)")
      }
      if(step$method == "remove_na_rows" && !is.null(step$additional_info)) {
        context <- paste0(context, " | threshold = ", step$additional_info, " missing")
      }
      if(step$method == "remove_na_cols" && !is.null(step$additional_info)) {
        context <- paste0(context, " | threshold = ", step$additional_info, "%")
      }
      if(is.null(step$method) || step$method == "") {
        method_name <- "No method selected"
      }
      step_elements <- append(step_elements, list(
        div(style = "display: flex; align-items: center; margin-bottom: 10px; padding: 8px; border-bottom: 1px solid #eee; background-color: #f8f9fa; border-radius: 4px;",
            div(style = "width: 35px; font-weight: bold; color: #2C3E50;", paste0(i, ".")),
            div(style = "flex: 1;", span(style = "font-weight: 500;", method_name), span(style = "color: #666; font-size: 12px; margin-left: 5px;", context)))
      ))
    }
    tagList(step_elements)
  })
  
  output$proc_data_summary_recipe <- renderPrint({
    data <- processed_data_working()
    total_missing <- sum(is.na(data))
    total_cells <- nrow(data) * ncol(data)
    missing_pct <- round(100 * total_missing / total_cells, 2)
    cat("Current Data Summary\n====================\n\n")
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
  })
  
  output$proc_data_preview_recipe <- renderDT({
    data <- processed_data_working()
    datatable(head(data, 20), options = list(scrollX = TRUE, pageLength = 10, dom = 't'), rownames = FALSE)
  })
  
  output$proc_log_recipe <- renderPrint({
    log_entries <- processing_log()
    if(length(log_entries) == 0) {
      cat("No processing actions yet.\nBuild a recipe and click 'Apply Recipe' to see results.")
    } else {
      recent_log <- tail(log_entries, 30)
      cat(paste(recent_log, collapse = "\n"))
    }
  })
  
  output$proc_download_data_recipe <- downloadHandler(
    filename = function() { paste("processed_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(processed_data_working(), file, row.names = FALSE) }
  )
  
  # Load dataset for modeling
  observeEvent(input$load_dataset_trigger, {
    ds_name <- input$load_dataset_trigger
    if(!is.null(ds_name) && ds_name != "" && ds_name %in% names(saved_datasets())) {
      saved_ds <- saved_datasets()[[ds_name]]
      if(is.list(saved_ds) && !is.null(saved_ds$data)) {
        processed_data_working(saved_ds$data)
        selected_model_dataset(ds_name)
        updateRadioButtons(session, "model_data_source", selected = "saved")
        updatePickerInput(session, "model_dataset_select", choices = names(saved_datasets()), selected = ds_name)
        comments_msg <- if(!is.null(saved_ds$comments) && saved_ds$comments != "") paste0(" - Notes: ", saved_ds$comments) else ""
        showNotification(paste("Loaded dataset:", ds_name, "(", saved_ds$rows, "rows,", saved_ds$cols, "cols)", comments_msg), type = "message")
        add_to_log(paste("Loaded saved dataset:", ds_name, comments_msg))
      } else {
        showNotification(paste("Dataset", ds_name, "has invalid structure"), type = "error")
      }
    }
  })
  
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 12: INTRODUCTION TAB OUTPUTS
  # ========================================================================
  # ========================================================================
  
  output$intro_data_desc <- renderText({ 
    paste("Dataset contains", nrow(df), "rows and", ncol(df), "columns. ",
          "The outcome variable is DEATH_RATE (projected death rate across ten years). ",
          "There are", length(numeric_cols), "numeric predictors and", length(categorical_cols), 
          "categorical variables including the state CODE and OBS_TYPE for train/test allocation.")
  })
  
  output$intro_missing_strategy <- renderText({ 
    paste("Missing values are represented by '-99', '--', and 'NA' strings. ",
          "Users can remove columns exceeding missing thresholds, remove rows with too many missing values, ",
          "impute numeric missing values using median/mean/constant, ",
          "or impute categorical missing values using mode/new category/constant.")
  })
  
  output$intro_outlier_strategy <- renderText({ 
    paste("Outlier detection using multiple methods: Mahalanobis Distance, Cook's Distance, ",
          "Local Outlier Factor (LOF), One-Class SVM, Random Forest Residuals, and Isolation Forest. ",
          "Users can adjust thresholds and view consensus across methods.")
  })
  
  output$intro_preprocess_strategy <- renderText({ 
    paste("Data preprocessing includes missing value imputation, transformation options (log, sqrt, Box-Cox, Yeo-Johnson), ",
          "scaling/centering, and a pipeline builder for reproducible processing steps.")
  })
  
  output$intro_glmnet_explanation <- renderText({ 
    paste("GLMNET (Elastic Net) combines L1 (Lasso) and L2 (Ridge) regularization. ",
          "Alpha controls the mix (0 = Ridge, 1 = Lasso). ",
          "Cross-validation selects the optimal lambda parameter to balance bias and variance.")
  })
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 13: GLMNET MODELING OUTPUTS
  # ========================================================================
  # ========================================================================
  
  # Data Preparation and Train/Test Split
  observeEvent(
    list(input$model_data_source, input$model_dataset_select, processed_data_working()),
    {
      req(input$model_data_source)
      
      if(input$model_data_source == "saved") {
        ds_name <- input$model_dataset_select
        if(!is.null(ds_name) && ds_name != "" && ds_name %in% names(saved_datasets())) {
          data <- saved_datasets()[[ds_name]]$data
        } else {
          data <- standardize_missing(df)
          showNotification("No saved dataset selected. Using original data.", type = "warning")
        }
      } else {
        data <- standardize_missing(df)
      }
      
      if("DEATH_RATE" %in% names(data) && is.character(data$DEATH_RATE)) {
        data$DEATH_RATE[data$DEATH_RATE %in% c("-99", "--", "NA")] <- NA
        data$DEATH_RATE <- suppressWarnings(as.numeric(data$DEATH_RATE))
      }
      
      data <- data[!is.na(data$DEATH_RATE), ]
      
      if("OBS_TYPE" %in% names(data)) {
        train_idx <- which(trimws(as.character(data$OBS_TYPE)) == "Train")
        test_idx  <- which(trimws(as.character(data$OBS_TYPE)) == "Test")
        if(length(train_idx) == 0 || length(test_idx) == 0) {
          set.seed(123)
          train_idx <- sample(1:nrow(data), 0.7 * nrow(data))
          test_idx  <- setdiff(1:nrow(data), train_idx)
          showNotification("OBS_TYPE column missing Train/Test values. Using random 70/30 split.", type = "warning")
        }
      } else {
        set.seed(123)
        train_idx <- sample(1:nrow(data), 0.7 * nrow(data))
        test_idx  <- setdiff(1:nrow(data), train_idx)
      }
      
      train_test_split(list(train = data[train_idx, ], test = data[test_idx, ]))
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  
  # Model Training Observer
  observeEvent(input$train_model, {
    showNotification("Starting model training...", type = "message", duration = 3)
    
    if(is.null(train_test_split())) {
      showNotification("No train/test split available. Please check your data source.", type = "error")
      return()
    }
    
    split <- train_test_split()
    
    if(is.null(split) || is.null(split$train) || is.null(split$test)) {
      showNotification("Train/test split is invalid. Please check your data.", type = "error")
      return()
    }
    
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
    predictor_vars <- predictor_vars[is_numeric]
    
    if(length(predictor_vars) == 0) {
      showNotification("No valid numeric predictors found. Please check your data.", type = "error")
      return()
    }
    
    tryCatch({
      X_train <- as.matrix(train_data[, predictor_vars, drop = FALSE])
      y_train <- train_data$DEATH_RATE
      
      complete_train <- complete.cases(X_train, y_train)
      
      if(sum(complete_train) < 10) {
        showNotification(paste("Insufficient complete cases:", sum(complete_train), "Need at least 10."), type = "error")
        return()
      }
      
      X_train <- X_train[complete_train, , drop = FALSE]
      y_train <- y_train[complete_train]
      
      # Prepare test data
      X_test <- NULL
      y_test <- NULL
      
      if(!is.null(test_data) && nrow(test_data) > 0) {
        X_test <- tryCatch({
          as.matrix(test_data[, predictor_vars, drop = FALSE])
        }, error = function(e) NULL)
        
        if(!is.null(X_test)) {
          y_test <- test_data$DEATH_RATE
          complete_test <- complete.cases(X_test, y_test)
          if(sum(complete_test) > 0) {
            X_test <- X_test[complete_test, , drop = FALSE]
            y_test <- y_test[complete_test]
          } else {
            X_test <- NULL
            y_test <- NULL
          }
        }
      }
      
      set.seed(input$cv_seed %||% 123)
      alpha_val <- as.numeric(input$glmnet_alpha %||% 0.5)
      if(is.na(alpha_val)) alpha_val <- 0.5
      
      nfolds <- min(input$cv_folds %||% 5, nrow(X_train) - 1)
      if(nfolds < 3) nfolds <- 3
      
      cv_model <- cv.glmnet(X_train, y_train, alpha = alpha_val, nfolds = nfolds)
      
      lambda_input <- input$glmnet_lambda %||% 0
      if(is.null(lambda_input) || is.na(lambda_input) || lambda_input == "" || lambda_input == 0) {
        lambda_opt <- cv_model$lambda.min
      } else {
        lambda_opt <- as.numeric(lambda_input)
      }
      
      final_model <- glmnet(X_train, y_train, alpha = alpha_val, lambda = lambda_opt)
      
      train_pred <- as.numeric(predict(final_model, newx = X_train))
      train_rmse <- sqrt(mean((y_train - train_pred)^2))
      train_r2 <- 1 - sum((y_train - train_pred)^2) / sum((y_train - mean(y_train))^2)
      
      test_pred <- NULL
      test_rmse <- NA
      test_r2 <- NA
      
      if(!is.null(X_test) && length(y_test) > 0) {
        test_pred <- as.numeric(predict(final_model, newx = X_test))
        test_rmse <- sqrt(mean((y_test - test_pred)^2))
        test_r2 <- 1 - sum((y_test - test_pred)^2) / sum((y_test - mean(y_test))^2)
      }
      
      model_results(list(
        model = final_model,
        cv_model = cv_model,
        train_pred = train_pred,
        test_pred = test_pred,
        y_train = y_train,
        y_test = y_test,
        lambda_opt = lambda_opt,
        lambda_min = cv_model$lambda.min,
        lambda_1se = cv_model$lambda.1se,
        train_rmse = train_rmse,
        test_rmse = test_rmse,
        train_r2 = train_r2,
        test_r2 = test_r2,
        predictors = predictor_vars,
        data_source = input$model_data_source %||% "original"
      ))
      
      showNotification(paste("Model trained successfully! Test RMSE:", 
                             ifelse(is.na(test_rmse), "N/A", round(test_rmse, 4))), 
                       type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("Model training error:", e$message), type = "error", duration = 8)
    })
  })
  
  # Model Performance Outputs
  output$model_performance <- renderPrint({
    res <- model_results()
    if(is.null(res)) { cat("No model trained yet.\n\nSelect data source and click 'Train GLMNET Model'"); return() }
    
    cat("GLMNET Model Performance\n========================\n\n")
    cat("Predictors:", length(res$predictors), "\n")
    cat("Training observations:", length(res$y_train), "\n")
    if(!is.null(res$y_test) && length(res$y_test) > 0) cat("Test observations:", length(res$y_test), "\n")
    cat("\nTRAIN SET:\n  RMSE:", round(res$train_rmse, 4), "\n  R²:", round(res$train_r2, 4), "\n\n")
    cat("TEST SET:\n")
    if(!is.na(res$test_rmse)) cat("  RMSE:", round(res$test_rmse, 4), "\n  R²:", round(res$test_r2, 4), "\n")
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
    
    datatable(coef_df, 
              options = list(pageLength = nrow(coef_df), scrollX = TRUE, dom = 't'), 
              rownames = FALSE) %>%
      formatRound("Coefficient", digits = 4)
  })
  
  output$coef_plot <- renderPlotly({
    res <- model_results()
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "No model trained"))
    
    coef_matrix <- as.matrix(coef(res$model))
    coef_df <- data.frame(Variable = rownames(coef_matrix), Coefficient = as.numeric(coef_matrix[, 1]))
    coef_df <- coef_df[coef_df$Variable != "(Intercept)", ]
    coef_df <- coef_df[order(coef_df$Coefficient), ]
    
    colors <- ifelse(coef_df$Coefficient > 0, "#13D4D4", "#e74c3c")
    
    plot_ly(coef_df, x = ~Coefficient, y = ~reorder(Variable, Coefficient), type = "bar", orientation = "h",
            marker = list(color = colors), text = ~paste(Variable, ": ", round(Coefficient, 4)), hoverinfo = "text") %>%
      layout(title = "Coefficient Estimates", xaxis = list(title = "Coefficient Value"),
             yaxis = list(title = ""), margin = list(l = 120))
  })
  
  output$predictions_plot <- renderPlotly({
    res <- model_results()
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "No model trained"))
    
    split <- train_test_split()
    if(is.null(split)) return(plot_ly() %>% add_annotations(text = "No train/test split"))
    
    dataset_choice <- input$predictions_dataset %||% "both"
    plot_df <- data.frame()
    
    if(dataset_choice %in% c("train", "both")) {
      train_codes <- tryCatch({
        complete_idx <- complete.cases(split$train[, res$predictors, drop = FALSE])
        as.character(split$train$CODE[complete_idx])
      }, error = function(e) rep("Unknown", length(res$train_pred)))
      
      plot_df <- rbind(plot_df, data.frame(
        Actual = res$y_train, Predicted = res$train_pred, Set = "Train", CODE = train_codes
      ))
    }
    
    if(dataset_choice %in% c("test", "both") && !is.null(res$test_pred) && length(res$test_pred) > 0) {
      test_codes <- tryCatch({
        complete_idx <- complete.cases(split$test[, res$predictors, drop = FALSE])
        as.character(split$test$CODE[complete_idx])
      }, error = function(e) rep("Unknown", length(res$test_pred)))
      
      plot_df <- rbind(plot_df, data.frame(
        Actual = res$y_test, Predicted = res$test_pred, Set = "Test", CODE = test_codes
      ))
    }
    
    plot_df <- na.omit(plot_df)
    if(nrow(plot_df) == 0) return(plot_ly() %>% add_annotations(text = "No valid data"))
    
    p <- ggplot(plot_df, aes(x = Actual, y = Predicted, color = Set,
                             text = paste("CODE:", CODE, "<br>Actual:", round(Actual, 3),
                                          "<br>Predicted:", round(Predicted, 3)))) +
      geom_point(alpha = 0.6, size = 2.5) + 
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = c("Train" = "#13D4D4", "Test" = "#e74c3c")) +
      labs(title = "Predictions vs Actual Values", x = "Actual Death Rate", y = "Predicted Death Rate") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$predictions_summary <- renderPrint({
    res <- model_results()
    if(is.null(res)) { 
      cat("No model trained yet.\n\nSelect data source and click 'Train GLMNET Model'")
      return() 
    }
    
    cat("Predictions Summary\n==================\n\n")
    cat("TRAIN SET:\n")
    cat("  Min:", round(min(res$train_pred), 4), "\n")
    cat("  Max:", round(max(res$train_pred), 4), "\n")
    cat("  Mean:", round(mean(res$train_pred), 4), "\n")
    cat("  SD:", round(sd(res$train_pred), 4), "\n\n")
    
    if(!is.null(res$test_pred) && length(res$test_pred) > 0) {
      cat("TEST SET:\n")
      cat("  Min:", round(min(res$test_pred), 4), "\n")
      cat("  Max:", round(max(res$test_pred), 4), "\n")
      cat("  Mean:", round(mean(res$test_pred), 4), "\n")
      cat("  SD:", round(sd(res$test_pred), 4), "\n")
    } else { 
      cat("TEST SET: No predictions available\n") 
    }
  })
  
  output$residual_plot <- renderPlotly({
    res <- model_results()
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "No model trained"))
    
    split <- train_test_split()
    if(is.null(split)) return(plot_ly() %>% add_annotations(text = "No train/test split"))
    
    dataset_choice <- input$residual_display_dataset %||% "both"
    plot_df <- data.frame()
    
    if(dataset_choice %in% c("train", "both")) {
      train_residuals <- res$y_train - res$train_pred
      train_codes <- tryCatch({
        complete_idx <- complete.cases(split$train[, res$predictors, drop = FALSE])
        as.character(split$train$CODE[complete_idx])
      }, error = function(e) rep("Unknown", length(train_residuals)))
      
      plot_df <- rbind(plot_df, data.frame(
        Residuals = train_residuals, Set = "Train", Predicted = res$train_pred, CODE = train_codes
      ))
    }
    
    if(dataset_choice %in% c("test", "both") && !is.null(res$test_pred) && length(res$test_pred) > 0) {
      test_residuals <- res$y_test - res$test_pred
      test_codes <- tryCatch({
        complete_idx <- complete.cases(split$test[, res$predictors, drop = FALSE])
        as.character(split$test$CODE[complete_idx])
      }, error = function(e) rep("Unknown", length(test_residuals)))
      
      plot_df <- rbind(plot_df, data.frame(
        Residuals = test_residuals, Set = "Test", Predicted = res$test_pred, CODE = test_codes
      ))
    }
    
    plot_df <- na.omit(plot_df)
    if(nrow(plot_df) == 0) return(plot_ly() %>% add_annotations(text = "No valid data"))
    
    p <- ggplot(plot_df, aes(x = Predicted, y = Residuals, color = Set,
                             text = paste("CODE:", CODE, "<br>Predicted:", round(Predicted, 3),
                                          "<br>Residual:", round(Residuals, 3)))) +
      geom_point(alpha = 0.6, size = 2) + 
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = c("Train" = "#13D4D4", "Test" = "#e74c3c")) +
      labs(title = "Residuals vs Fitted Values", x = "Predicted Death Rate", y = "Residuals") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$residual_stats <- renderPrint({
    res <- model_results()
    if(is.null(res)) { 
      cat("No model trained yet.")
      return() 
    }
    
    train_residuals <- res$y_train - res$train_pred
    cat("Residual Analysis\n=================\n\n")
    
    cat("TRAIN SET:\n")
    cat("  Min:", round(min(train_residuals), 4), "\n")
    cat("  Max:", round(max(train_residuals), 4), "\n")
    cat("  Mean:", round(mean(train_residuals), 4), "\n")
    cat("  SD:", round(sd(train_residuals), 4), "\n")
    cat("  Q1:", round(quantile(train_residuals, 0.25), 4), "\n")
    cat("  Median:", round(median(train_residuals), 4), "\n")
    cat("  Q3:", round(quantile(train_residuals, 0.75), 4), "\n\n")
    
    if(!is.null(res$test_pred) && length(res$test_pred) > 0) {
      test_residuals <- res$y_test - res$test_pred
      cat("TEST SET:\n")
      cat("  Min:", round(min(test_residuals), 4), "\n")
      cat("  Max:", round(max(test_residuals), 4), "\n")
      cat("  Mean:", round(mean(test_residuals), 4), "\n")
      cat("  SD:", round(sd(test_residuals), 4), "\n")
      cat("  Q1:", round(quantile(test_residuals, 0.25), 4), "\n")
      cat("  Median:", round(median(test_residuals), 4), "\n")
      cat("  Q3:", round(quantile(test_residuals, 0.75), 4), "\n")
    }
  })
  
  output$residual_outliers <- renderDT({
    res <- model_results()
    if(is.null(res) || is.null(res$test_pred) || length(res$test_pred) == 0) {
      return(datatable(data.frame(Message = "No test predictions available. Train a model first.")))
    }
    
    residuals <- res$y_test - res$test_pred
    Q1 <- quantile(residuals, 0.25, na.rm = TRUE)
    Q3 <- quantile(residuals, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    iqr_mult_val <- input$residual_iqr %||% 1.5
    lower_bound <- Q1 - iqr_mult_val * IQR
    upper_bound <- Q3 + iqr_mult_val * IQR
    outlier_idx <- which(residuals < lower_bound | residuals > upper_bound)
    
    if(length(outlier_idx) == 0) {
      return(datatable(data.frame(Message = paste("No outliers detected with IQR multiplier =", iqr_mult_val))))
    }
    
    split <- train_test_split()
    if(!is.null(split) && !is.null(split$test)) {
      test_data <- split$test
      predictor_vars <- res$predictors
      complete_test <- complete.cases(test_data[, predictor_vars, drop = FALSE])
      test_data_complete <- test_data[complete_test, ]
      
      outlier_codes <- test_data_complete$CODE[outlier_idx]
      if(is.null(outlier_codes) || length(outlier_codes) != length(outlier_idx)) {
        outlier_codes <- outlier_idx
      }
    } else {
      outlier_codes <- outlier_idx
    }
    
    outlier_df <- data.frame(
      CODE = outlier_codes,
      Residual = round(residuals[outlier_idx], 4),
      Predicted = round(res$test_pred[outlier_idx], 4), 
      Actual = round(res$y_test[outlier_idx], 4),
      Std_Residual = round(residuals[outlier_idx] / sd(residuals, na.rm = TRUE), 3)
    )
    
    datatable(outlier_df, options = list(scrollX = TRUE, dom = 'frtip'), rownames = FALSE,
              caption = paste("Total outliers:", length(outlier_idx), "(", round(100 * length(outlier_idx) / length(residuals), 1), "% of test data)"))
  })
  
  output$residual_boxplot <- renderPlotly({
    res <- model_results()
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "No model trained"))
    
    split <- train_test_split()
    if(is.null(split)) return(plot_ly() %>% add_annotations(text = "No train/test split"))
    
    dataset_choice <- input$residual_dataset %||% "both"
    iqr_mult <- input$residual_boxplot_iqr %||% 1.5
    
    plot_data <- data.frame()
    
    if(dataset_choice %in% c("train", "both")) {
      train_residuals <- res$y_train - res$train_pred
      train_codes <- tryCatch({
        complete_idx <- complete.cases(split$train[, res$predictors, drop = FALSE])
        as.character(split$train$CODE[complete_idx])
      }, error = function(e) rep("Unknown", length(train_residuals)))
      
      train_df <- data.frame(
        Residual = train_residuals,
        Dataset = "Train",
        CODE = train_codes,
        Predicted = res$train_pred,
        Actual = res$y_train
      )
      plot_data <- rbind(plot_data, train_df)
    }
    
    if(dataset_choice %in% c("test", "both") && !is.null(res$test_pred) && length(res$test_pred) > 0) {
      test_residuals <- res$y_test - res$test_pred
      test_codes <- tryCatch({
        complete_idx <- complete.cases(split$test[, res$predictors, drop = FALSE])
        as.character(split$test$CODE[complete_idx])
      }, error = function(e) rep("Unknown", length(test_residuals)))
      
      test_df <- data.frame(
        Residual = test_residuals,
        Dataset = "Test",
        CODE = test_codes,
        Predicted = res$test_pred,
        Actual = res$y_test
      )
      plot_data <- rbind(plot_data, test_df)
    }
    
    plot_data <- na.omit(plot_data)
    if(nrow(plot_data) == 0) return(plot_ly() %>% add_annotations(text = "No valid data"))
    
    plot_data$IsOutlier <- FALSE
    for(ds in unique(plot_data$Dataset)) {
      ds_idx <- plot_data$Dataset == ds
      if(sum(ds_idx) > 0) {
        Q1 <- quantile(plot_data$Residual[ds_idx], 0.25, na.rm = TRUE)
        Q3 <- quantile(plot_data$Residual[ds_idx], 0.75, na.rm = TRUE)
        IQR_val <- Q3 - Q1
        lower_bound <- Q1 - iqr_mult * IQR_val
        upper_bound <- Q3 + iqr_mult * IQR_val
        plot_data$IsOutlier[ds_idx] <- plot_data$Residual[ds_idx] < lower_bound | 
          plot_data$Residual[ds_idx] > upper_bound
      }
    }
    
    p <- ggplot(plot_data, aes(x = Dataset, y = Residual, fill = Dataset,
                               text = paste("CODE:", CODE, "<br>Dataset:", Dataset,
                                            "<br>Residual:", round(Residual, 4),
                                            "<br>Predicted:", round(Predicted, 4),
                                            "<br>Actual:", round(Actual, 4),
                                            "<br>Outlier:", IsOutlier))) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      geom_point(aes(color = IsOutlier), position = position_jitter(width = 0.2), 
                 size = 2, alpha = 0.6) +
      scale_color_manual(values = c("FALSE" = "#2C3E50", "TRUE" = "#e74c3c")) +
      scale_fill_manual(values = c("Train" = "#13D4D4", "Test" = "#e74c3c")) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(title = paste("Residual Distribution -", 
                         switch(dataset_choice, train = "Train Set", test = "Test Set", both = "Train + Test Sets")),
           subtitle = paste("IQR Multiplier:", iqr_mult, "| Total outliers:", sum(plot_data$IsOutlier)),
           y = "Residuals", x = "Dataset") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% 
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 10)))
  })
  
  output$residual_boxplot_stats <- renderPrint({
    res <- model_results()
    if(is.null(res)) { 
      cat("No model trained yet.")
      return() 
    }
    
    split <- train_test_split()
    if(is.null(split)) { 
      cat("No train/test split available.")
      return() 
    }
    
    dataset_choice <- input$residual_dataset %||% "both"
    iqr_mult <- input$residual_boxplot_iqr %||% 1.5
    
    cat("Residual Boxplot Statistics\n")
    cat("===========================\n\n")
    cat("IQR Multiplier:", iqr_mult, "\n\n")
    
    if(dataset_choice %in% c("train", "both")) {
      train_residuals <- res$y_train - res$train_pred
      Q1_train <- quantile(train_residuals, 0.25, na.rm = TRUE)
      Q3_train <- quantile(train_residuals, 0.75, na.rm = TRUE)
      IQR_train <- Q3_train - Q1_train
      lower_train <- Q1_train - iqr_mult * IQR_train
      upper_train <- Q3_train + iqr_mult * IQR_train
      outliers_train <- sum(train_residuals < lower_train | train_residuals > upper_train)
      
      cat("TRAIN SET:\n")
      cat("  Observations:", length(train_residuals), "\n")
      cat("  Median:", round(median(train_residuals), 4), "\n")
      cat("  Q1:", round(Q1_train, 4), "\n")
      cat("  Q3:", round(Q3_train, 4), "\n")
      cat("  IQR:", round(IQR_train, 4), "\n")
      cat("  Lower whisker:", round(lower_train, 4), "\n")
      cat("  Upper whisker:", round(upper_train, 4), "\n")
      cat("  Outliers:", outliers_train, "(", round(100 * outliers_train / length(train_residuals), 2), "%)\n\n")
    }
    
    if(dataset_choice %in% c("test", "both") && !is.null(res$test_pred) && length(res$test_pred) > 0) {
      test_residuals <- res$y_test - res$test_pred
      Q1_test <- quantile(test_residuals, 0.25, na.rm = TRUE)
      Q3_test <- quantile(test_residuals, 0.75, na.rm = TRUE)
      IQR_test <- Q3_test - Q1_test
      lower_test <- Q1_test - iqr_mult * IQR_test
      upper_test <- Q3_test + iqr_mult * IQR_test
      outliers_test <- sum(test_residuals < lower_test | test_residuals > upper_test)
      
      cat("TEST SET:\n")
      cat("  Observations:", length(test_residuals), "\n")
      cat("  Median:", round(median(test_residuals), 4), "\n")
      cat("  Q1:", round(Q1_test, 4), "\n")
      cat("  Q3:", round(Q3_test, 4), "\n")
      cat("  IQR:", round(IQR_test, 4), "\n")
      cat("  Lower whisker:", round(lower_test, 4), "\n")
      cat("  Upper whisker:", round(upper_test, 4), "\n")
      cat("  Outliers:", outliers_test, "(", round(100 * outliers_test / length(test_residuals), 2), "%)\n\n")
    }
  })
  
  output$residual_boxplot_outliers <- renderDT({
    res <- model_results()
    if(is.null(res)) {
      return(datatable(data.frame(Message = "No model trained yet. Train a model first.")))
    }
    
    split <- train_test_split()
    if(is.null(split)) {
      return(datatable(data.frame(Message = "No train/test split available.")))
    }
    
    dataset_choice <- input$residual_dataset %||% "both"
    iqr_mult <- input$residual_boxplot_iqr %||% 1.5
    
    outlier_data <- data.frame()
    
    if(dataset_choice %in% c("train", "both")) {
      train_residuals <- res$y_train - res$train_pred
      Q1_train <- quantile(train_residuals, 0.25, na.rm = TRUE)
      Q3_train <- quantile(train_residuals, 0.75, na.rm = TRUE)
      IQR_train <- Q3_train - Q1_train
      lower_train <- Q1_train - iqr_mult * IQR_train
      upper_train <- Q3_train + iqr_mult * IQR_train
      train_outlier_idx <- which(train_residuals < lower_train | train_residuals > upper_train)
      
      if(length(train_outlier_idx) > 0) {
        train_codes <- tryCatch({
          complete_idx <- complete.cases(split$train[, res$predictors, drop = FALSE])
          as.character(split$train$CODE[complete_idx])
        }, error = function(e) rep("Unknown", length(train_residuals)))
        
        train_outlier_df <- data.frame(
          CODE = train_codes[train_outlier_idx],
          Dataset = "Train",
          Residual = round(train_residuals[train_outlier_idx], 4),
          Predicted = round(res$train_pred[train_outlier_idx], 4),
          Actual = round(res$y_train[train_outlier_idx], 4)
        )
        outlier_data <- rbind(outlier_data, train_outlier_df)
      }
    }
    
    if(dataset_choice %in% c("test", "both") && !is.null(res$test_pred) && length(res$test_pred) > 0) {
      test_residuals <- res$y_test - res$test_pred
      Q1_test <- quantile(test_residuals, 0.25, na.rm = TRUE)
      Q3_test <- quantile(test_residuals, 0.75, na.rm = TRUE)
      IQR_test <- Q3_test - Q1_test
      lower_test <- Q1_test - iqr_mult * IQR_test
      upper_test <- Q3_test + iqr_mult * IQR_test
      test_outlier_idx <- which(test_residuals < lower_test | test_residuals > upper_test)
      
      if(length(test_outlier_idx) > 0) {
        test_codes <- tryCatch({
          complete_idx <- complete.cases(split$test[, res$predictors, drop = FALSE])
          as.character(split$test$CODE[complete_idx])
        }, error = function(e) rep("Unknown", length(test_residuals)))
        
        test_outlier_df <- data.frame(
          CODE = test_codes[test_outlier_idx],
          Dataset = "Test",
          Residual = round(test_residuals[test_outlier_idx], 4),
          Predicted = round(res$test_pred[test_outlier_idx], 4),
          Actual = round(res$y_test[test_outlier_idx], 4)
        )
        outlier_data <- rbind(outlier_data, test_outlier_df)
      }
    }
    
    if(nrow(outlier_data) == 0) {
      return(datatable(data.frame(Message = paste("No outliers detected with IQR multiplier =", iqr_mult))))
    }
    
    outlier_data <- outlier_data[order(abs(outlier_data$Residual), decreasing = TRUE), ]
    
    datatable(outlier_data, options = list(scrollX = TRUE, dom = 'frtip'), rownames = FALSE,
              caption = paste("Total outliers:", nrow(outlier_data)))
  })
  
  output$cv_plot <- renderPlotly({
    res <- model_results()
    if(is.null(res) || is.null(res$cv_model)) {
      return(plot_ly() %>% add_annotations(text = "No CV model available"))
    }
    
    cv_df <- data.frame(log_lambda = log(res$cv_model$lambda), cvm = res$cv_model$cvm,
                        cvup = res$cv_model$cvup, cvlo = res$cv_model$cvlo)
    
    p <- ggplot(cv_df, aes(x = log_lambda, y = cvm)) +
      geom_ribbon(aes(ymin = cvlo, ymax = cvup), alpha = 0.3, fill = "#13D4D4") +
      geom_line(color = "#13D4D4", size = 1) + geom_point(size = 1.5) +
      geom_vline(xintercept = log(res$lambda_min), linetype = "dashed", color = "#e74c3c") +
      geom_vline(xintercept = log(res$lambda_1se), linetype = "dotted", color = "#13D4D4") +
      labs(title = "Cross-Validation Error", x = "log(Lambda)", y = "CV Error (MSE)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  # Cross-Validation Summary
  output$cv_summary <- renderPrint({
    res <- model_results()
    if(is.null(res) || is.null(res$cv_model)) { 
      cat("No CV model available. Train a model first.") 
      return() 
    }
    
    cat("Cross-Validation Summary\n")
    cat("========================\n\n")
    cat("Number of folds:", input$cv_folds, "\n")
    cat("Lambda.min (minimum CV error):", round(res$lambda_min, 6), "\n")
    cat("Lambda.1se (1-standard error rule):", round(res$lambda_1se, 6), "\n")
    cat("Selected lambda:", round(res$lambda_opt, 6), "\n\n")
    cat("CV Error at lambda.min:", round(min(res$cv_model$cvm), 6), "\n")
    cat("CV Error at lambda.1se:", round(res$cv_model$cvm[which(res$cv_model$lambda == res$lambda_1se)], 6), "\n")
  })
  
  # Tuning CV Summary
  output$tune_cv_summary <- renderPrint({
    res <- tuning_results()
    if(is.null(res)) {
      cat("No tuning results available.\nClick 'Run Hyperparameter Tuning' to start.")
      return()
    }
    
    cat("HYPERPARAMETER TUNING SUMMARY\n")
    cat("=============================\n\n")
    cat("Data Information:\n")
    cat("  Complete observations:", res$n_complete, "\n")
    cat("  Predictor variables:", res$n_predictors, "\n\n")
    cat("Tuning Grid:\n")
    cat("  Alpha values:", length(res$alpha_values), 
        "(from", min(res$alpha_values), "to", max(res$alpha_values), ")\n")
    cat("  Lambda values:", length(res$lambda_values), 
        "(from", round(min(res$lambda_values), 6), 
        "to", round(max(res$lambda_values), 6), ")\n")
    cat("  Total combinations tested:", nrow(res$grid), "\n\n")
    cat("Cross-Validation:\n")
    cat("  Number of folds:", input$tune_cv_folds, "\n")
    cat("  Random seed:", input$tune_cv_seed, "\n")
    cat("  Optimization metric:", res$metric_name, "\n\n")
    cat("BEST PARAMETERS:\n")
    cat("  Alpha:", round(res$best_alpha, 4), "\n")
    cat("  Lambda:", round(res$best_lambda, 6), "\n")
    cat("  Best", res$metric_name, ":", round(res$best_metric, 4), "\n")
  })
  
  # Top Results Table
  output$tune_top_results <- renderDT({
    res <- tuning_results()
    if(is.null(res)) {
      return(datatable(data.frame(Message = "Run tuning first")))
    }
    
    top_n <- min(20, nrow(res$grid))
    
    if(res$metric_name %in% c("RMSE", "MAE")) {
      top_results <- res$grid[order(res$grid$metric_value), ][1:top_n, ]
    } else {
      top_results <- res$grid[order(-res$grid$metric_value), ][1:top_n, ]
    }
    
    colnames(top_results) <- c("Alpha", "Lambda", res$metric_name)
    top_results$Lambda <- round(top_results$Lambda, 6)
    top_results[, 3] <- round(top_results[, 3], 4)
    
    datatable(top_results, 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE,
              caption = paste("Top", top_n, "parameter combinations by", res$metric_name))
  })
  
  
  
  
  
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 14: HYPERPARAMETER TUNING
  # ========================================================================
  # ========================================================================
  
  tuning_results <- reactiveVal(NULL)
  
  output$tuning_complete <- reactive({ !is.null(tuning_results()) })
  outputOptions(output, "tuning_complete", suspendWhenHidden = FALSE)
  
  observeEvent(input$run_tuning, {
    if(input$model_data_source == "saved" && !is.null(selected_model_dataset()) && 
       selected_model_dataset() %in% names(saved_datasets())) {
      data <- saved_datasets()[[selected_model_dataset()]]$data
    } else {
      data <- standardize_missing(df)
    }
    
    if("DEATH_RATE" %in% names(data) && is.character(data$DEATH_RATE)) {
      data$DEATH_RATE[data$DEATH_RATE %in% c("-99", "--", "NA")] <- NA
      data$DEATH_RATE <- suppressWarnings(as.numeric(data$DEATH_RATE))
    }
    
    data <- data[!is.na(data$DEATH_RATE), ]
    
    predictor_vars <- setdiff(names(data), c("DEATH_RATE", "CODE", "OBS_TYPE"))
    is_numeric <- sapply(data[, predictor_vars, drop = FALSE], is.numeric)
    predictor_vars <- predictor_vars[is_numeric]
    
    if(length(predictor_vars) == 0) {
      showNotification("No valid numeric predictors found for tuning.", type = "error")
      return()
    }
    
    X <- as.matrix(data[, predictor_vars, drop = FALSE])
    y <- data$DEATH_RATE
    
    complete_idx <- complete.cases(X, y)
    if(sum(complete_idx) < 10) {
      showNotification(paste("Insufficient complete cases:", sum(complete_idx)), type = "error")
      return()
    }
    
    X <- X[complete_idx, , drop = FALSE]
    y <- y[complete_idx]
    
    alpha_values <- seq(input$tune_alpha_min, input$tune_alpha_max, length.out = input$tune_alpha_steps)
    lambda_values <- exp(seq(input$tune_lambda_min, input$tune_lambda_max, length.out = input$tune_lambda_steps))
    
    showNotification(paste("Starting tuning with", length(alpha_values), "alpha values and",
                           length(lambda_values), "lambda values..."), type = "message", duration = 3)
    
    progress <- shiny::Progress$new()
    progress$set(message = "Tuning hyperparameters", value = 0)
    on.exit(progress$close())
    
    results_grid <- expand.grid(alpha = alpha_values, lambda = lambda_values, stringsAsFactors = FALSE)
    total_combinations <- nrow(results_grid)
    results_grid$metric_value <- NA
    
    set.seed(input$tune_cv_seed)
    
    for(i in 1:total_combinations) {
      progress$set(value = i / total_combinations, detail = paste("Testing", i, "of", total_combinations))
      
      alpha_val <- results_grid$alpha[i]
      lambda_val <- results_grid$lambda[i]
      
      cv_folds <- createFolds(y, k = min(input$tune_cv_folds, sum(complete_idx) - 1), list = TRUE, returnTrain = FALSE)
      fold_metrics <- c()
      
      for(fold in seq_along(cv_folds)) {
        test_idx <- cv_folds[[fold]]
        train_idx <- setdiff(1:length(y), test_idx)
        
        model <- tryCatch({
          glmnet(X[train_idx, , drop = FALSE], y[train_idx], alpha = alpha_val, lambda = lambda_val)
        }, error = function(e) NULL)
        
        if(!is.null(model)) {
          pred <- predict(model, newx = X[test_idx, , drop = FALSE])
          
          if(input$tune_metric == "RMSE") {
            metric <- sqrt(mean((y[test_idx] - pred)^2))
          } else if(input$tune_metric == "MAE") {
            metric <- mean(abs(y[test_idx] - pred))
          } else {
            ss_res <- sum((y[test_idx] - pred)^2)
            ss_tot <- sum((y[test_idx] - mean(y[test_idx]))^2)
            metric <- 1 - ss_res / ss_tot
          }
          fold_metrics <- c(fold_metrics, metric)
        }
      }
      
      if(length(fold_metrics) > 0) {
        results_grid$metric_value[i] <- mean(fold_metrics, na.rm = TRUE)
      }
    }
    
    if(input$tune_metric %in% c("RMSE", "MAE")) {
      best_idx <- which.min(results_grid$metric_value)
    } else {
      best_idx <- which.max(results_grid$metric_value)
    }
    
    tuning_results(list(
      grid = results_grid,
      best_alpha = results_grid$alpha[best_idx],
      best_lambda = results_grid$lambda[best_idx],
      best_metric = results_grid$metric_value[best_idx],
      metric_name = input$tune_metric,
      alpha_values = alpha_values,
      lambda_values = lambda_values,
      n_complete = sum(complete_idx),
      n_predictors = length(predictor_vars)
    ))
    
    showNotification(paste("Tuning complete! Best", input$tune_metric, "achieved with alpha =",
                           round(results_grid$alpha[best_idx], 3), "and lambda =", round(results_grid$lambda[best_idx], 6)),
                     type = "message", duration = 5)
  })
  
  output$best_alpha_display <- renderText({ if(is.null(tuning_results())) "—" else round(tuning_results()$best_alpha, 4) })
  output$best_lambda_display <- renderText({ if(is.null(tuning_results())) "—" else round(tuning_results()$best_lambda, 6) })
  output$best_metric_display <- renderText({ if(is.null(tuning_results())) "—" else round(tuning_results()$best_metric, 4) })
  
  output$tune_alpha_plot <- renderPlotly({
    res <- tuning_results()
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "Run tuning first"))
    
    alpha_summary <- res$grid %>% group_by(alpha) %>%
      summarise(best_metric = if(res$metric_name %in% c("RMSE", "MAE")) min(metric_value) else max(metric_value))
    
    p <- ggplot(alpha_summary, aes(x = alpha, y = best_metric)) +
      geom_line(color = "#13D4D4", size = 1.5) + geom_point(size = 3, color = "#13D4D4") +
      geom_vline(xintercept = res$best_alpha, linetype = "dashed", color = "#e74c3c") +
      labs(title = paste("Best", res$metric_name, "vs Alpha"), x = "Alpha", y = res$metric_name) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$tune_lambda_plot <- renderPlotly({
    res <- tuning_results()
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "Run tuning first"))
    
    lambda_subset <- res$grid[abs(res$grid$alpha - res$best_alpha) < 0.001, ]
    if(nrow(lambda_subset) == 0) return(plot_ly() %>% add_annotations(text = "No data"))
    
    p <- ggplot(lambda_subset, aes(x = log(lambda), y = metric_value)) +
      geom_line(color = "#13D4D4", size = 1.5) + geom_point(size = 2, color = "#13D4D4") +
      geom_vline(xintercept = log(res$best_lambda), linetype = "dashed", color = "#e74c3c") +
      labs(title = paste("Best", res$metric_name, "vs Lambda"), x = "log(Lambda)", y = res$metric_name) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$tune_heatmap <- renderPlotly({
    res <- tuning_results()
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "Run tuning first"))
    
    heatmap_data <- matrix(NA, nrow = length(res$alpha_values), ncol = length(res$lambda_values))
    rownames(heatmap_data) <- round(res$alpha_values, 3)
    colnames(heatmap_data) <- round(log(res$lambda_values), 3)
    
    for(i in 1:nrow(res$grid)) {
      alpha_idx <- which.min(abs(res$alpha_values - res$grid$alpha[i]))
      lambda_idx <- which.min(abs(res$lambda_values - res$grid$lambda[i]))
      heatmap_data[alpha_idx, lambda_idx] <- res$grid$metric_value[i]
    }
    
    if(res$metric_name %in% c("RMSE", "MAE")) {
      colorscale <- list(list(0, "#e74c3c"), list(0.5, "#f39c12"), list(1, "#13D4D4"))
    } else {
      colorscale <- list(list(0, "#e74c3c"), list(0.5, "#f39c12"), list(1, "#13D4D4"))
    }
    
    plot_ly(z = heatmap_data, x = as.numeric(colnames(heatmap_data)), y = as.numeric(rownames(heatmap_data)),
            type = "heatmap", colorscale = colorscale,
            hovertemplate = "<b>Alpha: %{y:.3f}</b><br><b>log(Lambda): %{x:.3f}</b><br><b>Value: %{z:.4f}</b><extra></extra>") %>%
      layout(title = "Hyperparameter Tuning Heatmap", xaxis = list(title = "log(Lambda)"), yaxis = list(title = "Alpha"))
  })
  
  observeEvent(input$use_tuned_params, {
    res <- tuning_results()
    if(is.null(res)) {
      showNotification("No tuning results available. Run tuning first.", type = "warning")
      return()
    }
    updateSliderInput(session, "glmnet_alpha", value = res$best_alpha)
    updateNumericInput(session, "glmnet_lambda", value = res$best_lambda)
    showNotification(paste("Updated parameters: Alpha =", round(res$best_alpha, 4), ", Lambda =", round(res$best_lambda, 6)), type = "message")
  })
  
  
  
  # ========================================================================
  # ========================================================================
  # SECTION 15: SYNC HTML5 SLIDERS WITH SHINY INPUTS
  # ========================================================================
  # ========================================================================
  
  if(!exists("runjs")) runjs <- shinyjs::runjs
  
  observeEvent(input$glmnet_alpha_slider, { updateNumericInput(session, "glmnet_alpha", value = input$glmnet_alpha_slider) })
  observeEvent(input$residual_iqr_slider, { updateNumericInput(session, "residual_iqr", value = input$residual_iqr_slider) })
  observeEvent(input$residual_boxplot_iqr_slider, { updateNumericInput(session, "residual_boxplot_iqr", value = input$residual_boxplot_iqr_slider) })
  
  observeEvent(input$glmnet_alpha, { runjs(paste0("document.getElementById('glmnet_alpha_slider').value = ", input$glmnet_alpha)) })
  observeEvent(input$residual_iqr, { runjs(paste0("document.getElementById('residual_iqr_slider').value = ", input$residual_iqr)) })
  observeEvent(input$residual_boxplot_iqr, { runjs(paste0("document.getElementById('residual_boxplot_iqr_slider').value = ", input$residual_boxplot_iqr)) })
  
}  # Close server function