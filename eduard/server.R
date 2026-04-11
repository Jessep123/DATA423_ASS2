# server.R - COMPLETE VERSION WITH ALL FUNCTIONALITY

server <- function(input, output, session) {
  
  # ============================================================================
  # ============================================================================
  # SECTION 1: HELPER FUNCTIONS
  # ============================================================================
  # ============================================================================
  
  # Safe NULL handler for missing value checks
  safe_in <- function(value, vector) {
    if(is.null(vector)) return(FALSE)
    return(value %in% vector)
  }
  
  # ----------------------------------------------------------------------------
  # Missing Value Standardization
  # ----------------------------------------------------------------------------
  
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
  
  # ----------------------------------------------------------------------------
  # Categorical Filter Application
  # ----------------------------------------------------------------------------
  
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
  
  # ----------------------------------------------------------------------------
  # Center and Scale for Boxplot
  # ----------------------------------------------------------------------------
  
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
  
  # ----------------------------------------------------------------------------
  # Outlier Flag Addition
  # ----------------------------------------------------------------------------
  
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
  
  
  # ----------------------------------------------------------------------------
  # Remove outliers using IQR method
  # ----------------------------------------------------------------------------
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
  
  # ----------------------------------------------------------------------------
  # Remove outliers using Mahalanobis distance
  # ----------------------------------------------------------------------------
  remove_outliers_mahalanobis <- function(data, threshold_prob = 0.999) {
    # Get numeric columns only
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    numeric_data <- numeric_data[, !names(numeric_data) %in% c("CODE", "OBS_TYPE"), drop = FALSE]
    
    if(ncol(numeric_data) < 2) {
      return(list(data = data, removed = 0, message = "Need at least 2 numeric variables"))
    }
    
    # Remove rows with NA
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
    
    # Map back to original data
    outlier_indices <- which(complete_idx)[outlier_rows_md]
    outlier_flag <- rep(FALSE, nrow(data))
    outlier_flag[outlier_indices] <- TRUE
    
    result <- data[!outlier_flag, , drop = FALSE]
    removed <- sum(outlier_flag)
    
    return(list(data = result, removed = removed))
  }
  
  # ----------------------------------------------------------------------------
  # Remove outliers using Cook's Distance
  # ----------------------------------------------------------------------------
  remove_outliers_cooks <- function(data, method = "4mean") {
    if(!"DEATH_RATE" %in% names(data)) {
      return(list(data = data, removed = 0, message = "DEATH_RATE column required"))
    }
    
    # Get predictors (all numeric except DEATH_RATE)
    predictor_vars <- names(data)[sapply(data, is.numeric)]
    predictor_vars <- predictor_vars[!predictor_vars %in% c("DEATH_RATE", "CODE", "OBS_TYPE")]
    
    if(length(predictor_vars) < 1) {
      return(list(data = data, removed = 0, message = "Need at least 1 predictor variable"))
    }
    
    # Prepare data
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
  
  # ----------------------------------------------------------------------------
  # Remove outliers using Local Outlier Factor (LOF)
  # ----------------------------------------------------------------------------
  remove_outliers_lof <- function(data, minPts = 5, threshold = 2) {
    # Get numeric columns
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
  
  # ----------------------------------------------------------------------------
  # Remove outliers using Isolation Forest
  # ----------------------------------------------------------------------------
  remove_outliers_iforest <- function(data, threshold = 0.6) {
    # Get numeric columns
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
  
  # ----------------------------------------------------------------------------
  # Remove rows by index
  # ----------------------------------------------------------------------------
  remove_rows_by_index <- function(data, indices) {
    if(is.null(indices) || length(indices) == 0) {
      return(list(data = data, removed = 0))
    }
    indices <- as.numeric(indices)
    indices <- indices[indices >= 1 & indices <= nrow(data)]
    result <- data[-indices, , drop = FALSE]
    return(list(data = result, removed = length(indices)))
  }
  
  # ----------------------------------------------------------------------------
  # Keep rows by index (select specific rows)
  # ----------------------------------------------------------------------------
  keep_rows_by_index <- function(data, indices) {
    if(is.null(indices) || length(indices) == 0) {
      return(list(data = data, removed = 0))
    }
    indices <- as.numeric(indices)
    indices <- indices[indices >= 1 & indices <= nrow(data)]
    result <- data[indices, , drop = FALSE]
    return(list(data = result, removed = nrow(data) - length(indices)))
  }
  
  # ----------------------------------------------------------------------------
  # Remove columns by name
  # ----------------------------------------------------------------------------
  remove_cols_by_name <- function(data, cols) {
    if(is.null(cols) || length(cols) == 0) {
      return(list(data = data, removed = 0))
    }
    cols_to_remove <- cols[cols %in% names(data)]
    result <- data[, !names(data) %in% cols_to_remove, drop = FALSE]
    return(list(data = result, removed = length(cols_to_remove)))
  }
  
  # ----------------------------------------------------------------------------
  # Keep columns by name (select specific columns)
  # ----------------------------------------------------------------------------
  keep_cols_by_name <- function(data, cols) {
    if(is.null(cols) || length(cols) == 0) {
      return(list(data = data, removed = 0))
    }
    cols_to_keep <- cols[cols %in% names(data)]
    # Always keep CODE and OBS_TYPE if they exist
    always_keep <- c("CODE", "OBS_TYPE")
    cols_to_keep <- unique(c(cols_to_keep, always_keep[always_keep %in% names(data)]))
    result <- data[, cols_to_keep, drop = FALSE]
    return(list(data = result, removed = ncol(data) - ncol(result)))
  }
  
  
  # ============================================================================
  # Variable Selection Features
  # ============================================================================
  
  original_var_order <- c("CODE", "GOVERN_TYPE", "POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", 
                          "AGE50_PROPTN", "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", 
                          "VAX_RATE", "HEALTHCARE_BASIS", "HEALTHCARE_COST", "DEATH_RATE", "OBS_TYPE")
  
  
  # ============================================================================
  # ============================================================================
  # SECTION 2: REACTIVE VALUES
  # ============================================================================
  # ============================================================================
  
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
  selected_model_dataset <- reactiveVal(NULL)
  
  
  # Pipeline steps reactive
  pipeline_steps <- reactiveVal(list())
  
  # DEBUG: Monitor pipeline steps
  observe({
    steps <- pipeline_steps()
    cat("\n=== PIPELINE STEPS UPDATED ===\n")
    cat("Number of steps:", length(steps), "\n")
    if(length(steps) > 0) {
      # Check if steps is a list and each step is a list
      if(is.list(steps) && length(steps) > 0) {
        for(i in seq_along(steps)) {
          step <- steps[[i]]
          if(is.list(step)) {
            cat("Step", i, ":", step$method %||% "No method", "- cols:", length(step$cols %||% character(0)), "\n")
          } else {
            cat("Step", i, ": Invalid step object - not a list\n")
          }
        }
      } else {
        cat("Steps object is not a valid list\n")
      }
    }
    cat("===============================\n\n")
  })
  
  # Saved pipelines storage
  saved_pipelines <- reactiveVal(list())
  
  # Saved datasets storage
  saved_datasets <- reactiveVal(list())
  
  # Method display names for pipeline
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
    "impute_knn" = "KNN Imputation",
    "impute_manual" = "Manual Imputation",
    "impute_bag" = "Bagged Trees Imputation",
    "log_transform" = "Log Transformation",
    "sqrt_transform" = "Square Root Transformation",
    "yeojohnson" = "Yeo-Johnson Transformation",
    "boxcox" = "Box-Cox Transformation",
    "scale" = "Scale Data (Z-score)",
    "center" = "Center Data",
    "nzv" = "Remove Near-Zero Variance",
    "lincomb" = "Remove Linear Combinations"
  )
  
  # ----------------------------------------------------------------------------
  # Processing Log
  # ----------------------------------------------------------------------------
  
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
  
  pipeline_steps(list())
  
  
  # ============================================================================
  # ============================================================================
  # SECTION 3: MASTER FILTERED DATA (for Data Processing Strategy tab)
  # ============================================================================
  # ============================================================================
  
  master_filtered_data <- reactive({
    # Check if we're in the Data Processing Strategy tab
    # Only run if master sidebar controls exist (they only exist in Tab 2)
    if(is.null(input$master_cat_vars) || is.null(input$master_numeric_vars)) {
      return(data.frame())
    }
    
    selected_cat <- input$master_cat_vars
    selected_num <- input$master_numeric_vars
    selected_vars <- unique(c(selected_cat, selected_num))
    
    # Better validation - return empty data frame instead of validate error
    if(length(selected_vars) == 0) {
      return(data.frame(CODE = character(), stringsAsFactors = FALSE))
    }
    
    # ... rest of the function remains the same from here
    data <- df
    selected_vars <- selected_vars[!selected_vars %in% c("CODE", "OBS_TYPE")]
    
    # Ensure we have valid columns
    selected_vars <- selected_vars[selected_vars %in% names(data)]
    if(length(selected_vars) == 0) {
      return(data.frame(CODE = character(), stringsAsFactors = FALSE))
    }
    
    # Continue with the rest of your existing code...
    data <- data[, selected_vars, drop = FALSE]
    
    col_threshold <- input$master_col_missing_threshold %||% 100
    row_threshold <- input$master_row_missing_threshold %||% 15
    row_percent_threshold <- input$master_row_missing_percent %||% 100
    
    # Convert all columns to character first for consistent handling
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
    
    # Ensure CODE is included for reference if needed
    if("CODE" %in% names(df) && !"CODE" %in% names(data)) {
      data$CODE <- df$CODE[keep_rows]
    }
    
    return(data)
  })
  
  
  
  master_filtered_data_standardized <- reactive({
    data <- master_filtered_data()
    
    # Return empty data frame if no data
    if(is.null(data) || nrow(data) == 0) {
      return(data.frame())
    }
    
    # Rest of your existing code...
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
  
  
  
  
  
  
  # ============================================================================
  # ============================================================================
  # SECTION 4: SIDEBAR TOGGLE OBSERVERS
  # ============================================================================
  # ============================================================================
  
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
  
  # Master sidebar toggle observer
  master_sidebar_state <- reactiveValues(visible = TRUE)
  
  observeEvent(input$toggle_master_sidebar, {
    if(master_sidebar_state$visible) {
      shinyjs::addClass(id = "master_sidebar_wrapper", class = "sidebar-hidden")
      master_sidebar_state$visible <- FALSE
      updateActionButton(session, "toggle_master_sidebar", 
                         label = HTML('<i class="fa fa-sliders-h"></i> Show Master Filters'))
    } else {
      shinyjs::removeClass(id = "master_sidebar_wrapper", class = "sidebar-hidden")
      master_sidebar_state$visible <- TRUE
      updateActionButton(session, "toggle_master_sidebar", 
                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Master Filters'))
    }
  })
  
  
  
  # ============================================================================
  # ============================================================================
  # SECTION 5: TAB 1 - SUMMARY & EDA OUTPUTS
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
  
  filtered_boxplot_data <- reactive({
    data <- df
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
  
  corr_filtered_data <- reactive({
    data <- df
    if(!is.null(input$corr_categorical_vars) && length(input$corr_categorical_vars) > 0) {
      data <- apply_categorical_filters(data, input$corr_categorical_vars, "corr_filter_", input$include_null_correlation)
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
  
  
  # ============================================================================
  # Rising Values
  # ============================================================================
  
  rv_filtered_data <- reactive({
    req(input$rv_numeric_vars, length(input$rv_numeric_vars) > 0)
    data <- df
    include_null <- if(is.null(input$rv_include_null)) FALSE else input$rv_include_null
    treat_neg99 <- safe_in("neg99", input$rv_mv_types)
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
    has_neg99 <- FALSE; has_dash <- FALSE; has_na_string <- FALSE; has_r_na <- FALSE
    
    for(i in 1:n_obs) {
      for(j in 1:n_vars) {
        val <- plot_df[i, j]
        if(!is.na(val) && val == "-99") {
          has_neg99 <- TRUE
          if(show_neg99) color_matrix[i, j] <- "#FF69B4"
        } else if(!is.na(val) && val == "--") {
          has_dash <- TRUE
          if(show_dash) color_matrix[i, j] <- "#32CD32"
        } else if(!is.na(val) && val == "NA") {
          has_na_string <- TRUE
          if(show_na_string) color_matrix[i, j] <- "#FF0000"
        } else if(is.na(val)) {
          has_r_na <- TRUE
          color_matrix[i, j] <- "#808080"
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
    valid_predictors <- predictor_vars[predictor_vars %in% names(data)]
    valid_predictors <- valid_predictors[valid_predictors != target_var]
    if(length(valid_predictors) == 0) {
      showNotification("No valid predictor variables found. Please select different predictors.", type = "error")
      return(NULL)
    }
    
    result_data <- data[, c(target_var, valid_predictors), drop = FALSE]
    
    target_vals <- as.character(result_data[[target_var]])
    target_missing <- rep(FALSE, nrow(result_data))
    
    # FIXED: Each if statement needs its body
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
      scale_color_manual(values = c("FALSE" = "#ADD8E6", "TRUE" = "#e74c3c")) +
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
      scale_color_manual(values = c("FALSE" = "#ADD8E6", "TRUE" = "#e74c3c")) +
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
      scale_color_manual(values = c("FALSE" = "#ADD8E6", "TRUE" = "#e74c3c")) +
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
      scale_color_manual(values = c("FALSE" = "#ADD8E6", "TRUE" = "#e74c3c")) +
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
      scale_color_manual(values = c("FALSE" = "#ADD8E6", "TRUE" = "#e74c3c")) +
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
      scale_color_manual(values = c("FALSE" = "#ADD8E6", "TRUE" = "#e74c3c")) +
      labs(title = "Isolation Forest",
           subtitle = paste("Threshold:", if_result$threshold,
                            "| Outliers:", sum(if_result$is_outlier)),
           x = "Observation Index", y = "Anomaly Score") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Statistics outputs for each method (optional but nice to have)
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ============================================================================
  # ============================================================================
  # SECTION 7: DATA PROCESSING CONTROLS (Pipeline Builder)
  # ============================================================================
  # ============================================================================
  
  # ----------------------------------------------------------------------------
  # 7.1 UI OUTPUTS - Pipeline Step Builder Interface
  # ----------------------------------------------------------------------------
  
  output$processing_steps_ui <- renderUI({
    steps <- pipeline_steps()
    
    # Validate steps is a list and not an atomic vector
    if(!is.list(steps) || length(steps) == 0) {
      return(div(class = "alert alert-info", 
                 icon("info-circle"), 
                 "No steps added yet. Click 'Add Step' to begin building your processing pipeline."))
    }
    
    # Check if steps is a named list or has valid step objects
    if(!is.list(steps[[1]])) {
      # Reset pipeline_steps if it's corrupted
      pipeline_steps(list())
      return(div(class = "alert alert-warning", 
                 icon("exclamation-triangle"), 
                 "Pipeline data was corrupted. Please add steps again."))
    }
    
    current_data <- processed_data_working()
    numeric_cols_available <- names(current_data)[sapply(current_data, is.numeric)]
    numeric_cols_available <- numeric_cols_available[!numeric_cols_available %in% c("CODE", "OBS_TYPE", "DEATH_RATE")]
    
    tagList(lapply(seq_along(steps), function(idx) {
      step <- steps[[idx]]
      
      # Ensure step is a list
      if(!is.list(step)) {
        return(div(class = "alert alert-danger", "Invalid step configuration"))
      }
      
      step_id <- step$id %||% idx
      
      # Safely extract numeric values from additional_info (which might be a list)
      additional_info <- step$additional_info
      na_row_threshold <- if(is.numeric(additional_info)) additional_info else 5
      na_col_threshold <- if(is.numeric(additional_info)) additional_info else 50
      knn_k <- if(is.numeric(additional_info)) additional_info else 5
      manual_val <- if(is.numeric(additional_info)) additional_info else 0
      bag_trees <- if(is.numeric(additional_info)) additional_info else 25
      
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
                                  "Impute — KNN" = "impute_knn",
                                  "Impute — Manual Value" = "impute_manual",
                                  "Impute — Bagged Trees" = "impute_bag",
                                  "Transformations" = "---",
                                  "Transform — Log" = "log_transform",
                                  "Transform — Square Root" = "sqrt_transform",
                                  "Transform — Yeo-Johnson" = "yeojohnson",
                                  "Transform — Box-Cox" = "boxcox",
                                  "Scale Data" = "scale",
                                  "Center Data" = "center",
                                  "Feature Selection" = "---",
                                  "Remove Near-Zero Variance" = "nzv",
                                  "Remove Linear Combinations" = "lincomb"),
                      selected = step$method),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] != '' && 
                             input['step_method_", step_id, "'] != 'remove_na_rows' && 
                             input['step_method_", step_id, "'] != 'remove_na_cols' &&
                             input['step_method_", step_id, "'] != 'nzv' &&
                             input['step_method_", step_id, "'] != 'lincomb'"),
            pickerInput(paste0("step_cols_", step_id), "Select Variables",
                        choices = numeric_cols_available,
                        selected = step$cols,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                       `selected-text-format` = "count > 3"))
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'impute_knn'"),
            numericInput(paste0("step_knn_k_", step_id), "Number of Neighbors", 
                         value = knn_k, min = 1, max = 20)
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'impute_manual'"),
            numericInput(paste0("step_manual_val_", step_id), "Imputation Value", 
                         value = manual_val)
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'impute_bag'"),
            numericInput(paste0("step_bag_trees_", step_id), "Number of Trees", 
                         value = bag_trees, min = 10, max = 100)
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_na_rows'"),
            sliderInput(paste0("step_na_row_threshold_", step_id), "Max Missing Values per Row",
                        min = 0, max = 15, value = na_row_threshold, step = 1),
            checkboxInput(paste0("step_na_row_apply_test_", step_id), "Apply to Test Data", 
                          value = if(isTRUE(step$skip_test)) TRUE else FALSE)
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_na_cols'"),
            sliderInput(paste0("step_na_col_threshold_", step_id), "Max Missing % per Column",
                        min = 0, max = 100, value = na_col_threshold, step = 5, post = "%")
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'lincomb'"),
            div(class = "alert alert-warning", style = "padding: 5px; margin-top: 5px; font-size: 11px;",
                icon("exclamation-triangle"), 
                "Make sure all NA values have been addressed before adding this step")
          ),
          
          # New conditional panels for manual operations and outlier removal
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_rows_by_index'"),
            textAreaInput(paste0("step_row_indices_", step_id), "Row indices to remove (comma-separated, e.g., 1,5,10,25 or 1:10):",
                          value = "", rows = 2, placeholder = "1, 5, 10, 25, 50"),
            p("Specify row numbers to remove. Can be single numbers or ranges like 1:10.", 
              style = "font-size: 11px; color: #666; margin-top: 5px;")
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'keep_rows_by_index'"),
            textAreaInput(paste0("step_row_keep_indices_", step_id), "Row indices to keep (comma-separated, e.g., 1,5,10,25 or 1:10):",
                          value = "", rows = 2, placeholder = "1, 5, 10, 25, 50"),
            p("Specify row numbers to keep. All other rows will be removed.", 
              style = "font-size: 11px; color: #666; margin-top: 5px;")
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_cols_by_name'"),
            pickerInput(paste0("step_cols_remove_", step_id), "Select columns to remove:",
                        choices = names(current_data),
                        selected = NULL,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE))
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'keep_cols_by_name'"),
            pickerInput(paste0("step_cols_keep_", step_id), "Select columns to keep:",
                        choices = names(current_data),
                        selected = NULL,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)),
            p("Note: CODE and OBS_TYPE are always kept automatically.", 
              style = "font-size: 11px; color: #666; margin-top: 5px;")
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_outliers_iqr'"),
            pickerInput(paste0("step_outlier_vars_", step_id), "Select variables for outlier detection:",
                        choices = numeric_cols,
                        selected = numeric_cols[1:min(3, length(numeric_cols))],
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)),
            sliderInput(paste0("step_outlier_iqr_mult_", step_id), "IQR Multiplier:",
                        min = 1, max = 5, value = 1.5, step = 0.1)
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_outliers_mahalanobis'"),
            sliderInput(paste0("step_outlier_mahalanobis_prob_", step_id), "Chi-Square Probability Threshold:",
                        min = 0.9, max = 0.9999, value = 0.999, step = 0.001)
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_outliers_cooks'"),
            selectInput(paste0("step_outlier_cooks_method_", step_id), "Threshold Method:",
                        choices = c("4 * mean" = "4mean", "4/n" = "4n", "Quantile 0.99" = "quantile"),
                        selected = "4mean")
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_outliers_lof'"),
            sliderInput(paste0("step_outlier_lof_minpts_", step_id), "minPts (neighbors):",
                        min = 3, max = 20, value = 5, step = 1),
            sliderInput(paste0("step_outlier_lof_threshold_", step_id), "LOF Threshold:",
                        min = 1, max = 5, value = 2, step = 0.1)
          ),
          
          conditionalPanel(
            condition = paste0("input['step_method_", step_id, "'] == 'remove_outliers_iforest'"),
            sliderInput(paste0("step_outlier_if_threshold_", step_id), "Isolation Forest Threshold:",
                        min = 0.5, max = 0.95, value = 0.6, step = 0.01)
          )
      )
    }))
  })
  
  
  # ----------------------------------------------------------------------------
  # 7.2 PIPELINE STEP MANAGEMENT - Add, Remove, and Update Steps
  # ----------------------------------------------------------------------------
  
  # Add step observer
  observeEvent(input$add_processing_step, {
    current <- pipeline_steps()
    id <- if(length(current) == 0) 1 else max(sapply(current, function(x) x$id)) + 1
    
    new_step <- list(
      id = id,
      method = "",
      cols = character(0),
      additional_info = NULL,
      skip_test = FALSE
    )
    pipeline_steps(append(current, list(new_step)))
    add_to_log(paste("Added pipeline step", id))
  })
  
  
  # Remove step observer
  observeEvent(input$remove_step, {
    step_to_remove <- as.numeric(input$remove_step)
    current <- pipeline_steps()
    if(step_to_remove <= length(current) && step_to_remove > 0) {
      current <- current[-step_to_remove]
      pipeline_steps(current)
      add_to_log(paste("Removed pipeline step", step_to_remove))
    }
  })
  
  
  # Update step parameters - SAFER VERSION with error handling for all new methods
  observe({
    steps <- pipeline_steps()
    if(length(steps) == 0) return()
    
    updated <- FALSE
    
    for(i in seq_along(steps)) {
      step_id <- steps[[i]]$id
      
      # Safely get method input
      method_input <- tryCatch(input[[paste0("step_method_", step_id)]], error = function(e) NULL)
      if(!is.null(method_input) && method_input != "") {
        if(steps[[i]]$method != method_input) {
          steps[[i]]$method <- method_input
          updated <- TRUE
        }
      }
      
      # Safely get columns input (for imputation/transformation steps)
      cols_input <- tryCatch(input[[paste0("step_cols_", step_id)]], error = function(e) NULL)
      if(!is.null(cols_input)) {
        if(!identical(steps[[i]]$cols, cols_input)) {
          steps[[i]]$cols <- cols_input
          updated <- TRUE
        }
      }
      
      # Safely handle additional info based on method
      if(!is.null(steps[[i]]$method) && steps[[i]]$method != "") {
        
        # Existing methods
        if(steps[[i]]$method == "impute_knn") {
          val <- tryCatch(input[[paste0("step_knn_k_", step_id)]], error = function(e) NULL)
          if(!is.null(val)) steps[[i]]$additional_info <- val
        } else if(steps[[i]]$method == "impute_manual") {
          val <- tryCatch(input[[paste0("step_manual_val_", step_id)]], error = function(e) NULL)
          if(!is.null(val)) steps[[i]]$additional_info <- val
        } else if(steps[[i]]$method == "impute_bag") {
          val <- tryCatch(input[[paste0("step_bag_trees_", step_id)]], error = function(e) NULL)
          if(!is.null(val)) steps[[i]]$additional_info <- val
        } else if(steps[[i]]$method == "remove_na_rows") {
          val <- tryCatch(input[[paste0("step_na_row_threshold_", step_id)]], error = function(e) NULL)
          if(!is.null(val)) steps[[i]]$additional_info <- val
          skip <- tryCatch(input[[paste0("step_na_row_apply_test_", step_id)]], error = function(e) FALSE)
          steps[[i]]$skip_test <- if(!is.null(skip)) skip else FALSE
        } else if(steps[[i]]$method == "remove_na_cols") {
          val <- tryCatch(input[[paste0("step_na_col_threshold_", step_id)]], error = function(e) NULL)
          if(!is.null(val)) steps[[i]]$additional_info <- val
          
          # New manual row/column operations
        } else if(steps[[i]]$method == "remove_rows_by_index") {
          val <- tryCatch(input[[paste0("step_row_indices_", step_id)]], error = function(e) NULL)
          if(!is.null(val) && val != "") {
            if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
            steps[[i]]$additional_info$indices <- val
            updated <- TRUE
          }
        } else if(steps[[i]]$method == "keep_rows_by_index") {
          val <- tryCatch(input[[paste0("step_row_keep_indices_", step_id)]], error = function(e) NULL)
          if(!is.null(val) && val != "") {
            if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
            steps[[i]]$additional_info$keep_indices <- val
            updated <- TRUE
          }
        } else if(steps[[i]]$method == "remove_cols_by_name") {
          val <- tryCatch(input[[paste0("step_cols_remove_", step_id)]], error = function(e) NULL)
          if(!is.null(val)) {
            if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
            steps[[i]]$additional_info$remove_cols <- val
            updated <- TRUE
          }
        } else if(steps[[i]]$method == "keep_cols_by_name") {
          val <- tryCatch(input[[paste0("step_cols_keep_", step_id)]], error = function(e) NULL)
          if(!is.null(val)) {
            if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
            steps[[i]]$additional_info$keep_cols <- val
            updated <- TRUE
          }
          
          # New outlier removal methods
        } else if(steps[[i]]$method == "remove_outliers_iqr") {
          vars <- tryCatch(input[[paste0("step_outlier_vars_", step_id)]], error = function(e) NULL)
          mult <- tryCatch(input[[paste0("step_outlier_iqr_mult_", step_id)]], error = function(e) NULL)
          if(!is.null(vars) || !is.null(mult)) {
            if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
            if(!is.null(vars)) steps[[i]]$additional_info$outlier_vars <- vars
            if(!is.null(mult)) steps[[i]]$additional_info$iqr_multiplier <- mult
            updated <- TRUE
          }
        } else if(steps[[i]]$method == "remove_outliers_mahalanobis") {
          val <- tryCatch(input[[paste0("step_outlier_mahalanobis_prob_", step_id)]], error = function(e) NULL)
          if(!is.null(val)) {
            if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
            steps[[i]]$additional_info$mahalanobis_prob <- val
            updated <- TRUE
          }
        } else if(steps[[i]]$method == "remove_outliers_cooks") {
          val <- tryCatch(input[[paste0("step_outlier_cooks_method_", step_id)]], error = function(e) NULL)
          if(!is.null(val)) {
            if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
            steps[[i]]$additional_info$cooks_method <- val
            updated <- TRUE
          }
        } else if(steps[[i]]$method == "remove_outliers_lof") {
          minPts <- tryCatch(input[[paste0("step_outlier_lof_minpts_", step_id)]], error = function(e) NULL)
          threshold <- tryCatch(input[[paste0("step_outlier_lof_threshold_", step_id)]], error = function(e) NULL)
          if(!is.null(minPts) || !is.null(threshold)) {
            if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
            if(!is.null(minPts)) steps[[i]]$additional_info$lof_minpts <- minPts
            if(!is.null(threshold)) steps[[i]]$additional_info$lof_threshold <- threshold
            updated <- TRUE
          }
        } else if(steps[[i]]$method == "remove_outliers_iforest") {
          val <- tryCatch(input[[paste0("step_outlier_if_threshold_", step_id)]], error = function(e) NULL)
          if(!is.null(val)) {
            if(is.null(steps[[i]]$additional_info)) steps[[i]]$additional_info <- list()
            steps[[i]]$additional_info$if_threshold <- val
            updated <- TRUE
          }
        }
      }
    }
    
    if(updated) pipeline_steps(steps)
  })
  
  
  # ----------------------------------------------------------------------------
  # 7.3 PIPELINE EXECUTION - Apply Recipe Button (WITH ALL NEW STEP TYPES)
  # ----------------------------------------------------------------------------
  
  # MAIN PIPELINE PROCESSING - Applies all steps in sequence
  observeEvent(input$process_data_btn, {
    steps <- pipeline_steps()
    
    # Debug output
    cat("\n=== APPLYING PIPELINE ===\n")
    cat("Number of steps:", length(steps), "\n")
    
    if(length(steps) == 0) {
      showNotification("No steps in pipeline. Add steps first.", type = "warning")
      return()
    }
    
    # Start with current working data
    data <- processed_data_working()
    original_rows <- nrow(data)
    original_cols <- ncol(data)
    
    add_to_log("=== Starting pipeline processing ===")
    
    # Apply each step in sequence
    for(i in seq_along(steps)) {
      step <- steps[[i]]
      method <- step$method
      
      cat("Processing step", i, ":", method, "\n")
      
      if(is.null(method) || method == "") {
        cat("  Skipping - no method selected\n")
        next
      }
      
      cols <- step$cols
      
      # ================================================================
      # EXISTING STEP TYPES
      # ================================================================
      
      if(method == "remove_na_rows") {
        threshold <- step$additional_info %||% 5
        before <- nrow(data)
        row_na_count <- apply(data, 1, function(x) sum(is.na(x)))
        data <- data[row_na_count <= threshold, ]
        removed <- before - nrow(data)
        add_to_log(paste("  Removed", removed, "rows with >", threshold, "missing values"))
        cat("  Removed", removed, "rows\n")
        
      } else if(method == "remove_na_cols") {
        threshold <- step$additional_info %||% 50
        before <- ncol(data)
        col_na_pct <- colMeans(is.na(data)) * 100
        cols_to_keep <- names(col_na_pct)[col_na_pct <= threshold]
        data <- data[, cols_to_keep, drop = FALSE]
        removed <- before - ncol(data)
        add_to_log(paste("  Removed", removed, "columns with >", threshold, "% missing values"))
        cat("  Removed", removed, "columns\n")
        
      } else if(method == "impute_median" && !is.null(cols) && length(cols) > 0) {
        for(col in cols) {
          if(col %in% names(data) && is.numeric(data[[col]])) {
            na_count <- sum(is.na(data[[col]]))
            if(na_count > 0) {
              data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
              add_to_log(paste("  Imputed", na_count, "values in", col, "with median"))
            }
          }
        }
        cat("  Median imputed", length(cols), "variables\n")
        
      } else if(method == "impute_mean" && !is.null(cols) && length(cols) > 0) {
        for(col in cols) {
          if(col %in% names(data) && is.numeric(data[[col]])) {
            na_count <- sum(is.na(data[[col]]))
            if(na_count > 0) {
              data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
              add_to_log(paste("  Imputed", na_count, "values in", col, "with mean"))
            }
          }
        }
        cat("  Mean imputed", length(cols), "variables\n")
        
      } else if(method == "impute_manual" && !is.null(cols) && length(cols) > 0) {
        impute_val <- step$additional_info %||% 0
        for(col in cols) {
          if(col %in% names(data) && is.numeric(data[[col]])) {
            na_count <- sum(is.na(data[[col]]))
            if(na_count > 0) {
              data[[col]][is.na(data[[col]])] <- impute_val
              add_to_log(paste("  Imputed", na_count, "values in", col, "with", impute_val))
            }
          }
        }
        cat("  Manually imputed", length(cols), "variables with", impute_val, "\n")
        
      } else if(method == "log_transform" && !is.null(cols) && length(cols) > 0) {
        for(col in cols) {
          if(col %in% names(data) && is.numeric(data[[col]])) {
            min_val <- min(data[[col]], na.rm = TRUE)
            if(min_val <= 0) {
              data[[col]] <- log(data[[col]] + abs(min_val) + 1)
            } else {
              data[[col]] <- log(data[[col]])
            }
          }
        }
        add_to_log(paste("  Log transformed", length(cols), "variables"))
        cat("  Log transformed", length(cols), "variables\n")
        
      } else if(method == "sqrt_transform" && !is.null(cols) && length(cols) > 0) {
        for(col in cols) {
          if(col %in% names(data) && is.numeric(data[[col]])) {
            min_val <- min(data[[col]], na.rm = TRUE)
            if(min_val < 0) {
              data[[col]] <- sqrt(data[[col]] + abs(min_val) + 1)
            } else {
              data[[col]] <- sqrt(data[[col]])
            }
          }
        }
        add_to_log(paste("  Square root transformed", length(cols), "variables"))
        cat("  Square root transformed", length(cols), "variables\n")
        
      } else if(method == "boxcox" && !is.null(cols) && length(cols) > 0) {
        for(col in cols) {
          if(col %in% names(data) && is.numeric(data[[col]])) {
            min_val <- min(data[[col]], na.rm = TRUE)
            if(min_val > 0 && requireNamespace("MASS", quietly = TRUE)) {
              bc <- MASS::boxcox(data[[col]] ~ 1, plotit = FALSE)
              lambda <- bc$x[which.max(bc$y)]
              if(abs(lambda) < 0.01) {
                data[[col]] <- log(data[[col]])
              } else {
                data[[col]] <- (data[[col]]^lambda - 1) / lambda
              }
              add_to_log(paste("  Box-Cox transformed", col, "with lambda =", round(lambda, 3)))
            }
          }
        }
        cat("  Box-Cox transformed", length(cols), "variables\n")
        
      } else if(method == "yeojohnson" && !is.null(cols) && length(cols) > 0) {
        for(col in cols) {
          if(col %in% names(data) && is.numeric(data[[col]])) {
            if(requireNamespace("bestNormalize", quietly = TRUE)) {
              yj <- bestNormalize::yeojohnson(data[[col]])
              data[[col]] <- predict(yj, data[[col]])
              add_to_log(paste("  Yeo-Johnson transformed", col, "with lambda =", round(yj$lambda, 3)))
            }
          }
        }
        cat("  Yeo-Johnson transformed", length(cols), "variables\n")
        
      } else if(method == "scale" && !is.null(cols) && length(cols) > 0) {
        for(col in cols) {
          if(col %in% names(data) && is.numeric(data[[col]])) {
            data[[col]] <- scale(data[[col]])
          }
        }
        add_to_log(paste("  Scaled", length(cols), "variables"))
        cat("  Scaled", length(cols), "variables\n")
        
      } else if(method == "center" && !is.null(cols) && length(cols) > 0) {
        for(col in cols) {
          if(col %in% names(data) && is.numeric(data[[col]])) {
            data[[col]] <- data[[col]] - mean(data[[col]], na.rm = TRUE)
          }
        }
        add_to_log(paste("  Centered", length(cols), "variables"))
        cat("  Centered", length(cols), "variables\n")
        
        # ================================================================
        # NEW MANUAL ROW/COLUMN OPERATIONS
        # ================================================================
        
      } else if(method == "remove_rows_by_index") {
        indices_text <- step$additional_info$indices %||% ""
        if(indices_text != "") {
          # Parse comma-separated indices and ranges
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
          indices <- indices[indices >= 1 & indices <= nrow(data)]
          before <- nrow(data)
          if(length(indices) > 0) {
            data <- data[-indices, , drop = FALSE]
            removed <- before - nrow(data)
            add_to_log(paste("  Removed", removed, "rows by index"))
            cat("  Removed", removed, "rows by index\n")
          }
        }
        
      } else if(method == "keep_rows_by_index") {
        indices_text <- step$additional_info$keep_indices %||% ""
        if(indices_text != "") {
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
          indices <- indices[indices >= 1 & indices <= nrow(data)]
          before <- nrow(data)
          if(length(indices) > 0) {
            data <- data[indices, , drop = FALSE]
            removed <- before - nrow(data)
            add_to_log(paste("  Kept", nrow(data), "rows, removed", removed, "rows"))
            cat("  Kept", nrow(data), "rows, removed", removed, "rows\n")
          }
        }
        
      } else if(method == "remove_cols_by_name") {
        cols_to_remove <- step$additional_info$remove_cols %||% character(0)
        if(length(cols_to_remove) > 0) {
          protected_cols <- c("CODE", "OBS_TYPE", "DEATH_RATE")
          cols_to_remove <- cols_to_remove[!cols_to_remove %in% protected_cols]
          before <- ncol(data)
          data <- data[, !names(data) %in% cols_to_remove, drop = FALSE]
          removed <- before - ncol(data)
          add_to_log(paste("  Removed", removed, "columns by name"))
          cat("  Removed", removed, "columns by name\n")
        }
        
      } else if(method == "keep_cols_by_name") {
        cols_to_keep <- step$additional_info$keep_cols %||% character(0)
        if(length(cols_to_keep) > 0) {
          always_keep <- c("CODE", "OBS_TYPE", "DEATH_RATE")
          cols_to_keep <- unique(c(cols_to_keep, always_keep[always_keep %in% names(data)]))
          before <- ncol(data)
          data <- data[, names(data) %in% cols_to_keep, drop = FALSE]
          removed <- before - ncol(data)
          add_to_log(paste("  Kept", ncol(data), "columns, removed", removed, "columns"))
          cat("  Kept", ncol(data), "columns, removed", removed, "columns\n")
        }
        
        # ================================================================
        # NEW OUTLIER REMOVAL METHODS
        # ================================================================
        
      } else if(method == "remove_outliers_iqr") {
        outlier_vars <- step$additional_info$outlier_vars %||% character(0)
        iqr_mult <- step$additional_info$iqr_multiplier %||% 1.5
        if(length(outlier_vars) > 0) {
          result <- remove_outliers_iqr(data, outlier_vars, iqr_mult)
          removed <- result$removed
          data <- result$data
          add_to_log(paste("  Removed", removed, "outliers using IQR method"))
          cat("  Removed", removed, "outliers using IQR method\n")
        }
        
      } else if(method == "remove_outliers_mahalanobis") {
        prob <- step$additional_info$mahalanobis_prob %||% 0.999
        result <- remove_outliers_mahalanobis(data, prob)
        removed <- result$removed
        data <- result$data
        if(removed > 0) {
          add_to_log(paste("  Removed", removed, "outliers using Mahalanobis distance"))
        }
        cat("  Removed", removed, "outliers using Mahalanobis distance\n")
        
      } else if(method == "remove_outliers_cooks") {
        cooks_method <- step$additional_info$cooks_method %||% "4mean"
        result <- remove_outliers_cooks(data, cooks_method)
        removed <- result$removed
        data <- result$data
        if(removed > 0) {
          add_to_log(paste("  Removed", removed, "outliers using Cook's distance"))
        }
        cat("  Removed", removed, "outliers using Cook's distance\n")
        
      } else if(method == "remove_outliers_lof") {
        minPts <- step$additional_info$lof_minpts %||% 5
        lof_threshold <- step$additional_info$lof_threshold %||% 2
        result <- remove_outliers_lof(data, minPts, lof_threshold)
        removed <- result$removed
        data <- result$data
        if(removed > 0) {
          add_to_log(paste("  Removed", removed, "outliers using Local Outlier Factor"))
        }
        cat("  Removed", removed, "outliers using LOF\n")
        
      } else if(method == "remove_outliers_iforest") {
        if_threshold <- step$additional_info$if_threshold %||% 0.6
        result <- remove_outliers_iforest(data, if_threshold)
        removed <- result$removed
        data <- result$data
        if(removed > 0) {
          add_to_log(paste("  Removed", removed, "outliers using Isolation Forest"))
        }
        cat("  Removed", removed, "outliers using Isolation Forest\n")
      }
    }
    
    # Update the working data
    processed_data_working(data)
    
    add_to_log(paste("=== Pipeline complete: reduced from", original_rows, "rows to", nrow(data), 
                     "rows and", original_cols, "cols to", ncol(data), "cols ==="))
    
    showNotification(paste("Pipeline applied successfully!", nrow(data), "rows,", ncol(data), "columns"), 
                     type = "message", duration = 5)
    
    cat("=== PIPELINE COMPLETE ===\n")
    cat("Final dimensions:", nrow(data), "rows,", ncol(data), "cols\n\n")
  })
  
  
  # ----------------------------------------------------------------------------
  # 7.4 AUTO-SAVE FUNCTIONALITY - Save Recipe Results Automatically
  # ----------------------------------------------------------------------------
  
  # ----------------------------------------------------------------------------
  # 7.4 AUTO-SAVE FUNCTIONALITY - Save Recipe Results Automatically
  # ----------------------------------------------------------------------------
  
  # Auto-save processed dataset when pipeline is applied
  observeEvent(processed_data_working(), {
    data <- processed_data_working()
    
    isolate({
      steps <- pipeline_steps()
      
      if(length(steps) > 0) {
        # Try to get the user-provided pipeline name first
        pipeline_name <- input$pipeline_name
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
        
        # If user provided a name, use it
        if(!is.null(pipeline_name) && pipeline_name != "") {
          # Clean the name (remove spaces, make it filename-friendly)
          clean_name <- gsub(" ", "_", pipeline_name)
          clean_name <- gsub("[^a-zA-Z0-9_]", "", clean_name)
          auto_name <- paste0(clean_name, "_", timestamp)
        } else {
          # Fall back to auto-generated name based on operations
          method_map <- list(
            "remove_na_cols" = "cleaned",
            "remove_na_rows" = "cleaned",
            "remove_rows_by_index" = "filtered",
            "keep_rows_by_index" = "filtered",
            "remove_cols_by_name" = "selected",
            "keep_cols_by_name" = "selected",
            "remove_outliers_iqr" = "outliers_removed",
            "remove_outliers_mahalanobis" = "outliers_removed",
            "remove_outliers_cooks" = "outliers_removed",
            "remove_outliers_lof" = "outliers_removed",
            "remove_outliers_iforest" = "outliers_removed",
            "impute_median" = "imputed",
            "impute_mean" = "imputed",
            "impute_manual" = "imputed",
            "impute_knn" = "imputed",
            "impute_bag" = "imputed",
            "scale" = "scaled",
            "center" = "centered",
            "log_transform" = "log",
            "sqrt_transform" = "sqrt",
            "boxcox" = "transformed",
            "yeojohnson" = "transformed",
            "nzv" = "reduced",
            "lincomb" = "reduced"
          )
          
          # Collect unique operation types
          operation_names <- c()
          for(step in steps) {
            method <- step$method
            if(!is.null(method) && method != "") {
              short_name <- method_map[[method]] %||% "processed"
              if(!short_name %in% operation_names) {
                operation_names <- c(operation_names, short_name)
              }
            }
          }
          
          # Create base name from operations
          if(length(operation_names) > 0) {
            base_name <- paste(operation_names, collapse = "_")
          } else {
            base_name <- "processed"
          }
          
          auto_name <- paste0(base_name, "_", timestamp)
        }
        
        # Check for duplicates and add version number if needed
        existing_names <- names(saved_datasets())
        original_name <- auto_name
        counter <- 1
        while(auto_name %in% existing_names) {
          auto_name <- paste0(original_name, "_v", counter)
          counter <- counter + 1
        }
        
        # Save the dataset
        current <- saved_datasets()
        current[[auto_name]] <- list(
          data = data,
          name = auto_name,
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          rows = nrow(data),
          cols = ncol(data),
          log = tail(processing_log(), 20)
        )
        saved_datasets(current)
        
        # Update the picker input in the modeling tab
        updatePickerInput(session, "model_dataset_select", 
                          choices = names(saved_datasets()),
                          selected = auto_name)
        
        # Switch to saved dataset mode in modeling tab
        updateRadioButtons(session, "model_data_source", selected = "saved")
        
        # Show notification with the name used
        if(!is.null(pipeline_name) && pipeline_name != "") {
          showNotification(paste("Recipe saved as:", auto_name), type = "message", duration = 5)
        } else {
          showNotification(paste("Recipe auto-saved as:", auto_name, "(no name provided)"), type = "message", duration = 5)
        }
        add_to_log(paste("Auto-saved recipe result as:", auto_name))
      }
    })
  }, ignoreInit = TRUE)
  
  
  # ----------------------------------------------------------------------------
  # 7.5 PIPELINE RESET - Clear All Steps
  # ----------------------------------------------------------------------------
  
  observeEvent(input$reset_pipeline_btn, {
    pipeline_steps(list())
    add_to_log("Reset pipeline - all steps cleared")
    showNotification("Pipeline reset", type = "message")
  })
  
  
  # ----------------------------------------------------------------------------
  # 7.6 SAVED PIPELINES MANAGEMENT - Save and Load Recipes
  # ----------------------------------------------------------------------------
  
  # Save pipeline/recipe
  observeEvent(input$save_pipeline_btn, {
    pipeline_name <- trimws(input$pipeline_name)
    if(pipeline_name == "") {
      showNotification("Please enter a pipeline name", type = "error")
      return()
    }
    
    steps <- pipeline_steps()
    
    # Validate steps
    if(!is.list(steps) || length(steps) == 0) {
      showNotification("No steps in pipeline. Add steps first.", type = "error")
      return()
    }
    
    # Make a deep copy of steps to ensure it's saved correctly
    steps_to_save <- list()
    for(i in seq_along(steps)) {
      if(is.list(steps[[i]])) {
        # Create a clean copy of the step
        step_copy <- list(
          id = steps[[i]]$id,
          method = steps[[i]]$method,
          cols = steps[[i]]$cols %||% character(0),
          additional_info = steps[[i]]$additional_info,
          skip_test = steps[[i]]$skip_test %||% FALSE
        )
        steps_to_save[[i]] <- step_copy
      } else {
        showNotification("Cannot save: Invalid step data", type = "error")
        return()
      }
    }
    
    current <- saved_pipelines()
    current[[pipeline_name]] <- list(
      steps = steps_to_save,
      created = Sys.time(),
      log = tail(processing_log(), 50)
    )
    saved_pipelines(current)
    
    # Save to file (if persistent storage is set up)
    if(exists("save_pipelines_to_file")) {
      save_pipelines_to_file()
    }
    
    showNotification(paste("Pipeline", pipeline_name, "saved successfully with", length(steps_to_save), "steps"), type = "message")
    add_to_log(paste("Saved pipeline:", pipeline_name, "with", length(steps_to_save), "steps"))
  })
  
  # ADD THE CLEAR NAME OBSERVER RIGHT HERE ↓↓↓
  # Clear pipeline name after successful save
  observeEvent(input$save_pipeline_btn, {
    pipeline_name <- trimws(input$pipeline_name)
    if(pipeline_name != "" && length(pipeline_steps()) > 0) {
      # Only clear if save was successful
      updateTextInput(session, "pipeline_name", value = "")
    }
  }, priority = 100)
  
  
  # Display saved pipelines list
  output$saved_pipelines_list <- renderUI({
    pipelines <- saved_pipelines()
    
    if(length(pipelines) == 0) {
      return(div(class = "alert alert-info", 
                 "No saved recipes yet. Build one and save it above."))
    }
    
    tagList(lapply(names(pipelines), function(name) {
      pipeline <- pipelines[[name]]
      steps_count <- length(pipeline$steps)
      
      div(class = "well", style = "padding: 8px; margin-bottom: 8px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              div(
                strong(name, style = "color: #2C3E50;"),
                br(),
                tags$small(paste(steps_count, "steps |", format(pipeline$created, "%Y-%m-%d %H:%M")))
              ),
              div(
                actionButton(paste0("load_pipeline_", gsub(" ", "_", name)), 
                             label = "Load", 
                             class = "btn-primary btn-xs",
                             style = "margin-right: 5px;",
                             onclick = paste0("Shiny.setInputValue('load_pipeline_trigger', '", name, "', {priority: 'event'})")),
                actionButton(paste0("delete_pipeline_", gsub(" ", "_", name)), 
                             label = "Delete", 
                             class = "btn-danger btn-xs",
                             onclick = paste0("Shiny.setInputValue('delete_pipeline_trigger', '", name, "', {priority: 'event'})"))
              )
          )
      )
    }))
  })
  
  
  # Load pipeline trigger
  observeEvent(input$load_pipeline_trigger, {
    pipeline_name <- input$load_pipeline_trigger
    if(!is.null(pipeline_name) && pipeline_name != "" && pipeline_name %in% names(saved_pipelines())) {
      # Get the saved pipeline
      saved_pipeline <- saved_pipelines()[[pipeline_name]]
      
      # Extract steps and validate they are a list
      loaded_steps <- saved_pipeline$steps
      
      # Check if loaded_steps is valid
      if(is.null(loaded_steps)) {
        showNotification(paste("Recipe", pipeline_name, "has no steps data"), type = "error")
        return()
      }
      
      # If loaded_steps is not a list, try to convert or show error
      if(!is.list(loaded_steps)) {
        showNotification(paste("Recipe", pipeline_name, "data is corrupted. Cannot load."), type = "error")
        add_to_log(paste("Failed to load recipe:", pipeline_name, "- steps data is not a list"))
        return()
      }
      
      # If loaded_steps is empty, show warning
      if(length(loaded_steps) == 0) {
        showNotification(paste("Recipe", pipeline_name, "has no steps"), type = "warning")
        pipeline_steps(list())
        return()
      }
      
      # Validate each step is a list
      valid_steps <- TRUE
      for(i in seq_along(loaded_steps)) {
        if(!is.list(loaded_steps[[i]])) {
          valid_steps <- FALSE
          break
        }
      }
      
      if(!valid_steps) {
        showNotification(paste("Recipe", pipeline_name, "contains invalid step data. Cannot load."), type = "error")
        return()
      }
      
      # Load the steps
      pipeline_steps(loaded_steps)
      showNotification(paste("Loaded recipe:", pipeline_name, "-", length(loaded_steps), "steps"), type = "message")
      add_to_log(paste("Loaded saved recipe:", pipeline_name, "with", length(loaded_steps), "steps"))
    }
  })
  
  
  # ----------------------------------------------------------------------------
  # 7.7 PIPELINE SUMMARY DISPLAY - Show Current Steps
  # ----------------------------------------------------------------------------
  
  output$pipeline_summary_ui <- renderUI({
    steps <- pipeline_steps()
    
    # Validate steps
    if(!is.list(steps) || length(steps) == 0) {
      return(p(icon("info-circle"), " No steps added yet. Click 'Add Step' to begin.", style = "color: grey;"))
    }
    
    # Check if first step is valid
    if(!is.list(steps[[1]])) {
      return(p(icon("exclamation-triangle"), " Pipeline data corrupted. Please reset and add steps again.", style = "color: orange;"))
    }
    
    tagList(
      lapply(seq_along(steps), function(i) {
        step <- steps[[i]]
        
        # Ensure step is a list
        if(!is.list(step)) {
          return(div(style = "color: red;", paste("Step", i, ": Invalid step configuration")))
        }
        
        method_name <- method_display_names[[step$method]] %||% (step$method %||% "Unknown method")
        
        # Build contextual info string with safe access
        context <- ""
        if(!is.null(step$cols) && length(step$cols) > 0) {
          context <- paste0(" (", length(step$cols), " vars)")
        }
        if(step$method == "impute_knn" && !is.null(step$additional_info)) {
          context <- paste0(context, " | k = ", step$additional_info)
        }
        if(step$method == "remove_na_rows" && !is.null(step$additional_info)) {
          context <- paste0(context, " | threshold = ", step$additional_info, " missing")
          if(!is.null(step$skip_test) && step$skip_test) context <- paste0(context, " | skip test")
        }
        if(step$method == "remove_na_cols" && !is.null(step$additional_info)) {
          context <- paste0(context, " | >", step$additional_info, "% missing removed")
        }
        if(step$method == "remove_rows_by_index" && !is.null(step$additional_info$indices)) {
          context <- paste0(context, " | removing specific rows")
        }
        if(step$method == "keep_rows_by_index" && !is.null(step$additional_info$keep_indices)) {
          context <- paste0(context, " | keeping specific rows")
        }
        if(step$method == "remove_outliers_iqr" && !is.null(step$additional_info$outlier_vars)) {
          context <- paste0(context, " | IQR=", step$additional_info$iqr_multiplier %||% 1.5)
        }
        
        div(style = "display: flex; align-items: center; margin-bottom: 8px; padding: 5px; border-bottom: 1px solid #eee;",
            div(style = "width: 30px;", strong(paste0(i, "."))),
            div(style = "flex: 1;", method_name, context)
        )
      })
    )
  })
  
  
  # ----------------------------------------------------------------------------
  # 7.8 SAVED DATASETS MANAGEMENT - Save and Load Processed Data
  # ----------------------------------------------------------------------------
  
  # Saved datasets list UI
  output$saved_datasets_list <- renderUI({
    datasets <- saved_datasets()
    
    if (length(datasets) == 0) {
      return(div(class = "alert alert-info", 
                 "No saved datasets yet. Process data and save one above."))
    }
    
    tagList(lapply(names(datasets), function(name) {
      ds <- datasets[[name]]
      
      div(class = "well", style = "padding: 8px; margin-bottom: 8px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              div(
                strong(name, style = "color: #2C3E50;"),
                br(),
                tags$small(paste(ds$rows, "rows,", ds$cols, "cols |", ds$timestamp))
              ),
              actionButton(
                "load_dataset_trigger",
                label = "Load for Modeling",
                class = "btn-primary btn-xs",
                onclick = paste0(
                  "Shiny.setInputValue('load_dataset_trigger', '",
                  name, "', {priority: 'event'})"
                )
              )
          )
      )
    }))
  })
  
  
  # Load dataset trigger
  observeEvent(input$load_dataset_trigger, {
    ds_name <- input$load_dataset_trigger
    if (!is.null(ds_name) && ds_name != "" && ds_name %in% names(saved_datasets())) {
      selected_model_dataset(ds_name)
      showNotification(paste("Loaded dataset for modeling:", ds_name), type = "message")
    }
  })
  
  
  # Save current dataset manually
  observeEvent(input$save_dataset_btn, {
    dataset_name <- trimws(input$dataset_name)
    if(dataset_name == "") {
      showNotification("Please enter a dataset name", type = "error")
      return()
    }
    
    current_data <- processed_data_working()
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    full_name <- paste0(dataset_name, " (", timestamp, ")")
    
    current <- saved_datasets()
    current[[full_name]] <- list(
      data = current_data,
      name = dataset_name,
      timestamp = timestamp,
      rows = nrow(current_data),
      cols = ncol(current_data),
      log = tail(processing_log(), 20)
    )
    saved_datasets(current)
    
    # Update the picker input in the modeling tab
    updatePickerInput(session, "model_dataset_select", 
                      choices = names(saved_datasets()),
                      selected = full_name)
    
    showNotification(paste("Dataset saved as:", full_name), type = "message")
    add_to_log(paste("Saved dataset:", full_name))
  })
  
  
  # Model dataset selection observer
  observeEvent(input$model_dataset_select, {
    if(!is.null(input$model_dataset_select) && input$model_dataset_select != "") {
      selected_model_dataset(input$model_dataset_select)
      showNotification(paste("Selected dataset for modeling:", input$model_dataset_select), type = "message")
    }
  })
  
  
  # ----------------------------------------------------------------------------
  # 7.9 BATCH PROCESSOR FUNCTIONS - Quick Transformations
  # ----------------------------------------------------------------------------
  
  # Apply transformation
  observeEvent(input$proc_transform_btn, {
    req(input$proc_transform_column)
    
    current_data <- processed_data_working()
    
    if(input$proc_transform_column %in% names(current_data)) {
      col_data <- current_data[[input$proc_transform_column]]
      
      if(is.numeric(col_data)) {
        method <- input$proc_transform_method
        
        tryCatch({
          if(method == "log") {
            if(min(col_data, na.rm = TRUE) > 0) {
              current_data[[input$proc_transform_column]] <- log(col_data)
              add_to_log(paste("Applied log transformation to:", input$proc_transform_column))
              showNotification(paste("Log transformation applied to:", input$proc_transform_column), type = "message")
            } else {
              showNotification("Log transformation requires positive values (>0)", type = "error")
              return()
            }
          } else if(method == "sqrt") {
            if(min(col_data, na.rm = TRUE) >= 0) {
              current_data[[input$proc_transform_column]] <- sqrt(col_data)
              add_to_log(paste("Applied square root transformation to:", input$proc_transform_column))
              showNotification(paste("Square root transformation applied to:", input$proc_transform_column), type = "message")
            } else {
              showNotification("Square root requires non-negative values (>=0)", type = "error")
              return()
            }
          } else if(method == "square") {
            current_data[[input$proc_transform_column]] <- col_data^2
            add_to_log(paste("Applied square transformation to:", input$proc_transform_column))
            showNotification(paste("Square transformation applied to:", input$proc_transform_column), type = "message")
          } else if(method == "boxcox") {
            if(requireNamespace("MASS", quietly = TRUE)) {
              if(min(col_data, na.rm = TRUE) > 0) {
                bc <- MASS::boxcox(col_data ~ 1, plotit = FALSE)
                lambda <- bc$x[which.max(bc$y)]
                if(abs(lambda) < 0.01) {
                  current_data[[input$proc_transform_column]] <- log(col_data)
                } else {
                  current_data[[input$proc_transform_column]] <- (col_data^lambda - 1) / lambda
                }
                add_to_log(paste("Applied Box-Cox transformation (lambda =", round(lambda, 3), ") to:", input$proc_transform_column))
                showNotification(paste("Box-Cox transformation applied (lambda =", round(lambda, 3), ")"), type = "message")
              } else {
                showNotification("Box-Cox requires positive values (>0)", type = "error")
                return()
              }
            } else {
              showNotification("MASS package required for Box-Cox transformation", type = "error")
              return()
            }
          } else if(method == "yeojohnson") {
            if(requireNamespace("bestNormalize", quietly = TRUE)) {
              yj <- bestNormalize::yeojohnson(col_data)
              current_data[[input$proc_transform_column]] <- predict(yj, col_data)
              add_to_log(paste("Applied Yeo-Johnson transformation (lambda =", round(yj$lambda, 3), ") to:", input$proc_transform_column))
              showNotification(paste("Yeo-Johnson transformation applied (lambda =", round(yj$lambda, 3), ")"), type = "message")
            } else {
              showNotification("bestNormalize package required for Yeo-Johnson transformation", type = "error")
              return()
            }
          }
          
          processed_data_working(current_data)
          
        }, error = function(e) {
          showNotification(paste("Transformation error:", e$message), type = "error", duration = 5)
        })
      } else {
        showNotification("Selected column is not numeric", type = "error")
      }
    } else {
      showNotification(paste("Column not found:", input$proc_transform_column), type = "warning")
    }
  })
  
  
  # Numeric imputation
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
          showNotification(paste("Imputed", na_count, "missing values with median =", round(impute_val, 3)), type = "message")  
          
        } else if(method == "mean" && is.numeric(col_data)) {
          impute_val <- mean(col_data, na.rm = TRUE)
          current_data[[input$proc_impute_column]][is.na(col_data)] <- impute_val
          add_to_log(paste("Imputed", na_count, "missing values in", input$proc_impute_column, "with mean =", round(impute_val, 3)))
          showNotification(paste("Imputed", na_count, "missing values with mean =", round(impute_val, 3)), type = "message")
          
        } else if(method == "constant") {
          impute_val <- input$proc_impute_value
          current_data[[input$proc_impute_column]][is.na(col_data)] <- impute_val
          add_to_log(paste("Imputed", na_count, "missing values in", input$proc_impute_column, "with constant =", impute_val))
          showNotification(paste("Imputed", na_count, "missing values with constant =", impute_val), type = "message")
          
        } else {
          showNotification("Imputation method not compatible with this column type", type = "error")
          return()
        }
        
        processed_data_working(current_data)
        
      } else {
        showNotification(paste("No missing values found in column:", input$proc_impute_column), type = "message")
      }
    } else {
      showNotification(paste("Column not found:", input$proc_impute_column), type = "warning")
    }
  })
  
  
  # Categorical imputation
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
    
    col_data <- current_data[[column]]
    na_count <- sum(is.na(col_data))
    
    if(na_count == 0) {
      showNotification(paste("No missing values in column:", column), type = "message")
      return()
    }
    
    if(method == "mode") {
      tbl <- table(col_data)
      impute_val <- names(tbl)[which.max(tbl)]
      current_data[[column]][is.na(col_data)] <- impute_val
      add_to_log(paste("Imputed", na_count, "missing values in", column, "with mode =", impute_val))
      showNotification(paste("Imputed", na_count, "missing values with mode =", impute_val), type = "message")
      
    } else if(method == "new_category") {
      current_data[[column]] <- as.character(current_data[[column]])
      current_data[[column]][is.na(current_data[[column]])] <- "Missing"
      current_data[[column]] <- as.factor(current_data[[column]])
      add_to_log(paste("Imputed", na_count, "missing values in", column, "with new category 'Missing'"))
      showNotification(paste("Imputed", na_count, "missing values with new category 'Missing'"), type = "message")
      
    } else if(method == "constant") {
      current_data[[column]][is.na(col_data)] <- constant_value
      add_to_log(paste("Imputed", na_count, "missing values in", column, "with constant =", constant_value))
      showNotification(paste("Imputed", na_count, "missing values with constant =", constant_value), type = "message")
    }
    
    processed_data_working(current_data)
  })
  
  
  # Remove rows
  observeEvent(input$proc_remove_rows_btn, {
    current_data <- processed_data_working()
    
    row_missing <- apply(current_data, 1, function(x) sum(is.na(x)))
    rows_to_keep <- row_missing <= input$proc_max_missing_per_row
    rows_removed <- sum(!rows_to_keep)
    
    if(rows_removed > 0) {
      new_data <- current_data[rows_to_keep, , drop = FALSE]
      processed_data_working(new_data)
      add_to_log(paste("Removed", rows_removed, "rows with >=", input$proc_max_missing_per_row, "missing values"))
      showNotification(paste("Removed", rows_removed, "rows"), type = "message")
    } else {
      showNotification(paste("No rows exceeded the missing value threshold of", input$proc_max_missing_per_row), type = "message")
    }
  })
  
  
  # Remove column
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
      showNotification(paste("Removed column:", input$proc_remove_column), type = "message")
    } else {
      showNotification(paste("Column not found:", input$proc_remove_column), type = "warning")
    }
  })
  
  
  # Batch apply all quick actions
  observeEvent(input$proc_apply_all, {
    data <- standardize_missing(df)
    add_to_log("Starting batch processing with standardized data")
    
    if(!is.null(input$proc_impute_column) && input$proc_impute_column != "" && 
       input$proc_impute_column %in% names(data) && any(is.na(data[[input$proc_impute_column]]))) {
      if(is.numeric(data[[input$proc_impute_column]])) {
        impute_val <- median(data[[input$proc_impute_column]], na.rm = TRUE)
        na_count <- sum(is.na(data[[input$proc_impute_column]]))
        data[[input$proc_impute_column]][is.na(data[[input$proc_impute_column]])] <- impute_val
        add_to_log(paste("Imputed", na_count, "missing values in", input$proc_impute_column, "with median =", round(impute_val, 3)))
      }
    }
    
    if(!is.null(input$proc_transform_column) && input$proc_transform_column != "" && 
       input$proc_transform_column %in% names(data) && is.numeric(data[[input$proc_transform_column]])) {
      col_data <- data[[input$proc_transform_column]]
      if(min(col_data, na.rm = TRUE) > 0) {
        data[[input$proc_transform_column]] <- log(col_data)
        add_to_log(paste("Applied log transformation to:", input$proc_transform_column))
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
    add_to_log("--- Batch processing complete ---")
    showNotification("Batch processing applied!", type = "message", duration = 5)
  })
  
  
  # Reset to original data
  observeEvent(input$proc_reset_all, {
    processed_data_working(standardize_missing(df))
    add_to_log("--- Reset to original data ---")
    showNotification("Reset to original data", type = "message")
  })
  
  
  # ----------------------------------------------------------------------------
  # 7.10 DATA PREVIEW AND SUMMARY OUTPUTS
  # ----------------------------------------------------------------------------
  
  # Data summary output
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
  })
  
  
  # Data preview output
  output$proc_data_preview <- renderDT({
    data <- processed_data_working()
    datatable(head(data, 20), 
              options = list(scrollX = TRUE, pageLength = 10, dom = 't'), 
              rownames = FALSE)
  })
  
  
  # Processing log output
  output$proc_log <- renderPrint({
    log_entries <- processing_log()
    if(length(log_entries) == 0) {
      cat("No processing actions yet.\nUse the controls above to modify the data.")
    } else {
      cat(paste(log_entries, collapse = "\n"))
    }
  })
  
  
  # Download handler
  output$proc_download_data <- downloadHandler(
    filename = function() {
      paste("processed_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_data_working(), file, row.names = FALSE)
    }
  )
  
  
  # ----------------------------------------------------------------------------
  # 7.11 BATCH PROCESSOR OUTPUTS (for the Batch Processor sub-tab)
  # ----------------------------------------------------------------------------
  
  output$proc_log_batch <- renderPrint({
    log_entries <- processing_log()
    if(length(log_entries) == 0) {
      cat("No processing actions yet.\nUse the controls above to modify the data.")
    } else {
      cat(paste(log_entries, collapse = "\n"))
    }
  })
  
  
  output$proc_data_summary_batch <- renderPrint({
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
  })
  
  
  output$proc_data_preview_batch <- renderDT({
    data <- processed_data_working()
    datatable(head(data, 20), 
              options = list(scrollX = TRUE, pageLength = 10, dom = 't'), 
              rownames = FALSE)
  })
  
  
  # ----------------------------------------------------------------------------
  # 7.12 TAB SWITCH WARNING - Remind to save recipes
  # ----------------------------------------------------------------------------
  
  # Warn when leaving Recipe Builder with unsaved changes
  observeEvent(input$processing_approach, {
    steps <- pipeline_steps()
    if(length(steps) > 0) {
      showNotification("Remember to save your recipe before switching tabs!", 
                       type = "warning", duration = 3)
    }
  })
  
  
  # ----------------------------------------------------------------------------
  # 7.13 PERSISTENT STORAGE - Save/Load Recipes to File
  # ----------------------------------------------------------------------------
  
    # Save all pipelines to a CSV file
  save_pipelines_to_file <- function() {
    pipelines <- saved_pipelines()
    if(length(pipelines) == 0) return()
    
    # Convert pipelines to a data frame for saving
    pipelines_df <- data.frame()
    for(name in names(pipelines)) {
      pipeline <- pipelines[[name]]
      # Ensure steps is valid before converting to JSON
      if(is.list(pipeline$steps)) {
        # Create a simplified version of steps for JSON
        steps_simple <- list()
        for(i in seq_along(pipeline$steps)) {
          if(is.list(pipeline$steps[[i]])) {
            steps_simple[[i]] <- list(
              id = pipeline$steps[[i]]$id,
              method = pipeline$steps[[i]]$method,
              cols = pipeline$steps[[i]]$cols %||% character(0),
              additional_info = pipeline$steps[[i]]$additional_info,
              skip_test = pipeline$steps[[i]]$skip_test %||% FALSE
            )
          }
        }
        steps_json <- jsonlite::toJSON(steps_simple, auto_unbox = TRUE)
      } else {
        steps_json <- "[]"
      }
      
      pipelines_df <- rbind(pipelines_df, data.frame(
        name = name,
        created = as.character(pipeline$created),
        steps = steps_json,
        stringsAsFactors = FALSE
      ))
    }
    
    # Save to a CSV file in the app directory
    write.csv(pipelines_df, "saved_recipes.csv", row.names = FALSE)
    
    # Use tryCatch for logging since this might be called from non-reactive context
    tryCatch({
      add_to_log("Saved all recipes to file")
    }, error = function(e) {
      # Silently fail - logging not critical
    })
  }
  
  # Auto-save pipelines whenever they change
  observeEvent(saved_pipelines(), {
    save_pipelines_to_file()
  }, ignoreInit = TRUE)
  
  
  
  # Load pipelines on app startup - wrap in observe to make it reactive
  observe({
    # Only run once when the app starts
    isolate({
      load_pipelines_from_file()
    })
  })
  
  # Load pipelines from CSV file
  load_pipelines_from_file <- function() {
    if(file.exists("saved_recipes.csv")) {
      pipelines_df <- read.csv("saved_recipes.csv", stringsAsFactors = FALSE)
      loaded_pipelines <- list()
      
      for(i in 1:nrow(pipelines_df)) {
        name <- pipelines_df$name[i]
        steps_json <- pipelines_df$steps[i]
        
        # Parse JSON safely
        steps <- tryCatch({
          jsonlite::fromJSON(steps_json)
        }, error = function(e) {
          NULL
        })
        
        # Ensure steps have the correct structure and are a list
        if(!is.null(steps) && is.list(steps)) {
          # If steps is a named list, convert to unnamed list if needed
          if(!is.null(names(steps)) && length(steps) > 0) {
            # Convert named list to unnamed list of steps
            steps_list <- list()
            for(j in seq_along(steps)) {
              if(is.list(steps[[j]])) {
                steps_list[[j]] <- steps[[j]]
              }
            }
            steps <- steps_list
          }
          
          loaded_pipelines[[name]] <- list(
            steps = steps,
            created = as.POSIXct(pipelines_df$created[i]),
            log = character(0)
          )
        }
      }
      
      if(length(loaded_pipelines) > 0) {
        saved_pipelines(loaded_pipelines)
        
        # Use tryCatch for logging since this might be called from non-reactive context
        tryCatch({
          add_to_log(paste("Loaded", length(loaded_pipelines), "recipes from file"))
        }, error = function(e) {
          # Silently fail - logging not critical
        })
      }
    }
  }
  
  
  # Delete pipeline/recipe - Add this after the load_pipeline_trigger observer
  observeEvent(input$delete_pipeline_trigger, {
    pipeline_name <- input$delete_pipeline_trigger
    if(!is.null(pipeline_name) && pipeline_name != "" && pipeline_name %in% names(saved_pipelines())) {
      # Show confirmation dialog
      showModal(modalDialog(
        title = "Delete Recipe",
        paste("Are you sure you want to delete the recipe:", pipeline_name, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete_pipeline", "Delete", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
      
      # Store the name to delete
      pipeline_to_delete <- pipeline_name
      
      # Handle confirmation
      observeEvent(input$confirm_delete_pipeline, {
        # Remove the pipeline
        current <- saved_pipelines()
        current[[pipeline_to_delete]] <- NULL
        saved_pipelines(current)
        
        # Save to file after deletion
        if(exists("save_pipelines_to_file")) {
          save_pipelines_to_file()
        }
        
        showNotification(paste("Recipe", pipeline_to_delete, "deleted"), type = "message")
        add_to_log(paste("Deleted recipe:", pipeline_to_delete))
        
        # Remove the modal
        removeModal()
      }, once = TRUE)
    }
  })
  
  
  
  
  # ============================================================================
  # ============================================================================
  # SECTION 8: REPORT SUMMARY OUTPUTS
  # ============================================================================
  # ============================================================================
  
  output$report_data_desc <- renderText({ 
    paste("Dataset contains", nrow(df), "rows and", ncol(df), "columns. ",
          "The outcome variable is DEATH_RATE (projected death rate across ten years). ",
          "There are", length(numeric_cols), "numeric predictors and", length(categorical_cols), 
          "categorical variables including the state CODE and OBS_TYPE for train/test allocation.")
  })
  
  output$report_missing_strategy <- renderText({ 
    paste("Missing values are represented by '-99', '--', and 'NA' strings. ",
          "Users can remove columns exceeding missing thresholds, remove rows with too many missing values, ",
          "impute numeric missing values using median/mean/constant, ",
          "or impute categorical missing values using mode/new category/constant.")
  })
  
  output$report_outlier_strategy <- renderText({ 
    paste("Outlier detection using multiple methods: Mahalanobis Distance, Cook's Distance, ",
          "Local Outlier Factor (LOF), One-Class SVM, Random Forest Residuals, and Isolation Forest. ",
          "Users can adjust thresholds and view consensus across methods.")
  })
  
  output$report_preprocess_strategy <- renderText({ 
    paste("Data preprocessing includes missing value imputation, transformation options (log, sqrt, Box-Cox, Yeo-Johnson), ",
          "scaling/centering, and a pipeline builder for reproducible processing steps.")
  })
  
  output$report_glmnet_explanation <- renderText({ 
    paste("GLMNET (Elastic Net) combines L1 (Lasso) and L2 (Ridge) regularization. ",
          "Alpha controls the mix (0 = Ridge, 1 = Lasso). ",
          "Cross-validation selects the optimal lambda parameter to balance bias and variance.")
  })
  
  output$report_model_performance <- renderText({ 
    res <- model_results()
    if(is.null(res)) return("Train a model in the GLMNET Modeling tab to see performance metrics.")
    paste("Model trained with", length(res$predictors), "predictors. ",
          "Test RMSE =", round(res$test_rmse, 4), 
          ", Test R² =", round(res$test_r2, 4), ".")
  })
  
  output$report_residual_summary <- renderText({ 
    res <- model_results()
    if(is.null(res) || is.null(res$test_pred)) return("Train a model first to see residual analysis.")
    residuals <- res$y_test - res$test_pred
    paste("Test residuals analysis: mean =", round(mean(residuals), 4), 
          ", sd =", round(sd(residuals), 4),
          ", min =", round(min(residuals), 4), 
          ", max =", round(max(residuals), 4), ".")
  })
  
  
  # ============================================================================
  # ============================================================================
  # SECTION 9: GLMNET MODELING OUTPUTS
  # ============================================================================
  # ============================================================================
  
  model_ready_data <- reactive({
    ds_name <- selected_model_dataset()
    if(!is.null(ds_name) && ds_name %in% names(saved_datasets())) {
      return(saved_datasets()[[ds_name]]$data)
    }
    return(processed_data_working())
  })
  
  observe({
    # Determine which data to use based on selection
    if(input$model_data_source == "saved" && !is.null(selected_model_dataset()) && selected_model_dataset() %in% names(saved_datasets())) {
      # Use saved dataset
      data <- saved_datasets()[[selected_model_dataset()]]$data
      showNotification(paste("Using saved dataset:", selected_model_dataset()), type = "message", duration = 3)
    } else if(input$model_data_source == "processed") {
      # Use current processed data (already standardized)
      data <- processed_data_working()
    } else {
      # Use original data with standardization
      data <- standardize_missing(df)
    }
    
    # Ensure DEATH_RATE is numeric
    if("DEATH_RATE" %in% names(data) && is.character(data$DEATH_RATE)) {
      data$DEATH_RATE[data$DEATH_RATE %in% c("-99", "--", "NA")] <- NA
      data$DEATH_RATE <- suppressWarnings(as.numeric(data$DEATH_RATE))
    }
    
    # Remove rows with NA in DEATH_RATE
    data <- data[!is.na(data$DEATH_RATE), ]
    
    # Create train/test split
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
      
      showNotification(paste("Model trained! Test RMSE:", ifelse(is.na(test_rmse), "N/A", round(test_rmse, 4))), type = "message", duration = 5)
      
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
    
    colors <- ifelse(coef_df$Coefficient > 0, "#ADD8E6", "coral")
    
    plot_ly(coef_df, x = ~Coefficient, y = ~reorder(Variable, Coefficient), type = "bar", orientation = "h",
            marker = list(color = colors), text = ~paste(Variable, ": ", round(Coefficient, 4)), hoverinfo = "text") %>%
      layout(title = "Coefficient Estimates", xaxis = list(title = "Coefficient Value", gridcolor = "lightgray"),
             yaxis = list(title = "", gridcolor = "lightgray"), plot_bgcolor = "white", margin = list(l = 120))
  })
  
  output$predictions_plot <- renderPlotly({
    res <- model_results()
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "No model trained", x = 0.5, y = 0.5))
    
    split <- train_test_split()
    if(is.null(split)) return(plot_ly() %>% add_annotations(text = "No train/test split available", x = 0.5, y = 0.5))
    
    train_data <- split$train
    test_data <- split$test
    
    # Get CODE values safely
    train_codes <- tryCatch({
      complete_idx <- complete.cases(train_data[, res$predictors, drop = FALSE])
      codes <- as.character(train_data$CODE[complete_idx])
      if(length(codes) > length(res$train_pred)) codes[1:length(res$train_pred)] else codes
    }, error = function(e) rep("Unknown", length(res$train_pred)))
    
    test_codes <- tryCatch({
      if(!is.null(res$test_pred) && length(res$test_pred) > 0) {
        complete_idx <- complete.cases(test_data[, res$predictors, drop = FALSE])
        codes <- as.character(test_data$CODE[complete_idx])
        if(length(codes) > length(res$test_pred)) codes[1:length(res$test_pred)] else codes
      } else {
        character(0)
      }
    }, error = function(e) rep("Unknown", length(res$test_pred)))
    
    # Build the plot data frame safely
    plot_df <- data.frame(
      Actual = c(res$y_train, if(!is.null(res$y_test)) res$y_test else numeric(0)),
      Predicted = c(res$train_pred, if(!is.null(res$test_pred)) res$test_pred else numeric(0)),
      Set = c(rep("Train", length(res$y_train)), if(!is.null(res$y_test)) rep("Test", length(res$y_test)) else character(0)),
      CODE = c(train_codes, if(length(test_codes) > 0) test_codes else character(0)),
      stringsAsFactors = FALSE
    )
    
    # Remove any rows with NA
    plot_df <- na.omit(plot_df)
    
    if(nrow(plot_df) == 0) {
      return(plot_ly() %>% add_annotations(text = "No valid prediction data available", x = 0.5, y = 0.5))
    }
    
    p <- ggplot(plot_df, aes(x = Actual, y = Predicted, color = Set, 
                             text = paste("CODE:", CODE, "<br>Set:", Set, "<br>Actual:", round(Actual, 3), "<br>Predicted:", round(Predicted, 3)))) +
      geom_point(alpha = 0.6, size = 2) + 
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", size = 1) +
      labs(title = "Predictions vs Actual Values", x = "Actual Death Rate", y = "Predicted Death Rate") +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white", font = list(size = 10)))
  })
  
  
  output$residual_plot <- renderPlotly({
    res <- model_results()
    if(is.null(res)) return(plot_ly() %>% add_annotations(text = "No model trained", x = 0.5, y = 0.5))
    
    split <- train_test_split()
    if(is.null(split)) return(plot_ly() %>% add_annotations(text = "No train/test split available", x = 0.5, y = 0.5))
    
    train_residuals <- res$y_train - res$train_pred
    
    # Get train codes safely
    train_codes <- tryCatch({
      complete_idx <- complete.cases(split$train[, res$predictors, drop = FALSE])
      codes <- as.character(split$train$CODE[complete_idx])
      if(length(codes) > length(train_residuals)) codes[1:length(train_residuals)] else codes
    }, error = function(e) rep("Unknown", length(train_residuals)))
    
    plot_df <- data.frame(
      Residuals = train_residuals,
      Set = "Train",
      Predicted = res$train_pred,
      CODE = train_codes,
      stringsAsFactors = FALSE
    )
    
    if(!is.null(res$test_pred) && length(res$test_pred) > 0) {
      test_residuals <- res$y_test - res$test_pred
      test_codes <- tryCatch({
        complete_idx <- complete.cases(split$test[, res$predictors, drop = FALSE])
        codes <- as.character(split$test$CODE[complete_idx])
        if(length(codes) > length(test_residuals)) codes[1:length(test_residuals)] else codes
      }, error = function(e) rep("Unknown", length(test_residuals)))
      
      plot_df <- rbind(plot_df, data.frame(
        Residuals = test_residuals,
        Set = "Test",
        Predicted = res$test_pred,
        CODE = test_codes,
        stringsAsFactors = FALSE
      ))
    }
    
    # Remove NA rows
    plot_df <- na.omit(plot_df)
    
    if(nrow(plot_df) == 0) {
      return(plot_ly() %>% add_annotations(text = "No valid residual data available", x = 0.5, y = 0.5))
    }
    
    p <- ggplot(plot_df, aes(x = Predicted, y = Residuals, color = Set, 
                             text = paste("CODE:", CODE, "<br>Predicted:", round(Predicted, 3), "<br>Residual:", round(Residuals, 3)))) +
      geom_point(alpha = 0.6, size = 2) + 
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 1) +
      labs(title = "Residuals vs Fitted Values", x = "Predicted Death Rate", y = "Residuals") +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
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
    
    # Get the CODE values for the outlier observations
    # Need to access the original test data to get CODE
    split <- train_test_split()
    if(!is.null(split) && !is.null(split$test)) {
      test_data <- split$test
      # Get the predictor variables used in the model
      predictor_vars <- res$predictors
      # Find complete cases in test data
      complete_test <- complete.cases(test_data[, predictor_vars, drop = FALSE])
      test_data_complete <- test_data[complete_test, ]
      
      # Get CODE values for outlier indices
      outlier_codes <- test_data_complete$CODE[outlier_idx]
      # If CODE is missing or not available, use index as fallback
      if(is.null(outlier_codes) || length(outlier_codes) != length(outlier_idx)) {
        outlier_codes <- outlier_idx
      } else if(any(is.na(outlier_codes))) {
        outlier_codes[is.na(outlier_codes)] <- outlier_idx[is.na(outlier_codes)]
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
      geom_ribbon(aes(ymin = cvlo, ymax = cvup), alpha = 0.3, fill = "#ADD8E6") +
      geom_line(color = "#ADD8E6", size = 1) + geom_point(size = 1.5) +
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
  
}  # Close server function