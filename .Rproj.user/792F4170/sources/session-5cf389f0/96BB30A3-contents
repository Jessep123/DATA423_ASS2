
shinyServer(function(input, output, session) {
  
# ================================================================================
#       RESETTING INPUTS
# ================================================================================
  
  #Action button event to reset all inputs across whole page
  observeEvent(input$reset_input_all, {
    reset("")
  })
  
  #Helper function to reset all filter columns in a page
  reset_filters <- function(label){
    for (col in names(data)){
      input <- paste0("selected_", tolower(col), "_", label)
      reset(input)
    }
  }

# ================================================================================
#       REACTIVE DATASETS
# ================================================================================
  #Helper function to filter reactive datasets based on tab label
  # Apply categorical and numeric filters for a given tab label to any dataframe
  filter_data <- function(df, label) {
    
    # Categorical filters
    for (col in cat_filter_cols) {
      if (!col %in% names(df)) next
      
      input_name <- paste0("selected_", tolower(col), "_", label)
      selected_vals <- input[[input_name]]
      
      if (!is.null(selected_vals) && length(selected_vals) > 0) {
        df <- df %>%
          filter(
            (.data[[col]] %in% selected_vals) |
              (is.na(.data[[col]]) & "NA" %in% selected_vals)
          )
      }
    }
    
    # Numeric filters
    for (col in num_filter_cols) {
      if (!col %in% names(df)) next
      
      # Use the dataframe being filtered, not global data
      full_min <- min(df[[col]], na.rm = TRUE)
      full_max <- max(df[[col]], na.rm = TRUE)
      
      input_name <- paste0("selected_", tolower(col), "_", label)
      selected_vals <- input[[input_name]]
      
      if (!is.null(selected_vals) &&
          length(selected_vals) == 2 &&
          (selected_vals[1] > full_min || selected_vals[2] < full_max)) {
        
        df <- df %>%
          filter(
            .data[[col]] >= selected_vals[1] &
              .data[[col]] <= selected_vals[2]
          )
      }
    }
    
    df
  }
  
  
  #Helper function to create reactive datasets  
  
  #Returns dataset filtered by tab filters with selected columns
  reactive_dataset <- function(label) {
      
      reactive({
        
        df <- data
        
        # Input ID's
        cat_id <- paste0("selected_vars_categorical_", label)
        num_id <- paste0("selected_vars_numeric_", label)
        

        # Selected input columns
        cat_cols <-  input[[cat_id]]
        num_cols <-  input[[num_id]]
        
        
        #If cat_col input is null, all categorical variables are available for filtering
        #Used for tabs that don't require selection of categorical variables (i.e. scatterplot)
        if (is.null(cat_cols)){cat_cols <-  cat_filter_cols}
        

        #applying reactive filter function 
        df <- filter_data(df, label)

        
        #Special condition for selecting columns for plots based on requirements
        #Updated as app was developed
        if (label == "ggpairs"){
        #Selected columns to be return for plot
          #Unique() used to remove double ups of colour/input[[cat_id]]
        selected_vars <- unique(c(
          input[[cat_id]],
          num_cols,
          #If a colourby variable is selected, return that
          if (input$ggpairs_colourby != "None") input$ggpairs_colourby
        )
        )
        } else if(label == "mosaic"){

          #Only return categorical cols
          selected_vars <- unlist(c(
            cat_cols
          ), use.names = FALSE)
        }
        else if (label == "counts_over_time"){
          selected_vars <- unlist(c(
            cat_cols,
            input$selected_x_scatter,
            input$selected_y_scatter
          ), use.names = FALSE)}
        else {
          #Selected columns to be return for plot
          #If not special case, just return selected inputs
          selected_vars <- unlist(c(
            input[[cat_id]],
            num_cols
          ), use.names = FALSE)}
        

        
        #If no variables are selected return empty dataframe
        if (length(selected_vars) == 0) {
          return(df[, 0, drop = FALSE])
        }

        df %>% select(all_of(selected_vars))
      })
    }
    
  #Reactive datasets used in each tab
  #Label refers to which tab it is related too
    reactive_missing <- reactive_dataset("missing")
    reactive_rising <- reactive_dataset("rising")
    reactive_ggpairs <- reactive_dataset("ggpairs")
    reactive_boxplot <- reactive_dataset("boxplot")
    reactive_mosaic <- reactive_dataset("mosaic")
    reactive_data <- reactive_dataset("data")
    reactive_counts_over_time <- reactive_dataset("counts_over_time") #this is the scatterplot
    reactive_corr <- reactive_dataset("corr")
    
    #Processing dataset
    reactive_missing_processing <- reactive_dataset("missing_processing")
    

# ================================================================================
#       PROCESSING SECTION DATATABLES
# ================================================================================
    
    #Reactive dataset for processing missing data - First stage of pipeline
    processed_missing_reactive <- reactive({
      df <- reactive_missing_processing()

      
      # Column missingness percentages
      missing_col_range <- input$missing_col_threshold_processing
      col_missing_pct <- colMeans(is.na(df)) * 100
      
      keep_names <- names(col_missing_pct)[
        col_missing_pct >= missing_col_range[1] &
          col_missing_pct <= missing_col_range[2]
      ]
      
      # Always keep missing_count
      keep_names <- union(keep_names, "missing_count")
      
      # Only keep names that actually exist
      keep_names <- intersect(keep_names, names(df))
      
      req(length(keep_names) > 0)
      
      df <- df[, keep_names, drop = FALSE]
      
      
      
      #Calculating missing count for each row
      df <- df %>%
        #Sum total of columns with a missing value
        mutate(missing_count = rowSums(is.na(.)))
      
      #Object for threshold of missing values per row
      missing_row_threshold <- input$missing_threshold_processing 
      
      #filtering rows that don't meet the missing value threshold
        df <- df  %>%
          #Removing rows that have more missing variables than threshold
          filter(
            missing_count >= missing_row_threshold[1],
            missing_count <= missing_row_threshold[2]
          )     
        
      
      
      df
      
    })
    
    #Updating outlier processing select inputs based on output of processed_missing_reactive()
    observe({
      df <- processed_missing_reactive()
      
      req(df)
      
      #dropping missing_count
      df$missing_count <-  NULL
      
      numeric_choices <- names(df)[sapply(df, is.numeric)]
      categorical_choices <- names(df)[sapply(df, is.factor)]
      
      updatePickerInput(
        session,
        inputId = "selected_vars_numeric_outlier_processing",
        choices = numeric_choices,
        selected = numeric_choices
      )
      
      updatePickerInput(
        session,
        inputId = "selected_vars_categorical_outlier_processing",
        choices = categorical_choices,
        selected = categorical_choices
      )
    })
    
    
    #Second stage of pipeline - dependant on processed_missing_reactive
    #Base reactive table without any imputations applied
    processed_outlier_reactive <-  reactive({
      
      #Takes result of missing processing section and goes from there
      df <- processed_missing_reactive()

      #Applying any tab filters
      df <- filter_data(df, "outlier_processing")
      
      #Only returning selected columns from page inputs
      selected_vars <- unlist(c(
        input$selected_vars_numeric_outlier_processing,
        input$selected_vars_categorical_outlier_processing
      ), use.names = FALSE)
      
      selected_vars <- intersect(selected_vars, names(df))
      
      if (length(selected_vars) == 0) {
        return(df)
      }
      df <- df %>% select(all_of(selected_vars))
      
      
      #Imputation changes for columns 
      missing_value_imputations <- outlier_impute_settings()
      
      if (is.null(missing_value_imputations)) {
        return(df)
      }
      

      for (imputation in missing_value_imputations) {
        cols <- intersect(imputation[[2]], names(df))
        if (length(cols) == 0) next
        
        if (imputation[[1]] == "Manual") {
          for (col in cols) {
            df[[col]][is.na(df[[col]])] <- imputation[[3]]
          }
        }
        if (imputation[[1]] == "Median") {
          for (col in cols) {
            if (is.numeric(df[[col]])) {
              med <- median(df[[col]], na.rm = TRUE)
              df[[col]][is.na(df[[col]])] <- med
            }
          }
        }
        if (imputation[[1]] == "Mean") {
          for (col in cols) {
            if (is.numeric(df[[col]])) {
              mean <- mean(df[[col]], na.rm = TRUE)
              df[[col]][is.na(df[[col]])] <- mean
            }
          }
        }
        if (imputation[[1]] == "KNN") {
          knn_cols <- cols[cols %in% names(df) & sapply(df[cols], is.numeric)]
          
          # remove columns with no observed values
          knn_cols <- knn_cols[colSums(!is.na(df[knn_cols])) > 0]
          
          if (length(knn_cols) > 0) {
            temp <- df[, knn_cols, drop = FALSE]
            na_mask <- is.na(temp)
            
            if (any(na_mask)) {
              temp_imputed <- VIM::kNN(
                temp,
                variable = knn_cols,
                k = input$knn_neighbours,
                imp_var = FALSE
              )
              for (col in knn_cols) {
                df[[col]][na_mask[, col]] <- temp_imputed[[col]][na_mask[, col]]
              }
            }
          }
        }
       }
      
      df
    })
    
    #Impute missing variables options change depending on processed_missing_reactive()
    observe({
      df <- processed_outlier_reactive()
      
      req(df)
      
      #dropping missing_count
      df$missing_count <-  NULL
      
      choices <- names(df)[colSums(is.na(df)) > 0]
      
      updatePickerInput(
        session,
        inputId = "selected_vars_impute_missing",
        choices = choices,
        selected = NULL
      )
    })
    
    #Transform variables options change depending on processed_outlier_reactive()
    observe({
      df <- processed_outlier_reactive()
      
      req(df)
      
      #dropping missing_count
      df$missing_count <-  NULL
      
      choices <- names(df[, sapply(df, is.numeric)])
      
      updatePickerInput(
        session,
        inputId = "selected_vars_transform",
        choices = choices,
        selected = NULL
      )
    })
    
    #List of imputation changes for reactive dataset
    outlier_impute_settings <- reactiveVal(list())
    
    #Everytime apply is selected, add specified imputation 
    observeEvent(input$impute_missing_values, {
      current_steps <- outlier_impute_settings()
      
      new_step <- list(
        method <-  input$missing_imputate_method,
        cols <-  input$selected_vars_impute_missing,
        manual_val <-  input$missing_impute_manual_value,
        knn_neighbours <- input$knn_neighbours
      )

      
      outlier_impute_settings(append(current_steps, list(new_step)))
    })
    
    
    #Event to reset imputations
    observeEvent(input$reset_missing_imputes, {
      outlier_impute_settings(list())
    })
    
    #ADD IN EVENTS/OBSERVATIONS FOR TRANSFORMING VARIABLES HERE
    
    #Reactive data table for checking data in processing data section
    output$missing_processing_reactive_table <- renderDataTable({
      processed_missing_reactive()
    })
    
    #Reactive data table for checking data in processing data section
    output$outlier_processing_reactive_table <- renderDataTable({
      processed_outlier_reactive()
    })
    
  # ================================================================================
  #       DYNAMIC VARIABLE FILTERS
  # ================================================================================
  
    #Function to make dynamic categorical filters
    make_dynamic_filters_categorical <- function(label) {
      
     #Dynamically create pickerinput select boxes for each filter variable defined
      #Creates full list for each tab with label at end
      #Can be iterated through in reactive dataset function
      tagList(lapply(cat_filter_cols, function(col) {
        input_id <- paste0("selected_", tolower(col), "_", label)
          selectizeInput(
            inputId = input_id,
            label = col,
            choices = unique(data[[col]]),
            multiple = TRUE,
            selected = NULL
          )
      }))
    }
    
    #UI outputs using dynamic filters function for different tabs
    output$dynamic_filters_categorical_missing <- renderUI(make_dynamic_filters_categorical("missing"))
    output$dynamic_filters_categorical_rising  <- renderUI(make_dynamic_filters_categorical("rising"))
    output$dynamic_filters_categorical_ggpairs <- renderUI(make_dynamic_filters_categorical("ggpairs"))
    output$dynamic_filters_categorical_boxplot <- renderUI(make_dynamic_filters_categorical("boxplot"))
    output$dynamic_filters_categorical_mosaic <- renderUI(make_dynamic_filters_categorical("mosaic"))
    output$dynamic_filters_categorical_data <- renderUI(make_dynamic_filters_categorical("data"))
    output$dynamic_filters_categorical_counts_over_time <- renderUI(make_dynamic_filters_categorical("counts_over_time"))
    output$dynamic_filters_categorical_corr <- renderUI(make_dynamic_filters_categorical("corr"))
    
    #Dynamic filters for assignment 2
    output$dynamic_filters_categorical_missing_processing <- renderUI(make_dynamic_filters_categorical("missing_processing"))
    output$dynamic_filters_categorical_outlier_processing <- renderUI(make_dynamic_filters_categorical("outlier_processing"))
    
    
    #Function to make dynamic numeric filters
    make_dynamic_filters_numeric <- function(label) {
      
      #Dynamically create pickerinput select boxes for each filter variable defined
      #Creates full list for each tab with label at end
      #Can be iterated through in reactive dataset function
      tagList(
          lapply(num_filter_cols, function(col) {
        input_id <- paste0("selected_", tolower(col), "_", label)
        
        #Setting min/max values. ROunded up/down depending
        var_min <- floor(min(data[[col]], na.rm = TRUE))
        var_max <- ceiling(max(data[[col]], na.rm = TRUE))
        
        sliderInput(
          inputId = input_id,
          label = col,
          min = var_min,
          max = var_max,
          value = c(var_min, var_max),
          ticks = FALSE
        )
      })
      )
    }
    #UI outputs using dynamic filters function for different tabs
    output$dynamic_filters_numeric_missing <- renderUI(make_dynamic_filters_numeric("missing"))
    output$dynamic_filters_numeric_rising  <- renderUI(make_dynamic_filters_numeric("rising"))
    output$dynamic_filters_numeric_ggpairs <- renderUI(make_dynamic_filters_numeric("ggpairs"))
    output$dynamic_filters_numeric_boxplot <- renderUI(make_dynamic_filters_numeric("boxplot"))
    output$dynamic_filters_numeric_mosaic <- renderUI(make_dynamic_filters_numeric("mosaic"))
    output$dynamic_filters_numeric_data <- renderUI(make_dynamic_filters_numeric("data"))
    output$dynamic_filters_numeric_counts_over_time <- renderUI(make_dynamic_filters_numeric("counts_over_time"))
    output$dynamic_filters_numeric_corr <- renderUI(make_dynamic_filters_numeric("corr"))

    #For assignment 2/processing section
    output$dynamic_filters_numeric_missing_processing <- renderUI(make_dynamic_filters_numeric("missing_processing"))
    output$dynamic_filters_numeric_outlier_processing <- renderUI(make_dynamic_filters_numeric("outlier_processing"))
    
  
  
  # ================================================================================
  #       SUMMARIES
  # ================================================================================
  
    #Total summary table, not used in final product 
   output$summary_table <- renderUI({
     data %>% 
       summarytools::dfSummary(col.widths = c(10,80,150,120,120,180,220)) %>%
       summarytools::view(, method = "render")
   })

    #Summary boxes for quick insights
    output$variable_summary <- renderUI({
      layout_columns(
        value_box("Rows", nrow(data)),
        value_box("Variables", ncol(data)),
        value_box("Numeric Variables", sum(sapply(data, is.numeric))),
        value_box("Categorical Variables", sum(sapply(data, is.factor))),
        value_box("Date Variables", sum(sapply(data, is.Date))),
        col_widths = c(2, 2, 2, 2, 2),
        gap = "10px"
        
      )
    })
    
    #Numeric variable summary
    output$numeric_summary <- renderUI({
      data %>%
        select(where(is.numeric)) %>% 
        summarytools::dfSummary(col.widths = c(10,80,200,120,120,10,10),
                                graph.magnif = 0.5) %>%
        summarytools::view(
          method = "render")
    })
    
    #Categorical variable summary
    output$categorical_summary <- renderUI({
      data %>%
        select(where(is.factor)) %>% 
        summarytools::dfSummary(col.widths = c(10,80,150,120,120,180,220),
                                graph.magnif = 0.5) %>%
        summarytools::view(, method = "render")
      })
    
    #Date summary
    output$date_summary <- renderUI({
      data %>%
        select(where(~inherits(.x, "Date"))) %>% 
        summarytools::dfSummary(col.widths = c(10,80,150,120,120,180,220)) %>%
        summarytools::view(, method = "render")
    })
    

    #Plotly graph of date counts for analysing distribution of rows over months
    output$date_counts <- renderPlotly({
      #Creating a bar chart of row counts for each month across the data range
     plot <-  data %>% 
        mutate(YearMonth = floor_date(Date, "month")) %>%
        group_by(YearMonth) %>%
        summarise(Month_Count = n(), .groups = "drop") %>%
        ggplot(aes(x = YearMonth, y = Month_Count)) +
        geom_col()+
        
        labs(
          x = "Date (6 Month Breaks)",
          y = "Number of Rows",
          title = paste0("Total Row Counts by Month\n",
                         min(data$Date), " - ", max(data$Date))
        ) +
       scale_x_date(
          date_breaks = "6 months",
          date_labels = "%b\n%Y"
        ) +
       
       theme_minimal() +
       theme(
         plot.title = element_text(hjust = 0.5, face = "bold"),
         plot.margin = margin(t = 50)
             )
     
     ggplotly(plot) %>% 
      #Readjusting title positions so they look a bit nicer
       layout(
         margin = list(t = 90),  
         xaxis = list(
           title = list(standoff = 30)
         ),
         yaxis = list(
           title = list(standoff = 30)
         )
       )
    })
    
# ================================================================================
#       Data Export
# ================================================================================
    
    #Reactive table to visualize data that will be exported
    data_export_table <- reactive({
      
      df <- reactive_data()
      
      #If scale/centre selected, apply it to data
      if (input$center_data_export || input$scale_data_export) {
        
        num_cols <- sapply(df, is.numeric)
        
        df[num_cols] <- as.data.frame(
          scale(
            df[num_cols],
            center = input$center_data_export,
            scale  = input$scale_data_export
          )
        )
      }
      
      df
    })
    
    
    #Different export options for datatypes
    
    #Export as csv file
    output$data_export_csv <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass1_Shiny_Export-", Sys.Date(), ".csv")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write.csv(data_export_table(), file, row.names = FALSE)
      }
    )
    
    #Export as excel workboox file
    output$data_export_xlsx <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass1_Shiny_Export-", Sys.Date(), ".xlsx")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write.xlsx(data_export_table(), file)
      }
    )
    
    #Export as .sav file for SPSS
    output$data_export_spss <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass1_Shiny_Export-", Sys.Date(), ".sav")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write_sav(data_export_table(), file)
      }
    )
    
    output$data_export_tsv <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass1_Shiny_Export-", Sys.Date(), ".tsv")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write_tsv(data_export_table(), file)
      }
    )
    
    output$data_export_rds <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass1_Shiny_Export-", Sys.Date(), ".rds")
      },
      
      content = function(file){
        saveRDS(data_export_table(), file)
      }
    )
    
    #Data table output for viewing
    output$data <- renderDataTable({
      data_export_table()
    })
    
    #Reset data export inputs
    observeEvent(input$reset_input_data, {
      reset("center_data_export")
      reset("scale_data_export")
      reset("selected_vars_numeric_data")
      reset("selected_vars_categorical_data")
      reset_filters("data")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_data, {
      reset_filters("data")
    })
    

    
# ================================================================================
#       CORRELATION CHART
# ================================================================================

    output$corr_chart <- renderPlot({
      
      #Was suppose to change this to reactive, but ran out of time
      df <- data
      
      df <- df[, sapply(df, is.numeric)]
      
      corr_matrix <- cor(df,
                         method = input$corr_method,
                         use = "complete.obs")
      
      if (input$corr_absolute) {
        corr_matrix <- abs(corr_matrix)
      }
      

      if (input$corr_display == "No") {
        display  <- NULL
      } else{ display <- "black"}
      
      corrplot(
        corr_matrix,
        method = "color",
        type = "upper",
        order = input$corr_order,
        col = colorRampPalette(c("blue","white","red"))(200),
        addCoef.col = display,
        cl.pos = "r",
        tl.col = "black",
        title = paste0(
          "Correlation Chart\n",
          "Type - ", input$corr_method,
          " | ",
          "Order - ", input$corr_order
        ),
        mar = c(0,0,2,0)
      )
      
    })
    
    #Reset data export inputs
    observeEvent(input$reset_input_corr, {
      reset("corr_method")
      reset("corr_order")
      reset("corr_absolute")
    })
  
    
# ================================================================================
#       Missing Values Chart
# ================================================================================
    
    output$missing_data <- renderPlot({
      
      req(reactive_missing())
      

      df <- reactive_missing()
      
      missing_threshold_title <- NULL
      
      #filtering rows that don't meet the missing value threshold
      if(input$missing_threshold > 0){
      df <- df %>%
        #Sum total of columns with a missing value
        mutate(missing_count = rowSums(is.na(.))) %>%
        #Removing rows that have less missing variables than threshold
        filter(missing_count >= input$missing_threshold)
      
      missing_threshold_title <- paste0("Rows With < ", input$missing_threshold, " Missing Variables | ")
      }
      
      
      
      order_title <- NULL
      break_lines <- NULL
      #If order by missing values per row is selected, reorder dataframe
      if (input$missing_order_na_row_sum == TRUE){
        df <- df %>% arrange(desc(missing_count))
        
        order_title <- paste0(" | Ordered by Missing Variable Count")
        
        #Identifying breaks in counts so line can be drawn
        breaks <- which(diff(df$missing_count) != 0)
        
        #Proportion threshold for grouping labels together
        prop_threshold <-  0.03
        
        #Calculating missing value group info 
        group_info <- df %>%
          group_by(missing_count) %>%
          summarise(n = n(), .groups = "drop") %>%
          #Calculating proportion each group takes of df
          mutate(
            prop = n / sum(n)
                ) %>%
          arrange(desc(missing_count))
        
        # Groups smaller than proportion threshold
        small_groups <- group_info %>%
          filter(prop < prop_threshold)
        
        #If there are small groups then combine them all to one row
        if (nrow(small_groups) > 0) {
          
          #Cutoff is minimum value in small groups
          cutoff <- min(small_groups$missing_count)
          
          # Collapse all rows into one row with summary stats
          collapsed <- group_info %>%
            filter(missing_count >= cutoff) %>%
            summarise(
              missing_count = cutoff,
              n = sum(n)
            ) %>%
            mutate(label = paste0("Missing ≥ ", cutoff, " | n = ", n))
          
          # Adding together large group and collapsed group
          group_info <- group_info %>%
            
            #Removing values that are included in collapsed df
            filter(missing_count < cutoff) %>%
            mutate(label = paste0("Missing = ", missing_count, " | n = ", n)) %>%
            bind_rows(collapsed)
          #Otherwise apply label for all rows
        }else{
          group_info <- group_info %>% 
            mutate(label = paste0("Missing = ", missing_count, " | n = ", n))
        }
        
        #Computing plotting variables
        group_info <- group_info %>%
          arrange(desc(missing_count)) %>%
          mutate(
            end = cumsum(n),
            start = end - n + 1,
            mid = (start + end) / 2
          )
        
        
        break_lines <- list(
          geom_hline(yintercept = group_info$end, colour = "black", size = 0.3),
          annotate(
            "text",
            x = ncol(df - 1),
            y = group_info$mid,
            label = group_info$label,
            hjust = 0,
            size = 3
          )
        )
      }
      
      #Removing missing_count col before plotting
      df$missing_count <-  NULL
      

      factor_cols   <- names(df)[sapply(df, is.factor) & !sapply(df, is.ordered)]
      ordered_cols  <- names(df)[sapply(df, is.ordered)]
      numeric_cols  <- names(df)[sapply(df, is.numeric)]
      
      #Calculating missing % for each variable
      missing_pct <- colMeans(is.na(df)) * 100
      
      #Ordering each group of columns by missing %
      factor_cols   <- factor_cols[order(missing_pct[factor_cols], decreasing = TRUE)]
      ordered_cols  <- ordered_cols[order(missing_pct[ordered_cols], decreasing = TRUE)]
      numeric_cols  <- numeric_cols[order(missing_pct[numeric_cols], decreasing = TRUE)]
      
      #Specifying specific order so factor cols and ordered factor cols are together
      new_order <- c(factor_cols, ordered_cols, numeric_cols)
      
      #Applying my new special order
      df <- df[, new_order]


      #Setting colour/legend inputs depending on if distinct datatypes selected or not
      if (input$distinct_datatypes_missing){
        dtype_colours <-  datatype_colours
        na_value <- "red"
        
        legend_position <- "top"
        
        legend_breaks <- names(dtype_colours)
        #Adjusting column labels to include missing %
        names(df) <-  paste0(names(df)," (", round(missing_pct[names(df)], 0),"%)")
        
        
        viz <- vis_dat(df, sort_type = FALSE)
        scale_fill <- scale_fill_manual(
          
          values = dtype_colours,
          
          breaks = legend_breaks,
          
          na.value = na_value)
        
        is_it_coloured <-  "| Coloured by Data Type"
        
        

        
      } else{
        na_value = "red"
        legend_position <- "top"
        scale_fill <-  scale_fill_manual(
          values = c("FALSE" = "grey80", "TRUE" = "red"),
          labels = c("Not Missing", "Missing"),
          name = "Value Status"
        )
        
        viz <- vis_miss(df, sort_miss = FALSE, show_perc = FALSE)
        
        is_it_coloured <-  NULL
      }
      
      
      row_count <- nrow(df)
        
      #Visualizing dataframe with new order and sort_type = FALSE
      #This way factor/ordered factors are side by side in the final plot
    viz +
        labs(title = paste0("Missing Data",
                            is_it_coloured,
                            "\n",
                            missing_threshold_title,
                            row_count,
                            " / ",
                            nrow(data),
                            " Rows",
                            order_title)) +
        scale_fill + #datatype colours list found in global.r
      
      break_lines +  
      
       coord_cartesian(clip = "off") +
      
      theme(
        axis.text.x = element_text(
          angle = 70,
          hjust = 0,
          vjust = 0,
          margin = margin(t = 10)),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 50, r = 120),
        legend.position = legend_position) 

      

    })
    
    #Reset missing chart inputs
    observeEvent(input$reset_input_missing, {
      reset("distinct_datatypes_missing")
      reset("selected_vars_numeric_missing")
      reset("selected_vars_categorical_missing")
      reset("missing_threshold")
      reset("missing_order_na_row_sum")
      reset_filters("missing")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_missing, {
      reset_filters("missing")
    })
    

    
# ================================================================================
#       RISING VALUE CHART
# ================================================================================
    #Object for selecting variables in ui

    output$rising_value <- renderPlot({

      req(reactive_rising())
      df <- reactive_rising()

      

      #Different y-axis label depending on scale input
      if (input$center_data == TRUE && input$scale_data == TRUE){
        y_label = "Standardised Values (Z-Score)"
      }
      else if (input$center_data == TRUE && input$scale_data == FALSE){
        y_label = "Centered Values"
      }
      else if (input$center_data == FALSE && input$scale_data == TRUE){
        y_label = "Scaled Values"
      }
      else{y_label = "Unadjusted Values"}
      
      #Plot titles/labels
      title = paste0("Rising Value Chart \n", "Jump Threshold = ", input$jump_threshold, " | ", y_label)
      x_label = "Percentile (%)"

      #Plot if no variables are selected
      if(ncol(df) == 0) {
        #Plot with same axis as others
        plot(
          x = c(0, 100),
          y = c(-3, 3),
          type = "n",
          xlab = x_label,
          ylab = y_label,
          main = title
        )

        #Adding text to plot telling people to select a variable
        text(
          x = 50,
          y = 0,
          labels = "PLEASE SELECT A VARIABLE",
          cex = 1.5,
          font = 2
        )
        return()
      }
      
      

        #Ensuring dataframe has only numeric columns
        df <- df[, sapply(df, is.numeric), drop = FALSE]

        #Ordering each column in ascending order with NA last
        for (col in 1:ncol(df)) {
          df[,col] <- df[order(df[,col]),col] 
        }

        #Scaling data 
        df <- scale(
          df,
          center = input$center_data,
          scale  = input$scale_data
        )
        
        #Only keeping variables with a jump in values
        jump_threshold <- input$jump_threshold
        
        #Function to filter out columns that don't meet the jump threshold
        #Default - 0.5
        keep_cols <- sapply(1:ncol(df), function(i) {
          
          col <- na.omit(df[, i])

          col_sorted <- sort(col)
          
          max_jump <- max(diff(col_sorted))
          sd_col   <- sd(col_sorted)
          
          jump_score <- max_jump / sd_col   # scale-independent
          
          return(jump_score > jump_threshold)
        })
        
        #Removing columns that don't meet jump threshold
        df <- df[, keep_cols, drop = FALSE]

        # x_vals <- (1:nrow(df)) / nrow(df) * 100
        x_vals <- seq(0, 100, length.out = nrow(df))
        

      mypalette <- rainbow(ncol(df))

      #Plotting function for single selected variable
      if (ncol(df) == 1){
        plot(x = x_vals,
                y = df,
                type = "l",
                xlab = x_label,
                ylab = y_label,
                lty = 1,
                lwd = 1,
                col = mypalette,
                main = title,
                sub = paste0("Variable - ", colnames(df))
             ) + 
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.margin = margin(t = 50)
          )
        
        #Control if no variables are selected
      } else { #PLotting for all other variables
      
      matplot(x = x_vals,
              y = df,
              type = "l",
              xlab = x_label,
              ylab = y_label,
              lty = 1,
              lwd = 1,
              col = mypalette,
              main = title,
              sub = paste0(ncol(df), " Variables")) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.margin = margin(t = 50)
          )
        

      legend(legend = colnames(df),
             x = "topleft",
             y = "top",
             lty = 1,
             lwd = 1,
             col = mypalette,
             ncol = round(ncol(df)^0.3))
            }
    })
    
    #Reset rising value inputs
    observeEvent(input$reset_input_rising, {
      reset("jump_threshold")
      reset("selected_vars_numeric_rising")
      reset("center_data")
      reset("scale_data")
      reset_filters("rising")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_rising, {
      reset_filters("rising")
    })
# =================================================================================
#       GG PAIRS PLOT
# =================================================================================

    output$ggpairs <- renderPlot({
      
      df <- reactive_ggpairs()
      
      req(ncol(df) > 0)
      
      if (input$ggpairs_colourby != "None"){
      cols <- ncol(df) - 1
      colouring <- paste0("Coloured by ", input$ggpairs_colourby)
      } else {
        cols <- ncol(df)
        colouring <- NULL
        
        }
      
      title <-  paste0("Pairs Plot of Data \n",
                       cols,
                       " / ",
                       ncol(data),
                       " Variables Selected",
                       " | ",
                       colouring)
      
      colour_var <- input$ggpairs_colourby
      
      theme <- theme(
                plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
                plot.margin = margin(t = 50)
                    ) 
      
      if (colour_var != "None") {
        
        cols_to_plot <- setdiff(names(df), colour_var)
        
        ggpairs(
          df,
          columns = cols_to_plot,
          mapping = aes(colour = .data[[colour_var]]),
          progress = FALSE,
          title = title
        ) +
          theme
        
      } else {
        
        ggpairs(
          df,
          progress = FALSE,
          title = title
        ) +
          theme
      }
      

    })
    
    #Reset ggpairs value inputs
    observeEvent(input$reset_input_ggpairs, {
      reset("selected_vars_numeric_ggpairs")
      reset("selected_vars_categorical_ggpairs")
      reset("ggpairs_colourby")
      reset_filters("ggpairs")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_ggpairs, {
      reset_filters("ggpairs")
    })
    
# =================================================================================
        # BOXPLOT
# =================================================================================
  output$boxplot <-  renderPlotly({
    df <-  reactive_boxplot()
    
    num_var <- input$selected_vars_numeric_boxplot
    colour_var <- input$selected_vars_categorical_boxplot
    
    
    if (length(num_var) > 1) {
    #Removing categorical variables
    df <- df[, sapply(df, is.numeric), drop = FALSE]
    colour_var <-  NULL
    }
    
    req(ncol(df) > 0)
    
    #Different y-axis label depending on scale input
    if (input$center_data_boxplot == TRUE && input$scale_data_boxplot == TRUE){
      x_label = "Standardised Values (Z-Score)"
    }
    else if (input$center_data_boxplot == TRUE && input$scale_data_boxplot == FALSE){
      x_label = "Centered Values"
    }
    else if (input$center_data_boxplot == FALSE && input$scale_data_boxplot == TRUE){
      x_label = "Scaled Values"
    }
    else{x_label = "Unadjusted Values"}
    
    #Plot titles/labels
    y_label = paste0("Variable (", ncol(df), " / ", ncol(data[, (sapply(data, is.numeric)), drop =FALSE]), " Numeric Variables Selected)")
    
    title = paste0("Boxplot \n", x_label, " | ", ncol(df), " / ", ncol(data), " Variables Selected | ", "IQR ", input$iqr_boxplot)


    #Scaling/centering data depending on input
    if(input$center_data_boxplot || input$scale_data_boxplot) {
      df <- as.data.frame(scale(df, 
                                  center = input$center_data_boxplot, 
                                  scale = input$scale_data_boxplot))
    }
    
    iqr_multiplier <-  input$iqr_boxplot
    

  
    # Different plotting arguments required if colourby is selected
    if (length(num_var) == 1 && !is.null(colour_var)) {

      df_long <- df %>%
        select(all_of(c(num_var, colour_var))) %>%
        rename(
          Value = all_of(num_var),
          Group = all_of(colour_var)
        )
      
      df_stats <- df_long %>%
        group_by(Group) %>%
        summarise(
          q1 = quantile(Value, 0.25, na.rm = TRUE),
          q3 = quantile(Value, 0.75, na.rm = TRUE),
          median = median(Value, na.rm = TRUE),
          iqr = IQR(Value, na.rm = TRUE),
          lower = q1 - iqr_multiplier * iqr,
          upper = q3 + iqr_multiplier * iqr,
          .groups = "drop"
        )
      
      outliers <- df_long %>%
        left_join(df_stats, by = "Group") %>%
        filter(Value < lower | Value > upper)
      
      plot_ly(
        type = "box",
        orientation = "h",
        q1 = df_stats$q1,
        median = df_stats$median,
        q3 = df_stats$q3,
        lowerfence = df_stats$lower,
        upperfence = df_stats$upper,
        y = df_stats$Group
      ) %>%
        add_markers(
          data = outliers,
          x = ~Value,
          y = ~Group,
          marker = list(color = "red", size = 6),
          name = "Outliers",
          inherit = FALSE
        )
      
    } else {
      
      # Original multiple-variable behaviour
      df_long <- tidyr::pivot_longer(
        df,
        cols = everything(),
        names_to = "Variable",
        values_to = "Value"
      )
      
      df_stats <- df_long %>%
        group_by(Variable) %>%
        summarise(
          q1 = quantile(Value, 0.25, na.rm = TRUE),
          q3 = quantile(Value, 0.75, na.rm = TRUE),
          median = median(Value, na.rm = TRUE),
          iqr = IQR(Value, na.rm = TRUE),
          lower = q1 - iqr_multiplier * iqr,
          upper = q3 + iqr_multiplier * iqr,
          .groups = "drop"
        )
      
      outliers <- df_long %>%
        left_join(df_stats, by = "Variable") %>%
        filter(Value < lower | Value > upper)
      
      plot_ly(
        type = "box",
        orientation = "h",
        q1 = df_stats$q1,
        median = df_stats$median,
        q3 = df_stats$q3,
        lowerfence = df_stats$lower,
        upperfence = df_stats$upper,
        y = df_stats$Variable
      ) %>%
        add_markers(
          data = outliers,
          x = ~Value,
          y = ~Variable,
          marker = list(color = "red", size = 6),
          name = "Outliers",
          inherit = FALSE
        ) }
    
  })
    #Reset boxplot value inputs
    observeEvent(input$reset_input_boxplot, {
      reset("selected_vars_numeric_boxplot")
      reset("center_data_boxplot")
      reset("scale_data_boxplot")
      reset("iqr_boxplot")
      reset_filters("boxplot")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_boxplot, {
      reset_filters("boxplot")
    })
    
# =================================================================================
#         MOSAIC
# =================================================================================
      output$mosaic <-  renderPlot({
        
        df <- reactive_mosaic()
        req(!is.null(df))
        
        #Defining formula for mosaic plot
        if (input$selected_vars_mosaic_z == "None" && is.null(input$selected_vars_mosaic_xyz)){
        formula <- as.formula(
                            paste("~",
                            input$selected_vars_mosaic_x,
                            "+",
                            input$selected_vars_mosaic_y)
                             )
        #If a third variable is selected 
        } else if (input$selected_vars_mosaic_z != "None" &&  is.null(input$selected_vars_mosaic_xyz)){
          formula <- as.formula(
            paste("~",
                  input$selected_vars_mosaic_x,
                  "+",
                  input$selected_vars_mosaic_y,
                  "+",
                  input$selected_vars_mosaic_z)
          )
          
          #If 4+ variables are selected
        }else if (!is.null(input$selected_vars_mosaic_z) && !is.null(input$selected_vars_mosaic_xyz)){
          extra_vars <- 
          
          formula <- as.formula(
            paste("~",
                  input$selected_vars_mosaic_x,
                  "+",
                  input$selected_vars_mosaic_y,
                  "+",
                  input$selected_vars_mosaic_z,
                  "+",
                  paste(input$selected_vars_mosaic_xyz, collapse = " + "))
          )
        } 
        
        plot <- xtabs(formula, data = df)
        

        #Plotting mosaic
        mosaic(plot,
               shade = TRUE,
               legend = TRUE,
               # split_vertical = TRUE, 
               # highlighting = input$selected_vars_mosaic_x,
               main = paste("Mosaic Plot",
                            paste(deparse(formula)))
        )
          
          # theme_minimal()
      })
    
    #Reset mosaic value inputs
    observeEvent(input$reset_input_mosaic, {
      reset("selected_vars_mosaic_x")
      reset("selected_vars_mosaic_y")
      reset("selected_vars_mosaic_z")
      reset("selected_vars_mosaic_xyz")
      reset_filters("mosaic")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_mosaic, {
      reset_filters("mosaic")
    })
    
# =================================================================================
# Scatter Plot
# =================================================================================
    output$counts_over_time <-  renderPlotly({
      df <-  reactive_counts_over_time()


      
      x <-  input$selected_x_scatter
      y <-  input$selected_y_scatter
      
      
      
      #Colour of Points
      colour <-  input$counts_colourby
      
      #If no colour selected, mapping aesthetic has no colour
      if (input$counts_colourby == "None" && is.null(input$counts_colourby_manual)){
        plot <-  ggplot(df,
                        aes(x = .data[[x]],
                            y = .data[[y]]
                        )) 
        #If colour is selected, mapping colours points based on selected variable
      } else if (input$counts_colourby != "None" && is.null(input$counts_colourby_manual)){
        plot <-  ggplot(df,
                        aes(x = .data[[x]],
                            y = .data[[y]],
                            colour = .data[[colour]]
                        )) 
      } 
      
      #Creating final plot
      plot  <-  plot +
      
        geom_point() +
        
        labs(
          x = paste0(x),
          y = paste0(y),
          title = paste0(y, " Variable Values by ", x)
        ) +
        
        theme_minimal()+
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.margin = margin(t = 50)
        )
      
      #Rendering plot as plotly object
    ggplotly(plot) 
    
    })
    
    #Reset boxplot value inputs
    observeEvent(input$reset_input_counts_over_time, {
      reset("selected_x_scatter")
      reset("selected_y_scatter")
      reset("counts_colourby")
      reset_filters("counts_over_time")

    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_counts_over_time, {
      reset_filters("counts_over_time")
    })
    
    
# =================================================================================
# PROCESSING - MISSING VALUES PLOTS
# =================================================================================
    
    #PLOT CAPTION
    #Same caption for all missing value plots since they use the same data
    missing_data_caption <- reactive({
      
      df <- processed_missing_reactive()
      
      row_count <- nrow(df)
      col_count <- ncol(df)
    
    row_range <- input$missing_threshold_processing
    col_range <- input$missing_col_threshold_processing
    
    
    missing_row_threshold_title <- paste0("Rows With ", row_range[1], " to ", row_range[2], " Missing Variables | ")
    missing_col_threshold_title <- paste0("Variables With ", col_range[1], "% to ", col_range[2],"% Rows Missing")
    
    paste0(
      row_count, " / ", nrow(data), " Rows | ",
      col_count, " / ", ncol(data), " Columns",
      "\n",
      missing_row_threshold_title,
      missing_col_threshold_title
    )
    })
    
    #Missing values plot
    output$missing_data_processing <- renderPlot({
      
      req(reactive_missing_processing())
      
      
      df <- processed_missing_reactive()
      
      
      
      order_title <- NULL
      break_lines <- NULL
      
      #If order by missing values per row is selected, reorder dataframe
      if (input$missing_order_na_row_sum_processing == TRUE){
        df <- df %>% arrange(desc(missing_count))
        
        order_title <- paste0(" | Ordered by Missing Variable Count")
        
        #Identifying breaks in counts so line can be drawn
        breaks <- which(diff(df$missing_count) != 0)
        
        #Proportion threshold for grouping labels together
        prop_threshold <-  0.1
        
        #Calculating missing value group info 
        group_info <- df %>%
          group_by(missing_count) %>%
          summarise(n = n(), .groups = "drop") %>%
          arrange(desc(missing_count)) %>% 
          #Calculating proportion each group takes of df
          mutate(
            prop = n / sum(n),
            cum_prop = cumsum(prop)
          ) 
        
        # Groups smaller than proportion threshold
        small_groups <- group_info %>%
          filter(cum_prop < prop_threshold)
        
        #If there are small groups then combine them all to one row
        if (nrow(small_groups) > 1) {
          
          #Cutoff is minimum value in small groups
          cutoff <- min(small_groups$missing_count)
          
          # Collapse all rows into one row with summary stats
          collapsed <- group_info %>%
            filter(missing_count >= cutoff) %>%
            summarise(
              missing_count = cutoff,
              n = sum(n)
            ) %>%
            mutate(label = paste0("Missing ≥ ", cutoff, " | n = ", n))
          
          # Adding together large group and collapsed group
          group_info <- group_info %>%
            
            #Removing values that are included in collapsed df
            filter(missing_count < cutoff) %>%
            mutate(label = paste0("Missing = ", missing_count, " | n = ", n)) %>%
            bind_rows(collapsed)
          #Otherwise apply label for all rows
        }else{
          group_info <- group_info %>% 
            mutate(label = paste0("Missing = ", missing_count, " | n = ", n))
        }
        
        #Computing plotting variables
        group_info <- group_info %>%
          arrange(desc(missing_count)) %>%
          mutate(
            end = cumsum(n),
            start = end - n + 1,
            mid = (start + end) / 2
          )
        
        
        break_lines <- list(
          geom_hline(yintercept = group_info$end, colour = "black", size = 0.3),
          annotate(
            "text",
            x = ncol(df),
            y = group_info$mid,
            label = group_info$label,
            hjust = 0,
            size = 3
          )
        )
      }
      
      
      #Removing missing_count col before plotting
      df$missing_count <-  NULL
      
      
      factor_cols   <- names(df)[sapply(df, is.factor) & !sapply(df, is.ordered)]
      ordered_cols  <- names(df)[sapply(df, is.ordered)]
      numeric_cols  <- names(df)[sapply(df, is.numeric)]
      
      #Calculating missing % for each variable
      missing_pct <- colMeans(is.na(df)) * 100
      
      #Ordering each group of columns by missing %
      factor_cols   <- factor_cols[order(missing_pct[factor_cols], decreasing = TRUE)]
      ordered_cols  <- ordered_cols[order(missing_pct[ordered_cols], decreasing = TRUE)]
      numeric_cols  <- numeric_cols[order(missing_pct[numeric_cols], decreasing = TRUE)]
      
      #Specifying specific order so factor cols and ordered factor cols are together
      new_order <- c(factor_cols, ordered_cols, numeric_cols)
      
      #Applying my new special order
      df <- df[, new_order]
      
      
      #Setting colour/legend inputs depending on if distinct datatypes selected or not
      if (input$distinct_datatypes_missing_processing){
        dtype_colours <-  datatype_colours
        na_value <- "red"
        
        legend_position <- "top"
        
        legend_breaks <- names(dtype_colours)
        #Adjusting column labels to include missing %
        names(df) <-  paste0(names(df)," (", round(missing_pct[names(df)], 0),"%)")
        
        
        viz <- vis_dat(df, sort_type = FALSE)
        scale_fill <- scale_fill_manual(
          
          values = dtype_colours,
          
          breaks = legend_breaks,
          
          na.value = na_value)
        
        is_it_coloured <-  " | Coloured by Data Type"
        
        
        
        
      } else{
        na_value = "red"
        legend_position <- "top"
        scale_fill <-  scale_fill_manual(
          values = c("FALSE" = "grey80", "TRUE" = "red"),
          labels = c("Not Missing", "Missing"),
          name = "Value Status"
        )
        
        viz <- vis_miss(df, sort_miss = FALSE, show_perc = FALSE)
        
        is_it_coloured <-  NULL
      }
      
      
      
      
      #Visualizing dataframe with new order and sort_type = FALSE
      #This way factor/ordered factors are side by side in the final plot
      viz +
        labs(title = paste0("Missing Data",
                            is_it_coloured,
                            order_title
                            ),
             caption = missing_data_caption()) +
        scale_fill + #datatype colours list found in global.r
        
        break_lines +  
        
        coord_cartesian(clip = "off") +
        
        theme(
          axis.text.x = element_text(
            angle = 70,
            hjust = 0,
            vjust = 0,
            margin = margin(t = 10)),
          plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
          plot.margin = margin(t = 50, r = 120),
          plot.caption = element_text(
            size = 11,
            hjust = 0,   
            margin = margin(t = 10)
          ),
          legend.position = legend_position) 
      
      
      
    })
  
    #gg_miss plot
    output$gg_miss <-  renderPlot({
      df <- processed_missing_reactive()
      
      order <- input$order_by_gg_miss
      
      if (input$order_by_gg_miss == "Both"){
        order <- c("freq", "degree")
      }


      gg_miss_upset(df,
                    nsets = input$gg_miss_nsets, 
                    nintersects = input$gg_miss_nintersects, 
                    order.by = order) 
      
    })
    
    #Updating rpart colunms that can be selected depending on what is in processed_missing_reactive()
    observe({
      df <- processed_missing_reactive()
      
      valid_cols <- names(df)[!names(df) %in% c("missing_count", "CODE", "OBS_TYPE")]
      
      updatePickerInput(
        session,
        "rpart_exclude_vars",
        choices = valid_cols
      )
    })
    
    #rpart plot
    output$rpart <- renderPlot({
      df <- processed_missing_reactive()
      
      #Dropping code/obs type options
      df <-  df[, !names(df) %in% c("CODE", "OBS_TYPE"), drop = FALSE]
      
      #Removing manually specified options if needed
      df <- df[, !names(df) %in% input$rpart_exclude_vars, drop = FALSE]
      
      # Default method is regression
      method_type <- "anova"
      
      #Default title
      prediction_title <- "Predicting Number of Missing Values Per Observation"
      
      #Special condition for binary classification
      if (input$rpart_target_type == "Binary (Missing/Not Missing)"){
        
        #Converting missing_count to binary Missing/No missing
        df$missing_count <- factor(
          ifelse(df$missing_count > 0, "Missing", "Complete")
        )        
        
        #Converting method_type to class
        method_type <- "class"
        
        prediction_title <- "Predicting Missing vs Complete Observations"
      }
      
      rpart_model <- rpart(formula = missing_count ~ .,
                     data = df,
                     method = method_type,
                     control = rpart.control(
                       maxdepth = input$rpart_maxdepth,
                       cp = input$rpart_cp,
                       minsplit = input$rpart_minsplit
                     ))
      #Plot title
      rpart_title <- paste0(prediction_title)
      
      rpart_subtitle <- paste0(
        "Max Depth: ", input$rpart_maxdepth,
        " | Min Split: ", input$rpart_minsplit,
        " | CP: ", input$rpart_cp
      )
      
      if (input$rpart_plot_mode == "tree"){
      
      rpart.plot(rpart_model, 
                             shadow.col = "gray",
                             type = as.integer(input$rpart_type),
                             extra = ifelse(input$rpart_extra, 1, 0),
                             main = rpart_title,
                             sub = rpart_subtitle
                             )
        
        #Change plot and titles if variable importance is selected
      } else if(input$rpart_plot_mode == "varimp"){
        
        #Creating dataframe of model importance features
        imp_df <- data.frame(
          Variable = names(rpart_model$variable.importance),
          Importance = as.numeric(rpart_model$variable.importance)
        )
        
        #Ordering imp_df by importance 
        imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]
        
        #Bar plot of importance by variable
        ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
          geom_col() +
          coord_flip() +
          labs(
            title = "Variable Importance",
            subtitle = missing_data_caption(),
            x = "Variable",
            y = "Importance"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5)
          )
      }
    })

    
    #Missing value correlation plot
    output$missing_corr_plot <-  renderPlot({
      
      df <- processed_missing_reactive()
      

      
      df_shadow <- is.na(df) + 0 
      col_means <- colMeans(df_shadow)
      
      #ignore vars that are all (or never) missing
      df_shadow <- df_shadow[ , col_means > 0 & col_means < 1, drop = FALSE]
      

      corr_matrix <- cor(df_shadow,
                         method = input$corr_missing_method)
      
      if (input$corr_missing_absolute) {
        corr_matrix <- abs(corr_matrix)
      }
      
      
      if (input$corr_missing_display) {
        display <- "black"
      } else{ display <- NULL}
      
      corrplot(
        corr_matrix,
        method = "color",
        type = "upper",
        order = input$corr_missing_order,
        col = colorRampPalette(c("blue","white","red"))(200),
        addCoef.col = display,
        cl.pos = "r",
        tl.col = "black",
        title = paste0(
          "Correlation Chart\n",
          "Type - ", input$corr_missing_method,
          " | ",
          "Order - ", input$corr_missing_order
        ),
        mar = c(0,0,2,0)
      )
      
      #Adding caption for context
      mtext(
        missing_data_caption(),
        side = 1,       
        line = 3,      
        adj = 0,       
        cex = 0.8       
      )
    })
 
# =================================================================================
# PROCESSING - MISSING VALUES RESET INPUTS 
# =================================================================================     
    #Reset missing plot specific inputs
    observeEvent(input$reset_missing_plot, {
      reset("distinct_datatypes_missing_processing")
      reset("missing_order_na_row_sum_processing")
    })
    
    #Reset gg miss plot specific inputs
    observeEvent(input$reset_gg_miss_plot, {
      reset("gg_miss_nsets")
      reset("gg_miss_nintersects")
      
    })
    
    #Reset rpart plot specific inputs
    observeEvent(input$reset_rpart_plot, {
      reset("rpart_maxdepth")
      reset("rpart_cp")
      reset("rpart_minsplit")
      reset("rpart_type")
      reset("rpart_extra")
      reset("rpart_exclude_vars")
      reset("rpart_target_type")
      reset("rpart_plot_mode")
      
    })
    
    #Reeset correlation plot specifc inputs
    observeEvent(input$reset_corr_missing_plot, {
      reset("corr_missing_order")
      reset("corr_missing_method")
      reset("corr_missing_display")
      reset("corr_missing_absolute")
    })
    
    #Reset missing dataframe processing inputs
    observeEvent(input$reset_input_missing_processing, {
      reset("selected_vars_numeric_missing_processing")
      reset("selected_vars_categorical_missing_processing")
      reset("missing_threshold_processing")
      reset("missing_col_threshold_processing")
      reset_filters("missing_processing")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_missing_processing, {
      reset_filters("missing_processing")
    })
    
# =================================================================================
# PROCESSING - MISSING VALUES EXPORT OPTIONS
# =================================================================================
    #Export as csv file
    output$data_export_csv_missing_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".csv")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write.csv(processed_missing_reactive(), file, row.names = FALSE)
      }
    )
    
    #Export as excel workboox file
    output$data_export_xlsx_missing_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".xlsx")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write.xlsx(processed_missing_reactive(), file)
      }
    )
    
    #Export as .sav file for SPSS
    output$data_export_spss_missing_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".sav")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write_sav(processed_missing_reactive(), file)
      }
    )
    
    output$data_export_tsv_missing_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".tsv")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write_tsv(processed_missing_reactive(), file)
      }
    )
    
    output$data_export_rds_missing_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".rds")
      },
      
      content = function(file){
        saveRDS(processed_missing_reactive(), file)
      }
    )
    

# =================================================================================
# PROCESSING - OUTLIER VALUES PLOTS
# =================================================================================
   #Density plot 
    #Density plot values that can be selected depend on processed_missing_reactive()
    observe({
      df <- processed_missing_reactive()
      
      req(df)
      
      #dropping missing_count
      df$missing_count <-  NULL
      
      numeric_choices <- names(df)[sapply(df, is.numeric)]
      
      updatePickerInput(
        session,
        inputId = "selected_vars_numeric_outlier_density_plot",
        choices = numeric_choices,
        selected = numeric_choices
      )

    })
    
    output$outlier_density_plot <- renderPlot({
      df <- processed_outlier_reactive()
      req(df)

      #Selected values from input
      selected <- input$selected_vars_numeric_outlier_density_plot
      
      #By default colour_by is selected value
      colour_by <- input$outlier_density_colourby
      
      #If colour_by is none, change it to NULL
      if (colour_by == "None") {
      colour_by <- NULL
      } 
      
      #Pivotting data to only have value/variable + colour variable 
      df_pivot <- df %>%
        select(all_of(c(selected, colour_by))) %>%
        pivot_longer(
          cols = all_of(selected),
          names_to = "Variable",
          values_to = "Value"
        ) %>%
        filter(!is.na(Value), is.finite(Value))
      
      # If everything got filtered out
      if (nrow(df_pivot) == 0) {
        plot.new()
        text(0.5, 0.5, "No valid data")
        return()
      }
      
      # Base plot if no colour_by variable selected
      if (is.null(colour_by)) {
        
        ggplot(df_pivot, aes(x = Value)) +
          geom_density(fill = "steelblue", alpha = 0.6) +
          facet_wrap(~ Variable, scales = "free") +
          labs(
            title = "Density Plots",
            x = "Value",
            y = "Density"
          ) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        
      } else {
        ggplot(df_pivot, aes(
      x = Value,
      fill = .data[[colour_by]],
      colour = .data[[colour_by]]
    )) +
      geom_density(alpha = 0.4) +
      facet_wrap(~ Variable, scales = "free") +
      labs(
        title = paste("Density Plots by", colour_by),
        x = "Value",
        y = "Density",
        fill = colour_by,
        colour = colour_by
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  }
    })

# =================================================================================
# PROCESSING - OUTLIER VALUES RESET INPUTS/EXPORT OPTIONS
# =================================================================================  
    #Reset sidebar values
    observeEvent(input$reset_input_outlier_processing, {
      reset_filters("outlier_processed")
      reset("selected_vars_numeric_outlier_processing")
      reset("selected_vars_categorical_outlier_processing")
      
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_outlier_processed, {
      reset_filters("outlier_processing")
    }) 
    
    #Resetting density plot inputs
    observeEvent(input$reset_density_outlier_plot, {
      reset("selected_vars_numeric_outlier_density_plot")
      reset("outlier_density_colourby")
    })
    
    
    #Export options in datatable tab
    #Export as csv file
    output$data_export_csv_outlier_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".csv")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write.csv(processed_outlier_reactive(), file, row.names = FALSE)
      }
    )
    
    #Export as excel workboox file
    output$data_export_xlsx_outlier_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".xlsx")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write.xlsx(processed_outlier_reactive(), file)
      }
    )
    
    #Export as .sav file for SPSS
    output$data_export_spss_outlier_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".sav")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write_sav(processed_outlier_reactive(), file)
      }
    )
    
    output$data_export_tsv_outlier_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".tsv")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write_tsv(processed_outlier_reactive(), file)
      }
    )
    
    output$data_export_rds_outlier_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".rds")
      },
      
      content = function(file){
        saveRDS(processed_outlier_reactive(), file)
      }
    )
    

    
    } 

)
