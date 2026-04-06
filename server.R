
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
        
        df <- train_data
        
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
    processed_eda_reactive <- reactive({
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
        
      
      
        #Applying any tab filters
        df <- filter_data(df, "outlier_processing")
        
        #Hashed out selecting columns manually in outlier section for now because it being buggy
        # #Only returning selected columns from page inputs
        # selected_vars <- unlist(c(
        #   input$selected_vars_numeric_outlier_processing,
        #   input$selected_vars_categorical_outlier_processing
        # ), use.names = FALSE)
        # 
        # selected_vars <- intersect(selected_vars, names(df))
        # 
        # if (length(selected_vars) == 0) {
        #   return(df)
        # }
        # df <- df %>% select(all_of(c(selected_vars, "missing_count")))
        # 
        
        #Imputation changes for columns 
        missing_value_imputations <- outlier_impute_settings()
        
        # if (is.null(missing_value_imputations)) {
        #   return(df)
        # }
        
        #Recipe for imputation/transformation processing
        processing_recipe <- recipe(~ ., data = df)
        
        for (imputation in missing_value_imputations) {
          method <- imputation[[1]]
          cols <- intersect(imputation[[2]], names(df))
          if (length(cols) == 0) next
          
          if (method == "Manual") {
            for (col in cols) {
              df[[col]][is.na(df[[col]])] <- imputation[[3]]
            }
            
          }
          if (method == "Median") {
            processing_recipe <- processing_recipe %>% step_impute_median(all_of(cols))
          }
          if (method == "Mean") {
            processing_recipe <- processing_recipe %>% step_impute_mean(all_of(cols))
          }
          if (method == "KNN") {
              
              knn_neighbours <- imputation[[4]]
              processing_recipe <- processing_recipe %>% step_impute_knn(all_of(cols), neighbors = knn_neighbours)
            
          }
          if (method == "Bagged Trees") {
            
            processing_recipe <- processing_recipe %>% step_impute_bag(all_of(cols), trees = input$bag_trees)
            
          }
        }
        
        col_transformations <- variable_transformation_settings()
        
        for (transformation in col_transformations){
          cols <- intersect(names(df),transformation[[2]])
          method <-  transformation[[1]]
          
          if (length(cols) == 0) next
          
          if (method == "Box-Cox"){

            processing_recipe <- processing_recipe %>% step_BoxCox(all_of(cols))
          }
          
          if (method == "Yeo-Johnson"){
            processing_recipe <- processing_recipe %>% step_YeoJohnson(all_of(cols))
          }
          
        }
        
        #Scaling/centering data depending on input
        # if(input$center_data_outlier || input$scale_data_outlier) {
        #   df <- as.data.frame(scale(df,
        #                             center = input$center_data_outlier,
        #                             scale = input$scale_data_outlier))
        # }
        
        # if (length(col_transformations) > 0){df <- processing_recipe %>%
        #   browser()
        #   prep(training = df) %>%
        #   bake(new_data = df)}
        
        #Apply recipe steps if there are any
        if (length(processing_recipe$steps) > 0) {
          df <- processing_recipe %>%
            prep(training = df) %>%
            bake(new_data = df)
        }
        
        df
      
    })
    
    #Updating outlier processing select inputs based on output of processed_eda_reactive()
    observe({
      df <- processed_eda_reactive()
      
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
    
    #Impute missing variables options change depending on processed_eda_reactive()
    observe({
      df <- processed_eda_reactive()
      
      req(df)
      
      #dropping missing_count
      df$missing_count <-  NULL
      
      choices <- names(df)[
        colSums(is.na(df)) > 0 & sapply(df, is.numeric)
      ]
      
      updatePickerInput(
        session,
        inputId = "selected_vars_impute_missing",
        choices = choices,
        selected = NULL
      )
    })
    
    #Transform variables options change depending on processed_eda_reactive()
    observe({
      df <- processed_eda_reactive()
      
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
    
    #List of variable transformation info
    variable_transformation_settings <- reactiveVal(list())   
    
    
    observeEvent(input$transform_variables, {
      current_steps <- variable_transformation_settings()
      
      new_step <- list(
        trans_method <-  input$transform_variable_method,
        trans_cols <-  input$selected_vars_transform
      )
      
      #Add new transformation command to list 
      variable_transformation_settings(append(current_steps, list(new_step)))
    })
    
    #Event to reset transformations
    observeEvent(input$reset_transform_variables, {
      variable_transformation_settings(list())
    })
    
    
    
    #Reactive data table for checking data in processing data section
    output$missing_processing_reactive_table <- renderDataTable({
      processed_eda_reactive()
    })
    
    #Reactive data table for checking data in processing data section
    output$outlier_processing_reactive_table <- renderDataTable({
      processed_eda_reactive()
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
      
      df <- processed_eda_reactive()
      
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
      
      
      df <- processed_eda_reactive()
      
      
      
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
            margin = ggplot2::margin(t = 10)),
          plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
          plot.margin = ggplot2::margin(t = 50, r = 120),
          plot.caption = element_text(
            size = 11,
            hjust = 0,   
            margin = ggplot2::margin(t = 10)
          ),
          legend.position = legend_position) 
      
      
      
    })
  
    #gg_miss plot
    output$gg_miss <-  renderPlot({
      df <- processed_eda_reactive()
      
      order <- input$order_by_gg_miss
      
      if (input$order_by_gg_miss == "Both"){
        order <- c("freq", "degree")
      }


      gg_miss_upset(df,
                    nsets = input$gg_miss_nsets, 
                    nintersects = input$gg_miss_nintersects, 
                    order.by = order) 
      
    })
    
    #Updating rpart colunms that can be selected depending on what is in processed_eda_reactive()
    observe({
      df <- processed_eda_reactive()
      
      valid_cols <- names(df)[!names(df) %in% c("missing_count", "CODE", "OBS_TYPE")]
      
      updatePickerInput(
        session,
        "rpart_exclude_vars",
        choices = valid_cols
      )
    })
    
    observe({
      df <- processed_eda_reactive()
      
      valid_cols <- names(df)[!names(df) %in% c("missing_count", "CODE", "OBS_TYPE")]
      
      updatePickerInput(
        session,
        "rpart_binary_predict",
        choices = valid_cols
      )
    })
    
    #rpart plot
    output$rpart <- renderPlot({
      df <- processed_eda_reactive()
      
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
        
        #Default title 
        prediction_title <- "Predicting Missing vs Complete Observations"
        
        if(is.null(input$rpart_binary_predict)){
        #Converting missing_count to binary Missing/No missing
        df$missing_count <- factor(
          ifelse(df$missing_count > 0, "Missing", "Complete")
        )}
        
      else{
          cols <- input$rpart_binary_predict
          cols <- intersect(cols, names(df))
          req(length(cols) > 0)
          
          df$missing_count <- factor(
            ifelse(rowSums(is.na(df[, cols, drop = FALSE])) > 0,
                   "Missing",
                   "Complete")
          )     
          
          prediction_title <-  paste0("Predicting Missing Values for ", input$rpart_binary_predict)
          }
      
        
        #Converting method_type to class
        method_type <- "class"
        
        
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
      
      df <- processed_eda_reactive()
      

      
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
        write.csv(processed_eda_reactive(), file, row.names = FALSE)
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
        write.xlsx(processed_eda_reactive(), file)
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
        write_sav(processed_eda_reactive(), file)
      }
    )
    
    output$data_export_tsv_missing_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".tsv")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write_tsv(processed_eda_reactive(), file)
      }
    )
    
    output$data_export_rds_missing_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".rds")
      },
      
      content = function(file){
        saveRDS(processed_eda_reactive(), file)
      }
    )
    

# =================================================================================
# PROCESSING - OUTLIER VALUES PLOTS
# =================================================================================
   #Density plot 
    #Density plot values that can be selected depend on processed_eda_reactive()
    observe({
      df <- processed_eda_reactive()
      
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
      df <- processed_eda_reactive()
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
    
    
    
    #Boxplot values that can be selected depend on processed_eda_reactive()
    observe({
      df <- processed_eda_reactive()
      
      req(df)
      
      #dropping missing_count
      df$missing_count <-  NULL
      
      numeric_choices <- names(df)[sapply(df, is.numeric)]
      
      updatePickerInput(
        session,
        inputId = "selected_vars_boxplot_outlier",
        choices = numeric_choices,
        selected = NULL
      )
      
    })
    
    #Boxplot outlier plot
    output$boxplot_outlier <-  renderPlotly({
      df <-  processed_eda_reactive()
      
      #Removing missing count as it isnt needed
      df$missing_count <- NULL
      
      num_var <- input$selected_vars_boxplot_outlier
      num_var <- intersect(num_var, names(df))
      
      if (is.null(num_var)){
        num_var <- names(df[, sapply(df, is.numeric)])
      }
      

      colour_var <- input$colour_by_boxplot_outlier
      
      
      if (length(num_var) > 1) {
        #Removing categorical variables
        df <- df[, sapply(df, is.numeric), drop = FALSE]
        colour_var <-  NULL
      }
      
      df <-  df %>% select(all_of(c(num_var, colour_var)))
      
      req(ncol(df) > 0)
      
      #Different y-axis label depending on scale input
      if (input$center_data_outlier == TRUE && input$scale_data_outlier == TRUE){
        x_label = "Standardised Values (Z-Score)"
      }
      else if (input$center_data_outlier == TRUE && input$scale_data_outlier == FALSE){
        x_label = "Centered Values"
      }
      else if (input$center_data_outlier == FALSE && input$scale_data_outlier == TRUE){
        x_label = "Scaled Values"
      }
      else{x_label = "Unadjusted Values"}
      
      #Plot titles/labels
      y_label = paste0("Variable (", ncol(df), " / ", ncol(data[, (sapply(data, is.numeric)), drop =FALSE]), " Numeric Variables Selected)")
      
      title = paste0("Boxplot \n", x_label, " | ", ncol(df), " / ", ncol(data), " Variables Selected | ", "IQR ", input$iqr_outlier)
      
      
      #Scaling/centering data depending on input
      if(input$center_data_outlier || input$scale_data_outlier) {
        df[num_var] <- as.data.frame(scale(df[num_var], 
                                  center = input$center_data_outlier, 
                                  scale = input$scale_data_outlier))
      }
      
      iqr_multiplier <-  input$iqr_outlier
      
      
      
      # Different plotting arguments required if colourby is selected
      if (length(num_var) == 1 && !is.null(colour_var)) {
        
        df_long <- df %>%
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
            lower_fence = q1 - iqr_multiplier * iqr,
            upper_fence = q3 + iqr_multiplier * iqr,
            .groups = "drop"
          )%>%
          #Adding 'NA' as string to be plotted
          mutate(
            Group = as.character(Group),
            Group = ifelse(is.na(Group), "NA", Group)
          )
        
        df_stats <- df_stats %>%
          left_join(
            df_long %>%
              left_join(df_stats, by = "Group") %>%
              group_by(Group) %>%
              summarise(
                lower_whisker = min(Value[Value >= first(lower_fence)], na.rm = TRUE),
                upper_whisker = max(Value[Value <= first(upper_fence)], na.rm = TRUE),
                .groups = "drop"
              ),
            by = "Group"
          )
        
        outliers <- df_long %>%
          left_join(df_stats, by = "Group") %>%
          filter(Value < lower_fence | Value > upper_fence)
        

        plot_ly(
          type = "box",
          orientation = "h",
          q1 = df_stats$q1,
          median = df_stats$median,
          q3 = df_stats$q3,
          lowerfence = df_stats$lower_whisker,
          upperfence = df_stats$upper_whisker,
          y = df_stats$Group,
          name = "Boxplot"
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
            lower_fence = q1 - iqr_multiplier * iqr,
            upper_fence = q3 + iqr_multiplier * iqr,
            .groups = "drop"
          )
        
        df_stats <- df_stats %>%
          left_join(
            df_long %>%
              left_join(df_stats, by = "Variable") %>%
              group_by(Variable) %>%
              summarise(
                lower_whisker = min(Value[Value >= first(lower_fence)], na.rm = TRUE),
                upper_whisker = max(Value[Value <= first(upper_fence)], na.rm = TRUE),
                .groups = "drop"
              ),
            by = "Variable"
          )
        
        
        outliers <- df_long %>%
          left_join(df_stats, by = "Variable") %>%
          filter(Value < lower_fence | Value > upper_fence)
        
        plot_ly(
          type = "box",
          orientation = "h",
          q1 = df_stats$q1,
          median = df_stats$median,
          q3 = df_stats$q3,
          lowerfence = df_stats$lower_whisker,
          upperfence = df_stats$upper_whisker,
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
    observeEvent(input$reset_plot_input_boxplot_outlier, {
      reset("colour_by_boxplot_outlier")
      reset("selected_vars_boxplot_outlier")
      reset("iqr_outlier")
    })

    #Outlier pattern plot vars selected depend on processed_eda_reactive()
    observe({
      df <- processed_eda_reactive()
      
      req(df)
      
      #dropping missing_count
      df$missing_count <-  NULL
      
      numeric_choices <- names(df)[sapply(df, is.numeric)]
      
      updatePickerInput(
        session,
        inputId = "selected_var_mahal",
        choices = numeric_choices,
        selected = numeric_choices
      )
      
    })
    
    output$outlier_scatter <- renderPlot({
      df <- processed_eda_reactive()
      var <- input$outlier_scatter_var
      outlier_method <- input$outlier_scatter_method
      mark_outlier_flag <- input$outlier_scatter_mark
      
      df$id <- seq_len(nrow(df))
      
      
      if (mark_outlier_flag){
        
        selected_col <- df[[var]]
        
        if (outlier_method == "Inter-Quartile Range"){
          #Calculating IQR for selected col
          q1 <- quantile(selected_col, 0.25, na.rm = TRUE)
          q3 <- quantile(selected_col, 0.75, na.rm = TRUE)
          iqr <- IQR(selected_col, na.rm = TRUE)
          
          iqr_multiplier <- input$outlier_scatter_iqr_range
          
          lower_bound <- q1 - iqr_multiplier * iqr
          upper_bound <- q3 + iqr_multiplier * iqr
          
        } else if (outlier_method == "Standard Deviation"){
          #Calculating standard deviation of col
          mean_col <- mean(selected_col, na.rm = TRUE)
          sd_col <- sd(selected_col, na.rm = TRUE)
          
          sd_val <- input$outlier_scatter_sd_num
          
          lower_bound <- mean_col - sd_val * sd_col
          upper_bound <- mean_col + sd_val * sd_col
          
        }
        #Applying upper/lower bounds to flag outliers
        df$outlier_flag <- ifelse(
          !is.na(selected_col) & (selected_col < lower_bound | selected_col > upper_bound),
          "outlier",
          "non-outlier"
        )
        
        df$label <- NA_character_
        df$label[df$outlier_flag == "outlier"] <- as.character(df$CODE[df$outlier_flag == "outlier"])

      }
      

      plot <- ggplot(df, aes(x = id,
                             y = .data[[var]])) +
        labs(
          y = var,
          x = "Complete Observations",
          title = paste("Scatter Plot of", var)
        )
      
      #If mark outliers is selected then colour by the calculated column and label
      if (mark_outlier_flag) {
        plot <- plot + 
          geom_point(aes(colour = outlier_flag)) +
          ggrepel::geom_text_repel(max.overlaps = 50, mapping = aes(label = label))
      } else {
        plot <- plot + geom_point()
      }
      
      plot
      
    })
    
    output$outlier_pattern <- renderPlot({
      df <- processed_eda_reactive()
      
      #Removing missing count
      df$missing_count <- NULL
      

      distance_method <- input$outlier_distance_method_plot
      

      codes <- df$CODE
      if (distance_method == "Mahalanobis"){
        #Selected variable to calculate distance for mahalanobis
        selected_vars <- input$selected_var_outlier_pattern
        
        excluded_vars <- setdiff(names(df), input$selected_var_mahal)
        
        # Add a row index before the recipe
        df$row_index <- seq_len(nrow(df))
        
        df_processed <- recipe(~ ., data = df) %>%
          step_rm(any_of(excluded_vars)) %>%
          step_rm(all_nominal_predictors()) %>%
          step_naomit(everything()) %>%
          step_nzv(all_predictors()) %>%
          step_lincomb(all_numeric_predictors()) %>%
          prep(training = df) %>%
          bake(new_data = df)
        
        # Use surviving row indices to get matching CODE values
        surviving_codes <- df$CODE[df_processed$row_index]
        
        # dropping row index
        df_processed$row_index <- NULL
      
      
      if (input$maha_robust){
        robust_maha <- MASS::cov.rob(df_processed, method = "mcd")
        covar <- robust_maha$cov
        centre <- robust_maha$center
      }else {
        covar  <- var(df_processed)
        centre <- colMeans(df_processed)
      }
      
      md2 <- mahalanobis(x = df_processed, center = centre, cov = covar)
      threshold <- qchisq(p = input$mahal_threshold, df = ncol(df_processed))
      
      # Use surviving_codes as labels
      label <- ifelse(md2 > threshold, as.character(surviving_codes), NA)
      Observations <- ifelse(md2 > threshold, "outlier", "non-outlier")
      id <- (1:length(md2))/length(md2)
      df <- data.frame(md2, label, id, Observations)
      plot <-  ggplot(data = df, mapping = aes(y = md2, x = id)) +
        geom_point(mapping = aes(colour = Observations)) +
        ggrepel::geom_text_repel(max.overlaps = 50, mapping = aes(label = label)) +
        labs(y = "Mahalanobis distance squared",
             x = "Complete Observations",
             title = paste("Mahalanobis Distance Outlier pattern using", round(threshold,1), "threshold")) 
        
      }else if (distance_method == "Local Outlier Factors"){
        
        # Save row index before recipe
        df$row_index <- seq_len(nrow(df))
        
        
        df_processed <- recipe(~ ., data = df) %>%
          step_rm(all_nominal_predictors()) %>%
          step_naomit(everything()) %>%
          prep(training = df) %>%
          bake(new_data = df)
        
        # Recover CODE using surviving row indices
        surviving_codes <- df$CODE[df_processed$row_index]
        df_processed$row_index <- NULL
        
        
        mat <- as.matrix(df_processed)
        minPoints <- input$lof_min_points
        threshold <- input$lof_threshold
        lof <- dbscan::lof(mat, minPts = minPoints)
        id <- (1:length(lof))/length(lof)
        label <- ifelse(lof > threshold, as.character(surviving_codes), NA)
        Observations <- ifelse(lof > threshold, "outlier", "non-outlier")
        data <- data.frame(lof, id, label, Observations)
        
        
        plot <- ggplot(data = data, mapping = aes(y = lof, x = id)) +
          geom_point(mapping = aes(colour = Observations)) +
          ggrepel::geom_text_repel(max.overlaps = 50, mapping = aes(label = label)) +
          scale_y_continuous(limits = c(0, NA)) +
          labs(y = "LOF", x = "Complete Observations", 
               title = paste("Local Outlier Factor Outlier Pattern | Minimum Points =", minPoints, "| Outlier Threshold =", threshold 
                             )) 
      } else if (distance_method == "Isolation Forest"){
        threshold <- input$iso_for_threshold
        
        itree <- isotree::isolation.forest(df)
        scores <- predict(itree, newdata = df)
        isfOutliers <- scores > threshold
        df_plot <- data.frame(
          scores       = scores, 
          id           = (1:length(scores))/length(scores),
          Observations = ifelse(isfOutliers, "outlier", "non-outlier"),
          label        = ifelse(isfOutliers, df$CODE, NA)
        )
        
       plot <-  ggplot(data = df_plot, mapping = aes(y = scores, x = id)) +
          geom_point(mapping = aes(colour = Observations)) +
          geom_text_repel(max.overlaps = 50, mapping = aes(label = label)) +
          scale_y_continuous(limits = c(0.3333, NA)) +
          labs(y = "Iso Score",
               x = "Complete Observations", 
               title = paste("Outlier pattern using Isolation Forest with threshold =",
                             threshold)) 
      }
      
      plot +
        scale_colour_manual(values = c("blue", "red")) +
        
        geom_hline(yintercept = threshold, colour = "black") +
        scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%"))
      
    })

    #Reset outlier pattern plot value inputs
    observeEvent(input$reset_plot_input_outlier_pattern, {
      reset("selected_var_mahal")
    })
# =================================================================================
# PROCESSING - OUTLIER VALUES RESET INPUTS/EXPORT OPTIONS
# =================================================================================  
    #Reset sidebar values
    observeEvent(input$reset_input_outlier_processing, {
      reset_filters("outlier_processing")
      # reset("selected_vars_numeric_outlier_processing")
      # reset("selected_vars_categorical_outlier_processing")
      reset("center_data_outlier")
      reset("scale_data_outlier")
      outlier_impute_settings(list())
      
      
      
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
        write.csv(processed_eda_reactive(), file, row.names = FALSE)
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
        write.xlsx(processed_eda_reactive(), file)
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
        write_sav(processed_eda_reactive(), file)
      }
    )
    
    output$data_export_tsv_outlier_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".tsv")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write_tsv(processed_eda_reactive(), file)
      }
    )
    
    output$data_export_rds_outlier_processed <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass2_Shiny_Export-", Sys.Date(), ".rds")
      },
      
      content = function(file){
        saveRDS(processed_eda_reactive(), file)
      }
    )
    

    
    
# ====================================================================================
#       PROCESSING PIPELINE BUILDER SECTION
# ===================================================================================
    
    #This whole section is for the processing pipeline builder section
    #It allows user to select steps for a pipeline manually and save them for later use
    
    # Reactive list tracking each step
    pipeline_steps <- reactiveVal(list())
    
    observe({
      print(pipeline_steps())
    })
    
    # Add a new empty step
    observeEvent(input$add_processing_step, {
      current <- pipeline_steps()
      
      id <-  length(current) + 1
      
      #Selected input method, must be selected
      input_method  = paste0("step_method_", id)
      
      #Row removal test skip is NULL
      #Will be changed if NA_REMOVE_ROW selected
      test_skip <- NULL
      
      # Validating a method has been selected
      if (input[[input_method]] == "") {
        showNotification(
          "Please select a method before adding a new step.",
          type = "error"
        )
        return()  # stop execution
      }
      
      #Input for selected columns (if applicable, otherwise NULL)
      input_cols <- paste0("step_cols_", id)
      
      if (input[[input_method]] == "impute_knn"){
        threshold_input <- paste0("step_knn_k_", id)
        additional_info <- isolate(input[[threshold_input]])
      }
      else if (input[[input_method]] == "impute_manual"){
        threshold_input <- paste0("step_manual_val_", id)
        additional_info <- isolate(input[[threshold_input]])
      }
      else if (input[[input_method]] == "impute_bag"){
        num_trees <- paste0("step_bag_trees_", id)
        additional_info <- isolate(input[[num_trees]])
      }
      else if (input[[input_method]] == "remove_na_rows"){
        threshold_input <- paste0("step_na_row_threshold_", id)
        additional_info <- isolate(input[[threshold_input]])
        
        #Row Removal Apply to test data flag
        row_remove_apply_to_test <- paste0("step_na_row_apply_test", id)
        
        if (input[[row_remove_apply_to_test]]){ #If checkbox is TRUE
          test_skip <- FALSE #skip = FALSE in processing pipeline so row removal applied to test data
        } else{test_skip <- TRUE}#skip = TRUE in processing pipeline so rows aren't removed in test and all imputed instead
        
        
      }
      else if (input[[input_method]] == "remove_na_cols"){
        threshold_input <- paste0("step_na_col_threshold_", id)
        additional_info <- isolate(input[[threshold_input]])
      }
      else {additional_info <- 0}
      
      


      new_step <- list(
        id     = id,
        method = isolate(input[[input_method]]) ,
        cols   = isolate(input[[input_cols]]),
        additional_info = additional_info,
        skip_test = test_skip
      )
      pipeline_steps(append(current, list(new_step)))
    })
    
    #Pipeline step adding UI output
    output$processing_steps_ui <- renderUI({
      steps <- pipeline_steps()
      
      #Defining Dataframe from global.r
      df <- data
      
      #Columns for selection
      numeric_cols     <- names(df)[sapply(df, is.numeric)]
      categorical_cols <- names(df)[sapply(df, is.factor)]
      all_cols         <- names(df)[!names(df) %in% c("CODE", "OBS_TYPE")]
      
      # tagList(lapply(seq_along(steps), function(i) {
        # step_id <- i
      step_id <- length(steps) + 1
      
      card(
        style = "margin-bottom: 10px; border: 1px solid #dee2e6; min-height: 700px;",
        card_header(
          style = "background-color: #f8f9fa;",
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            strong(paste("Step", step_id)),
            
          )
        ),
        
        # Selecting step method
        selectInput(
          inputId  = paste0("step_method_", step_id),
          label    = "Processing Method",
          choices  = c(
            "-- Select --"             = "",
            "Remove Rows with NAs"     = "remove_na_rows",
            "Remove Columns with NAs"  = "remove_na_cols",
            "Impute — Median"          = "impute_median",
            "Impute — Mean"            = "impute_mean",
            "Impute — KNN"             = "impute_knn",
            "Impute — Manual Value"    = "impute_manual",
            "Impute — Bagged Trees"    = "impute_bag",
            "Transform — Box-Cox"      = "boxcox",
            "Transform — Yeo-Johnson"  = "yeojohnson",
            "Scale Data"      = "scale",
            "Center Data"      = "center",
            "Remove Near-Zero Variance"= "nzv",
            "Remove Linear Combos"     = "lincomb",
            "Remove Specific Observations"     = "remove_obs",
            "Remove Multivariate Outliers"     = "remove_outliers",
            "Replace Outliers"     = "replace_outliers",
            "Winsorize Outliers"     = "winsorize",
            "Add Interaction Term"     = "interaction_term"
            
            
          ),
          selected = ""
        ),
        
        # Conditional panel for selecting columns depending on selected method
        conditionalPanel(
          condition = paste0(
            "input['step_method_", step_id, "'] == 'impute_median' || ",
            "input['step_method_", step_id, "'] == 'impute_mean'   || ",
            "input['step_method_", step_id, "'] == 'impute_knn'    || ",
            "input['step_method_", step_id, "'] == 'impute_manual' || ",
            "input['step_method_", step_id, "'] == 'impute_bag'    || ",
            "input['step_method_", step_id, "'] == 'boxcox'        || ",
            "input['step_method_", step_id, "'] == 'yeojohnson'    || ",
            "input['step_method_", step_id, "'] == 'scale'         ||",
            "input['step_method_", step_id, "'] == 'winsorize'     ||",
            "input['step_method_", step_id, "'] == 'interaction_term'     ||",
            "input['step_method_", step_id, "'] == 'replace_outliers'"
            
          ),
          pickerInput(
            inputId  = paste0("step_cols_", step_id),
            label    = "Select Variables",
            choices  = numeric_cols[numeric_cols != "DEATH_RATE"],  
            selected = numeric_cols[numeric_cols != "DEATH_RATE"],
            multiple = TRUE,
            options  = list(`actions-box` = TRUE, `live-search` = TRUE)
          )
        ),
        
        # KNN neighbours
        conditionalPanel(
          condition = paste0("input['step_method_", step_id, "'] == 'impute_knn'"),
          numericInput(
            paste0("step_knn_k_", step_id),
            "Number of Neighbours",
            value = 5, min = 1
          )
        ),
        
        # Bagged Tree
        conditionalPanel(
          condition = paste0("input['step_method_", step_id, "'] == 'impute_bag'"),
          numericInput(
            paste0("step_bag_trees_", step_id),
            "Number of Trees",
            value = 25
          )
        ),
        
        # Manual impute value
        conditionalPanel(
          condition = paste0("input['step_method_", step_id, "'] == 'impute_manual'"),
          numericInput(
            paste0("step_manual_val_", step_id),
            "Value to Replace NA",
            value = 0
          )
        ),
        
        # NA threshold for column removal
        conditionalPanel(
          condition = paste0("input['step_method_", step_id, "'] == 'remove_na_cols'"),
          sliderInput(
            paste0("step_na_col_threshold_", step_id),
            "Remove columns with more than X% missing",
            min = 0, max = 100, value = 50, post = "%"
          )
        ),
        
        # NA threshold for row removal  
        conditionalPanel(
          condition = paste0("input['step_method_", step_id, "'] == 'remove_na_rows'"),
          sliderInput(
            paste0("step_na_row_threshold_", step_id),
            "Remove rows with more than X missing values",
            min = 0, max = ncol(df), value = 0, step = 1
          ),
          checkboxInput(
            paste0("step_na_row_apply_test", step_id),
            "Apply Row Removal to Test Data",
            value = TRUE
          )
        ),
        # Just a note if lincomb is selected  
        conditionalPanel(
          condition = paste0("input['step_method_", step_id, "'] == 'lincomb'"),
          p("NOTE - Make sure all NA values have been addressed before adding this step", style = "color:red;")
        ),
        
        #Remove observations panel
        conditionalPanel(
          condition = paste0("input['step_method_", step_id, "'] == 'remove_obs'"),
          pickerInput(
            inputId  = paste0("step_", step_id, "obs_remove_train"),
            label    = "Observations to Remove - Training Data",
            choices  = unique(train_data$CODE),  
            selected = NULL,
            multiple = TRUE,
            options  = list(`actions-box` = TRUE, `live-search` = TRUE)
          ),
          pickerInput(
            inputId  = paste0("step_", step_id, "obs_remove_test"),
            label    = "Observations to Remove - Test Data",
            choices  = unique(train_data$CODE),  
            selected = NULL,
            multiple = TRUE,
            options  = list(`actions-box` = TRUE, `live-search` = TRUE)
          )
        ),
        #Remove Outliers panel
        conditionalPanel(
          condition = paste0("input['step_method_", step_id, "'] == 'remove_outliers'"),
          pickerInput(
            inputId  = paste0("step_", step_id, "remove_outliers"),
            label    = "Method",
            choices  = c("Mahalanobis Distance", "Isolation Forest", "Local Outlier Factors"),  
            selected = NULL,
            multiple = FALSE,
            options  = list(`actions-box` = TRUE, `live-search` = TRUE)
          )
        ),
      )
      # })) #taglist end brackets
})
    
    #Pipeline Summary UI Output
    output$pipeline_summary_ui <- renderUI({
      steps <- pipeline_steps()
      
      if (length(steps) == 0) {
        return(p("No steps added yet.", style = "color: grey;"))
      }
      
      #List of method labels in global.r
    
      tagList(
        lapply(seq_along(steps), function(i) {
          step <- steps[[i]]
          
          #Selected step method and columns
          method <- step$method
          
          cols   <- step$cols
          
          #Number of columns selected
          num_cols <- length(cols)
          num_vars <- paste(num_cols, "Variables")
          
          #Contextual info to be displayed
          if (method == "remove_na_rows"){
            row_threshold <- step$additional_info
            skip_test <- step$skip_test
            contextual_info <- paste("| Threshold =", row_threshold, "| Skip Test =", skip_test)
          }
          else  if (method == "remove_na_cols"){
              col_threshold <- step$additional_info
              contextual_info <- paste0("| Threshold = ", col_threshold, "%")
          } 
          else  if (method == "impute_knn"){
            neighbours <- step$additional_info
            contextual_info <- paste0(" | Neighbours = ", neighbours)
          } 
          else  if (method == "impute_manual"){
            manual_val <- step$additional_info
            contextual_info <- paste0("| Value = ", manual_val)
          } 
          else  if (method == "impute_bag"){
            trees <- step$additional_info
            contextual_info <- paste0("| Trees = ", trees)
          } 
          else{contextual_info <- NULL}
          
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            
            div(
              strong(paste("Step", i, ": ")),
              methods[[method]],
              contextual_info,
            ),
            # actionButton(
            #   inputId = paste0("remove_step_", i),
            #   label   = "✕ Remove",
            #   class   = "btn btn-danger btn-sm"
            # )
          )
        }
        ))
      
    })
    
    
    #Remove all steps if reset_pipeline selected
    observeEvent(input$reset_pipeline_btn, {
      pipeline_steps(list())
    })
    
    
    
    # Reactive object for saving pipelines
    #Already has default pipeline defined
    saved_pipelines <- reactiveVal(list(
      "Default Pipeline" = list(
        recipe  = default_recipe,
        df      = juice(default_prepped),
        steps   = default_steps,
        created = Sys.time()
      )
    ))
    
    pipeline_recipe <- eventReactive(input$process_data_btn, {
      steps <- pipeline_steps()
      
      #Using training data for pipeline
      df <- train_data
      

      #Initial pipeline recipe using modified dataframe
      pipeline_recipe <- recipe(DEATH_RATE ~ ., data = df) %>% 
        update_role("CODE", new_role = "id") %>% 
        update_role("OBS_TYPE", new_role = "split") %>%
        step_unknown() %>% #Assigning missing factors to unknown
        step_dummy(all_nominal_predictors(), one_hot = TRUE) #Encoding categorical variables
      
      
      # Track removed cols as loop progresses
      removed_cols <- c("CODE", "OBS_TYPE", "DEATH_RATE")  # protect these from ever being used
      imputed_cols <- c()
      
      #Adding each step into recipe
      for (i in seq_along(steps)) {
        step   <- steps[[i]]
        method <- step$method

        if (is.null(method) || method == "") next
        
        cols <- intersect(names(df), setdiff(step$cols, removed_cols))
        
        print(paste("STEP", i))
        print(method)
        print(cols)
        print(step$additional_info)
        
        needs_cols <- method %in% c(
          "impute_median", "impute_mean", "impute_knn", "impute_bag",
          "impute_manual", "boxcox", "yeojohnson",
          "scale", "center"
        )
        
        # # Skip steps that need cols if all have already been used
        # needs_cols <- method %in% c("impute_median", "impute_mean", "impute_knn",
        #                             "impute_manual", "boxcox", "yeojohnson", 
        #                             "scale", "center")
        # if (needs_cols && length(cols) == 0) {
        #   showNotification(
        #     paste0("Step ", i, " (", method, ") skipped — all selected columns have been removed"),
        #     type = "warning"
        #   )
        #   next
        # }
        
        #Removing rows/cols from data based on thresholds
        if (method == "remove_na_rows") {
          threshold <- step$additional_info
          
          pipeline_recipe <- pipeline_recipe %>%
            step_mutate(
              .row_na_count = rowSums(is.na(across(everything())))
            ) %>%
            step_filter(.row_na_count <= !!threshold, skip = step$skip_test) %>%
            step_rm(.row_na_count)
          

        } else if (method == "remove_na_cols") {
          threshold <- step$additional_info
          active_cols  <- setdiff(names(df), c(imputed_cols, removed_cols))
          
          # Calculate which cols to remove based on FULL dataset once
          cols_to_remove <- active_cols[
            colMeans(is.na(df[, active_cols, drop = FALSE])) * 100 > threshold
          ]

          pipeline_recipe <- pipeline_recipe %>%  step_filter_missing(all_predictors(), threshold = threshold / 100)
          
          # Update removed column tracker
          removed_cols <- c(removed_cols, cols_to_remove)
          
        } else if (method == "impute_manual") {
          val  <- step$additional_info
          
          for (col in cols) {
            pipeline_recipe <- pipeline_recipe %>%
              step_mutate(!!col := ifelse(is.na(.data[[col]]), val, .data[[col]]))
          }

          imputed_cols <- c(imputed_cols, cols)
          
        } else if (method == "impute_median") {
          pipeline_recipe <- pipeline_recipe %>% step_impute_median(all_of(cols))
          imputed_cols <- c(imputed_cols, cols)
          
        } else if (method == "impute_mean") {
          pipeline_recipe <- pipeline_recipe %>% step_impute_mean(all_of(cols))
          imputed_cols <- c(imputed_cols, cols)
          
        } else if (method == "impute_knn") {
          k <- step$additional_info
          pipeline_recipe <- pipeline_recipe %>% step_impute_knn(all_of(cols), neighbors = k)
          imputed_cols <- c(imputed_cols, cols)
        } else if (method == "impute_bag") {
          
          tree <- step$additional_info
          pipeline_recipe <- pipeline_recipe %>% step_impute_bag(all_of(cols), trees = tree)
          imputed_cols <- c(imputed_cols, cols)
          
        } else if (method == "boxcox") {
          pipeline_recipe <- pipeline_recipe %>% step_BoxCox(all_of(cols))
          
        } else if (method == "yeojohnson") {
          pipeline_recipe <- pipeline_recipe %>% step_YeoJohnson(all_of(cols))
          
        } else if (method == "scale") {
          pipeline_recipe <- pipeline_recipe %>% step_scale(all_of(cols))
          
        } else if (method == "center") {
          pipeline_recipe <- pipeline_recipe %>% step_center(all_of(cols))
          
        } else if (method == "nzv") {
          pipeline_recipe <- pipeline_recipe %>% step_nzv(all_predictors())
          
        } else if (method == "lincomb") {
          pipeline_recipe <- pipeline_recipe %>% step_lincomb(all_numeric_predictors())
        }
      }
      
      # Prep and bake the recipe on the modified dataframe
      prepped_recipe <- prep(pipeline_recipe, training = df)


      
      #Returning list with transformed df and recipe
      list(
        df  = juice(prepped_recipe),
        recipe = pipeline_recipe
      )
      
    })
    
    # Save pipeline when button clicked
    observeEvent(input$process_data_btn, {
      
      steps <- pipeline_steps()
      result <- pipeline_recipe()
      req(result)
      

      #Validating steps have been selected
      # if (length(steps) == 0) {
      #   showNotification(
      #     "Pipeline is empty, please add processing steps",
      #     type = "error"
      #   )
      #   return()  # stop execution
      # }
      
      # Using name input and making sure one has been selected
      name <- input$pipeline_name
      if (name == "") {
          showNotification(
            "Please select a name before saving pipeline",
            type = "error"
          )
          return()  # stop execution
        }      
      
      current <- saved_pipelines()
      # Save recipe + processed df + step summary together
      current[[name]] <- list(
        recipe  = result$recipe,
        df      = result$df,
        steps   = pipeline_steps(),     # save the step config too for display
        created = Sys.time()
      )
      
      saved_pipelines(current)
      
      #Resetting pipeline steps for next pipeline to be made
      pipeline_steps(list())
      
      # Confirm to user
      showNotification(
        paste0("Pipeline '", name, "' saved successfully"),
        type = "message",
        duration = 3
      )
      
    })
    
    # Update pipeline selector with saved pipelines
    observe({
      pipelines <- saved_pipelines()
      
      updatePickerInput(
        session,
        inputId  = "selected_pipeline_model",
        choices  = names(pipelines),
        selected = names(pipelines)[length(pipelines)]  # default to most recent
      )
    })
    
    output$processed_datasets_list <- renderUI({
      
      pipelines <- saved_pipelines()
      
      if (length(pipelines) == 0) {
        return(p("No processed pipelines saved yet."))
      }
        
      tagList(lapply(seq_along(pipelines), function(i){

        pipeline <- pipelines[[i]]
        
        pipeline_name <- names(pipelines)[i]
        
        steps <- pipeline$steps
        
        #UI for processing steps in dataframe
        step_ui <- if (length(steps) == 0) {
          p("No steps.")
        } else {
          tags$ol(
            lapply(seq_along(steps), function(j) {
              step <- steps[[j]]
              
              method <- step$method
              cols   <- step$cols
              
              method_label <- if (!is.null(methods[[method]])) {
                methods[[method]]
              } else {
                method
              }
              
              #Contextual info to be displayed
              if (method == "remove_na_rows"){
                row_threshold <- step$additional_info
                skip_test <- step$skip_test
                contextual_info <- paste("| Threshold =", row_threshold, "| Skip Test =", skip_test)
              }
              else  if (method == "remove_na_cols"){
                col_threshold <- step$additional_info
                contextual_info <- paste0("| Threshold = ", col_threshold, "%")
              } 
              else  if (method == "impute_knn"){
                neighbours <- step$additional_info
                contextual_info <- paste0(" | Neighbours = ", neighbours)
              } 
              else  if (method == "impute_manual"){
                manual_val <- step$additional_info
                contextual_info <- paste0("| Value = ", manual_val)
              } 
              else{contextual_info <- NULL}

              tags$li(
                paste0(method_label, contextual_info)
              )
            })
          )}

        card(
          style = "margin-bottom: 15px;",
          card_header(
            strong(pipeline_name),
            actionButton(
              inputId = paste0("delete_pipeline_", i),
              label   = "🗑 Delete",
              class   = "btn btn-danger btn-sm",
              style   = "margin-left: auto;"
            )
          ),
          p(strong("Created: "), as.character(pipeline$created)),
          p(strong("Pipeline:")),
          step_ui,
          tags$pre(
            paste(capture.output(print(head(pipeline$df))), collapse = "\n")
          )
        )
        
      }))
    })
    
    observe({
  pipelines <- saved_pipelines()
  
  lapply(seq_along(pipelines), function(i) {
    local({
      idx <- i
      pipeline_name <- names(pipelines)[i]
      
      observeEvent(input[[paste0("delete_pipeline_", idx)]], {
        current <- saved_pipelines()
        current[[pipeline_name]] <- NULL
        saved_pipelines(current)
        
        showNotification(
          paste0("Pipeline '", pipeline_name, "' deleted"),
          type = "message",
          duration = 3
        )
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
})
    

# =============================================================================== 
    #       GLM MODEL SECTION
# ====================================================================================
    #This section is related to making/running/evaluating the GLM model
    
    #Change selected_pipeline_model options to be whatever pipelines are saved
    observe({
      pipelines <- saved_pipelines()
      
      updatePickerInput(
        session,
        inputId  = "selected_pipeline_model",
        choices  = names(pipelines),
        selected = names(pipelines)[length(pipelines)]  # default to most recent
      )
    })
    
    #Reactive object for the selected pipeline
    selected_pipeline <- reactive({
      req(input$selected_pipeline_model)
      pipelines <- saved_pipelines()
      req(length(pipelines) > 0)
      pipelines[[input$selected_pipeline_model]]
    })
    
    #UI for selected pipeline summary
    output$selected_pipeline_summary <- renderUI({
      req(input$selected_pipeline_model)
      pipelines <- saved_pipelines()
      
      if (length(pipelines) == 0){
        return(p("No pipelines made yet :(", style = "color: grey;"))
      }

      pipeline <- pipelines[[input$selected_pipeline_model]]
      steps    <- pipeline$steps
      
      if (length(steps) == 0){
        return(p("No steps in this pipeline, just base data", style = "color: grey;"))
      }
      
      tags$ol(
        style = "padding-left: 15px; font-size: 0.9em;",
        lapply(seq_along(steps), function(j) {
          step   <- steps[[j]]
          method <- step$method
          
          contextual <- switch(method,
                               "remove_na_rows" = paste("| Threshold =", step$additional_info),
                               "remove_na_cols" = paste0("| ", step$additional_info, "% missing"),
                               "impute_knn"     = paste0("| k = ", step$additional_info),
                               "impute_manual"  = paste0("| Value = ", step$additional_info),
                               ""
          )
          
          tags$li(paste(methods[[method]], contextual))
        })
      )
      
    })
    
    # Data summary for selected pipeline
    output$model_data_summary <- renderUI({
      req(input$selected_pipeline_model)
      pipelines <- saved_pipelines()
      req(length(pipelines) > 0)
      
      pipeline <- pipelines[[input$selected_pipeline_model]]
      
      tagList(
        p(strong("Rows: "),   nrow(pipeline$df),
          strong(" | Cols: "), ncol(pipeline$df)),
        p(strong("Created: "), format(pipeline$created, "%d %b %Y %H:%M"))
      )
    })
    
    # Saving model result
    model_result <- reactiveVal(NULL)
    
    reactive_model <-  reactive({
      pipeline <- selected_pipeline()
      req(pipeline)
      
      #Extracting pipeline recipe for model
      pipeline_recipe  <- pipeline$recipe  
      
      #Final step removing any observations with NA values so none make it to GLM
      pipeline_recipe <- pipeline_recipe %>%
        step_naomit(all_predictors(), all_outcomes(), skip = FALSE)
      
      prepped <- prep(pipeline_recipe, training = train_data)
      
      train_processed <- juice(prepped)
      test_processed  <- bake(prepped, new_data = test_data)
      
      
      ctrl <- trainControl(
        method          = "cv",
        number          = input$cv_folds,
        savePredictions = "final",
        verboseIter     = FALSE
      )
      
      # Optional log transform on outcome
      # if (input$log_transform_outcome) {
      #   pipeline_recipe <- pipeline_recipe %>% step_log(all_outcomes(), base = 10)
      # }
      
      model <- caret::train(
        DEATH_RATE ~ . - CODE - OBS_TYPE,
        data      = train_processed,     
        method    = "glmnet",
        trControl = ctrl,
        tuneLength = input$glmnet_tune_length,
        tuneGrid  = expand.grid(
          alpha  = seq(0, 1, by = 0.1),
          lambda = 10^seq(-4, 1, length.out = input$glmnet_tune_length)
        )
      )
      
      test_preds <- predict(model, newdata = test_processed)
      test_performance <- postResample(pred = test_preds, obs = test_processed$DEATH_RATE)
      
      
      list(
        model = model,
        performance = test_performance,
        test_preds = test_preds,
        test_actual = test_processed$DEATH_RATE,
        train_processed = train_processed,
        test_processed = test_processed
      )
    })
    
    # #Running model
    # observeEvent(input$run_model_btn, {
    #   pipeline <- selected_pipeline()
    #   req(pipeline)
    #   
    #   #Extracting pipeline recipe for model
    #   pipeline_recipe  <- pipeline$recipe  
    #   
    #   #Final step removing any observations with NA values so none make it to GLM
    #   pipeline_recipe <- pipeline_recipe %>%
    #     step_naomit(all_predictors(), all_outcomes(), skip = FALSE)
    #   
    #   prepped <- prep(pipeline_recipe, training = train_data)
    #   
    #   train_processed <- juice(prepped)
    #   test_processed  <- bake(prepped, new_data = test_data)
    #   
    #   
    #   
    # 
    #   ctrl <- trainControl(
    #     method          = "cv",
    #     number          = input$cv_folds,
    #     savePredictions = "final",
    #     verboseIter     = FALSE
    #   )
    #   
    #   # Optional log transform on outcome
    #   # if (input$log_transform_outcome) {
    #   #   pipeline_recipe <- pipeline_recipe %>% step_log(all_outcomes(), base = 10)
    #   # }
    #   
    #   model <- caret::train(
    #     DEATH_RATE ~ .,
    #     data      = train_processed,     
    #     method    = "glmnet",
    #     trControl = ctrl,
    #     tuneLength = input$glmnet_tune_length,
    #     tuneGrid  = expand.grid(
    #       alpha  = input$glmnet_alpha,
    #       lambda = 10^seq(-4, 1, length.out = input$glmnet_tune_length)
    #     )
    #   )
    #   
    #   test_preds <- predict(model, newdata = test_processed)
    #   test_performance <- postResample(pred = test_preds, obs = test_processed$DEATH_RATE)
    #   
    # 
    #   model_result(list(
    #     model = model,
    #     performance = test_performance,
    #     test_preds = test_preds,
    #     test_actual = test_processed$DEATH_RATE
    #   ))
    # })
    
    
    output$model_predictions <- renderPlot({
      result <- reactive_model()

      
      if (is.null(result$test_preds)){
        plot.new()
        text(
          x = 0.5, y = 0.5,
          labels = "No model run yet",
          cex = 1.5,
          font = 2
        )
        return()
      }
      
      df_plot <- data.frame(
        Actual = result$test_actual,
        Pred   = result$test_preds
      )
      
      
      
      ggplot(df_plot, aes(x = Actual, y = Pred)) +
        geom_point(alpha = 0.6) +
        geom_abline(slope = 1, intercept = 0, color = "red") +
        labs(title = "Actual vs Predicted",
             x = "Actual", 
             y = "Predicted",
             subtitle = paste0(nrow(df_plot)," / ", nrow(test_data), " Test Observations | ", round(result$performance[[2]], 2))) +
        theme_minimal()
    })
    
    output$model_diagnostic_plot <- renderPlot({
      model <- reactive_model()
      final_model <- model$model$finalModel
      test_actual <- model$test_actual
      test_pred   <- model$test_preds
      model_resid  <- test_actual - test_pred
      
      # 2x2 layout for model diagnostics
      par(mfrow = c(1, 2))
      # plot(final_model, xvar = "lambda", label = TRUE) # Coefficients
      # plot(final_model)  
      # plot(model$model)     

      plot(test_pred, model_resid, main="Residuals vs Fitted", xlab="Fitted", ylab="Residuals")
      abline(h=0, col="red")
      qqnorm(model_resid)
      qqline(model_resid, col="red")
      
    })
    
    output$model_results <- renderTable({
      model_list <- reactive_model()
      model <- model_list$model
      final_model <- model$finalModel
      results <- model$results
      final_results <- final_model$results
      
      perf <- model_list$performance
      

      data.frame(
        Metric = names(perf),
        Value = as.numeric(perf)
      )
      
    })
    
    output$varimp_plot <- renderPlotly({
      result <- reactive_model()
      req(result)
      
      imp <- varImp(result$model)$importance
      imp_df <- data.frame(
        Variable   = rownames(imp),
        Importance = imp$Overall
      ) %>% arrange(desc(Importance)) %>% 
        filter(Importance != 0)
      
      ggplotly(
        ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
          geom_col(fill = "steelblue") +
          coord_flip() +
          labs(title = "Variable Importance", x = NULL, y = "Importance") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      )
    })
    
    # Model equation as text for viewing
    output$model_equation <- renderUI({
      result <- reactive_model()
      req(result)
      
      best_alpha  <- result$model$bestTune$alpha
      best_lambda <- result$model$bestTune$lambda
      coefs <- coef(result$model$finalModel, s = best_lambda)
      
      coef_df <- data.frame(
        Variable    = rownames(coefs),
        Coefficient = as.numeric(coefs)
      ) %>% filter(Coefficient != 0)
      
      intercept <- coef_df$Coefficient[coef_df$Variable == "(Intercept)"]
      predictors <- coef_df %>% filter(Variable != "(Intercept)")
      
      # Build equation string
      terms <- paste0(
        ifelse(predictors$Coefficient > 0, " + ", " - "),
        round(abs(predictors$Coefficient), 3),
        " × ", predictors$Variable,
        collapse = ""
      )
      
      equation <- paste0(
        "DEATH_RATE = ",
        round(intercept, 3),
        terms
      )
      
      tagList(
        p(strong(paste0(
          "Best Alpha: ", best_alpha,
          " | Best Lambda: ", round(best_lambda, 4),
          " | Non-zero Coefficients: ", nrow(predictors)
        ))),
        p(style = "font-family: monospace; font-size: 0.85em; 
               word-break: break-all; background: #f8f9fa; 
               padding: 10px; border-radius: 5px;",
          equation
        )
      )
    })
    
    #Plot to look at residual outliers
    output$model_outlier_plot <- renderPlot({
      result <- reactive_model()
      req(result)
      
      df <- result$test_processed
      #Joining test data for categorical variable columns
      df <- df %>%
        left_join(
          test_data %>% select(CODE, GOVERN_TYPE, HEALTHCARE_BASIS),
          by = "CODE"
        )
      test_actual <- result$test_actual
      test_preds  <- result$test_preds
      residuals   <- test_actual - test_preds
      
      # Standardise residuals
      std_resid <- residuals / sd(residuals, na.rm = TRUE)
      
      threshold <- input$model_outlier_sd_threshold  
      
      #Dataframe for plotting
      plot_df <- data.frame(
        Fitted    = test_preds,
        StdResid  = std_resid,
        CODE      = df$CODE,
        Outlier   = abs(std_resid) > threshold,
        GOVERN_TYPE = df$GOVERN_TYPE,
        HEALTHCARE_BASIS = df$HEALTHCARE_BASIS
      
      )
      
      if (!isTRUE(input$model_outlier_label)){plot_df$CODE <- ""}

      colour_choice <- input$model_outlier_colourby
      if (colour_choice == "Outlier"){plot_df$ColourGroup <- as.character(plot_df$Outlier)}
      else if (colour_choice == "GOVERN_TYPE"){plot_df$ColourGroup <- as.character(plot_df$GOVERN_TYPE)}
      else if(colour_choice == "HEALTHCARE_BASIS"){plot_df$ColourGroup <- as.character(plot_df$HEALTHCARE_BASIS)}
      else {plot_df$ColourGroup <- "All Observations"}
      
      ggplot(plot_df, aes(x = Fitted, y = StdResid)) +
        geom_point(aes(colour = ColourGroup), alpha = 0.7) +
        ggrepel::geom_text_repel(
          data = plot_df[plot_df$Outlier, ],
          aes(label = CODE), max.overlaps = 20, na.rm = TRUE
        ) +
        geom_hline(yintercept = c(-threshold, threshold), 
                   colour = "red", linetype = "dashed") +
        geom_hline(yintercept = 0, colour = "black") +
        # scale_colour_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
        labs(
          title    = "Standardised Residuals vs Fitted",
          x        = "Fitted Values",
          y        = "Standardised Residuals",
          subtitle = paste0(sum(plot_df$Outlier), " observations beyond ±", threshold, " SD")
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    
    #Boxplot/density plot of residuals
    output$outlier_density_box_plot <- renderPlot({
      result <- reactive_model()
      req(result)
      
      resid <- result$test_actual - result$test_preds
      
      plot_df <- data.frame(
        CODE = as.character(result$test_processed$CODE),
        Residual = resid,
        stringsAsFactors = FALSE
      )
      
      q1 <- quantile(plot_df$Residual, 0.25, na.rm = TRUE)
      q3 <- quantile(plot_df$Residual, 0.75, na.rm = TRUE)
      iqr_val <- IQR(plot_df$Residual, na.rm = TRUE)
      
      iqr_mult <- input$plot_iqr_mult
      
      lower_bound <- q1 - iqr_mult * iqr_val
      upper_bound <- q3 + iqr_mult * iqr_val
      
      plot_df$Outlier <- plot_df$Residual < lower_bound | plot_df$Residual > upper_bound
      plot_df$Label <- ifelse(plot_df$Outlier, plot_df$CODE, NA)
      
      #If label outlier box not ticked, then labels are blank
      if (!isTRUE(input$outlier_boxplot_label)){plot_df$Label <- ""}
      
      p_density <- ggplot(plot_df, aes(x = Residual)) +
        geom_density(fill = "steelblue", alpha = 0.4) +
        geom_vline(xintercept = mean(plot_df$Residual, na.rm = TRUE), linetype = "dashed") +
        labs(
          title = "Residual Distribution",
          y = "Density",
          subtitle = paste0("IQR Multiplier = ", iqr_mult, "\n", sum(plot_df$Outlier), " Observations Beyond ± " , iqr_mult, " IQR Multiplier Range")

        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
      p_box <- ggplot(plot_df, aes(x = "", y = Residual)) +
        geom_boxplot(outlier.shape = NA, width = 0.25) +
        geom_point(
          data = subset(plot_df, Outlier),
          colour = "red",
          position = position_jitter(width = 0.05)
        ) +
        ggrepel::geom_text_repel(
          data = subset(plot_df, Outlier),
          aes(label = Label),
          max.overlaps = 20,
          na.rm = TRUE
        ) +
        coord_flip() +
        labs(x = NULL, y = "Residual") +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
      
      p_density / p_box + patchwork::plot_layout(heights = c(3, 1))
    })
# ===================================================================================
    
    } 

)
