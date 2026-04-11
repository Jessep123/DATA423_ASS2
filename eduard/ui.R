# ui.R - CORRECTED WITH TOGGLE BUTTON ABOVE THE PLOT

ui <- navbarPage(
  title = "DATA423 Assignment 2 - Eduard Bradley",
  theme = shinytheme("cerulean"),
  useShinyjs(),
  
  # Add custom CSS for sidebar toggling
  tags$head(
    tags$style(HTML("
    .sidebar-wrapper {
      position: relative;
      transition: all 0.3s ease;
    }
    .sidebar-hidden .sidebar-panel {
      display: none !important;
    }
    .sidebar-hidden .main-panel {
      width: 100% !important;
      margin-left: 0 !important;
    }
    .sidebar-panel {
      transition: all 0.3s ease;
    }
    .main-panel {
      transition: all 0.3s ease;
    }
    .toggle-container {
      margin-bottom: 15px;
      text-align: right;
    }
    
    /* Specific rule for correlation tab - target the sidebar column */
    #correlation_wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    #correlation_wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
    }
    
    /* Specific rule for datatable tab */
    #datatable_wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    #datatable_wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
    }
    
    /* Specific rule for heatmap tab */
    #heatmap_wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    #heatmap_wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
    }
    
    /* Specific rule for distribution tab */
    #distribution_wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    #distribution_wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
    }
    
    /* Specific rule for boxplot tab */
    #boxplot_wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    #boxplot_wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
    }
    
    /* Specific rule for scatter tab */
    #scatter_wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    #scatter_wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
    }
    
    /* Specific rule for mosaic tab */
    #mosaic_wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    #mosaic_wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
    }
    
    /* Specific rule for ggpairs tab */
    #ggpairs_wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    #ggpairs_wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
    }
    
    /* Specific rule for tabplot tab */
    #tabplot_wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    #tabplot_wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
    }
    
    /* Specific rule for rising values tab */
    #rising_wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    #rising_wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
    }
  "))
  ),
  
  # Add custom CSS for master sidebar toggling
  tags$head(
    tags$style(HTML("
    /* Existing CSS remains here */
    
    /* Master sidebar toggle styles */
    #master_sidebar_wrapper.sidebar-hidden #master_sidebar_panel {
      display: none !important;
    }
    #master_sidebar_wrapper.sidebar-hidden #master_main_panel {
      width: 100% !important;
      margin-left: 0 !important;
    }
    #master_sidebar_panel {
      transition: all 0.3s ease;
    }
    #master_main_panel {
      transition: all 0.3s ease;
    }
    "))
  ),
  
  
  # ==========================================================================
  # ==========================================================================
  # TAB 1: SUMMARY & EDA
  # ==========================================================================
  # ==========================================================================
  
  tabPanel("Summary & EDA",
           fluidPage(
             h2("Exploratory Data Analysis"),
             hr(),
             
             tabsetPanel(
               
               # ============================================================================
               # 1. SUMMARY SUB-TAB
               # ============================================================================
               
               tabPanel("Summary",
                        fluidPage(
                          
                          h2("Dataset Summary Statistics"),
                          hr(),
                          
                          # -------------------------
                          # Dataset Overview Stats
                          # -------------------------
                          fluidRow(
                            column(4, 
                                   div(class = "well", style = "padding: 10px;",
                                       h5("Dataset Dimensions", style = "margin-top: 2px; margin-bottom: 5px;"),
                                       hr(style = "margin-top: 5px; margin-bottom: 10px;"),
                                       fluidRow(
                                         column(6,
                                                h4(textOutput("summary_row_count"), style = "margin: 2px;"),
                                                p("Rows", style = "font-size: 12px; margin: 0;"),
                                                br(),
                                                h4(textOutput("summary_total_cells"), style = "margin: 2px;"),
                                                p("Total Cells", style = "font-size: 12px; margin: 0;")
                                         ),
                                         column(6,
                                                h4(textOutput("summary_col_count"), style = "margin: 2px;"),
                                                p("Columns", style = "font-size: 12px; margin: 0;"),
                                                br(),
                                                h4(textOutput("summary_non_na_values"), style = "margin: 2px;"),
                                                p("Data Points", style = "font-size: 12px; margin: 0;")
                                         )
                                       )
                                   )
                            ),
                            column(8,
                                   div(class = "well", style = "padding: 10px;",
                                       h5("Data Quality", style = "margin-top: 2px; margin-bottom: 5px;"),
                                       hr(style = "margin-top: 5px; margin-bottom: 10px;"),
                                       fluidRow(
                                         column(3, uiOutput("summary_complete_cases")),
                                         column(3, uiOutput("summary_incomplete_cases")),
                                         column(3, uiOutput("summary_missing_cells")),
                                         column(3, uiOutput("summary_data_completeness"))
                                       ),
                                       fluidRow(
                                         column(3, uiOutput("summary_numeric_count")),
                                         column(3, uiOutput("summary_categorical_count")),
                                         column(6, uiOutput("summary_missing_types"))
                                       )
                                   )
                            )
                          ),
                          
                          br(),
                          
                          # -------------------------
                          # Numeric Variables Summary
                          # -------------------------
                          tags$div(
                            class = "panel-group",
                            tags$div(
                              class = "panel panel-default",
                              tags$div(
                                class = "panel-heading",
                                tags$h4(
                                  class = "panel-title",
                                  tags$a(`data-toggle` = "collapse", href = "#collapseNumeric", 
                                         "Numeric Variables Summary (11 variables)",
                                         style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                )
                              ),
                              tags$div(
                                id = "collapseNumeric",
                                class = "panel-collapse collapse",
                                tags$div(
                                  class = "panel-body",
                                  DTOutput("summary_numeric_table")
                                )
                              )
                            )
                          ),
                          
                          br(),
                          
                          # -------------------------
                          # Categorical Variables Summary
                          # -------------------------
                          tags$div(
                            class = "panel-group",
                            tags$div(
                              class = "panel panel-default",
                              tags$div(
                                class = "panel-heading",
                                tags$h4(
                                  class = "panel-title",
                                  tags$a(`data-toggle` = "collapse", href = "#collapseCategorical", 
                                         "Categorical Variables Summary (4 variables)",
                                         style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                )
                              ),
                              tags$div(
                                id = "collapseCategorical",
                                class = "panel-collapse collapse",
                                tags$div(
                                  class = "panel-body",
                                  DTOutput("summary_categorical_table")
                                )
                              )
                            )
                          ),
                          
                          br(),
                          
                          # -------------------------
                          # Missing Values Summary
                          # -------------------------
                          tags$div(
                            class = "panel-group",
                            tags$div(
                              class = "panel panel-default",
                              tags$div(
                                class = "panel-heading",
                                tags$h4(
                                  class = "panel-title",
                                  tags$a(`data-toggle` = "collapse", href = "#collapseMissing", 
                                         "Missing Values Summary by Variable",
                                         style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                )
                              ),
                              tags$div(
                                id = "collapseMissing",
                                class = "panel-collapse collapse",
                                tags$div(
                                  class = "panel-body",
                                  DTOutput("summary_missing_table")
                                )
                              )
                            )
                          ),
                          
                          br(),
                          
                          # -------------------------
                          # Summarytools dfSummary
                          # -------------------------
                          tags$div(
                            class = "panel-group",
                            tags$div(
                              class = "panel panel-default",
                              tags$div(
                                class = "panel-heading",
                                tags$h4(
                                  class = "panel-title",
                                  tags$a(`data-toggle` = "collapse", href = "#collapseDFSummary", 
                                         "Complete Data Summary (summarytools)",
                                         style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                )
                              ),
                              tags$div(
                                id = "collapseDFSummary",
                                class = "panel-collapse collapse",
                                tags$div(
                                  class = "panel-body",
                                  div(class = "summarytools-container",
                                      htmlOutput("summary_dfsummary")
                                  )
                                )
                              )
                            )
                          ),
                          
                          br()
                        )
               ),
               
               # ==========================================================================
               # 2. BOXPLOT ANALYSIS SUB-TAB
               # ==========================================================================
               
               tabPanel("Boxplot Analysis",
                        div(id = "boxplot_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "boxplot_sidebar", class = "sidebar-panel",
                                           width = 3,
                                           
                                           # -----------------------------
                                           # Visualisation Options
                                           # -----------------------------
                                           h4("Data Transformation:"),
                                           sliderInput("iqr_boxplot", "Outlier IQR Multiplier:", 
                                                       min = 0.5, max = 15, value = 1.5, step = 0.1),
                                           checkboxInput("boxplot_center", "Center data:", TRUE),
                                           checkboxInput("boxplot_scale", "Scale data:", TRUE),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Variable Selection
                                           # -----------------------------
                                           h4("Variable Selection:"),
                                           pickerInput("boxplot_numeric_vars", "Select Numeric Variables:", 
                                                       choices = c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", 
                                                                   "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE", 
                                                                   "HEALTHCARE_COST", "DEATH_RATE"),
                                                       selected = c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", 
                                                                    "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE", 
                                                                    "HEALTHCARE_COST", "DEATH_RATE"),
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           pickerInput("boxplot_cat_vars_group", "Group By (Optional):", 
                                                       choices = c("None", "GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE"),
                                                       selected = "None",
                                                       multiple = FALSE),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Categorical Filters (for subsetting data)
                                           # -----------------------------
                                           h4("Filter Data by Categorical Variables:"),
                                           pickerInput("boxplot_filter_vars", "Select Categorical Variables to Filter:", 
                                                       choices = c("GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE"),
                                                       selected = NULL,
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           uiOutput("boxplot_cat_filters"),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Missing Value Handling
                                           # -----------------------------
                                           h4("Missing Value Handling:"),
                                           checkboxInput("include_null_boxplot", "Include R NA values in visualisation", value = FALSE),
                                           p("Note: -99, --, and 'NA' strings are always treated as missing. ",
                                             "R NA values are actual R missing values. When unchecked, rows with any R NA are excluded.",
                                             style = "font-size: 11px; color: #666; margin-top: 8px;"),
                                           
                                           br(),
                                           
                                           actionButton("reset_boxplot", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel",
                                        width = 9,
                                        
                                        # Toggle button above the plot
                                        div(class = "toggle-container",
                                            actionButton("toggle_boxplot_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        
                                        shinycssloaders::withSpinner(
                                          plotlyOutput("boxplot_plot", height = "600px"),
                                          type = 4,
                                          color = "#2C3E50",
                                          size = 0.7
                                        ),
                                        br(),
                                        hr(),
                                        
                                        # Collapsible Boxplot Statistics Summary
                                        tags$div(
                                          class = "panel-group",
                                          tags$div(
                                            class = "panel panel-default",
                                            tags$div(
                                              class = "panel-heading",
                                              tags$h4(
                                                class = "panel-title",
                                                tags$a(`data-toggle` = "collapse", href = "#collapseBoxplotStats", 
                                                       "Boxplot Statistics",
                                                       style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                              )
                                            ),
                                            tags$div(
                                              id = "collapseBoxplotStats",
                                              class = "panel-collapse collapse",
                                              tags$div(
                                                class = "panel-body",
                                                verbatimTextOutput("boxplot_stats")
                                              )
                                            )
                                          )
                                        )
                              )
                            )
                        )
               ),
               
               # ==========================================================================
               # 3. CORRELATION ANALYSIS SUB-TAB
               # ==========================================================================
               
               tabPanel("Correlation Analysis",
                        div(id = "correlation_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "correlation_sidebar", class = "sidebar-panel",
                                           width = 3,
                                           
                                           # -----------------------------
                                           # Visualisation Options
                                           # -----------------------------
                                           h4("Correlation Settings:"),
                                           selectInput("corr_method", "Correlation Method:",
                                                       choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"),
                                                       selected = "pearson"),
                                           selectInput("corr_order", "Variable Ordering:",
                                                       choices = c("Original" = "original", "AOE", "FPC", 
                                                                   "Hierarchical Clustering" = "hclust", 
                                                                   "Alphabetical" = "alphabet"),
                                                       selected = "AOE"),
                                           conditionalPanel(
                                             condition = "input.corr_order == 'hclust'",
                                             selectInput("hclust_method", "Clustering Method:",
                                                         choices = c("ward", "ward.D", "ward.D2", "single", "complete", 
                                                                     "average", "mcquitty", "median", "centroid"),
                                                         selected = "complete")
                                           ),
                                           hr(),
                                           h5("Further Features:"),
                                           checkboxInput("corr_abs", "Use Absolute Correlations:", FALSE),
                                           checkboxInput("corr_show_values", "Show Correlation Values:", TRUE),
                                           conditionalPanel(
                                             condition = "input.corr_show_values == true",
                                             selectInput("corr_digits", "Decimal Places:",
                                                         choices = c("1" = 1, "2" = 2, "3" = 3),
                                                         selected = 2)
                                           ),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Variable Selection
                                           # -----------------------------
                                           h4("Variable Selection:"),
                                           pickerInput("corr_numeric_vars", "Select Numeric Variables:", 
                                                       choices = c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", 
                                                                   "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE", 
                                                                   "HEALTHCARE_COST", "DEATH_RATE"),
                                                       selected = c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", 
                                                                    "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE", 
                                                                    "HEALTHCARE_COST", "DEATH_RATE"),
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           pickerInput("corr_categorical_vars", "Select Categorical Variables to Filter:", 
                                                       choices = c("GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE"),
                                                       selected = NULL,
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           uiOutput("corr_categorical_filters"),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Missing Value Handling
                                           # -----------------------------
                                           h4("Missing Value Handling:"),
                                           checkboxGroupInput("corr_mv_types", 
                                                              label = "Treat as missing:",
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")),
                                           
                                           checkboxInput("include_null_correlation", 
                                                         "Include NULL values in categorical filtering", 
                                                         value = FALSE),
                                           
                                           uiOutput("corr_null_explanation"),
                                           
                                           br(),
                                           
                                           actionButton("reset_correlation", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel",
                                        width = 9,
                                        
                                        # Toggle button above the plot
                                        div(class = "toggle-container",
                                            actionButton("toggle_correlation_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        
                                        shinycssloaders::withSpinner(
                                          plotlyOutput("correlation_plot", height = "600px"),
                                          type = 4,
                                          color = "#2C3E50",
                                          size = 0.7
                                        ),
                                        br(),
                                        hr(),
                                        
                                        # Collapsible Correlation Statistics Summary
                                        tags$div(
                                          class = "panel-group",
                                          tags$div(
                                            class = "panel panel-default",
                                            tags$div(
                                              class = "panel-heading",
                                              tags$h4(
                                                class = "panel-title",
                                                tags$a(`data-toggle` = "collapse", href = "#collapseCorrelationStats", 
                                                       "Correlation Statistics",
                                                       style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                              )
                                            ),
                                            tags$div(
                                              id = "collapseCorrelationStats",
                                              class = "panel-collapse collapse",
                                              tags$div(
                                                class = "panel-body",
                                                verbatimTextOutput("correlation_stats")
                                              )
                                            )
                                          )
                                        ),
                                        
                                        br(),
                                        
                                        # Observation Count (not collapsible)
                                        textOutput("corr_obs_count")
                              )
                            )
                        )
               ),
               
               # ==========================================================================
               # 4. RISING VALUES SUB-TAB
               # ==========================================================================
               
               tabPanel("Rising Values",
                        div(id = "rising_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "rising_sidebar", class = "sidebar-panel",
                                           width = 3,
                                           
                                           # -----------------------------
                                           # Visualisation Options
                                           # -----------------------------
                                           h4("Plot Settings:"),
                                           materialSwitch("rv_standardise", "Standardise Variables (Z-score)", 
                                                          value = TRUE, status = "primary"),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Variable Selection
                                           # -----------------------------
                                           h4("Variable Selection:"),
                                           pickerInput("rv_numeric_vars", "Select Numeric Variables:", 
                                                       choices = numeric_cols,
                                                       selected = "DEATH_RATE",
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                                      `selected-text-format` = "count > 3",
                                                                      `count-selected-text` = "{0} variables selected")),
                                           
                                           pickerInput("rv_categorical_vars", "Select Categorical Variables (for filtering):", 
                                                       choices = categorical_cols[categorical_cols != "CODE"],
                                                       selected = NULL,
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                                      `selected-text-format` = "count > 2",
                                                                      `count-selected-text` = "{0} variables selected")),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Categorical Filters
                                           # -----------------------------
                                           uiOutput("rv_categorical_filters"),
                                           
                                           br(),
                                           
                                           # -----------------------------
                                           # Missing Value Handling
                                           # -----------------------------
                                           h4("Missing Value Handling:"),
                                           checkboxGroupInput("rv_mv_types", 
                                                              label = "Treat as missing:",
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")),
                                           
                                           materialSwitch("rv_include_null", "Include NULL values in filtering", 
                                                          value = FALSE, status = "primary"),
                                           
                                           br(),
                                           
                                           actionButton("reset_rising", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel",
                                        width = 9,
                                        div(class = "toggle-container",
                                            actionButton("toggle_rising_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        shinycssloaders::withSpinner(
                                          plotlyOutput("rising_plot", height = "600px"),
                                          type = 4,
                                          color = "#2C3E50",
                                          size = 0.7
                                        ),
                                        br(),
                                        hr(),
                                        
                                        # Expandable box for summary
                                        tags$div(class = "panel-group",
                                                 tags$div(class = "panel panel-default",
                                                          tags$div(class = "panel-heading",
                                                                   tags$h4(class = "panel-title",
                                                                           tags$a(`data-toggle` = "collapse", href = "#collapseRisingStats", 
                                                                                  "Rising Values Summary",
                                                                                  style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                                   )
                                                          ),
                                                          tags$div(id = "collapseRisingStats", class = "panel-collapse collapse",
                                                                   tags$div(class = "panel-body",
                                                                            verbatimTextOutput("rising_obs_count")
                                                                   )
                                                          )
                                                 )
                                        )
                              )  
                            )  
                        ) 
               ),
               
               # ==========================================================================
               # 5. MISSING VALUES HEATMAP SUB-TAB
               # ==========================================================================
               
               tabPanel("Missing Values Heatmap",
                        div(id = "heatmap_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "heatmap_sidebar", class = "sidebar-panel",
                                           width = 3,
                                           
                                           # -----------------------------
                                           # Visualisation Options
                                           # -----------------------------
                                           h4("Missing Value Types to Display:"),
                                           checkboxGroupInput("mv_types", 
                                                              label = NULL,
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")
                                           ),
                                           
                                           hr(),
                                           
                                           h4("Missingness Thresholds:"),
                                           sliderInput("col_missing_threshold", "Max Column Missing %:", 
                                                       min = 0, max = 100, value = 100, step = 5, post = "%"),
                                           sliderInput("row_missing_threshold", "Max Row Missing Count:", 
                                                       min = 0, max = 15, value = 15, step = 1),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Variable Selection
                                           # -----------------------------
                                           h4("Variable Selection:"),
                                           
                                           pickerInput("heatmap_cat_vars", "Select Categorical Variables:", 
                                                       choices = c("GOVERN_TYPE", "HEALTHCARE_BASIS"),
                                                       selected = c("GOVERN_TYPE", "HEALTHCARE_BASIS"),
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE)
                                           ),
                                           
                                           pickerInput("heatmap_numeric_vars", "Select Numeric Variables:", 
                                                       choices = c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", 
                                                                   "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE", 
                                                                   "HEALTHCARE_COST", "DEATH_RATE"),
                                                       selected = c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", 
                                                                    "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE", 
                                                                    "HEALTHCARE_COST", "DEATH_RATE"),
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE)
                                           ),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Categorical Variable Filters
                                           # -----------------------------
                                           uiOutput("heatmap_cat_filters"),
                                           
                                           # -----------------------------
                                           # Numeric Variable Filters
                                           # -----------------------------
                                           uiOutput("heatmap_num_filters"),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Display Options
                                           # -----------------------------
                                           radioButtons("heatmap_order", "Order Variables:",
                                                        choices = c("Original" = "original", "Most Missing First" = "desc"),
                                                        selected = "original"
                                           ),
                                           
                                           br(),
                                           
                                           actionButton("reset_heatmap", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel",
                                        width = 9,
                                        div(class = "toggle-container",
                                            actionButton("toggle_heatmap_sidebar",  # <-- FIXED: was "toggle_rising_sidebar"
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm toggle-btn",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;"
                                            )
                                        ),
                                        
                                        shinycssloaders::withSpinner(
                                          plotlyOutput("heatmap_plot", height = "700px"),  # <-- FIXED: was "rising_plot"
                                          type = 4,
                                          color = "#2C3E50",
                                          size = 0.7
                                        ),
                                        
                                        br(),
                                        hr(),
                                        
                                        # Collapsible Missingness Summary
                                        tags$div(
                                          class = "panel-group",
                                          tags$div(
                                            class = "panel panel-default",
                                            tags$div(
                                              class = "panel-heading",
                                              tags$h4(
                                                class = "panel-title",
                                                tags$a(`data-toggle` = "collapse", href = "#collapseHeatmapSummary", 
                                                       "Missingness Summary",
                                                       style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                              )
                                            ),
                                            tags$div(
                                              id = "collapseHeatmapSummary",
                                              class = "panel-collapse collapse",
                                              tags$div(
                                                class = "panel-body",
                                                verbatimTextOutput("heatmap_summary")
                                              )
                                            )
                                          )
                                        )
                              ) 
                            )   
                        )  
               ),  
               
               # ==========================================================================
               # 6. DISTRIBUTION PLOTS SUB-TAB
               # ==========================================================================
               
               tabPanel("Distribution Plots",
                        div(id = "distribution_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "distribution_sidebar", class = "sidebar-panel",
                                           width = 3,
                                           
                                           # -----------------------------
                                           # Visualisation Options
                                           # -----------------------------
                                           h4("Plot Settings:"),
                                           sliderInput("dist_bins", "Number of Bins:", min = 5, max = 50, value = 30),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Variable Selection
                                           # -----------------------------
                                           h4("Variable Selection:"),
                                           selectInput("dist_var", "Select Variable:",
                                                       choices = c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", 
                                                                   "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE", 
                                                                   "HEALTHCARE_COST", "DEATH_RATE", "GOVERN_TYPE", "HEALTHCARE_BASIS"),
                                                       selected = "DEATH_RATE"
                                           ),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Missing Value Handling
                                           # -----------------------------
                                           h4("Missing Value Handling:"),
                                           checkboxGroupInput("dist_mv_types", 
                                                              label = "Treat as missing (exclude from plot):",
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")
                                           ),
                                           
                                           p("Note: Values treated as missing are excluded from the histogram and rug plot.",
                                             style = "font-size: 11px; color: #666; margin-top: 8px;"),
                                           
                                           br(),
                                           
                                           actionButton("reset_distribution", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel",
                                        width = 9,
                                        
                                        # Toggle button above the plot
                                        div(class = "toggle-container",
                                            actionButton("toggle_distribution_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;"
                                            )
                                        ),
                                        
                                        plotlyOutput("distribution_plot", height = "500px"),
                                        br(),
                                        hr(),
                                        
                                        # Collapsible Statistics Summary
                                        tags$div(
                                          class = "panel-group",
                                          tags$div(
                                            class = "panel panel-default",
                                            tags$div(
                                              class = "panel-heading",
                                              tags$h4(
                                                class = "panel-title",
                                                tags$a(`data-toggle` = "collapse", href = "#collapseDistStats", 
                                                       "Distribution Statistics",
                                                       style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                              )
                                            ),
                                            tags$div(
                                              id = "collapseDistStats",
                                              class = "panel-collapse collapse",
                                              tags$div(
                                                class = "panel-body",
                                                verbatimTextOutput("distribution_stats")
                                              )
                                            )
                                          )
                                        )
                              )
                            )
                        )
               ),
               
               # ==========================================================================
               # 7. SCATTER PLOT SUB-TAB
               # ==========================================================================
               
               tabPanel("Scatter Plot",
                        div(id = "scatter_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "scatter_sidebar", class = "sidebar-panel",
                                           width = 3,
                                           
                                           # -----------------------------
                                           # Variable Selection
                                           # -----------------------------
                                           h4("Variable Selection:"),
                                           selectInput("scatter_x", "X-Axis Variable:", 
                                                       choices = numeric_cols, selected = "POP_DENSITY"),
                                           selectInput("scatter_y", "Y-Axis Variable:", 
                                                       choices = numeric_cols, selected = "DEATH_RATE"),
                                           selectInput("scatter_color", "Color By:", 
                                                       choices = c("None", "GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE"), selected = "None"),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Missing Value Handling
                                           # -----------------------------
                                           h4("Missing Value Handling:"),
                                           checkboxGroupInput("scatter_mv_types", 
                                                              label = "Treat as missing:",
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")),
                                           
                                           br(),
                                           
                                           actionButton("reset_scatter", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel",
                                        width = 9,
                                        
                                        # Toggle button above the plot
                                        div(class = "toggle-container",
                                            actionButton("toggle_scatter_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        
                                        shinycssloaders::withSpinner(
                                          plotlyOutput("scatter_plot", height = "500px"),
                                          type = 4,
                                          color = "#2C3E50",
                                          size = 0.7
                                        ),
                                        br(),
                                        hr(),
                                        
                                        # Collapsible Scatter Plot Summary
                                        tags$div(
                                          class = "panel-group",
                                          tags$div(
                                            class = "panel panel-default",
                                            tags$div(
                                              class = "panel-heading",
                                              tags$h4(
                                                class = "panel-title",
                                                tags$a(`data-toggle` = "collapse", href = "#collapseScatterStats", 
                                                       "Scatter Plot Summary",
                                                       style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                              )
                                            ),
                                            tags$div(
                                              id = "collapseScatterStats",
                                              class = "panel-collapse collapse",
                                              tags$div(
                                                class = "panel-body",
                                                verbatimTextOutput("scatter_correlation")
                                              )
                                            )
                                          )
                                        )
                              )
                            )
                        )
               ),
               
               # ==========================================================================
               # 8. GGPAIRS PLOT SUB-TAB
               # ==========================================================================
               
               tabPanel("GGpairs Plot",
                        div(id = "ggpairs_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "ggpairs_sidebar", class = "sidebar-panel",
                                           width = 3,
                                           
                                           # -----------------------------
                                           # Visualisation Options
                                           # -----------------------------
                                           h4("Plot Settings:"),
                                           selectInput("ggpairs_color", "Color By:", 
                                                       choices = c("None", "GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE"), 
                                                       selected = "None"),
                                           
                                           p("Note: Color adds a categorical variable to the plot", 
                                             style = "font-size: 11px; color: #666; margin-top: 5px;"),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Variable Selection
                                           # -----------------------------
                                           h4("Variable Selection:"),
                                           div(
                                             style = "margin-bottom: 5px; text-align: right;",
                                             actionLink("deselect_numeric", "Deselect All", 
                                                        style = "font-size: 11px; color: #dc3545;")
                                           ),
                                           pickerInput("ggpairs_numeric", "Select Numeric Variables (min 2, max 11 recommended):", 
                                                       choices = c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", 
                                                                   "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE", 
                                                                   "HEALTHCARE_COST", "DEATH_RATE"),
                                                       selected = c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN"),
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                                      `selected-text-format` = "count > 3",
                                                                      `count-selected-text` = "{0} variables selected")),
                                           
                                           uiOutput("ggpairs_total_count"),
                                           
                                           div(
                                             class = "alert alert-info",
                                             style = "padding: 8px; margin-top: 5px; font-size: 11px;",
                                             icon("info-circle"),
                                             HTML("<strong>Plot Features:</strong><br>
               • Diagonal: Smoothed density plots<br>
               • Lower triangle: Scatter plots with hover showing:<br>
               &nbsp;&nbsp;&nbsp;• CODE (State identifier)<br>
               &nbsp;&nbsp;&nbsp;• X variable and value<br>
               &nbsp;&nbsp;&nbsp;• Y variable and value<br>
               &nbsp;&nbsp;&nbsp;• Color (if selected)<br>
               • Upper triangle: Correlation values")
                                           ),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Missing Value Handling
                                           # -----------------------------
                                           h4("Missing Value Handling:"),
                                           checkboxGroupInput("ggpairs_mv_types", 
                                                              label = "Treat as missing:",
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")),
                                           
                                           p("Note: Rows with missing values are removed from the plot.", 
                                             style = "font-size: 11px; color: #666; margin-top: 5px;"),
                                           
                                           br(),
                                           
                                           actionButton("reset_ggpairs", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel",
                                        width = 9,
                                        
                                        # Toggle button above the plot
                                        div(class = "toggle-container",
                                            actionButton("toggle_ggpairs_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        
                                        shinycssloaders::withSpinner(
                                          plotlyOutput("ggpairs_plot", height = "800px"),
                                          type = 4,
                                          color = "#2C3E50",
                                          size = 0.7
                                        ),
                                        br(),
                                        hr(),
                                        bsCollapse(
                                          id = "ggpairs_summary_collapse",
                                          open = NULL,
                                          bsCollapsePanel(
                                            title = "GGpairs Plot Summary",
                                            style = "info",
                                            verbatimTextOutput("ggpairs_obs_count")
                                          )
                                        )
                              )
                            )
                        )
               ),
               
               # ==========================================================================
               # 9. TABPLOT SUB-TAB
               # ==========================================================================
               
               tabPanel("Tableplot",
                        div(id = "tabplot_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "tabplot_sidebar", class = "sidebar-panel",
                                           width = 3,
                                           
                                           # -----------------------------
                                           # Visualisation Options
                                           # -----------------------------
                                           h4("Tableplot Settings:"),
                                           sliderInput("tabplot_nbins", "Number of Bins:", 
                                                       min = 10, max = 200, value = 100, step = 10),
                                           radioButtons("tabplot_code_order", "CODE Order:",
                                                        choices = c("Ascending (A→Z)" = "asc", 
                                                                    "Descending (Z→A)" = "desc"),
                                                        selected = "asc"),
                                           materialSwitch("tabplot_showNA", "Show NA Values", value = TRUE),
                                           materialSwitch("tabplot_include_null", "Include NULL Values", value = TRUE),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Variable Selection
                                           # -----------------------------
                                           h4("Variable Selection:"),
                                           p("CODE is automatically included as the first variable", 
                                             style = "font-size: 11px; color: #2C3E50; margin-top: 5px; margin-bottom: 10px; font-style: italic;"),
                                           
                                           pickerInput("tabplot_numeric_vars", "Select Numeric Variables:", 
                                                       choices = numeric_cols,
                                                       selected = numeric_cols,  # Select ALL numeric variables
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                                      `selected-text-format` = "count > 3",
                                                                      `count-selected-text` = "{0} variables selected")),
                                           
                                           pickerInput("tabplot_categorical_vars", "Select Categorical Variables:", 
                                                       choices = categorical_cols[categorical_cols != "CODE"],
                                                       selected = categorical_cols[categorical_cols != "CODE"],  # Select ALL categorical variables
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                                      `selected-text-format` = "count > 2",
                                                                      `count-selected-text` = "{0} variables selected")),
                                           
                                           hr(),
                                           
                                           # -----------------------------
                                           # Missing Value Handling
                                           # -----------------------------
                                           h4("Missing Value Handling:"),
                                           checkboxGroupInput("tabplot_mv_types", 
                                                              label = "Treat as missing:",
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")),
                                           
                                           # -----------------------------
                                           # Categorical Filters
                                           # -----------------------------
                                           uiOutput("tabplot_categorical_filters"),
                                           
                                           br(),
                                           
                                           actionButton("reset_tabplot", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel",
                                        width = 9,
                                        
                                        # Toggle button above the plot
                                        div(class = "toggle-container",
                                            actionButton("toggle_tabplot_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        
                                        shinycssloaders::withSpinner(
                                          plotOutput("tabplot_plot", height = "700px"),
                                          type = 4,
                                          color = "#2C3E50",
                                          size = 0.7
                                        ),
                                        br(),
                                        hr(),
                                        verbatimTextOutput("tabplot_obs_count")
                              )
                            )
                        )
               ),
               
               # ==========================================================================
               # 10. MOSAIC PLOT SUB-TAB
               # ==========================================================================
               
               tabPanel("Mosaic Plot",
                        div(id = "mosaic_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "mosaic_sidebar", class = "sidebar-panel",
                                           width = 3,
                                           
                                           # -----------------------------
                                           # Variable Selection
                                           # -----------------------------
                                           h4("Variable Selection:"),
                                           selectInput("mosaic_x", "X-Axis Variable:", 
                                                       choices = c("GOVERN_TYPE", "HEALTHCARE_BASIS"), selected = "GOVERN_TYPE"),
                                           selectInput("mosaic_y", "Y-Axis Variable:", 
                                                       choices = c("GOVERN_TYPE", "HEALTHCARE_BASIS"), selected = "HEALTHCARE_BASIS"),
                                           selectInput("mosaic_z", "Third Variable (Optional):", 
                                                       choices = c("None", "GOVERN_TYPE", "HEALTHCARE_BASIS"), selected = "None"),
                                           
                                           hr()
                                           
                              ),
                              
                              mainPanel(class = "main-panel",
                                        width = 9,
                                        
                                        # Toggle button above the plot
                                        div(class = "toggle-container",
                                            actionButton("toggle_mosaic_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm toggle-btn",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;"
                                            )
                                        ),
                                        
                                        shinycssloaders::withSpinner(
                                          plotOutput("mosaic_plot", height = "500px"),
                                          type = 4,
                                          color = "#2C3E50",
                                          size = 0.7
                                        ),
                                        br(),
                                        hr(),
                                        verbatimTextOutput("mosaic_stats")
                              )
                            )
                        )
               ),
               
               # ==========================================================================
               # 11. RAW DATA SUB-TAB
               # ==========================================================================
               
               tabPanel("Raw Data",
                        div(id = "datatable_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "sidebar_datatable", class = "sidebar-panel",
                                           width = 3,
                                           
                                           h4("Data Table Options"),
                                           
                                           # Row range slider
                                           sliderInput("dt_row_range", 
                                                       "Select Row Range:",
                                                       min = 1, 
                                                       max = nrow(df), 
                                                       value = c(1, nrow(df)),
                                                       step = 1),
                                           
                                           hr(),
                                           
                                           # Column type selection tabs
                                           h4("Column Selection"),
                                           tabsetPanel(
                                             id = "dt_column_tabs",
                                             type = "tabs",
                                             
                                             # All Columns tab
                                             tabPanel(
                                               "All Columns",
                                               br(),
                                               div(
                                                 style = "margin-bottom: 10px;",
                                                 actionButton("dt_all_select_all", "Select All Columns", 
                                                              class = "btn-primary btn-sm", style = "margin-right: 10px;"),
                                                 actionButton("dt_all_deselect_all", "Deselect All Columns", 
                                                              class = "btn-danger btn-sm")
                                               ),
                                               pickerInput("dt_all_columns", "Choose columns to display:",
                                                           choices = NULL,
                                                           multiple = TRUE,
                                                           options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                                          `selected-text-format` = "count > 5",
                                                                          `count-selected-text` = "{0} columns selected"))
                                             ),
                                             
                                             # Numeric Columns tab
                                             tabPanel(
                                               "Numeric Columns",
                                               br(),
                                               div(
                                                 style = "margin-bottom: 10px;",
                                                 actionButton("dt_num_select_all", "Select All Numeric", 
                                                              class = "btn-primary btn-sm", style = "margin-right: 10px;"),
                                                 actionButton("dt_num_deselect_all", "Deselect All Numeric", 
                                                              class = "btn-danger btn-sm")
                                               ),
                                               pickerInput("dt_numeric_columns", "Choose numeric columns to display:",
                                                           choices = numeric_cols,
                                                           multiple = TRUE,
                                                           options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                                          `selected-text-format` = "count > 5",
                                                                          `count-selected-text` = "{0} columns selected")),
                                               p("Note: CODE and OBS_TYPE are always included for context.", 
                                                 style = "font-size: 11px; color: #666; margin-top: 8px;")
                                             ),
                                             
                                             # Categorical Columns tab
                                             tabPanel(
                                               "Categorical Columns",
                                               br(),
                                               div(
                                                 style = "margin-bottom: 10px;",
                                                 actionButton("dt_cat_select_all", "Select All Categorical", 
                                                              class = "btn-primary btn-sm", style = "margin-right: 10px;"),
                                                 actionButton("dt_cat_deselect_all", "Deselect All Categorical", 
                                                              class = "btn-danger btn-sm")
                                               ),
                                               pickerInput("dt_categorical_columns", "Choose categorical columns to display:",
                                                           choices = categorical_cols,
                                                           multiple = TRUE,
                                                           options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                                          `selected-text-format` = "count > 5",
                                                                          `count-selected-text` = "{0} columns selected")),
                                               p("Note: CODE and OBS_TYPE are always included for context.", 
                                                 style = "font-size: 11px; color: #666; margin-top: 8px;")
                                             )
                                           ),
                                           
                                           hr(),
                                           
                                           # Display Options
                                           h4("Display Options"),
                                           
                                           selectInput("dt_page_length",
                                                       "Rows per page:",
                                                       choices = c("10" = 10, 
                                                                   "25" = 25, 
                                                                   "50" = 50, 
                                                                   "100" = 100,
                                                                   "All" = -1),
                                                       selected = 25),
                                           
                                           br(),
                                           
                                           p("Use the filters below each column to subset the data."),
                                           p("Export buttons available for copying, CSV, Excel, PDF, and printing."),
                                           
                                           br(),
                                           
                                           # Reset button
                                           actionButton("reset_datatable", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel",
                                        width = 9,
                                        
                                        # Toggle button above the plot
                                        div(class = "toggle-container",
                                            actionButton("toggle_datatable_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        
                                        # Row range info
                                        div(
                                          style = "margin-bottom: 10px; padding: 8px; background-color: #f8f9fa; border-radius: 4px;",
                                          textOutput("dt_row_info")
                                        ),
                                        
                                        shinycssloaders::withSpinner(
                                          DTOutput("raw_data_table"),
                                          type = 4,
                                          color = "#2C3E50",
                                          size = 0.7
                                        )
                              )
                            )
                        )
               )
             )
           )
  ),
  
  
  
  
  # ==========================================================================
  # ==========================================================================
  # TAB 2: DATA PROCESSING STRATEGY (Missingness + Outlier Analysis Only)
  # ==========================================================================
  # ==========================================================================
  
  tabPanel("Data Processing Strategy",
           fluidPage(
             h2("Missing Data & Outlier Processing Strategy"),
             hr(),
             
             fluidRow(
               column(12,
                      div(class = "alert alert-info",
                          icon("info-circle"),
                          "Analyze missing data patterns and detect outliers. Use the sidebar to filter and explore."
                      )
               )
             ),
             
             div(id = "master_sidebar_wrapper", class = "sidebar-wrapper",
                 div(class = "toggle-container", style = "margin-bottom: 15px; text-align: right;",
                     actionButton("toggle_master_sidebar", 
                                  label = HTML('<i class="fa fa-sliders-h"></i> Hide Master Filters'),
                                  class = "btn-primary btn-sm",
                                  style = "background-color: #2C3E50; border-color: #2C3E50;")
                 ),
                 
                 fluidRow(
                   column(3, id = "master_sidebar_panel",
                          div(class = "control-card",
                              h4("Data Filtering Controls", style = "color: #2C3E50; margin-top: 0;"),
                              h5("Variable Selection"),
                              pickerInput("master_cat_vars", "Categorical Variables:", 
                                          choices = categorical_cols[categorical_cols != "CODE"], 
                                          selected = categorical_cols[categorical_cols != "CODE"],  # Default selected
                                          multiple = TRUE,
                                          options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                              pickerInput("master_numeric_vars", "Numeric Variables:", 
                                          choices = numeric_cols, 
                                          selected = numeric_cols,  # Default selected
                                          multiple = TRUE,
                                          options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                              hr(),
                              h5("Missing Value Thresholds"),
                              sliderInput("master_col_missing_threshold", "Max Column Missing %:", 
                                          min = 0, max = 100, value = 100, step = 5, post = "%"),
                              sliderInput("master_row_missing_threshold", "Max Row Missing Count:", 
                                          min = 0, max = 15, value = 15, step = 1),
                              sliderInput("master_row_missing_percent", "Max Row Missing %:", 
                                          min = 0, max = 100, value = 100, step = 5, post = "%"),
                              hr(),
                              h5("Missing Value Types to Display"),
                              checkboxGroupInput("master_mv_types", NULL,
                                                 choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                 selected = c("neg99", "dash", "na_string")),
                              hr(),
                              h5("Column Ordering"),
                              radioButtons("master_order", "Order Variables:",
                                           choices = c("Original" = "original", "Most Missing First" = "desc"),
                                           selected = "original"),
                              br(),
                              actionButton("reset_master_filters", "Reset All Filters", 
                                           class = "btn-warning btn-sm", style = "width: 100%;")
                          )
                   ),
                   
                   column(9, id = "master_main_panel",
                          tabsetPanel(id = "processing_tabs",
                                      
                                      # ================================================================
                                      # SUB-TAB 1: MISSINGNESS ANALYSIS
                                      # ================================================================
                                      tabPanel("Missingness Analysis",
                                               br(),
                                               tabsetPanel(
                                                 tabPanel("Missing Values Heatmap",
                                                          br(),
                                                          shinycssloaders::withSpinner(plotlyOutput("proc_missing_heatmap", height = "550px"),
                                                                                       type = 4, color = "#2C3E50")
                                                 ),
                                                 tabPanel("Missingness Combinations (Upset)",
                                                          br(),
                                                          div(class = "control-card", style = "margin-bottom: 15px;",
                                                              h5("Upset Plot Settings", style = "margin-top: 0;"),
                                                              fluidRow(
                                                                column(4, numericInput("upset_nsets", "Number of Sets (Variables)", value = 11, min = 2, max = 15)),
                                                                column(4, numericInput("upset_nintersects", "Number of Intersections", value = 20, min = 5, max = 50)),
                                                                column(4, selectInput("upset_order", "Order By", choices = c("Frequency" = "freq", "Degree" = "degree", "Both" = "both"), selected = "freq"))
                                                              )
                                                          ),
                                                          uiOutput("pattern_selector"),
                                                          br(),
                                                          shinycssloaders::withSpinner(plotOutput("upset_plot", height = "500px"), type = 4, color = "#2C3E50"),
                                                          br(),
                                                          h5("Pattern Details:"),
                                                          uiOutput("pattern_description"),
                                                          br(),
                                                          h5("Observations with Selected Pattern:"),
                                                          DTOutput("pattern_data_table")
                                                 ),
                                                 tabPanel("Missingness Prediction (RPART)",
                                                          br(),
                                                          fluidRow(
                                                            column(6,
                                                                   selectInput("missing_target_var", "Predict when this variable is missing:", choices = names(df), selected = "HEALTHCARE_COST"),
                                                                   checkboxGroupInput("missing_rpart_types", "Missing Value Types:", choices = c("-99" = "neg99", "--" = "dash", "'NA'" = "na_string", "R NA" = "r_na"), selected = c("neg99", "dash", "na_string", "r_na")),
                                                                   pickerInput("missing_predictors", "Predictor Variables:", choices = names(df), selected = names(df), multiple = TRUE),
                                                                   actionButton("run_missing_rpart", "Build Missingness Tree", class = "btn-primary", style = "width: 100%;")
                                                            ),
                                                            column(6,
                                                                   sliderInput("missing_rpart_cp", "Complexity Parameter:", min = 0, max = 0.1, value = 0.01, step = 0.001),
                                                                   sliderInput("missing_rpart_minsplit", "Minimum Split:", min = 2, max = 50, value = 20, step = 1)
                                                            )
                                                          ),
                                                          br(),
                                                          shinycssloaders::withSpinner(plotOutput("missing_rpart_tree", height = "450px"), type = 4, color = "#2C3E50"),
                                                          br(),
                                                          shinycssloaders::withSpinner(plotlyOutput("missing_rpart_importance", height = "400px"), type = 4, color = "#2C3E50")
                                                 ),
                                                 tabPanel("Missing Variable Correlation",
                                                          br(),
                                                          fluidRow(
                                                            column(3,
                                                                   div(class = "control-card",
                                                                       selectInput("proc_missing_corr_method", "Correlation Method:", choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"), selected = "pearson"),
                                                                       selectInput("proc_missing_corr_order", "Ordering:", choices = c("Original" = "original", "AOE" = "AOE", "FPC" = "FPC", "Hierarchical Clustering" = "hclust", "Alphabetical" = "alphabet"), selected = "AOE"),
                                                                       checkboxInput("proc_missing_corr_abs", "Use Absolute Correlations", FALSE),
                                                                       checkboxInput("proc_missing_corr_display", "Show Values", TRUE)
                                                                   )
                                                            ),
                                                            column(9,
                                                                   shinycssloaders::withSpinner(plotlyOutput("proc_missing_corr_plot", height = "500px"), type = 4, color = "#2C3E50")
                                                            )
                                                          )
                                                 )
                                               )
                                      ),
                                      
                                      # ================================================================
                                      # SUB-TAB 2: OUTLIER ANALYSIS
                                      # ================================================================
                                      tabPanel("Outlier Analysis",
                                               br(),
                                               fluidRow(
                                                 column(3,
                                                        div(class = "control-card", h4("Mahalanobis Distance"), sliderInput("outlier_mahalanobis_threshold", "Chi-Square Threshold:", min = 0.9, max = 0.9999, value = 0.999, step = 0.001)),
                                                        div(class = "control-card", h4("Cook's Distance"), selectInput("outlier_cooks_method", "Threshold Method:", choices = c("4 * mean" = "4mean", "4/n" = "4n", "Quantile 0.99" = "quantile"), selected = "4mean")),
                                                        div(class = "control-card", h4("Local Outlier Factor"), sliderInput("outlier_lof_minpts", "minPts (neighbors):", min = 3, max = 10, value = 5, step = 1), sliderInput("outlier_lof_threshold", "LOF Threshold:", min = 1, max = 5, value = 2, step = 0.1)),
                                                        div(class = "control-card", h4("One-Class SVM"), sliderInput("outlier_svm_nu", "nu parameter:", min = 0.01, max = 0.5, value = 0.05, step = 0.01), selectInput("outlier_svm_kernel", "Kernel:", choices = c("linear", "radial", "polynomial", "sigmoid"), selected = "linear")),
                                                        div(class = "control-card", h4("Random Forest"), sliderInput("outlier_rf_iqr", "IQR Multiplier for Residuals:", min = 1, max = 5, value = 2, step = 0.5)),
                                                        div(class = "control-card", h4("Isolation Forest"), sliderInput("outlier_if_threshold", "Isolation Forest Threshold:", min = 0.5, max = 0.9, value = 0.6, step = 0.01)),
                                                        div(class = "control-card", h4("Consensus Table Filter"), sliderInput("outlier_min_flags", "Minimum number of methods flagging:", min = 1, max = 6, value = 2, step = 1, post = " methods"))
                                                 ),
                                                 column(9,
                                                        tabsetPanel(
                                                          tabPanel("Combined View",
                                                                   br(),
                                                                   shinycssloaders::withSpinner(DTOutput("outlier_consensus_table"), type = 4, color = "#2C3E50"),
                                                                   br(),
                                                                   div(class = "well", style = "padding: 15px; margin-top: 10px;",
                                                                       fluidRow(
                                                                         column(4, h5("Display Controls", style = "margin-top: 0;"), sliderInput("outlier_display_count", "Number of outliers to display:", min = 5, max = 100, value = 20, step = 5)),
                                                                         column(8,
                                                                                h5("Select Methods to Include", style = "margin-top: 0;"),
                                                                                fluidRow(column(4, checkboxInput("show_mahalanobis", "Mahalanobis Distance", value = TRUE)), column(4, checkboxInput("show_cooks", "Cook's Distance", value = TRUE)), column(4, checkboxInput("show_lof", "Local Outlier Factor", value = TRUE))),
                                                                                fluidRow(column(4, checkboxInput("show_svm", "One-Class SVM", value = TRUE)), column(4, checkboxInput("show_rf", "Random Forest Residuals", value = TRUE)), column(4, checkboxInput("show_iforest", "Isolation Forest", value = TRUE)))
                                                                         )
                                                                       )
                                                                   )
                                                          ),
                                                          tabPanel("Mahalanobis Distance", br(), shinycssloaders::withSpinner(plotlyOutput("outlier_mahalanobis_plot", height = "500px"), type = 4, color = "#2C3E50"), br(), verbatimTextOutput("outlier_mahalanobis_stats")),
                                                          tabPanel("Cook's Distance", br(), shinycssloaders::withSpinner(plotlyOutput("outlier_cooks_plot", height = "500px"), type = 4, color = "#2C3E50"), br(), verbatimTextOutput("outlier_cooks_stats")),
                                                          tabPanel("Local Outlier Factor", br(), shinycssloaders::withSpinner(plotlyOutput("outlier_lof_plot", height = "500px"), type = 4, color = "#2C3E50"), br(), verbatimTextOutput("outlier_lof_stats")),
                                                          tabPanel("One-Class SVM", br(), shinycssloaders::withSpinner(plotlyOutput("outlier_svm_plot", height = "500px"), type = 4, color = "#2C3E50"), br(), verbatimTextOutput("outlier_svm_stats")),
                                                          tabPanel("Random Forest Residuals", br(), shinycssloaders::withSpinner(plotlyOutput("outlier_rf_plot", height = "500px"), type = 4, color = "#2C3E50"), br(), verbatimTextOutput("outlier_rf_stats")),
                                                          tabPanel("Isolation Forest", br(), shinycssloaders::withSpinner(plotlyOutput("outlier_if_plot", height = "500px"), type = 4, color = "#2C3E50"), br(), verbatimTextOutput("outlier_if_stats"))
                                                        )
                                                 )
                                               )
                                      )
                          )  # Close tabsetPanel (processing_tabs)
                   )  # Close column(9, id = "master_main_panel")
                 )  # Close fluidRow
             )  # Close div(id = "master_sidebar_wrapper")
           )  # Close fluidPage
  ),  # <-- THIS CLOSES THE DATA PROCESSING STRATEGY TAB - IMPORTANT!
  
  
  # ==========================================================================
  # ==========================================================================
  # TAB 3: DATA PROCESSING CONTROLS (Pipeline Builder) - TOP LEVEL
  # ==========================================================================
  # ==========================================================================
  
  tabPanel("Data Processing Controls",
           fluidPage(
             h2("Data Processing Controls"),
             hr(),
             
             fluidRow(
               column(12,
                      div(class = "alert alert-info",
                          icon("info-circle"),
                          "Choose between two approaches: Build a step-by-step reproducible recipe, or use quick batch transformations."
                      )
               )
             ),
             
             tabsetPanel(
               id = "processing_approach",
               type = "tabs",
               
               # ================================================================
               # SUB-TAB 1: RECIPE BUILDER (Step-by-step)
               # ================================================================
               tabPanel("Recipe Builder",
                        br(),
                        fluidRow(
                          column(6,
                                 div(class = "control-card",
                                     h4("Step-by-Step Recipe Builder", style = "color: #2C3E50;"),
                                     p("Build a reproducible sequence of processing steps. Each step is applied in order.", 
                                       style = "font-size: 12px; color: #666;"),
                                     hr(),
                                     
                                     # Dynamic step panels
                                     uiOutput("processing_steps_ui"),
                                     
                                     # Add step button
                                     actionButton("add_processing_step", "＋ Add Step", 
                                                  class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 15px;"),
                                     
                                     hr(),
                                     
                                     h4("Recipe Controls"),
                                     fluidRow(
                                       column(8,
                                              textInput("pipeline_name", "Recipe Name", 
                                                        placeholder = "e.g., Cleaned_Data_v1")
                                       ),
                                       column(4,
                                              br(),
                                              actionButton("save_pipeline_btn", "Save Recipe", 
                                                           class = "btn-info btn-sm", style = "width: 100%;")
                                       )
                                     ),
                                     
                                     fluidRow(
                                       column(6,
                                              actionButton("process_data_btn", "▶ Apply Recipe", 
                                                           class = "btn-success", style = "width: 100%; margin-bottom: 10px;")
                                       ),
                                       column(6,
                                              actionButton("reset_pipeline_btn", "Reset Recipe", 
                                                           class = "btn-danger", style = "width: 100%; margin-bottom: 10px;")
                                       )
                                     ),
                                     
                                     hr(),
                                     
                                     h4("Saved Recipes", style = "color: #2C3E50; margin-top: 10px;"),
                                     uiOutput("saved_pipelines_list")
                                 )
                          ),
                          
                          column(6,
                                 div(class = "control-card",
                                     h4("Recipe Preview & Summary", style = "color: #2C3E50;"),
                                     hr(),
                                     h5("Current Recipe Steps:"),
                                     uiOutput("pipeline_summary_ui"),
                                     hr(),
                                     h5("Apply Recipe to View Results:"),
                                     p("Click 'Apply Recipe' above to process your data with these steps.",
                                       style = "font-size: 12px; color: #666; font-style: italic;")
                                 )
                          )
                        ),
                        
                        hr(),
                        
                        fluidRow(
                          column(12,
                                 div(class = "control-card",
                                     h4("Processing Log & Data Summary"),
                                     hr(),
                                     verbatimTextOutput("proc_log"),
                                     hr(),
                                     verbatimTextOutput("proc_data_summary"),
                                     hr(),
                                     h5("Data Preview (First 20 rows):"),
                                     DTOutput("proc_data_preview"),
                                     hr(),
                                     downloadButton("proc_download_data", "Download Processed Data (CSV)",
                                                    class = "btn-success", style = "width: 100%;")
                                 )
                          )
                        )
               ),
               
               # ================================================================
               # SUB-TAB 2: BATCH PROCESSOR (Quick transformations)
               # ================================================================
               tabPanel("Batch Processor",
                        br(),
                        fluidRow(
                          column(6,
                                 div(class = "control-card",
                                     h4("Missing Value Imputation", style = "color: #2C3E50;"),
                                     hr(),
                                     selectInput("proc_impute_column", "Select numeric column:", 
                                                 choices = numeric_cols, selected = NULL),
                                     selectInput("proc_impute_method", "Imputation method:", 
                                                 choices = c("Median" = "median", "Mean" = "mean", "Constant" = "constant"), 
                                                 selected = "median"),
                                     conditionalPanel(
                                       condition = "input.proc_impute_method == 'constant'",
                                       numericInput("proc_impute_value", "Constant value:", value = 0)
                                     ),
                                     actionButton("proc_impute_btn", "Apply Numeric Imputation",
                                                  class = "btn-success btn-sm", style = "width: 100%; margin-bottom: 15px;"),
                                     
                                     selectInput("proc_impute_cat_column", "Select categorical column:", 
                                                 choices = categorical_cols[categorical_cols != "CODE"], selected = NULL),
                                     selectInput("proc_impute_cat_method", "Imputation method:", 
                                                 choices = c("Mode" = "mode", "New Category 'Missing'" = "new_category", "Constant" = "constant"), 
                                                 selected = "mode"),
                                     conditionalPanel(
                                       condition = "input.proc_impute_cat_method == 'constant'",
                                       textInput("proc_impute_cat_value", "Constant value:", value = "Missing")
                                     ),
                                     actionButton("proc_impute_cat_btn", "Apply Categorical Imputation",
                                                  class = "btn-info btn-sm", style = "width: 100%; margin-bottom: 15px;")
                                 )
                          ),
                          
                          column(6,
                                 div(class = "control-card",
                                     h4("Transformations", style = "color: #2C3E50;"),
                                     hr(),
                                     selectInput("proc_transform_column", "Select numeric column:", 
                                                 choices = numeric_cols, selected = NULL),
                                     selectInput("proc_transform_method", "Transformation method:", 
                                                 choices = c("Log (ln)" = "log", "Square Root" = "sqrt", "Square" = "square",
                                                             "Box-Cox" = "boxcox", "Yeo-Johnson" = "yeojohnson"),
                                                 selected = "log"),
                                     actionButton("proc_transform_btn", "Apply Transformation",
                                                  class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 15px;")
                                 )
                          )
                        ),
                        
                        fluidRow(
                          column(6,
                                 div(class = "control-card",
                                     h4("Row/Column Operations", style = "color: #2C3E50;"),
                                     hr(),
                                     sliderInput("proc_max_missing_per_row", "Max missing values per row:",
                                                 min = 0, max = 15, value = 5, step = 1),
                                     actionButton("proc_remove_rows_btn", "Remove Rows Exceeding Threshold",
                                                  class = "btn-warning btn-sm", style = "width: 100%; margin-bottom: 15px;"),
                                     
                                     selectInput("proc_remove_column", "Select column to remove:",
                                                 choices = names(df), selected = NULL),
                                     actionButton("proc_remove_col_btn", "Remove Selected Column",
                                                  class = "btn-danger btn-sm", style = "width: 100%; margin-bottom: 15px;")
                                 )
                          ),
                          
                          column(6,
                                 div(class = "control-card",
                                     h4("Batch Actions", style = "color: #2C3E50;"),
                                     hr(),
                                     actionButton("proc_apply_all", "Apply All Quick Actions",
                                                  class = "btn-primary btn-lg", style = "width: 100%; margin-bottom: 10px;"),
                                     actionButton("proc_reset_all", "Reset to Original Data",
                                                  class = "btn-danger btn-lg", style = "width: 100%;"),
                                     hr(),
                                     h4("Save Processed Dataset", style = "color: #2C3E50;"),
                                     textInput("dataset_name", "Dataset Name", 
                                               placeholder = "e.g., Log_Transformed_Data"),
                                     actionButton("save_dataset_btn", "Save Current Dataset", 
                                                  class = "btn-success", style = "width: 100%; margin-bottom: 15px;"),
                                     hr(),
                                     h4("Saved Datasets", style = "color: #2C3E50;"),
                                     uiOutput("saved_datasets_list")
                                 )
                          )
                        ),
                        
                        hr(),
                        
                        fluidRow(
                          column(12,
                                 div(class = "control-card",
                                     h4("Processing Log & Data Summary"),
                                     hr(),
                                     verbatimTextOutput("proc_log_batch"),
                                     hr(),
                                     verbatimTextOutput("proc_data_summary_batch"),
                                     hr(),
                                     h5("Data Preview (First 20 rows):"),
                                     DTOutput("proc_data_preview_batch")
                                 )
                          )
                        )
               )
             )
           )
  ),
  
  
  # ==========================================================================
  # ==========================================================================
  # TAB 4: GLMNET MODELING - TOP LEVEL
  # ==========================================================================
  # ==========================================================================
  
  tabPanel("GLMNET Modeling",
           fluidPage(
             h2("GLMNET Model for Death Rate Prediction"),
             hr(),
             
             fluidRow(
               column(3,
                      div(class = "well",
                          h4("Data Source"),
                          hr(),
                          radioButtons("model_data_source", "Select Data Source:", 
                                       choices = c("Original Data" = "original", "Saved Dataset" = "saved"),
                                       selected = "original"),
                          
                          conditionalPanel(condition = "input.model_data_source == 'saved'",
                                           div(style = "color: #17a2b8; background-color: #d1ecf1; padding: 8px; border-radius: 4px; margin-bottom: 15px;",
                                               icon("database"), HTML("<strong>Using saved dataset</strong><br>Select a dataset saved from the Data Processing tab.")
                                           ),
                                           pickerInput("model_dataset_select", "Select Saved Dataset:", choices = NULL, options = list(`live-search` = TRUE)),
                                           hr()
                          ),
                          
                          conditionalPanel(condition = "input.model_data_source == 'processed'",
                                           div(style = "color: #28a745; background-color: #d4edda; padding: 8px; border-radius: 4px; margin-bottom: 15px;",
                                               icon("check-circle"), HTML("<strong>Using current processed data</strong><br>From the Data Processing tab.")
                                           ),
                                           actionButton("refresh_model_data", "Refresh Processed Data", class = "btn-info btn-sm", style = "width: 100%; margin-bottom: 15px;")
                          ),
                          
                          hr(),
                          h4("Model Settings"),
                          hr(),
                          selectInput("glmnet_alpha", "Alpha (Elastic Net Mix):", 
                                      choices = c("Ridge (0)" = 0, "Elastic Net (0.5)" = 0.5, "Lasso (1)" = 1), 
                                      selected = 0.5),
                          numericInput("glmnet_lambda", "Lambda:", value = NULL, min = 0.001, max = 1, step = 0.001),
                          hr(),
                          numericInput("cv_folds", "CV Folds:", value = 5, min = 3, max = 10),
                          numericInput("cv_seed", "Random Seed:", value = 123),
                          hr(),
                          actionButton("train_model", "Train GLMNET Model", class = "btn-primary", style = "width: 100%;")
                      )
               ),
               
               column(9,
                      tabsetPanel(
                        tabPanel("Model Summary", 
                                 verbatimTextOutput("model_performance"), 
                                 hr(), 
                                 verbatimTextOutput("model_hyperparameters"), 
                                 hr(), 
                                 DTOutput("model_coefficients")
                        ),
                        tabPanel("Coefficient Plot", 
                                 plotlyOutput("coef_plot", height = "500px")
                        ),
                        tabPanel("Predictions", 
                                 plotlyOutput("predictions_plot", height = "500px"), 
                                 hr(),
                                 # Collapsible Predictions Summary
                                 tags$div(
                                   class = "panel-group",
                                   tags$div(
                                     class = "panel panel-default",
                                     tags$div(
                                       class = "panel-heading",
                                       tags$h4(
                                         class = "panel-title",
                                         tags$a(`data-toggle` = "collapse", href = "#collapsePredictionsSummary", 
                                                "Predictions Summary",
                                                style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                       )
                                     ),
                                     tags$div(
                                       id = "collapsePredictionsSummary",
                                       class = "panel-collapse collapse",
                                       tags$div(
                                         class = "panel-body",
                                         verbatimTextOutput("predictions_summary")
                                       )
                                     )
                                   )
                                 )
                        ),
                        tabPanel("Residual Analysis", 
                                 # IQR Multiplier control above the plot
                                 div(class = "well", style = "padding: 10px; margin-bottom: 15px;",
                                     h5("Residual Outlier Detection Settings", style = "margin-top: 0;"),
                                     sliderInput("residual_iqr", "IQR Multiplier for Outlier Detection:", 
                                                 min = 1, max = 5, value = 1.5, step = 0.1)
                                 ),
                                 plotlyOutput("residual_plot", height = "500px"), 
                                 hr(),
                                 # Collapsible Residual Statistics
                                 tags$div(
                                   class = "panel-group",
                                   tags$div(
                                     class = "panel panel-default",
                                     tags$div(
                                       class = "panel-heading",
                                       tags$h4(
                                         class = "panel-title",
                                         tags$a(`data-toggle` = "collapse", href = "#collapseResidualStats", 
                                                "Residual Statistics",
                                                style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                       )
                                     ),
                                     tags$div(
                                       id = "collapseResidualStats",
                                       class = "panel-collapse collapse",
                                       tags$div(
                                         class = "panel-body",
                                         verbatimTextOutput("residual_stats")
                                       )
                                     )
                                   )
                                 ),
                                 hr(),
                                 # Collapsible Residual Outliers Table
                                 tags$div(
                                   class = "panel-group",
                                   tags$div(
                                     class = "panel panel-default",
                                     tags$div(
                                       class = "panel-heading",
                                       tags$h4(
                                         class = "panel-title",
                                         tags$a(`data-toggle` = "collapse", href = "#collapseResidualOutliers", 
                                                "Residual Outliers (Test Set)",
                                                style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                       )
                                     ),
                                     tags$div(
                                       id = "collapseResidualOutliers",
                                       class = "panel-collapse collapse",
                                       tags$div(
                                         class = "panel-body",
                                         DTOutput("residual_outliers")
                                       )
                                     )
                                   )
                                 )
                        ),
                        tabPanel("Cross-Validation", 
                                 plotlyOutput("cv_plot", height = "500px"), 
                                 hr(),
                                 # Collapsible CV Summary
                                 tags$div(
                                   class = "panel-group",
                                   tags$div(
                                     class = "panel panel-default",
                                     tags$div(
                                       class = "panel-heading",
                                       tags$h4(
                                         class = "panel-title",
                                         tags$a(`data-toggle` = "collapse", href = "#collapseCVSummary", 
                                                "Cross-Validation Summary",
                                                style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                       )
                                     ),
                                     tags$div(
                                       id = "collapseCVSummary",
                                       class = "panel-collapse collapse",
                                       tags$div(
                                         class = "panel-body",
                                         verbatimTextOutput("cv_summary")
                                       )
                                     )
                                   )
                                 )
                        )
                      )
               )
             )
           )
  ),  # Close tabPanel "GLMNET Modeling"
  
  
  # ==========================================================================
  # ==========================================================================
  # TAB 5: REPORT SUMMARY - TOP LEVEL
  # ==========================================================================
  # ==========================================================================
  
  tabPanel("Report Summary",
           fluidPage(
             h2("Analysis Report Summary"),
             hr(),
             
             fluidRow(
               column(6, div(class = "well", h4("Data Description"), hr(), textOutput("report_data_desc"))),
               column(6, div(class = "well", h4("Missingness Strategy"), hr(), textOutput("report_missing_strategy")))
             ),
             fluidRow(
               column(6, div(class = "well", h4("Outlier Strategy"), hr(), textOutput("report_outlier_strategy"))),
               column(6, div(class = "well", h4("Pre-processing Strategy"), hr(), textOutput("report_preprocess_strategy")))
             ),
             fluidRow(
               column(6, div(class = "well", h4("GLMNET Explanation"), hr(), textOutput("report_glmnet_explanation"))),
               column(6, div(class = "well", h4("Model Performance"), hr(), textOutput("report_model_performance")))
             ),
             fluidRow(
               column(12, div(class = "well", h4("Residual Analysis Summary"), hr(), textOutput("report_residual_summary")))
             )
           )
  )  # Close tabPanel "Report Summary"
  
)  # Close navbarPage