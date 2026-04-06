# ui.R - DATA423 Assignment 2 - Eduard Bradley

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
    
    /* Sidebar hiding rules for each tab */
    #correlation_wrapper.sidebar-hidden .col-sm-3,
    #datatable_wrapper.sidebar-hidden .col-sm-3,
    #heatmap_wrapper.sidebar-hidden .col-sm-3,
    #distribution_wrapper.sidebar-hidden .col-sm-3,
    #boxplot_wrapper.sidebar-hidden .col-sm-3,
    #scatter_wrapper.sidebar-hidden .col-sm-3,
    #mosaic_wrapper.sidebar-hidden .col-sm-3,
    #ggpairs_wrapper.sidebar-hidden .col-sm-3,
    #tabplot_wrapper.sidebar-hidden .col-sm-3,
    #rising_wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    
    #correlation_wrapper.sidebar-hidden .col-sm-9,
    #datatable_wrapper.sidebar-hidden .col-sm-9,
    #heatmap_wrapper.sidebar-hidden .col-sm-9,
    #distribution_wrapper.sidebar-hidden .col-sm-9,
    #boxplot_wrapper.sidebar-hidden .col-sm-9,
    #scatter_wrapper.sidebar-hidden .col-sm-9,
    #mosaic_wrapper.sidebar-hidden .col-sm-9,
    #ggpairs_wrapper.sidebar-hidden .col-sm-9,
    #tabplot_wrapper.sidebar-hidden .col-sm-9,
    #rising_wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
    }
  "))
  ),
  
  # ==========================================================================
  # TAB 1: SUMMARY & EDA
  # ==========================================================================
  tabPanel("Summary & EDA",
           fluidPage(
             h2("Exploratory Data Analysis"),
             hr(),
             
             tabsetPanel(
               
               # ====================================================================
               # SUMMARY SUB-TAB
               # ====================================================================
               tabPanel("Summary",
                        fluidPage(
                          h2("Dataset Summary Statistics"),
                          hr(),
                          
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
                          
                          tags$div(class = "panel-group",
                                   tags$div(class = "panel panel-default",
                                            tags$div(class = "panel-heading",
                                                     tags$h4(class = "panel-title",
                                                             tags$a(`data-toggle` = "collapse", href = "#collapseNumeric", 
                                                                    "Numeric Variables Summary (11 variables)",
                                                                    style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                     )
                                            ),
                                            tags$div(id = "collapseNumeric", class = "panel-collapse collapse",
                                                     tags$div(class = "panel-body", DTOutput("summary_numeric_table"))
                                            )
                                   )
                          ),
                          
                          br(),
                          
                          tags$div(class = "panel-group",
                                   tags$div(class = "panel panel-default",
                                            tags$div(class = "panel-heading",
                                                     tags$h4(class = "panel-title",
                                                             tags$a(`data-toggle` = "collapse", href = "#collapseCategorical", 
                                                                    "Categorical Variables Summary (4 variables)",
                                                                    style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                     )
                                            ),
                                            tags$div(id = "collapseCategorical", class = "panel-collapse collapse",
                                                     tags$div(class = "panel-body", DTOutput("summary_categorical_table"))
                                            )
                                   )
                          ),
                          
                          br(),
                          
                          tags$div(class = "panel-group",
                                   tags$div(class = "panel panel-default",
                                            tags$div(class = "panel-heading",
                                                     tags$h4(class = "panel-title",
                                                             tags$a(`data-toggle` = "collapse", href = "#collapseMissing", 
                                                                    "Missing Values Summary by Variable",
                                                                    style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                     )
                                            ),
                                            tags$div(id = "collapseMissing", class = "panel-collapse collapse",
                                                     tags$div(class = "panel-body", DTOutput("summary_missing_table"))
                                            )
                                   )
                          ),
                          
                          br(),
                          
                          tags$div(class = "panel-group",
                                   tags$div(class = "panel panel-default",
                                            tags$div(class = "panel-heading",
                                                     tags$h4(class = "panel-title",
                                                             tags$a(`data-toggle` = "collapse", href = "#collapseDFSummary", 
                                                                    "Complete Data Summary (summarytools)",
                                                                    style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                     )
                                            ),
                                            tags$div(id = "collapseDFSummary", class = "panel-collapse collapse",
                                                     tags$div(class = "panel-body",
                                                              div(class = "summarytools-container", htmlOutput("summary_dfsummary"))
                                                     )
                                            )
                                   )
                          ),
                          
                          br()
                        )
               ),
               
               # ====================================================================
               # BOXPLOT ANALYSIS SUB-TAB
               # ====================================================================
               tabPanel("Boxplot Analysis",
                        div(id = "boxplot_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "boxplot_sidebar", class = "sidebar-panel", width = 3,
                                           h4("Data Transformation:"),
                                           sliderInput("iqr_boxplot", "Outlier IQR Multiplier:", 
                                                       min = 0.5, max = 15, value = 1.5, step = 0.1),
                                           checkboxInput("boxplot_center", "Center data:", TRUE),
                                           checkboxInput("boxplot_scale", "Scale data:", TRUE),
                                           
                                           hr(),
                                           
                                           h4("Variable Selection:"),
                                           pickerInput("boxplot_numeric_vars", "Select Numeric Variables:", 
                                                       choices = numeric_cols, selected = numeric_cols,
                                                       multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           pickerInput("boxplot_cat_vars_group", "Group By (Optional):", 
                                                       choices = c("None", "GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE"),
                                                       selected = "None", multiple = FALSE),
                                           
                                           hr(),
                                           
                                           h4("Filter Data by Categorical Variables:"),
                                           pickerInput("boxplot_filter_vars", "Select Categorical Variables to Filter:", 
                                                       choices = c("GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE"),
                                                       selected = NULL, multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           uiOutput("boxplot_cat_filters"),
                                           
                                           hr(),
                                           
                                           h4("Missing Value Handling:"),
                                           checkboxInput("include_null_boxplot", "Include NULL (R NA) values in visualisation", value = FALSE),
                                           p("Note: -99, --, and 'NA' strings are always treated as missing.",
                                             style = "font-size: 11px; color: #666; margin-top: 8px;"),
                                           
                                           br(),
                                           actionButton("reset_boxplot", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel", width = 9,
                                        div(class = "toggle-container",
                                            actionButton("toggle_boxplot_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        shinycssloaders::withSpinner(plotlyOutput("boxplot_plot", height = "600px"),
                                                                     type = 4, color = "#2C3E50", size = 0.7),
                                        br(), hr(),
                                        
                                        tags$div(class = "panel-group",
                                                 tags$div(class = "panel panel-default",
                                                          tags$div(class = "panel-heading",
                                                                   tags$h4(class = "panel-title",
                                                                           tags$a(`data-toggle` = "collapse", href = "#collapseBoxplotStats", 
                                                                                  "Boxplot Statistics",
                                                                                  style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                                   )
                                                          ),
                                                          tags$div(id = "collapseBoxplotStats", class = "panel-collapse collapse",
                                                                   tags$div(class = "panel-body", verbatimTextOutput("boxplot_stats"))
                                                          )
                                                 )
                                        )
                              )
                            )
                        )
               ),
               
               # ====================================================================
               # CORRELATION ANALYSIS SUB-TAB
               # ====================================================================
               tabPanel("Correlation Analysis",
                        div(id = "correlation_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "correlation_sidebar", class = "sidebar-panel", width = 3,
                                           h4("Correlation Settings:"),
                                           selectInput("corr_method", "Correlation Method:",
                                                       choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"),
                                                       selected = "pearson"),
                                           selectInput("corr_order", "Variable Ordering:",
                                                       choices = c("Original" = "original", "AOE", "FPC", 
                                                                   "Hierarchical Clustering" = "hclust", "Alphabetical" = "alphabet"),
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
                                             selectInput("corr_digits", "Decimal Places:", choices = c("1" = 1, "2" = 2, "3" = 3), selected = 2)
                                           ),
                                           
                                           hr(),
                                           
                                           h4("Variable Selection:"),
                                           pickerInput("corr_numeric_vars", "Select Numeric Variables:", 
                                                       choices = numeric_cols, selected = numeric_cols,
                                                       multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           pickerInput("corr_categorical_vars", "Select Categorical Variables to Filter:", 
                                                       choices = c("GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE"),
                                                       selected = NULL, multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           uiOutput("corr_categorical_filters"),
                                           
                                           hr(),
                                           
                                           h4("Missing Value Handling:"),
                                           checkboxGroupInput("corr_mv_types", label = "Treat as missing:",
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")),
                                           checkboxInput("include_null_correlation", "Include NULL values in categorical filtering", value = FALSE),
                                           uiOutput("corr_null_explanation"),
                                           
                                           br(),
                                           actionButton("reset_correlation", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel", width = 9,
                                        div(class = "toggle-container",
                                            actionButton("toggle_correlation_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        shinycssloaders::withSpinner(plotlyOutput("correlation_plot", height = "600px"),
                                                                     type = 4, color = "#2C3E50", size = 0.7),
                                        br(), hr(),
                                        
                                        tags$div(class = "panel-group",
                                                 tags$div(class = "panel panel-default",
                                                          tags$div(class = "panel-heading",
                                                                   tags$h4(class = "panel-title",
                                                                           tags$a(`data-toggle` = "collapse", href = "#collapseCorrelationStats", 
                                                                                  "Correlation Statistics",
                                                                                  style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                                   )
                                                          ),
                                                          tags$div(id = "collapseCorrelationStats", class = "panel-collapse collapse",
                                                                   tags$div(class = "panel-body", verbatimTextOutput("correlation_stats"))
                                                          )
                                                 )
                                        ),
                                        br(),
                                        textOutput("corr_obs_count")
                              )
                            )
                        )
               ),
               
               # ====================================================================
               # RISING VALUES SUB-TAB
               # ====================================================================
               tabPanel("Rising Values",
                        div(id = "rising_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "rising_sidebar", class = "sidebar-panel", width = 3,
                                           h4("Plot Settings:"),
                                           materialSwitch("rv_standardise", "Standardise Variables (Z-score)", 
                                                          value = TRUE, status = "primary"),
                                           
                                           hr(),
                                           
                                           h4("Variable Selection:"),
                                           pickerInput("rv_numeric_vars", "Select Numeric Variables:", 
                                                       choices = numeric_cols, selected = numeric_cols,
                                                       multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                                                       `selected-text-format` = "count > 3")),
                                           
                                           pickerInput("rv_categorical_vars", "Select Categorical Variables (for filtering):", 
                                                       choices = categorical_cols[categorical_cols != "CODE"],
                                                       selected = NULL, multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           hr(),
                                           
                                           uiOutput("rv_categorical_filters"),
                                           
                                           br(),
                                           
                                           h4("Missing Value Handling:"),
                                           checkboxGroupInput("rv_mv_types", label = "Treat as missing:",
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")),
                                           materialSwitch("rv_include_null", "Include NULL values in filtering", 
                                                          value = TRUE, status = "primary"),
                                           
                                           br(),
                                           actionButton("reset_rising", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel", width = 9,
                                        div(class = "toggle-container",
                                            actionButton("toggle_rising_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        shinycssloaders::withSpinner(plotlyOutput("rising_plot", height = "600px"),
                                                                     type = 4, color = "#2C3E50", size = 0.7),
                                        br(), hr(),
                                        verbatimTextOutput("rising_obs_count")
                              )
                            )
                        )
               ),
               
               # ====================================================================
               # MISSING VALUES HEATMAP SUB-TAB
               # ====================================================================
               tabPanel("Missing Values Heatmap",
                        div(id = "heatmap_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "heatmap_sidebar", class = "sidebar-panel", width = 3,
                                           h4("Missing Value Types to Display:"),
                                           checkboxGroupInput("mv_types", label = NULL,
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")),
                                           
                                           hr(),
                                           
                                           h4("Missingness Thresholds:"),
                                           sliderInput("col_missing_threshold", "Max Column Missing %:", 
                                                       min = 0, max = 100, value = 100, step = 5, post = "%"),
                                           sliderInput("row_missing_threshold", "Max Row Missing Count:", 
                                                       min = 0, max = 15, value = 15, step = 1),
                                           
                                           hr(),
                                           
                                           h4("Variable Selection:"),
                                           pickerInput("heatmap_cat_vars", "Select Categorical Variables:", 
                                                       choices = c("GOVERN_TYPE", "HEALTHCARE_BASIS", "CODE", "OBS_TYPE"),
                                                       selected = c("GOVERN_TYPE", "HEALTHCARE_BASIS", "CODE", "OBS_TYPE"),
                                                       multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           pickerInput("heatmap_numeric_vars", "Select Numeric Variables:", 
                                                       choices = numeric_cols, selected = numeric_cols,
                                                       multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           hr(),
                                           
                                           radioButtons("heatmap_order", "Order Variables:",
                                                        choices = c("Original" = "original", "Most Missing First" = "desc"),
                                                        selected = "original"),
                                           
                                           br(),
                                           actionButton("reset_heatmap", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel", width = 9,
                                        div(class = "toggle-container",
                                            actionButton("toggle_heatmap_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm toggle-btn",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        shinycssloaders::withSpinner(plotlyOutput("heatmap_plot", height = "700px"),
                                                                     type = 4, color = "#2C3E50", size = 0.7),
                                        br(), hr(),
                                        
                                        tags$div(class = "panel-group",
                                                 tags$div(class = "panel panel-default",
                                                          tags$div(class = "panel-heading",
                                                                   tags$h4(class = "panel-title",
                                                                           tags$a(`data-toggle` = "collapse", href = "#collapseHeatmapSummary", 
                                                                                  "Missingness Summary",
                                                                                  style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                                   )
                                                          ),
                                                          tags$div(id = "collapseHeatmapSummary", class = "panel-collapse collapse",
                                                                   tags$div(class = "panel-body", verbatimTextOutput("heatmap_summary"))
                                                          )
                                                 )
                                        )
                              )
                            )
                        )
               ),
               
               # ====================================================================
               # DISTRIBUTION PLOTS SUB-TAB
               # ====================================================================
               tabPanel("Distribution Plots",
                        div(id = "distribution_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "distribution_sidebar", class = "sidebar-panel", width = 3,
                                           h4("Plot Settings:"),
                                           sliderInput("dist_bins", "Number of Bins:", min = 5, max = 50, value = 30),
                                           
                                           hr(),
                                           
                                           h4("Variable Selection:"),
                                           selectInput("dist_var", "Select Variable:",
                                                       choices = c(numeric_cols, "GOVERN_TYPE", "HEALTHCARE_BASIS"),
                                                       selected = "VAX_RATE"),
                                           
                                           hr(),
                                           
                                           h4("Missing Value Handling:"),
                                           checkboxGroupInput("dist_mv_types", label = "Treat as missing (exclude from plot):",
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")),
                                           p("Note: Values treated as missing are excluded from the histogram and rug plot.",
                                             style = "font-size: 11px; color: #666; margin-top: 8px;"),
                                           
                                           br(),
                                           actionButton("reset_distribution", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel", width = 9,
                                        div(class = "toggle-container",
                                            actionButton("toggle_distribution_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        plotlyOutput("distribution_plot", height = "500px"),
                                        br(), hr(),
                                        
                                        tags$div(class = "panel-group",
                                                 tags$div(class = "panel panel-default",
                                                          tags$div(class = "panel-heading",
                                                                   tags$h4(class = "panel-title",
                                                                           tags$a(`data-toggle` = "collapse", href = "#collapseDistStats", 
                                                                                  "Distribution Statistics",
                                                                                  style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                                   )
                                                          ),
                                                          tags$div(id = "collapseDistStats", class = "panel-collapse collapse",
                                                                   tags$div(class = "panel-body", verbatimTextOutput("distribution_stats"))
                                                          )
                                                 )
                                        )
                              )
                            )
                        )
               ),
               
               # ====================================================================
               # SCATTER PLOT SUB-TAB
               # ====================================================================
               tabPanel("Scatter Plot",
                        div(id = "scatter_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "scatter_sidebar", class = "sidebar-panel", width = 3,
                                           h4("Variable Selection:"),
                                           selectInput("scatter_x", "X-Axis Variable:", choices = numeric_cols, selected = "GDP"),
                                           selectInput("scatter_y", "Y-Axis Variable:", choices = numeric_cols, selected = "DEATH_RATE"),
                                           selectInput("scatter_color", "Color By:", 
                                                       choices = c("None", "GOVERN_TYPE", "HEALTHCARE_BASIS"), selected = "None"),
                                           
                                           hr(),
                                           
                                           h4("Missing Value Handling:"),
                                           checkboxGroupInput("scatter_mv_types", label = "Treat as missing:",
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")),
                                           
                                           br(),
                                           actionButton("reset_scatter", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel", width = 9,
                                        div(class = "toggle-container",
                                            actionButton("toggle_scatter_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        shinycssloaders::withSpinner(plotlyOutput("scatter_plot", height = "500px"),
                                                                     type = 4, color = "#2C3E50", size = 0.7),
                                        br(), hr(),
                                        
                                        tags$div(class = "panel-group",
                                                 tags$div(class = "panel panel-default",
                                                          tags$div(class = "panel-heading",
                                                                   tags$h4(class = "panel-title",
                                                                           tags$a(`data-toggle` = "collapse", href = "#collapseScatterStats", 
                                                                                  "Scatter Plot Summary",
                                                                                  style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                                   )
                                                          ),
                                                          tags$div(id = "collapseScatterStats", class = "panel-collapse collapse",
                                                                   tags$div(class = "panel-body", verbatimTextOutput("scatter_correlation"))
                                                          )
                                                 )
                                        )
                              )
                            )
                        )
               ),
               
               # ====================================================================
               # GGPAIRS PLOT SUB-TAB
               # ====================================================================
               tabPanel("GGpairs Plot",
                        div(id = "ggpairs_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "ggpairs_sidebar", class = "sidebar-panel", width = 3,
                                           h4("Plot Settings:"),
                                           selectInput("ggpairs_color", "Color By:", 
                                                       choices = c("None", "GOVERN_TYPE", "HEALTHCARE_BASIS"), selected = "None"),
                                           p("Note: Color adds a categorical variable to the plot", 
                                             style = "font-size: 11px; color: #666; margin-top: 5px;"),
                                           
                                           hr(),
                                           
                                           h4("Variable Selection:"),
                                           div(style = "margin-bottom: 5px; text-align: right;",
                                               actionLink("deselect_numeric", "Deselect All", style = "font-size: 11px; color: #dc3545;")
                                           ),
                                           pickerInput("ggpairs_numeric", "Select Numeric Variables (min 2, max 11 recommended):", 
                                                       choices = numeric_cols, selected = numeric_cols[1:4],
                                                       multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           uiOutput("ggpairs_total_count"),
                                           
                                           div(class = "alert alert-info", style = "padding: 8px; margin-top: 5px; font-size: 11px;",
                                               icon("info-circle"),
                                               HTML("<strong>Plot Features:</strong><br>
                    • Diagonal: Smoothed density plots<br>
                    • Lower triangle: Scatter plots with hover showing CODE<br>
                    • Upper triangle: Correlation values")
                                           ),
                                           
                                           hr(),
                                           
                                           h4("Missing Value Handling:"),
                                           checkboxGroupInput("ggpairs_mv_types", label = "Treat as missing:",
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")),
                                           p("Note: Rows with missing values are removed from the plot.", 
                                             style = "font-size: 11px; color: #666; margin-top: 5px;"),
                                           
                                           br(),
                                           actionButton("reset_ggpairs", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel", width = 9,
                                        div(class = "toggle-container",
                                            actionButton("toggle_ggpairs_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        shinycssloaders::withSpinner(plotlyOutput("ggpairs_plot", height = "800px"),
                                                                     type = 4, color = "#2C3E50", size = 0.7),
                                        br(), hr(),
                                        bsCollapse(id = "ggpairs_summary_collapse", open = NULL,
                                                   bsCollapsePanel(title = "GGpairs Plot Summary", style = "info",
                                                                   verbatimTextOutput("ggpairs_obs_count"))
                                        )
                              )
                            )
                        )
               ),
               
               # ====================================================================
               # TABPLOT SUB-TAB
               # ====================================================================
               tabPanel("Tableplot",
                        div(id = "tabplot_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "tabplot_sidebar", class = "sidebar-panel", width = 3,
                                           h4("Tableplot Settings:"),
                                           sliderInput("tabplot_nbins", "Number of Bins:", min = 10, max = 200, value = 100, step = 10),
                                           radioButtons("tabplot_code_order", "CODE Order:",
                                                        choices = c("Ascending (A→Z)" = "asc", "Descending (Z→A)" = "desc"),
                                                        selected = "asc"),
                                           materialSwitch("tabplot_showNA", "Show NA Values", value = TRUE),
                                           materialSwitch("tabplot_include_null", "Include NULL Values", value = TRUE),
                                           
                                           hr(),
                                           
                                           h4("Variable Selection:"),
                                           p("CODE is automatically included as the first variable", 
                                             style = "font-size: 11px; color: #2C3E50; margin-top: 5px; margin-bottom: 10px; font-style: italic;"),
                                           
                                           pickerInput("tabplot_numeric_vars", "Select Numeric Variables:", 
                                                       choices = numeric_cols, selected = numeric_cols[1:3],
                                                       multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           pickerInput("tabplot_categorical_vars", "Select Categorical Variables:", 
                                                       choices = categorical_cols[categorical_cols != "CODE"],
                                                       selected = categorical_cols[categorical_cols != "CODE"][1:2],
                                                       multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           hr(),
                                           
                                           h4("Missing Value Handling:"),
                                           checkboxGroupInput("tabplot_mv_types", label = "Treat as missing:",
                                                              choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                              selected = c("neg99", "dash", "na_string")),
                                           
                                           uiOutput("tabplot_categorical_filters"),
                                           
                                           br(),
                                           actionButton("reset_tabplot", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel", width = 9,
                                        div(class = "toggle-container",
                                            actionButton("toggle_tabplot_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        shinycssloaders::withSpinner(plotOutput("tabplot_plot", height = "700px"),
                                                                     type = 4, color = "#2C3E50", size = 0.7),
                                        br(), hr(),
                                        verbatimTextOutput("tabplot_obs_count")
                              )
                            )
                        )
               ),
               
               # ====================================================================
               # MOSAIC PLOT SUB-TAB
               # ====================================================================
               tabPanel("Mosaic Plot",
                        div(id = "mosaic_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "mosaic_sidebar", class = "sidebar-panel", width = 3,
                                           h4("Variable Selection:"),
                                           selectInput("mosaic_x", "X-Axis Variable:", 
                                                       choices = c("GOVERN_TYPE", "HEALTHCARE_BASIS"), selected = "GOVERN_TYPE"),
                                           selectInput("mosaic_y", "Y-Axis Variable:", 
                                                       choices = c("GOVERN_TYPE", "HEALTHCARE_BASIS"), selected = "HEALTHCARE_BASIS"),
                                           selectInput("mosaic_z", "Third Variable (Optional):", 
                                                       choices = c("None", "GOVERN_TYPE", "HEALTHCARE_BASIS"), selected = "None"),
                                           hr()
                              ),
                              
                              mainPanel(class = "main-panel", width = 9,
                                        div(class = "toggle-container",
                                            actionButton("toggle_mosaic_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm toggle-btn",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        shinycssloaders::withSpinner(plotOutput("mosaic_plot", height = "500px"),
                                                                     type = 4, color = "#2C3E50", size = 0.7),
                                        br(), hr(),
                                        verbatimTextOutput("mosaic_stats")
                              )
                            )
                        )
               ),
               
               # ====================================================================
               # RAW DATA SUB-TAB
               # ====================================================================
               tabPanel("Raw Data",
                        div(id = "datatable_wrapper", class = "sidebar-wrapper",
                            sidebarLayout(
                              sidebarPanel(id = "sidebar_datatable", class = "sidebar-panel", width = 3,
                                           h4("Data Table Options"),
                                           
                                           sliderInput("dt_row_range", "Select Row Range:",
                                                       min = 1, max = nrow(df), value = c(1, nrow(df)), step = 1),
                                           
                                           hr(),
                                           
                                           h4("Column Selection"),
                                           tabsetPanel(id = "dt_column_tabs", type = "tabs",
                                                       tabPanel("All Columns",
                                                                br(),
                                                                div(style = "margin-bottom: 10px;",
                                                                    actionButton("dt_all_select_all", "Select All Columns", 
                                                                                 class = "btn-primary btn-sm", style = "margin-right: 10px;"),
                                                                    actionButton("dt_all_deselect_all", "Deselect All Columns", 
                                                                                 class = "btn-danger btn-sm")
                                                                ),
                                                                pickerInput("dt_all_columns", "Choose columns to display:",
                                                                            choices = NULL, multiple = TRUE,
                                                                            options = list(`actions-box` = TRUE, `live-search` = TRUE))
                                                       ),
                                                       tabPanel("Numeric Columns",
                                                                br(),
                                                                div(style = "margin-bottom: 10px;",
                                                                    actionButton("dt_num_select_all", "Select All Numeric", 
                                                                                 class = "btn-primary btn-sm", style = "margin-right: 10px;"),
                                                                    actionButton("dt_num_deselect_all", "Deselect All Numeric", 
                                                                                 class = "btn-danger btn-sm")
                                                                ),
                                                                pickerInput("dt_numeric_columns", "Choose numeric columns to display:",
                                                                            choices = numeric_cols, multiple = TRUE,
                                                                            options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                                                p("Note: CODE and OBS_TYPE are always included for context.", 
                                                                  style = "font-size: 11px; color: #666; margin-top: 8px;")
                                                       ),
                                                       tabPanel("Categorical Columns",
                                                                br(),
                                                                div(style = "margin-bottom: 10px;",
                                                                    actionButton("dt_cat_select_all", "Select All Categorical", 
                                                                                 class = "btn-primary btn-sm", style = "margin-right: 10px;"),
                                                                    actionButton("dt_cat_deselect_all", "Deselect All Categorical", 
                                                                                 class = "btn-danger btn-sm")
                                                                ),
                                                                pickerInput("dt_categorical_columns", "Choose categorical columns to display:",
                                                                            choices = categorical_cols, multiple = TRUE,
                                                                            options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                                                p("Note: CODE and OBS_TYPE are always included for context.", 
                                                                  style = "font-size: 11px; color: #666; margin-top: 8px;")
                                                       )
                                           ),
                                           
                                           hr(),
                                           
                                           h4("Display Options"),
                                           selectInput("dt_page_length", "Rows per page:",
                                                       choices = c("10" = 10, "25" = 25, "50" = 50, "100" = 100, "All" = -1),
                                                       selected = 25),
                                           
                                           br(),
                                           p("Use the filters below each column to subset the data."),
                                           p("Export buttons available for copying, CSV, Excel, PDF, and printing."),
                                           
                                           br(),
                                           actionButton("reset_datatable", "Reset to Defaults", 
                                                        class = "btn-warning btn-sm", style = "width: 100%;")
                              ),
                              
                              mainPanel(class = "main-panel", width = 9,
                                        div(class = "toggle-container",
                                            actionButton("toggle_datatable_sidebar", 
                                                         label = HTML('<i class="fa fa-sliders-h"></i> Hide Filters'),
                                                         class = "btn-primary btn-sm",
                                                         style = "background-color: #2C3E50; border-color: #2C3E50;")
                                        ),
                                        div(style = "margin-bottom: 10px; padding: 8px; background-color: #f8f9fa; border-radius: 4px;",
                                            textOutput("dt_row_info")
                                        ),
                                        shinycssloaders::withSpinner(DTOutput("raw_data_table"),
                                                                     type = 4, color = "#2C3E50", size = 0.7)
                              )
                            )
                        )
               )
             )
           )
  ),
  
  # ==========================================================================
  # TAB 2: DATA PROCESSING STRATEGY
  # ==========================================================================
  tabPanel("Data Processing Strategy",
           fluidPage(
             h2("Missing Data & Outlier Processing Strategy"),
             hr(),
             
             fluidRow(
               column(12,
                      div(class = "alert alert-info",
                          icon("info-circle"),
                          "Explore missing data patterns and outlier detection. ",
                          "All changes are applied to a working copy - the original dataset remains untouched."
                      )
               )
             ),
             
             tabsetPanel(id = "processing_tabs",
                         
                         tabPanel("Missingness Analysis",
                                  br(),
                                  div(class = "row",
                                      div(class = "col-sm-3",
                                          wellPanel(
                                            h5("Missing Data Settings"),
                                            
                                            pickerInput("proc_heatmap_cat_vars", "Categorical Variables:", 
                                                        choices = c("GOVERN_TYPE", "HEALTHCARE_BASIS"),
                                                        selected = c("GOVERN_TYPE", "HEALTHCARE_BASIS"), 
                                                        multiple = TRUE),
                                            
                                            pickerInput("proc_heatmap_numeric_vars", "Numeric Variables:", 
                                                        choices = numeric_cols, 
                                                        selected = numeric_cols, 
                                                        multiple = TRUE),
                                            
                                            hr(),
                                            
                                            checkboxGroupInput("proc_mv_types", "Missing Value Types to Display:",
                                                               choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                               selected = c("neg99", "dash", "na_string")),
                                            
                                            hr(),
                                            
                                            sliderInput("proc_col_missing_threshold", "Max Column Missing %:", 
                                                        min = 0, max = 100, value = 100, step = 5, post = "%"),
                                            
                                            sliderInput("proc_row_missing_threshold", "Max Row Missing Count:", 
                                                        min = 0, max = 15, value = 15, step = 1),
                                            
                                            radioButtons("proc_heatmap_order", "Order Variables:",
                                                         choices = c("Original" = "original", "Most Missing First" = "desc"),
                                                         selected = "original")
                                          )
                                      ),
                                      div(class = "col-sm-9",
                                          tabsetPanel(
                                            tabPanel("Missing Values Heatmap",
                                                     br(),
                                                     shinycssloaders::withSpinner(plotlyOutput("proc_missing_heatmap", height = "550px"),
                                                                                  type = 4, color = "#2C3E50")
                                            ),
                                            tabPanel("Missingness Combinations (Upset)",
                                                     br(),
                                                     fluidRow(
                                                       column(3,
                                                              wellPanel(
                                                                h5("Upset Plot Settings"),
                                                                pickerInput("upset_cat_vars", "Categorical Variables:", 
                                                                            choices = categorical_cols[categorical_cols != "CODE"], 
                                                                            selected = categorical_cols[categorical_cols != "CODE"][1:2], 
                                                                            multiple = TRUE,
                                                                            options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                                                pickerInput("upset_numeric_vars", "Numeric Variables:", 
                                                                            choices = numeric_cols, 
                                                                            selected = numeric_cols[1:3], 
                                                                            multiple = TRUE,
                                                                            options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                                                hr(),
                                                                checkboxGroupInput("upset_mv_types", "Missing Value Types to Include:",
                                                                                   choices = c("-99 values" = "neg99", "-- values" = "dash", "'NA' strings" = "na_string"),
                                                                                   selected = c("neg99", "dash", "na_string")),
                                                                hr(),
                                                                sliderInput("upset_missing_count", "Observations with missing count between:",
                                                                            min = 1, max = 15, value = c(1, 15), step = 1),
                                                                sliderInput("upset_n_combinations", "Number of combinations to show:",
                                                                            min = 5, max = 30, value = 15, step = 1),
                                                                sliderInput("upset_nsets", "Number of sets to show:",
                                                                            min = 3, max = 15, value = 8, step = 1),
                                                                selectInput("upset_n_rows", "Rows to display in table:",
                                                                            choices = c("All", "10", "25", "50"), selected = "All")
                                                              )
                                                       ),
                                                       column(9,
                                                              uiOutput("pattern_selector"),
                                                              br(),
                                                              shinycssloaders::withSpinner(plotOutput("upset_plot", height = "600px"),
                                                                                           type = 4, color = "#2C3E50"),
                                                              br(),
                                                              h5("Pattern Details:"),
                                                              uiOutput("pattern_description"),
                                                              br(),
                                                              h5("Observations with Selected Pattern:"),
                                                              DTOutput("pattern_data_table")
                                                       )
                                                     )
                                            ),
                                            tabPanel("Missingness Prediction (RPART)",
                                                     br(),
                                                     fluidRow(
                                                       column(6,
                                                              selectInput("missing_target_var", "Predict when this variable is missing:",
                                                                          choices = names(df), selected = "POPULATION"),
                                                              checkboxGroupInput("missing_rpart_types", "Missing Value Types:",
                                                                                 choices = c("-99" = "neg99", "--" = "dash", "'NA'" = "na_string", "R NA" = "r_na"),
                                                                                 selected = c("neg99", "dash", "na_string", "r_na")),
                                                              pickerInput("missing_predictors", "Predictor Variables:",
                                                                          choices = names(df), selected = names(df)[1:5], multiple = TRUE),
                                                              actionButton("run_missing_rpart", "Build Missingness Tree",
                                                                           class = "btn-primary", style = "width: 100%;")
                                                       ),
                                                       column(6,
                                                              sliderInput("missing_rpart_cp", "Complexity Parameter:",
                                                                          min = 0, max = 0.1, value = 0.01, step = 0.001),
                                                              sliderInput("missing_rpart_minsplit", "Minimum Split:",
                                                                          min = 2, max = 50, value = 20, step = 1)
                                                       )
                                                     ),
                                                     br(),
                                                     shinycssloaders::withSpinner(plotOutput("missing_rpart_tree", height = "450px"),
                                                                                  type = 4, color = "#2C3E50"),
                                                     br(),
                                                     shinycssloaders::withSpinner(plotlyOutput("missing_rpart_importance", height = "400px"),
                                                                                  type = 4, color = "#2C3E50")
                                            ),
                                            tabPanel("Missing Variable Correlation",
                                                     br(),
                                                     div(class = "row",
                                                         div(class = "col-sm-3",
                                                             wellPanel(
                                                               selectInput("proc_missing_corr_method", "Correlation Method:",
                                                                           choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"),
                                                                           selected = "pearson"),
                                                               selectInput("proc_missing_corr_order", "Ordering:",
                                                                           choices = c("Original" = "original", "AOE" = "AOE", "FPC" = "FPC",
                                                                                       "Hierarchical Clustering" = "hclust", "Alphabetical" = "alphabet"),
                                                                           selected = "AOE"),
                                                               checkboxInput("proc_missing_corr_abs", "Use Absolute Correlations", FALSE),
                                                               checkboxInput("proc_missing_corr_display", "Show Values", FALSE)
                                                             )
                                                         ),
                                                         div(class = "col-sm-9",
                                                             shinycssloaders::withSpinner(plotlyOutput("proc_missing_corr_plot", height = "500px"),
                                                                                          type = 4, color = "#2C3E50")
                                                         )
                                                     )
                                            )
                                          )
                                      )
                                  )
                         ),
                         
                         # ==============================================================
                         # TAB B: OUTLIER ANALYSIS
                         # ==============================================================
                         tabPanel("Outlier Analysis",
                                  br(),
                                  div(class = "row",
                                      div(class = "col-sm-3",
                                          wellPanel(
                                            h5("Outlier Detection Settings"),
                                            
                                            h6("Mahalanobis Distance:"),
                                            sliderInput("outlier_mahalanobis_threshold", "Chi-Square Threshold:",
                                                        min = 0.9, max = 0.9999, value = 0.999, step = 0.001),
                                            
                                            hr(),
                                            
                                            h6("Cook's Distance:"),
                                            selectInput("outlier_cooks_method", "Threshold Method:",
                                                        choices = c("4 * mean" = "4mean", "4/n" = "4n", "Quantile 0.99" = "quantile"),
                                                        selected = "4mean"),
                                            
                                            hr(),
                                            
                                            h6("Local Outlier Factor:"),
                                            sliderInput("outlier_lof_minpts", "minPts (neighbors):", min = 3, max = 10, value = 5, step = 1),
                                            sliderInput("outlier_lof_threshold", "LOF Threshold:", min = 1, max = 5, value = 2, step = 0.1),
                                            
                                            hr(),
                                            
                                            h6("One-Class SVM:"),
                                            sliderInput("outlier_svm_nu", "nu parameter:", min = 0.01, max = 0.5, value = 0.05, step = 0.01),
                                            selectInput("outlier_svm_kernel", "Kernel:",
                                                        choices = c("linear", "radial", "polynomial", "sigmoid"), selected = "linear"),
                                            
                                            hr(),
                                            
                                            h6("Random Forest:"),
                                            sliderInput("outlier_rf_iqr", "IQR Multiplier for Residuals:", min = 1, max = 5, value = 2, step = 0.5),
                                            
                                            hr(),
                                            
                                            h6("Isolation Forest:"),
                                            sliderInput("outlier_if_threshold", "Isolation Forest Threshold:", min = 0.5, max = 0.9, value = 0.6, step = 0.01),
                                            
                                            hr(),
                                            
                                            div(class = "alert alert-info", style = "margin-top: 15px;",
                                                icon("info-circle"), 
                                                HTML("<strong>Live Updates:</strong> Analysis runs automatically when you change any parameter above."))
                                          )
                                      ),
                                      div(class = "col-sm-9",
                                          tabsetPanel(
                                            tabPanel("Combined View",
                                                     br(),
                                                     shinycssloaders::withSpinner(plotlyOutput("outlier_combined_plot", height = "500px"),
                                                                                  type = 4, color = "#2C3E50"),
                                                     br(),
                                                     h5("Outlier Summary Table (Observations flagged by 2+ methods):"),
                                                     DTOutput("outlier_consensus_table")
                                            ),
                                            tabPanel("Mahalanobis Distance",
                                                     br(),
                                                     shinycssloaders::withSpinner(plotlyOutput("outlier_mahalanobis_plot", height = "500px"),
                                                                                  type = 4, color = "#2C3E50"),
                                                     br(),
                                                     verbatimTextOutput("outlier_mahalanobis_stats")
                                            ),
                                            tabPanel("Cook's Distance",
                                                     br(),
                                                     shinycssloaders::withSpinner(plotlyOutput("outlier_cooks_plot", height = "500px"),
                                                                                  type = 4, color = "#2C3E50"),
                                                     br(),
                                                     verbatimTextOutput("outlier_cooks_stats")
                                            ),
                                            tabPanel("Local Outlier Factor",
                                                     br(),
                                                     shinycssloaders::withSpinner(plotlyOutput("outlier_lof_plot", height = "500px"),
                                                                                  type = 4, color = "#2C3E50"),
                                                     br(),
                                                     verbatimTextOutput("outlier_lof_stats")
                                            ),
                                            tabPanel("One-Class SVM",
                                                     br(),
                                                     shinycssloaders::withSpinner(plotlyOutput("outlier_svm_plot", height = "500px"),
                                                                                  type = 4, color = "#2C3E50"),
                                                     br(),
                                                     verbatimTextOutput("outlier_svm_stats")
                                            ),
                                            tabPanel("Random Forest Residuals",
                                                     br(),
                                                     shinycssloaders::withSpinner(plotlyOutput("outlier_rf_plot", height = "500px"),
                                                                                  type = 4, color = "#2C3E50"),
                                                     br(),
                                                     verbatimTextOutput("outlier_rf_stats")
                                            ),
                                            tabPanel("Isolation Forest",
                                                     br(),
                                                     shinycssloaders::withSpinner(plotlyOutput("outlier_if_plot", height = "500px"),
                                                                                  type = 4, color = "#2C3E50"),
                                                     br(),
                                                     verbatimTextOutput("outlier_if_stats")
                                            )
                                          )
                                      )
                                  )
                         ),
                         
                         # ==============================================================
                         # TAB C: DATA PROCESSING CONTROLS
                         # ==============================================================
                         tabPanel("Data Processing Controls",
                                  br(),
                                  fluidRow(
                                    column(6,
                                           div(class = "well",
                                               h4("Column Removal:"),
                                               selectInput("proc_remove_column", "Select column to remove:",
                                                           choices = names(df), selected = NULL),
                                               actionButton("proc_remove_col_btn", "Remove Selected Column",
                                                            class = "btn-danger btn-sm", style = "width: 100%; margin-bottom: 15px;"),
                                               
                                               hr(),
                                               
                                               h4("Numeric Column Transformation:"),
                                               selectInput("proc_transform_column", "Select numeric column:",
                                                           choices = numeric_cols, selected = NULL),
                                               selectInput("proc_transform_method", "Transformation method:",
                                                           choices = c("Log (ln)" = "log", "Square Root" = "sqrt", "Square" = "square",
                                                                       "Box-Cox" = "boxcox", "Yeo-Johnson" = "yeojohnson"),
                                                           selected = "log"),
                                               actionButton("proc_transform_btn", "Apply Transformation",
                                                            class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 15px;")
                                           )
                                    ),
                                    column(6,
                                           div(class = "well",
                                               h4("Missing Value Imputation:"),
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
                                               
                                               hr(),
                                               
                                               h4("Categorical Imputation:"),
                                               selectInput("proc_impute_cat_column", "Select categorical column:",
                                                           choices = categorical_cols[categorical_cols != "CODE"], selected = NULL),
                                               selectInput("proc_impute_cat_method", "Imputation method:",
                                                           choices = c("Mode" = "mode", "New Category 'Missing'" = "new_category", "Constant" = "constant"),
                                                           selected = "mode"),
                                               actionButton("proc_impute_cat_btn", "Apply Categorical Imputation",
                                                            class = "btn-info btn-sm", style = "width: 100%; margin-bottom: 15px;")
                                           )
                                    )
                                  ),
                                  fluidRow(
                                    column(6,
                                           div(class = "well",
                                               h4("Row Removal:"),
                                               sliderInput("proc_max_missing_per_row", "Max missing values per row:",
                                                           min = 0, max = 15, value = 5, step = 1),
                                               actionButton("proc_remove_rows_btn", "Remove Rows Exceeding Threshold",
                                                            class = "btn-warning btn-sm", style = "width: 100%; margin-bottom: 15px;"),
                                               
                                               hr(),
                                               
                                               h4("Create Ghost/Shadow Variables:"),
                                               selectInput("proc_create_shadow", "Select numeric column:",
                                                           choices = numeric_cols, selected = NULL),
                                               actionButton("proc_create_shadow_btn", "Create Missing Indicator Column",
                                                            class = "btn-info btn-sm", style = "width: 100%; margin-bottom: 15px;")
                                           )
                                    ),
                                    column(6,
                                           div(class = "well",
                                               h4("Batch Processing:"),
                                               actionButton("proc_apply_all", "Apply All Processing Steps",
                                                            class = "btn-primary btn-lg", style = "width: 100%; margin-bottom: 10px;"),
                                               actionButton("proc_reset_all", "Reset to Original Data",
                                                            class = "btn-danger btn-lg", style = "width: 100%;")
                                           )
                                    )
                                  ),
                                  hr(),
                                  fluidRow(
                                    column(12,
                                           div(class = "well",
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
                         )
             )
           )
  ),
  
  # ==========================================================================
  # TAB 3: RECIPE PIPELINE
  # ==========================================================================
  tabPanel("Recipe Pipeline",
           fluidPage(
             h2("Recipe-Based Processing Pipeline"),
             hr(),
             
             fluidRow(
               column(6,
                      div(class = "well",
                          h4("Recipe Configuration"),
                          hr(),
                          
                          h5("Missing Data Imputation:"),
                          selectInput("recipe_impute_method", "Imputation Method:",
                                      choices = c("None" = "none", "Median" = "median", "Mean" = "mean",
                                                  "KNN" = "knn", "Bagged Trees" = "bag"),
                                      selected = "median"),
                          conditionalPanel(
                            condition = "input.recipe_impute_method == 'knn'",
                            sliderInput("recipe_knn_k", "Number of Neighbors (k):", min = 1, max = 15, value = 5, step = 1)
                          ),
                          
                          hr(),
                          
                          h5("Outlier Treatment:"),
                          selectInput("recipe_outlier_method", "Method:",
                                      choices = c("None" = "none", "Winsorization (Capping)" = "capping", "Remove Outliers" = "remove"),
                                      selected = "none"),
                          conditionalPanel(
                            condition = "input.recipe_outlier_method != 'none'",
                            sliderInput("recipe_outlier_percentile", "Percentile Threshold:",
                                        min = 1, max = 20, value = 5, step = 1, post = "%")
                          ),
                          
                          hr(),
                          
                          h5("Data Transformations:"),
                          selectInput("recipe_transform", "Transformation:",
                                      choices = c("None" = "none", "Log" = "log", "Square Root" = "sqrt",
                                                  "Yeo-Johnson" = "yeojohnson", "Box-Cox" = "boxcox"),
                                      selected = "none"),
                          
                          hr(),
                          
                          h5("Feature Engineering:"),
                          checkboxInput("recipe_add_interactions", "Add Interaction Terms", value = FALSE),
                          conditionalPanel(
                            condition = "input.recipe_add_interactions == true",
                            selectInput("recipe_interaction_vars", "Select Variables for Interactions:",
                                        choices = numeric_cols, multiple = TRUE, selected = NULL)
                          ),
                          
                          checkboxInput("recipe_add_polynomial", "Add Polynomial Terms", value = FALSE),
                          conditionalPanel(
                            condition = "input.recipe_add_polynomial == true",
                            selectInput("recipe_poly_vars", "Select Variables for Polynomials:",
                                        choices = numeric_cols, multiple = TRUE, selected = NULL),
                            sliderInput("recipe_poly_degree", "Polynomial Degree:", min = 2, max = 3, value = 2, step = 1)
                          ),
                          
                          hr(),
                          
                          h5("Preprocessing:"),
                          checkboxInput("recipe_center_scale", "Center and Scale Variables", value = TRUE),
                          checkboxInput("recipe_dummy", "Create Dummy Variables for Categorical", value = TRUE),
                          
                          hr(),
                          
                          h5("Feature Selection:"),
                          checkboxInput("recipe_feature_selection", "Select Top Features", value = FALSE),
                          conditionalPanel(
                            condition = "input.recipe_feature_selection == true",
                            sliderInput("recipe_select_n_features", "Number of Features:", min = 1, max = 20, value = 10, step = 1)
                          ),
                          
                          hr(),
                          
                          actionButton("build_recipe", "Build Recipe", 
                                       class = "btn-primary", style = "width: 48%; margin-right: 4%;"),
                          actionButton("save_recipe", "Save Recipe", 
                                       class = "btn-info", style = "width: 48%;"),
                          br(), br(),
                          textInput("recipe_save_name", "Recipe Name (optional):", 
                                    placeholder = "e.g., Log_Transformed_Centered"),
                          br(),
                          actionButton("apply_recipe", "Apply Recipe", 
                                       class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
                          actionButton("reset_recipe", "Reset", 
                                       class = "btn-danger", style = "width: 100%;")
                      )
               ),
               
               column(6,
                      div(class = "well",
                          h4("Recipe Information"),
                          hr(),
                          h5("Variable Roles:"), 
                          verbatimTextOutput("recipe_roles"),
                          hr(),
                          h5("Recipe Steps:"), 
                          verbatimTextOutput("recipe_imputation"),
                          textOutput("recipe_outlier"),
                          textOutput("recipe_transformation"),
                          textOutput("recipe_dummy"),
                          hr(),
                          h5("Recipe Structure:"), 
                          verbatimTextOutput("recipe_structure"),
                          hr(),
                          h5("Processing Summary:"), 
                          verbatimTextOutput("recipe_summary")
                      )
               )
             ),
             
             hr(),
             
             fluidRow(
               column(12,
                      div(class = "well",
                          h4("Processed Data Preview"),
                          hr(),
                          DTOutput("recipe_processed_data")
                      )
               )
             ),
             
             hr(),
             
             fluidRow(
               column(12,
                      div(class = "well",
                          h4("Recipe Comparison"),
                          hr(),
                          fluidRow(
                            column(6,
                                   selectInput("compare_recipe_select", "Select Recipes to Compare:",
                                               choices = NULL, multiple = TRUE, selectize = TRUE)
                            ),
                            column(6,
                                   br(),
                                   actionButton("run_recipe_comparison", "Compare Selected Recipes",
                                                class = "btn-primary", style = "width: 100%; margin-top: 25px;")
                            )
                          ),
                          hr(),
                          h5("Comparison Results:"),
                          DTOutput("recipe_comparison_table"),
                          br(),
                          h5("Performance Visualization:"),
                          plotlyOutput("recipe_comparison_plot", height = "400px")
                      )
               )
             )
           )
  ),
  
  # ==========================================================================
  # TAB 4: GLMNET MODELING
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
                                       choices = c("Original Data" = "original",
                                                   "Processed Data (from Tab 2)" = "processed",
                                                   "Recipe Processed Data (from Tab 3)" = "recipe"),
                                       selected = "original"),
                          
                          conditionalPanel(
                            condition = "input.model_data_source == 'processed'",
                            div(style = "color: #28a745; background-color: #d4edda; padding: 8px; border-radius: 4px; margin-bottom: 15px;",
                                icon("check-circle"), 
                                HTML("<strong>Using processed data</strong><br>Transformations from Tab 2 will be applied.")
                            ),
                            actionButton("refresh_model_data", "Refresh Processed Data", 
                                         class = "btn-info btn-sm", style = "width: 100%; margin-bottom: 15px;")
                          ),
                          
                          conditionalPanel(
                            condition = "input.model_data_source == 'recipe'",
                            div(style = "color: #0c5460; background-color: #d1ecf1; padding: 8px; border-radius: 4px; margin-bottom: 15px;",
                                icon("info-circle"), 
                                HTML("<strong>Using recipe processed data</strong><br>Apply a recipe in Tab 3 first.")
                            )
                          ),
                          
                          hr(),
                          
                          h4("Model Settings"),
                          hr(),
                          numericInput("cv_folds", "CV Folds:", value = 5, min = 3, max = 10),
                          numericInput("cv_seed", "Random Seed:", value = 123),
                          sliderInput("glmnet_alpha", "Alpha (Elastic Net Mix - 0=Ridge, 1=Lasso):", 
                                      min = 0, max = 1, value = 0.5, step = 0.1),
                          checkboxInput("glmnet_tune_lambda", "Tune Lambda automatically", value = TRUE),
                          conditionalPanel(
                            condition = "input.glmnet_tune_lambda == false",
                            sliderInput("glmnet_lambda", "Lambda (Regularization strength):",
                                        min = 0.001, max = 1, value = 0.05, step = 0.01)
                          ),
                          hr(),
                          actionButton("train_model", "Train GLMNET Model", 
                                       class = "btn-primary", style = "width: 100%;")
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
                                 verbatimTextOutput("predictions_summary")
                        ),
                        tabPanel("Residual Analysis",
                                 plotlyOutput("residual_plot", height = "500px"),
                                 hr(),
                                 verbatimTextOutput("residual_stats"),
                                 hr(),
                                 sliderInput("residual_iqr", "IQR Multiplier:", min = 1, max = 5, value = 1.5, step = 0.1),
                                 DTOutput("residual_outliers")
                        ),
                        tabPanel("Cross-Validation",
                                 plotlyOutput("cv_plot", height = "500px"),
                                 hr(),
                                 verbatimTextOutput("cv_summary")
                        )
                      )
               )
             )
           )
  ),
  
  # ==========================================================================
  # TAB 5: REPORT SUMMARY
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
  )
)