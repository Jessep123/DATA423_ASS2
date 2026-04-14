# ui.R 

ui <- navbarPage(
  title = "DATA423 Assignment 2 - Eduard Bradley",
  theme = shinytheme("cerulean"),
  useShinyjs(),
  
  # ============================================================================
  # CONSOLIDATED CUSTOM CSS
  # ============================================================================
  
  tags$head(
    # CSS Styles
    tags$style(HTML("
    /* Base sidebar wrapper styles */
    .sidebar-wrapper {
      position: relative;
    }
    
    /* Hide sidebar panels */
    .sidebar-hidden .sidebar-panel,
    .sidebar-wrapper.sidebar-hidden .sidebar-panel {
      display: none !important;
    }
    
    /* Expand main panel when sidebar hidden */
    .sidebar-hidden .main-panel {
      width: 100% !important;
      margin-left: 0 !important;
      flex: 0 0 100% !important;
      max-width: 100% !important;
    }
    
    /* For all tab wrappers - target the sidebar column directly */
    .sidebar-wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    
    .sidebar-wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
      flex: 0 0 100% !important;
      max-width: 100% !important;
    }
    
    /* Specific rule for each tab wrapper */
    #boxplot_wrapper.sidebar-hidden .col-sm-3,
    #correlation_wrapper.sidebar-hidden .col-sm-3,
    #datatable_wrapper.sidebar-hidden .col-sm-3,
    #heatmap_wrapper.sidebar-hidden .col-sm-3,
    #distribution_wrapper.sidebar-hidden .col-sm-3,
    #scatter_wrapper.sidebar-hidden .col-sm-3,
    #mosaic_wrapper.sidebar-hidden .col-sm-3,
    #ggpairs_wrapper.sidebar-hidden .col-sm-3,
    #tabplot_wrapper.sidebar-hidden .col-sm-3,
    #rising_wrapper.sidebar-hidden .col-sm-3 {
      display: none !important;
    }
    
    #boxplot_wrapper.sidebar-hidden .col-sm-9,
    #correlation_wrapper.sidebar-hidden .col-sm-9,
    #datatable_wrapper.sidebar-hidden .col-sm-9,
    #heatmap_wrapper.sidebar-hidden .col-sm-9,
    #distribution_wrapper.sidebar-hidden .col-sm-9,
    #scatter_wrapper.sidebar-hidden .col-sm-9,
    #mosaic_wrapper.sidebar-hidden .col-sm-9,
    #ggpairs_wrapper.sidebar-hidden .col-sm-9,
    #tabplot_wrapper.sidebar-hidden .col-sm-9,
    #rising_wrapper.sidebar-hidden .col-sm-9 {
      width: 100% !important;
      margin-left: 0 !important;
      flex: 0 0 100% !important;
      max-width: 100% !important;
    }
    
    /* Master sidebar wrapper for Data Processing Strategy */
    #master_sidebar_wrapper.sidebar-hidden #master_sidebar_panel {
      display: none !important;
    }
    
    #master_sidebar_wrapper.sidebar-hidden #master_main_panel {
      width: 100% !important;
      margin-left: 0 !important;
      flex: 0 0 100% !important;
      max-width: 100% !important;
    }
    
    /* GLMNET Modeling sidebar toggle */
    #model_sidebar_wrapper.sidebar-hidden {
      display: none !important;
    }
    
    #model_main_content_wrapper {
      transition: all 0.3s ease;
    }
    
    /* Toggle button container */
    .toggle-container {
      margin-bottom: 15px;
      text-align: right;
    }
    
    .toggle-sidebar-btn {
      margin-bottom: 10px;
    }
    
    /* Collapsible sections for GLMNET sidebar */
    .collapsible-section {
      margin-bottom: 10px;
    }
    .collapsible-header {
      background-color: #f8f9fa;
      cursor: pointer;
      padding: 10px;
      border-radius: 4px;
      font-weight: bold;
      color: #2C3E50;
      transition: all 0.3s ease;
    }
    .collapsible-header:hover {
      background-color: #e9ecef;
    }
    .collapsible-content {
      padding: 10px;
      border-left: 2px solid #dee2e6;
      border-right: 2px solid #dee2e6;
      border-bottom: 2px solid #dee2e6;
      border-radius: 0 0 4px 4px;
    }
    .collapsible-icon {
      float: right;
      transition: transform 0.3s ease;
    }
    .collapsed .collapsible-icon {
      transform: rotate(-90deg);
    }
    
    /* Style for HTML5 range sliders */
    input[type=range] {
      -webkit-appearance: none;
      background: #e9ecef;
      border-radius: 5px;
      height: 4px;
      width: 100%;
    }
    input[type=range]::-webkit-slider-thumb {
      -webkit-appearance: none;
      background: #13D4D4;
      border-radius: 50%;
      cursor: pointer;
      height: 16px;
      width: 16px;
    }
    input[type=range]::-webkit-slider-thumb:hover {
      background: #0fb0b0;
    }
    input[type=range]::-moz-range-thumb {
      background: #13D4D4;
      border: none;
      border-radius: 50%;
      cursor: pointer;
      height: 16px;
      width: 16px;
    }
    
    /* Help button style */
    .btn-help {
      cursor: help;
      border-bottom: 1px dotted #999;
    }
    ")),
    
    # =========================================================================
    # JAVASCRIPT - Place ALL JavaScript here inside tags$script(HTML(...))
    # =========================================================================
    tags$script(HTML("
      $(document).on('input', '#glmnet_alpha_slider', function() {
        document.getElementById('glmnet_alpha').value = this.value;
        Shiny.setInputValue('glmnet_alpha', parseFloat(this.value));
      });
      
      $(document).on('input', '#residual_iqr_slider', function() {
        Shiny.setInputValue('residual_iqr', parseFloat(this.value));
      });
      
      $(document).on('input', '#residual_boxplot_iqr_slider', function() {
        Shiny.setInputValue('residual_boxplot_iqr', parseFloat(this.value));
      });
      
      Shiny.addCustomMessageHandler('update_dataset_picker', function(message) {
        var select = document.getElementById('model_dataset_select');
        if (select) {
          select.innerHTML = '';
          for (var i = 0; i < message.choices.length; i++) {
            var option = document.createElement('option');
            option.value = message.choices[i];
            option.text = message.choices[i];
            select.appendChild(option);
          }
          if (message.selected) {
            select.value = message.selected;
          } else if (message.choices.length > 0) {
            select.value = message.choices[0];
          }
          var event = new Event('change', { bubbles: true });
          select.dispatchEvent(event);
        }
      });
      
      Shiny.addCustomMessageHandler('update_recipe_picker', function(message) {
        console.log('Recipes updated:', message.choices);
      });
      
      Shiny.addCustomMessageHandler('update_progress', function(message) {
        var statusDiv = document.getElementById('progress_status');
        var barDiv = document.getElementById('progress_bar');
        if (statusDiv) statusDiv.innerHTML = message.status;
        if (barDiv) {
          barDiv.style.width = message.percent + '%';
          barDiv.innerHTML = message.percent + '%';
        }
      });
      
      Shiny.addCustomMessageHandler('click_process_btn', function(message) {
        var btn = document.getElementById('process_data_btn');
        if (btn) {
          btn.click();
        }
      });
      
      $(document).ready(function() {
        $('.collapsible-header').click(function() {
          var content = $(this).next('.collapsible-content');
          content.slideToggle(200);
          $(this).toggleClass('collapsed');
        });
      });
    "))
  ),
  
  # ==========================================================================
  # TAB 1: INTRODUCTION
  # ==========================================================================
  
  tabPanel("Introduction",
           fluidPage(
             h2("Covid-19 Data Analysis: Predicting Death Rates"),
             hr(),
             
             fluidRow(
               column(12,
                      div(class = "well",
                          h4("Background"),
                          p("Covid-19 data, all measurements are as at 2019."),
                          br(),
                          h4("Dataset Variables"),
                          p("The supplied CSV contains the following variables:"),
                          tags$ul(
                            tags$li(tags$strong("CODE"), " - Anonymised state or country"),
                            tags$li(tags$strong("GOVERN_TYPE"), " - Type of government: 'STABLE DEM', 'UNSTABLE DEM', 'DICTATORSHIP', 'OTHER'"),
                            tags$li(tags$strong("POPULATION"), " - Total population"),
                            tags$li(tags$strong("AGE25_PROPTN"), " - The proportion of the population that is at or below 25"),
                            tags$li(tags$strong("AGE_MEDIAN"), " - The median age of the population"),
                            tags$li(tags$strong("AGE50_PROPTN"), " - The proportion of the population that is at or above 50"),
                            tags$li(tags$strong("POP_DENSITY"), " - The population density"),
                            tags$li(tags$strong("GDP"), " - The Gross National Product"),
                            tags$li(tags$strong("INFANT_MORT"), " - The infant mortality rate"),
                            tags$li(tags$strong("DOCS"), " - The number of doctors per 10,000"),
                            tags$li(tags$strong("VAX_RATE"), " - The mean vaccination rate for Covid-19"),
                            tags$li(tags$strong("HEALTHCARE_BASIS"), " - Type of healthcare system: 'INSURANCE', 'PRIVATE', 'FREE'"),
                            tags$li(tags$strong("HEALTHCARE_COST"), " - Healthcare costs per person where applicable"),
                            tags$li(tags$strong("DEATH_RATE"), tags$em("(Outcome Variable)"), " - The projected death rate (across ten years)"),
                            tags$li(tags$strong("OBS_TYPE"), " - The allocation to test or train")
                          ),
                          br(),
                          p("The outcome variable is the ", tags$strong("DEATH_RATE"), ".")
                      )
               )
             ),
             
             hr(),
             
             fluidRow(
               column(6, 
                      div(class = "well", 
                          h4("Data Description"), 
                          hr(), 
                          textOutput("intro_data_desc")
                      )
               ),
               column(6, 
                      div(class = "well", 
                          h4("Missingness Strategy"), 
                          hr(), 
                          textOutput("intro_missing_strategy")
                      )
               )
             ),
             
             fluidRow(
               column(6, 
                      div(class = "well", 
                          h4("Outlier Strategy"), 
                          hr(), 
                          textOutput("intro_outlier_strategy")
                      )
               ),
               column(6, 
                      div(class = "well", 
                          h4("Pre-processing Strategy"), 
                          hr(), 
                          textOutput("intro_preprocess_strategy")
                      )
               )
             ),
             
             fluidRow(
               column(6, 
                      div(class = "well", 
                          h4("GLMNET Explanation"), 
                          hr(), 
                          textOutput("intro_glmnet_explanation")
                      )
               ),
               column(12, 
                      div(class = "well", 
                          h4("Note"), 
                          hr(), 
                          p("This application allows you to explore the data, process it with custom recipes, 
                          train GLMNET models, and evaluate performance. Use the tabs above to navigate 
                          through the analysis workflow.")
                      )
               )
             )
           )
  ),
  
  
  # ==========================================================================
  # ==========================================================================
  # TAB 2: SUMMARY & EDA
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
                                           hr(),
                                           h4("Filter Data by Categorical Variables:"),
                                           pickerInput("boxplot_filter_vars", "Select Categorical Variables to Filter:", 
                                                       choices = c("GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE"),
                                                       selected = NULL,
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                           
                                           uiOutput("boxplot_cat_filters_ui"),
                                           
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
                                           
                                           # Dynamic UI for checkboxes will appear here
                                           uiOutput("corr_cat_filters_ui"),
                                           
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
                                        
                                        # Collapsible GGpairs Summary
                                        tags$div(
                                          class = "panel-group",
                                          tags$div(
                                            class = "panel panel-default",
                                            tags$div(
                                              class = "panel-heading",
                                              tags$h4(
                                                class = "panel-title",
                                                tags$a(`data-toggle` = "collapse", href = "#collapseGGpairsStats", 
                                                       "GGpairs Plot Summary",
                                                       style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                              )
                                            ),
                                            tags$div(
                                              id = "collapseGGpairsStats",
                                              class = "panel-collapse collapse",
                                              tags$div(
                                                class = "panel-body",
                                                verbatimTextOutput("ggpairs_obs_count")
                                              )
                                            )
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
  # TAB 3: DATA PROCESSING STRATEGY (Missingness + Outlier Analysis Only)
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
                                          selected = categorical_cols[categorical_cols != "CODE"],
                                          multiple = TRUE,
                                          options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                              pickerInput("master_numeric_vars", "Numeric Variables:", 
                                          choices = numeric_cols, 
                                          selected = numeric_cols,
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
                                      
                                      # Missingness Analysis Sub-tab
                                      tabPanel("Missingness Analysis",
                                               br(),
                                               tabsetPanel(
                                                 tabPanel("Missing Values Heatmap",
                                                          br(),
                                                          shinycssloaders::withSpinner(plotlyOutput("proc_missing_heatmap", height = "550px"), type = 4, color = "#2C3E50")
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
                                                                   pickerInput("missing_predictors", "Predictor Variables:", 
                                                                               choices = names(df)[!names(df) %in% c("CODE")],
                                                                               selected = names(df)[!names(df) %in% c("CODE", "DEATH_RATE", "OBS_TYPE")],
                                                                               multiple = TRUE),
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
                                      
                                      # Outlier Analysis Sub-tab
                                      tabPanel("Outlier Analysis",
                                               br(),
                                               fluidRow(
                                                 column(3,
                                                        div(class = "control-card", h4("Mahalanobis Distance"), 
                                                            sliderInput("outlier_mahalanobis_threshold", "Chi-Square Threshold:", min = 0.9, max = 0.9999, value = 0.999, step = 0.001)),
                                                        div(class = "control-card", h4("Cook's Distance"), 
                                                            selectInput("outlier_cooks_method", "Threshold Method:", choices = c("4 * mean" = "4mean", "4/n" = "4n", "Quantile 0.99" = "quantile"), selected = "4mean")),
                                                        div(class = "control-card", h4("Local Outlier Factor"), 
                                                            sliderInput("outlier_lof_minpts", "minPts (neighbors):", min = 3, max = 10, value = 5, step = 1), 
                                                            sliderInput("outlier_lof_threshold", "LOF Threshold:", min = 1, max = 5, value = 2, step = 0.1)),
                                                        div(class = "control-card", h4("One-Class SVM"), 
                                                            sliderInput("outlier_svm_nu", "nu parameter:", min = 0.01, max = 0.5, value = 0.05, step = 0.01), 
                                                            selectInput("outlier_svm_kernel", "Kernel:", choices = c("linear", "radial", "polynomial", "sigmoid"), selected = "linear")),
                                                        div(class = "control-card", h4("Random Forest"), 
                                                            sliderInput("outlier_rf_iqr", "IQR Multiplier for Residuals:", min = 1, max = 5, value = 2, step = 0.5)),
                                                        div(class = "control-card", h4("Isolation Forest"), 
                                                            sliderInput("outlier_if_threshold", "Isolation Forest Threshold:", min = 0.5, max = 0.9, value = 0.6, step = 0.01)),
                                                        div(class = "control-card", h4("Consensus Table Filter"), 
                                                            sliderInput("outlier_min_flags", "Minimum number of methods flagging:", min = 1, max = 6, value = 2, step = 1, post = " methods"))
                                                 ),
                                                 column(9,
                                                        tabsetPanel(
                                                          tabPanel("Combined View",
                                                                   br(),
                                                                   shinycssloaders::withSpinner(DTOutput("outlier_consensus_table"), type = 4, color = "#2C3E50"),
                                                                   br(),
                                                                   div(class = "well", style = "padding: 15px; margin-top: 10px;",
                                                                       fluidRow(
                                                                         column(4, 
                                                                                h5("Display Controls", style = "margin-top: 0;"), 
                                                                                sliderInput("outlier_display_count", "Number of outliers to display:", min = 5, max = 100, value = 20, step = 5)
                                                                         ),
                                                                         column(8,
                                                                                h5("Select Methods to Include", style = "margin-top: 0;"),
                                                                                fluidRow(
                                                                                  column(4, checkboxInput("show_mahalanobis", "Mahalanobis Distance", value = TRUE)),
                                                                                  column(4, checkboxInput("show_cooks", "Cook's Distance", value = TRUE)),
                                                                                  column(4, checkboxInput("show_lof", "Local Outlier Factor", value = TRUE))
                                                                                ),
                                                                                fluidRow(
                                                                                  column(4, checkboxInput("show_svm", "One-Class SVM", value = TRUE)),
                                                                                  column(4, checkboxInput("show_rf", "Random Forest Residuals", value = TRUE)),
                                                                                  column(4, checkboxInput("show_iforest", "Isolation Forest", value = TRUE))
                                                                                )
                                                                         )
                                                                       )
                                                                   )
                                                          ),
                                                          tabPanel("Mahalanobis Distance", 
                                                                   br(), 
                                                                   shinycssloaders::withSpinner(plotlyOutput("outlier_mahalanobis_plot", height = "500px"), type = 4, color = "#2C3E50"), 
                                                                   br(), hr(),
                                                                   tags$div(class = "panel-group",
                                                                            tags$div(class = "panel panel-default",
                                                                                     tags$div(class = "panel-heading",
                                                                                              tags$h4(class = "panel-title",
                                                                                                      tags$a(`data-toggle` = "collapse", href = "#collapseMahalanobisStats", "Mahalanobis Distance Statistics",
                                                                                                             style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                                                              )
                                                                                     ),
                                                                                     tags$div(id = "collapseMahalanobisStats", class = "panel-collapse collapse",
                                                                                              tags$div(class = "panel-body", verbatimTextOutput("outlier_mahalanobis_stats"))
                                                                                     )
                                                                            )
                                                                   )
                                                          ),
                                                          tabPanel("Cook's Distance", 
                                                                   br(), 
                                                                   shinycssloaders::withSpinner(plotlyOutput("outlier_cooks_plot", height = "500px"), type = 4, color = "#2C3E50"), 
                                                                   br(), hr(),
                                                                   tags$div(class = "panel-group",
                                                                            tags$div(class = "panel panel-default",
                                                                                     tags$div(class = "panel-heading",
                                                                                              tags$h4(class = "panel-title",
                                                                                                      tags$a(`data-toggle` = "collapse", href = "#collapseCooksStats", "Cook's Distance Statistics",
                                                                                                             style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                                                              )
                                                                                     ),
                                                                                     tags$div(id = "collapseCooksStats", class = "panel-collapse collapse",
                                                                                              tags$div(class = "panel-body", verbatimTextOutput("outlier_cooks_stats"))
                                                                                     )
                                                                            )
                                                                   )
                                                          ),
                                                          tabPanel("Local Outlier Factor", 
                                                                   br(), 
                                                                   shinycssloaders::withSpinner(plotlyOutput("outlier_lof_plot", height = "500px"), type = 4, color = "#2C3E50"), 
                                                                   br(), hr(),
                                                                   tags$div(class = "panel-group",
                                                                            tags$div(class = "panel panel-default",
                                                                                     tags$div(class = "panel-heading",
                                                                                              tags$h4(class = "panel-title",
                                                                                                      tags$a(`data-toggle` = "collapse", href = "#collapseLOFStats", "Local Outlier Factor Statistics",
                                                                                                             style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                                                              )
                                                                                     ),
                                                                                     tags$div(id = "collapseLOFStats", class = "panel-collapse collapse",
                                                                                              tags$div(class = "panel-body", verbatimTextOutput("outlier_lof_stats"))
                                                                                     )
                                                                            )
                                                                   )
                                                          ),
                                                          tabPanel("One-Class SVM", 
                                                                   br(), 
                                                                   shinycssloaders::withSpinner(plotlyOutput("outlier_svm_plot", height = "500px"), type = 4, color = "#2C3E50"), 
                                                                   br(), hr(),
                                                                   tags$div(class = "panel-group",
                                                                            tags$div(class = "panel panel-default",
                                                                                     tags$div(class = "panel-heading",
                                                                                              tags$h4(class = "panel-title",
                                                                                                      tags$a(`data-toggle` = "collapse", href = "#collapseSVMStats", "One-Class SVM Statistics",
                                                                                                             style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                                                              )
                                                                                     ),
                                                                                     tags$div(id = "collapseSVMStats", class = "panel-collapse collapse",
                                                                                              tags$div(class = "panel-body", verbatimTextOutput("outlier_svm_stats"))
                                                                                     )
                                                                            )
                                                                   )
                                                          ),
                                                          tabPanel("Random Forest Residuals", 
                                                                   br(), 
                                                                   shinycssloaders::withSpinner(plotlyOutput("outlier_rf_plot", height = "500px"), type = 4, color = "#2C3E50"), 
                                                                   br(), hr(),
                                                                   tags$div(class = "panel-group",
                                                                            tags$div(class = "panel panel-default",
                                                                                     tags$div(class = "panel-heading",
                                                                                              tags$h4(class = "panel-title",
                                                                                                      tags$a(`data-toggle` = "collapse", href = "#collapseRFStats", "Random Forest Residuals Statistics",
                                                                                                             style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                                                              )
                                                                                     ),
                                                                                     tags$div(id = "collapseRFStats", class = "panel-collapse collapse",
                                                                                              tags$div(class = "panel-body", verbatimTextOutput("outlier_rf_stats"))
                                                                                     )
                                                                            )
                                                                   )
                                                          ),
                                                          tabPanel("Isolation Forest", 
                                                                   br(), 
                                                                   shinycssloaders::withSpinner(plotlyOutput("outlier_if_plot", height = "500px"), type = 4, color = "#2C3E50"), 
                                                                   br(), hr(),
                                                                   tags$div(class = "panel-group",
                                                                            tags$div(class = "panel panel-default",
                                                                                     tags$div(class = "panel-heading",
                                                                                              tags$h4(class = "panel-title",
                                                                                                      tags$a(`data-toggle` = "collapse", href = "#collapseIFStats", "Isolation Forest Statistics",
                                                                                                             style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                                                              )
                                                                                     ),
                                                                                     tags$div(id = "collapseIFStats", class = "panel-collapse collapse",
                                                                                              tags$div(class = "panel-body", verbatimTextOutput("outlier_if_stats"))
                                                                                     )
                                                                            )
                                                                   )
                                                          )
                                                        )
                                                 )
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
  # TAB 4: DATA PROCESSING CONTROLS (Pipeline Builder)
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
                          "Build a step-by-step reproducible recipe for data processing."
                      )
               )
             ),
             
             # Recipe Builder only (no Quick Batch Actions tab)
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
                            column(6,
                                   textInput("pipeline_name", "Recipe Name", 
                                             placeholder = "e.g., Cleaned_Data_v1"),
                                   textAreaInput("pipeline_comments", "Comments (optional)",
                                                 placeholder = "Describe what this recipe does...",
                                                 rows = 2),
                                   actionButton("save_pipeline_btn", "Save Recipe", 
                                                class = "btn-info btn-sm", style = "width: 100%; margin-top: 10px;")
                            ),
                            column(6,
                                   actionButton("save_as_pipeline_btn", "Save As New Recipe", 
                                                class = "btn-success btn-sm", style = "width: 100%; margin-top: 10px;"),
                                   br(),
                                   actionButton("clear_pipeline_btn", "Clear All Steps", 
                                                class = "btn-warning btn-sm", style = "width: 100%; margin-top: 10px;")
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
                          
                          # Apply Loaded Recipe button
                          actionButton("apply_loaded_recipe_btn", "Apply Loaded Recipe", 
                                       class = "btn-success btn-sm", style = "width: 100%; margin-bottom: 10px;"),
                          
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
                          h5("Current Data Summary:"),
                          verbatimTextOutput("proc_data_summary_recipe"),
                          hr(),
                          h5("Data Preview (First 20 rows):"),
                          DTOutput("proc_data_preview_recipe"),
                          hr(),
                          downloadButton("proc_download_data_recipe", "Download Processed Data (CSV)",
                                         class = "btn-success", style = "width: 100%;")
                      )
               )
             ),
             
             hr(),
             
             fluidRow(
               column(12,
                      div(class = "control-card",
                          h4("Processing Log"),
                          hr(),
                          verbatimTextOutput("proc_log_recipe")
                      )
               )
             )
           )
  ),  
  
  
  # ==========================================================================
  # TAB 5: GLMNET MODELING
  # ==========================================================================
  
  tabPanel("GLMNET Modeling",
           fluidPage(
             fluidRow(
               column(12,
                      actionButton("toggle_model_sidebar", 
                                   HTML('<i class="fa fa-sliders-h"></i> Hide Settings Panel'),
                                   class = "btn-primary btn-sm",
                                   style = "margin-bottom: 10px; background-color: #2C3E50; border-color: #2C3E50; width: auto;"
                      )
               )
             ),
             
             fluidRow(
               # Sidebar column - with ID that matches server hide/show
               div(id = "model_sidebar_panel", class = "col-sm-3",
                   div(class = "well",
                       # Data Source Section - Collapsible
                       div(class = "collapsible-section",
                           div(class = "collapsible-header", 
                               onclick = "Shiny.setInputValue('collapse_data_source', Math.random())",
                               icon("database"), " Data Source",
                               span(class = "collapsible-icon", icon("chevron-down"))),
                           div(id = "collapse_data_source_content", class = "collapsible-content",
                               style = "display: block;",
                               radioButtons("model_data_source", NULL, 
                                            choices = c("Original Data" = "original", "Saved Dataset" = "saved"),
                                            selected = "original"),
                               
                               conditionalPanel(condition = "input.model_data_source == 'saved'",
                                                div(style = "color: #17a2b8; background-color: #d1ecf1; padding: 8px; border-radius: 4px; margin-bottom: 15px;",
                                                    icon("database"), HTML("<strong>Using saved dataset</strong><br>Select a dataset saved from the Data Processing tab.")
                                                ),
                                                pickerInput("model_dataset_select", "Select Saved Dataset:", choices = NULL, options = list(`live-search` = TRUE))
                               ),
                               
                               conditionalPanel(condition = "input.model_data_source == 'processed'",
                                                div(style = "color: #28a745; background-color: #d4edda; padding: 8px; border-radius: 4px; margin-bottom: 15px;",
                                                    icon("check-circle"), HTML("<strong>Using current processed data</strong><br>From the Data Processing tab.")
                                                ),
                                                actionButton("refresh_model_data", "Refresh Processed Data", class = "btn-info btn-sm", style = "width: 100%;")
                               )
                           )
                       ),
                       
                       hr(),
                       
                       # Model Settings Section - Collapsible
                       div(class = "collapsible-section",
                           div(class = "collapsible-header", 
                               onclick = "Shiny.setInputValue('collapse_model_settings', Math.random())",
                               icon("sliders-h"), " Model Settings",
                               span(class = "collapsible-icon", icon("chevron-down"))),
                           div(id = "collapse_model_settings_content", class = "collapsible-content",
                               style = "display: block;",
                               div(
                                 tags$label("Alpha (Elastic Net Mix):", `for` = "glmnet_alpha_slider", style = "font-weight: bold;"),
                                 tags$input(id = "glmnet_alpha_slider", type = "range", 
                                            min = 0, max = 1, step = 0.05, value = 0.5,
                                            style = "width: 100%; margin-top: 10px;"),
                                 tags$input(id = "glmnet_alpha", type = "hidden", value = 0.5),
                                 
                                 tags$div(style = "display: flex; justify-content: space-between; margin-top: 5px; margin-bottom: 10px;",
                                          tags$span("0", style = "font-size: 11px; color: #2C3E50; font-weight: bold;"),
                                          tags$span("0.2", style = "font-size: 11px; color: #666;"),
                                          tags$span("0.4", style = "font-size: 11px; color: #666;"),
                                          tags$span("0.6", style = "font-size: 11px; color: #666;"),
                                          tags$span("0.8", style = "font-size: 11px; color: #666;"),
                                          tags$span("1", style = "font-size: 11px; color: #2C3E50; font-weight: bold;")
                                 ),
                                 p(class = "help-block", "0 = Ridge | 0.1-0.9 = Elastic Net | 1 = Lasso", 
                                   style = "font-size: 11px; color: #666; margin-top: 5px; margin-bottom: 10px;")
                               ),
                               div(
                                 numericInput("glmnet_lambda", "Lambda (Regularization):", 
                                              value = 0.01, min = 0.001, max = 1, step = 0.001),
                                 p(class = "help-block", "Leave empty for CV-optimized lambda", 
                                   style = "font-size: 11px; color: #666; margin-top: -10px; margin-bottom: 10px;")
                               ),
                               numericInput("cv_folds", "CV Folds:", value = 5, min = 3, max = 10),
                               numericInput("cv_seed", "Random Seed:", value = 123)
                           )
                       ),
                       
                       hr(),
                       
                       actionButton("train_model", "Train GLMNET Model", class = "btn-primary", style = "width: 100%;")
                   )
               ),
               
               # Main content column
               div(id = "model_main_content_wrapper", class = "main-content-wrapper",
                   column(9,
                          mainPanel(width = 12,
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
                                               fluidRow(
                                                 column(3,
                                                        div(class = "well",
                                                            h4("Display Controls"),
                                                            hr(),
                                                            radioButtons("predictions_dataset", "Dataset to Display:",
                                                                         choices = c("Train Only" = "train", "Test Only" = "test", "Both (Train + Test)" = "both"),
                                                                         selected = "both"),
                                                            hr(),
                                                            p("Points represent individual observations.", style = "font-size: 11px; color: #666;"),
                                                            p("The dashed line shows perfect prediction (Actual = Predicted).", style = "font-size: 11px; color: #666;")
                                                        )
                                                 ),
                                                 column(9,
                                                        plotlyOutput("predictions_plot", height = "500px"), 
                                                        hr(),
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
                                                 )
                                               )
                                      ),
                                      tabPanel("Residual Analysis",
                                               fluidRow(
                                                 column(3,
                                                        div(class = "well",
                                                            h4("Display Controls"),
                                                            hr(),
                                                            radioButtons("residual_display_dataset", "Dataset to Display:",
                                                                         choices = c("Train Only" = "train", "Test Only" = "test", "Both (Train + Test)" = "both"),
                                                                         selected = "both"),
                                                            hr(),
                                                            div(
                                                              tags$label("IQR Multiplier for Outlier Detection:", `for` = "residual_iqr_slider", style = "font-weight: bold;"),
                                                              tags$input(id = "residual_iqr_slider", type = "range",
                                                                         min = 1, max = 5, step = 0.1, value = 1.5,
                                                                         style = "width: 100%; margin-top: 10px;"),
                                                              tags$div(style = "display: flex; justify-content: space-between; margin-top: 5px; margin-bottom: 10px;",
                                                                       tags$span("1", style = "font-size: 11px; color: #2C3E50; font-weight: bold;"),
                                                                       tags$span("2", style = "font-size: 11px; color: #666;"),
                                                                       tags$span("3", style = "font-size: 11px; color: #2C3E50; font-weight: bold;"),
                                                                       tags$span("4", style = "font-size: 11px; color: #666;"),
                                                                       tags$span("5", style = "font-size: 11px; color: #2C3E50; font-weight: bold;")
                                                              ),
                                                              p(class = "help-block", "Standard IQR multiplier is 1.5. Higher values = fewer outliers detected.",
                                                                style = "font-size: 11px; color: #666; margin-top: 5px;")
                                                            )
                                                        )
                                                 ),  # <-- comma here, column(3) closed
                                                 column(9,  # <-- column(9) now inside fluidRow
                                                        plotlyOutput("residual_plot", height = "500px"),
                                                        hr(),
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
                                                              class = "panel-collapse collapse in",
                                                              tags$div(
                                                                class = "panel-body",
                                                                DTOutput("residual_outliers")
                                                              )
                                                            )
                                                          )
                                                        )
                                                 )  
                                               )    
                                      ),          
                                      tabPanel("Residual Boxplot",
                                               fluidRow(
                                                 column(3,
                                                        div(class = "well",
                                                            h4("Residual Analysis Controls"),
                                                            hr(),
                                                            radioButtons("residual_dataset", "Dataset to Display:",
                                                                         choices = c("Train Only" = "train", "Test Only" = "test", "Both (Train + Test)" = "both"),
                                                                         selected = "both"),
                                                            hr(),
                                                            div(
                                                              tags$label("IQR Multiplier for Outlier Detection:", `for` = "residual_boxplot_iqr_slider", style = "font-weight: bold;"),
                                                              tags$input(id = "residual_boxplot_iqr_slider", type = "range", 
                                                                         min = 1, max = 5, step = 0.1, value = 1.5,
                                                                         style = "width: 100%; margin-top: 10px;"),
                                                              tags$div(style = "display: flex; justify-content: space-between; margin-top: 5px; margin-bottom: 10px;",
                                                                       tags$span("1", style = "font-size: 11px; color: #2C3E50; font-weight: bold;"),
                                                                       tags$span("2", style = "font-size: 11px; color: #666;"),
                                                                       tags$span("3", style = "font-size: 11px; color: #2C3E50; font-weight: bold;"),
                                                                       tags$span("4", style = "font-size: 11px; color: #666;"),
                                                                       tags$span("5", style = "font-size: 11px; color: #2C3E50; font-weight: bold;")
                                                              ),
                                                              p(class = "help-block", "Standard IQR multiplier is 1.5. Higher values = fewer outliers detected.", 
                                                                style = "font-size: 11px; color: #666; margin-top: 5px;")
                                                            ),
                                                            hr(),
                                                            p("Outliers are points beyond the whiskers (Q1 - IQR×multiplier, Q3 + IQR×multiplier).",
                                                              style = "font-size: 11px; color: #666;"),
                                                            p("Hover over points to see CODE and residual value.",
                                                              style = "font-size: 11px; color: #666; margin-top: 5px;")
                                                        )
                                                 ),
                                                 column(9,
                                                        shinycssloaders::withSpinner(
                                                          plotlyOutput("residual_boxplot", height = "500px"),
                                                          type = 4, color = "#2C3E50", size = 0.7
                                                        ),
                                                        br(),
                                                        hr(),
                                                        tags$div(
                                                          class = "panel-group",
                                                          tags$div(
                                                            class = "panel panel-default",
                                                            tags$div(
                                                              class = "panel-heading",
                                                              tags$h4(
                                                                class = "panel-title",
                                                                tags$a(`data-toggle` = "collapse", href = "#collapseResidualBoxplotStats", 
                                                                       "Residual Boxplot Statistics",
                                                                       style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                              )
                                                            ),
                                                            tags$div(
                                                              id = "collapseResidualBoxplotStats",
                                                              class = "panel-collapse collapse",
                                                              tags$div(
                                                                class = "panel-body",
                                                                verbatimTextOutput("residual_boxplot_stats")
                                                              )
                                                            )
                                                          )
                                                        ),
                                                        hr(),
                                                        tags$div(
                                                          class = "panel-group",
                                                          tags$div(
                                                            class = "panel panel-default",
                                                            tags$div(
                                                              class = "panel-heading",
                                                              tags$h4(
                                                                class = "panel-title",
                                                                tags$a(`data-toggle` = "collapse", href = "#collapseResidualBoxplotOutliers", 
                                                                       "Residual Outliers (by IQR method)",
                                                                       style = "cursor: pointer; text-decoration: none; color: #2C3E50;")
                                                              )
                                                            ),
                                                            tags$div(
                                                              id = "collapseResidualBoxplotOutliers",
                                                              class = "panel-collapse collapse in",
                                                              tags$div(
                                                                class = "panel-body",
                                                                DTOutput("residual_boxplot_outliers")
                                                              )
                                                            )
                                                          )
                                                        )
                                                 )
                                               )
                                      ),
                                      tabPanel("Cross-Validation", 
                                               plotlyOutput("cv_plot", height = "500px"), 
                                               hr(),
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
                                      ),
                                      tabPanel("Hyperparameter Tuning",
                                               fluidRow(
                                                 column(4,
                                                        div(class = "well",
                                                            h4("Tuning Controls"),
                                                            hr(),
                                                            h5("Alpha (Elastic Net Mix) Grid"),
                                                            sliderInput("tune_alpha_min", "Alpha Minimum:", 
                                                                        min = 0, max = 1, value = 0, step = 0.1),
                                                            sliderInput("tune_alpha_max", "Alpha Maximum:", 
                                                                        min = 0, max = 1, value = 1, step = 0.1),
                                                            numericInput("tune_alpha_steps", "Number of Alpha Values:", 
                                                                         value = 11, min = 2, max = 21),
                                                            
                                                            hr(),
                                                            
                                                            h5("Lambda Grid"),
                                                            numericInput("tune_lambda_min", "Lambda Minimum (log scale):", 
                                                                         value = -6, min = -10, max = 0, step = 0.5),
                                                            numericInput("tune_lambda_max", "Lambda Maximum (log scale):", 
                                                                         value = 2, min = -5, max = 5, step = 0.5),
                                                            numericInput("tune_lambda_steps", "Number of Lambda Values:", 
                                                                         value = 50, min = 20, max = 100),
                                                            
                                                            hr(),
                                                            
                                                            h5("Cross-Validation Settings"),
                                                            numericInput("tune_cv_folds", "Number of CV Folds:", 
                                                                         value = 5, min = 3, max = 10),
                                                            numericInput("tune_cv_seed", "Random Seed:", 
                                                                         value = 123, min = 1, max = 9999),
                                                            
                                                            hr(),
                                                            
                                                            selectInput("tune_metric", "Optimization Metric:",
                                                                        choices = c("RMSE" = "RMSE", "MAE" = "MAE", "R²" = "Rsquared"),
                                                                        selected = "RMSE"),
                                                            
                                                            hr(),
                                                            
                                                            actionButton("run_tuning", "Run Hyperparameter Tuning", 
                                                                         class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                                                            
                                                            conditionalPanel(
                                                              condition = "output.tuning_complete == true",
                                                              actionButton("use_tuned_params", "Use Tuned Parameters for Modeling", 
                                                                           class = "btn-success", style = "width: 100%;")
                                                            )
                                                        )
                                                 ),
                                                 
                                                 column(8,
                                                        tabsetPanel(
                                                          tabPanel("Tuning Results",
                                                                   br(),
                                                                   h4("Best Parameters Found"),
                                                                   hr(),
                                                                   fluidRow(
                                                                     column(4, div(class = "well", 
                                                                                   h5("Best Alpha", style = "text-align: center;"),
                                                                                   h3(textOutput("best_alpha_display"), style = "text-align: center; color: #2C3E50;"))),
                                                                     column(4, div(class = "well", 
                                                                                   h5("Best Lambda", style = "text-align: center;"),
                                                                                   h3(textOutput("best_lambda_display"), style = "text-align: center; color: #2C3E50;"))),
                                                                     column(4, div(class = "well", 
                                                                                   h5("Best Metric Value", style = "text-align: center;"),
                                                                                   h3(textOutput("best_metric_display"), style = "text-align: center; color: #2C3E50;")))
                                                                   ),
                                                                   br(),
                                                                   h5("Performance vs Alpha (at optimal lambda)"),
                                                                   shinycssloaders::withSpinner(
                                                                     plotlyOutput("tune_alpha_plot", height = "400px"),
                                                                     type = 4, color = "#2C3E50", size = 0.7
                                                                   ),
                                                                   br(),
                                                                   h5("Performance vs Lambda (at optimal alpha)"),
                                                                   shinycssloaders::withSpinner(
                                                                     plotlyOutput("tune_lambda_plot", height = "400px"),
                                                                     type = 4, color = "#2C3E50", size = 0.7
                                                                   )
                                                          ),
                                                          
                                                          tabPanel("Heatmap",
                                                                   br(),
                                                                   h5("Hyperparameter Heatmap (Alpha vs Lambda)"),
                                                                   p("Darker colors indicate better performance (lower RMSE/MAE or higher R²).",
                                                                     style = "font-size: 11px; color: #666; margin-bottom: 15px;"),
                                                                   shinycssloaders::withSpinner(
                                                                     plotlyOutput("tune_heatmap", height = "600px"),
                                                                     type = 4, color = "#2C3E50", size = 0.7
                                                                   )
                                                          ),
                                                          
                                                          tabPanel("CV Summary",
                                                                   br(),
                                                                   h5("Cross-Validation Summary"),
                                                                   hr(),
                                                                   verbatimTextOutput("tune_cv_summary"),
                                                                   br(),
                                                                   h5("Top Parameter Combinations"),
                                                                   DTOutput("tune_top_results")
                                                          )
                                                        )
                                                 )
                                               )
                                      )
                                    ) 
                          )  
                   )  
               )  
             )  
           )  
  )  
  
  
  
)  # Close navbarPage # server.R - COMPLETE VERSION