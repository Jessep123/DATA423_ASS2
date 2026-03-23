shinyUI(
  page_navbar(
    title = "DATA423 Assignment 2 - Jesse Pilcher",
    
    #Setting theme and font
    theme = bs_theme(
      bootswatch = "flatly",
      base_font = "Times New Roman"
      ),
    
    useShinyjs(),
    

    tabPanel('Introduction',

             ),
    
    navbarMenu("Exploratory Data Analysis",
               
    
    tabPanel("Summary",
             h3("Data Summary"),
             uiOutput('variable_summary'),
             accordion(open = FALSE,
               accordion_panel('Numeric Variables Summary', htmlOutput('numeric_summary')),
               accordion_panel('Categorical Variables Summary', htmlOutput('categorical_summary'))
               )
    ),

    tabPanel("Missing Values",
             h3("Missing Values"),
             layout_sidebar(
               
               sidebar = sidebar(
                 title = '',
                 actionButton("reset_input_missing","Reset Inputs"),
                 sliderInput(
                   "missing_threshold",
                   "Missing Values per Row Threshold",
                   min = 0,
                   max = 9,
                   value = 0,
                   step = 1
                 ),
                 
                 conditionalPanel(
                   condition = "input.missing_threshold > 0",
                   checkboxInput(
                     "missing_order_na_row_sum",
                     "Order Table by Missing Values per Row",
                     value = FALSE
                   )
                 ),
                 
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_numeric_missing,
                             select_variables_categorical_missing),
                           
                           accordion_panel(
                             "Filter Categorical Variables",
                             uiOutput("dynamic_filters_categorical_missing"),
                           ),
                           accordion_panel(
                             "Filter Numeric Variables",
                             uiOutput("dynamic_filters_numeric_missing"),
                           )
                          ),
                 actionButton("reset_filter_input_missing", "Reset Filters"),
                 
                 checkboxInput("distinct_datatypes_missing",
                               "Display Distinct Datatypes",
                               value = FALSE),
                 
                 open = "open",
                 width = "350px"),
               
              plotOutput("missing_data")
             )

    ),
    
    tabPanel("Scatter Plot",
             h3("Scatter Plot"),
             layout_sidebar(
               sidebar = sidebar(
                 title = '',
                 actionButton("reset_input_counts_over_time", "Reset Inputs"),
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             selected_x_scatter,
                             selected_y_scatter
                           ),
                           accordion_panel(
                             "Filter Categorical Variables",
                             uiOutput("dynamic_filters_categorical_counts_over_time"),
                           ),
                           accordion_panel(
                             "Filter Numeric Variables",
                             uiOutput("dynamic_filters_numeric_counts_over_time"),
                           ),
                           actionButton("reset_filter_input_counts_over_time", "Reset Filters")
                 ),
                 
                 pickerInput(
                   inputId = "counts_colourby",
                   label = "Colour By",
                   choices = c("None", names(cat_vars)),
                   selected = NULL,
                   multiple = FALSE,
                   options = list(
                     `actions-box` = TRUE,  
                     `live-search` = TRUE    
                   )
                 ),

                 
                 open = "open",
                 width = "350px"),
               
               plotlyOutput("counts_over_time")
             )
    ),
    
    
    tabPanel("Rising Order",
             h3("Rising Value Chart"),
             layout_sidebar(
               sidebar = sidebar(
                 title = '',
                 actionButton("reset_input_rising", "Reset Inputs"),
                 sliderInput("jump_threshold", 
                             "Jump Threshold",
                             min = 0.1,
                             max = 2,
                             value = 0.5),
                 checkboxInput("center_data", 
                               "Center Data",
                               value = TRUE),
                 checkboxInput("scale_data", 
                               "Scale Data",
                               value = TRUE),
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_numeric_rising),
                           accordion_panel(
                             "Filter Categorical Variables",
                             uiOutput("dynamic_filters_categorical_rising"),
                           ),
                           accordion_panel(
                             "Filter Numeric Variables",
                             uiOutput("dynamic_filters_numeric_rising"),
                           )
                 ),
                 actionButton("reset_filter_input_rising", "Reset Filters"),
                 
                 
                 open = "open",
                 width = "350px"),
               
               plotOutput("rising_value")

             )
    ),
    
    tabPanel("Correlations",
             h3("Correlations"),
             layout_sidebar(
               sidebar = sidebar(
                 actionButton("reset_input_corr", "Reset Inputs"),
                 radioButtons("corr_method",
                              "Correlation Type",
                              c("Pearson" = "pearson",
                                "Spearman" = "spearman",
                                "Kendall" = "kendall"
                              )),
                 radioButtons("corr_order",
                              "Order Variables:",
                              c(
                                "Original" = "original",
                                "AOE",
                                "FPC",
                                "Heirarchical Clustering" = "hclust",
                                "Alphabetically" = "alphabet")
                              
                 ),
                 radioButtons("corr_absolute",
                              "Use Absolute Values?",
                              c("No" = FALSE,
                                "Yes" = TRUE)
                              ),
                  radioButtons("corr_display",
                                "Display Values?",
                                c("No",
                                 "Yes")
                 ),
                 open = "open",
                 width = "350px"
               ),
               plotOutput("corr_chart")             
               
             )
    ),
    
    
    tabPanel("Boxplot",
             h3("Boxplot"),
             layout_sidebar(
               sidebar = sidebar(
                 title = '',
                 actionButton("reset_input_boxplot", "Reset Input"),
                 sliderInput("iqr_boxplot", 
                             "Interquartile Range Multiplier",
                             min = 0.1,
                             max = 3,
                             value = 1.5),
                 checkboxInput("center_data_boxplot", 
                               "Center Data",
                               value = FALSE),
                 checkboxInput("scale_data_boxplot", 
                               "Scale Data",
                               value = FALSE),
                 
                 conditionalPanel(
                   condition = "input.selected_vars_numeric_boxplot.length == 1",
                   select_variables_categorical_boxplot
                 ),
                 
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_numeric_boxplot),
                           accordion_panel(
                             "Filter Categorical Variables",
                             uiOutput("dynamic_filters_categorical_boxplot"),
                           ),
                           accordion_panel(
                             "Filter Numeric Variables",
                             uiOutput("dynamic_filters_numeric_boxplot"),
                           ),

                 ),
                 actionButton("reset_filter_input_boxplot", "Reset Filters"),
                 
                 
                 open = "open",
                 width = "350px"),
               
               plotlyOutput("boxplot")
             )
             
    ),
    
    
    tabPanel("ggPairs Plot",
             h3("ggPairs"),
             layout_sidebar(
               sidebar = sidebar(
                 title = '',
                 actionButton("reset_input_ggpairs", "Reset Inputs"),
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_numeric_ggpairs,
                             select_variables_categorical_ggpairs
                           ),
                           accordion_panel(
                             "Filter Categorical Variables",
                             uiOutput("dynamic_filters_categorical_ggpairs"),
                           ),
                           accordion_panel(
                             "Filter Numeric Variables",
                             uiOutput("dynamic_filters_numeric_ggpairs"),
                           ),

                 ),
                 actionButton("reset_filter_input_ggpairs", "Reset Filters"),
                 
                 pickerInput(
                   inputId = "ggpairs_colourby",
                   label = "Colour By",
                   choices = c("None", names(cat_vars)),
                   selected = "Operator",
                   multiple = FALSE,
                   options = list(
                     `actions-box` = TRUE,  
                     `live-search` = TRUE    
                   )
                 ),
                 open = "open",
                 width = "350px"),
               
               
               plotOutput("ggpairs")
             )
    ),
    
    tabPanel("Mosaic Chart",
             h3("Mosaic"),
             layout_sidebar(
               sidebar = sidebar(
                 title = '',
                 actionButton("reset_input_mosaic", "Reset Inputs"),
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_mosaic_x,
                             select_variables_mosaic_y,
                             select_variables_mosaic_z,
                             select_variables_mosaic_xyz
                             ),
                           accordion_panel(
                             "Filter Categorical Variables",
                             uiOutput("dynamic_filters_categorical_mosaic"),
                           ),
                           accordion_panel(
                             "Filter Numeric Variables",
                             uiOutput("dynamic_filters_numeric_mosaic"),
                           )
                 ),
                 actionButton("reset_filter_input_mosaic", "Reset Filters"),
                 
                 
                 open = "open",
                 width = "350px"),
               
               plotOutput("mosaic")
             )
             )
    ),
    
    navbarMenu("Data Processing",
               tabPanel("Introduction"), #Fill this out with info explaining processing module
               tabPanel("Missing Values",
                        value = "missing_tab",
                        layout_sidebar(
                          sidebar = sidebar(
                            title = "Fuck around and find out I guess",
                            
                            actionButton("reset_input_missing_processing","Reset Inputs"),
                            sliderInput(
                              "missing_threshold_processing",
                              "Missing Values per Row Threshold Range",
                              min = 0,
                              max = 9,
                              value = c(0, 9),
                              step = 1
                            ),
                            
                            p("Rows with # values missing above this threshold will be removed", style = "color:red;"),
                            
                            
                            sliderInput(
                              "missing_col_threshold_processing",
                              "Variable % Missing Threshold Range",
                              min = 0,
                              max = 100,
                              value = c(0, 100),
                              step = 1,
                              post = "%"
                            ),
                            
                            p("Columns with % values missing above this threshold will be removed", style = "color:red;"),
                            

                            accordion(open = FALSE,
                                      
                                      #For selecting variables to be in dataset
                                      accordion_panel(
                                        "Select Variables",
                                        select_variables_numeric_missing_processing,
                                        select_variables_categorical_missing_processing
                                      ),
                                      
                                      #For filtering variables in dataset
                                      accordion_panel(
                                        "Filter Categorical Variables",
                                        uiOutput("dynamic_filters_categorical_missing_processing")
                                                      ),
                                      accordion_panel(
                                        "Filter Numeric Variables",
                                        uiOutput("dynamic_filters_numeric_missing_processing")
                                                      )
                                    ),
                            
                            actionButton("reset_filter_input_missing_processing", "Reset Filters")
                            
                            
                            ),
                        tabsetPanel( id = "missing_processing_tabs",
                          tabPanel("Missing Values Chart",
                                   value = "missing_plot_tab",
                                   plotOutput("missing_data_processing", height = "600px"),
                                   accordion( open = FALSE,
                                     accordion_panel("Chart Controls", 
                                         chart_console(
                                           fluidRow(
                                             column(4,
                                                    checkboxInput("distinct_datatypes_missing_processing",
                                                                  "Display Datatypes",
                                                                  value = FALSE)
                                             ),
                                             column(4,
                                                    checkboxInput("missing_order_na_row_sum_processing",
                                                                  "Order by Missing Count",
                                                                  value = FALSE)
                                             ),
                                             column(4,
                                                    div(
                                                      style = "display: flex; justify-content: flex-end;",
                                                      actionButton("reset_missing_plot", "Reset Plot Adjustments")
                                                    )
                                                    )
                                           )
                                         ))
                                   )
                                   # DTOutput("reactive_table")
                                   ),
                          tabPanel("gg_miss_upset",
                                   value = "gg_miss_tab",
                                   
                                   plotOutput("gg_miss"),
                                   
                                   accordion( open = FALSE,
                                     accordion_panel("Chart Controls", 
                                                     chart_console(
                                                       fluidRow(
                                                         column(3,
                                                                numericInput(
                                                                  "gg_miss_nsets",
                                                                  "Number of Sets",
                                                                  value = 5, 
                                                                  min = 1, 
                                                                  max = 10 
                                                                            )
                                                            
                                                         ),
                                                         column(3,
                                                                numericInput(
                                                                  "gg_miss_nintersects",
                                                                  "Number of Intersections",
                                                                  value = 40, 
                                                                  min = 1, 
                                                                  max = 100 
                                                                            )
                                                                  
                                                         ),
                                                         
                                                         column(3,
                                                                pickerInput(
                                                                  inputId = "order_by_gg_miss",
                                                                  label = "Order By",
                                                                  choices = c("Frequency" = "freq", "Degree" = "degree", "Both"),
                                                                  selected = "Frequency",
                                                                  multiple = FALSE,
                                                                  options = list(
                                                                    `actions-box` = TRUE,
                                                                    `live-search` = TRUE
                                                                  )
                                                                )
                                                                
                                                         ),
                                                         column(3,
                                                                div(
                                                                  style = "display: flex; justify-content: flex-end;",
                                                                  actionButton("reset_gg_miss_plot", "Reset Plot Adjustments")
                                                                    )
                                                         )
                                                       )
                                                     )
                                               )
                                             )
                                   ),
                          tabPanel("rpart",
                                   value = "rpart_tab",
                                   
                                   plotOutput("rpart", height = "600px"),
                                   
                                   accordion(open = FALSE,
                                     accordion_panel("Chart Controls", 
                                                     chart_console(
                                                       fluidRow(
                                                         column(4, sliderInput("rpart_maxdepth", "Max Depth", 1, 10, 5)),
                                                         column(4, sliderInput("rpart_cp", "Pruning (cp)", 0.001, 0.1, 0.01, step = 0.01)),
                                                         column(4, sliderInput("rpart_minsplit", "Min Split", 10, 100, 20))
                                                       ),
                                                       
                                                       fluidRow(
                                                         column(4, selectInput("rpart_type", "Layout", c("Standard"=2, "Vertical"=3))),
                                                         column(4, pickerInput("rpart_exclude_vars", "Exclude Variables",
                                                                               choices = NULL, multiple = TRUE)),
                                                         selectInput("rpart_target_type", "Target Type",
                                                                     choices = c("# Missing Values", "Binary (Missing/Not Missing)"),
                                                                     selected = "# Missing Values")

                                                       ),
                                                       fluidRow(
                                                         column(4, checkboxInput("rpart_extra", "Show Extra Info", TRUE)),
                                                         
                                                         column(
                                                           4,
                                                           radioButtons(
                                                             "rpart_plot_mode",
                                                             "Display",
                                                             choices = c("Tree" = "tree", "Variable Importance" = "varimp"),
                                                             selected = "tree",
                                                             inline = TRUE
                                                           )
                                                         ),
                                                         
                                                         conditionalPanel(
                                                           condition = "input.rpart_target_type == 'Binary (Missing/Not Missing)'",
                                                         column(
                                                           4,
                                                           pickerInput("rpart_binary_predict", "Columns to Predict",
                                                                       choices = NULL, multiple = TRUE)
                                                         )
                                                       ),
                                                         
                                                         column(4,
                                                                div(
                                                                  style = "display: flex; justify-content: flex-end;",
                                                                  actionButton("reset_rpart_plot", "Reset Plot Adjustments")
                                                                )
                                                         )
                                                       )
                                                     )                                  
                                                     )
                                   )
                                   
                                   ),
                          tabPanel("Missing Values Correlation",
                                   value = "corr_missing_tab",
                                   
                                   plotOutput("missing_corr_plot", height = "600px"),
                                   
                                   accordion(open = FALSE,
                                             accordion_panel("Chart Controls", 
                                                             chart_console(
                                                               fluidRow(
                                                                 
                                                                 column(3,
                                                                        pickerInput(
                                                                          inputId = "corr_missing_method",
                                                                          label = "Correlation Type",
                                                                          choices = c(
                                                                            "Pearson" = "pearson",
                                                                            "Spearman" = "spearman",
                                                                            "Kendall" = "kendall"
                                                                          ),
                                                                          selected = "pearson",
                                                                          multiple = FALSE,
                                                                          options = list(
                                                                            `live-search` = TRUE
                                                                          )
                                                                        )
                                                                 ),
                                                                 
                                                                 column(3,
                                                                        pickerInput(
                                                                          inputId = "corr_missing_order",
                                                                          label = "Order Variables",
                                                                          choices = c(
                                                                            "Original" = "original",
                                                                            "AOE" = "AOE",
                                                                            "FPC" = "FPC",
                                                                            "Hierarchical Clustering" = "hclust",
                                                                            "Alphabetical" = "alphabet"
                                                                          ),
                                                                          selected = "original",
                                                                          multiple = FALSE,
                                                                          options = list(
                                                                            `live-search` = TRUE
                                                                          )
                                                                        )
                                                                 ),
                                                                 
                                                                 column(2,
                                                                        checkboxInput("corr_missing_absolute", "Absolute", value = FALSE),
                                                                        checkboxInput("corr_missing_display", "Show Values", value = FALSE)
                                                                 ),
                                                                 
                                                                 column(4,
                                                                        div(
                                                                          style = "display: flex;justify-content: flex-end;align-items: center;height: 100%;",
                                                                          actionButton(
                                                                            "reset_corr_missing_plot",
                                                                            "Reset"
                                                                          )
                                                                        )
                                                                 )
                                                               )
                                                             )                                  
                                             )
                                   )
                                   
                          ),
                          tabPanel("Data Table Export",
                                   value = "data_tab",
                                   div(
                                     style = "margin-bottom: 24px;",
                                     DTOutput("missing_processing_reactive_table")
                                   ),
                                  accordion(open = FALSE,
                                            accordion_panel("Export Options", 
                                                            chart_console(
                                                              fluidRow(
                                                                downloadButton('data_export_csv_missing_processed', 'Export as .csv'),
                                                                
                                                                downloadButton('data_export_xlsx_missing_processed', 'Export as .xlsx'),
                                                                
                                                                downloadButton('data_export_spss_missing_processed', 'Export as .sav (for SPSS)'),
                                                                
                                                                downloadButton('data_export_tsv_missing_processed', 'Export as .tsv'),
                                                                
                                                                downloadButton('data_export_rds_missing_processed', 'Export as .rds (R Data File)')
                                                              ))))
                                                              ) 
                          )   
                        )
                  ),
               tabPanel("Outliers",
                        value = "outlier_tab",
                        layout_sidebar(
                          sidebar = sidebar(
                            title = "Fuck around and find out I guess - outlier edition",
                            
                            actionButton("reset_input_outlier_processing","Reset Inputs"),

                            accordion(open = FALSE,
                                      
                                      #Controls to impute/adjust missing values
                                      accordion_panel(
                                        "Impute Missing Values",
                                        select_variables_impute_missing,
                                        conditionalPanel(
                                          condition = "input.missing_imputate_method == 'KNN'",
                                          p("NOTE - KNN Requires Multiple Columns", style = "color:red;")
                                        ),
                                        pickerInput(inputId = "missing_imputate_method",
                                                    label = "Select Imputation Method",
                                                    choices = c("Manual", "KNN", "Median","Mean"),
                                                    selected = NULL,
                                                    multiple = FALSE),
                                        conditionalPanel(
                                          condition = "input.missing_imputate_method == 'Manual'",
                                          numericInput(
                                            "missing_impute_manual_value",
                                            "Value to Replace NA",
                                            value = 0
                                          )
                                        ),
                                          conditionalPanel(
                                            condition = "input.missing_imputate_method == 'KNN'",
                                            numericInput(
                                              "knn_neighbours",
                                              "Number of k Nearest Neighbours",
                                              value = 5
                                            )
                                        ),
                                        actionButton("impute_missing_values", "Apply"),
                                        actionButton("reset_missing_imputes", "Remove All Imputes")
                                      ),
                                      
                                      accordion_panel(
                                        "Transform Variables",
                                        select_variables_transform,
                                        pickerInput(inputId = "transform_variable_method",
                                                    label = "Select Transformation Method",
                                                    choices = c("Box-Cox", "Yeo-Johnson"),
                                                    selected = NULL,
                                                    multiple = FALSE),
                                        actionButton("transform_variables", "Apply"),
                                        actionButton("reset_transform_variables", "Remove All Transformations")
                                      ),

                                      # #For selecting variables to be in dataset
                                      # accordion_panel(
                                      #   "Select Variables",
                                      #   select_variables_numeric_outlier_processing,
                                      #   select_variables_categorical_outlier_processing
                                      # ),
                                      
                                      #For filtering variables in dataset
                                      accordion_panel(
                                        "Filter Categorical Variables",
                                        uiOutput("dynamic_filters_categorical_outlier_processing")
                                      ),
                                      accordion_panel(
                                        "Filter Numeric Variables",
                                        uiOutput("dynamic_filters_numeric_outlier_processing")
                                      )
                            ),
                            
                            actionButton("reset_filter_input_outlier_processed", "Reset Filters")
                            
                            
                          ),#end of sidebar()
                          
                          tabsetPanel(
                          tabPanel("Variable Density",
                                   plotOutput("outlier_density_plot")),
                            accordion(open = FALSE,
                                      accordion_panel("Chart Controls", 
                                                      chart_console(
                                                        fluidRow(
                                                          column(4,
                                                                 pickerInput(
                                                                   inputId = "selected_vars_numeric_outlier_density_plot",
                                                                   label = "Variables to Plot",
                                                                   choices = NULL,
                                                                   selected = NULL,
                                                                   multiple = TRUE,
                                                                   options = list(
                                                                     `actions-box` = TRUE,
                                                                     `live-search` = TRUE
                                                                   )
                                                                 )),
                                                          column(4,
                                                                 pickerInput(
                                                            inputId = "outlier_density_colourby",
                                                            label = "Colour By (Optional)",
                                                            choices = c("None", "GOVERN_TYPE", "HEALTHCARE_BASIS"),
                                                            selected = "None",
                                                            multiple = FALSE,
                                                            options = list(
                                                              `live-search` = TRUE
                                                            )
                                                          )),
                                                          column(4,
                                                                 div(
                                                                   style = "display: flex;justify-content: flex-end;align-items: center;height: 100%;",
                                                                   actionButton(
                                                                     "reset_density_outlier_plot",
                                                                     "Reset"
                                                                   )
                                                                 )
                                                          )
                                                        )))),
                          tabPanel("Data Table Export",
                                   div(
                                     style = "margin-bottom: 24px;",
                                     DTOutput("outlier_processing_reactive_table")
                                   ),
                                   accordion(open = FALSE,
                                             accordion_panel("Export Options", 
                                                             chart_console(
                                                               fluidRow(
                                                                 downloadButton('data_export_csv_outlier_processed', 'Export as .csv'),
                                                                 
                                                                 downloadButton('data_export_xlsx_outlier_processed', 'Export as .xlsx'),
                                                                 
                                                                 downloadButton('data_export_spss_outlier_processed', 'Export as .sav (for SPSS)'),
                                                                 
                                                                 downloadButton('data_export_tsv_outlier_processed', 'Export as .tsv'),
                                                                 
                                                                 downloadButton('data_export_rds_outlier_processed', 'Export as .rds (R Data File)')
                                                               ))))
                          )
                          
                          
                          ) #End of tabset panel
                        )#End of layout_sidebar
                        
                      ),#End of outlier tab panel
               
               
               tabPanel("Data Processing"),
               
               
               tabPanel("GLM Net Model")
               
    ),


             
    tabPanel("Export Data",
             h3("Export Data"),
             layout_sidebar(
               sidebar = sidebar(
                 title = 'Adjust Export',
                 actionButton("reset_input_data", "Reset Inputs"),
                 checkboxInput("center_data_export", 
                               "Center Numeric Data",
                               value = FALSE),
                 checkboxInput("scale_data_export", 
                               "Scale Numeric Data",
                               value = FALSE),
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_categorical_data,
                             select_variables_numeric_data),
                           accordion_panel(
                             "Filter Categorical Variables",
                             uiOutput("dynamic_filters_categorical_data"),
                           ),
                           accordion_panel(
                             "Filter Numeric Variables",
                             uiOutput("dynamic_filters_numeric_data"),
                           ),
                           actionButton("reset_filter_input_data", "Reset Filters"),
                           
                           accordion_panel(
                             "Export Data",
                             downloadButton('data_export_csv', 'Export as .csv'),
                             
                             downloadButton('data_export_xlsx', 'Export as .xlsx'),
                             
                             downloadButton('data_export_spss', 'Export as .sav (for SPSS)'),
                             
                             downloadButton('data_export_tsv', 'Export as .tsv'),
                             
                             downloadButton('data_export_rds', 'Export as .rds (R Data File)')
                           )
                 ),
                 open = "open",
                 width = "350px"),
                 DTOutput("data")
             )
          ),
    
    nav_spacer(),
    nav_item(
      actionButton("reset_input_all", 
                   "Reset Inputs Across All Tabs",
                   class = "btn btn-danger btn-sm")
    )  )
)
