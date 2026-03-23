
library(shiny)
library(shinyjs)
library(bslib)
library(ggplot2)
# install.packages("DT")
library(DT)
library(dplyr)
# install.packages("psych")
library(psych)
# install.packages("gtsummary")
library(gtsummary)
# install.packages("vtable")
library(vtable)
library(tidyr)
library(readr)
library(lubridate)
library(summarytools)
library(corrgram)
library(visdat)
library(shinyWidgets)
library(GGally)
library(vcd)
library(openxlsx)
library(haven)
library(sass)
library(plotly)
library(rsconnect)
library(corrplot)
library(naniar)
library(rpart)
library(rpart.plot)
library(VIM)


#Data without any changes made to it
data_og <-  read.csv('Ass2Data.csv',
                     stringsAsFactors = TRUE)

#Reading in dataset
data <- read.csv('Ass2Data.csv',
                  stringsAsFactors = TRUE)

#Addressing null categorical values
data[data == "--"] <- NA

#ANy numeric variable value < 0 turned to NULL 
data[data < -1] <- NA

# # convert away from factor
# data$GOVERN_TYPE <-  as.character(data$GOVERN_TYPE)
# data$GOVERN_TYPE[is.na(data$GOVERN_TYPE)] <-  "none"
# # convert back to factor
# data$GOVERN_TYPE <- as.factor(data$GOVERN_TYPE)

#Shadow dataframe with binary values for missing (0)/present (1) values
data_shadow <- as.data.frame(
  lapply(data, function(x) as.numeric(is.na(x)))
)

#Sum total of missing values for each row
data_shadow$sum_missing <- rowSums(data_shadow)
 



# Global datatype colours to be used in server/ui
datatype_colours <- c(
  "factor"    = "#86cfa5",
  "numeric"   = "#8fbce6",
  "integer"   = "#8fbce6",
  "Date"      = "#c2a5e2",
  "ordered\nfactor" = "#b7e4c7"
)

#Datatype colours if distinct colour input not selected
datatype_colours_grey <- c(
  "character" = "grey",
  "factor"    = "grey",
  "numeric"   = "grey",
  "integer"   = "grey",
  "Date"      = "grey",
  "ordered\nfactor" = "grey"
)


#Object for selecting variables in ui
cat_vars <- data[, sapply(data, is.factor) & names(data) != "OBS_TYPE"]

non_cat_vars <- data[, (sapply(data, is.numeric))]

cat_filter_cols <- names(cat_vars)
num_filter_cols <- names(non_cat_vars)

#Chart console for data processing plots
chart_console <- function(...) {
  div(
    style = "
      background-color: #f8f9fa;
      padding: 15px;
      border-radius: 10px;
      margin-top: 10px;
      border: 1px solid #dee2e6;
    ",
    ...
  )
}


# =================================================================================
#   Rising Value Chart Inputs
# ================================================================================

select_variables_numeric_rising <- pickerInput(
  inputId = "selected_vars_numeric_rising",
  label = "Numeric Variables",
  choices = names(non_cat_vars),
  selected = names(non_cat_vars),
  multiple = FALSE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)

# =================================================================================
#   Missing Value Chart Inputs
# ================================================================================

select_variables_numeric_missing <- pickerInput(
  inputId = "selected_vars_numeric_missing",
  label = "Numeric Variables",
  choices = names(non_cat_vars),
  selected = names(non_cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)
select_variables_categorical_missing <- pickerInput(
  inputId = "selected_vars_categorical_missing",
  label = "Categorical Variables",
  choices = names(cat_vars),
  selected = names(cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,   # adds Select All / Deselect All
    `live-search` = TRUE    # search bar
  )
)
# =================================================================================
#   GGPAIRS Inputs 
# ================================================================================

select_variables_numeric_ggpairs <- pickerInput(
  inputId = "selected_vars_numeric_ggpairs",
  label = "Numeric Variables",
  choices = names(non_cat_vars),
  selected = names(non_cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)
select_variables_categorical_ggpairs <- pickerInput(
  inputId = "selected_vars_categorical_ggpairs",
  label = "Categorical Variables",
  choices = names(cat_vars[names(cat_vars) != "CODE", drop = FALSE]), #Not allowing CODE as an option for ggPairs becauses thats the unique ID
  selected = c("GOVERN_TYPE", "HEALTHCARE_BASIS"),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)
# =================================================================================
#   Boxplot Inputs 
# ================================================================================

select_variables_numeric_boxplot <- pickerInput(
  inputId = "selected_vars_numeric_boxplot",
  label = "Numeric Variables",
  choices =  names(non_cat_vars),
  selected = names(non_cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)

select_variables_categorical_boxplot <- pickerInput(
  inputId = "selected_vars_categorical_boxplot",
  label = "Split Apart By",
  choices = names(cat_vars[names(cat_vars) != "CODE", drop = FALSE]), #Not allowing CODE as an option for boxplot becauses thats the unique ID
  selected = NULL,
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)

# =================================================================================
#   Mosaic Inputs 
# ================================================================================

select_variables_mosaic_x <- pickerInput(
  inputId = "selected_vars_mosaic_x",
  label = "Select X-Axis Variable",
  choices = names(cat_vars),
  selected = NULL,
  multiple = FALSE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)

select_variables_mosaic_y <- pickerInput(
  inputId = "selected_vars_mosaic_y",
  label = "Select Y-Axis Variable",
  choices = names(cat_vars),
  selected = NULL,
  multiple = FALSE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)

select_variables_mosaic_z <- pickerInput(
  inputId = "selected_vars_mosaic_z",
  label = "Select 3rd Variable (Optional)",
  choices = c("None", names(cat_vars)),
  selected = "None",
  multiple = FALSE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
  
)
select_variables_mosaic_xyz <- pickerInput(
  inputId = "selected_vars_mosaic_xyz",
  label = "Addtional Variables (Optional - Not Recommended)",
  choices = names(cat_vars),
  selected = NULL,
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)
# =================================================================================
#   Data Table Export 
# ================================================================================

select_variables_numeric_data <- pickerInput(
  inputId = "selected_vars_numeric_data",
  label = "Numeric Variables",
  choices = names(non_cat_vars),
  selected = names(non_cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)



select_variables_categorical_data <- pickerInput(
  inputId = "selected_vars_categorical_data",
  label = "Categorical Variables",
  choices = names(cat_vars),
  selected = names(cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)

# =================================================================================
#   Scatterpplot
# ================================================================================
selected_x_scatter <- pickerInput(
  inputId = "selected_x_scatter",
  label = "X-Axis Variable",
  choices = names(non_cat_vars),
  selected = "GDP",
  multiple = FALSE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)

selected_y_scatter <- pickerInput(
  inputId = "selected_y_scatter",
  label = "Y-Axis Variable",
  choices = names(non_cat_vars),
  selected = "POPULATION",
  multiple = FALSE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)

# =================================================================================
#   PROCESSING OF MISSING VALUES 
# ================================================================================

select_variables_numeric_missing_processing <- pickerInput(
  inputId = "selected_vars_numeric_missing_processing",
  label = "Numeric Variables",
  choices = names(non_cat_vars),
  selected = names(non_cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)



select_variables_categorical_missing_processing <- pickerInput(
  inputId = "selected_vars_categorical_missing_processing",
  label = "Categorical Variables",
  choices = names(cat_vars),
  selected = names(cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)
# =================================================================================
#   PROCESSING OF OUTLIER VALUES 
# ================================================================================

select_variables_numeric_outlier_processing <- pickerInput(
  inputId = "selected_vars_numeric_outlier_processing",
  label = "Numeric Variables",
  choices = NULL,
  selected = NULL,
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)

select_variables_categorical_outlier_processing <- pickerInput(
  inputId = "selected_vars_categorical_outlier_processing",
  label = "Categorical Variables",
  choices = NULL,
  selected = NULL,
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)

select_variables_impute_missing <- pickerInput(
  inputId = "selected_vars_impute_missing",
  label = "Select Variable",
  choices = NULL,
  selected = NULL,
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)

select_variables_transform <- pickerInput(
  inputId = "selected_vars_transform",
  label = "Select Variable",
  choices = NULL,
  selected = NULL,
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)