# global.R

# ============================================================================
# LIBRARY LOADING
# ============================================================================

# List of required packages
required_packages <- c(
  "shiny", "shinythemes", "shinyjs", "shinycssloaders",
  "dplyr", "ggplot2", "plotly", "DT", "tidyr",
  "corrplot", "VIM", "mice", "missForest",
  "dbscan", "isotree", "e1071", "randomForest",
  "glmnet", "rpart", "rpart.plot",
  "caret", "summarytools", "tabplot", "ComplexUpset",
  "rlang", "shinyWidgets"
)

# Check and install missing packages
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing missing package:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  } else {
    cat("✓", pkg, "\n")
  }
}

cat("\nAll packages ready! Launching app...\n")  


# ============================================================================
# DATASET LOADING - PRESERVE ALL ORIGINAL VALUES
# ============================================================================

# Load the dataframe with NO automatic NA conversion
df_original <- read.csv("Ass2Data.csv", 
                        header = TRUE, 
                        stringsAsFactors = FALSE,
                        na.strings = NULL)

# ============================================================================
# DEFINE COLUMN TYPES
# ============================================================================

# Define numeric columns
numeric_cols <- c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", 
                  "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE", 
                  "HEALTHCARE_COST", "DEATH_RATE")

# Define categorical columns
categorical_cols <- c("CODE", "GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE")

# All columns
all_cols <- names(df_original)

# ============================================================================
# CREATE WORKING COPY
# ============================================================================

# Create a working copy for the app (starts as identical to original)
df <- df_original

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

classify_missing_type <- function(x) {
  if(is.na(x)) return("R_NA")
  x_char <- as.character(x)
  if(x_char == "-99") return("NEG99")
  if(x_char == "--") return("DASH")
  if(x_char == "NA") return("STRING_NA")
  return("PRESENT")
}

get_missing_counts <- function(data, var) {
  vals <- as.character(data[[var]])
  data.frame(
    Variable = var,
    NEG99 = sum(vals == "-99", na.rm = TRUE),
    DASH = sum(vals == "--", na.rm = TRUE),
    STRING_NA = sum(vals == "NA", na.rm = TRUE),
    R_NA = sum(is.na(vals)),
    PRESENT = sum(!vals %in% c("-99", "--", "NA") & !is.na(vals)),
    Total = length(vals)
  )
}