# setup 
# download and load required packages
if (!require(tidyverse)) {
  install.packages("tidyverse")
}
library(tidyverse)


#-----------------------------------------------------------------------------
#load data sets

tryCatch({
  raw_data <- read_csv("UNSW-NB15_uncleaned.csv")
  
  features <- read_csv("6. NUSW-NB15_features (data description).csv")
  
  cat("SUCCESS: Datasets loaded successfully.\n")
  
}, error = function(e) {
  cat("ERROR: Could not read the CSV files.\n")
  cat("Please ensure 'UNSW-NB15_uncleaned.csv' and '6. NUSW-NB15_features (data description).csv' are in your R working directory.\n")
  cat("R Error Message:", e$message, "\n")
})


#
# --- SECTION 3: INITIAL DATA INSPECTION & EXPLORATION ---
#

# 3.1: Get a first look at the raw data
# This helps us understand its structure and identify immediate issues.
cat("\n--- Initial Data Inspection ---\n")
cat("\nDimensions of the raw dataset (rows, columns):\n")
print(dim(raw_data))

cat("\nFirst 6 rows of the raw dataset:\n")
print(head(raw_data))

cat("\nStructure of the raw dataset:\n")
# The str() function gives us a compact overview of the data types and first few values of each column.
str(raw_data, list.len = 5) # Limiting output to first 5 columns for brevity

cat("\nSummary of the raw dataset:\n")
# summary() provides basic descriptive statistics for each column.
# This can help us spot strange values (e.g., negative counts) and missing data (NAs).
print(summary(raw_data))

# From the initial inspection, we can see several issues:
# 1. Many columns that should be numeric were read as character type ('chr').
#    This is due to non-numeric characters like '?' and '_' in the data.
# 2. There are missing values (NA).
# 3. The column names from the CSV are valid, but we should ensure they match our expectations.


#
# --- SECTION 4: DATA CLEANING AND PRE-PROCESSING ---
#
# As per the assignment, all cleaning must be done via scripting.

cat("\n--- Starting Data Cleaning and Pre-processing ---\n")

# Make a copy of the raw data to work on, preserving the original.
cleaned_data <- raw_data

# 4.1: Clean Non-Numeric Characters from columns that should be numeric
# We'll create a function to remove any character that is not a digit or a decimal point.
clean_numeric_column <- function(column) {
  # The gsub function substitutes parts of a string.
  # The pattern '[^0-9.]' matches any character that is NOT a digit (0-9) or a period (.).
  # We replace these matched characters with an empty string "".
  cleaned_column <- gsub("[^0-9.]", "", column)
  # After cleaning, we convert the column to the numeric data type.
  return(as.numeric(cleaned_column))
}

# Identify columns that are character type but should be numeric.
# Based on the feature description and initial inspection, most columns are numeric.
# Categorical columns are: 'proto', 'service', 'state', 'attack_cat'.
# We will clean 'id' and 'label' as numeric first, then convert 'label' to a factor.
cols_to_clean <- setdiff(names(cleaned_data), c("proto", "service", "state", "attack_cat"))

# Apply the cleaning function to all identified columns.
cleaned_data <- cleaned_data %>%
  mutate(across(all_of(cols_to_clean), clean_numeric_column))

cat("\nSUCCESS: Removed non-numeric characters and converted columns to numeric type.\n")


# 4.2: Handle Missing Values (NA)
# The assignment requires a clean dataset. First, let's see how many NAs we have.
cat("\nTotal missing values (NA) before handling:\n")
print(sum(is.na(cleaned_data)))

cat("\nRows with missing values before handling:\n")
print(sum(!complete.cases(cleaned_data)))

# For this assignment, a straightforward approach is to remove all rows containing any NA values.
# This ensures that all subsequent analyses are run on a complete dataset.
# NOTE for report: You can discuss that other strategies like imputation exist,
# but for this context, removal is chosen for simplicity and data integrity.
cleaned_data <- cleaned_data %>%
  na.omit()

cat("\nSUCCESS: Removed rows with missing values.\n")
cat("Total missing values (NA) after handling:", sum(is.na(cleaned_data)), "\n")


# 4.3: Remove Duplicate Rows
# The assignment explicitly states that no duplication is allowed.
cat("\nNumber of rows before removing duplicates:", nrow(cleaned_data), "\n")

cleaned_data <- cleaned_data %>%
  distinct()

cat("SUCCESS: Removed duplicate rows.\n")
cat("Number of rows after removing duplicates:", nrow(cleaned_data), "\n")


# 4.4: Convert Categorical Columns to Factors
# Converting character columns to factors is good practice in R for statistical modeling and visualization.
# It makes them easier to work with and can improve performance.
categorical_cols <- c("proto", "service", "state", "attack_cat", "label")
cleaned_data <- cleaned_data %>%
  mutate(across(all_of(categorical_cols), as.factor))

cat("\nSUCCESS: Converted categorical columns to factor type.\n")


#
# --- SECTION 5: DATA VALIDATION ---
#
# This step involves checking for logical inconsistencies in the cleaned data.

cat("\n--- Starting Data Validation ---\n")

# 5.1: Validate that key numeric features are non-negative.
# For example, duration, packets, bytes, and load cannot be negative.
validation_cols <- c("dur", "spkts", "dpkts", "sbytes", "dbytes", "sload", "dload")
negative_values_check <- sapply(cleaned_data[validation_cols], function(col) any(col < 0))

if (any(negative_values_check)) {
  cat("WARNING: Negative values found in the following columns:\n")
  print(names(negative_values_check[negative_values_check]))
  # Optional: Code to handle these, e.g., remove them or set them to 0.
  # cleaned_data <- cleaned_data %>% filter(dur >= 0, spkts >= 0, ...)
} else {
  cat("SUCCESS: Validation passed. No negative values found in key numeric columns.\n")
}

# 5.2: Check the 'label' column
# The 'label' column should indicate if a record is an attack (1) or normal (0).
# Let's ensure 'attack_cat' is 'Normal' when 'label' is 0.
mismatch_check <- cleaned_data %>%
  filter((label == 0 & attack_cat != "Normal") | (label == 1 & attack_cat == "Normal"))

if (nrow(mismatch_check) > 0) {
  cat("WARNING: Found", nrow(mismatch_check), "rows where 'label' and 'attack_cat' mismatch.\n")
} else {
  cat("SUCCESS: Validation passed. The 'label' column is consistent with the 'attack_cat' column.\n")
}


#
# --- SECTION 6: FINAL REVIEW OF CLEANED DATA ---
#

cat("\n--- Final Review of Cleaned Dataset ---\n")

cat("\nDimensions of the final cleaned dataset (rows, columns):\n")
print(dim(cleaned_data))

cat("\nStructure of the cleaned dataset:\n")
str(cleaned_data, list.len = 5)

cat("\nSummary of the cleaned dataset:\n")
print(summary(cleaned_data))


#
# --- SECTION 7: SAVE THE CLEANED DATA ---
#
# It is a good practice to save the cleaned data to a new file.
# This creates a checkpoint and makes the next steps (analysis, visualization) faster.
output_filename <- "UNSW-NB15_cleaned.csv"
write_csv(cleaned_data, output_filename)

cat("\nSUCCESS: The cleaned dataset has been saved as '", output_filename, "'.\n")

