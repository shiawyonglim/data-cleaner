# setup 
# download and load required packages
if (!require(tidyverse)) {
  install.packages("tidyverse", repos = "https://cran.rstudio.com/")
}
if (!require(corrplot)) {
  install.packages("corrplot", repos = "https://cran.rstudio.com/")
}
if (!require(broom)) {
  install.packages("broom", repos = "https://cran.rstudio.com/")
}
library(tidyverse)
library(corrplot)
library(broom)

# check for result folder
if (!dir.exists("result")) {
  dir.create("result")
}


#-----------------------------------------------------------------------------
#load data set

cleaned_file <- "UNSW-NB15_cleaned.csv"
prep_script <- "data-prep.r" 

# Check if the cleaned file exists. If not, try to create it by running the prep script.
if (!file.exists(cleaned_file)) {
  cat("INFO: Cleaned data file ('", cleaned_file, "') not found.\n", sep="")
  cat("Attempting to run '", prep_script, "' to generate it...\n", sep="")
  
  if (file.exists(prep_script)) {
    tryCatch({
      source(prep_script)
      cat("SUCCESS: The '", prep_script, "' script ran successfully.\n", sep="")
    }, error = function(e) {
      cat("ERROR: Failed to run '", prep_script, "'. Please check that script for errors.\n", sep="")
      cat("R Error Message:", e$message, "\n")
      stop("Halting execution because data preparation failed.") # Stop execution
    })
  } else {
    cat("ERROR: The data preparation script '", prep_script, "' was not found in this directory.\n", sep="")
    stop("Halting execution because the required data preparation script is missing.")
  }
}

tryCatch({
  cleaned_data <- read_csv(cleaned_file, show_col_types = FALSE)
  cat("SUCCESS: Cleaned dataset loaded successfully.\n")
  
  categorical_cols <- c("proto", "service", "state", "attack_cat", "label")
  cleaned_data <- cleaned_data %>%
    mutate(across(all_of(categorical_cols), as.factor))
  
}, error = function(e) {
  cat("ERROR: Could not read the cleaned CSV file, even after attempting to generate it.\n")
  cat("Please ensure '", cleaned_file, "' can be created and read correctly.\n", sep="")
  cat("R Error Message:", e$message, "\n")
  stop("Halting execution because data could not be loaded.")
})


#------------------------------------------------------------------------------
# data analysis

attack_counts <- cleaned_data %>% count(attack_cat, sort = TRUE)
# used for debugging
# cat("\n--- Count of Records by Attack Category ---\n")
# print(attack_counts)

attack_distribution_plot <- ggplot(attack_counts, aes(x = reorder(attack_cat, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + 
  labs(title = "Distribution of Attack Categories", x = "Attack Category", y = "Number of Records") +
  theme_minimal() +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5)

# used for debugging
# print(attack_distribution_plot)
ggsave("result/attack_distribution.png", plot = attack_distribution_plot, width = 10, height = 8)
cat("\nSUCCESS: Example plot 'attack_distribution.png' has been saved to the 'result' folder.\n")



#-------------------------------------------------------------------------------
# Objective sections

# Objective 1:  compare the duration of network connections for different attack types.
duration_quantile <- quantile(cleaned_data$dur, 0.95, na.rm = TRUE)
attack_duration_data <- cleaned_data %>%
  filter(attack_cat != "Normal" & dur < duration_quantile)

duration_boxplot <- ggplot(attack_duration_data, aes(x = attack_cat, y = dur, fill = attack_cat)) +
  geom_boxplot() +
  labs(title = "Connection Duration by Attack Category (up to 95th Percentile)", x = "Attack Category", y = "Connection Duration (seconds)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# used for debugging
# print(duration_boxplot)
ggsave("result/duration_by_attack_category.png", plot = duration_boxplot, width = 12, height = 7)
cat("SUCCESS: Analysis 1 plot 'duration_by_attack_category.png' has been saved to the 'result' folder.\n")


#===============================================================================
# Objective 2: To identify relationships between key numerical features.


numeric_features <- cleaned_data %>%
  select(dur, spkts, dpkts, sbytes, dbytes, rate, sload, dload, sloss, dloss, sinpkt, dinpkt, sjit, djit)
correlation_matrix <- cor(numeric_features, use = "complete.obs")

png("result/correlation_heatmap.png", width = 10, height = 10, units = "in", res = 300)
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.5, title = "Correlation Matrix of Network Features", mar=c(0,0,1,0))
dev.off()
cat("SUCCESS: Analysis 2 plot 'correlation_heatmap.png' has been saved to the 'result' folder.\n")

#===============================================================================
# Objective 3: To identify significant predictors of cyber-attacks.

model_data <- cleaned_data %>% select(label, sttl, dttl, sbytes, dbytes, rate, service)
attack_model <- glm(label ~ ., data = model_data, family = "binomial")

predicted_probabilities <- predict(attack_model, type = "response")
prediction_results <- model_data %>%
  mutate(
    predicted_probability = predicted_probabilities,
    predicted_label = ifelse(predicted_probability > 0.5, "Predicted Attack", "Predicted Normal")
  )

write_csv(prediction_results, "result/prediction_results.csv")
cat("SUCCESS: Model predictions have been saved to 'result/prediction_results.csv'.\n")


#===============================================================================
# Objective 4: To analyze the relationship between data volume and packet loss.

sbytes_quantile <- quantile(cleaned_data$sbytes, 0.99, na.rm = TRUE)
sloss_quantile <- quantile(cleaned_data$sloss, 0.99, na.rm = TRUE)
packet_loss_data <- cleaned_data %>%
  filter(attack_cat != "Normal" & sbytes < sbytes_quantile & sloss < sloss_quantile)

packet_loss_scatter <- ggplot(packet_loss_data, aes(x = sbytes, y = sloss, color = attack_cat)) +
  geom_point(alpha = 0.6) +
  labs(title = "Source Bytes vs. Source Packet Loss by Attack Type", x = "Source to Destination Transaction Bytes (sbytes)", y = "Source Packets Lost (sloss)", color = "Attack Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

# used for debugging
# print(packet_loss_scatter)
ggsave("result/sbytes_vs_sloss_by_attack.png", plot = packet_loss_scatter, width = 12, height = 8)
cat("SUCCESS: Analysis 4 plot 'sbytes_vs_sloss_by_attack.png' has been saved to the 'result' folder.\n")


#===============================================================================
# Objective 5: To analyze the distribution of connection states for different attack types.

state_distribution_plot <- cleaned_data %>%
  filter(attack_cat != "Normal") %>%
  ggplot(aes(x = state, fill = state)) +
  geom_bar() +
  facet_wrap(~ attack_cat, scales = "free_y") +
  labs(
    title = "Distribution of Connection States Across Attack Categories",
    x = "Connection State",
    y = "Number of Records"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# used for debugging
# print(state_distribution_plot)
ggsave("result/state_distribution_by_attack.png", plot = state_distribution_plot, width = 14, height = 10)
cat("SUCCESS: Analysis 5 plot 'state_distribution_by_attack.png' has been saved to the 'result' folder.\n")


#===============================================================================
# Objective 6: To statistically test the difference in data volume across attack types.

top_attacks <- c("Generic", "Exploits", "Fuzzers", "DoS", "Reconnaissance")
anova_data <- cleaned_data %>%
  filter(attack_cat %in% top_attacks) %>%
  mutate(log_sbytes = log1p(sbytes)) 

anova_result <- aov(log_sbytes ~ attack_cat, data = anova_data)
write_csv(broom::tidy(anova_result), "result/anova_result.csv")
cat("SUCCESS: ANOVA test results have been saved to 'result/anova_result.csv'.\n")

# used for debugging
# cat("\n\n--- ANOVA Test for Difference in Mean Source Bytes ---\n")
# print(summary(anova_result))


#===============================================================================
# Objective 7: To compare the distribution of connection rates between Normal and Exploits traffic.
rate_quantile <- quantile(cleaned_data$rate, 0.98, na.rm = TRUE)
rate_comparison_data <- cleaned_data %>%
  filter(attack_cat %in% c("Normal", "Exploits") & rate < rate_quantile)

rate_density_plot <- ggplot(rate_comparison_data, aes(x = rate, fill = attack_cat)) +
  geom_density(alpha = 0.6) + # Use alpha for transparency
  labs(
    title = "Distribution of Connection Rate for Normal vs. Exploits Traffic",
    x = "Connection Rate (packets per second)",
    y = "Density",
    fill = "Category"
  ) +
  theme_minimal()

# used for debugging
# print(rate_density_plot)
ggsave("result/rate_density_normal_vs_exploits.png", plot = rate_density_plot, width = 12, height = 7)
cat("SUCCESS: Analysis 7 plot 'rate_density_normal_vs_exploits.png' has been saved to the 'result' folder.\n")


#--------------------------------------------------------------------------------
# Additional Features

# Additional Feature 1: Reusable Plotting Function

# roxygen2 style documentation for easier understanding
#' Generate and save a bar chart for a specific feature within an attack category.
#'
#' This function filters the dataset for a given attack type, counts the occurrences
#' of each value in a specified categorical column, and creates a bar chart.
#'
#' @param attack_name A character string specifying the attack category to filter by (e.g., "DoS").
#' @param feature_column A character string specifying the column name to analyze (e.g., "proto").
#' @return The ggplot object is saved as a PNG file.
#' @examples
#' # This function is now called via the interactive ask_the_data() explorer.

plot_attack_breakdown <- function(attack_name, feature_column) {
  if (!attack_name %in% unique(cleaned_data$attack_cat)) stop("Error: Attack name not found.")
  if (!feature_column %in% names(cleaned_data)) stop("Error: Feature column not found.")
  
  breakdown_data <- cleaned_data %>%
    filter(attack_cat == attack_name) %>%
    count(.data[[feature_column]], sort = TRUE) %>%
    rename(feature_value = 1)
    
  plot <- ggplot(breakdown_data, aes(x = reorder(feature_value, n), y = n)) +
    geom_bar(stat = "identity", fill = "darkred") +
    coord_flip() +
    labs(title = paste("Distribution of", feature_column, "for", attack_name, "Attacks"), x = feature_column, y = "Number of Records") +
    theme_minimal()
    
  filename <- paste0("result/breakdown_", tolower(attack_name), "_by_", feature_column, ".png")
  ggsave(filename, plot = plot, width = 10, height = 8)
  cat("--> SUCCESS: Generated and saved plot:", filename, "to the 'result' folder.\n")
  # We print the plot here so the user sees it when requested.
  print(plot)
}


#-------------------------------------------------------------------------------
# Additional Feature 2: Function to Visualize Model Coefficients

#' Creates a bar chart visualizing the coefficients of a logistic regression model.
#'
#' This function takes a model object, extracts the coefficients, and plots them
#' to show their importance and direction. Significant coefficients are highlighted.
#'
#' @param model A model object created by the glm() function.
#' @return A ggplot object is saved as a PNG file.
#' @examples
#' # This function is now called via the interactive ask_the_data() explorer.

plot_model_coefficients <- function(model) {
  coeffs <- tidy(model) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      term = reorder(term, estimate),
      significance = ifelse(p.value < 0.05, "Significant", "Not Significant")
    )
  
  plot <- ggplot(coeffs, aes(x = term, y = estimate, fill = significance)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Predictor Importance from Logistic Regression Model",
      x = "Predictor Variable",
      y = "Estimate (Impact on Attack Likelihood)"
    ) +
    scale_fill_manual(values = c("Significant" = "darkgreen", "Not Significant" = "grey")) +
    theme_minimal()
    
  filename <- "result/model_coefficients.png"
  ggsave(filename, plot = plot, width = 10, height = 8)
  cat("--> SUCCESS: Generated and saved plot:", filename, "to the 'result' folder.\n")
  print(plot)
}


#-------------------------------------------------------------------------------
# Additional Feature 3: Function to Generate an Attack Summary Report 

#' Prints a text-based summary report for a specific attack category.
#'
#' This function calculates and displays key numerical averages and the most common
#' categorical features for a given attack type.
#'
#' @param attack_name A character string specifying the attack category to summarize.
#' @return Prints a formatted summary directly to the console.
#' @examples
#' # This function is now called via the interactive ask_the_data() explorer.

generate_attack_summary <- function(attack_name) {
  if (!attack_name %in% unique(cleaned_data$attack_cat)) {
    stop("Error: Attack name '", attack_name, "' not found in the dataset.")
  }
  
  summary_data <- cleaned_data %>%
    filter(attack_cat == attack_name)
    most_common_proto <- summary_data %>% count(proto, sort = TRUE) %>% slice_head(n = 1)
  most_common_service <- summary_data %>% count(service, sort = TRUE) %>% slice_head(n = 1)
  most_common_state <- summary_data %>% count(state, sort = TRUE) %>% slice_head(n = 1)
  
  cat("\n\n---------------------------------------------------\n")
  cat("--- Summary Report for:", attack_name, "Attacks ---\n")
  cat("---------------------------------------------------\n")
  cat("Total Records:", nrow(summary_data), "\n\n")
  
  cat("Key Numerical Averages:\n")
  cat("  - Average Duration (dur):", round(mean(summary_data$dur, na.rm = TRUE), 4), "seconds\n")
  cat("  - Average Source Bytes (sbytes):", round(mean(summary_data$sbytes, na.rm = TRUE), 2), "\n")
  cat("  - Average Destination Bytes (dbytes):", round(mean(summary_data$dbytes, na.rm = TRUE), 2), "\n")
  cat("  - Average Connection Rate (rate):", round(mean(summary_data$rate, na.rm = TRUE), 2), "packets/sec\n\n")
  
  cat("Most Common Categorical Features:\n")
  cat("  - Protocol (proto):", as.character(most_common_proto$proto), "(", most_common_proto$n, "records)\n")
  cat("  - Service (service):", as.character(most_common_service$service), "(", most_common_service$n, "records)\n")
  cat("  - State (state):", as.character(most_common_state$state), "(", most_common_state$n, "records)\n")
  cat("---------------------------------------------------\n\n")
}


#--------------------------------------------------------------------------------
# data explorer 

#' Starts an interactive session in the R console to query the dataset.
#'
#' This function presents a menu of questions to the user and provides answers
#' by analyzing the in-memory 'cleaned_data' data frame.
#'
#' @return This function does not return a value but prints results to the console.
#' @examples
#' 
#' # To run, type ask_the_data() directly into the R console.

ask_the_data <- function() {
  cat("\n\n--- Welcome to the Interactive Data Explorer ---\n")
  
  #infinite loop
  while(TRUE) {
    cat("\nWhat would you like to know? (Enter 'exit' to quit)\n")
    cat("  1: What is the most common protocol for a specific attack?\n")
    cat("  2: What is the average connection duration for a specific attack?\n")
    cat("  3: Which attack type has the highest average source bytes (sbytes)?\n")
    cat("  4: How many connections used a specific service?\n")
    cat("  5: Generate a text summary report for a specific attack.\n")
    cat("  6: Plot a feature breakdown for a specific attack (e.g., protocols for DoS).\n")
    cat("  7: Show the summary for the Attack Prediction Model (Logistic Regression).\n")
    cat("  8: Show the summary for the ANOVA test on source bytes.\n")
    cat("  9: Plot the predictors from the Attack Prediction Model.\n")
    cat("  10: Filter data by a specific value and export to CSV.\n")
    
    user_choice <- readline(prompt = "Enter your choice (1-10): ")
    
    if (user_choice == "exit") {
      cat("Exiting the Data Explorer. Goodbye!\n")
      break
    }
    
    switch(user_choice,
           "1" = {
             attack_name <- readline(prompt = "Enter the attack name (e.g., DoS, Exploits): ")
             if (attack_name %in% cleaned_data$attack_cat) {
               result <- cleaned_data %>%
                 filter(attack_cat == attack_name) %>%
                 count(proto, sort = TRUE) %>%
                 slice_head(n = 1)
               cat("--> For", attack_name, "attacks, the most common protocol is '", as.character(result$proto), "' with", result$n, "records.\n")
             } else {
               cat("--> Error: Attack name not found.\n")
             }
           },
           "2" = {
             attack_name <- readline(prompt = "Enter the attack name (e.g., Generic, Fuzzers): ")
             if (attack_name %in% cleaned_data$attack_cat) {
               result <- cleaned_data %>%
                 filter(attack_cat == attack_name) %>%
                 summarise(avg_dur = mean(dur, na.rm = TRUE))
               cat("--> The average duration for", attack_name, "attacks is", round(result$avg_dur, 4), "seconds.\n")
             } else {
               cat("--> Error: Attack name not found.\n")
             }
           },
           "3" = {
             result <- cleaned_data %>%
               group_by(attack_cat) %>%
               summarise(avg_sbytes = mean(sbytes, na.rm = TRUE)) %>%
               arrange(desc(avg_sbytes)) %>%
               slice_head(n = 1)
             cat("--> The attack type with the highest average source bytes is '", as.character(result$attack_cat), "' with an average of", round(result$avg_sbytes, 2), "bytes.\n")
           },
           "4" = {
             service_name <- readline(prompt = "Enter the service name (e.g., http, dns, -): ")
             if (service_name %in% cleaned_data$service) {
               result <- cleaned_data %>%
                 filter(service == service_name) %>%
                 nrow()
               cat("--> There are", result, "connections that used the '", service_name, "' service.\n")
             } else {
               cat("--> Error: Service name not found.\n")
             }
           },
           "5" = {
             attack_name <- readline(prompt = "Enter the attack name to summarize (e.g., Fuzzers, DoS): ")
             if (attack_name %in% cleaned_data$attack_cat) {
               generate_attack_summary(attack_name)
             } else {
               cat("--> Error: Attack name not found.\n")
             }
           },
           "6" = {
             attack_name <- readline(prompt = "Enter the attack name to plot (e.g., Exploits): ")
             feature_col <- readline(prompt = "Enter the feature to plot (e.g., service, state): ")
             if (attack_name %in% cleaned_data$attack_cat && feature_col %in% names(cleaned_data)) {
               plot_attack_breakdown(attack_name, feature_col)
             } else {
               cat("--> Error: Invalid attack name or feature column.\n")
             }
           },
           "7" = {
             cat("\n--- Logistic Regression Model Summary ---\n")
             print(summary(attack_model))
             cat("--> Look for variables with '***' or '**' to identify significant predictors.\n")
           },
           "8" = {
             cat("\n--- ANOVA Test for Difference in Mean Source Bytes ---\n")
             print(summary(anova_result))
             cat("--> The 'Pr(>F)' value (p-value) indicates if there's a significant difference.\n")
           },
           "9" = {
             cat("--> Generating the plot for model predictors...\n")
             plot_model_coefficients(attack_model)
           },
           "10" = {
             cat("--> You've chosen to filter and export data.\n")
             filter_col <- readline(prompt = "Enter the column name to filter by (e.g., state, proto): ")
             
             # Validate the column name
             if (!filter_col %in% names(cleaned_data)) {
               cat("--> Error: Column '", filter_col, "' not found. Please check the spelling.\n")
             } else {
               filter_val <- readline(prompt = paste("Enter the exact value you want to see in the '", filter_col, "' column (e.g., FIN, tcp): "))
               
               # Perform the filter and check if any rows were found
               filtered_df <- cleaned_data %>% filter(.data[[filter_col]] == filter_val)
               
               if (nrow(filtered_df) == 0) {
                 cat("--> No records found for value '", filter_val, "' in column '", filter_col, "'. Please check the value.\n")
               } else {
                 cat("--> Found", nrow(filtered_df), "records matching your criteria.\n")
                 output_filename <- readline(prompt = "Enter a filename for the CSV export (e.g., fin_state_data.csv): ")
                 
                 # Save the filtered data to the 'result' folder
                 tryCatch({
                   write_csv(filtered_df, paste0("result/", output_filename))
                   cat("--> SUCCESS: Successfully exported the filtered data to 'result/", output_filename, "'.\n")
                 }, error = function(e) {
                   cat("--> ERROR: Could not save the file. R Error Message:", e$message, "\n")
                 })
               }
             }
           },
           cat("--> Invalid choice. Please enter a number between 1 and 10, or 'exit'.\n")
    )
  }
}

# how to use the explorer
# After running the entire script
# type the command ask_the_data() in the R console to start the session.

