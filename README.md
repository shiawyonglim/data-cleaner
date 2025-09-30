Cyber Intrusion Detection and Classification Analysis
Project Overview
This project performs an in-depth analysis of the UNSW-NB15 dataset to identify key characteristics of various cyber-attacks. It uses the R programming language to clean the raw data, perform extensive exploratory and statistical analysis, build a predictive model, and provide an interactive console for on-demand data exploration.

The project is structured into two main R scripts:

data_preparation.R: Handles all data cleaning and pre-processing.

visualize.R: Performs all data analysis, generates plots, and contains the final interactive data explorer.

File Structure
To run this project, ensure the following files are all in the same directory:

/your-project-folder
|-- data_preparation.R
|-- visualize.R
|-- UNSW-NB15_uncleaned.csv
|-- 6. NUSW-NB15_features (data description).csv

A new folder named result/ will be created automatically when you run the analysis script to store all generated plots and data files.

Prerequisites
R and RStudio: You must have R and RStudio installed on your system.

R Packages: The script is designed to be self-contained. It will automatically check for, download, and install the required packages (tidyverse, corrplot, and broom) if they are not already present.

How to Use This Project: A Step-by-Step Guide
Step 1: Open the Project in RStudio
Open the visualize.R file in RStudio. This is the main script you will interact with.

Step 2: Run the Analysis Script
With the visualize.R file open, run the entire script. You can do this by clicking the "Source" button in RStudio or by using the shortcut Ctrl+Shift+Enter.

The script is designed to be robust:

It will first check if the clean data file (UNSW-NB15_cleaned.csv) exists.

If it does not exist, it will automatically run the data_preparation.R script to create it for you.

It will then proceed to run all 7 analysis objectives, saving all plots and data files to the result/ folder.

You will see a series of "SUCCESS" messages printed in the console as each step is completed.

Step 3: Using the Interactive Data Explorer
After the script has finished running (which may take a moment), your R console will be ready for the final, interactive part of the project.

To start the interactive session, type the following command directly into the R console and press Enter:

ask_the_data()

You will be greeted with a welcome message and a menu of 10 options.

How it works: Simply type the number corresponding to your question and press Enter. The function will then prompt you for any additional information it needs (like an attack name or a filename) and will instantly perform the analysis and print the answer or save the requested file.

To Exit: When you are finished, simply type exit and press Enter.

This interactive tool allows you to directly query the dataset, generate plots and reports on demand, and even export specific subsets of the data without needing to write any new code.


what are the use of these files
Plots for Your Main Objectives
attack_distribution.png:  This is your starting point. It's a bar chart that answers the basic question: "How many records of each attack type (and normal traffic) are in the dataset?" It gives you an overview of the data's composition.

duration_by_attack_category.png: This box plot (from Objective 1) compares the connection durations for different attacks. It helps you understand the behavior of attacks, showing that some (like DoS) are very quick, while others (like Backdoors) are longer-lasting.

correlation_heatmap.png:  This advanced plot (from Objective 2) is a high-level overview of how all the numerical features relate to each other. The dark blue and red squares show you strong relationships that you can investigate further.

sbytes_vs_sloss_by_attack.png: This scatter plot (from Objective 4) is a deep-dive analysis inspired by the heatmap. It visualizes the relationship between the amount of data sent (sbytes) and the number of lost packets (sloss), showing how this pattern differs for each attack type.

state_distribution_by_attack.png: This faceted bar chart (from Objective 5) shows which connection states (FIN, INT, RST, etc.) are most common for each attack. It provides powerful clues about how different attacks manipulate network protocols.

rate_density_normal_vs_exploits.png: This density plot (from Objective 7) directly compares the behavior of "Normal" traffic against "Exploits" attacks based on their connection rate. The different shapes of the curves visually prove that their behaviors are distinct.

Outputs from Your Additional Features
breakdown_dos_by_proto.png and breakdown_exploits_by_service.png: These two plots are the output of your first additional feature (plot_attack_breakdown). They demonstrate the function's ability to automatically generate detailed analyses for specific attacks, showing which protocols are used in DoS attacks and which services are targeted by Exploits.

model_coefficients.png:  This plot is the output of your second additional feature (plot_model_coefficients). It makes the complex results of your predictive model easy to understand by visually showing which network features are the most important predictors of an attack.

The Prediction Data File
prediction_results.csv: This is the CSV file generated by your predictive model in Objective 3. It contains the original input data plus two new columns: the model's calculated probability of an attack, and its final prediction ("Predicted Attack" or "Predicted Normal"). This file is the raw output of your prediction analysis.
