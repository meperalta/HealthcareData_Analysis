# HealthcareData_Analysis
# Load the healthcare dataset

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
install.packages("caret")
library(caret)      # For modeling

# 'healthcare_dataset.csv' with the actual name and path of your dataset
healthcare_dataset <- read.csv('healthcare_dataset.csv')

# Display the structure and summary of the dataset
str(healthcare_dataset)
summary(healthcare_dataset)

# Check for missing values
missing_values <- colSums(is.na(healthcare_dataset))
print(missing_values)

#Output: no missing values

# Handle missing values (if necessary)
# Impute missing numerical values with the mean
healthcare_dataset[is.na(healthcare_dataset$numeric_column), 'numeric_column'] <- mean(healthcare_dataset$numeric_column, na.rm = TRUE)

# Remove rows with missing categorical values
healthcare_dataset <- healthcare_dataset[complete.cases(healthcare_dataset[c('categorical_column')]), ]

# Explore the distribution of the target variable 'Test Results'
table(healthcare_dataset$Test_Results)

#Check summary statistics to provide the central tendency, dispersion and shape distribution of each variable of the dataset.
summary_stats <- summary(healthcare_dataset)

# Print the summary statistics
print(summary_stats)

# Begin exploring relationships and patterns in the data
#DATA VISUALIZATION
#HISTOGRAM
# Visualize the distribution of 'Age' with 'Test Results'
# Check column names
names(healthcare_dataset)

# Verify the presence of 'Test_Results' in the dataset
head(healthcare_dataset$Test_Results)

# Check the structure of the dataset
str(healthcare_dataset)

# Histogram using data.frame notation
ggplot(healthcare_dataset, aes(x = Age, fill = `Test Results`)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Distribution of Age by Test Results",
       x = "Age",
       y = "Count")

#PIECHART

# Pie chart for the distribution of Medical Conditions
library(dplyr)
library(ggplot2)

# Create a summary of Medical Conditions
medical_conditions_summary <- healthcare_dataset %>%
  group_by(`Medical Condition`) %>%
  summarise(count = n())

# Sort the summary by count
medical_conditions_summary <- arrange(medical_conditions_summary, desc(count))

# Plot the pie chart
ggplot(medical_conditions_summary, aes(x = "", y = count, fill = `Medical Condition`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Medical Conditions",
       fill = "Medical Condition") +
  theme_minimal()

#HEATMAP
#CORRELATION ANALYSIS
#To perform correlation analysis and visualize the correlation matrix between numerical variables in R Studio, 
#Use the cor() function for calculating the correlation matrix and the corrplot package for creating an informative visualization. 

# Install and load necessary packages
library(ggplot2)
library(reshape2)

# Extract numerical columns for correlation analysis
numerical_columns <- select_if(healthcare_dataset, is.numeric)

# Calculate correlation matrix
correlation_matrix <- cor(numerical_columns)

# Melt the correlation matrix for visualization
melted_corr_matrix <- melt(correlation_matrix)

# Plot the heatmap
ggplot(melted_corr_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables",
       fill = "Correlation") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "lightgreen", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal()


