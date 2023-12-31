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

Output: 
str(healthcare_dataset)
spc_tbl_ [10,000 × 15] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
 $ Name              : chr [1:10000] "Tiffany Ramirez" "Ruben Burns" "Chad Byrd" "Antonio Frederick" ...
 $ Age               : num [1:10000] 81 35 61 49 51 41 82 55 33 39 ...
 $ Gender            : chr [1:10000] "Female" "Male" "Male" "Male" ...
 $ Blood Type        : chr [1:10000] "O-" "O+" "B-" "B-" ...
 $ Medical Condition : chr [1:10000] "Diabetes" "Asthma" "Obesity" "Asthma" ...
 $ Date of Admission : Date[1:10000], format: "2022-11-17" "2023-06-01" ...
 $ Doctor            : chr [1:10000] "Patrick Parker" "Diane Jackson" "Paul Baker" "Brian Chandler" ...
 $ Hospital          : chr [1:10000] "Wallace-Hamilton" "Burke, Griffin and Cooper" "Walton LLC" "Garcia Ltd" ...
 $ Insurance Provider: chr [1:10000] "Medicare" "UnitedHealthcare" "Medicare" "Medicare" ...
 $ Billing Amount    : num [1:10000] 37491 47304 36875 23303 18086 ...
 $ Room Number       : num [1:10000] 146 404 292 480 477 180 161 384 215 310 ...
 $ Admission Type    : chr [1:10000] "Elective" "Emergency" "Emergency" "Urgent" ...
 $ Discharge Date    : Date[1:10000], format: "2022-12-01" "2023-06-15" ...
 $ Medication        : chr [1:10000] "Aspirin" "Lipitor" "Lipitor" "Penicillin" ...
 $ Test Results      : chr [1:10000] "Inconclusive" "Normal" "Normal" "Abnormal" ...
 - attr(*, "spec")=
  .. cols(
  ..   Name = col_character(),
  ..   Age = col_double(),
  ..   Gender = col_character(),
  ..   `Blood Type` = col_character(),
  ..   `Medical Condition` = col_character(),
  ..   `Date of Admission` = col_date(format = ""),
  ..   Doctor = col_character(),
  ..   Hospital = col_character(),
  ..   `Insurance Provider` = col_character(),
  ..   `Billing Amount` = col_double(),
  ..   `Room Number` = col_double(),
  ..   `Admission Type` = col_character(),
  ..   `Discharge Date` = col_date(format = ""),
  ..   Medication = col_character(),
  ..   `Test Results` = col_character()
  .. )
 - attr(*, "problems")=<externalptr> 

summary(healthcare_dataset)
Output: 
summary(healthcare_dataset)
     Name                Age           Gender           Blood Type        Medical Condition  Date of Admission   
 Length:10000       Min.   :18.00   Length:10000       Length:10000       Length:10000       Min.   :2018-10-30  
 Class :character   1st Qu.:35.00   Class :character   Class :character   Class :character   1st Qu.:2020-02-10  
 Mode  :character   Median :52.00   Mode  :character   Mode  :character   Mode  :character   Median :2021-05-02  
                    Mean   :51.45                                                            Mean   :2021-05-01  
                    3rd Qu.:68.00                                                            3rd Qu.:2022-07-23  
                    Max.   :85.00                                                            Max.   :2023-10-30  
    Doctor            Hospital         Insurance Provider Billing Amount   Room Number    Admission Type    
 Length:10000       Length:10000       Length:10000       Min.   : 1000   Min.   :101.0   Length:10000      
 Class :character   Class :character   Class :character   1st Qu.:13507   1st Qu.:199.0   Class :character  
 Mode  :character   Mode  :character   Mode  :character   Median :25258   Median :299.0   Mode  :character  
                                                          Mean   :25517   Mean   :300.1                     
                                                          3rd Qu.:37734   3rd Qu.:400.0                     
                                                          Max.   :49996   Max.   :500.0                     
 Discharge Date        Medication        Test Results      
 Min.   :2018-11-01   Length:10000       Length:10000      
 1st Qu.:2020-02-23   Class :character   Class :character  
 Median :2021-05-18   Mode  :character   Mode  :character  
 Mean   :2021-05-17                                        
 3rd Qu.:2022-08-07                                        
 Max.   :2023-11-27  
 
# Check for missing values
missing_values <- colSums(is.na(healthcare_dataset))
print(missing_values)

#Output: no missing values
print(missing_values)
              Name                Age             Gender         Blood Type  Medical Condition  Date of Admission 
                 0                  0                  0                  0                  0                  0 
            Doctor           Hospital Insurance Provider     Billing Amount        Room Number     Admission Type 
                 0                  0                  0                  0                  0                  0 
    Discharge Date         Medication       Test Results 
                 0                  0                  0 
                 
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
 Name                Age           Gender           Blood Type        Medical Condition  Date of Admission   
 Length:10000       Min.   :18.00   Length:10000       Length:10000       Length:10000       Min.   :2018-10-30  
 Class :character   1st Qu.:35.00   Class :character   Class :character   Class :character   1st Qu.:2020-02-10  
 Mode  :character   Median :52.00   Mode  :character   Mode  :character   Mode  :character   Median :2021-05-02  
                    Mean   :51.45                                                            Mean   :2021-05-01  
                    3rd Qu.:68.00                                                            3rd Qu.:2022-07-23  
                    Max.   :85.00                                                            Max.   :2023-10-30  
    Doctor            Hospital         Insurance Provider Billing Amount   Room Number    Admission Type    
 Length:10000       Length:10000       Length:10000       Min.   : 1000   Min.   :101.0   Length:10000      
 Class :character   Class :character   Class :character   1st Qu.:13507   1st Qu.:199.0   Class :character  
 Mode  :character   Mode  :character   Mode  :character   Median :25258   Median :299.0   Mode  :character  
                                                          Mean   :25517   Mean   :300.1                     
                                                          3rd Qu.:37734   3rd Qu.:400.0                     
                                                          Max.   :49996   Max.   :500.0                     
 Discharge Date        Medication        Test Results      
 Min.   :2018-11-01   Length:10000       Length:10000      
 1st Qu.:2020-02-23   Class :character   Class :character  
 Median :2021-05-18   Mode  :character   Mode  :character  
 Mean   :2021-05-17                                        
 3rd Qu.:2022-08-07                                        
 Max.   :2023-11-27                             

# Begin exploring relationships and patterns in the data
#DATA VISUALIZATION
#HISTOGRAM
# Visualize the distribution of 'Age' with 'Test Results'
# Check column names
names(healthcare_dataset)
Output: 
 [1] "Name"               "Age"                "Gender"             "Blood Type"         "Medical Condition" 
 [6] "Date of Admission"  "Doctor"             "Hospital"           "Insurance Provider" "Billing Amount"    
[11] "Room Number"        "Admission Type"     "Discharge Date"     "Medication"         "Test Results"     

# Verify the presence of 'Test_Results' in the dataset
head(healthcare_dataset$Test_Results)

# Check the structure of the dataset
str(healthcare_dataset)
#Library(ggplot2)
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


