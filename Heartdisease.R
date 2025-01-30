pacman::p_load(
  rio, # Importieren von Daten
  here, # Relative Dateipfade
  janitor, # Datacleaning
  lubridate, # Working with data
  matchmaker, # Dictionary-basierte Bereinigung
  epikit, # age_categories() Funktion
  readr, # for big data
  dplyr, # for using piping
  tidyverse, # Datamanagemt and visualization
  styler, # source code formatting
  lintr, # analyze problematic code patterns
  skimr, # preview Tibbles
  todor, # TODO
  ggcorrplot, # Visualizing correlations
  ggplot2, #visualizing
)
library(rio)
library(here)
library(janitor)
library(lubridate)
library(matchmaker)
library(epikit)
library(tidyverse)
library(styler)
library(lintr)
library(skimr)
library(todor)
library(readr)
library(dplyr)
library(ggcorrplot)
library(ggplot2)


# Loading Original Data
original_data <- import(here("heart_disease.csv"))

# Looking at Original Data
glimpse(original_data)

# Copy Original Data in new Table
cleaned_data <- original_data

# Remove rows with missing values (NA)
cleaned_data <- cleaned_data %>%
  drop_na()

# Remove rows where 'age' or 'cholesterol_level' are NA
cleaned_data <- cleaned_data %>%
  drop_na(Age, `Cholesterol Level`)

# Simplify the column names
cleaned_data <- cleaned_data %>%
  clean_names()

# Display the new column names
colnames(cleaned_data)

# Simplify the column names
cleaned_data <- cleaned_data %>%
  clean_names()

# Display the new column names
colnames(cleaned_data)

# show unique column names low_hdl_cholesterol'
unique(cleaned_data$low_hdl_cholesterol)

#Check Data
skim(cleaned_data)

# Replace empty strings and spaces with NA
cleaned_data <- cleaned_data %>%
  mutate(low_hdl_cholesterol = na_if(low_hdl_cholesterol, "")) %>%
  mutate(low_hdl_cholesterol = na_if(low_hdl_cholesterol, " "))

# Remove rows with NA in 'low_hdl_cholesterol'
cleaned_data <- cleaned_data %>%
  drop_na(low_hdl_cholesterol)

# Check the cleaned data
skimr::skim(cleaned_data)

# Count NAs and empty strings across all columns
missing_summary <- cleaned_data %>%
  summarise(across(everything(), ~ sum(is.na(.)) + sum(. == "", na.rm = TRUE)))

# Display the summary
print(missing_summary)

# Replace empty strings with NA in character columns
cleaned_data <- cleaned_data %>%
  mutate(across(where(is.character), ~ na_if(., "")))

# Remove rows with any NA values
cleaned_data <- cleaned_data %>%
  drop_na()

# Round BMI and Sleeping Hours to one decimal place
cleaned_data <- cleaned_data %>%
  mutate(
    bmi = round(bmi, 1),
    sleep_hours = round(sleep_hours, 1)
  )

# Check the cleaned dataset
skimr::skim(cleaned_data)

# Round CRP Level to 1 decimal place and Homocysteine Level to 2 decimal places
cleaned_data <- cleaned_data %>%
  mutate(
    crp_level = round(crp_level, 1),
    homocysteine_level = round(homocysteine_level, 2)
  )
# Round Sleeping Hours to whole numbers
cleaned_data <- cleaned_data %>%
  mutate(sleep_hours = round(sleep_hours, 0))

# Check the cleaned dataset
skimr::skim(cleaned_data)

# Simplify column names (keeping blood_pressure unchanged)
cleaned_data <- cleaned_data %>%
  rename(
    activity = exercise_habits,
    family_history = family_heart_disease,
    cholesterol = cholesterol_level,
    low_hdl = low_hdl_cholesterol,
    high_ldl = high_ldl_cholesterol,
    alcohol = alcohol_consumption,
    stress = stress_level,
    sleep = sleep_hours,
    sugar = sugar_consumption,
    triglycerides = triglyceride_level,
    fasting_sugar = fasting_blood_sugar,
    crp = crp_level,
    homocysteine = homocysteine_level,
    heart_disease = heart_disease_status
  )
# Display updated column names
colnames(cleaned_data)

# Add a new column indicating combined information
cleaned_data <- cleaned_data %>%
  mutate(
    combined_blood_pressure = case_when(
      high_blood_pressure == "Yes" ~ "High Blood Pressure",
      TRUE ~ "Normal Blood Pressure"
    )
  )
# Combine cholesterol-related columns
cleaned_data <- cleaned_data %>%
  mutate(
    combined_cholesterol = case_when(
      low_hdl == "Yes" & high_ldl == "Yes" ~ paste(cholesterol, "(Low HDL & High LDL)"),
      low_hdl == "Yes" ~ paste(cholesterol, "(Low HDL)"),
      high_ldl == "Yes" ~ paste(cholesterol, "(High LDL)"),
      TRUE ~ as.character(cholesterol)
    )
  ) %>%
 
 select(-low_hdl, -high_ldl)  # Optionally, remove the original columns

# Clean up the categories to avoid confusion and ensure they match
cleaned_data <- cleaned_data %>%
  mutate(
    activity = tolower(activity),
    smoking = tolower(smoking),
    family_history = recode(family_history, "Yes" = "Family History", "No" = "No History"),
    alcohol = tolower(alcohol),
    stress = tolower(stress)
  )
# Replace outliers with NA to avoid false Data
cleaned_data <- cleaned_data %>%
  mutate(
    triglycerides = ifelse(triglycerides > 500 | triglycerides < 50, NA, triglycerides),
    crp = ifelse(crp > 15, NA, crp),  # CRP over 15 is unnatural high
    homocysteine = ifelse(homocysteine > 30, NA, homocysteine)
  )
# Add age categories to have an better overview
cleaned_data <- cleaned_data %>%
  mutate(
    age_category = case_when(
      age < 30 ~ "Young",
      age >= 30 & age < 50 ~ "Middle-aged",
      age >= 50 & age < 70 ~ "Older",
      age >= 70 ~ "Senior"
    )
  )
cleaned_data <- cleaned_data %>%
  mutate(
    risk_group = case_when(
      heart_disease == "Yes" ~ "High Risk",
      diabetes == "Yes" & smoking == "yes" ~ "Medium Risk",
      TRUE ~ "Low Risk"
    )
  )
# Health score (simple aggregation of key lifestyle metrics)
cleaned_data <- cleaned_data %>%
  mutate(
    health_score = case_when(
      activity == "high" & stress == "low" & sleep >= 7 ~ "Good",
      activity == "low" | stress == "high" ~ "Poor",
      TRUE ~ "Average"
    )
  )
# Add BMI categories for better understanding
cleaned_data <- cleaned_data %>%
  mutate(
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obese"
    )
  )
# Remove duplicates
cleaned_data <- cleaned_data %>%
  distinct()
# Select only numeric columns for correlation
numeric_data <- cleaned_data %>%
  select(where(is.numeric))

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
print(cor_matrix)

# Group by gender and calculate summary statistics
gender_summary <- cleaned_data %>%
  group_by(gender) %>%
  summarise(
    avg_age = mean(age, na.rm = TRUE),
    avg_blood_pressure = mean(blood_pressure, na.rm = TRUE),
    avg_bmi = mean(bmi, na.rm = TRUE),
    avg_sleep = mean(sleep, na.rm = TRUE),
    avg_cholesterol = mean(cholesterol, na.rm = TRUE),
    count = n()
  )

# Display the summary
print(gender_summary)

# Let's analyze the data for men and women.

gender_summary <- cleaned_data %>%
  group_by(gender) %>%
  summarise(
    avg_age = mean(age, na.rm = TRUE),  # Average age
    avg_blood_pressure = mean(blood_pressure, na.rm = TRUE),  # Average blood pressure
    avg_bmi = mean(bmi, na.rm = TRUE),  # Average BMI
    avg_sleep = mean(sleep, na.rm = TRUE),  # Average sleep duration
    avg_cholesterol = mean(cholesterol, na.rm = TRUE),  # Average cholesterol level
    count = n()  # Total number of people in each group
  ) %>%
  arrange(gender)  # Sort the summary by gender (e.g., Female first, Male second)

# View the summary statistics for each gender
print(gender_summary)

# Let's sort the entire dataset by gender.
cleaned_data <- cleaned_data %>%
  arrange(gender)  # Sorts the data by gender: "Female" first, "Male" second.

# View the sorted dataset to ensure it's organized by gender
head(cleaned_data)
# Create a new column 'age_group' that categorizes individuals based on their age.
cleaned_data <- cleaned_data %>%
  mutate(age_group = case_when(
    age < 20 ~ "Teenagers (Below 20)",
    age >= 20 & age < 40 ~ "Young Adults (20-39)",
    age >= 40 & age < 60 ~ "Middle-aged Adults (40-59)",
    age >= 60 ~ "Seniors (60+)"
  )) %>%
  arrange(age_group, age)  # Sort the data by age_group first, then by age within the group.

# View the updated dataset to confirm the grouping and sorting.
head(cleaned_data)

# Group BMI into categories for better interpretation of weight-related health trends.
cleaned_data <- cleaned_data %>%
  mutate(bmi_category = case_when(
    bmi < 18.5 ~ "Underweight",
    bmi >= 18.5 & bmi < 24.9 ~ "Normal weight",
    bmi >= 25 & bmi < 29.9 ~ "Overweight",
    bmi >= 30 ~ "Obesity"
  )) %>%
  arrange(bmi_category)

# View the updated dataset.
head(cleaned_data)
# Group blood pressure into clinical categories.
cleaned_data <- cleaned_data %>%
  mutate(blood_pressure_category = case_when(
    blood_pressure < 120 ~ "Normal",
    blood_pressure >= 120 & blood_pressure < 130 ~ "Elevated",
    blood_pressure >= 130 & blood_pressure < 140 ~ "Hypertension Stage 1",
    blood_pressure >= 140 ~ "Hypertension Stage 2"
  )) %>%
  arrange(blood_pressure_category)

# View the updated dataset.
head(cleaned_data)

# Group cholesterol into meaningful categories.
cleaned_data <- cleaned_data %>%
  mutate(cholesterol_risk = case_when(
    cholesterol < 200 ~ "Desirable",
    cholesterol >= 200 & cholesterol < 240 ~ "Borderline High",
    cholesterol >= 240 ~ "High"
  )) %>%
  arrange(cholesterol_risk)

# View the updated dataset.
head(cleaned_data)

# Group alcohol consumption into categories.
cleaned_data <- cleaned_data %>%
  mutate(alcohol_category = case_when(
    alcohol == "None" ~ "No Consumption",
    alcohol %in% c("Low", "Medium") ~ "Moderate Consumption",
    alcohol == "High" ~ "High Consumption"
  )) %>%
  arrange(alcohol_category)

# View the updated dataset.
head(cleaned_data)

# Group stress levels into categories.
cleaned_data <- cleaned_data %>%
  mutate(stress_category = case_when(
    stress == "Low" ~ "Low Stress",
    stress == "Medium" ~ "Moderate Stress",
    stress == "High" ~ "High Stress"
  )) %>%
  arrange(stress_category)

# View the updated dataset.
head(cleaned_data)
# Group sleep hours into categories.
cleaned_data <- cleaned_data %>%
  mutate(sleep_category = case_when(
    sleep < 6 ~ "Sleep Deprivation",
    sleep >= 6 & sleep <= 8 ~ "Healthy Sleep",
    sleep > 8 ~ "Excess Sleep"
  )) %>%
  arrange(sleep_category)

# View the updated dataset.
head(cleaned_data)

# Group triglyceride levels into categories.
cleaned_data <- cleaned_data %>%
  mutate(triglycerides_category = case_when(
    triglycerides < 150 ~ "Normal",
    triglycerides >= 150 & triglycerides < 200 ~ "Borderline High",
    triglycerides >= 200 ~ "High"
  )) %>%
  arrange(triglycerides_category)

# View the updated dataset.
head(cleaned_data)

# Simplify diabetes status into a binary category.
cleaned_data <- cleaned_data %>%
  mutate(diabetes_status = ifelse(diabetes == "Yes", "Diabetic", "Non-Diabetic")) %>%
  arrange(diabetes_status)

# View the updated dataset.
head(cleaned_data, 50)
# safety reasons backup because some things got deleted
write_csv(cleaned_data, "cleaned_data_backup.csv")
#running some codes again some got deleted
# Sort the data by age and gender in ascending order

cleaned_data <- cleaned_data %>%
  arrange(gender, age)

# View the sorted dataset
head(cleaned_data)

# Remove alcohol_category and stress_category columns
cleaned_data <- cleaned_data %>%
  select(-alcohol_category, -stress_category)

# View the updated dataset
head(cleaned_data)

cleaned_data <- cleaned_data %>%
  distinct()

# Show struckture of Dataframe
str(data)

# Show names of data
colnames(cleaned_data)


# View the updated dataset.
head(cleaned_data, 50)

# Calculate the correlation matrix for numeric columns
correlation_matrix <- cor(cleaned_data[sapply(cleaned_data, is.numeric)], use = "complete.obs")
# Print the correlation matrix
print(correlation_matrix)

# Grouping by gender and summarizing BMI and Blood Pressure
grouped_summary_gender <- cleaned_data %>%
  group_by(gender) %>%
  summarise(
    average_bmi = mean(bmi, na.rm = TRUE),
    average_blood_pressure = mean(blood_pressure, na.rm = TRUE),
    count = n()
  )

# Print the summary
print(grouped_summary_gender)


# Check the data type of the sugar column
str(cleaned_data$sugar)

# Convert sugar levels from categorical to numeric (ordinal encoding)
cleaned_data <- cleaned_data %>%
  mutate(sugar = case_when(
    sugar == "Low" ~ 1,
    sugar == "Medium" ~ 2,
    sugar == "High" ~ 3,
    TRUE ~ NA_real_  # Handle unexpected values
  ))

# Add a new column for BMI categories to reach everyone
cleaned_data <- cleaned_data %>%
  mutate(
    bmi_category_new = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal weight",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obesity",
      TRUE ~ "Unknown"
    )
  )

# View the first few rows
head(cleaned_data)

# Frequency of heart disease by risk group
heart_disease_summary <- cleaned_data %>%
  group_by(risk_group) %>%
  summarise(
    heart_disease_count = sum(heart_disease == "Yes"),
    total_count = n(),
    proportion = heart_disease_count / total_count
  )

# Print the summary
print(heart_disease_summary)

# Calculate outliers for blood pressure using IQR
iqr_blood_pressure <- IQR(cleaned_data$blood_pressure, na.rm = TRUE)
q1 <- quantile(cleaned_data$blood_pressure, 0.25, na.rm = TRUE)
q3 <- quantile(cleaned_data$blood_pressure, 0.75, na.rm = TRUE)

# Define lower and upper bounds
lower_bound <- q1 - 1.5 * iqr_blood_pressure
upper_bound <- q3 + 1.5 * iqr_blood_pressure

# Find rows with outliers
outliers_blood_pressure <- cleaned_data %>%
  filter(blood_pressure < lower_bound | blood_pressure > upper_bound)

# Print outliers
print(outliers_blood_pressure)

# Remove the 'bmi_category' column because i created the new category
cleaned_data <- cleaned_data[, !colnames(cleaned_data) %in% "bmi_category"]



# Select only numeric columns for correlation analysis
numeric_cols <- cleaned_data %>% select(where(is.numeric))

# Compute correlation matrix
cor_matrix <- cor(numeric_cols, use = "complete.obs", method = "pearson")

# Output the correlation matrix
print(cor_matrix)

#Save the correlation matrix to a CSV file for later inspection
write.csv(cor_matrix, "correlation_matrix.csv")

# Visualizing correlations (optional, heatmap-style)
ggcorrplot(cor_matrix, type = "lower", lab = TRUE, outline.color = "white")

group_summary <- cleaned_data %>%
  group_by(age_group, gender) %>%
  summarise(
    mean_bmi = mean(bmi, na.rm = TRUE),
    mean_bp = mean(blood_pressure, na.rm = TRUE),
    mean_cholesterol = mean(cholesterol, na.rm = TRUE),
    mean_triglycerides = mean(triglycerides, na.rm = TRUE),
    heart_disease_rate = mean(ifelse(heart_disease == "Yes", 1, 0), na.rm = TRUE),
    .groups = "drop"
  )

# View the summary
print(group_summary)

# Create a new risk_level column based on bmi and cholesterol
cleaned_data <- cleaned_data %>%
  mutate(
    risk_level = case_when(
      bmi > 30 & cholesterol > 240 ~ "High Risk",
      bmi > 25 & cholesterol > 200 ~ "Moderate Risk",
      TRUE ~ "Low Risk"
    )
  )

# Check distribution of risk levels
table(cleaned_data$risk_level)


# Group by stress and sleep_category, does sleep affect stress
stress_sleep_summary <- cleaned_data %>%
  group_by(stress, sleep_category) %>%
  summarise(
    avg_crp = mean(crp, na.rm = TRUE),
    avg_homocysteine = mean(homocysteine, na.rm = TRUE),
    .groups = "drop"
  )

# View the summary
print(stress_sleep_summary)

# Correlation analysis by gender
cor_by_gender <- cleaned_data %>%
  group_by(gender) %>%
  summarise(
    correlation_bp_cholesterol = cor(blood_pressure, cholesterol, use = "complete.obs", method = "pearson"),
    correlation_bmi_triglycerides = cor(bmi, triglycerides, use = "complete.obs", method = "pearson"),
    .groups = "drop"
  )

# View correlations by gender
print(cor_by_gender)

# Table for new Data
data <- tibble::tibble(
  gender = c("Female", "Male"),
  correlation_bp_cholesterol = c(-0.0175, -0.00595),
  correlation_bmi_triglycerides = c(0.00404, 0.00288)
)

# prep the data for visualisation
data_long <- tidyr::pivot_longer(
  data,
  cols = c(correlation_bp_cholesterol, correlation_bmi_triglycerides),
  names_to = "correlation_type",
  values_to = "value"
)

# Visualisierung with ggplot
ggplot(data_long, aes(x = gender, y = value, fill = correlation_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Genderspecific Correlations",
    x = "Gender",
    y = "Correlation value",
    fill = "Correlation type"
  ) +
  theme_minimal()

# Grouping data by Heart Disease status and calculating averages for all numeric columns
grouped_data <- cleaned_data %>%
  group_by(heart_disease) %>%  # Assuming "heart_disease" is the column indicating presence/absence
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

# View the grouped data
print(grouped_data)

# Split data into two groups based on heart disease
grouped_data <- cleaned_data %>%
  arrange(heart_disease) %>%
  group_split(heart_disease)

# View all values grouped by heart disease status
names(grouped_data) <- c("No_Heart_Disease", "Heart_Disease")

# Print both groups for inspection
list(
  No_Heart_Disease = grouped_data[[1]],
  Heart_Disease = grouped_data[[2]]
)
# Create age groups for better visualization
cleaned_data$age_group <- cut(cleaned_data$age, breaks = seq(20, 80, by = 10), right = FALSE, 
                              labels = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79"))

# Bar plot for age groups by heart disease
ggplot(cleaned_data, aes(x = age_group, fill = heart_disease)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Age Group Distribution by Heart Disease Status",
    x = "Age Group",
    y = "Count",
    fill = "Heart Disease"
  ) +
  theme_minimal()

# Boxplot for cholesterol levels by heart disease status
ggplot(cleaned_data, aes(x = heart_disease, y = cholesterol, fill = heart_disease)) +
  geom_boxplot() +
  labs(
    title = "Cholesterol Levels by Heart Disease Status",
    x = "Heart Disease",
    y = "Cholesterol"
  ) +
  theme_minimal()

# Bar plot for gender by heart disease
ggplot(cleaned_data, aes(x = gender, fill = heart_disease)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Gender Distribution by Heart Disease Status",
    x = "Gender",
    y = "Count",
    fill = "Heart Disease"
  ) +
  theme_minimal()


# Density plot for cholesterol
ggplot(cleaned_data, aes(x = cholesterol, fill = heart_disease)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Cholesterol Distribution by Heart Disease Status",
    x = "Cholesterol",
    y = "Density",
    fill = "Heart Disease"
  ) +
  theme_minimal()


#check colnames
colnames(cleaned_data)

#see how sleep duration compares to heart disease
ggplot(cleaned_data, aes(x = heart_disease, y = sleep, fill = heart_disease)) +
  geom_boxplot() +
  labs(
    title = "Sleep Duration by Heart Disease Status",
    x = "Heart Disease",
    y = "Sleep Duration (hours)",
    fill = "Heart Disease"
  ) +
  theme_minimal()
#how stress level compares to heart disease density plot
ggplot(cleaned_data, aes(x = stress, fill = heart_disease)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Stress Levels by Heart Disease Status",
    x = "Stress Level",
    y = "Density",
    fill = "Heart Disease"
  ) +
  theme_minimal()

# Bar plot for alcohol consumption by heart disease status
ggplot(cleaned_data, aes(x = alcohol, fill = heart_disease)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Alcohol Consumption by Heart Disease Status",
    x = "Alcohol Consumption",
    y = "Count",
    fill = "Heart Disease"
  ) +
  theme_minimal()

# Bar plot for smoking by heart disease status
ggplot(cleaned_data, aes(x = smoking, fill = heart_disease)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Smoking by Heart Disease Status",
    x = "Smoking Status",
    y = "Count",
    fill = "Heart Disease"
  ) +
  theme_minimal()

# Bar plot for smoking by heart disease status
ggplot(cleaned_data, aes(x = smoking, fill = heart_disease)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Smoking by Heart Disease Status",
    x = "Smoking Status",
    y = "Count",
    fill = "Heart Disease"
  ) +
  theme_minimal()


# Create a combined variable for alcohol and smoking
cleaned_data <- cleaned_data %>%
  mutate(alcohol_smoking = paste(alcohol, smoking, sep = "_"))

# Stacked bar plot
ggplot(cleaned_data, aes(x = alcohol_smoking, fill = heart_disease)) +
  geom_bar(position = "fill") +
  labs(
    title = "Combined Alcohol and Smoking by Heart Disease Status",
    x = "Alcohol and Smoking",
    y = "Proportion",
    fill = "Heart Disease"
  ) +
  theme_minimal()
# Density plot for stress levels by alcohol consumption
ggplot(cleaned_data, aes(x = stress, fill = alcohol)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Stress Levels by Alcohol Consumption",
    x = "Stress Level",
    y = "Density",
    fill = "Alcohol"
  ) +
  theme_minimal()

# Density plot for stress levels by smoking status
ggplot(cleaned_data, aes(x = stress, fill = smoking)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Stress Levels by Smoking Status",
    x = "Stress Level",
    y = "Density",
    fill = "Smoking"
  ) +
  theme_minimal()
#take a look at the distribution of age among individuals
ggplot(cleaned_data, aes(x = age, fill = heart_disease)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "dodge") +
  labs(title = "Age Distribution by Heart Disease Status", x = "Age", y = "Count", fill = "Heart Disease") +
  theme_minimal()

#visualize Blood pressure distributions for individuals with and without heart disease
ggplot(cleaned_data, aes(x = blood_pressure, fill = heart_disease)) +
  geom_density(alpha = 0.6) +
  labs(title = "Blood Pressure Distribution by Heart Disease Status", x = "Blood Pressure", y = "Density", fill = "Heart Disease") +
  theme_minimal()

#how does the BMI cholesterol work against heart disease
ggplot(cleaned_data, aes(x = bmi, y = cholesterol, color = heart_disease)) +
  geom_point(alpha = 0.6) +
  labs(title = "BMI vs. Cholesterol by Heart Disease Status", x = "BMI", y = "Cholesterol", color = "Heart Disease") +
  theme_minimal()

#diabetes vs heart disease
ggplot(cleaned_data, aes(x = diabetes, fill = heart_disease)) +
  geom_bar(position = "fill") +
  labs(title = "Diabetes Prevalence by Heart Disease Status", x = "Diabetes", y = "Proportion", fill = "Heart Disease") +
  theme_minimal()
#how does family history and heart disease correlate
ggplot(cleaned_data, aes(x = family_history, fill = risk_level)) +
  geom_bar(position = "fill") +
  labs(title = "Family History and Risk Levels", x = "Family History", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

#show proportions of individuals in different risk levels
cleaned_data %>%
  count(risk_level) %>%
  ggplot(aes(x = "", y = n, fill = risk_level)) +
  geom_col() +
  coord_polar("y") +
  labs(title = "Proportion of Individuals by Risk Level", x = NULL, y = NULL, fill = "Risk Level") +
  theme_minimal()
#show distribution of numeric variable by violin plot
ggplot(cleaned_data, aes(x = bmi, y = cholesterol, size = stress, color = heart_disease)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Bubble Plot: BMI vs Cholesterol by Stress Levels",
    x = "BMI",
    y = "Cholesterol",
    size = "Stress Level",
    color = "Heart Disease"
  ) +
  theme_minimal()
ggplot(cleaned_data, aes(x = heart_disease, y = cholesterol, fill = heart_disease)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(
    title = "Cholesterol Distribution by Heart Disease Status",
    x = "Heart Disease",
    y = "Cholesterol",
    fill = "Heart Disease"
  ) +
  theme_minimal()
#how does cholesterol work on age groups
age_cholesterol <- cleaned_data %>%
  group_by(age_group) %>%
  summarise(mean_cholesterol = mean(cholesterol, na.rm = TRUE))

ggplot(age_cholesterol, aes(x = reorder(age_group, mean_cholesterol), y = mean_cholesterol)) +
  geom_segment(aes(xend = age_group, yend = 0), color = "skyblue") +
  geom_point(size = 4, color = "blue") +
  labs(
    title = "Mean Cholesterol Levels by Age Group",
    x = "Age Group",
    y = "Mean Cholesterol"
  ) +
  coord_flip() +
  theme_minimal()
#show the average mbi by gender
gender_data <- cleaned_data %>%
  group_by(gender) %>%
  summarise(mean_bmi = mean(bmi, na.rm = TRUE))

ggplot(gender_data, aes(x = gender, y = mean_bmi, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_polar() +
  labs(
    title = "Average BMI by Gender",
    x = NULL,
    y = NULL,
    fill = "Gender"
  ) +
  theme_minimal()
#looking for missing data again just to be safe
colSums(is.na(cleaned_data))

#delete column age group
cleaned_data <- cleaned_data[, !colnames(cleaned_data) %in% "age_group"]

# create new age groups

cleaned_data$age_group <- cut(
  cleaned_data$age, 
  breaks = c(18, 29, 39, 49, 59, 69, 80), 
  labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-80"),
  right = FALSE 
)
 #check colnames
  colnames(cleaned_data)
# see how age, cholesterol and heart diseases interact
  ggplot(cleaned_data, aes(x = age, y = cholesterol, color = heart_disease)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "Interaction Between Age and Cholesterol by Heart Disease Status",
      x = "Age",
      y = "Cholesterol",
      color = "Heart Disease"
    ) +
    theme_minimal()
  
#bar plot for gender and heart disease comparison
  ggplot(cleaned_data, aes(x = gender, fill = heart_disease)) +
    geom_bar(position = "fill") +
    labs(
      title = "Proportion of Heart Disease by Gender",
      x = "Gender",
      y = "Proportion",
      fill = "Heart Disease"
    ) +
    theme_minimal()
  
  #look at the bmi distribution by risk levels
  ggplot(cleaned_data, aes(x = bmi, fill = risk_level)) +
    geom_density(alpha = 0.6) +
    labs(
      title = "BMI Distribution by Risk Levels",
      x = "BMI",
      y = "Density",
      fill = "Risk Level"
    ) +
    theme_minimal()
  
  
#combine the risk level with gender and age groups
  ggplot(cleaned_data, aes(x = age_group, fill = risk_level)) +
    geom_bar(position = "dodge") +
    facet_wrap(~ gender) +
    labs(
      title = "Risk Levels Across Age Groups by Gender",
      x = "Age Group",
      y = "Count",
      fill = "Risk Level"
    ) +
    theme_minimal()
  
  cleaned_data$age_group <- cut(
    cleaned_data$age, 
    breaks = c(18, 29, 39, 49, 59, 69, 80), 
    labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-80"),
    right = FALSE)
  
  str(cleaned_data$age)
  ggplot(cleaned_data, aes(x = age_group, fill = risk_level)) +
    geom_bar(position = "dodge") +
    facet_wrap(~ gender) +
    labs(
      title = "Risk Levels Across Age Groups by Gender",
      x = "Age Group",
      y = "Count",
      fill = "Risk Level"
    ) +
    theme_minimal()
  #Visualize correltations between numeric variables
  numeric_data <- cleaned_data %>% select_if(is.numeric)
  correlation_matrix <- cor(numeric_data, use = "complete.obs")
  ggcorrplot(correlation_matrix, method = "circle", type = "lower", 
             title = "Correlation Matrix of Numeric Variables", 
             lab = TRUE, lab_size = 3)
  
  # Check for NA values in 'sugar'
  sum(is.na(cleaned_data$sugar))
  
  # Check for non-numeric values in 'sugar' (if they weren't handled already)
  unique(cleaned_data$sugar)
  cleaned_data %>%
    group_by(risk_group) %>%
    summarise(
      count_na_sugar = sum(is.na(sugar)),
      count_valid_sugar = sum(!is.na(sugar))
    )
  # Impute missing values in sugar with the group-wise median
  cleaned_data <- cleaned_data %>%
    group_by(risk_group) %>%
    mutate(sugar = ifelse(is.na(sugar), median(sugar, na.rm = TRUE), sugar))
  
  # Now re-run the summarization
  grouped_summary_risk <- cleaned_data %>%
    group_by(risk_group) %>%
    summarise(
      median_cholesterol = median(cholesterol, na.rm = TRUE),
      median_sugar = median(sugar, na.rm = TRUE),
      total_count = n()
    )
  # Check if sugar is entirely NA
  sum(is.na(cleaned_data$sugar))  # Total number of NA values in sugar
  sum(!is.na(cleaned_data$sugar))  # Total number of valid (non-NA) values in sugar
  
  # View the first few rows of the 'sugar' column for inspection
  head(cleaned_data$sugar)
  # Inspect unique values in 'sugar'
  unique(cleaned_data$sugar)
  # If 'sugar' column is entirely NA, you can try imputing with a global median
  global_median_sugar <- median(cleaned_data$sugar, na.rm = TRUE)
  
  cleaned_data <- cleaned_data %>%
    mutate(sugar = ifelse(is.na(sugar), global_median_sugar, sugar))
  
  # Re-run the summarization
  grouped_summary_risk <- cleaned_data %>%
    group_by(risk_group) %>%
    summarise(
      median_cholesterol = median(cholesterol, na.rm = TRUE),
      median_sugar = median(sugar, na.rm = TRUE),
      total_count = n()
    )
  
  print(grouped_summary_risk)
  # Remove 'sugar' column from the dataset
  cleaned_data <- cleaned_data %>%
    select(-sugar)
  
  # Now, you can re-run your summarization without the 'sugar' column
  grouped_summary_risk <- cleaned_data %>%
    group_by(risk_group) %>%
    summarise(
      median_cholesterol = median(cholesterol, na.rm = TRUE),
      total_count = n()
    )
  
  # Print the result
  print(grouped_summary_risk)
  
  
  
  print(grouped_summary_risk)
  
  print(str(cleaned_data))

# Speichern der 'cleaned_data' im angegebenen Verzeichnis
write_csv(cleaned_data, "C:/Shinyapp/ThaleaData/cleaned_data.csv")




