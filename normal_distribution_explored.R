# MODULE 6

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load dataset
library(readxl)
data <- read_excel("Desktop/Module6_Dataset.xlsx")

# Step 1: Overview of the dataset
str(data)
sum(is.na(data))  # Check for missing values

# Step 2: Regression analysis on full dataset
model_full <- lm(average_score ~ gender + race_ethnicity + parental_level_of_education + 
                   lunch + test_preparation_course, data = data)
summary(model_full)

# Step 3: Create dummy variables for gender
data <- data %>% mutate(gender_dummy = ifelse(gender == 0, "Female", "Male"))

# Step 4: Regression for subsets based on gender
model_female <- lm(average_score ~ math_score + reading_score + writing_score, 
                   data = data %>% filter(gender_dummy == "Female"))
model_male <- lm(average_score ~ math_score + reading_score + writing_score, 
                 data = data %>% filter(gender_dummy == "Male"))

summary(model_female)
summary(model_male)

# Step 5: Visualization
plot <- ggplot(data, aes(x = math_score, y = average_score, color = gender_dummy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot with Regression Lines by Gender",
       x = "Math Score", y = "Average Score") +
  theme_minimal()

print(plot)

# Save the plot
ggsave("scatterplot_regression_lines.png", plot)

# Create dummy variable for lunch
data <- data %>% mutate(lunch_dummy = ifelse(lunch == "standard", "Standard", "Free/Reduced"))

# Subset and run regression for each group
model_standard <- lm(average_score ~ math_score + reading_score + writing_score, 
                     data = data %>% filter(lunch_dummy == "Standard"))
model_reduced <- lm(average_score ~ math_score + reading_score + writing_score, 
                    data = data %>% filter(lunch_dummy == "Free/Reduced"))

# Summarize results
summary(model_standard)
summary(model_reduced)
