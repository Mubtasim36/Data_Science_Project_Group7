data("mtcars")
head(mtcars)

library(ggplot2)

#install.packages("ggplot2")

#Trend Analysis:Horsepower vs. Miles per Gallon
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "HP vs MPG", x = "Horsepower", y = "Miles per Gallon")

#Detecting Outliers with Boxplot
ggplot(mtcars, aes(y = mpg)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Miles per Gallon", y = "MPG")

#Correlation Heatmap
#install.packages("GGally")
library(GGally)
ggcorr(mtcars, label = TRUE)

# --- Handling Missing Values ---

# Check how many NA values are in each column
colSums(is.na(mtcars))

# Create a new dataframe with rows containing NA values removed
mtcars_clean <- na.omit(mtcars)

# Verify that there are no more NA values
cat("Total NA values after cleaning:", sum(is.na(mtcars_clean)), "\n")

#Remove Duplicate Rows
mtcars_clean <- mtcars_clean[!duplicated(mtcars_clean), ]

#Filter cars with mpg > 20
library(dplyr)
mtcars_filtered <- mtcars_clean %>% filter(mpg > 20)

#Select specific columns
mtcars_selected <- mtcars_filtered %>% select(mpg, hp, wt)

#Create a new variable: Power-to-Weight Ratio
mtcars_mutated <- mtcars_selected %>%
mutate(power_to_weight = hp / wt)

#Normalize mpg, hp, wt
mtcars_scaled <- mtcars_selected %>%
  mutate(across(c(mpg, hp, wt), ~ scale(.)[,1]))
head(mtcars_scaled)




#Excersice 

library(readr)
github_url <- "https://raw.githubusercontent.com/Shahriar16Hossain/Data-Science-Project/main/employee_performance.csv"
employee_performance <- read_csv(github_url)
head(employee_performance)

#Trend Analysis:Age vs. Salary
ggplot(employee_performance, aes(x = Age, y = Salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Age vs Salary", x = "AGE", y = "SALARY")

#Detecting Outliers with Boxplot
ggplot(employee_performance, aes(y = Salary)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Salary", y = "SALARY")

#Correlation Heatmap
ggcorr(employee_performance, label = TRUE)

# Check how many NA values are in each column
colSums(is.na(employee_performance))

# Create a new dataframe with rows containing NA values removed
employee_performance_clean <- na.omit(employee_performance)

# Verify that there are no more NA values
cat("Total NA values after cleaning:", sum(is.na(employee_performance_clean)), "\n")

#Remove Duplicate Rows
employee_performance_clean <- employee_performance_clean[!duplicated(employee_performance_clean), ]

#Filter cars with Salary > 20
employee_performance_filtered <- employee_performance_clean %>% filter(Salary > 20)

#Select specific columns
employee_performance_selected <- employee_performance_filtered %>% select(Age, Salary,Experience_Years )

#Create a new variable: Age-to-Salary Ratio
employee_performance_mutated <- employee_performance_selected %>%
  mutate(Age_to_Salary = Age / Salary)

#Normalize Age, Salary, Experience_Years
employee_performance_scaled <- employee_performance_selected %>%
  mutate(across(c(Age,Salary,Experience_Years), ~ scale(.)[,1]))
head(employee_performance_scaled)






