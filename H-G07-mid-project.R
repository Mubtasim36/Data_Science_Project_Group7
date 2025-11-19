#Data Understanding

#1. reading the dataset
url <- "https://raw.githubusercontent.com/Mubtasim36/Data_Science_Project_Group7/refs/heads/master/employee_performance.csv"
employee <- read.csv(url)


#2. displaying few rows:
head(employee)


#3.shape of the data set 
print(paste("The shape of the dataset is: ",nrow(employee),"x",ncol(employee)))


#4.show data type
cat(
  "Data Type of Employee_ID column:", class(employee$Employee_ID), "\n",
  "Data Type of Age column:", class(employee$Age), "\n",
  "Data Type of Gender column:", class(employee$Gender), "\n",
  "Data Type of Department column:", class(employee$Department), "\n",
  "Data Type of Experience_Years column:", class(employee$Experience_Years), "\n",
  "Data Type of Performance_Score column:", class(employee$Performance_Score), "\n",
  "Data Type of Salary column:", class(employee$Salary), "\n"
)


#5.Generate basic descriptive statistics

summary(employee)
library(dplyr)

#Generate basic descriptive statistics for Age (by using this method) 
employee %>%
  
  summarise(
    count = n(),
    mean_Age = mean(employee$Age),
    sd_Age = sd(employee$Age),
    mean_Age = mean(employee$Age),
    sd_Age = sd(employee$Age)
  )


#6.Identify categorical and numerical features
sapply(employee, function(col) {
  if (is.numeric(col)) {
    return("Numerical Feature")
  } else if (is.character(col)) {
    return("Categorical Feature")
  }
})


#B.1 Data Exploration & Visualization
#Creating Histogram for Each Numerical Data
hist(employee$Age)
hist(employee$Experience_Years)
hist(employee$Performance_Score)


#Creating 3 Box-plot for Each Numerical Data

ggplot(employee, aes(y = Experience_Years)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot of Experience_Years", y = "Experience_Years")

ggplot(employee, aes(y = Age)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot of Age", y = "Age")


ggplot(employee, aes(y = Performance_Score)) +
  geom_boxplot(fill = "red") +
  labs(title = "Boxplot of Performance_Score", y = "Performance_Score")


#Creating 3 Bar-chart for Each Numerical Data

#Bar chart for Employee Age 
Age_counts <- table(employee$Age)
barplot(Age_counts,
        main = "Age Distribution",
        xlab = "Age",
        ylab = "Count",
        col = "blue",
        ylim = c(0, 1.2 * max(Age_counts)),#limit for y axis values
        las = 2)

#Bar chart for Employee Gender 

Gender_counts <- table(employee$Gender)
barplot(Gender_counts,
        main = "Gender Distribution",
        xlab = "Gender",
        ylab = "Count",
        col = "yellow",
        ylim = c(0, 1.2 * max(Gender_counts)), #limit for y axis values
        las = 2)


#Bar chart for Employee Department 
dept_counts <- table(employee$Department)
barplot(dept_counts,
        main = "Department Distribution",
        xlab = "Department",
        ylab = "Count",
        col = "red",
        ylim = c(0, 1.2 * max(dept_counts)),  #limit for y axis values
        las = 2)



#Frequency of categorical variables
table(employee$Gender)
table(employee$Department)
 


#B.2
#Bivariate Analysis
#Correlation matrix (heatmap)

install.packages("reshape2")


library(reshape2)

num_data <- employee[sapply(employee, is.numeric)]
corr_mat <- cor(num_data)
 
melted_corr_mat <- melt(corr_mat)       #Convert matrix into long format

library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,fill=value)) + 
geom_tile()+                       #to draw the colored tiles for correlation values.
ggtitle("Heatmap for Correlation")          #for heatmap title



#Scatterplot

ggplot(employee, aes(x = Salary, y = Performance_Score )) +
  geom_point() +     #Adds scatter plot points (one point for each employee).
  geom_smooth(method = "lm", se = FALSE, color = "blue") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "Salary vs PS", x = "Salary", y = "Performance_Score ")


#BoxPlot
colnames(employee)

#1
ggplot(employee, aes(x=Gender,y = Age)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Gender and Age", x="Gender", y = "Age")

#2
ggplot(employee, aes(x=Department,y = Age)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Dept and Age", x="Department", y = "Age")

#3
ggplot(employee, aes(x=Gender,y = Performance_Score)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Gender & Score", x="Gender", y = "Performance_Score")

#4
ggplot(employee, aes(x=Department,y = Salary)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Department & Salary", x="Department", y = "Salary")


#B.3
#Identify patterns, skewness, and possible outliers


#C.1
#Handling Missing Values
#Detect missing values
colSums(is.na(employee))    #Columnwise no. of NA values
cat("Total NA values:", sum(is.na(employee)), "\n")    #Sum of number of NA values

Clean_employee <- na.omit(employee)   #Dataframe of the dataset with 0 NA values

cat("Total NA values after cleaning:", sum(is.na(employee)), "\n") #Sum of number of NA values in  new Dataframe



