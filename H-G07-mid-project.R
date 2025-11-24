#Data Understanding
#Packages:

#List of required packages
packages <- c("ggplot2", "dplyr","reshape2","outliers","e1071")

# Install any packages that are not already installed
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}


#1. reading the Dataset
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


#Column Names
colnames(employee)

#BoxPlot
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
#Scatter Plot for showing Patterns

library(ggplot2)
ggplot(employee, aes(x = Performance_Score, y = Experience_Years )) +
  geom_point() +     #Adds scatter plot points (one point for each employee).
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "P_Score vs E_Year", x = "P_Score", y = "E_Year ")



#Skewness based on Mean vs Median
#For Salary
sal_mean <- mean(employee$Salary)
sal_median <- median(employee$Salary)
if(sal_mean>sal_median){
  print("Salary is Positively Skewed")
} else if(sal_mean < sal_median){
  print("Salary is Negatively Skewed")
}  else {
  print("Salary is Symmetric")
}

#For Experience_Years
Year_mean <- mean(employee$Experience_Years, na.rm = TRUE)     #Ignoring NA values
Year_median <- median(employee$Experience_Years,na.rm = TRUE)

if(Year_mean > Year_median){
  print("Experience_Years is Positively Skewed")
} else if(Year_mean < Year_median){
  print("Experience_Years is Negatively Skewed")
} else{
  print("Experience_Years is Symmetric")
}

#For Performance_Score
Score_mean <- mean(employee$Performance_Score, na.rm = TRUE)     #Ignoring NA values
Score_median <- median(employee$Performance_Score,na.rm = TRUE)

if(Score_mean > Score_median){
  print("Performance_Score is Positively Skewed")
} else if(Score_mean < Score_median){
  print("Performance_Score is Negatively Skewed")
} else{
  print("Performance_Score is Symmetric")
}


#Possible Outliers for AGE

# Computing IQR 
Q1 <- quantile(employee$Age, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(employee$Age, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify outliers 
outliers <- employee$Age[!is.na(employee$Age) &  #!is.na to find outliers excluding NA values
                           (employee$Age < lower_bound | employee$Age > upper_bound)]

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste(val, "is an outlier"))  #Loop to check for multiple outliers
  }
} else {
  print("There are no outliers in Age")
}



#Possible Outliers for Salary

# Computing IQR 
Q1 <- quantile(employee$Salary, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(employee$Salary, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify outliers 
outliers <- employee$Salary[!is.na(employee$Salary) &  #!is.na to find outliers excluding NA values
                           (employee$Salary < lower_bound | employee$Salary > upper_bound)]

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste(format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in Salary")
}





#C.1
#Handling Missing Values
#Detect missing values
colSums(is.na(employee))    #Column wise no. of NA values
cat("Total NA values:", sum(is.na(employee$Experience_Years)), "\n")    #Sum of number of NA values

#Removing NA Values in Experience_Years column
employee <- employee[!is.na(employee$Experience_Years), ]

#Checking NA values after removing missing values
cat("Total NA values in Experience_Years after cleaning:", sum(is.na(employee$Experience_Years)), "\n")


#Replacing NA values of Performance_Score with the Mean of Performance_Score
employee$Performance_Score[is.na(employee$Performance_Score)] <- mean(employee$Performance_Score, na.rm = TRUE)
cat("Total NA values after replacing:", sum(is.na(Clean_employee)), "\n") #Sum of number of NA values in  new Dataframe




#C2

#Possible Outliers for Performance_Score

# Computing IQR 
Q1 <- quantile(employee$Performance_Score, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(employee$Performance_Score, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- employee$Performance_Score[!is.na(employee$Performance_Score) &  #!is.na to find outliers excluding NA values
                                         (employee$Performance_Score < lower_bound | employee$Performance_Score > upper_bound)]

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste(format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in Performance_Score")
}


#Capping Performance_score; As there may be real performers among the Outliers, so taking an appropriate low and a high value to cap the scores
lower_limit <- 50   #As most low values are close to 50
upper_limit <- 95    #As most high values are close to 95
# Cap the outliers
employee$Performance_Score <- pmin(pmax(employee$Performance_Score, lower_limit), upper_limit)



#Checking for Outliers after Capping
# Computing IQR 
Q1 <- quantile(employee$Performance_Score, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(employee$Performance_Score, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- employee$Performance_Score[!is.na(employee$Performance_Score) &  #!is.na to find outliers excluding NA values
                                         (employee$Performance_Score < lower_bound | employee$Performance_Score > upper_bound)]

if(length(outliers) > 0){
  for(val in outliers){
    print(paste(format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in Performance_Score")
}

#C3
# One Hot Encoding for Gender
gender_encode <- model.matrix(~ Gender - 1, data = employee)  #model.matrix() function to change categorical values to numerical     ~Gender means OHE on Gender column   #-1 is to create 1 col per category

# Combining with original Dataset
employee <- cbind(employee, gender_encode)      #cbind to add the new columns to the original table

head(employee) #checking if OHE was added

#One Hot Encoding for Department
Dept_encode <- model.matrix(~ Department - 1, data = employee)

# Combining with original Dataset
employee <- cbind(employee, Dept_encode)

View(employee) #To view full dataset


#To delete extra columns:
#employee$DepartmentFinance  <- NULL
#employee$DepartmentHR  <- NULL  
#employee$DepartmentIT  <- NULL
#employee$DepartmentMarketing   <- NULL
#employee$DepartmentOperations    <- NULL
#employee$DepartmentSales    <- NULL


sapply(employee, is.numeric)   #to check which columns are Numeric



#C4
#Data Transformation

#Normalization using Min-Max scaling

#For Salary
employee$Salary <- (employee$Salary - min(employee$Salary)) / (max(employee$Salary) - min(employee$Salary))

#For Exp_Years
employee$Experience_Years <- (employee$Experience_Years - min(employee$Experience_Years, na.rm = TRUE)) / (max(employee$Experience_Years, na.rm = TRUE) - min(employee$Experience_Years, na.rm = TRUE))
head(employee)


#Skewness for salary using e1071 library(for skewness() function)
library(e1071)
skewness_value <- skewness(employee$Salary)
print(skewness_value)
hist(employee$Salary, main="Salary Distribution", xlab="Salary")  #showing Original salary Histogram


#Fixing Salary Skewness using log
employee$Salary_log <- log(employee$Salary + 1)

#Skewness based on Mean vs Median
#For Salary
sal_log_mean <- mean(employee$Salary_log)
sal_log_median <- median(employee$Salary_log)
if(sal_log_mean>sal_log_median){
  print("Log_Salary is Positively Skewed")
} else if(sal_log_mean < sal_log_median){
  print("Log_Salary is Negatively Skewed")
}  else {
  print("Log_Salary is Symmetric")
}

#showing new logged salary Histogram
hist(employee$Salary_log, main="Salary Distribution", xlab="Salary") 



