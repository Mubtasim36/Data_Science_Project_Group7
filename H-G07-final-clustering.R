#Clustering
#A. Data Collection

#Dataset Source: Kaggle
#Dataset Link: https://www.kaggle.com/datasets/mamunhasan2cs/employee-performance-and-salary-synthetic-dataset


#Dataset Description: This dataset contains information on over 1,000 employees from a 
#medium-sized organization across six departments. It includes attributes such as age, 
#gender, department, years of experience, performance score, and salary. The dataset also 
#contains missing values, duplicate records, and outliers to reflect real-world data 
#challenges commonly encountered in data science and machine learning projects.



#Packages:

#List of required packages
packages <- c("ggplot2", "dplyr","reshape2","outliers","e1071","corrplot")

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
colnames(employee) #To see column names



#B. Data Understanding & Exploration 

#Shape of the data set 
print(paste("The shape of the dataset is: ",nrow(employee),"x",ncol(employee)))


#Showing data type
cat(                         #Cat is used to print multi line 
  "Data Type of Employee_ID column:", class(employee$Employee_ID), "\n",
  "Data Type of Age column:", class(employee$Age), "\n",
  "Data Type of Gender column:", class(employee$Gender), "\n",
  "Data Type of Department column:", class(employee$Department), "\n",
  "Data Type of Experience_Years column:", class(employee$Experience_Years), "\n",
  "Data Type of Performance_Score column:", class(employee$Performance_Score), "\n",
  "Data Type of Salary column:", class(employee$Salary), "\n"
)


#Showing Summary Statistics
summary(employee)


#.Identify categorical and numerical features
sapply(employee, function(col) {
  if (is.numeric(col)) {
    return("Numerical Feature")
  } else if (!is.numeric(col)) {
    return("Categorical Feature")
  }
})



#Creating Histogram
hist(employee$Age)
hist(employee$Experience_Years)
hist(employee$Performance_Score)
hist(employee$Salary)



#Creating Box-plot for Each Numerical Data
library(ggplot2)
ggplot(employee, aes(y = Experience_Years)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot of Experience_Years", y = "Experience_Years")

ggplot(employee, aes(y = Age)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot of Age", y = "Age")


ggplot(employee, aes(y = Performance_Score)) +
  geom_boxplot(fill = "red") +
  labs(title = "Boxplot of Performance_Score", y = "Performance_Score")


ggplot(employee, aes(y = Salary)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Boxplot of Salary", y = "Salary")



#Scatterplots:

ggplot(employee, aes(x = Salary, y = Performance_Score )) +
  geom_point() +     #Adds scatter plot points (one point for each employee).
  geom_smooth(method = "lm", se = FALSE, color = "blue") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "Salary vs PS", x = "Salary", y = "Performance_Score ")

ggplot(employee, aes(x = Salary, y = Experience_Years )) +
  geom_point() +     #Adds scatter plot points (one point for each employee).
  geom_smooth(method = "lm", se = FALSE, color = "red") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "Salary vs Year", x = "Salary", y = "Experience_Years ")

ggplot(employee, aes(x = Age, y = Performance_Score )) +
  geom_point() +     #Adds scatter plot points (one point for each employee).
  geom_smooth(method = "lm", se = FALSE, color = "blue") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "Salary vs PS", x = "Salary", y = "Performance_Score ")

ggplot(employee, aes(x = Age, y = Experience_Years )) +
  geom_point() +     #Adds scatter plot points (one point for each employee).
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "Age vs Year", x = "Age", y = "Experience_Years ")



#Heatmap

library(reshape2)

num_data <- employee[sapply(employee, is.numeric)]
corr_mat <- cor(num_data)

melted_corr_mat <- melt(corr_mat)       #Convert matrix into long format

library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,fill=value)) + 
  geom_tile()+                       #to draw the colored tiles for correlation values.
  ggtitle("Heatmap for Correlation")          #for heatmap title


#Identifying missing values
colSums(is.na(employee))    #Column wise no. of NA values
cat("Total NA values:", sum(is.na(employee$Experience_Years)), "\n")    #Sum of number of NA values



#Identify Outliers

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
    print(paste("Performance Score ",format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in Performance_Score")
}


#Possible Outliers for Age

# Computing IQR 
Q1 <- quantile(employee$Age, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(employee$Age, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- employee$Age[!is.na(employee$Age) &  #!is.na to find outliers excluding NA values
                           (employee$Age < lower_bound | employee$Age > upper_bound)]

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste("Age ",format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
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

# Identify Outliers 
outliers <- unique(employee$Salary[!is.na(employee$Salary) &  #!is.na to find outliers excluding NA values
                                     (employee$Salary < lower_bound | employee$Salary > upper_bound)])

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste("Salary ",format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in Salary")
}


#Possible Outliers for Experience_years

# Computing IQR 
Q1 <- quantile(employee$Experience_Years, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(employee$Experience_Years, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- unique(employee$Experience_Years[!is.na(employee$Experience_Years) &  #!is.na to find outliers excluding NA values
                                               (employee$Experience_Years < lower_bound | employee$Experience_Years > upper_bound)])

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste("Experience Year ",format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in Experience_Years")
}



#Relationships shown using correlation
numeric_data <- employee[sapply(employee, is.numeric)]  #Finds numeric data
corr_matrix <- cor(numeric_data, use = "complete.obs")   #cor() calculates pairwise correlations between all numeric columns.    use = "complete.obs" ensures that only rows without missing values are considered for each pair of columns.
print("Correlation matrix:")
print(corr_matrix)   



#C. Data Preprocessing 


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
cat("Total NA values after replacing:", sum(is.na(employee)), "\n") #Sum of number of NA values in  new Dataframe



#Handling Outliers

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



#Encoding Categorical values

# One Hot Encoding for Gender
gender_encode <- model.matrix(~ Gender - 1, data = employee)  #model.matrix() function to change categorical values to numerical     ~Gender means OHE on Gender column   #-1 is to create 1 col per category
# Combining with original Dataset
Encoded_employee <- cbind(employee, gender_encode)      #cbind to add the new columns to a new table
head(Encoded_employee) #checking if OHE was added



#One Hot Encoding for Department
Dept_encode <- model.matrix(~ Department - 1, data = employee)

# Combining with original Dataset
Encoded_employee <- cbind(employee, Dept_encode)


#Normalizing Numeric Values using Min-Max Scaling

Norm_employee<-employee #Creating new Table for Other Tasks so that original table stays same

#For Salary
Norm_employee$Salary <- (employee$Salary - min(employee$Salary)) / (max(employee$Salary) - min(employee$Salary))

#For Exp_Years
Norm_employee$Experience_Years <- (employee$Experience_Years - min(employee$Experience_Years, na.rm = TRUE)) / (max(employee$Experience_Years, na.rm = TRUE) - min(employee$Experience_Years, na.rm = TRUE))
head(employee)



#Applying Transformation to fix skewness

#Salary
library(e1071)
skewness_value <- skewness(Norm_employee$Salary)
print(skewness_value)
hist(Norm_employee$Salary, main="Salary Distribution", xlab="Salary")  #showing Original salary Histogram

#Fixing Salary Skewness using log 
Norm_employee$Salary <- log(Norm_employee$Salary + 1)           # +1 to avoid negative/NaN values; Original salary replaced with log salary in same variable



#Experience_Years
library(e1071)
sal_skewness_value <- skewness(Norm_employee$Experience_Years)
print(sal_skewness_value)
hist(Norm_employee$Experience_Years, main="Experience_Years Distribution", xlab="Experience_Years")  #showing Original Experience_Years Histogram

#Fixing Experience_Years Skewness using log 
Norm_employee$Experience_Years <- log(Norm_employee$Experience_Years + 1)           # +1 to avoid negative/NaN values; Original salary replaced with log salary in same variable



New_employees<-employee   #New table for Feature Engineering

#Feature_Engineering
New_employees$Sal_per_Exp<-employee$Salary/employee$Experience_Years  #Creating a new table for Salary per 

