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


#5.Generate basic descriptive statistics 1

stats <- function(employee) {
  
  num_col <- employee[, sapply(employee, is.numeric)]
  
  get_mode <- function(x) {
    freq_table <- table(x)
    names(freq_table)[which.max(freq_table)]
  }
  
  statistics <- data.frame(  #using na.rm to remove NA from calculations
    Mean = sapply(num_col, mean, na.rm = TRUE),                  #sapply(matrix or dataframe,function to apply)
    Median = sapply(num_col, median, na.rm = TRUE),
    Mode = sapply(num_col, get_mode),
    Std_Deviation = sapply(num_col, sd, na.rm = TRUE),
    Minimum = sapply(num_col, min, na.rm = TRUE),
    Maximum = sapply(num_col, max, na.rm = TRUE),
    Count = sapply(num_col, length)
  )  
  
  return(statistics)
}

stats(employee)



#5.descriptive stat using summary()
summary(employee)



#6.Identify categorical and numerical features
sapply(employee, function(col) {
  if (is.numeric(col)) {
    return("Numerical Feature")
  } else if (is.character(col)) {
    return("Categorical Feature")
  }
})


#B. Data Exploration & Visualization


