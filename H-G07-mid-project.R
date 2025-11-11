#Data Understanding

#reading the dataset
url <- "https://raw.githubusercontent.com/Mubtasim36/Data_Science_Project_Group7/refs/heads/master/employee_performance.csv"
employee <- read.csv(url)


#displaying few rows:
head(employee)


#shape of the data set 
print(paste("The shape of the dataset is: ",nrow(employee),"x",ncol(employee)))


#show data type
cat(
  "Data Type of Employee_ID column:", class(employee$Employee_ID), "\n",
  "Data Type of Age column:", class(employee$Age), "\n",
  "Data Type of Gender column:", class(employee$Gender), "\n",
  "Data Type of Department column:", class(employee$Department), "\n",
  "Data Type of Experience_Years column:", class(employee$Experience_Years), "\n",
  "Data Type of Performance_Score column:", class(employee$Performance_Score), "\n",
  "Data Type of Salary column:", class(employee$Salary), "\n"
)


#Generate basic descriptive statistics

stats <-function(employee){
  num_col<-employee[,sapply(employee,is.numeric)]
    statistics<-data.frame(
      Mean=mean(num_col)
    )
    
  }

stats(employee)

