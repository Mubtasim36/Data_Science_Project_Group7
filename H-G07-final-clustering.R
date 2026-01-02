#Clustering
#A. Data Collection

#Dataset Source: Kaggle
#Dataset Link: https://www.kaggle.com/datasets/kundanbedmutha/exam-score-prediction-dataset


#Dataset Description: This dataset provides an extensive and realistic representation of 
#various factors that contribute to student exam performance. It contains 20,000 records, 
#each describing a student’s academic behavior, study habits, lifestyle routines, and exam 
#conditions. These variables collectively help understand how different aspects of a student’s 
#daily life and learning environment influence their exam outcomes.



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



#Extra-
#Creating NA values:
#Score_predict <- read.csv("Exam_Score_Prediction.csv")
#Score_predict[Score_predict == ""] <- NA
#colSums(is.na(Score_predict))
#write.csv(Score_predict, "Exam_Score_Prediction.csv", row.names = FALSE, na = "")



#1. reading the Dataset
url <- "https://raw.githubusercontent.com/Mubtasim36/Data_Science_Project_Group7/refs/heads/master/Exam_Score_Prediction.csv"
Score_predict <- read.csv(url)
colnames(Score_predict) #To see column names



#B. Data Understanding & Exploration 

#Shape of the data set 
print(paste("The shape of the dataset is: ",nrow(Score_predict),"x",ncol(Score_predict)))


#Showing data type
sapply(Score_predict, class)


#Showing Summary Statistics
summary(Score_predict)


#.Identify categorical and numerical features
sapply(Score_predict, function(col) {
  if (is.numeric(col)) "Numerical Feature" else "Categorical Feature"
})


#Creating Histogram
hist(Score_predict$age)
hist(Score_predict$study_hours)
hist(Score_predict$class_attendance)
hist(Score_predict$sleep_hours)
hist(Score_predict$exam_score)



#Creating Box-plot for Each Numerical Data
library(ggplot2)
ggplot(Score_predict, aes(y = age)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot of age", y = "age")

ggplot(Score_predict, aes(y = study_hours)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot of study_hours", y = "study_hours")


ggplot(Score_predict, aes(y = class_attendance)) +
  geom_boxplot(fill = "red") +
  labs(title = "Boxplot of class_attendance", y = "class_attendance")


ggplot(Score_predict, aes(y = sleep_hours)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Boxplot of sleep_hours", y = "sleep_hours")


ggplot(Score_predict, aes(y = exam_score)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Boxplot of exam_score", y = "exam_score")



#Scatterplots:

ggplot(Score_predict, aes(x = age, y = study_hours )) +
  geom_point() +     #Adds scatter plot points (one point for each Score_predict).
  geom_smooth(method = "lm", se = FALSE, color = "blue") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "age vs StudyHours", x = "age", y = "study_hours ")

ggplot(Score_predict, aes(x = exam_score, y = sleep_hours )) +
  geom_point() +     #Adds scatter plot points (one point for each exam_score).
  geom_smooth(method = "lm", se = FALSE, color = "red") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "exam_score vs SleepHours", x = "exam_score", y = "sleep_hours ")

ggplot(Score_predict, aes(x = class_attendance, y = study_hours )) +
  geom_point() +     #Adds scatter plot points (one point for each exam_score).
  geom_smooth(method = "lm", se = FALSE, color = "blue") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "class_attendance vs study_hours", x = "class_attendance", y = "study_hours ")

ggplot(Score_predict, aes(x = study_hours, y = sleep_hours )) +
  geom_point() +     #Adds scatter plot points (one point for each exam_score).
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "study_hours vs sleep_hours", x = "study_hours", y = "sleep_hours ")



#Heatmap

library(reshape2)

num_data <- Score_predict[sapply(Score_predict, is.numeric)]
corr_mat <- cor(num_data)

melted_corr_mat <- melt(corr_mat)       #Convert matrix into long format

library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,fill=value)) + 
  geom_tile()+                       #to draw the colored tiles for correlation values.
  ggtitle("Heatmap for Correlation")          #for heatmap title


#Identifying missing values
colSums(is.na(Score_predict))    #Column wise no. of NA values
cat("Total NA values:", sum(is.na(Score_predict)), "\n")    #Sum of number of NA values



#Identify Outliers

#Possible Outliers for exam_score

# Computing IQR 
Q1 <- quantile(Score_predict$exam_score, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(Score_predict$exam_score, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- Score_predict$exam_score[!is.na(Score_predict$exam_score) &  #!is.na to find outliers excluding NA values
                                         (Score_predict$exam_score < lower_bound | Score_predict$exam_score > upper_bound)]

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste("Performance Score ",format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in exam_score")
}


#Possible Outliers for age

# Computing IQR 
Q1 <- quantile(Score_predict$age, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(Score_predict$age, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- Score_predict$age[!is.na(Score_predict$age) &  #!is.na to find outliers excluding NA values
                           (Score_predict$age < lower_bound | Score_predict$age > upper_bound)]

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste("age ",format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in age")
}


#Possible Outliers for class_attendance

# Computing IQR 
Q1 <- quantile(Score_predict$class_attendance, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(Score_predict$class_attendance, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- unique(Score_predict$class_attendance[!is.na(Score_predict$class_attendance) &  #!is.na to find outliers excluding NA values
                                     (Score_predict$class_attendance < lower_bound | Score_predict$class_attendance > upper_bound)])

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste("class_attendance ",format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in class_attendance")
}


#Possible Outliers for study_hours

# Computing IQR 
Q1 <- quantile(Score_predict$study_hours, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(Score_predict$study_hours, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- unique(Score_predict$study_hours[!is.na(Score_predict$study_hours) &  #!is.na to find outliers excluding NA values
                                               (Score_predict$study_hours < lower_bound | Score_predict$study_hours > upper_bound)])

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste("Study Hour ",format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in study_hours")
}



#Relationships shown using correlation
numeric_data <- Score_predict[sapply(Score_predict, is.numeric)]  #Finds numeric data
corr_matrix <- cor(numeric_data, use = "complete.obs")   #cor() calculates pairwise correlations between all numeric columns.    use = "complete.obs" ensures that only rows without missing values are considered for each pair of columns.
print("Correlation matrix:")
print(corr_matrix)   



#C. Data Preprocessing 


#Handling Missing Values

#Detect missing values
colSums(is.na(Score_predict))    #Column wise no. of NA values
cat("Total NA values:", sum(is.na(Score_predict)), "\n")    #Sum of number of NA values

#Removing NA Values in exam_score column
Score_predict <- Score_predict[!is.na(Score_predict$exam_score), ]

#Checking NA values after removing missing values
cat("Total NA values in exam_score after cleaning:", sum(is.na(Score_predict$exam_score)), "\n")




#Replacing NA values of class_attendance with the Mean of class_attendance
Score_predict$class_attendance[is.na(Score_predict$class_attendance)] <- mean(Score_predict$class_attendance, na.rm = TRUE)
cat("Total NA values after replacing:", sum(is.na(Score_predict)), "\n") #Sum of number of NA values in  new Dataframe



#Handling Outliers

range(Score_predict$study_hours) #to find the non-outlier values  range
#Capping study_hours; As there may be real performers among the Outliers, so taking an appropriate low and a high value to cap the scores
lower_limit <- 0.08   #As most low values are close to 0.08
upper_limit <- 12    #As most high values are close to 12
# Cap the outliers
Score_predict$study_hours <- pmin(pmax(Score_predict$study_hours, lower_limit), upper_limit)



#Checking for Outliers after Capping
# Computing IQR 
Q1 <- quantile(Score_predict$study_hours, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(Score_predict$study_hours, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- Score_predict$study_hours[!is.na(Score_predict$study_hours) &  #!is.na to find outliers excluding NA values
                                         (Score_predict$study_hours < lower_bound | Score_predict$study_hours > upper_bound)]

if(length(outliers) > 0){
  for(val in outliers){
    print(paste(format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in study_hours")
}



#Encoding Categorical values

# One Hot Encoding for gender
gender_encode <- model.matrix(~ gender - 1, data = Score_predict)  #model.matrix() function to change categorical values to numerical     ~gender means OHE on gender column   #-1 is to create 1 col per category
# Combining with original Dataset
Encoded_Score_predict <- cbind(Score_predict, gender_encode)      #cbind to add the new columns to a new table
head(Encoded_Score_predict) #checking if OHE was added



#One Hot Encoding for internet_access
internet_encode <- model.matrix(~ internet_access - 1, data = Score_predict)

# Combining with original Dataset
Encoded_Score_predict <- cbind(Score_predict, internet_encode)


#Normalizing Numeric Values using Min-Max Scaling

Norm_Score_predict<-Score_predict #Creating new Table for Other Tasks so that original table stays same

#For class_attendance
Norm_Score_predict$class_attendance <- (Score_predict$class_attendance - min(Score_predict$class_attendance, na.rm = TRUE)) / (max(Score_predict$class_attendance, na.rm = TRUE) - min(Score_predict$class_attendance, na.rm = TRUE))
head(Norm_Score_predict)

#For study_hours
Norm_Score_predict$study_hours <- (Score_predict$study_hours - min(Score_predict$study_hours, na.rm = TRUE)) / (max(Score_predict$study_hours, na.rm = TRUE) - min(Score_predict$study_hours, na.rm = TRUE))
head(Norm_Score_predict)



#Applying Transformation to fix skewness

#class_attendance
library(e1071)
skewness_value <- skewness(Norm_Score_predict$class_attendance)
print(skewness_value)
hist(Norm_Score_predict$class_attendance, main="class_attendance Distribution", xlab="class_attendance")  #showing Original class_attendance Histogram


#Fixing class_attendance Skewness using log 
Norm_Score_predict$class_attendance <- log(Norm_Score_predict$class_attendance + 1)           # +1 to avoid negative/NaN values; Original salary replaced with log salary in same variable



#study_hours
library(e1071)
sal_skewness_value <- skewness(Norm_Score_predict$study_hours)
print(sal_skewness_value)
hist(Norm_Score_predict$study_hours, main="study_hours Distribution", xlab="study_hours")  #showing Original study_hours Histogram

#Fixing study_hours Skewness using log 
Norm_Score_predict$study_hours <- log(Norm_Score_predict$study_hours + 1)           # +1 to avoid negative/NaN values; Original salary replaced with log salary in same variable



New_Score_predicts<-Score_predict   #New table for Feature Engineering

#Feature_Engineering
New_Score_predicts$score_per_hoursSleep<-Score_predict$exam_score/Score_predict$sleep_hours  #Creating a new table for class_attendance per 
head(New_Score_predicts)



