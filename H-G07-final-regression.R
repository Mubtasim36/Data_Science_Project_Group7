#Regression
#A. Data Collection

#Dataset Source: Kaggle
#Dataset Link:https://www.kaggle.com/datasets/mirichoi0218/insurance


#Dataset Description:This dataset contains information on individuals covered by a health insurance 
#plan in the United States. It includes demographic, lifestyle, and health-related attributes such 
#as age, gender, body mass index (BMI), number of dependents, smoking status, and residential region. 
#The dataset also records the individual medical costs billed by health insurance. 
#With over a thousand records, the dataset reflects realistic characteristics and variability 
#commonly encountered in healthcare and insurance analytics.



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
url <- "https://raw.githubusercontent.com/Mubtasim36/Data_Science_Project_Group7/refs/heads/master/insurance.csv"
insurance_data <- read.csv(url)
colnames(insurance_data) #To see column names



#B. Data Understanding & Exploration 

#Shape of the data set 
print(paste("The shape of the dataset is: ",nrow(insurance_data),"x",ncol(insurance_data)))


#Showing data type
cat(                         #Cat is used to print multi line 
  "Data Type of age column:", class(insurance_data$age), "\n",
  "Data Type of sex column:", class(insurance_data$sex), "\n",
  "Data Type of bmi column:", class(insurance_data$bmi), "\n",
  "Data Type of children column:", class(insurance_data$children), "\n",
  "Data Type of smoker column:", class(insurance_data$smoker), "\n",
  "Data Type of region column:", class(insurance_data$region), "\n",
  "Data Type of charges column:", class(insurance_data$charges), "\n"
)


#Showing Summary Statistics
summary(insurance_data)


#.Identify categorical and numerical features
sapply(insurance_data, function(col) {
  if (is.numeric(col)) {
    return("Numerical Feature")
  } else if (!is.numeric(col)) {
    return("Categorical Feature")
  }
})



#Creating Histogram
hist(insurance_data$age)
hist(insurance_data$bmi)
hist(insurance_data$children)
hist(insurance_data$charges)



#Creating Box-plot for Each Numerical Data
library(ggplot2)
ggplot(insurance_data, aes(y = age)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot of age", y = "age")

ggplot(insurance_data, aes(y = bmi)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot of bmi", y = "bmi")


ggplot(insurance_data, aes(y = children)) +
  geom_boxplot(fill = "red") +
  labs(title = "Boxplot of children", y = "children")


ggplot(insurance_data, aes(y = charges)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Boxplot of charges", y = "charges")



#Scatterplots:

ggplot(insurance_data, aes(x = age, y = charges )) +
  geom_point() +     #Adds scatter plot points (one point for each insurance_data).
  geom_smooth(method = "lm", se = FALSE, color = "blue") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "age vs charges", x = "age", y = "charges ")

ggplot(insurance_data, aes(x = age, y = bmi )) +
  geom_point() +     #Adds scatter plot points (one point for each insurance_data).
  geom_smooth(method = "lm", se = FALSE, color = "red") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "age vs bmi", x = "age", y = "bmi ")

ggplot(insurance_data, aes(x = children, y = smoker )) +
  geom_point() +     #Adds scatter plot points (one point for each insurance_data).
  geom_smooth(method = "lm", se = FALSE, color = "blue") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "children vs smoker", x = "children", y = "smoker ")

ggplot(insurance_data, aes(x = smoker, y = bmi )) +
  geom_point() +     #Adds scatter plot points (one point for each insurance_data).
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +     #Adds a linear regression line ("lm" = linear model).se = FALSE removes the shaded confidence interval.
  labs(title = "smoker vs bmi", x = "smoker", y = "bmi ")



#Heatmap

library(reshape2)

num_data <- insurance_data[sapply(insurance_data, is.numeric)]
corr_mat <- cor(num_data)

melted_corr_mat <- melt(corr_mat)       #Convert matrix into long format

library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,fill=value)) + 
  geom_tile()+                       #to draw the colored tiles for correlation values.
  ggtitle("Heatmap for Correlation")          #for heatmap title


#Identifying missing values
colSums(is.na(insurance_data))    #Column wise no. of NA values
cat("Total NA values:", sum(is.na(insurance_data$smoker)), "\n")    #Sum of number of NA values



#Identify Outliers

#Possible Outliers for region

# Computing IQR 
Q1 <- quantile(insurance_data$age, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(insurance_data$age, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- insurance_data$age[!is.na(insurance_data$age) &  #!is.na to find outliers excluding NA values
                                         (insurance_data$age < lower_bound | insurance_data$age > upper_bound)]

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste("Performance Score ",format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in age")
}


#Possible Outliers for bmi

# Computing IQR 
Q1 <- quantile(insurance_data$bmi, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(insurance_data$bmi, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- insurance_data$bmi[!is.na(insurance_data$bmi) &  #!is.na to find outliers excluding NA values
                           (insurance_data$bmi < lower_bound | insurance_data$bmi > upper_bound)]

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste("bmi ",format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in bmi")
}


#Possible Outliers for charges

# Computing IQR 
Q1 <- quantile(insurance_data$charges, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(insurance_data$charges, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- unique(insurance_data$charges[!is.na(insurance_data$charges) &  #!is.na to find outliers excluding NA values
                                     (insurance_data$charges < lower_bound | insurance_data$charges > upper_bound)])

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste("charges ",format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in charges")
}


#Possible Outliers for children

# Computing IQR 
Q1 <- quantile(insurance_data$children, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(insurance_data$children, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- unique(insurance_data$children[!is.na(insurance_data$children) &  #!is.na to find outliers excluding NA values
                                               (insurance_data$children < lower_bound | insurance_data$children > upper_bound)])

#Print using if-else
if(length(outliers) > 0){
  for(val in outliers){
    print(paste("Experience Year ",format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in children")
}



#Relationships shown using correlation
numeric_data <- insurance_data[sapply(insurance_data, is.numeric)]  #Finds numeric data
corr_matrix <- cor(numeric_data, use = "complete.obs")   #cor() calculates pairwise correlations between all numeric columns.    use = "complete.obs" ensures that only rows without missing values are considered for each pair of columns.
print("Correlation matrix:")
print(corr_matrix,4)   #Rounding to 4 decimal



#C. Data Preprocessing 


#Handling Missing Values

#Detect missing values
colSums(is.na(insurance_data))    #Column wise no. of NA values
cat("Total NA values:", sum(is.na(insurance_data$smoker)), "\n")    #Sum of number of NA values

#Removing NA Values in smoker column
insurance_data <- insurance_data[!is.na(insurance_data$smoker), ]

#Checking NA values after removing missing values
cat("Total NA values in smoker after cleaning:", sum(is.na(insurance_data$smoker)), "\n")


#Replacing NA values of region with the Mean of region
insurance_data$region[is.na(insurance_data$region)] <- mean(insurance_data$region, na.rm = TRUE)
cat("Total NA values after replacing:", sum(is.na(insurance_data)), "\n") #Sum of number of NA values in  new Dataframe



#Handling Outliers
range(insurance_data$bmi)
#Capping BMI; As there may be real performers among the Outliers, so taking an appropriate low and a high value to cap the scores
lower_limit <- 15   #As most low values are close to 15
upper_limit <- 46    #As most high values are close to 46
# Cap the outliers
insurance_data$bmi <- pmin(pmax(insurance_data$bmi, lower_limit), upper_limit)



#Checking for Outliers after Capping
# Computing IQR 
Q1 <- quantile(insurance_data$bmi, 0.25, na.rm = TRUE)    #quantile function, first value is the value to work on, the second is the position of the sorted data; in this case values are from 0 to 1 or 0 to 100%; so 0.25 is 25% and so on
Q3 <- quantile(insurance_data$bmi, 0.75, na.rm = TRUE)   #na.rm=TRUE to calculate without considering the NA values
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify Outliers 
outliers <- insurance_data$bmi[!is.na(insurance_data$bmi) &  #!is.na to find outliers excluding NA values
                                         (insurance_data$bmi < lower_bound | insurance_data$bmi > upper_bound)]

if(length(outliers) > 0){
  for(val in outliers){
    print(paste(format(val, scientific = FALSE), "is an outlier"))  #format(val, scientific = FALSE) to show high values fully
  }
} else {
  print("There are no outliers in bmi")
}



#Encoding Categorical values

# One Hot Encoding for sex
gender_encode <- model.matrix(~ sex - 1, data = insurance_data)  #model.matrix() function to change categorical values to numerical     ~sex means OHE on sex column   #-1 is to create 1 col per category
# Combining with original Dataset
Encoded1_insurance_data <- cbind(insurance_data, gender_encode)      #cbind to add the new columns to a new table
head(Encoded1_insurance_data) #checking if OHE was added



#One Hot Encoding for smoker
Dept_encode <- model.matrix(~ smoker - 1, data = insurance_data)

# Combining with original Dataset
Encoded2_insurance_data <- cbind(insurance_data, Dept_encode)
head(Encoded2_insurance_data) #checking if OHE was added


#Normalizing Numeric Values using Min-Max Scaling

Norm_insurance_data<-insurance_data #Creating new Table for Other Tasks so that original table stays same

#For charges
Norm_insurance_data$charges <- (insurance_data$charges - min(insurance_data$charges)) / (max(insurance_data$charges) - min(insurance_data$charges))

#For children
Norm_insurance_data$children <- (insurance_data$children - min(insurance_data$children, na.rm = TRUE)) / (max(insurance_data$children, na.rm = TRUE) - min(insurance_data$children, na.rm = TRUE))
head(Norm_insurance_data)



#Applying Transformation to fix skewness

#charges
library(e1071)
skewness_value <- skewness(Norm_insurance_data$charges)
print(skewness_value)
hist(Norm_insurance_data$charges, main="charges Distribution", xlab="charges")  #showing Original salary Histogram

#Fixing charges Skewness using log 
Norm_insurance_data$charges <- log(Norm_insurance_data$charges + 1)           # +1 to avoid negative/NaN values; Original salary replaced with log salary in same variable



#bmi
library(e1071)
bmi_skewness_value <- skewness(Norm_insurance_data$bmi)
print(bmi_skewness_value)
hist(Norm_insurance_data$bmi, main="bmi Distribution", xlab="bmi")  #showing Original bmi Histogram

#Fixing bmi Skewness using log 
Norm_insurance_data$bmi <- log(Norm_insurance_data$bmi + 1)           # +1 to avoid negative/NaN values; Original bmi replaced with log bmi in same variable
hist(Norm_insurance_data$bmi, main="bmi Distribution", xlab="bmi")  #showing new bmi Histogram



New_insurance_datas<-insurance_data   #New table for Feature Engineering

#Feature_Engineering
New_insurance_datas$charges_per_children<-insurance_data$charges/insurance_data$children  #Creating a new table for bmi per age
New_insurance_datas$charges_per_children[is.infinite(New_insurance_datas$charges_per_children)] <- 0  #to replace Inf(infinity value for 0 children) with 0; no child=no charge(only for this feature engineering)
head(New_insurance_datas)