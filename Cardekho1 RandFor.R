library(dplyr)
#Importing the data
dataset=read.csv("C:/Users/Abhishek/Desktop/Datasets from Kaggle and all/Vehicles Cardekho/car data.csv")
colnames(dataset)

#creating an new column and getting rid of some columns and as we know about the outliers 
dataset$Age=2020-dataset$Year
dataset=dataset[-c(86,87,197),3:10]

# # Taking care of missing data
# dataset$Age = ifelse(is.na(dataset$Age),
#                      ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
#                      dataset$Age)
# dataset$Salary = ifelse(is.na(dataset$Salary),
#                         ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
#                         dataset$Salary)

# Encoding categorical data
#Checking the distinct values
unique(dataset$Fuel_Type)
unique(dataset$Seller_Type)
unique(dataset$Transmission)
#Encoding
dataset$Fuel_Type = factor(dataset$Fuel_Type,
                           levels = c('Petrol', 'Diesel', 'CNG'),
                           labels = c(1, 2, 3))
dataset$Seller_Type = factor(dataset$Seller_Type,
                             levels = c('Dealer', 'Individual'),
                             labels = c(1, 2))
dataset$Transmission = factor(dataset$Transmission,
                              levels = c('Manual', 'Automatic'),
                              labels = c(1, 2))

# #Removing missing data
# sum(is.na(dataset$Selling_Price))
# sum(is.na(dataset$Kms_Driven))
# sum(is.na(dataset$Age))
# #Only categorical variables contain missing values
# #Missing observations should be removed
# dataset=na.omit(dataset)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Selling_Price, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Fitting the Random Forest Model
library(randomForest)
rf.Cardekho1=randomForest(Selling_Price~.,data=training_set)
rf.Cardekho1

pred=predict(rf.Cardekho1,test_set)
test_MSE1_rf=with(test_set,mean((Selling_Price-pred)^2))
test_MSE1_rf
