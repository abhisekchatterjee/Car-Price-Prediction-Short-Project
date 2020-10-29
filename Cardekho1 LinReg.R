#Importing the data
dataset=read.csv("C:/Users/Abhishek/Desktop/Datasets from Kaggle and all/Vehicles Cardekho/car data.csv")
colnames(dataset)

#creating an new column and getting rid of some columns 
dataset$Age=2020-dataset$Year
dataset=dataset[,3:10]

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

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Selling_Price, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set$Kms_Driven = scale(training_set$Kms_Driven)
test_set$Kms_Driven = scale(test_set$Kms_Driven)

#Fitting the linear regression
LM=lm(Selling_Price~.,data=training_set)
summary(LM)
par(mfrow=c(2,2))
plot(LM,which = 1:4)

#Checking the model assumptions
pairs(training_set)
X=data.matrix(training_set[,2:8])
det(t(X)%*%X)
e=LM$residuals
et=e[2:240]
et1=e[1:239]
d=sum((et-et1)^2)/sum((LM$residuals)^2)
d
plot(LM$residuals,training_set[,1])
#No Multicollinearity
#Although the linearity is not clear enough 
#homoscedasticity assumption seem to be not that valid
#Autocorrelation is not present
#Normality of the errors is moderately valid

#Box cox transformation and fitting again
library(MASS)
bc = boxcox(LM,lambda = seq(-3,3))
lam=bc$x[which(bc$y==max(bc$y))]
LMtrans=lm((((training_set$Selling_Price^lam)-1)/lam)~.,data=training_set)
summary(LMtrans)
par(mfrow=c(2,2))
plot(LMtrans,which = 1:4)

#We have to deal with outliers in both training and test sets,so let us fit the model for the entire dataset
LMtrans2=lm((((Selling_Price^lam)-1)/lam)~.,data=dataset)
par(mfrow=c(2,2))
plot(LMtrans2,which = 1:4)

#It seems that the observation 87 should be discarded first,because the cook's distance is
#almost 10.
LMtrans2_2=lm((((Selling_Price^lam)-1)/lam)~.,data=dataset[-87,])
par(mfrow=c(2,2))
plot(LMtrans2_2,which = 1:4)

#if we fix the threshold to k/n,then observations 86 and 197 should also be discarded
LMtrans2_3=lm((((Selling_Price^lam)-1)/lam)~.,data=dataset[-c(86,87,197),])
par(mfrow=c(2,2))
plot(LMtrans2_3,which = 1:4)

#Spitting the data again and fitting the model again
dataset_1F=dataset[-c(86,87,197),]
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset_1F$Selling_Price, SplitRatio = 0.8)
training_set = subset(dataset_1F, split == TRUE)
test_set = subset(dataset_1F, split == FALSE)

# Feature Scaling
training_set$Kms_Driven = scale(training_set$Kms_Driven)
test_set$Kms_Driven = scale(test_set$Kms_Driven)

#Fitting the linear regression
LM1_2=lm(Selling_Price~.,data=training_set)
summary(LM1_2)
par(mfrow=c(2,2))
plot(LM1_2,which = 1:4)
#We again see that we have to perform box-cox transformation
training_set$Kms_Driven=as.vector(training_set$Kms_Driven)
#Box cox transformation and fitting again

library(MASS)
bc = boxcox(LM1_2,lambda = seq(-3,3))
lam=bc$x[which(bc$y==max(bc$y))]
LMtrans=lm((((training_set$Selling_Price^lam)-1)/lam)~.,data=training_set)
summary(LMtrans)
par(mfrow=c(2,2))
plot(LMtrans,which = 1:4)
#All our assumptions are satisfied

#predicting on test set
test_set$Kms_Driven=as.vector(test_set$Kms_Driven)
y_pred1lr=predict(LMtrans,test_set)

#Transforming again to get the original predictions
y_pred1lr=lam*y_pred1lr+1
y_pred1lr=(y_pred1lr)^(1/lam)

test_MSE1_lr=mean((test_set[,1]-y_pred1lr)^2)
test_MSE1_lr


#Predicting a new observation with two alternatives
test_set[61,2:8]=data.frame(Present_Price=5.5,Kms_Driven=1.777042,Fuel_Type=2,
                            Seller_Type=2,Transmission=1,Owner=0,Age=9
                            )
new_pred1=predict(LMtrans,test_set[61,],interval = 'prediction',level = 0.95)
new_pred1=lam*new_pred1+1
new_pred1=(new_pred1)^(1/lam)

test_set[62,2:8]=data.frame(Present_Price=5.5,Kms_Driven=1.777042,Fuel_Type=2,
                            Seller_Type=1,Transmission=1,Owner=0,Age=9
)
new_pred2=predict(LMtrans,test_set[62,],interval = 'prediction',level = 0.95)
new_pred2=lam*new_pred2+1
new_pred2=(new_pred2)^(1/lam)