dataset1=read.csv("C:/Users/Abhishek/Desktop/Datasets from Kaggle and all/Vehicles Cardekho/car data.csv")
colnames(dataset1)
dataset2=read.csv("C:/Users/Abhishek/Desktop/Datasets from Kaggle and all/Vehicles Cardekho/CAR DETAILS FROM CAR DEKHO.csv")
#Renaming the columns of the second dataset to prepare for merging
colnames(dataset2)=c("Car_Name","Year","Selling_Price","Kms_Driven",   
                     "Fuel_Type","Seller_Type","Transmission","Owner")
#Scaling the response variable in the second dataset to adjust
dataset2$Selling_Price=dataset2$Selling_Price/100000

unique(dataset1$Owner)
unique(dataset2$Owner)
#We have to categorize the owner column before merging the dataframes
dataset2$Owner=factor(dataset2$Owner,
                      levels = c("First Owner","Second Owner","Third Owner","Fourth & Above Owner","Test Drive Car"),
                      labels = c("0","1","2","3","4"))
unique(dataset2$Owner)
dataset1$Owner=factor(dataset1$Owner,
                      levels = c("0","1","3"),
                      labels = c("0","1","3"))

#Merging the two datasets
merged=merge(dataset1,dataset2,by=c("Car_Name","Year","Selling_Price","Kms_Driven",   
                                    "Fuel_Type","Seller_Type","Transmission","Owner"),
             all.x = TRUE,all.y = TRUE)
dataset=merged[,1:8]

#Checking for duplicates and removing them
dups=dataset[!duplicated(dataset[2:8]),]
dim(dups)
dataset=dups


#creating an new column and getting rid of some columns 
dataset$Age=2020-dataset$Year
dataset=dataset[,3:9]

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
#Removing missing data
sum(is.na(dataset$Selling_Price))
sum(is.na(dataset$Kms_Driven))
sum(is.na(dataset$Age))
#Only categorical variables contain missing values
#Missing observations should be removed
dataset=na.omit(dataset)
rownames(dataset) <- 1:nrow(dataset)

#Let us first check for outliers
LM2_F=lm(Selling_Price~.,data = dataset)
par(mfrow=c(2,2))
plot(LM2_F,which = 1:4)
#So we observe that 365,3675 and 3667 are possible outliers
dataset=dataset[-c(333,2977,2969),]

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
rownames(training_set) <- 1:nrow(training_set)
rownames(test_set) <- 1:nrow(test_set)

#Fitting the linear regression
LM=lm(Selling_Price~.,data=training_set)
summary(LM)
par(mfrow=c(2,2))
plot(LM,which = 1:4)

#Checking the model assumptions
pairs(training_set)
X=data.matrix(training_set[,2:7])
det(t(X)%*%X)
e=LM$residuals
et=e[2:length(e)]
et1=e[1:length(e)-1]
d=sum((et-et1)^2)/sum((LM$residuals)^2)
plot(LM$residuals,training_set[,1])
#No Multicollinearity
#The linearity is moderately valid
#homoscedasticity assumption seem to be not that valid
#Autocorrelation is moderately positive
#Normality of the errors is moderately valid
#there are some outliers

#Box cox transformation and fitting again
library(MASS)
bc = boxcox(LM,lambda = seq(-3,3))
lam=bc$x[which(bc$y==max(bc$y))]
LMtrans=lm((((training_set$Selling_Price^lam)-1)/lam)~.,data=training_set)
summary(LMtrans)
par(mfrow=c(2,2))
plot(LMtrans,which = 1:4)

#Checking the assumptions again
e_2=LMtrans$residuals
et_2=e_2[2:length(e_2)]
et1_2=e_2[1:length(e_2)-1]
d=sum((et_2-et1_2)^2)/sum((LMtrans$residuals)^2)

#Predicting the test set values
y_pred2lr=predict(LMtrans,test_set)
#Transforming again to get the original predictions
y_pred2lr=lam*y_pred2lr+1
y_pred2lr=(y_pred2lr)^(1/lam)

test_MSE2_lr=mean((y_pred2lr-test_set$Selling_Price)^2)
test_MSE2_lr
