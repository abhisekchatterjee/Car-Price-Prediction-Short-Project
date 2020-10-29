library(randomForest)
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
#We have to categorize the owner column before merging te dataframes
dataset2$Owner=factor(dataset2$Owner,
                      levels = c("First Owner","Second Owner","Third Owner","Fourth & Above Owner","Test Drive Car"),
                      labels = c("0","1","2","3","4"))
unique(dataset2$Owner)
dataset1$Owner=factor(dataset1$Owner,
                      levels = c("0","1","3"),
                      labels = c("0","1","3"))

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
#from the previous model,discarding the outliers
dataset=dataset[-c(333,2977,2969),]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(1)
split = sample.split(dataset$Selling_Price, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Fitting the Random Forest Model
rf.Cardekho=randomForest(Selling_Price~.,data=training_set)
rf.Cardekho

# #Checking the mtry parameter interpretation
# oob.err=double(6)
# test.err=double(6)
# for(mtry in 1:6){
#   fit=randomForest(Selling_Price~.,data=training_set,mtry=mtry,ntree=400)
#   oob.err[mtry]=fit$mse[400]
#   pred=predict(fit,test_set)
#   test.err[mtry]=with(test_set,mean((Selling_Price-pred)^2))
#   cat(mtry," ")
# }
# 
# matplot(1:mtry,cbind(test.err,oob.err),pch=18,col=c("red","green"),type="b",
#         ylab="Mean Squared Error")
# legend("topright",legend=c("test","oob"),pch=18,col=c("red","green"))

y_pred2rf=predict(rf.Cardekho,test_set)
test_MSE2_rf=mean((test_set$Selling_Price-y_pred)^2)
test_MSE2_rf
