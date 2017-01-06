#Loading the Training and the Testing Dataset
train_bigmart <- read.csv("D:/Analytics_Vidhya/train_bigmart.csv",na.strings=c(""," ","NA"))
test_bigmart <- read.csv("D:/Analytics_Vidhya/test_bigmart.csv",na.strings=c(""," ","NA"))

combined <- rbind(train_bigmart,test_bigmart)

#checking the datatype of both Training and Testing Dataset
str(combined)
summary(combined)

#correcting irregularities in the Factor of Item_Fat_Content
combined$Item_Fat_Content[combined$Item_Fat_Content=="LF"] <- "Low Fat"
combined$Item_Fat_Content[combined$Item_Fat_Content=="low fat"] <- "Low Fat"
combined$Item_Fat_Content[combined$Item_Fat_Content=="reg"] <- "Regular"

#Replacing the Item Visibility as 0% with the median value
combined$Item_Visibility[combined$Item_Visibility==0.00000] <- 0.05402

#Calculating the number of missing values
sapply(combined, function(x) sum(is.na(x)))


trainRows <- nrow(train_bigmart)
testRows <- nrow(test_bigmart)

train_data <- combined[1:trainRows,]
test_data <- combined[trainRows+1:testRows,]

#Subsetting the Data without missing values Item Weight
Subs1_1<-subset(train_data, (!is.na(train_data$Item_Weight)))

#Summarize Item_Weight against Item Type
train_data%>%
  group_by(Item_Type)%>%
  summarise(Average = mean(Item_Weight, na.rm = TRUE))

#Replacing the missing value by the average weight of each Item Type

train_data_2 <- ddply(train_data, "Item_Type", mutate, Item_Weight = impute(Item_Weight, mean))
test_data_2 <- ddply(test_data, "Item_Type", mutate, Item_Weight = impute(Item_Weight, mean))


#Changing the class from imputed to a numeric 
train_data_2$Item_Weight <- as.numeric(train_data_2$Item_Weight)
test_data_2$Item_Weight <- as.numeric(test_data_2$Item_Weight)

#Taking a copy of train and test data
train_data_3 <- train_data_2
test_data_3 <- test_data_2


#For imputing missing values for Outlet Size - Training and Testing Dataset

train_data_3$Outlet_Size[is.na(train_data_3$Outlet_Size)]<-sample(train_data_3$Outlet_Size,length(train_data_3$Outlet_Size[is.na(train_data_3$Outlet_Size)]),replace=TRUE)
train_data_3$Outlet_Size[is.na(train_data_3$Outlet_Size)]<-sample(train_data_3$Outlet_Size,length(train_data_3$Outlet_Size[is.na(train_data_3$Outlet_Size)]),replace=TRUE)
train_data_3$Outlet_Size[is.na(train_data_3$Outlet_Size)]<-sample(train_data_3$Outlet_Size,length(train_data_3$Outlet_Size[is.na(train_data_3$Outlet_Size)]),replace=TRUE)

test_data_3$Outlet_Size[is.na(test_data_3$Outlet_Size)]<-sample(test_data_3$Outlet_Size,length(test_data_3$Outlet_Size[is.na(test_data_3$Outlet_Size)]),replace=TRUE)
test_data_3$Outlet_Size[is.na(test_data_3$Outlet_Size)]<-sample(test_data_3$Outlet_Size,length(test_data_3$Outlet_Size[is.na(test_data_3$Outlet_Size)]),replace=TRUE)
test_data_3$Outlet_Size[is.na(test_data_3$Outlet_Size)]<-sample(test_data_3$Outlet_Size,length(test_data_3$Outlet_Size[is.na(test_data_3$Outlet_Size)]),replace=TRUE)

###Now the Data is cleansed. The Final Training and Testing Dataset is cleansed.
##They are Train_Data_3 and Test_Data_3

#Predicting the Item Outlet Sales
str(train_data_3)
summary(train_data_3)
#Running a Regression Model
model_lm <- lm(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility
               +Item_Type+Item_MRP+Outlet_Size+Outlet_Location_Type
               +Outlet_Type,data=train_data_3)

model_lm_3 <- lm(Item_Outlet_Sales~Item_Fat_Content+Item_MRP+Outlet_Size+Outlet_Type,data=train_data_3)
summary(model_lm_3)
summary(model_lm)
#Running a Random Forest Model
model_rf <-randomForest(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility
             +Item_Type+Item_MRP+Outlet_Size+Outlet_Location_Type
             +Outlet_Type,data=train_data_3,ntree=500)

#Using the Random Forest Model to Predict the Item Outlet Sales
test_data_3$Item_Outlet_Sales <- predict(model_rf, test_data_3)

#Writing the test data to a csv file.
write.csv(test_data_3,file="D:/Analytics_Vidhya/Test_Data_output.csv")
