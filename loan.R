#Loading the Training Loan Data
train_loan <- read.csv("D:/Analytics_Vidhya/train_loan.csv")
#Loading the Testing Loan Data
test_loan <- read.csv("D:/Analytics_Vidhya/test_loan.csv")
#Adding the Loan_Status Column in Testing Dataset
test_loan$Loan_Status <- ""
#Combining the Training and Testing Dataset
combined <- rbind(train_loan,test_loan)

#Checking for Target Variable distribution
prop.table(table(train_loan$Loan_Status))

#Output of Proportion 0f Target Variable Distribution
#N         Y 
#0.3127036 0.6872964

#Finding out the Number of missing values
sapply(combined, function(x) sum(is.na(x)))
summary(combined)

#Output of missing values
#LoanAmount  Loan_Amount_Term    Credit_History     
#27                20                79  

#Treating the missing values
#finding out the datatypes
str(combined)
# Loan Amount is Integer Datatype
#Loan Amount Term is Integer Datatype
#Credit_History is Factor Datatype, but is displayed as Integer Datatype
combined$Credit_History <- as.factor(combined$Credit_History)

#Replace the missing values of Integer datatype with the mean value
combined$LoanAmount[which(is.na(combined$LoanAmount))] <- 142.5
combined$Loan_Amount_Term[which(is.na(combined$Loan_Amount_Term))] <- 360

#Replace the missing values of Factor Datatype with the Mode Value
combined$Credit_History[which(is.na(combined$Credit_History))] <- 1

##For a few Variables NA are displayed as Blank Space
combined$Gender[combined$Gender==""] <- "Male"
combined$Married[combined$Married==""] <- "Yes"
combined$Dependents[combined$Dependents==""] <- "0"
combined$Self_Employed[combined$Self_Employed==""] <- "No"
#Update the Training and The Testing Data
#Update train and test dataset
trainRows <- nrow(train_loan)
testRows <- nrow(test_loan)

train_data <- combined[1:trainRows,]
test_data <- combined[trainRows+1:testRows,]
str(test_data)
summary(test_data_1)

#Taking a copy of the Train_Data
train_data_1 <- train_data

train_data_1 <- train_data_1[,-1]
#Taking a copy of the Test Data
test_data_1 <- test_data
test_data_1 <- test_data_1[,-1]

train_data_1$Loan_Status <- ifelse(train_data_1$Loan_Status=="Y",1,0)
train_data_1$Loan_Status <- as.factor(train_data_1$Loan_Status)

#Running a Random Forest Model to come up with Prediction
rf_loan <- randomForest(Loan_Status~.,data=train_data_1,ntree=50)

test_data_1$Loan_Pred <- predict(rf_loan,test_data_1)

test_data_1$Loan_Pred <- ifelse(test_data_1$Loan_Pred==1,"Y","N")

write.csv(test_data_1,file="D:\\Analytics_Vidhya\\test_data.csv")
