library(tidyverse)
library(ROSE)

telcoData = read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')
summary(telcoData)
View(telcoData)
# remove observations with missing values
telcoData = na.omit(telcoData)
summary(telcoData) 

# show class imbalance
table(telcoData$Churn)


#replacing the No internet service with No 
telcoData$OnlineSecurity[telcoData$OnlineSecurity == 'No internet service']  <- "No" 
telcoData$OnlineBackup[telcoData$OnlineBackup == 'No internet service']  <- "No" 
telcoData$DeviceProtection[telcoData$DeviceProtection == 'No internet service']  <- "No" 
telcoData$TechSupport[telcoData$TechSupport == 'No internet service']  <- "No"
telcoData$StreamingMovies[telcoData$StreamingMovies == 'No internet service']  <- "No" 

#replacing the No phone service with No
telcoData$MultipleLines[telcoData$MultipleLines == 'No phone service']  <- "No"

View(telcoData)


# test to see if tenure * monthly charge = total charge
telcoData$MonthlyChargeByTenure = telcoData$tenure * telcoData$MonthlyCharges
# shows high similarity between MonthlyChargeByTenure and TotalCharges. Mild discrepancy due to discounts or failure to pay.
telcoData %>% dplyr::select(MonthlyChargeByTenure, TotalCharges)
# due to high similarity to tenure*MonthlyCharges, remove total charges 
# Also, customer Id has no relation with response variable
# StreamingMovies is highly correlated with StreamingTV, we drop these 3 columns
telcoDataBalanced = telcoData %>% dplyr::select(-c(TotalCharges,customerID,StreamingTV))
colnames(telcoDataBalanced) #demonstrates removal of column
View(telcoDataBalanced)


# Splitting the data into test and train
num <- sample(row.names(telcoDataBalanced),4923) # 80 % for training & 20% for testing
telco.training <- telcoDataBalanced[num,]
num2 <- setdiff(row.names(telcoDataBalanced), num)
telco.valid <- telcoDataBalanced[num2,]
table(telco.training$Churn)

# random oversampling using the ROSE ovun function
oversampleResult = ovun.sample(Churn~., data=telco.training, seed=1, method="over")
telco.train = oversampleResult$data
table(telco.train$Churn)

# Finding the important variables
set.seed(100)
library(randomForest)
modrf<-randomForest(telco.train$Churn~.,data = telco.train,na.action = na.roughfix)
imp_RF <- importance(modrf)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]
ggplot(imp_DF[1:18,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")
# The output gives us a range of variables in descending order of their importance
# We will use this information to eliminate one by one, the low importance variables and 
# check corresponding model accuracy, for variable selection

library(caret)
# With all variables
caret_matrix <- train(x=telco.train[,-c(18)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=5))
caret_matrix #accuracy = 0.9001340
solution_rf <- predict(caret_matrix, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf),positive = 'Yes') #accuracy = 0.7576


# Eliminating PhoneServices 
caret_matrix1 <- train(x=telco.train[,-c(18,6)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix1 
solution_rf1 <- predict(caret_matrix1, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf1),positive = 'Yes') 

# Eliminating Streaming Movies 
caret_matrix2 <- train(x=telco.train[,-c(18,6,13)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix2 
solution_rf2 <- predict(caret_matrix2, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf2),positive = 'Yes') 

# Eliminating Device Protection  
caret_matrix3 <- train(x=telco.train[,-c(18,6,13,11)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix3 
solution_rf3 <- predict(caret_matrix3, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf3),positive = 'Yes') 

# Eliminating Multiple Lines   
caret_matrix4 <- train(x=telco.train[,-c(18,6,13,11,7)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix4 
solution_rf4 <- predict(caret_matrix4, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf4),positive = 'Yes') 

# Eliminating Senior Citizen     
caret_matrix5 <- train(x=telco.train[,-c(18,6,13,11,7,2)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix5 
solution_rf5 <- predict(caret_matrix5, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf5),positive = 'Yes') 

# Eliminating Dependents     
caret_matrix6 <- train(x=telco.train[,-c(18,6,13,11,7,2,4)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix6 
solution_rf6 <- predict(caret_matrix6, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf6),positive = 'Yes') 

# Eliminating Online Backup     
caret_matrix7 <- train(x=telco.train[,-c(18,6,13,11,7,2,4,10)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix7 
solution_rf7 <- predict(caret_matrix7, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf7),positive = 'Yes') 

# Eliminating Partner     
caret_matrix8 <- train(x=telco.train[,-c(18,6,13,11,7,2,4,10,3)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix8 
solution_rf8 <- predict(caret_matrix8, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf8),positive = 'Yes') 

# Eliminating Tech Support     
caret_matrix9 <- train(x=telco.train[,-c(18,6,13,11,7,2,4,10,3,12)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix9 
solution_rf9 <- predict(caret_matrix9, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf9),positive = 'Yes') 

# Eliminating Online Security    
caret_matrix10 <- train(x=telco.train[,-c(18,6,13,11,7,2,4,10,3,12,9)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix10
solution_rf10 <- predict(caret_matrix10, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf10),positive = 'Yes')

# Eliminating Gender   
caret_matrix11 <- train(x=telco.train[,-c(18,6,13,11,7,2,4,10,3,12,9,1)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix11
solution_rf11 <- predict(caret_matrix11, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf11),positive = 'Yes')

# Eliminating Paperless Billing   
caret_matrix12 <- train(x=telco.train[,-c(18,6,13,11,7,2,4,10,3,12,9,1,15)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix12
solution_rf12 <- predict(caret_matrix12, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf12),positive = 'Yes')

# Eliminating Internet Service   
caret_matrix13 <- train(x=telco.train[,-c(18,6,13,11,7,2,4,10,3,12,9,1,15,8)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix13
solution_rf13 <- predict(caret_matrix13, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf13),positive = 'Yes')

# Eliminating Payment Method    
caret_matrix14 <- train(x=telco.train[,-c(18,6,13,11,7,2,4,10,3,12,9,1,15,8,16)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix14
solution_rf14 <- predict(caret_matrix14, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf14),positive = 'Yes')

# Eliminating Contract   
caret_matrix15 <- train(x=telco.train[,-c(18,6,13,11,7,2,4,10,3,12,9,1,15,8,16,14)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix15
solution_rf15 <- predict(caret_matrix15, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf15),positive = 'Yes')

# Eliminating Monthly Charges   
caret_matrix16 <- train(x=telco.train[,-c(18,6,13,11,7,2,4,10,3,12,9,1,15,8,16,14,17)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix16
solution_rf16 <- predict(caret_matrix16, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf16),positive = 'Yes')