telcoData <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", header = TRUE)
names(telcoData)
head(telcoData)

####
library(tidyverse) # metapackage with lots of helpful functions
library(ROSE)

# remove observations with missing values
telcoData = na.omit(telcoData)
summary(telcoData) #demonstrates removal of the missing values.

# show class imbalance
table(telcoData$Churn)

# test to see if tenure * monthly charge = total charge
telcoData$MonthlyChargeByTenure = telcoData$tenure * telcoData$MonthlyCharges
# shows high similarity between MonthlyChargeByTenure and TotalCharges. Mild discrepancy due to discounts or failure to pay.
telcoData %>% select(MonthlyChargeByTenure, TotalCharges)
# due to high similarity to tenure*MonthlyCharges, remove total charges 
telcoData = subset (telcoData, select = -c(MonthlyChargeByTenure))
telcoData = subset (telcoData, select = -c(TotalCharges))
colnames(telcoData) #demonstrates removal of column

#removing customerID
telcoData = subset (telcoData, select = -c(customerID))

# streaming movies and streaming tv show high similarity, so omit one column:
telcoData %>% select(StreamingMovies, StreamingTV)
telcoData = telcoData %>% select(-c(StreamingTV))
#telcoData = subset (telcoData, select = -c(StreamingTV))
colnames(telcoData)

set.seed(1234)
View(telcoData)
#creating train dataset with 60% of actual data
telcoData_train <- floor(0.6 * nrow(telcoData))
telcoData_train
telcoData_train <- sample(1:nrow(telcoData), telcoData_train)
#Training data
telcoData_train_data <- telcoData[telcoData_train, ]
View(telcoData_train_data)

#Validation dataset
#validation dataset with remain data
telcoData_valid_data <- telcoData[-telcoData_train, ]
View(telcoData_valid_data)


# random oversampling using the ROSE ovun function
#oversampling done on train data
oversampleResult = ovun.sample(Churn~., data=telcoData_train_data, seed=1, method="over")
telcoDataBalanced = oversampleResult$data
table(telcoDataBalanced$Churn)
library(openxlsx)
write.xlsx(telcoDataBalanced, "telcoDataBalanced.xlsx")

---------------------------------------------------------------------------

#telcoDataBalanced$SeniorCitizen <- as.numeric(telcoDataBalanced$SeniorCitizen) 

str(telcoDataBalanced)

#replacing the No internet service with No on train data(telcoDataBalanced)
telcoDataBalanced$OnlineSecurity[telcoDataBalanced$OnlineSecurity == 'No internet service']  <- "No" 
telcoDataBalanced$OnlineBackup[telcoDataBalanced$OnlineBackup == 'No internet service']  <- "No" 
telcoDataBalanced$DeviceProtection[telcoDataBalanced$DeviceProtection == 'No internet service']  <- "No" 
telcoDataBalanced$TechSupport[telcoDataBalanced$TechSupport == 'No internet service']  <- "No"
telcoDataBalanced$StreamingMovies[telcoDataBalanced$StreamingMovies == 'No internet service']  <- "No" 

#replacing the No phone service with No on train data(telcoDataBalanced)
telcoDataBalanced$MultipleLines[telcoDataBalanced$MultipleLines == 'No phone service']  <- "No" 

#replacing the No internet service with No on train data(telcoDataBalanced)
#telcoDataBalanced$SeniorCitizen[telcoDataBalanced$SeniorCitizen == 0]  <- "No"
#telcoDataBalanced$SeniorCitizen[telcoDataBalanced$SeniorCitizen == 1]  <- "Yes" 
telcoDataBalanced$SeniorCitizen <- as.factor(telcoDataBalanced$SeniorCitizen)


telcoData_valid_data$OnlineSecurity[telcoData_valid_data$OnlineSecurity == 'No internet service']  <- "No" 
telcoData_valid_data$OnlineBackup[telcoData_valid_data$OnlineBackup == 'No internet service']  <- "No" 
telcoData_valid_data$DeviceProtection[telcoData_valid_data$DeviceProtection == 'No internet service']  <- "No" 
telcoData_valid_data$TechSupport[telcoData_valid_data$TechSupport == 'No internet service']  <- "No"
telcoData_valid_data$StreamingMovies[telcoData_valid_data$StreamingMovies == 'No internet service']  <- "No" 

telcoData_valid_data$MultipleLines[telcoData_valid_data$MultipleLines == 'No phone service']  <- "No" 

------------------------------------------------------------------------------
library(ggplot2)
library(lattice)

#demographic informations
ggplot(telcoData, aes(SeniorCitizen)) +
  geom_bar(fill = "#0073C2FF")
ggplot(telcoData, aes(gender)) +
  geom_bar(fill = "#0073C2FF")
ggplot(telcoData, aes(Partner)) +
  geom_bar(fill = "#0073C2FF")
ggplot(telcoData, aes(Dependents)) +
  geom_bar(fill = "#0073C2FF")


plot(telcoDataBalanced$SeniorCitizen,telcoDataBalanced$MonthlyCharges, xlab="Senior Citizen",ylab="Monthly Charges",col=c("red","blue")[telcoDataBalanced$gender])
plot(telcoData$Partner,telcoData$MonthlyCharges, xlab="Partner",ylab="Monthly Charges",col=c("red","blue")[telcoData$gender])
plot(telcoData$Dependents,telcoData$MonthlyCharges, xlab="Dependents",ylab="Monthly Charges",col=c("red","blue")[telcoData$gender])

plot(telcoData$Dependents,telcoData$MonthlyCharges, xlab="Dependents",ylab="Monthly Charges",col=c("red","blue")[telcoData$SeniorCitizen])

plot(telcoData$tenure,telcoData$MonthlyCharges, xlab="tenure",ylab="Monthly Charges",col=c("red","blue")[telcoData$gender])

boxplot(telcoData$MonthlyCharges~telcoData$Churn,col=c("blue","red"), xlab="Churn Indicator",ylab="Monthly Payments")
boxplot(telcoData$tenure~telcoData$Churn,col=c("blue","red"), xlab="Churn Indicator", ylab="Tenure")

library(ggplot2)
ggplot(telcoData, aes(y = MonthlyCharges, x = tenure, colour= Churn)) +
  geom_point(alpha = 0.6)
ggplot(telcoData, aes(y = Contract, x = tenure, colour= Churn)) +
  geom_point(alpha = 0.6)
ggplot(telcoData, aes(y = PaymentMethod, x = tenure, colour= Churn)) +
  geom_point(alpha = 0.6)
ggplot(telcoData, aes(y = InternetService, x = tenure, colour= Churn)) +
  geom_point(alpha = 0.6)
ggplot(telcoData, aes(y = PhoneService, x = tenure, colour= Churn)) +
  geom_point(alpha = 0.6)
ggplot(telcoData, aes(y = PaymentMethod, x = tenure, colour= Churn)) +
  geom_point(alpha = 0.6)

densityplot(~telcoData$MonthlyCharges | telcoData$gender)

telcoDataBalanced2 <- telcoDataBalanced
#telcoDataBalanced <- telcoDataBalanced2
str(telcoDataBalanced)

#Gender (2=male, 1=female)
telcoDataBalanced$gender = factor(telcoDataBalanced$gender, labels = c("1","2"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$gender, xlab="Monthly Charges")

#Senior Citizen(No =0, Yes=1)
telcoDataBalanced$SeniorCitizen = factor(telcoDataBalanced$SeniorCitizen, labels = c("0","1"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$SeniorCitizen, xlab="Monthly Charges")

#Partner(No =0, Yes=1)
telcoDataBalanced$Partner = factor(telcoDataBalanced$Partner, labels = c("0","1"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$Partner, xlab="Monthly Charges")

#Dependents(No =0, Yes=1)
telcoDataBalanced$Dependents = factor(telcoDataBalanced$Dependents, labels = c("0","1"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$Dependents, xlab="Monthly Charges")

#PhoneService(No =0, Yes=1)
telcoDataBalanced$PhoneService = factor(telcoDataBalanced$PhoneService, labels = c("0","1"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$PhoneService, xlab="Monthly Charges")

#MultipleLines(No =0, Yes=1)
telcoDataBalanced$MultipleLines = factor(telcoDataBalanced$MultipleLines, labels = c("0","1"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$MultipleLines, xlab="Monthly Charges")

#InternetService(DSL=0, Fiber=1, No =2)
telcoDataBalanced$InternetService = factor(telcoDataBalanced$InternetService, labels = c("0","1","2"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$InternetService, xlab="Monthly Charges")

#OnlineSecurity(No =0, Yes=1)
telcoDataBalanced$OnlineSecurity = factor(telcoDataBalanced$OnlineSecurity, labels = c("0","1"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$OnlineSecurity, xlab="Monthly Charges")

#OnlineBackup(No =0, Yes=1)
telcoDataBalanced$OnlineBackup = factor(telcoDataBalanced$OnlineBackup, labels = c("0","1"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$OnlineBackup, xlab="Monthly Charges")

#DeviceProtection(No =0, Yes=1)
telcoDataBalanced$DeviceProtection = factor(telcoDataBalanced$DeviceProtection, labels = c("0","1"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$DeviceProtection, xlab="Monthly Charges")

#TechSupport(No =0, Yes=1)
telcoDataBalanced$TechSupport = factor(telcoDataBalanced$TechSupport, labels = c("0","1"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$TechSupport, xlab="Monthly Charges")

#StreamingMovies(No =0, Yes=1)
telcoDataBalanced$StreamingMovies = factor(telcoDataBalanced$StreamingMovies, labels = c("0","1"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$StreamingMovies, xlab="Monthly Charges")

#Contract(M-M =0, 1Year=1, 2Year=2)
telcoDataBalanced$Contract = factor(telcoDataBalanced$Contract, labels = c("0","1","2"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$Contract, xlab="Monthly Charges")

#PaperlessBilling(No =0, Yes=1)
telcoDataBalanced$PaperlessBilling = factor(telcoDataBalanced$PaperlessBilling, labels = c("0","1"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$PaperlessBilling, xlab="Monthly Charges")

#PaymentMethod(Elec =2, Mailed=3, Bank=0, Credit=1)
telcoDataBalanced$PaymentMethod = factor(telcoDataBalanced$PaymentMethod, labels = c("0","1","2","3"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$PaymentMethod, xlab="Monthly Charges")

#Churn(No =0, Yes=1)
telcoDataBalanced$Churn = factor(telcoDataBalanced$Churn, labels = c("0","1"))
histogram(~telcoDataBalanced$MonthlyCharges |  telcoDataBalanced$Churn, xlab="Monthly Charges")

----------------------------------------------------------
str(telcoData_valid_data)
#Gender (2=male, 1=female)
telcoData_valid_data$gender = factor(telcoData_valid_data$gender, labels = c("1","2"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$gender, xlab="Monthly Charges")

#Senior Citizen(No =0, Yes=1)
telcoData_valid_data$SeniorCitizen = factor(telcoData_valid_data$SeniorCitizen, labels = c("0","1"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$SeniorCitizen, xlab="Monthly Charges")

#Partner(No =0, Yes=1)
telcoData_valid_data$Partner = factor(telcoData_valid_data$Partner, labels = c("0","1"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$Partner, xlab="Monthly Charges")

#Dependents(No =0, Yes=1)
telcoData_valid_data$Dependents = factor(telcoData_valid_data$Dependents, labels = c("0","1"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$Dependents, xlab="Monthly Charges")

#PhoneService(No =0, Yes=1)
telcoData_valid_data$PhoneService = factor(telcoData_valid_data$PhoneService, labels = c("0","1"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$PhoneService, xlab="Monthly Charges")

#MultipleLines(No =0, Yes=1)
telcoData_valid_data$MultipleLines = factor(telcoData_valid_data$MultipleLines, labels = c("0","1"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$MultipleLines, xlab="Monthly Charges")

#InternetService(DSL=0, Fiber=1, No =2)
telcoData_valid_data$InternetService = factor(telcoData_valid_data$InternetService, labels = c("0","1","2"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$InternetService, xlab="Monthly Charges")

#OnlineSecurity(No =0, Yes=1)
telcoData_valid_data$OnlineSecurity = factor(telcoData_valid_data$OnlineSecurity, labels = c("0","1"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$OnlineSecurity, xlab="Monthly Charges")

#OnlineBackup(No =0, Yes=1)
telcoData_valid_data$OnlineBackup = factor(telcoData_valid_data$OnlineBackup, labels = c("0","1"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$OnlineBackup, xlab="Monthly Charges")

#DeviceProtection(No =0, Yes=1)
telcoData_valid_data$DeviceProtection = factor(telcoData_valid_data$DeviceProtection, labels = c("0","1"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$DeviceProtection, xlab="Monthly Charges")

#TechSupport(No =0, Yes=1)
telcoData_valid_data$TechSupport = factor(telcoData_valid_data$TechSupport, labels = c("0","1"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$TechSupport, xlab="Monthly Charges")

#StreamingMovies(No =0, Yes=1)
telcoData_valid_data$StreamingMovies = factor(telcoData_valid_data$StreamingMovies, labels = c("0","1"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$StreamingMovies, xlab="Monthly Charges")

#Contract(M-M =0, 1Year=1, 2Year=2)
telcoData_valid_data$Contract = factor(telcoData_valid_data$Contract, labels = c("0","1","2"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$Contract, xlab="Monthly Charges")

#PaperlessBilling(No =0, Yes=1)
telcoData_valid_data$PaperlessBilling = factor(telcoData_valid_data$PaperlessBilling, labels = c("0","1"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$PaperlessBilling, xlab="Monthly Charges")

#PaymentMethod(Elec =2, Mailed=3, Bank=0, Credit=1)
telcoData_valid_data$PaymentMethod = factor(telcoData_valid_data$PaymentMethod, labels = c("0","1","2","3"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$PaymentMethod, xlab="Monthly Charges")

#Churn(No =0, Yes=1)
telcoData_valid_data$Churn = factor(telcoData_valid_data$Churn, labels = c("0","1"))
histogram(~telcoData_valid_data$MonthlyCharges |  telcoData_valid_data$Churn, xlab="Monthly Charges")


###########################Logistic regression - Prediction of churn customers####################
# use glm() (general linear model) with family = "binomial" to fit a logistic regression.



# Logistic Model1 - training data
str(telcoDataBalanced)
logit.reg1 <-glm(telcoDataBalanced$Churn ~ as.factor(telcoDataBalanced$gender) + as.factor(telcoDataBalanced$SeniorCitizen) + as.factor(telcoDataBalanced$Partner) + as.factor(telcoDataBalanced$Dependents) + telcoDataBalanced$tenure + as.factor(telcoDataBalanced$PhoneService) +
                   as.factor(telcoDataBalanced$MultipleLines) + as.factor(telcoDataBalanced$InternetService) + as.factor(telcoDataBalanced$OnlineSecurity) + as.factor(telcoDataBalanced$OnlineBackup) + as.factor(telcoDataBalanced$DeviceProtection) + as.factor(telcoDataBalanced$TechSupport) + as.factor(telcoDataBalanced$StreamingMovies) +
                   as.factor(telcoDataBalanced$Contract) + as.factor(telcoDataBalanced$PaperlessBilling) + as.factor(telcoDataBalanced$PaymentMethod) + telcoDataBalanced$MonthlyCharges,
                 family=binomial(link="logit"))
summary(logit.reg1)
library(mfx)
library(ROCR)
library(AUC)
library(pROC)
probVal1 <- predict(logit.reg1, type="response", data=telcoDataBalanced)
probVal1
aucTrain1 <- auc(telcoDataBalanced$Churn,probVal1)
aucTrain1

cut.off <- 0.5;
churnCheck1 <- (probVal1 > cut.off);
table(churnCheck1);
cmTableTrainMod1 <- table(telcoDataBalanced$Churn, as.numeric(churnCheck1))
cmTableTrainMod1

accTrainMod1 <- (cmTableTrainMod1[1] + cmTableTrainMod1[4])/ (cmTableTrainMod1[1] + cmTableTrainMod1[2] + cmTableTrainMod1[3] + cmTableTrainMod1[4])
accTrainMod1

# Logistic Model2 - training data
logit.reg2 <-glm(telcoDataBalanced$Churn ~  as.factor(telcoDataBalanced$SeniorCitizen) + telcoDataBalanced$tenure + as.factor(telcoDataBalanced$PhoneService) +
                   as.factor(telcoDataBalanced$OnlineSecurity) + as.factor(telcoDataBalanced$OnlineBackup) + as.factor(telcoDataBalanced$DeviceProtection) + as.factor(telcoDataBalanced$TechSupport)  +
                   as.factor(telcoDataBalanced$Contract) + as.factor(telcoDataBalanced$PaperlessBilling) + as.factor(telcoDataBalanced$PaymentMethod) + telcoDataBalanced$MonthlyCharges,
                 family=binomial(link="logit"))
summary(logit.reg2)

probVal2 <- predict(logit.reg2, type="response", data=telcoDataBalanced)
probVal2
aucTrain2 <- auc(telcoDataBalanced$Churn,probVal2)
aucTrain2

cut.off <- 0.5;
churnCheck2 <- (probVal2 > cut.off);
table(churnCheck2);
cmTableTrainMod2 <- table(telcoDataBalanced$Churn, as.numeric(churnCheck2))
cmTableTrainMod2

accTrainMod2 <- (cmTableTrainMod2[1] + cmTableTrainMod2[4])/ (cmTableTrainMod2[1] + cmTableTrainMod2[2] + cmTableTrainMod2[3] + cmTableTrainMod2[4])
accTrainMod2

# Logistic Model3 - training data
logit.reg3 <-glm(telcoDataBalanced$Churn ~  telcoDataBalanced$tenure + as.factor(telcoDataBalanced$PhoneService) + as.factor(telcoDataBalanced$Dependents) +
                   as.factor(telcoDataBalanced$OnlineSecurity) + as.factor(telcoDataBalanced$OnlineBackup) + as.factor(telcoDataBalanced$DeviceProtection) + as.factor(telcoDataBalanced$TechSupport)  +
                   as.factor(telcoDataBalanced$Contract) + telcoDataBalanced$MonthlyCharges + telcoDataBalanced$MonthlyCharges*telcoDataBalanced$tenure + as.factor(telcoDataBalanced$OnlineSecurity)*as.factor(telcoDataBalanced$OnlineBackup)*as.factor(telcoDataBalanced$DeviceProtection)*as.factor(telcoDataBalanced$TechSupport) ,
                 family=binomial(link="logit"))
summary(logit.reg3)

probVal3 <- predict(logit.reg3, type="response", data=telcoDataBalanced)
probVal3
aucTrain3 <- auc(telcoDataBalanced$Churn,probVal3)
aucTrain3

cut.off <- 0.5;
churnCheck3 <- (probVal3 > cut.off);
table(churnCheck3);
cmTableTrainMod3 <- table(telcoDataBalanced$Churn, as.numeric(churnCheck3))
cmTableTrainMod3

accTrainMod3 <- (cmTableTrainMod3[1] + cmTableTrainMod3[4])/ (cmTableTrainMod3[1] + cmTableTrainMod3[2] + cmTableTrainMod3[3] + cmTableTrainMod3[4])
accTrainMod3

# Logistic Model1 - validation data
Validlogit.reg1 <-glm(telcoData_valid_data$Churn ~ as.factor(telcoData_valid_data$gender) + as.factor(telcoData_valid_data$SeniorCitizen) + as.factor(telcoData_valid_data$Partner) + as.factor(telcoData_valid_data$Dependents) + telcoData_valid_data$tenure + as.factor(telcoData_valid_data$PhoneService) +
                   as.factor(telcoData_valid_data$MultipleLines) + as.factor(telcoData_valid_data$InternetService) + as.factor(telcoData_valid_data$OnlineSecurity) + as.factor(telcoData_valid_data$OnlineBackup) + as.factor(telcoData_valid_data$DeviceProtection) + as.factor(telcoData_valid_data$TechSupport) + as.factor(telcoData_valid_data$StreamingMovies) +
                   as.factor(telcoData_valid_data$Contract) + as.factor(telcoData_valid_data$PaperlessBilling) + as.factor(telcoData_valid_data$PaymentMethod) + telcoData_valid_data$MonthlyCharges,
                 family=binomial(link="logit"))
summary(Validlogit.reg1)

probValValid1 <- predict(Validlogit.reg1, type="response", data=telcoData_valid_data)
probValValid1
aucTrainValid1 <- auc(telcoData_valid_data$Churn,probValValid1)
aucTrainValid1

cut.off <- 0.5;
churnCheckValid1 <- (probValValid1 > cut.off);
table(churnCheckValid1);
cmTableValidMod1 <- table(telcoData_valid_data$Churn, as.numeric(churnCheckValid1))
cmTableValidMod1
cmTableValidMod1[3] 
accValidMod1 <- (cmTableValidMod1[1] + cmTableValidMod1[4])/ (cmTableValidMod1[1] + cmTableValidMod1[2] + cmTableValidMod1[3] + cmTableValidMod1[4])
accValidMod1

snValidMod1 <- (cmTableValidMod1[4] / (cmTableValidMod1[4] + cmTableValidMod1[3]) )
snValidMod1

spValidMod1 <- (cmTableValidMod1[1] / (cmTableValidMod1[1] + cmTableValidMod1[2]) )
spValidMod1

library(caret)
levels(telcoData_valid_data$Churn)
confusionMatrix(table(telcoData_valid_data$Churn,Validlogit.reg1), positive = levels(telcoData_valid_data$Churn)[2])

caret_matrix1 <- train(x=telco.train[,-c(18,6,13,11,7,2,4,10,3,12,9,1,15,8,16,14,17)], y=telco.train[,18], data=telco.train, method='rf', trControl=trainControl(method="cv", number=10))
caret_matrix1 
solution_rf1 <- predict(caret_matrix1, telco.valid)
confusionMatrix(table(telco.valid$Churn,solution_rf1),positive = 'Yes')

# Logistic Model2 - validation data
Validlogit.reg2 <-glm(telcoData_valid_data$Churn ~  as.factor(telcoData_valid_data$SeniorCitizen) + telcoData_valid_data$tenure + as.factor(telcoData_valid_data$PhoneService) +
                   as.factor(telcoData_valid_data$OnlineSecurity) + as.factor(telcoData_valid_data$OnlineBackup) + as.factor(telcoData_valid_data$DeviceProtection) + as.factor(telcoData_valid_data$TechSupport)  +
                   as.factor(telcoData_valid_data$Contract) + telcoData_valid_data$MonthlyCharges,
                 family=binomial(link="logit"))
summary(Validlogit.reg2)

probValValid2 <- predict(Validlogit.reg2, type="response", data=telcoData_valid_data)
probValValid2
aucValid2 <- auc(telcoData_valid_data$Churn,probValValid2)
aucValid2

cut.off <- 0.5;
churnCheckValid2 <- (probValValid2 > cut.off);
table(churnCheckValid2);
cmTableValidMod2 <- table(telcoData_valid_data$Churn, as.numeric(churnCheckValid2))
cmTableValidMod2

accValidMod2 <- (cmTableValidMod2[1] + cmTableValidMod2[4])/ (cmTableValidMod2[1] + cmTableValidMod2[2] + cmTableValidMod2[3] + cmTableValidMod2[4])
accValidMod2

snValidMod2 <- (cmTableValidMod2[4] / (cmTableValidMod2[4] + cmTableValidMod2[3]) )
snValidMod2

spValidMod2 <- (cmTableValidMod2[1] / (cmTableValidMod2[1] + cmTableValidMod2[2]) )
spValidMod2


# Logistic Model3 - validation data
Validlogit.reg3 <-glm(telcoData_valid_data$Churn ~  telcoData_valid_data$tenure + as.factor(telcoData_valid_data$PhoneService) + as.factor(telcoData_valid_data$Dependents) +
                    as.factor(telcoData_valid_data$OnlineSecurity) + as.factor(telcoData_valid_data$OnlineBackup) + as.factor(telcoData_valid_data$DeviceProtection) + as.factor(telcoData_valid_data$TechSupport)  +
                    as.factor(telcoData_valid_data$Contract) + telcoData_valid_data$MonthlyCharges + as.factor(telcoData_valid_data$OnlineSecurity)*as.factor(telcoData_valid_data$OnlineBackup)*as.factor(telcoData_valid_data$DeviceProtection)*as.factor(telcoData_valid_data$TechSupport) ,
                  family=binomial(link="logit"))
summary(Validlogit.reg3)

probValValid3 <- predict(Validlogit.reg3, type="response", data=telcoData_valid_data)
probValValid3
aucValid3 <- auc(telcoData_valid_data$Churn, probValValid3)
aucValid3

cut.off <- 0.5;
churnCheckValid3 <- (probValValid3 > cut.off);
table(churnCheckValid3);
cmTableValidMod3 <- table(telcoData_valid_data$Churn, as.numeric(churnCheckValid3))
cmTableValidMod3

accValidMod3 <- (cmTableValidMod3[1] + cmTableValidMod3[4])/ (cmTableValidMod3[1] + cmTableValidMod3[2] + cmTableValidMod3[3] + cmTableValidMod3[4])
accValidMod3

snValidMod3 <- (cmTableValidMod3[4] / (cmTableValidMod3[4] + cmTableValidMod3[3]) )
snValidMod3

spValidMod3 <- (cmTableValidMod3[1] / (cmTableValidMod3[1] + cmTableValidMod3[2]))
spValidMod3

