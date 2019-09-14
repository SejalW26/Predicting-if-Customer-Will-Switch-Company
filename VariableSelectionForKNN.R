library(MASS)
library(dplyr)
library(ROSE)
library(stringr)
library(class)
library(caret)

SplitSets <- function(data, k){
  splits = floor(length(data)/k);
  set = c();
  for ( i in c(1:k) ){
    a = (i-1)*splits+1;
    b = (i)*splits;
    if ( b > length(data) ){
      b = length(data);
    }
    set = rbind(set, c(data[a:b], rep(0, i*splits - b)))
    #print(sets)
  }
  return(set)
}

# some test code
t = c(1:75)
SplitSets(t, k=10)

telcoData = read.csv('dataset/WA_Fn-UseC_-Telco-Customer-Churn.csv')

# edit columns such that each has only two factor levels
# remove no phone internet service factors
telcoData[telcoData$MultipleLines == "No phone service",]$MultipleLines = "No";
telcoData[telcoData$OnlineSecurity == "No internet service",]$OnlineSecurity = "No";
telcoData[telcoData$OnlineBackup == "No internet service",]$OnlineBackup = "No";
telcoData[telcoData$DeviceProtection == "No internet service",]$DeviceProtection = "No";
telcoData[telcoData$TechSupport == "No internet service",]$TechSupport = "No";
telcoData[telcoData$StreamingTV == "No internet service",]$StreamingTV = "No";
telcoData[telcoData$StreamingMovies == "No internet service",]$StreamingMovies = "No";


# create some additional dummy variables
telcoData$DSL = telcoData$InternetService == "DSL"
telcoData$Fiber = telcoData$InternetService == "Fiber optic"


# replace string columns spaces with _ (dummy var purposes for PaymentMethod and Contract)
telcoDataCats = telcoData %>% dplyr::select(PaymentMethod, Contract)
for ( i in c(1:ncol(telcoDataCats)) ){
  telcoDataCats[,i] = str_replace_all(telcoDataCats[,i], " ", "_")
}

dv = dummyVars(~., data = telcoDataCats)
telcoDataCatsDummy = data.frame(predict(dv, newdata = telcoDataCats))

# combine new dummy vars with original data (removing now redundant columns)
telcoDataBiVar = cbind( telcoData %>% select(-c(InternetService, PaymentMethod, Contract, customerID, TotalCharges)), telcoDataCatsDummy )

# covert all columns to logical then numerical
for ( i in c(1:ncol(telcoDataBiVar)) ){
  if ( is.numeric(telcoDataBiVar[,i]) ){
    next;
  } else {
    telcoDataBiVar[,i] = (telcoDataBiVar[,i] == "Yes" | telcoDataBiVar[,i] == "Female" | telcoDataBiVar[,i] == T)+0 
  }
}

# have a final test set
set.seed(2)
trIndices = sample(c(1:nrow(telcoDataBiVar)), size = 0.8 * nrow(telcoDataBiVar))
telcoDataBiVarTr = telcoDataBiVar[trIndices,]
telcoDataBiVarTe = telcoDataBiVar[-trIndices,]

# perform k-fold separation:
set.seed(1)
indices = sample( c(1:nrow(telcoDataBiVarTr)), size = nrow(telcoDataBiVarTr) )
folds = 10
indexSets = SplitSets(data = indices, folds)

# do k-fold validation
acc = c();
meanAcc = c();
cmr = c();
sens = c();
spec = c();
meanSens = c();
meanSpec = c();
kSet = c(1:15, 50, 100)
cnt = 1
varSets = list();
varSets[[1]] = c(1:ncol(telcoDataBiVar));
numSets = ncol(telcoDataBiVar)
NumNeighbors = 7
for ( m in c(2:(numSets-1)) ){
  meanAcc = c();
  for ( n in c(m:ncol(telcoDataBiVar)) ){
    nConsideredAlready = which( n %in% varSets[[m-1]] )
    if ( length(nConsideredAlready)==0 ){
      next; # skip if var already removed from previous subset
    }
    if ( n == 16 ){
      next; # skip for target column
    }
    #mk = c( varSets[[m-1]], n );
    idxToRemove = which( varSets[[m-1]] == n )
    mk = varSets[[m-1]][-idxToRemove]
    print(paste("n = ", n))
    for ( i in c(1:folds) ){
      holdOut = data.frame(telcoDataBiVarTr[ c(indexSets[i,]), mk])
      trainingSet = ovun.sample(Churn~., data=telcoDataBiVarTr[c(indexSets[-i,]), mk], seed=1, method="over")$data
      knnResults = knn(train = trainingSet, test = holdOut, cl = trainingSet$Churn, k=NumNeighbors)
      cmr = confusionMatrix(data = as.factor(knnResults), reference = as.factor(holdOut$Churn), positive = "1")
      acc[i] = cmr[["overall"]][['Accuracy']]
      sens[i] = cmr[["byClass"]][['Sensitivity']]
      spec[i] = cmr[["byClass"]][['Specificity']]
    }
    meanAcc[n] = mean(acc)
    meanSens[n] = mean(sens)
    meanSpec[n] = mean(spec)
    print("Mean Acc:")
    print(meanAcc[n])
    cnt = cnt + 1
  }
  bestMkIdx = which( varSets[[m-1]] == which.min( meanAcc ) );
  print(paste("Removing var= ", colnames(telcoDataBiVar)[bestMkIdx], sep=""))
  #varSets[[m]] = c(varSets[[m-1]], bestMkIdx);
  varSets[[m]] = varSets[[m-1]][-bestMkIdx]
}

# test out all chosen variable subsets
finalAcc = c();
numVars = c();
cms = list();
for ( i in (c(1:length(varSets))) ){
  vars = varSets[[i]]
  knnFinalTest = knn(train = telcoDataBiVarTrOs[,vars], test = telcoDataBiVarTe[,vars], cl = telcoDataBiVarTrOs$Churn, k=NumNeighbors)
  numVars[i] = length(vars)-1
  finalAcc[i] = sum(knnFinalTest == telcoDataBiVarTe$Churn)/nrow(telcoDataBiVarTe)
  cms[[i]] = confusionMatrix(data = as.factor(knnFinalTest), reference = as.factor(telcoDataBiVarTe$Churn), positive = "1")
}
plot(numVars, finalAcc, xlab="Number of Variables in Subset", ylab="Final Test Prediction Accuracy")
lines(lowess(numVars,finalAcc), col="blue")
bestVars = varSets[[which.max(finalAcc)]]
colnames(telcoDataBiVar)[bestVars]
cms[[which.max(finalAcc)]]