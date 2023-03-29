data <- read.csv("C:\\diabetes.csv")
str(data)
summary(data)

data[,'Outcome']=factor(data[,'Outcome'])
str(data)

data.subset <- data[c('Pregnancies','Glucose','BloodPressure','SkinThickness','Insulin','BMI','DiabetesPedigreeFunction','Age','Outcome')]
head(data.subset)

data_norm_func <- function(x){
  
  ((x - min(x))/ (max(x)-min(x)))
  
}

data.subset.n <- as.data.frame(lapply(data.subset[,1:8],data_norm_func)) 
str(data.subset.n)
summary(data.subset.n)

set.seed(146)
p <- 0.8 
train <- sample(nrow(data.subset.n),nrow(data.subset.n)*p)
str(train) 
summary(train) 
updated_data.train <- data.subset[train,] 
updated_data.val <- data.subset[-train,] 
str(updated_data.train)
summary(updated_data.train)
str(updated_data.val)
summary(updated_data.val)

updated_data.train_labels <- data.subset[train,9] 
updated_data.val_labels <- data.subset[-train,9]
str(updated_data.train_labels)
str(updated_data.val_labels)
NROW(updated_data.train_labels)
NROW(updated_data.val_labels)

#K - NEAREST NEIGHBOUR
library(class)  
Ypred_knn=knn(updated_data.train,  
              updated_data.val,    
              cl=updated_data.train_labels,                  
              k=25) 

updated_data.val$Pred_Outcome_knn = Ypred_knn
#View(updated_data.val)

confusion=table(Ypred_knn,updated_data.val_labels) 
confusion
sum(diag(confusion))/nrow(updated_data.val)

library('caret')
confusionMatrix(Ypred_knn,updated_data.val_labels)

#install.packages("pROC")
library('pROC')
par(pty="s")
roc(updated_data.val_labels, as.numeric(Ypred_knn), plot=TRUE, legacy.axes=TRUE, percent=TRUE, main="ROC curve for KNN",
    xlab="False Positive Percentage", ylab="True Positive Percentage")

#NAIVE BAYES
library(e1071)
NBM_Classifier= naiveBayes(Outcome~.,updated_data.train)
#NBM_Classifier
Pred_Outcome_NBM = predict(NBM_Classifier,updated_data.val)
updated_data.val$Pred_Outcome_NBM = Pred_Outcome_NBM
CMNB =table(updated_data.val$Pred_Outcome_NBM,updated_data.val_labels) 
CMNB
library('caret')
confusionMatrix(updated_data.val$Pred_Outcome_NBM,updated_data.val_labels)

library('pROC')
roc(updated_data.val_labels, as.numeric(updated_data.val$Pred_Outcome_NBM), plot=TRUE, legacy.axes=TRUE, percent=TRUE, main="ROC curve for NB",
    xlab="False Positive Percentage", ylab="True Positive Percentage")


#RANDOM FOREST
library('stats')
library('dplyr')
library('randomForest')
RFM_Classifier = randomForest(Outcome~.,updated_data.train)
#RFM_Classifier
Pred_Outcome_RFM = predict(RFM_Classifier,updated_data.val)
updated_data.val$Pred_Outcome_RFM = Pred_Outcome_RFM
#View(updated_data.val)
CMRF =table(updated_data.val$Pred_Outcome_RFM,updated_data.val_labels) 
CMRF
library('caret')
confusionMatrix(updated_data.val$Pred_Outcome_RFM,updated_data.val_labels)

library('pROC')
roc(updated_data.val_labels, as.numeric(updated_data.val$Pred_Outcome_RFM), plot=TRUE, legacy.axes=TRUE, percent=TRUE, main="ROC curve for RF",
    xlab="False Positive Percentage", ylab="True Positive Percentage")
