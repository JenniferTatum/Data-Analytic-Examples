library(caret)
library(mlbench)
library(randomForest)
library(rpart)
library(rpart.plot)
library(party)
library(partykit)
library(gmodels)
library(e1071)
options(scipen = 999)
 
setwd("C:/Users/Jennifer/Documents/Pred Mod/Final")
HR<-read.csv("WatsonHRnoOT.csv")
HR<-HR[,c(2, 1, 3:22, 24:35)]
HR<-HR[,c(1, 2:8, 11:21, 23:25, 27:34)]
summary(HR)
dim(HR)
str(HR)

set.seed(123)
HR_rand <- HR[order(runif(1054)), ]
1054*.8
HR_train <- HR_rand[1:843,]
HR_test  <- HR_rand[844:1054,]

prop.table(table(HR_train$Attrition))
prop.table(table(HR_test$Attrition))

##################################################################
#Random Forest

set.seed(123) 
HR_bag <- randomForest(Attrition~., data=HR_train, mtry=29, na.action=na.omit, importance=TRUE)
print(HR_bag) 
importance(HR_bag)
varImpPlot(HR_bag)

actualRF <- HR_test$Attrition 
predictedRF <- predict(HR_bag, HR_test, type="class") 
HR_bag_matrix <- confusionMatrix(predictedRF, actualRF, positive="Yes") 
print("Bagged results")
print(HR_bag_matrix)

HR_RF <- randomForest(Attrition~., data=HR_train, mtry=3, ntree=100, na.action=na.omit, importance=TRUE)
print(HR_RF) 
importance(HR_RF)
varImpPlot(HR_RF)

actualRF2 <- HR_test$Attrition 
predictedRF2 <- predict(HR_RF, HR_test, type="class") 
HR_RF_matrix <- confusionMatrix(predictedRF2, actualRF2, positive="Yes") 
print("RF results")
print(HR_RF_matrix)

##################################################################
#Decision Tree

set.seed(123)
HR_DT <- rpart(HR_train$Attrition~., method="class", parms = list(split="gini"), data=HR_train)
rpart.plot(HR_DT, type=1, extra=101)

set.seed(123)
cptable<-printcp(HR_DT)
plotcp(HR_DT, minline=TRUE, col="red") 

set.seed(123)
Pruned_HR_DT <-prune(HR_DT,cp=.012, minsplit=10, minbucket=round(minsplit/3)) 
rpart.plot(Pruned_HR_DT, type=1, extra=101)
Pruned_HR_DT_party<-as.party(Pruned_HR_DT)
plot(Pruned_HR_DT_party)

set.seed(123)
Pruned_HR_DT2 <-prune(HR_DT,cp=.025, minsplit=10, minbucket=round(minsplit/3)) 
rpart.plot(Pruned_HR_DT2, type=1, extra=101)
Pruned_HR_DT2_party2<-as.party(Pruned_HR_DT2)
plot(Pruned_HR_DT2_party2)

actualFullDT <- HR_test$Attrition
predictedFullDT <- predict(HR_DT, HR_test, type="class")
results.matrix <- confusionMatrix(predictedFullDT, actualFullDT, positive="Yes")
print(results.matrix)

actualDT <- HR_test$Attrition
predictedDT <- predict(Pruned_HR_DT, HR_test, type="class")
DT.matrix <- confusionMatrix(predictedDT, actualDT, positive="Yes")
print(DT.matrix)

actualDT2 <- HR_test$Attrition
predictedDT2 <- predict(Pruned_HR_DT2, HR_test, type="class")
DT.matrix2 <- confusionMatrix(predictedDT2, actualDT2, positive="Yes")
print(DT.matrix2)

#########################################################################################
#Logistic Regression

set.seed(123)
HR_train_logit<-HR_train
HR_train_logit$TotalWorkingYears<-ifelse(HR_train_logit$TotalWorkingYears>=1.5,"no","yes")
HR.logit <- glm(HR_train_logit$Attrition~., data=HR_train_logit, family=binomial())
summary(HR.logit) 
names(HR_train_logit)

HR_train_logit2<-HR_train_logit[,c(1,3, 6, 9, 12, 15, 19, 23:24)]
HR.logit2 <- glm(HR_train_logit2$Attrition~., data=HR_train_logit2, family=binomial())
summary(HR.logit2) 

HR_train_logit3<-HR_train_logit[,c(1, 12, 14, 17, 24)]
HR.logit3 <- glm(HR_train_logit3$Attrition~., data=HR_train_logit3, family=binomial())
summary(HR.logit3) 

odds<-exp(cbind(Odds_Ratio=coef(HR.logit3)))
odds
prob<-odds/(1+odds)
prob
anova(HR.logit2,test="Chisq") 


HR_test_logit<-HR_test[,c(1,3, 6, 9, 12, 15, 19, 23:24)]
HR_test_logit$TotalWorkingYears<-ifelse(HR_test_logit$TotalWorkingYears>=1.5,"no","yes")
HR_test_logit$predict.Attrition<-predict(HR.logit2, newdata=HR_test_logit,type = "response")

HR_test_logit_CI<-cbind(HR_test_logit,predict(HR.logit2, newdata=HR_test_logit,type="link",se=TRUE))

HR_test_logit_CI <- within(HR_test_logit_CI, 
                                    {
                                      PredictedProb <- plogis(fit)
                                      LL <- plogis(fit - (1.96 * se.fit))
                                      UL <- plogis(fit + (1.96 * se.fit))
                                    })                               

summary(HR_test_logit_CI)
dim(HR_test_logit_CI)

HR_test_logit_CI$predict.Attrition<-ifelse(HR_test_logit_CI$predict.Attrition>.5, "yes", "no")
probs<-HR_test_logit_CI[,c(1,10)] 

CrossTable(x=probs$Attrition, y=probs$predict.Attrition, prob.chisq=FALSE)
TP = 2
TN = 187
FP = 0
FN = 22
Sensitivity = TP/(TP+FN) #true positive rate; recall; TP/(TP+FN)
Specificity = TN/(TN+FP) #how often is the prediction negative when actual is negative?

Precision = TP/(TP+FP) #how often is prediction positive when actual is positive?
Accuracy = (TP+TN)/(TP+TN+FP+FN) #how often is classifier correct
Value<-round(c(TP,TN,FP,FN,Sensitivity,Specificity,Precision,Accuracy),digits=3)
Measure<-c("True Positive","True Negative","False Positive","False Negative","Sensitivity=TP/(TN+FP)",
           "Specificity=TN/(TN+TP)","Precision=TP/(TP+FP)","Accuracy=(TP+TN)/total")
table<-as.data.frame(cbind(Measure,Value))
library(knitr)
kable(table)
