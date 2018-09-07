#library
library(caTools)
library(dplyr)
library(ggplot2)
library(ISLR)
library(boot)
install.packages("corrplot")
library(corrplot)

cancer = read.csv("/Users/pooja.vasudevan/Downloads/cancerdata.csv")
summary(cancer)
str(cancer)
print(head(cancer, n=4))
View(cancer)

#Data Wrangling
cancer$BMI_scale <- (cancer$BMI-mean(cancer$BMI))/sd(cancer$BMI)
cancer$target[cancer$Classification==2]=1
cancer$target[cancer$Classification==1]=0
View(cancer)
mod <- glm(formula=target ~ Age+BMI+Glucose+HOMA+Insulin+Leptin+Adiponectin+Resistin, family=binomial, data=cancer)
step.mod <- step(mod)
step.mod
summary(step.mod)

#Cancer Train and Test Data
set.seed(88)
split = sample.split(cancer$target, SplitRatio = 0.75)
cancerTrain = subset(cancer, split==TRUE)
View(cancerTrain)
cancerTest = subset(cancer, split==FALSE)
head(cancerTrain)
CancerLog = glm(target ~ Age+BMI+Glucose+HOMA+Insulin+Leptin+Adiponectin+Resistin, family=binomial, data=cancerTrain)
final.mod2 <- step(CancerLog) #Step-wise model
plot(final.mod2)
summary(CancerLog)
View(cancerTest)
cancerTrain$predictTrain = predict(CancerLog, type="response")
pred = predict(CancerLog, newData=cancerTest, type="response")

cancerTrain$predictTrain2 = predict(final.mod2, type="response")
pred2 = predict(final.mod2, newData=cancerTest, type="response")

#Confusion Matrix - Total Model - Cancer Train
summary(predictTrain)
tapply(predictTrain, cancerTrain$target, mean)
table(cancerTrain$target, predictTrain > 0.6)

#Confusion Matrix - Total Model - Cancer Test
summary(pred)
tapply(pred, cancerTrain$target, mean)
table(cancerTrain$target, pred > 0.8)

#Confusion Matrix - Stepwise Model - Cancer Train
summary(predictTrain2)
tapply(predictTrain2, cancerTrain$target, mean)
table(cancerTrain$target, predictTrain2 > 0.6)

#Confusion Matrix - Step-wise Model - Cancer Test
summary(pred2)
tapply(pred2, cancerTrain$target, mean)
table(cancerTrain$target, pred2 > 0.85)

#Residuals
mod.res=resid(mod)
plot(mod)
plot(step.mod)

#ROC Curve - Cancer Train
ROCRpred = prediction(predictTrain, cancerTrain$target)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize=TRUE)

#ROC Curve - Cancer Test
ROCRpred = prediction(pred, cancerTrain$target)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize=TRUE)

#ROC Curve - Stepwise - Cancer Test
ROCRpred2 = prediction(pred2, cancerTrain$target)
ROCRpref2 = performance(ROCRpred2, "tpr", "fpr")
plot(ROCRpref2, colorize=TRUE)

#ROC Curve - Stepwise - Cancer Train
ROCRpred3 = prediction(predictTrain2, cancerTrain$target)
ROCRpref3 = performance(ROCRpred3, "tpr", "fpr")
plot(ROCRpref2, colorize=TRUE)

#Plots
plot(cancer$Age, cancer$BMI, col = cancer$target)
plot(cancer$BMI, cancer$Glucose, col = cancer$target)
plot(cancer$Glucose, cancer$HOMA, col = cancer$target)
plot(cancer$Leptin, cancer$BMI, col = cancer$target)
plot(cancerTrain$BMI, cancerTrain$predictTrain, col = cancer$target)
plot(cancerTrain$Insulin, cancerTrain$HOMA, col = cancer$target)
View(cancer)

cancer$New_BMI[which(cancer$BMI <= 19)] <-'Underweight'
cancer$New_BMI[which(cancer$BMI > 19 & cancer$BMI <= 25)] <-'Normal'
cancer$New_BMI[which(cancer$BMI > 25 & cancer$BMI <= 30)] <-'Overweight'
cancer$New_BMI[which(cancer$BMI > 30)] <- 'Obese'
View(cancer)

box_plot <- ggplot(cancer, aes(x = New_BMI, y = Age))
# Add the geometric object box plot
box_plot +
  geom_boxplot()

hist(cancer$Glucose, bin=50)
hist(cancer$BMI)

## LOOCV approach
set.seed(1)


#Fit a linear model
m = glm(BMI ~ Age, data = cancerTrain)
MSE_LOOCV = cv.glm(cancerTrain, m) #test model on data that hasb't been trained on
MSE_LOOCV$delta[1]

MSE_10_fold_cv = NULL
for(i in 1:10){
  m = glm(BMI~poly(Age, i), data=cancerTrain)
  MSE_10_fold_cv[i] = cv.glm(cancerTrain, m, K=10)$delta[1] ##divide data set in 10 parts
}
MSE_10_fold_cv 
MSE_LOOCV

cancer_feature_set <- cancer[1:9]
View(cancer_feature_set)
cor1 <- cor(cancer_feature_set)
corrplot(cor1, method="color")

cancer2 = unique(cancer)
str(cancer2)
distances = dist(cancer2, method="euclidean")
clusterCancer2 = hclust(distances, method="ward")
clusterGroups = cutree(clusterCancer2, k=10)
tapply(cancer2$BMI, clusterGroups, mean)
cluster2 = subset(cancer2, clusterGroups==2)
cluster2$BMI
plot(clusterCancer2)
