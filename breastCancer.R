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
train_index <- sample(1:nrow(cancer), 0.8 * nrow(cancer))
test_index <- setdiff(1:nrow(cancer), train_index)

#X_train <-cancer[train_index, -15]
#y_train <- cancer[train_index, "BMI"]

#X_test <-cancer[train_index, -15]
#y_test <- cancer[train_index, "BMI"]

#Data Wrangling
cancer$BMI_scale <- (cancer$BMI-mean(cancer$BMI))/sd(cancer$BMI)
cancer$target[cancer$Classification==2]=1
cancer$target[cancer$Classification==1]=0
View(cancer)
mod <- glm(formula=target ~ Age+BMI+Glucose+HOMA+Insulin+Leptin+Adiponectin+Resistin, family=binomial, data=cancer)
summary(mod)
set.seed(88)
split = sample.split(cancer$target, SplitRatio = 0.75)
cancerTrain = subset(cancer, split==TRUE)
View(cancerTrain)
cancerTest = subset(cancer, split==FALSE)
CancerLog = glm(target ~ Age + BMI + Glucose, family=binomial, data=cancerTrain)
summary(CancerLog)
cancerTrain$predictTrain = predict(CancerLog, type="response")

summary(predictTrain)
tapply(predictTrain, cancerTrain$target, mean)
table(cancerTrain$target, predictTrain > 0.5)
mod.res=resid(mod)

#Plots
plot(cancer$BMI, mod.res, ylab="Residuals", xlab="BMI", main="BMI Factors")
ROCRpred = prediction(predictTrain, cancerTrain$target)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize=TRUE)

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

boxplot(New_BMI ~ BMI, data = cancer,
        xlab = "Age", ylab = "Group BMI",
        main = "Age v. BMI"
)

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
