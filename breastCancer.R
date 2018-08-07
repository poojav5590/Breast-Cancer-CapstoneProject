cancer = read.csv("/Users/pooja.vasudevan/Downloads/cancerdata.csv")
summary(cancer)
str(cancer)
print(head(cancer, n=4))
train_index <- sample(1:nrow(cancer), 0.8 * nrow(cancer))
test_index <- setdiff(1:nrow(cancer), train_index)

X_train <-cancer[train_index, -15]
y_train <- cancer[train_index, "BMI"]

X_test <-cancer[train_index, -15]
y_test <- cancer[train_index, "BMI"]

#Data Wrangling
cancer$target[cancer$Classification==2]=1
cancer$target[cancer$Classification==1]=0
View(cancer)
mod <- glm(formula=target ~ Age+BMI+Glucose, family=binomial, data=cancer)
summary(mod)
library(caTools)
set.seed(88)
split = sample.split(cancer$target, SplitRatio = 0.75)
cancerTrain = subset(cancer, split==TRUE)
View(cancerTrain)
cancerTest = subset(cancer, split==FALSE)
CancerLog = glm(target ~ Age + BMI, family=binomial, data=cancerTrain)
summary(CancerLog)
cancerTrain$predictTrain = predict(CancerLog, type="response")

summary(predictTrain)
tapply(predictTrain, cancerTrain$target, mean)
table(cancerTrain$target, predictTrain > 0.5)
plot(mod)
mod.res=resid(mod)
plot(cancer$BMI, mod.res, ylab="Residuals", xlab="BMI", main="BMI Factors")
ROCRpred = prediction(predictTrain, cancerTrain$target)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize=TRUE)

plot(cancer$Age, cancer$BMI, col = cancer$target)
plot(cancer$BMI, cancer$Glucose, col = cancer$target)
plot(cancerTrain$BMI, cancerTrain$predictTrain, col = cancer$target)
View(cancer)

cancer$BMI_scale <- (cancer$BMI-mean(cancer$BMI))/sd(cancer$BMI)

boxplot(BMI ~ Age, data = cancer,
        xlab = "Age", ylab = "Maximum BMI",
        main = "Age v. BMI"
)

hist(cancer$Glucose, bin=20)
hist(cancer$BMI)
