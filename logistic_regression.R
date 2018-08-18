getwd()
setwd("/Users/pooja.vasudevan/Downloads")
NH11 <- readRDS("dataSets2/NatHealth2011.rds")
labs <- attributes(NH11)$labels
str(NH11$hypev)
levels(NH11$hypev)
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
               data=NH11, family="binomial")
coef(summary(hyp.out))
hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat)
install.packages("effects")
library("effects")
plot(allEffects(hyp.out))
View(NH11)
NH11$everwrk <- factor(NH11$everwrk, levels=c("2 No", "1 Yes"))

#Exercise
everwrk.out <- glm(everwrk~age_p+r_maritl,
               data=NH11, family="binomial")
coef(summary(everwrk.out))
plot(allEffects(everwrk.out))

predDat2 <- with(NH11,
                expand.grid(age_p = 33,
                            r_maritl=c("Married", "Widowed", "Divorced", "Separated", "Never Married")))
cbind(predDat2, predict(everwrk.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata2 = predDat2))
