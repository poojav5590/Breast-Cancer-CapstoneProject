getwd()
setwd("/Users/pooja.vasudevan/Downloads")
list.files("dataSets")
states.data <- readRDS("dataSets/states.rds")
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
tail(states.info, 8)
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
cor(sts.ex.sat)
plot(sts.ex.sat)
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data)
summary(sat.mod)
summary(lm(csat ~ expense + percent, data = states.data))
class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]
confint(sat.mod)
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
plot(sat.mod, which = c(1, 2))
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))
View(states.data)
#Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro) - more indicators didn't make the model significantly better
sat.mod.energy <- lm(energy ~ percent+pop,
                             data=states.data) 
summary(sat.mod.energy)
plot(sat.mod.energy)

###Modeling interactions
#Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
coef(summary(sat.expense.by.percent)) 
str(states.data$region)
states.data$region <- factor(states.data$region)
sat.region <- lm(csat ~ region,
                 data=states.data) 
coef(summary(sat.region))
anova(sat.region)
contrasts(states.data$region)
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##Exercise - interactions and factors
sat.mod.energy.by.percent <- lm(energy ~ percent*pop,
                     data=states.data) 
coef(summary(sat.mod.energy.by.percent))

sat.energy.region <- lm(energy ~ region,
                data=states.data) 
coef(summary(sat.energy.region))
coef(summary(lm(energy ~ C(region, base=4),
                data=states.data)))
