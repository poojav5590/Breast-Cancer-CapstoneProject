getwd()
titanic = read.csv("titanic_original.csv")
str(titanic)
View(titanic)
titanic$embarked[titanic$embarked==""] <- "S"
titanic$age[titanic$age=="NA"] <- mean(titanic$age)
titanic$embarked <- replace(titanic$embarked, titanic$embarked=="", "S")
titanic$age <- replace(titanic$age, titanic$age==NA, mean(titanic$age))
m <- mean(titanic$age)
m
me <- mean(titanic$age, na.rm=TRUE)
me
titanic$age <- replace(titanic$age, titanic$age=NA, mean(titanic$age))
titanic$age[is.na(titanic$age)] <- me
titanic$boat[titanic$boat==""] <- NA
titanic$has_cabin_number <- titanic$cabin
titanic$has_cabin_number <- ifelse(titanic$cabin == "", 0, 1)
write.csv(titanic, "titanic_clean.csv")



