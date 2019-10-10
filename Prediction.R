getwd()

df <-  read.csv(file = "Insurance.csv")
str(df)

# age = An integer indicating the age of the primary beneficiary
# Gender = The policy holder's gender 
# bmi = The body mass index
# children = An integer indicating the num of children
# smoker = Whether the insured smokes or not
# region = Place of residence

colnames(df)[7] <- "Expences"

str(df)
table(df$region)

hist(df$Expences)
summary(df$Expences)
cor(df[-c(2,5,6)])

library(psych)
pairs.panels(df[-c(2,5,6)])

attach(df)
model <- lm(Expences ~ age + children + bmi + Gender + smoker + region, data = df)

model
summary(model)

df$age2 <- df$age^2

df$bmi30 <- ifelse(test = df$bmi >= 30, yes =  1,no = 0)

model2 <- lm(Expences ~ age + children + bmi + Gender + smoker + region + age2 +bmi30*smoker,
             data = df)
summary(model2)

test <- data.frame(35,"female",32,2,"no","northeast",30000,1225,1)

colnames(test) <- c("age","Gender","bmi","children","smoker","region","Expences","age2","bmi30")

pred <- predict(object = model2 , newdata = test)

pred
