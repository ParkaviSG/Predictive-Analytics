library(dplyr)
library(caret)
dat <- read.csv(file.choose())
dt <- read.csv(file.choose())
dat$Embarked [dat$Embarked==""] <- "S"
dat$Age[is.na(dat$Age)] <- median(dat$Age,na.rm=T)
titanic_data <-dat %>% select(-c(Cabin, PassengerId, Ticket, Name))
for (i in c("Survived","Pclass","Sex","Embarked")){
  titanic_data[,i]=as.factor(titanic_data[,i])
}
model <- glm(Survived ~.,family=binomial(link='logit'),data=dat)
anova(model, test="Chisq")
result <- predict(model,newdata=dt,type='response')
result <- ifelse(result > 0.5,1,0)
confusionMatrix(data=result, reference=test$Survived)