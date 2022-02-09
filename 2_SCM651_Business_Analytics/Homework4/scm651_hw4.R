library(caret)
library(tidyr)
library(car)
bankData<-read.csv("~/OneDrive - Syracuse University/651 Business Analytics/HW4/Homework 4 Data Set - Universal Bank.csv")

summary(bankData)

model.logit<-glm(formula = PersonalLoan ~ .,data=bankData,family=binomial(logit))
summary(model.logit)
vif(model.logit,bankData)


model.probit<-glm(formula = PersonalLoan ~ .,data=bankData,family=binomial(probit))
summary(model.probit)
vif(model.probit,bankData)

model.logit_2<-glm(formula = PersonalLoan ~ .^2,data=bankData,family=binomial(logit))
summary(model.logit_2)

model.logit_significant<-glm(formula = PersonalLoan ~ Age+Income+Family+CCAvg+Education+SecuritiesAccount+CDAccount+Online+CreditCard,data=bankData,family=binomial(logit))

summary(model.logit_significant)

#Second Question
formula_sig <- PersonalLoan ~ Age+Income+Family+CCAvg+Education+SecuritiesAccount+CDAccount+Online+CreditCard
model.logit_significant_2<-glm(formula = PersonalLoan ~ (Age+Income+Family+CCAvg+Education+SecuritiesAccount+CDAccount+Online+CreditCard)^2,data=bankData,family=binomial(logit))
summary(model.logit_significant_2)

#3rd Question 
interaction_vars<- c("Income","Family","CCAvg","Education","Income:Family","Income:CCAvg","Income:Education","Family:CCAvg","Family:Education","Family:SecuritiesAccount","Family:CDAccount","CCAvg:Education","Online:CreditCard")
interaction_vars_vif<-c(interaction_vars,"Age:Education","Age:SecuritiesAccount","Age:CDAccount")

formula_int<-paste("PersonalLoan", paste(interaction_vars_vif, collapse=" + "), sep=" ~ ")
model.logit_significant_interaction<-glm(formula =formula_int,data=bankData,family=binomial(logit))
summary(model.logit_significant_interaction)


#3rd Question 2nd attempt , lower AIC that before , skip it
interaction_vars2<- c("Income","Family","CCAvg","Education","Income:Family","Income:CCAvg","Income:Education","Family:CCAvg","Family:Education","CCAvg:Education","Online:CreditCard")
interaction_vars2<-c(interaction_vars2,"Age:CDAccount")
formula_int2<-paste("PersonalLoan", paste(interaction_vars2, collapse=" + "), sep=" ~ ")
model.logit_significant_interaction_2<-glm(formula =formula_int2,data=bankData,family=binomial(logit))
summary(model.logit_significant_interaction_2)


model.probit_significant_interaction_2<-glm(formula =formula_int2,data=bankData,family=binomial(probit))
summary(model.probit_significant_interaction_2)
#Logit
#Without VIF
# Everything - AIC: 1311.3
# With Significant Vars - AIC: 1306.6
# Significant Vars + moderating effect - AIC: 611.62
# Significant Vars + significant moderating effect - AIC: 604.4

#With VIF
#Significant vars + vif + moderating : AIC: 588.98

#Question 4
library(neuralnet)
model_nn <- neuralnet(formula_sig,bankData,hidden = 3,lifesign = "minimal", linear.output = FALSE,threshold = 0.1)
#summary(model_nn)
model_nn$result.matrix
#plot(model_nn)

formula_nn_sig<-PersonalLoan ~ Age+Income+Family+CCAvg+Education+CDAccount+Online+CreditCard
model_nn2 <- neuralnet(formula_nn_sig,bankData,hidden = 3,lifesign = "minimal", linear.output = FALSE,threshold = 0.1)
#summary(model_nn)
plot(model_nn2)
model_nn2$result.matrix

library(ggplot2)
plot <- ggplot(bankData,aes(x=Income,y=Family))+ geom_point() 
plot


plot1 <- ggplot(bankData,aes(x=CCAvg,y=Education,color=Education,size=PersonalLoan,alpha=I(0.1))) + geom_point()
plot1

#unique(bankData$Education)
library(sqldf)
groupData<-sqldf("select avg(PersonalLoan) as approval,Education,CCAvg as CCAvg from bankData group by CCAvg,Education ")
groupData$Education<-as.factor(groupData$Education)
#library(reshape2)
#melted<-melt(groupData,id=c("CCAvg"))
plot2 <- ggplot(groupData,aes(x=CCAvg,y=approval,group=Education,color=Education)) + geom_smooth()
plot2
                