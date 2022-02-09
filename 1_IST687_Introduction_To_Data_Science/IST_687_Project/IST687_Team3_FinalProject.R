library(readxl)
library(lubridate)
library(dplyr)
library(zoo)
library(lmtest)
library(sqldf)
library(varhandle)
library(car)
library(caret)
library(tidyr)
library(tidyverse)    # data manipulation and visualization
library(kernlab)      # SVM methodology
library(e1071) 
#library(RColorBrewer)
library(arules)
library(arulesViz)

cleanData<- function(surveyDataRaw){
  columns <- colnames(surveyDataRaw)
  columns <- gsub(" ","_",columns)
  #columns <- gsub("\\.","",columns)
  columns <- gsub("%.","Percent_",columns)
  colnames(surveyDataRaw) <- columns
  
  
  numeric_columns<-colnames(surveyDataRaw)[!grepl('factor|logical|character',sapply(surveyDataRaw,class))]
  categorical_columns<-colnames(surveyDataRaw)[grepl('factor|logical|character',sapply(surveyDataRaw,class))]
  
  #surveyData <- na.aggregate(surveyDataRaw)
  #Does any column have NA
  sapply(surveyDataRaw,function(X) sum(is.na(X)))
  
  
  sapply(surveyDataRaw,function(X) sum(is.na(X)))
  
  #filter rows where Satisfaction is NA
  surveyDataRaw<-surveyDataRaw[!is.na(surveyDataRaw$Satisfaction),]
  
  #Set Departure delay and Arrival delay to mean if it is NA
  meanDepartureDelay<-mean(surveyDataRaw$Departure_Delay_in_Minutes,na.rm = TRUE)
  meanArrivalDelay<-mean(surveyDataRaw$Arrival_Delay_in_Minutes,na.rm = TRUE)
  
  surveyDataRaw$Departure_Delay_in_Minutes <- replace_na(surveyDataRaw$Departure_Delay_in_Minutes,meanDepartureDelay)
  surveyDataRaw$Arrival_Delay_in_Minutes <- replace_na(surveyDataRaw$Arrival_Delay_in_Minutes,meanArrivalDelay)
  
  #find the source-destination combination which has NA
  average_duration_by_path<-sqldf("select  avg(Flight_time_in_minutes) as average,Orgin_City,Destination_City from surveyDataRaw where Flight_time_in_minutes is not null group by Orgin_City,Destination_City")
  getAverageByFlightPath<- function(time,origin,destination){
    if(is.na(time)){
      avg<-average_duration_by_path[(average_duration_by_path$Orgin_City==origin & average_duration_by_path$Destination_City==destination),]["average"]
      avg <- as.list(avg)
      return(round(as.numeric(avg[1])))
    }
    return (time)
  }
  surveyDataRaw$Flight_time_in_minutes<-mapply(getAverageByFlightPath,surveyDataRaw$Flight_time_in_minutes,surveyDataRaw$Orgin_City,surveyDataRaw$Destination_City)
  
  return(surveyDataRaw)
  
}

loadSouthEastData <- function(){
  surveyDataRaw <- readxl::read_excel("~/Downloads/FinalProjectMaterial_2_2/Satisfaction Survey(2).xlsx",col_types = "guess" )

  surveyDataRaw<-cleanData(surveyDataRaw)
  
  #Filter Southeast data
  surveyDataRaw<- surveyDataRaw[surveyDataRaw$Airline_Name=="Southeast Airlines Co.",]
  
  
  #confirm no more NA in data set
  sapply(surveyDataRaw,function(X) sum(is.na(X)))
  
  #there are 3 routes which doesn't have any flight time data, drop them
  surveyDataRaw<-surveyDataRaw[!is.na(surveyDataRaw$Flight_time_in_minutes),]
  
  #confirm one more time
  sapply(surveyDataRaw,function(X) sum(is.na(X)))
  
  
  fixNames <- function(name){
    name <- gsub(" ","",name)
    name <- gsub(",","_",name)
    name <- gsub("&","_",name)
    name <- gsub("/","_",name)
    name <- gsub("-","_",name)
    return(name)
  }
  
  surveyDataRaw$path = fixNames(paste(surveyDataRaw$Orgin_City,surveyDataRaw$Destination_City,sep="_TO_"))
 
  #replace with means
  #surveyData <-replace_na(surveyDataRaw,as.list(colMeans(surveyDataRaw,na.rm=T)))
  
  surveyDataRaw$ageOfFirstflight = surveyDataRaw$Age - ( isoyear(surveyDataRaw$Flight_date) - surveyDataRaw$Year_of_First_Flight )
  
  surveyDataRaw$Month_of_Year =  lubridate::month((surveyDataRaw$Flight_date))
  surveyDataRaw$Month_of_Year<- as.factor(surveyDataRaw$Month_of_Year)
  surveyDataRaw$Day_of_Month <- as.factor(surveyDataRaw$Day_of_Month)
  
  #combine flight path
  
  surveyDataRaw$departure_delay_gt_5  <- ifelse(surveyDataRaw$Departure_Delay_in_Minutes>5,1,0)
  surveyDataRaw$departure_delay_gt_5 <- as.factor(surveyDataRaw$departure_delay_gt_5)
  #surveyDataRaw$departure_delay_gt_15  <- ifelse(surveyDataRaw$Departure_Delay_in_Minutes>15,1,0)
  #surveyDataRaw$departure_delay_gt_30  <- ifelse(surveyDataRaw$Departure_Delay_in_Minutes>30,1,0)
  #surveyDataRaw$departure_delay_gt_60  <- ifelse(surveyDataRaw$Departure_Delay_in_Minutes>60,1,0)
  #surveyDataRaw$departure_delay_gt_120  <- ifelse(surveyDataRaw$Departure_Delay_in_Minutes>120,1,0)
  
  surveyDataRaw$arrival_delay_gt_5  <- ifelse(surveyDataRaw$Arrival_Delay_in_Minutes>5,1,0)
  surveyDataRaw$arrival_delay_gt_5  <- as.factor(surveyDataRaw$arrival_delay_gt_5 )
  #surveyDataRaw$arrival_delay_gt_15  <- ifelse(surveyDataRaw$Arrival_Delay_in_Minutes>15,1,0)
  #surveyDataRaw$arrival_delay_gt_30  <- ifelse(surveyDataRaw$Arrival_Delay_in_Minutes>30,1,0)
  #surveyDataRaw$arrival_delay_gt_60  <- ifelse(surveyDataRaw$Arrival_Delay_in_Minutes>60,1,0)
  #surveyDataRaw$arrival_delay_gt_120  <- ifelse(surveyDataRaw$Arrival_Delay_in_Minutes>120,1,0)
  
  
  #Drop the airline name and code
  surveyDataRaw<- surveyDataRaw[,-which(names(surveyDataRaw) %in% c("Airline_Code","Airline_Name") )]
  return(surveyDataRaw)
  
}

convertToFactors<- function(surveyDataRaw){
  #Convert categorical to factors
  #surveyDataRaw$Satisfaction<-as.factor(surveyDataRaw$Satisfaction)
  #surveyDataRaw$Airline_Status<-as.factor(surveyDataRaw$Airline_Status)
  ##surveyDataRaw$Gender<-as.factor(surveyDataRaw$Gender)
  #surveyDataRaw$Type_of_Travel<-as.factor(surveyDataRaw$Type_of_Travel)
  #surveyDataRaw$Class<-as.factor(surveyDataRaw$Class)
  #surveyDataRaw$Flight_cancelled<-as.factor(surveyDataRaw$Flight_cancelled)
  #surveyDataRaw$path <- as.factor(surveyDataRaw$path)
  
  categorical_columns<-colnames(surveyDataRaw)[grepl('factor|logical|character',sapply(surveyDataRaw,class))]
  for(c in categorical_columns){
    surveyDataRaw[[c]]<-as.factor(surveyDataRaw[[c]])
  }
  return(surveyDataRaw)
}

surveyDataRaw <- loadSouthEastData()

#Summary Statistics
numeric_columns<-colnames(surveyDataRaw)[!grepl('factor|logical|character',sapply(surveyDataRaw,class))]
categorical_columns<-colnames(surveyDataRaw)[grepl('factor|logical|character',sapply(surveyDataRaw,class))]
responseColumn<-"Satisfaction"

for(c in numeric_columns){
  print(c)
  print(summary(surveyDataRaw[[c]]))
}
surveyDataRaw<-convertToFactors(surveyDataRaw)
for(c in categorical_columns){
  ##print(c)
  #print(summary(surveyDataRaw[[c]]))
  print(ggplot(surveyDataRaw,aes_string(x=c)) + geom_histogram(stat="count")) 
}

print(ggplot(surveyDataRaw,aes(Destination_State)) + geom_histogram(stat="count")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(ggplot(surveyDataRaw,aes(Destination_City)) + geom_histogram(stat="count")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

lmData<-surveyDataRaw
lm.base_model<-lm(formula=Satisfaction ~ . ,data=lmData)
summary(lm.base_model)


lm_coeff<-summary(lm.base_model)$coeff
lm_significant_vars<-lm_coeff[lm_coeff[,4] < 0.05,]

col_2<-(colnames(surveyDataRaw))
col_2<-col_2[col_2!="path"]
col_2<-col_2[col_2!="Orgin_City"]
col_2<-col_2[col_2!="Destination_City"]
col_2<-col_2[col_2!="Origin_State"]
col_2<-col_2[col_2!="Destination_State"]
col_2<-col_2[col_2!="Day_of_Month"]
col_2<-col_2[col_2!="Month_of_Year"]
col_2<-col_2[col_2!="Flight_Date"]
print(col_2)
lmData_2<-surveyDataRaw[,col_2]
lm.base_model_2<-lm(formula=Satisfaction ~ .^2 ,data=lmData_2)
lm_coeff_2<-summary(lm.base_model_2)$coeff
lm_significant_vars_2<-lm_coeff_2[lm_coeff_2[,4] < 0.05,]
lm_significant_vars_2



#LM Significant 

sig_cols<-c("Airline_Status","Gender","Age","No_of_Flights_p.a.","Type_of_Travel","Eating_and_Drinking_at_Airport","Class","Scheduled_Departure_Hour","Flight_time_in_minutes")
formula_sig<-paste("Satisfaction", paste(sig_cols, collapse=" + "), sep=" ~ ")
lm.sig_model<- lm(formula = formula_sig,lmData)
summary(lm.sig_model)

#Moderating + Significant
mod_cols<-c("Airline_Status:Type_of_Travel","Airline_Status:Arrival_Delay_greater_5_Mins","Age:No._of_other_Loyalty_Cards","Airline_Status:Class","Departure_Delay_in_Minutes:Flight_time_in_minutes","Age:Percent_of_Flight_with_other_Airlines","Type_of_Travel:Scheduled_Departure_Hour")
formula_sig_mod<-paste("Satisfaction", paste(c(sig_cols,mod_cols), collapse=" + "), sep=" ~ ")
lm.sig_mod_model<- lm(formula = formula_sig_mod,lmData)
summary(lm.sig_mod_model)
vif(lm.sig_mod_model)

#Final
sig_cols_vif<-c("Airline_Status","Gender","Age","No_of_Flights_p.a.","Eating_and_Drinking_at_Airport","Class","Scheduled_Departure_Hour","Flight_time_in_minutes")
mod_cols_vif<-c("Airline_Status:Type_of_Travel","Airline_Status:Arrival_Delay_greater_5_Mins","Age:No._of_other_Loyalty_Cards","Departure_Delay_in_Minutes:Flight_time_in_minutes","Age:Percent_of_Flight_with_other_Airlines")

formula_sig_mod_vif<-paste("Satisfaction", paste(c(sig_cols_vif,mod_cols_vif), collapse=" + "), sep=" ~ ")
lm.sig_mod_vif_model<- lm(formula = formula_sig_mod_vif,lmData)
summary(lm.sig_mod_vif_model)
#collinearity test
vif(lm.sig_mod_vif_model)

#linearity test
resettest(lm.sig_mod_vif_model)
#residual test 
bptest(lm.sig_mod_vif_model)
#serial correlation test
dwtest(lm.sig_mod_vif_model)
#outlier test
outlierTest(lm.sig_mod_vif_model)

outlier<-lmData[7104,]

plot(lm.sig_mod_vif_model)

boxTidwell(formula_sig_mod_vif,data=lmData)


sig_numeric_columns<-numeric_columns[numeric_columns %in% sig_cols_vif]
sig_categorical_columns<-categorical_columns[categorical_columns %in% sig_cols_vif]

randIndex <- sample(1:nrow(surveyDataRaw))
cutpoint<- floor(nrow(surveyDataRaw)*2/3)

getSignificantDataAsSparse<- function(data){
  sigData<-data[,c("Satisfaction",sig_numeric_columns)]
  sigData$Satisfaction<-ifelse(sigData$Satisfaction>3,TRUE,FALSE)
  #nnData$Satisfaction<-as.factor(ifelse(nnData$Satisfaction>3,1,0))
  #nnData<-convertToFactors(nnData)
  for(c in sig_categorical_columns){
    if(c %in% sig_categorical_columns){
      binary<-to.dummy(surveyDataRaw[[c]],c)
      sigData<- cbind(sigData,binary)
    }
  }
  return(sigData)
}


#SVM
#Above 3 is good, and below is bad Satisfaction
#svm_numerical_columns <- numeric_columns[!numeric_columns %in% c("Flight_date","ageOfFirstflight")]
#svm_categorical_columns <- categorical_columns[!categorical_columns %in% c("path","Orgin_City","Destination_City","Origin_State","Destination_State","Day_of_Month","Month_of_Year","Flight_Date")]

svmData<-getSignificantDataAsSparse(surveyDataRaw)

svm_trainData<- svmData[randIndex[1:cutpoint],]
svm_testData <- svmData[randIndex[(cutpoint+1):nrow(svmData)],]

svm_trainDataMatrix<-as.matrix(svm_trainData)
svm_testDataMatrix<-as.matrix(svm_testData)

#svm_sig_cols_vif<-c("Airline_Status","Gender","Age","No_of_Flights_p.a.","Eating_and_Drinking_at_Airport","Class","Scheduled_Departure_Hour","Flight_time_in_minutes")
#svm_mod_cols_vif<-c("Airline_Status*Type_of_Travel","Airline_Status*Arrival_Delay_greater_5_Mins","Age*No._of_other_Loyalty_Cards","Departure_Delay_in_Minutes*Flight_time_in_minutes","Age*Percent_of_Flight_with_other_Airlines")
#svm_formula_sig_mod_vif<-paste("Satisfaction", paste(c(numeric_columns,svm_categorical_columns), collapse=" + "), sep=" ~ ")

set.seed(2732)
svm.model<- ksvm(formula = Satisfaction ~.,svm_trainDataMatrix,kernel = "rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
#svm.sig_mod_vif_model<- ksvm(formula = svm_formula_sig_mod_vif,svmData,kernel = "stringdot",kpar=list(length = 4, lambda = 0.5),C=0.1,cross=2,prob.model=TRUE)
summary(svm.model)
#plot.svm(svm.model,svm_testData)
svm_testData$predicted<-predict(svm.model,svm_testDataMatrix)
table(svm_testData$Satisfaction,svm_testData$predicted)

percent_svm<-sum(ifelse(svm_testData$predicted==svm_testData$Satisfaction,1,0))/length(svm_testData$predicted)
percent_svm


#compare with lm model
lm_trainData<- surveyDataRaw[randIndex[1:cutpoint],]
lm_testData <- surveyDataRaw[randIndex[(cutpoint+1):nrow(surveyDataRaw)],]
lm.model<- lm(formula = formula_sig_mod_vif,lm_trainData)
summary(lm.model)
lm_testData$predicted<-predict(lm.model,lm_testData)
lm_percent<-sum(ifelse(round(lm_testData$predicted)==lm_testData$Satisfaction,1,0))/length(lm_testData$predicted)
table(lm_testData$Satisfaction,round(lm_testData$predicted))


#Try a Logit

logit_trainData<-lm_trainData
logit_testData<-lm_testData
logit_trainData$Satisfaction<-ifelse(logit_trainData$Satisfaction>3,1,0)
logit_testData$Satisfaction<-ifelse(logit_testData$Satisfaction>3,1,0)
logit.model<-glm(formula =formula_sig_mod_vif,data=logit_trainData,family=binomial(logit))
summary(logit.model)
logit_testData$predicted<-predict(logit.model,logit_testData)
logit_percent<-sum(ifelse(round(logit_testData$predicted)==logit_testData$Satisfaction,1,0))/length(logit_testData$predicted)
logit_percent
table(lm_testData$Satisfaction,round(lm_testData$predicted))
#Association Rules

#Arules
arules_columns<-colnames(surveyDataRaw)
#remove redundant columns
arules_columns<-arules_columns[!arules_columns %in% c("ageOfFirstflight","Flight_Distance","departure_delay_gt_5","arrival_delay_gt_5","Arrival_Delay_greater_5_Mins","path","Origin_State","Destination_State")]
arulesData<-surveyDataRaw[,arules_columns]
arulesTransactions<-as(arulesData, "transactions") 

itemFrequencyPlot(arulesTransactions,support=0.5)

arules.model<-apriori(arulesTransactions,parameter = list(support=0.05,confidence=0.2))

summary(arules.model)

quality(arules.model)
#inspect(arules.model)

#plot(arules.model)

goodrules <- arules.model[quality(arules.model)$lift > 2.5]

#plot(goodrules,method="graph",engine="interactive")

#plot(goodrules, method = "grouped", interactive = TRUE)

plot(goodrules, method = "grouped")

inspect(goodrules)


#Association rules for only significant columns
arulesData_sig<-surveyDataRaw[,sig_cols_vif]
arulesTransactions_sig<-as(arulesData_sig, "transactions") 
itemFrequencyPlot(arulesTransactions_sig,support=0.1)

arules.model_sig<-apriori(arulesTransactions_sig,parameter = list(support=0.001,confidence=0.1))
summary(arules.model_sig)
goodrules_sig <- arules.model_sig[quality(arules.model_sig)$lift > 2.5]
plot(goodrules_sig, method = "grouped")

#Whole data set
surveyDataFull<- readxl::read_excel("~/Downloads/FinalProjectMaterial_2_2/Satisfaction Survey(2).xlsx",col_types = "guess" )
surveyDataFull<- cleanData(surveyDataFull)
surveyDataFull$departure_delay_gt_5  <- ifelse(surveyDataFull$Departure_Delay_in_Minutes>5,1,0)
surveyDataFull$arrival_delay_gt_5  <- ifelse(surveyDataFull$Arrival_Delay_in_Minutes>5,1,0)

#Overall LM Model
#surveyDataFull<-surveyDataFull[surveyDataFull$Airline_Name=="West Airways Inc.",]
#surveyDataFull<-surveyDataFull[,-which(names(surveyDataFull) %in% c("Airline_Code","Airline_Name") )]
summary(surveyDataFull)
lm_fulldata<- lm(formula = Satisfaction ~., data=surveyDataFull)
lm_coeff_full<-summary(lm_fulldata)$coeff
lm_sig_full<-lm_coeff_full[lm_coeff_full[,4] < 0.05,]
lm_sig_full

summary(lm_fulldata)



#neuralnet
library(neuralnet)
nnData<-getSignificantDataAsSparse(surveyDataRaw)
nnData$Satisfaction<-ifelse(nnData$Satisfaction==TRUE,1,0)
nn_trainData<- nnData[randIndex[1:cutpoint],]
nn_testData <- nnData[randIndex[(cutpoint+1):nrow(nnData)],]

nn_trainDataMatrix<-as.matrix(nn_trainData)
nn_testDataMatrix<-as.matrix(nn_testData)

#nn_formula <-formula_sig_mod_vif<-paste("Satisfaction", paste(c(sig_cols_vif), collapse=" + "), sep=" ~ ")
nn_model<- neuralnet(formula=Satisfaction ~.,data = nn_trainDataMatrix,hidden = 12,lifesign = "minimal", linear.output = FALSE,threshold = 0.1)
plot(nn_model)
nn_predicted<-compute(nn_model,nn_testDataMatrix)
nn_testData$predicted<-nn_predicted$net.result

#table(nn_testData$Satisfaction,nn_testData$predicted)

percent_nn<-sum(ifelse(round(nn_testData$predicted)==nn_testData$Satisfaction,1,0))/length(nn_testData$predicted)
percent_nn


#VIsualizations  based on Significant LM vars
#Airline Status
#ggplot(surveyDataRaw,aes(Airline_Status)) + geom_histogram(stat="count")
ggplot(surveyDataRaw,aes(y=Satisfaction,x=Airline_Status)) + geom_boxplot()
# Except Blue, the rest of the class have higher average satisfaction score

#Age
#ggplot(surveyDataRaw,aes(Age)) + geom_histogram(stat="count",line="blue")
ggplot(surveyDataRaw,aes(y=Satisfaction,x=Age)) + geom_smooth(method="glm") + geom_count()
#We can see the  satisfaction decreases as age increases

#Age Group 53-85
ggplot(surveyDataRaw[surveyDataRaw$Age>=80,],aes(y=Satisfaction,x=Age)) + geom_smooth() + geom_count()
ggplot(surveyDataRaw[surveyDataRaw$Age==80,],aes(Age)) + geom_histogram(stat="count")

ggplot(surveyDataRaw,aes(y=Satisfaction,x=Age))  + geom_count() 
#There is no linear relation between Age and Satisfaction, so we have to drop it or try convert Age to become linear


#Gender
ggplot(surveyDataRaw,aes(Gender)) + geom_histogram(stat="count")
ggplot(surveyDataRaw,aes(y=Satisfaction,x=Gender)) + geom_boxplot()
# The mean and lower quater is same for female , which means female tend to score as low

ggplot(surveyDataRaw,aes(y=Satisfaction,x=Gender,color=Gender)) + geom_count()

#Full data
ggplot(surveyDataFull,aes(y=Satisfaction,x=Gender,color=Gender)) + geom_count()


#No of flights
#ggplot(surveyDataRaw,aes(No_of_Flights_p.a.)) + geom_histogram(stat="count")
ggplot(surveyDataRaw,aes(y=Satisfaction,x=No_of_Flights_p.a.)) + geom_smooth(method="glm") + geom_count()
# the higher the number of flight the lower the score is

#First flight
ggplot(surveyDataRaw[surveyDataRaw$No_of_Flights_p.a.==0,],aes(y=Satisfaction,x=Age,color=Satisfaction)) + geom_col(fill="white") + ggtitle("Age vs Satisfaction in their First Flight")
#No linear relation
ggplot(surveyDataRaw[surveyDataRaw$No_of_Flights_p.a.==0,],aes(y=Satisfaction,x=Age,color=Satisfaction)) + geom_smooth(method="glm") + ggtitle("Age vs Satisfaction in their First Flight")



#Type_of_Travel
ggplot(surveyDataRaw,aes(Type_of_Travel)) + geom_histogram(stat="count")
ggplot(surveyDataRaw,aes(y=Satisfaction,x=Type_of_Travel)) + geom_boxplot()


#Eating_and_Drinking_at_Airport
#ggplot(surveyDataRaw,aes(Eating_and_Drinking_at_Airport)) + geom_histogram(stat="count")
ggplot(surveyDataRaw,aes(Eating_and_Drinking_at_Airport)) + geom_boxplot()
#ggplot(surveyDataRaw,aes(y=Satisfaction,x=Eating_and_Drinking_at_Airport)) + geom_smooth(method="glm") + geom_count()
#Concentrated on lower 10's and lot of outliers 

#Class
ggplot(surveyDataRaw,aes(Class)) + geom_histogram(stat="count")
ggplot(surveyDataRaw,aes(y=Satisfaction,x=Class)) + geom_boxplot()

#Lot of Economy Travellers & Economy Plus tends to score us lower

#Day_of_Month
ggplot(surveyDataRaw,aes(Day_of_Month)) + geom_histogram(stat="count")
ggplot(surveyDataRaw,aes(y=Satisfaction,x=Day_of_Month)) + geom_boxplot()
ggplot(surveyDataRaw,aes(y=Satisfaction,x=Day_of_Month)) + geom_smooth(method="glm") + geom_count()


#Scheduled_Departure_Hour
ggplot(surveyDataRaw,aes(Scheduled_Departure_Hour)) + geom_histogram(stat="count")
#ggplot(surveyDataRaw,aes(y=Satisfaction,x=Scheduled_Departure_Hour)) + geom_boxplot()
#ggplot(surveyDataRaw,aes(y=Satisfaction,x=Scheduled_Departure_Hour)) + geom_smooth(method="glm") + geom_count()



#Flight_time_in_minutes
ggplot(surveyDataRaw,aes(Flight_time_in_minutes)) + geom_histogram(stat="count")
#ggplot(surveyDataRaw,aes(y=Satisfaction,x=Flight_time_in_minutes)) + geom_boxplot()
#ggplot(surveyDataRaw,aes(y=Satisfaction,x=Flight_time_in_minutes)) + geom_smooth(method="glm") + geom_count()


#Moderating Varibles

#Age vs Others
#Age:Percent_of_Flight_with_other_Airlines
ggplot(surveyDataRaw,aes(x=Age,y=Percent_of_Flight_with_other_Airlines,color=Satisfaction)) + geom_smooth(color="red",method="glm") + geom_count()

ggplot(surveyDataRaw[surveyDataRaw$Percent_of_Flight_with_other_Airlines<50,],aes(x=Age,y=Percent_of_Flight_with_other_Airlines,color=Satisfaction)) + geom_smooth(color="red") + geom_count()

#Age:No._of_other_Loyalty_Cards
ggplot(surveyDataRaw,aes(x=Age,y=No._of_other_Loyalty_Cards,color=Satisfaction)) + geom_smooth(color="red") + geom_count()

#Age:Eating_and_Drinking_at_Airport
ggplot(surveyDataRaw,aes(x=Age,y=Eating_and_Drinking_at_Airport,color=Satisfaction)) + geom_smooth(color="red") + geom_point()

#Age:ageOfFirstflight
ggplot(surveyDataRaw,aes(x=Age,y=ageOfFirstflight,color=Satisfaction)) + geom_smooth(color="red") + geom_point()
#looks like only linear variable
lm_age_ageoffirstflight <- lm(formula = Satisfaction ~ Age:ageOfFirstflight,surveyDataRaw)
summary(lm_age_ageoffirstflight)
plot(lm_age_ageoffirstflight)

#Age vs Satisfacation
ggplot(surveyDataRaw,aes(x=Age,y=Satisfaction)) + geom_smooth(color="red",method="glm") + geom_count()
ggplot(surveyDataRaw[surveyDataRaw$Satisfaction %in% c(4,5),],aes(x=Age)) + geom_bar()
ggplot(surveyDataRaw[!surveyDataRaw$Satisfaction %in% c(4,5),],aes(x=Age)) + geom_bar()
ggplot(surveyDataRaw,aes(x=Age)) + geom_bar()

#No_of_Flights_p.a.:Departure_Delay_in_Minutes
ggplot(surveyDataRaw,aes(x=No_of_Flights_p.a.,y=Departure_Delay_in_Minutes,color=Satisfaction)) + geom_count()


#No_of_Flights_p.a.:Arrival_Delay_in_Minutes
ggplot(surveyDataRaw,aes(x=No_of_Flights_p.a.,y=Arrival_Delay_in_Minutes,color=Satisfaction)) + geom_count()


#No_of_Flights_p.a.:Flight_time_in_minutes
ggplot(surveyDataRaw,aes(x=No_of_Flights_p.a.,y=Flight_time_in_minutes,color=Satisfaction)) + geom_count()

#No_of_Flights_p.a.:Flight_Distance
ggplot(surveyDataRaw,aes(x=No_of_Flights_p.a.,y=Flight_Distance,color=Satisfaction)) + geom_count()

#No_of_Flights_p.a.:Shopping_Amount_at_Airport
ggplot(surveyDataRaw,aes(y=No_of_Flights_p.a.,x=Shopping_Amount_at_Airport,color=Satisfaction)) + geom_smooth() + geom_count()

#Scheduled_Departure_Hour:Flight_time_in_minutes
ggplot(surveyDataRaw,aes(y=Scheduled_Departure_Hour,x=Flight_time_in_minutes,color=Satisfaction))  + geom_count()


#Shopping_Amount_at_Airport:ClassEco_Plus
ggplot(surveyDataRaw[surveyDataRaw$Class=="Eco Plus",],aes(x=Shopping_Amount_at_Airport,y=Satisfaction))  + geom_count() + ggtitle("Eco Plus")

#Type_of_TravelMileage_tickets:No._of_other_Loyalty_Cards
ggplot(surveyDataRaw[surveyDataRaw$Type_of_Travel=="Mileage tickets",],aes(x=No._of_other_Loyalty_Cards,y=Satisfaction))  + geom_count() + ggtitle("Mileage Tickets") + geom_smooth()

#Type_of_TravelPersonal_Travel:Scheduled_Departure_Hour
ggplot(surveyDataRaw[surveyDataRaw$Type_of_Travel=="Personal Travel",],aes(x=Scheduled_Departure_Hour,y=Satisfaction))  + geom_count() + ggtitle("Personal Travel")+ geom_smooth()

#Type_of_TravelPersonal_Travel:Shopping_Amount_at_Airport
ggplot(surveyDataRaw[surveyDataRaw$Type_of_Travel=="Personal Travel",],aes(x=Shopping_Amount_at_Airport,y=Satisfaction))  + geom_count() + ggtitle("Personal Travel")+ geom_smooth()



#No_of_Flights_p.a. vs Age Flight_Distance - For arules  verification, business question 2
ggplot(surveyDataRaw,aes(y=No_of_Flights_p.a.,x=Age,color=Satisfaction)) + geom_smooth() + geom_count()


ggplot(surveyDataRaw[surveyDataRaw$Type_of_Travel=="Personal Travel",],aes(x=Satisfaction))  + geom_bar() + ggtitle("Personal Travel")


ggplot(surveyDataRaw,aes(y=Satisfaction,x=Scheduled_Departure_Hour,group=Type_of_Travel,color=Type_of_Travel)) + geom_smooth() + geom_count()


ggplot(surveyDataRaw,aes(y=Satisfaction,x=No_of_Flights_p.a.,group=Type_of_Travel,color=Type_of_Travel)) + geom_smooth() + geom_count()


#1.	Based on Satisfaction, which airlines are performing well, and which are performing poorly? 
q1<-sqldf("select avg(Satisfaction) as Satisfaction,Airline_Name from surveyDataFull group by Airline_Name order by Satisfaction desc")
ggplot(q1,aes(y=Satisfaction,x=reorder(Airline_Name,Satisfaction),color=Satisfaction)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +ylab("Average Satisfaction") +xlab("Airline")

#2.	How does class affect customer satisfaction based on age group?
q2<-sqldf("select avg(Satisfaction) as Satisfaction,Age,Class from surveyDataRaw group by Age,Class")

ggplot(q2,aes(y=Satisfaction,x=Age,color=Class,group=Class)) + geom_point() +ylab("Average Satisfaction") + geom_smooth()



#3.	Could eating and drinking at the airport before a flight affect Satisfaction? 
q3<-sqldf("select avg(Satisfaction) as Satisfaction,Eating_and_Drinking_at_Airport from surveyDataRaw group by Eating_and_Drinking_at_Airport")
ggplot(q3,aes(y=Satisfaction,x=Eating_and_Drinking_at_Airport,color=Satisfaction)) + geom_point() +ylab("Average Satisfaction") + geom_smooth()


#5.	What times of operation are critical in affecting the satisfaction score? 
q5_1<-sqldf("select avg(Satisfaction) as Satisfaction_Bad,Scheduled_Departure_Hour from surveyDataRaw where Satisfaction>3  group by Scheduled_Departure_Hour")
q5_2<-sqldf("select avg(Satisfaction) as Satisfaction_Good,Scheduled_Departure_Hour from surveyDataRaw where Satisfaction<=3 group by Scheduled_Departure_Hour")
q5<-sqldf("select * from q5_1,q5_2 where q5_1.Scheduled_Departure_Hour=q5_2.Scheduled_Departure_Hour")
q5<-q5[,-4]
ggplot(q5,aes(y=Satisfaction_Bad,x=Scheduled_Departure_Hour)) + geom_line(color="red") +geom_line(aes(y=Satisfaction_Good),color="blue")+ylab("Average Satisfaction")


