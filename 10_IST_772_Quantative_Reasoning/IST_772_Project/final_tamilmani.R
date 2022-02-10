#'---
#'title: "IST 772 Quantitative Reasoning– Final Examination"
#'author: "Tamilselvan Tamilmani"
#'--- 
#'** Introduction **
#' We are tasked with analyses and then write up a technical report for a scientifically knowledgeable staff member in a state legislator’s office for the vaccine data in district 19 schools. The legislator's office is interested to know how to allocate financial assistance to school districts to improve both their vaccination rates and their reporting compliance.  
#' We will begin with exploratory analysis and come up with statistical analysis to help improve the vaccination rate and reporting compliance.
#'  
#'** Questions **    
#'** Question 1 **  
#'1.	How have U.S. vaccination rates varied over time? Are vaccination rates increasing or decreasing? Which vaccination has the highest rate at the conclusion of the time series? Which vaccination has the lowest rate at the conclusion of the time series? Which vaccine has the greatest volatility? 
set.seed(202112) 
load("districts19.RData")
load("allSchoolsReportStatus.RData")
load("usVaccines.RData")

summary(usVaccines)
usvaccineDF <- data.frame(usVaccines)
usvaccineDF$year <- 1980:2017
library(reshape2)
usvaccineDF_melted<-melt(usvaccineDF,id.vars="year")
colnames(usvaccineDF_melted) <- c("Year","Vaccine","Rate")
library(ggplot2)

ggplot(usvaccineDF_melted, aes(x=Year, y=Rate,group=Vaccine, color=Vaccine)) + 
  geom_line()  + ggtitle("US Vaccine Rates by Vaccine Type") +
  theme_minimal()

 
library(changepoint)

for (v in names(usvaccineDF)){
  #print(v)
  cp <- cpt.var(diff(usvaccineDF[[v]]),class=TRUE)
  print(paste(v,":",cpts(cp)))
}


#' The plot shows the vaccine rates of individual vaccines over the years.  
#' The US Vaccine rates gradually increased over time, except a sharp drop around late 80's  
#' DTP1 - First dose of Diphtheria/Pertussis/Tetanus has the highest rate as of 2017  
#' HepB_BD - Hepatitis B, Birth Dose has lowest rate as of 2017  
#' Pol3 - Polio third dose and MCV1 - Measles first dose has large number of change points at 16, but Pol3 has the greatest volatility, since it has the largest range.
#'  
#'  
#'** Question 2 **  
#'2.	What proportion of public schools reported vaccination data? What proportion of private schools reported vaccination data? Was there any credible difference in overall reporting proportions between public and private schools? 
library(scales)
pct_format = scales::percent_format(accuracy = .1)
ggplot(allSchoolsReportStatus, aes(x=pubpriv,fill=reported)) + 
  geom_bar(position="fill", stat="count" )  + 
  geom_text(aes(label =pct_format( ..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)])), stat = "count", position = "fill", vjust=1) +
  scale_y_continuous(labels = percent) + ggtitle("US Vaccine Report by School Type") +
  theme_minimal()

#Is there a difference, using chi.square test for categorical variable
pub_vs_private<-table(allSchoolsReportStatus$reported,allSchoolsReportStatus$pubpriv)
pub_vs_private
chisq.test(pub_vs_private)

#' The plot shows the distribution of vaccine reporting among public and private schools.  
#' 97.4% of public schools reported vaccine data.  
#' 84.7% of private schools reported vaccine data.  
#' The p-value of chi square test on the public vs private vaccine reporting is very low, so we can reject the null hypothesis of no difference between the public vs private reporting(non independence), thus favoring the alternate hypothesis of there is a difference in reporting(independence) between public and private schools. So we can conclude there is a credible difference in the reporting between public and private schools.  
#'  
#'  
#'** Question 3 **  
#'3.	What are 2013 vaccination rates for individual vaccines (i.e., DOT, Polio, MMR, and HepB) in California public schools?  How do these rates for individual vaccines in California districts compare with overall US vaccination rates (make an informal comparison to the final observations in the time series)?  

calf_2013_rate<- c(round(100 - mean(districts$WithoutDTP)), round(100 - mean(districts$WithoutHepB)),
                   round(100 - mean(districts$WithoutPolio)), round(100-mean(districts$WithoutMMR)), "California-2013" )
us_2017_rate <- usvaccineDF[usvaccineDF$year==2017,]

df <- subset(us_2017_rate,select=-c(Hib3,year))
df$Region<-c("US-2017")
df <- rbind(df,calf_2013_rate)
df_melted<-melt(df,id.vars="Region")
colnames(df_melted) <- c("Region","Vaccine","Rate")
df_melted$Rate<- as.integer(df_melted$Rate)
ggplot(df_melted,aes(x=Vaccine, y=Rate, fill=Region)) + 
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=Rate), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("US vs California Vaccine Rate") + theme_minimal()

#' The Plot show the comparison of vaccine rates between California in 2013 and overall US in 2017.  
#' The individual vaccine rates of DOT, Polio, MMR, and HepB are 90,92,90 and 90 respectively in California public schools.  
#' California is leading in HepB vaccine than overall US even before 3 years, and lagging on the remaining three vaccines.  
#'  
#'  
#'** Question 4 **  
#'4.	Among districts, how are the vaccination rates for individual vaccines related? In other words, if students are missing one vaccine are they missing all of the others?

library(ggcorrplot)
ggcorrplot(cor( districts[,c(2:5)]),lab = TRUE,
           hc.order = TRUE, type = "lower",
           p.mat=cor_pmat(districts[,c(2:5)]),
           colors = c("blue", "white", "orangered"))  + 
  ggtitle("Correlation Matrix of Vaccine") + theme_minimal()

#' We can use correlation matrix to compare numeric variables.The correlation among the  vaccine rates are very high and their p values are also high, so its highly likely students are missing all the vaccines if they miss any one.

#' ** EDA & Data Preparation **
#'(For all of these analyses, use PctChildPoverty, PctFreeMeal, PctFamilyPoverty, Enrolled,  and TotalSchools as predictors. Transform variables as necessary to improve prediction and/or interpretability. In general, if there is a Bayesian version of an analysis available, you are expected to run that analysis in addition to the frequentist version of the analysis.)

districts_melted<-melt(districts[,8:13])
colnames(districts_melted) <- c("Variable","Value")
ggplot(districts_melted, aes( y=Value,group=Variable, color=Variable)) + 
  geom_boxplot()  + ggtitle("US Vaccine Rates by Vaccine Type") +
  theme_minimal()
summary(districts)
Q <- quantile(districts$Enrolled, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(districts$Enrolled)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
outlier_removed<- subset(districts, districts$Enrolled > low & districts$Enrolled < up)
summary(outlier_removed)

#' The enrolled students have outlier in it, so we removed the outlier by using the IQR method described in https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/ . The Schools also have outliers in them, by removing the corresponding enrolled students the schools are also got rectified.

#'  
#'  
#'** Question 5 **  
#'5.	What variables predict whether or not a district’s reporting was complete?
glm5_1<- glm(DistrictComplete~PctChildPoverty+PctFreeMeal+PctFamilyPoverty+Enrolled+TotalSchools, data = outlier_removed, family = binomial())
summary(glm5_1)
exp(coef(glm5_1))

library(BaylorEdPsych)
PseudoR2(glm5_1)

library(car)
vif(glm5_1)

#' Both Enrolled and Totalschools are highly correlated due to collinearity. So we will combine them by normalizing the enrolled stutdents to enrolled per school.
#' Child Poverty and Family Povery are again correlated, so we will remove the child povery which has higher vcf

outlier_removed$Enrolled_norm <- outlier_removed$Enrolled / outlier_removed$TotalSchools
glm5_2<- glm(DistrictComplete~PctFreeMeal+PctFamilyPoverty+Enrolled_norm,
             data = outlier_removed, family = binomial())
summary(glm5_2)
exp(coef(glm5_2))

PseudoR2(glm5_2)
vif(glm5_2)



#' Free meal and Family poverty are some what correlated, so we will remove free meal since it has high vif.
glm5_3<- glm(DistrictComplete~PctFreeMeal+Enrolled_norm,
             data = outlier_removed, family = binomial())
summary(glm5_3)
exp(coef(glm5_3))

PseudoR2(glm5_3)
vif(glm5_3)

library(MCMCpack)
bayes_glm5_3<- MCMClogit(DistrictComplete~PctFreeMeal+Enrolled_norm, data = outlier_removed)
summary(bayes_glm5_3)
plot(bayes_glm5_3)

#' Out of the three models we select the third one which has lower AIC score after eliminating the collinear variables.  
#' PctFreeMeal and  Enrolled students per School predicts the Districts reporting is complete or not.
#' The frequentist method gives us a very low r square of 7% (Nagelkerke), makes us not very confident in our model.  
#' The Percent Free Meal is  significant with p-value .036, and also the HDI does not cross zero , -0.037(2.5%) to -0.002(97.5%), both the frequentist and Bayesian confirms the significance.  
#' The Enrolled per School is also significant with p-value .011, and also the HDI does not cross zero , 0.005(2.5%) to 0.029(97.5%), both the frequentist and Bayesian confirms the significance.  
#' Further the trace of the variables have no outliers, indicating the mcmc converged.
#' And both frequentist and Bayesian agree on the coefficients at -0.02 for PctFreeMeal and .02 for Enrolled per School.  

#'  
#'  
#'** Question 6 **  
#'6.	What variables predict the percentage of all enrolled students with completely up-to-date vaccines?  
glm6_1<- glm(PctUpToDate~PctChildPoverty+PctFreeMeal+PctFamilyPoverty+Enrolled+TotalSchools, data = outlier_removed, family = gaussian())
summary(glm6_1)
PseudoR2(glm6_1)
vif(glm6_1)

glm6_2<- glm(PctUpToDate~PctFreeMeal+Enrolled_norm, data = outlier_removed, family = gaussian())
vif(glm6_2)
PseudoR2(glm6_2)
summary(glm6_2)

lm6_2<-lm(PctUpToDate~PctFreeMeal+Enrolled_norm, data = outlier_removed)
summary(lm6_2)


library(BayesFactor)
bayes_glm6_2<- regressionBF(PctUpToDate~PctChildPoverty+PctFreeMeal+PctFamilyPoverty+Enrolled_norm, data = outlier_removed)
summary(bayes_glm6_2)
bayes_glm6_2_final<-lmBF(PctUpToDate~PctFreeMeal+Enrolled_norm, data = outlier_removed,posterior=TRUE, iterations=10000)
summary(bayes_glm6_2_final)
plot(bayes_glm6_2_final)

#' We tried linear modeling , it didn't give a good R square value, So we tried with GLM and normal distribution, which gave a high pseudo R square and low AIC making us more confident in our model.
#' Out of the two models we select the second one which has lower AIC score after eliminating the collinear variables.  
#' PctFreeMeal and  Enrolled students per School predicts the Percentage of Students up to date with vaccines.  
#' The frequentist method gives us a very high r square of 100% (Nagelkerke), makes us very confident in our model, although the  Adjusted McFadden is in line with the LM model at 11%  
#' The Percent Free Meal is very significant with p-value 1.15e-07, and  Enrolled per School is also significant with p-value 3.10e-09.  
#' And Bayesian method also picked PctFreeMeal + Enrolled_norm  as the predictors with highest factor st 2.77478e+12.  
#' The HDI intervals are also not crossing zero, giving us high confidence for the coefficients and are in line with frequentist estimates at 0.12 and 0.07 for PctFreeMeal and Enrolled students per School respectively.  
#' Further the trace of the variables have no outliers, indicating the mcmc converged.
#' 
#' 
#'  
#'  
#'** Question 7 **  
#'7.	What variables predict the percentage of all enrolled students with belief exceptions?

glm7_1<- glm(PctBeliefExempt~PctChildPoverty+PctFreeMeal+PctFamilyPoverty+Enrolled+TotalSchools, data = outlier_removed, family = gaussian())
summary(glm7_1)
PseudoR2(glm7_1)
vif(glm7_1)


glm7_2<- glm(PctBeliefExempt~PctFreeMeal+Enrolled_norm, data = outlier_removed, family = gaussian)
summary(glm7_2)
PseudoR2(glm7_2)
vif(glm7_2)


bayes_glm7_2<- regressionBF(PctBeliefExempt~PctChildPoverty+PctFreeMeal+PctFamilyPoverty+Enrolled_norm, data = outlier_removed)
summary(bayes_glm7_2)
bayes_glm7_2[9]

bayes_glm7_2_final<-lmBF(PctBeliefExempt~PctFreeMeal+Enrolled_norm, data = outlier_removed,posterior=TRUE, iterations=10000)
summary(bayes_glm7_2_final)
plot(bayes_glm7_2_final)

#' Out of the two models we selected the second one which has lower AIC score after eliminating the collinear variables.  
#' PctFreeMeal and  Enrolled students per School predicts the Percentage of Students with belief exemptions.  
#' The frequentist method gives us a very high r square of 99.99% (Nagelkerke), makes us very confident in our model.  
#' The Percent Free Meal is very significant with p-value 4.67e-10, and  Enrolled per School is also significant with p-value 4.49e-07.  
#' And Bayesian method also picked PctFreeMeal + Enrolled_norm  as the predictors with highest factor st 4.663353e+12.    
#' The HDI intervals are also not crossing zero, giving us high confidence for the coefficients and are in line with frequentist estimates at -0.1 and -0.04 for PctFreeMeal and Enrolled students per School respectively.
#' Further the trace of the variables have no outliers, indicating the mcmc converged.  
#' 
#'  
#'  
#'** Question 8 **  
#'8.	What’s the big picture, based on all of the foregoing analyses? The staff member in the state legislator’s office is interested to know how to allocate financial assistance to school districts to improve both their vaccination rates and their reporting compliance. What have you learned from the data and analyses that might inform this question?  
#'  

g1<-ggplot(outlier_removed,aes(y=PctBeliefExempt,x=PctFreeMeal)) + 
  geom_point() + theme_minimal() +
  geom_smooth(method="glm")

g2<-ggplot(outlier_removed,aes(y=PctBeliefExempt,x=Enrolled_norm)) + 
  geom_point() + theme_minimal() +
  geom_smooth(method="glm") 

library(patchwork)
g1 + g2  +  plot_annotation(title = "Percentage Belief Exempt Students")

#' The figure shows the change in percent Belief exemptions with respect to Free Meals and Enrolled students.
#' The percentage of belief exempt students go down with increase in percent  Free Meal and it is significant , so we advice the state department to increase the funding for free meal programs.  
#' The lower the enrolled students the higher the belief exemptions, so given the size of enrolled students we can guess this might belong to rural areas, we need to verify the school locations and concentrate on increasing the vaccine awareness.


g3<-ggplot(outlier_removed,aes(y=PctUpToDate,x=PctFreeMeal)) + 
  geom_point() + theme_minimal() +
  geom_smooth(method="glm")

g4<-ggplot(outlier_removed,aes(y=PctUpToDate,x=Enrolled_norm)) + 
  geom_point() + theme_minimal() +
  geom_smooth(method="glm") 

g3 + g4  +  plot_annotation(title = "Percentage up to date on Vaccines")

#' The figure shows the chnage in percent up to date on vaccines with respect to Free Meals and Enrolled students.
#' The percentage up to date go up with increase in percent  Free Meal and it is significant , so we advice the state department to increase the funding for free meal programs to improve the continued vaccination.
#' The lower the enrolled students the lower the percent up to date, so given the size of enrolled students we can guess this might belong to rural areas, we need to verify the school locations and concentrate on increasing the vaccine awareness.

p<-ggplot(outlier_removed, 
          aes(x=PctFreeMeal,y=Enrolled_norm,color=DistrictComplete)) +
  geom_point()  +theme_minimal()
p+facet_wrap(vars(DistrictComplete )) + ggtitle("Compliance Plot")

#' The figure shows the district complete on vaccination with respect to Free Meals and Enrolled students.
#' The Districts complaint go up with increase in percent  Free Meal and it is significant , so we advice the state department to increase the funding for free meal programs to improve the continued vaccination.
#' The lower the enrolled students the lower the district complaint, so given the size of enrolled students we can guess this might belong to rural areas, we need to verify the school locations and concentrate on increasing the vaccine awareness.

#'** Conclusion **
#' From the analysis we can clearly see investing in free meals at small to medium schools will increase the overall compliance and increase the vaccine adoption both in terms of completeness and belief exemptions.