library(readxl)
library(stats)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gdata)
library(qdap)
library(dplyr)
library(caret)
library(reshape2)
library(doParallel)
cl <- makePSOCKcluster(9)
registerDoParallel(cl)
set.seed(123)

setwd("/Users/tamtam/OneDrive - Syracuse University/MAR-653/Project")
df_raw <- read_excel("Complete Journey.xls", sheet = "d")
str(df_raw)

library(CLVTools)
df_transaction<- read_csv("data/transaction_data.csv")
summary(df_transaction)
df_transaction$year<- ifelse(df_transaction$DAY>365,"2009","2008")
df_transaction$day<- ifelse(df_transaction$year=="2009",df_transaction$DAY%%365,df_transaction$DAY)
df_transaction$date<- paste(df_transaction$year,df_transaction$day,sep="-")
#df_transaction$date<-lubridate::parse_date_time(df_transaction$date,"Yj")


clv_data<- clvdata(df_transaction,
                    date.format = "Yj",
                    time.unit="weeks",
                    name.id="household_key",
                    name.date="date",
                    name.price="SALES_VALUE")

est_pndb<-pnbd(clv_data,
               start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
               optimx.args = list(control=list(trace=5), method="Nelder-Mead")
               )
summary(est_pndb)
plot(est_pndb)
result<-predict(est_pndb,prediction.end = "2010-365")
summary(result)


df<-df_raw

df$COUPON_DISCOUNT<-replace_na(df$COUPON_DISCOUNT,0)
df$category<-replace_na(df$category,"UNKNOWN")

df<- na.omit(df)

df<- data.frame(unclass(df),stringsAsFactors=TRUE)
str(df)



df1<- merge(x = df, y = result, by.x = "household_key",, by.y="Id", all.x = TRUE)
summary(df1)





#group by house
df_house<- df1 %>% group_by(household_key,.add = TRUE) %>% summarise(
  category_total = length(unique(category)),
  #category_top = top_n(category,1),
  quantity_average = mean(quantity),
  quantity_min = min(quantity),
  quantity_max = max(quantity),
  sales_average = mean(sales.value),
  sales_total= sum(sales.value),
  sales_min = max(sales.value),
  sales_max = min(sales.value),
  coupon_total = sum(COUPON_DISCOUNT),
  coupon_average = mean(COUPON_DISCOUNT),
  coupon_min = min(COUPON_DISCOUNT),
  coupon_max = max(COUPON_DISCOUNT),
  num_national_average = mean(num_national),
  num_national_min = min(num_national),
  num_national_max = max(num_national)
)

df_house<- merge(x=df_house,y=df1, by="household_key",all.x = TRUE)

df_house<- distinct(df_house,household_key, .keep_all = TRUE)

df_house<- select(df_house,-c("DERT","CET","predicted.CLV","predicted.mean.spending",
                              "year","category","quantity","sales.value","COUPON_DISCOUNT","num_national",
                              "dh_coupon_redeemed","value.of.dh.coupon",
                              "period.first","period.last","period.length"))
summary(df_house)

df_house$clv<- df_house$sales_total/102 * ((1+0.01)/(1+0.01-df_house$PAlive))
summary(df_house)

#clusterDF<-fastDummies::dummy_cols(select(df_house,-c(PAlive,sales_average)))
#clusterDF<-fastDummies::dummy_cols(select(df_house,-c(MARITAL_STATUS_CODE,INCOME_DESC,HOMEOWNER_DESC)))
#clusterDF<- na.omit(clusterDF)

#remove categorical since we converted them to dummy
clusterDF<-select(df_house,-c(household_key,AGE_DESC,MARITAL_STATUS_CODE,INCOME_DESC,
                               HOMEOWNER_DESC,HH_COMP_DESC,HOUSEHOLD_SIZE_DESC,KID_CATEGORY_DESC))
summary(clusterDF)
fviz_nbclust(clusterDF, kmeans, method = "wss",k.max = 20)
fviz_nbclust(clusterDF, kmeans, method = "silhouette",k.max = 20)
fviz_nbclust(clusterDF, kmeans, method = "gap",k.max = 20)


kmeans1 <- kmeans(clusterDF,centers = 4)
fviz_cluster(kmeans1,data=clusterDF)

df_house$cluster<-kmeans1$cluster

write_csv(df_house,"cluster.csv")

#linear regression
df_lm<-select(df_house,-c("household_key","cluster","PAlive","sales_average","sales_min","sales_max","sales_total"))
lm<- lm(clv~.,data=df_lm)
summary(lm)


df_lm<-select(df_house,-c("household_key","cluster","PAlive","sales_average","sales_min","sales_max","sales_total"))
lm<- lm(clv~.,data=df_lm)
summary(lm)


#all 2500 house data
df_demographic<- read_csv("data/hh_demographic.csv")
df_product<- read_csv("data/product.csv")


df_demographic$household_key<-as.character(df_demographic$household_key)

df_all<- merge(x = result, y = df_demographic, by.y = "household_key", by.x="Id", all.x = TRUE)


df_transaction_all<-merge(df_transaction,df_product,by="PRODUCT_ID",x.all=TRUE)
df_transaction_all$coupon_discount<-df_transaction_all$RETAIL_DISC+df_transaction_all$COUPON_DISC+df_transaction_all$COUPON_MATCH_DISC
df_transaction_all$brand_national<-ifelse(df_transaction_all$BRAND=="National",1,0)
summary(df_transaction_all)
write_csv(df_transaction_all,"df_transaction_2500.csv")
df_house_all<-df_transaction_all %>% group_by(household_key,.add = TRUE) %>% 
  summarise(  department_total = length(unique(DEPARTMENT)),
              commodity_total = length(unique(COMMODITY_DESC)),
              commodity_sub_total = length(unique(SUB_COMMODITY_DESC)),
              quantity_average = mean(QUANTITY),
              quantity_min = min(QUANTITY),
              quantity_max = max(QUANTITY),
              sales_total= sum(SALES_VALUE),
              sales_min_item = min(SALES_VALUE),
              sales_max_item = max(SALES_VALUE),
              coupon_total = sum(coupon_discount),
              coupon_average_item = mean(coupon_discount),
              coupon_min_item = min(coupon_discount),
              coupon_max_item = max(coupon_discount),
              num_national = sum(brand_national),
              num_products = n()
  )
summary(df_house_all)
df_house_all$national_brand_percent<-df_house_all$num_national/df_house_all$num_products

df_house_all<- merge(x = df_house_all, y = df_all, by.x = "household_key", by.y="Id", all.x = TRUE)


df_house_all$clv<- (df_house_all$sales_total/102) * ((1+0.01)/(1+0.01-df_house_all$PAlive))
summary(df_house_all)
df_house_all<- select(df_house_all,-c("DERT","CET","predicted.CLV","predicted.mean.spending",
                                      "period.first","period.last","period.length"))
summary(df_house_all)



#clusterDF<-fastDummies::dummy_cols(select(df_house,-c(MARITAL_STATUS_CODE,INCOME_DESC,HOMEOWNER_DESC)))

#remove categorical since we converted them to dummy
clusterDF_all<-select(df_house_all,-c(household_key,PAlive,sales_total,AGE_DESC,MARITAL_STATUS_CODE,INCOME_DESC,
                               HOMEOWNER_DESC,HH_COMP_DESC,HOUSEHOLD_SIZE_DESC,KID_CATEGORY_DESC))
summary(clusterDF_all)
fviz_nbclust(clusterDF_all, kmeans, method = "wss",k.max = 20)
fviz_nbclust(clusterDF_all, kmeans, method = "silhouette",k.max = 20)
fviz_nbclust(clusterDF_all, kmeans, method = "gap",k.max = 20)


kmeans_all <- kmeans(clusterDF_all,centers = 2)
fviz_cluster(kmeans_all,data=clusterDF_all)

df_house_all$cluster<-kmeans_all$cluster

write_csv(df_house_all,"cluster_2500.csv")

sum(df_house_all[df_house_all$clv<1740,]$sales_total)
sum(df_house_all[df_house_all$clv>1740,]$sales_total)

#linear regression
df_lm_all<-select(df_house_all,-c("household_key","cluster","PAlive","sales_total",
                                  "quantity_average","quantity_max","num_products","num_national","commodity_total"))

lm_all<- lm((clv)~.,data=df_lm_all)
summary(lm_all)

df_lm_low<-select(df_house_all,-c("household_key","cluster","PAlive","sales_total",
                                  "quantity_average","quantity_max","num_products","num_national","commodity_total"))
df_lm_low<-df_lm_low[df_lm_low$clv<=1741,]
summary(df_lm_low)
lm_all_low<- lm(clv~department_total+coupon_total,data=df_lm_low)
summary(lm_all_low)


lm_all_high<- lm((clv)~.,data=df_lm_all[df_lm_all$clv>1741,])
summary(lm_all_high)


#Direct marketing
df_campaign<- read_csv("data/campaign_table.csv")

df_campaign_house<- df_campaign %>% count(household_key,DESCRIPTION) %>% spread(DESCRIPTION, n, fill=0)
column_names<-c("household_key","campaign_TypeA_count","campaign_TypeB_count","campaign_TypeC_count")
names(df_campaign_house)<-column_names

df_marketing<- merge(df_house_all,df_campaign_house,by="household_key",all.x=TRUE)
df_marketing$campaign_TypeA_count<-replace_na(df_marketing$campaign_TypeA_count,0)
df_marketing$campaign_TypeB_count<-replace_na(df_marketing$campaign_TypeB_count,0)
df_marketing$campaign_TypeC_count<-replace_na(df_marketing$campaign_TypeC_count,0)

#linear model
lm_marketing<- lm(clv~campaign_TypeA_count+campaign_TypeB_count+campaign_TypeC_count,data=df_marketing[df_marketing$clv>1741,])
summary(lm_marketing)

df_lm_marketing_all<-select(df_marketing,-c("household_key","cluster","PAlive","sales_total",
                                            "quantity_average","quantity_max","num_products","num_national","commodity_total"))

formula_campagin<-log(clv)~(campaign_TypeA_count+campaign_TypeB_count+campaign_TypeC_count)
lm_marketing_high<- lm(formula_campagin,data=df_lm_marketing_all[df_lm_marketing_all$clv>1741,])
summary(lm_marketing_high)

lm_marketing_low<- lm(formula_campagin,data=df_lm_marketing_all[df_lm_marketing_all$clv<=1741,])
summary(lm_marketing_low)



ggplot(df_marketing[df_marketing$clv<1741,],aes(x=log(num_products),y=log(clv),color=campaign_TypeA_count+campaign_TypeB_count+campaign_TypeC_count)) + 
  geom_point() + geom_abline() + ggthemes::theme_clean() + labs(color = "TypeA Count")

ggplot(df_marketing[df_marketing$clv>1741,],aes(x=(num_products),y=log(clv),color=campaign_TypeA_count+campaign_TypeB_count+campaign_TypeC_count)) + 
  geom_point() + geom_abline() + ggthemes::theme_clean() +labs(color = "Campaign Count") +scale_fill_discrete()


df_marketing$total_campaign<-as.factor(df_marketing$campaign_TypeA_count+df_marketing$campaign_TypeB_count+df_marketing$campaign_TypeC_count)
ggplot(df_marketing[df_marketing$clv>1741,],aes(x=(total_campaign),y=log(clv),color=total_campaign)) + 
  geom_point() + geom_abline() + ggthemes::theme_clean() +labs(color = "Campaign Count") + scale_fill_discrete()

lm_marketing2<-lm(clv~(num_national)+campaign_TypeA_count,df_marketing)
summary(lm_marketing2)
  


q4_df<-df_house_all
summary(q4_df)
q4_df$clv_bucket<-ifelse(q4_df$clv<1741,0,1)

train.control <- trainControl(method = "cv", number=5,allowParallel = T)

glm_1 = train(
  form = clv_bucket ~ AGE_DESC+HOMEOWNER_DESC+HOUSEHOLD_SIZE_DESC+KID_CATEGORY_DESC+MARITAL_STATUS_CODE+INCOME_DESC,
  data = q4_df,
  trControl = train.control,
  method = "glm",
  family = "binomial"
)



#EDA Plots
library(dlookr)
eda_report(df_house_all,clv,)
df_transaction_all %>% group_by(household_key) %>% plot_box_numeric(each = TRUE)

df_house_all %>% plot_box_numeric(each = TRUE)
