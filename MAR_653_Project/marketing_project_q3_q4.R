library(timetk)
library(tidyverse)
library(formattable)

setwd("/Users/tamtam/OneDrive - Syracuse University/MAR-653/Project")
trans <- read.csv('df_transaction_2500.csv')
cluster <- read.csv('cluster_2500.csv')

cluster <- cluster %>% 
  mutate(clv_level = case_when(clv > median(clv) ~ "High",
                               TRUE ~ "Low"
  )) %>% 
  dplyr::select(household_key, clv_level)

trans <- left_join(trans, cluster, by = 'household_key')

low <- trans %>% 
  filter(clv_level == 'Low') %>% 
  group_by(COMMODITY_DESC, year) %>% 
  summarise(sales = sum(SALES_VALUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = COMMODITY_DESC, names_from = year, values_from = sales ) %>% 
  mutate(absolute_diff = `2009` - `2008`,
         growth = (absolute_diff / `2009`) * 100) %>% 
  filter(`2009` >= quantile(`2009`, 0.95)) %>% 
  arrange(desc(-growth))


high <- trans %>% 
  filter(clv_level == 'High') %>% 
  group_by(COMMODITY_DESC, year) %>% 
  summarise(sales = sum(SALES_VALUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = COMMODITY_DESC, names_from = year, values_from = sales ) %>% 
  mutate(absolute_diff = `2009` - `2008`,
         growth = (absolute_diff / `2009`) * 100) %>% 
  filter(`2009` >= quantile(`2009`, 0.95, na.rm = T)) %>% 
  arrange(desc(growth))




top_low <- head(low,10)
top_high <- head(high,10)

top_high %>% 
  rename(commodity = COMMODITY_DESC, 
         sales_2008 = `2008`,
         sales_2009 = `2009`,
         YOY_difference = absolute_diff,
         YOY_growth = growth) %>% 
  mutate(YOY_growth = YOY_growth/100) %>% 
  mutate(YOY_growth = scales::percent(YOY_growth),
         sales_2008 = scales::dollar(sales_2008),
         sales_2009 = scales::dollar(sales_2009),
         YOY_difference = scales::dollar(YOY_difference)) %>% 
  arrange(desc(YOY_growth)) %>% 
  formattable()


top_low %>% 
  rename(commodity = COMMODITY_DESC, 
         sales_2008 = `2008`,
         sales_2009 = `2009`,
         YOY_difference = absolute_diff,
         YOY_growth = growth) %>% 
  mutate(YOY_growth = YOY_growth/100) %>% 
  mutate(YOY_growth = scales::percent(YOY_growth),
         sales_2008 = scales::dollar(sales_2008),
         sales_2009 = scales::dollar(sales_2009),
         YOY_difference = scales::dollar(YOY_difference)) %>% 
  formattable()

lo <- head(low,5)
hi <- head(high,5)

low_cat <- unique(lo$COMMODITY_DESC)
high_cat <- unique(hi$COMMODITY_DESC)

lo <- trans %>% 
  filter(clv_level == 'Low' & COMMODITY_DESC %in% low_cat) %>% 
  mutate(Date = as.Date(DAY, origin = "2008-01-01"))

hi <- trans %>% 
  filter(clv_level == 'High' & COMMODITY_DESC %in% high_cat) %>% 
  mutate(Date = as.Date(DAY, origin = "2008-01-01"))

lo %>% 
  group_by(COMMODITY_DESC) %>%
  summarise_by_time(.date_var = Date, .by = "month", value = sum(SALES_VALUE)) %>% 
  filter(Date < "2009-12-01") %>% 
  plot_time_series(Date, value, .title = 'Top 5 Declining Categories', .smooth_color = 'purple')


hi %>% 
  group_by(COMMODITY_DESC) %>%
  summarise_by_time(.date_var = Date, .by = "month", value = sum(SALES_VALUE)) %>% 
  filter(Date < "2009-12-01") %>% 
  plot_time_series(Date, value, .title = 'Top 5 Rising Categories', .smooth_color = 'purple')



