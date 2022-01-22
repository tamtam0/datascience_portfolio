library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(patchwork)
#library(plotly)
library(sqldf)
library(ggwordcloud)

#library(rjson)
#The data set comes from kaggle , https://www.kaggle.com/shivamb/netflix-shows
#This dataset consists of tv shows and movies available on Netflix as of January 2021 
#The dataset is collected from Flixable which is a third-party Netflix search engine
setwd("/Users/tamtam/Dropbox/Masters/s4-summer 2021/IST-719 – Information Visualization")

netflix<- read.csv("week3/netflix_titles.csv")


score <- (length(colnames(netflix)) * 4) * (nrow(netflix)/100)

print(score)


summary(netflix)

#Fix Data
netflix<-netflix%>% separate(duration,c("duration","duration_type"),sep=" ")
unique(netflix$duration_type)
netflix$duration_type <- ifelse(netflix$duration_type=="min","Minutes","Seasons")
unique(netflix$duration_type)

ggplot(netflix) + aes(x=type) + geom_bar()

netflix$duration<- as.integer(netflix$duration)
#netflix$duration_type<- as.factor(netflix$duration_type)

unique(netflix$date_added)
netflix$date_added<-lubridate::parse_date_time(netflix$date_added, "bdY")

netflix<-netflix[!is.na(netflix$date_added),]

#remove 2021 , since it has ony january

netflix<-netflix[netflix$release_year<2021,]

ratings_map<- list(    'TV-PG'='Older Kids',
                       'TV-MA'= 'Adults',
                       'TV-Y7-FV'= 'Older Kids',
                       'TV-Y7'= 'Older Kids',
                       'TV-14'= 'Teens',
                       'R'= 'Adults',
                       'TV-Y'= 'Kids',
                       'NR'= 'Adults',
                       'PG-13'= 'Teens',
                       'TV-G'= 'Kids',
                       'PG'= 'Older Kids',
                       'G'= 'Kids',
                       'UR'= 'Adults',
                       'NC-17'= 'Adults')
ratings_map<-data.frame(unlist(ratings_map))
names(ratings_map)<-c("ratings_str")

netflix<-merge(netflix,ratings_map,by.x="rating",by.y = 0)

#derived, 
#netflix<-netflix %>% 
#  rename(
#    ratings_str = ratings_str.y,
#  )
#netflix<- select(netflix, -c(ratings_str.x))


#is produced by netflix
netflix$netflix_original<-ifelse(netflix$release_year==year(netflix$date_added),"Yes","No")


#questions
#How is the release by country
netflix_bycountry<-separate_rows(netflix,country,sep = ",")
netflix_bycountry$country<-trimws(netflix_bycountry$country,which = "both")

netflix_bycountry_bygenere<-separate_rows(netflix_bycountry,listed_in,sep = ",")
netflix_bycountry_bygenere$listed_in<-trimws(netflix_bycountry_bygenere$listed_in,which = "both")
                                  
#what is top genere by country

total_by_country<-sqldf("select count(show_id) as total,country from netflix_bycountry group by country")

total_by_country_genere<-sqldf("select count(show_id) as genere_count,country,listed_in from netflix_bycountry_bygenere group by country,listed_in")
total_by_country_top_genere<-sqldf("select max(genere_count) as genere_count,country,listed_in from total_by_country_genere group by country")

library(countrycode)

by_country<-merge(total_by_country,total_by_country_top_genere,by="country")
by_country$country_code<- countrycode(by_country$country,
                                      origin="country.name",destination = "iso3c")

#world_map <- map_data("world")
#world_map <- world_map %>%
#  filter(region != "Antarctica")
#by_country<-merge(by_country,world_map,by.x="country",by.y="region")

#ggplot(by_country, aes(map_id = country))+
#  geom_map(aes(fill = total),map = world_map,  color = "white")+
#  expand_limits(x = world_map$long, y = world_map$lat)+
#  scale_fill_viridis_c(option = "C")


library(rworldmap)
library(colorspace)
#pal <- choose_palette()
#pal
#red_palet<-RColorBrewer::brewer.pal(5,"Reds")

#"#FF004F", "#FD0E35", "#FF5A5F"
colfunc <- colorRampPalette(c("#FF2400","#FA8072"))
plot(rep(1,5),col=colfunc(5),pch=19,cex=3)


by_country$total_bin<-cut(by_country$total,c(0,100,500,1000,2000,4000))

#red_palet<- c("#ED2939","#FF2400","#CD5C5C","#C21807","#EA3C53"," #D21F3C","#FF0800")
df.map<- joinCountryData2Map(by_country,joinCode = "ISO3", nameJoinColumn = "country_code")
df.map<- df.map[!is.na(df.map$continent),]
df.map<- df.map[df.map$continent!="Antarctica",]

length(unique(netflix_bycountry$country))
df.map$missing_countries<-ifelse(df.map$NAME==df.map$country,0,1)
df.map$missing_countries<- replace_na(df.map$missing_countries,1)

map<-mapCountryData(df.map, 
               nameColumnToPlot = "total_bin",
               nameColumnToHatch ="missing_countries",
               #numCats = 5,
              catMethod =c(1,100,500,1000,2000,4000),
               #catMethod = "logFixedWidth",
               colourPalette = rev(colfunc(5)),
               oceanCol = "white",
               borderCol = "peachpuff4",
               mapTitle = "Netflix Releases",
               missingCountryCol="red",
              aspect="sp"
)


#plot the totals
#country_coord<-data.frame(coordinates(df.map),stringsAsFactors=F)
# label the countries
#text(x=country_coord$X1,y=country_coord$X2,labels=df.map$total,adj=0,cex=.5)

top_50=df.map[order(-df.map$total),][1:50,]

#country_coord<-data.frame(coordinates(top_50),stringsAsFactors=T)
# label the countries

top_50_exclude<-top_50[top_50$listed_in!="International TV Shows",]
top_50_exclude<-top_50_exclude[top_50_exclude$listed_in!="International Movies",]
country_coord<-data.frame(coordinates(top_50_exclude),stringsAsFactors=T)

text(x=country_coord$X1,y=country_coord$X2,labels=top_50_exclude$listed_in,cex=.6)

top_50_itv<-top_50[top_50$listed_in=="International TV Shows",]
country_coord_itv<-data.frame(coordinates(top_50_itv),stringsAsFactors=T)
points(x=country_coord_itv$X1,y=country_coord_itv$X2,col="purple",pch=15)

top_50_itm<-top_50[top_50$listed_in=="International Movies",]
country_coord_itm<-data.frame(coordinates(top_50_itm),stringsAsFactors=T)
points(x=country_coord_itm$X1,y=country_coord_itm$X2,col="yellow",pch=19)


#text(df.map$, df.map$LON, df.map$total, pos = 1)

#"#FF004F", "#FD0E35", "#FF5A5F"
colfunc_bar <- colorRampPalette(c("#FA8072","#FF2400"))
plot(rep(1,2),col=colfunc_bar(2),pch=19,cex=3)

#How soon netflix is getting the show
stat1<-ggplot(netflix[netflix$release_year>2007,]) + 
  aes(x=release_year,fill=netflix_original) + 
  geom_bar(stat="count") + 
  scale_fill_manual(name = "netflix_original", values = colfunc_bar(2)) +
  theme_few()
stat1


stat2<-ggplot(netflix[netflix$release_year>2007,]) + 
  aes(x=release_year,fill=type) + 
  #geom_area(stat="count",position="fill") + 
  geom_bar(stat="count") + 
  scale_fill_manual(name = "type", values = colfunc_bar(2)) +
  theme_few()
stat2



netflix_sub<-read.csv("project/netflix_subscribers.csv")

stat3<- ggplot(netflix_sub) + aes(x=year) + 
  geom_area(aes(y=total,fill="total")) + 
  geom_area(aes(y=us,fill="us")) + 
  scale_fill_manual(name = "type", values = colfunc_bar(2)) +
  theme_few()
stat3

wrap_plots(stat1/stat2/stat3)


library(wordcloud)
library(tm)
text <- netflix$description
docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
word_df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
wordcloud<-wordcloud(words = word_df$word, freq = word_df$freq, min.freq = 100,          
          max.words=100, random.order=FALSE, rot.per=0.35,            
          colors=colfunc(100))

set.seed(1234)
ggplot(word_df[word_df$freq>50,],aes(label = word, size = freq,x = word, color = freq)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_few()

library(topicmodels)
k = 100;
SEED = 1234;
netflix.lda <- LDA(dtm, k, method="Gibbs", control=list(seed = SEED))
lda.topics <- as.matrix(topics(netflix.lda))
lda.topics
lda.terms <- terms(netflix.lda)
lda.terms

#ggplot(stats[stats$ngram==2,],aes(label = key, size = freq,x=key)) +
#  geom_text_wordcloud_area() +
#  scale_size_area(max_size = 20) +
  #facet_wrap(~doc_id) +
#  theme_few()

library(udpipe)

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

getTopics <- function(df){
  x <- udpipe_annotate(ud_model, x = df$description, doc_id = df$ratings_str)
  x <- as.data.frame(x)
  stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                         relevant = x$upos %in% c("NOUN", "ADJ"),ngram_max=4)
  stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
  #hist(stats$freq)
  

  
  wordcloud_df<- stats[stats$ngram>1,]
  return(wordcloud_df)
}

wordcloud_adults<-getTopics(netflix[netflix$ratings_str %in% c("Adults"),])

wordcloud<-wordcloud(words = wordcloud_adults$key, freq = wordcloud_adults$freq, min.freq = 2,          
                     max.words=50, random.order=FALSE, rot.per=0.35,        
                     colors=colfunc(50))


wordcloud_teen<-getTopics(netflix[netflix$ratings_str %in% c("Teens"),])

wordcloud2<-wordcloud(words = wordcloud_teen$key, freq = wordcloud_teen$freq, min.freq = 2,          
                     max.words=50, random.order=FALSE, rot.per=0.35,        
                     colors=colfunc(50))



wordcloud_kids<-getTopics(netflix[netflix$ratings_str %in% c("Older Kids","Kids"),])

wordcloud3<-wordcloud(words = wordcloud_kids$key, freq = wordcloud_kids$freq, min.freq = 2,          
                      max.words=50, random.order=FALSE, rot.per=0.35,        
                      colors=colfunc(50))
