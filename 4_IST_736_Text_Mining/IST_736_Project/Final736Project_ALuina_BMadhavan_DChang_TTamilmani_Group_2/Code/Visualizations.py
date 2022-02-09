#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: bhavyamadhavan
"""

import json
import glob
import pandas as pd
from io import StringIO
from collections import Counter
import re
import matplotlib.pyplot as plt
from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator

path = "/Users/bhavyamadhavan/IST736-TextMining/Assignments/Project/Twitter/Tweets_Latest/"
data_file = 'Twitter_Sentiment.csv'

all_files = glob.glob(path + "/*.json")

jsons = []
for filename in all_files:
    file1 = open(filename)
    memFile1=StringIO()
    memFile1.write("[")
    for line in file1:
        line = line.replace("}{","},{")
        memFile1.write(line)
    memFile1.write("]")
    memFile1.seek(0)
    df = json.load(memFile1)
    #jsons.append(df)
    #df = pd.read_json(memFile1)
    for i in df:
        if 'data' in i:
            jsons.append(pd.DataFrame.from_records(i['data']))


twitter_df = pd.concat(jsons, axis=0,ignore_index=True)

twitter_before_clean_df = twitter_df.copy()


def generate_wordcloud(input_df,input_col,fig_name):
    wordcloud = WordCloud(width = 1000, height = 500).generate(' '.join(input_df[input_col]))
    plt.figure(figsize=(15,8))
    plt.imshow(wordcloud)
    plt.axis("off")
    plt.savefig(fig_name + ".png", bbox_inches='tight')
    plt.show()
    plt.close()

#WordCloud before clean up
generate_wordcloud(twitter_before_clean_df,'text','WC_Before_Tweets')

def CleanPost(sText):
    sCleanText = sText
    #sCleanText = " ".join(filter(lambda x:x[0]!='#', sCleanText.split()))
    sCleanText = re.sub(r'\s'," ",sCleanText)
    sCleanText = re.sub(r'http[s]?://.+\b',"",sCleanText)
    sCleanText = re.sub(r'"',"",sCleanText)
    #sCleanText = re.sub(r'[^\w ]','',sCleanText)
    return sCleanText.strip()


for i in range(len(twitter_df)):
    twitter_df['text'][i] = CleanPost(twitter_df['text'][i])


#Creating a dataframe with all hashtags and created date

column_names = ["HashTag", "Created"]
hash_df = pd.DataFrame(columns = column_names)
hash_array = []
def extract_hash(input_string,input_date):
    for i in s.split():
        if i.startswith("#"):
            hash_array.append({'HashTag': i, 'Created': input_date})
    

for i in range(len(twitter_df)):
    s = twitter_df['text'][i]
    d = twitter_df['created_at'][i]
    extract_hash(s,d)
    
hash_df = hash_df.append(hash_array)  

hash_df['Created']= pd.to_datetime(hash_df['Created'])

temp_hash_df = hash_df.copy()



def remove_common_hashtag(input_df,input_hash):
    temp_hash_df.drop(temp_hash_df[temp_hash_df['HashTag'] ==input_hash].index, inplace = True) 
    pass


remove_common_hashtag(temp_hash_df, '#WFH')
remove_common_hashtag(temp_hash_df, '#workfromhome')
remove_common_hashtag(temp_hash_df, '#workingfromhome')
remove_common_hashtag(temp_hash_df, '#WorkFromHome')
remove_common_hashtag(temp_hash_df, '#wfh')
remove_common_hashtag(temp_hash_df, '#workingFromHome')

#Split the hash df into before and after COVID
#before

hash_before_df = temp_hash_df[(temp_hash_df['Created'] < '2020-03-01')].copy()

#after
hash_after_df = temp_hash_df[(temp_hash_df['Created'] >= '2020-03-01')].copy()



def plot_topn(input_df,n,input_col,file_name):
    tt=input_df[input_col].value_counts().sort_values(ascending=False)
    tt[:n].plot(kind='bar',figsize=(8, 7),color = 'indigo')
    plt.xlabel("Frequency of hashtags", labelpad=14)
    plt.ylabel("Hashtags", labelpad=14)
    plt.title("Frequency of Top 30 Hashtags", y=1.02)
    plt.savefig(file_name+".png", bbox_inches='tight')
    plt.show()
    plt.close()
   
#Top 30 hashtags when using #WFH,#workfromhome,#workingfromhome,WorkFromHome,#wfh
plot_topn(temp_hash_df, 30, 'HashTag', 'Frequency_Top30HasTags')

plot_topn(hash_before_df, 30, 'HashTag', 'Frequency_Top30HasTags_Before')

plot_topn(hash_after_df, 30, 'HashTag', 'Frequency_Top30HasTags_After')

#Plot number of tweets over time
data_file = 'Twitter_Sentiment.csv'
tweets_csv = path + data_file 
tweets_df = pd.read_csv (tweets_csv)
tweets_df['created_at']= pd.to_datetime(tweets_df['created_at'])
  
#Plot number of tweets over time
month_group = tweets_df['created_at'].groupby(tweets_df.created_at.dt.to_period("M")).agg('count')

overtime = month_group.plot(kind='bar',figsize=(15,8),color = 'indigo',legend=None)
overtime.set_alpha(0.8)
overtime.set_title("Total number of Tweets overtime", fontsize=22)
overtime.set_xlabel("Month,Year", fontsize=15);
overtime.set_ylabel("Number of Tweets", fontsize=15);
plt.savefig("Tweets_Overtime"+".png", bbox_inches='tight')
plt.show()



#Plotting number of redit posts overtime
redit_data_file = 'Redit_Sentiment.csv'
redit_csv = path + redit_data_file 
redit_df = pd.read_csv (redit_csv)
redit_df['timestamp']= pd.to_datetime(redit_df['timestamp'])

#WordCloud Reddit before cleanup
generate_wordcloud(redit_df,'body','WC_Before_Reddit')


month_group = redit_df['timestamp'].groupby(redit_df.timestamp.dt.to_period("M")).agg('count')

overtime = month_group.plot(kind='bar',figsize=(15,8),color = 'indigo',legend=None)
overtime.set_alpha(0.8)
overtime.set_title("Total number of Reddit posts overtime", fontsize=22)
overtime.set_xlabel("Month,Year", fontsize=15);
overtime.set_ylabel("Number of Reddit Posts", fontsize=15);
plt.savefig("Redit_Overtime"+".png", bbox_inches='tight')
plt.show()


#Plotting Positive and negative tweets
group_by_df = tweets_df.groupby([tweets_df.created_at.dt.to_period("M"),tweets_df.label]).agg('count')['created_at']

group_by_df.unstack().plot() 
plt.xticks(rotation=45) 
plt.xlabel('Tweets Created on')
plt.title('Number of Positive and Negative Tweets')
plt.ylabel('Count of Tweets')
plt.savefig("Positive_Negative_Overtime_Tweets"+".png", bbox_inches='tight')
plt.show()


#Plotting positive and negative reddit posts
group_by_df = redit_df.groupby([redit_df.timestamp.dt.to_period("M"),redit_df.label]).agg('count')['timestamp']

group_by_df.unstack().plot() 
plt.xticks(rotation=45) 
plt.xlabel('Reddit posts created on')
plt.title('Number of Positive and Negative Posts')
plt.ylabel('Count of Posts')
plt.savefig("Positive_Negative_Overtime_Reddit"+".png", bbox_inches='tight')
plt.show()

#Postive and Negative datasets
pos_df = tweets_df[tweets_df['label'] == 'POSITIVE']
neg_df = tweets_df[tweets_df['label'] == 'NEGATIVE']

# pos_plot = plt.plot(pos_df['created_at'],pos_df['score'],'o')
# ned_plot = plt.plot(neg_df['created_at'],neg_df['score'],'o',color = 'red')

# plt.plot(pos_plot,ned_plot)



