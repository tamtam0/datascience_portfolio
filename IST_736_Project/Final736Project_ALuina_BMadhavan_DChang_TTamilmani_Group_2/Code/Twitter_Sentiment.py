#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: tamil, bhavya
"""

import json
import glob
import pandas as pd
from io import StringIO
from transformers import pipeline

path = "Twitter"
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

#jsons[0][0]['data']
#pd.DataFrame.from_records(jsons[0])
#pd.read_json(all_files[0])
#json.load(open(all_files[0]))

twitter_df = pd.concat(jsons, axis=0,ignore_index=True)

import re
def CleanPost(sText):
    sCleanText = sText.lower()
    sCleanText = " ".join(filter(lambda x:x[0]!='#', sCleanText.split()))
    sCleanText = re.sub(r'\s'," ",sCleanText)
    sCleanText = re.sub(r'http[s]?://.+\b',"",sCleanText)
    sCleanText = re.sub(r"\.com", "dotcom", sCleanText)
    sCleanText = re.sub(r'"',"",sCleanText)
    sCleanText = re.sub(r'[^\w ]','',sCleanText)
    # remove escaped characters
    sCleanText = re.sub(r"\\[\w]{1}", " ", sCleanText)
    # removing anything that is not a letter or a number
    sCleanText = re.sub(r"[^A-Za-z ]|[0-9]", "", sCleanText)
    sCleanText = re.sub(r"wfh|workfromhome|workingfromhome", "", sCleanText)
    return sCleanText.strip()


for i in range(len(twitter_df)):
    twitter_df['text'][i] = CleanPost(twitter_df['text'][i])

classifier = pipeline('sentiment-analysis')

# data['sentiment'] = classifier(list(data.text))
# senti_list =[]
twitter_df = (
    twitter_df
        .assign(sentiment = lambda x: x['text'].apply(lambda s: classifier(s)))
        .assign(
        label = lambda x: x['sentiment'].apply(lambda s: (s[0]['label'])),
        score = lambda x: x['sentiment'].apply(lambda s: (s[0]['score']))
    )
)



twitter_df.to_csv ('labeled/Twitter_Sentiment.csv', index = False, header=True)

twitter_df.to_json ('labeled/Twitter_Sentiment.json')
