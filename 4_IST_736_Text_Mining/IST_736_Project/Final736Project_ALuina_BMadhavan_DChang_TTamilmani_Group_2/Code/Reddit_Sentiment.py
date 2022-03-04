#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: tamil, bhavya
"""

import json
import glob
import pandas as pd
from transformers import pipeline

path = "RedditMonthByMonth"
all_files = glob.glob(path + "/*.csv")

li = []

for filename in all_files:
    df = pd.read_csv(filename, index_col=None, header=0)
    li.append(df)

reddit_df = pd.concat(li, axis=0, ignore_index=True)


       
classifier = pipeline('sentiment-analysis')

# data['sentiment'] = classifier(list(data.text))

# senti_list =[]
reddit_df = (
    reddit_df
        .assign(sentiment = lambda x: x['body'].apply(lambda s: classifier(s)))
        .assign(
        label = lambda x: x['sentiment'].apply(lambda s: (s[0]['label'])),
        score = lambda x: x['sentiment'].apply(lambda s: (s[0]['score']))
    )
)



reddit_df.to_csv ('labeled/Redit_Sentiment.csv', index = False, header=True)