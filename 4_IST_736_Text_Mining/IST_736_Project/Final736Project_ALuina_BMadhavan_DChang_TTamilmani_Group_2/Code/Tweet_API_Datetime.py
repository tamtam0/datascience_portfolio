#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Mar 13 16:32:14 2021

@authors: Bhavya Madhavan, Daphne Chang
"""

import requests
import os
import json
import pandas as pd    
import datetime
import urllib.parse
import time

token = 'your bearer token'
 
start_date = '2019-01-01'
end_date = '2021-02-28'

tweet_fields='tweet.fields=text,author_id,created_at'
query='(%23workfromhome OR %23wfh OR %23workingfromhome) -is:retweet -is:nullcast has:geo lang:en place_country:US'
max_results='max_results=500'

start_date_obj = datetime.datetime.strptime(start_date, '%Y-%m-%d')
start_url_date = urllib.parse.quote(start_date_obj.isoformat(timespec='seconds'))
print('Start Date:', start_url_date)
 
end_date_obj = datetime.datetime.strptime(end_date, '%Y-%m-%d').replace(hour=23, minute=59, second=59, microsecond=999999)
end_url_date = urllib.parse.quote(end_date_obj.isoformat(timespec='seconds'))
print('End Date:', end_url_date)

def search_twitter(query, tweet_fields, start_time, end_time, max_results, bearer_token = token):   
    headers = {"Authorization": "Bearer {}".format(bearer_token)}
    url = 'https://api.twitter.com/2/tweets/search/all?query={}&{}&{}&{}&{}'.format(
        query, tweet_fields, start_time, end_time, max_results)    
    response = requests.request("GET", url, headers=headers)
    time.sleep(6)
    print(response.status_code)
    if response.status_code != 200:
        raise Exception(response.status_code, response.text)
    return response.json()  
 
def json_response_query(start_date_obj,end_date_obj):
    start_url_date = urllib.parse.quote(start_date_obj.isoformat(timespec='seconds')+'Z')
    end_url_date = urllib.parse.quote(end_date_obj.isoformat(timespec='seconds'))
    while start_url_date <= end_url_date:
        end_date = start_date_obj + datetime.timedelta(days=1,seconds=-1)
        eud = urllib.parse.quote(end_date.isoformat(timespec='seconds') + 'Z')
       
        print('Json stuff')
       
        json_response = search_twitter(
                        query=query,
                        tweet_fields=tweet_fields,
                        start_time='start_time=' + start_url_date,
                        end_time= 'end_time=' + eud,
                        max_results=max_results,
                        bearer_token=token)
        
        print("{}-{}", start_url_date, eud)
        start_date_obj += datetime.timedelta(days=1)
       
        start_url_date = urllib.parse.quote(start_date_obj.isoformat(timespec='seconds')+'Z')
        
        with open("tweets.json", "a") as twitter_data_file:
            json.dump(json_response, twitter_data_file, indent=4, sort_keys=True)
      
json_response_query(start_date_obj,end_date_obj)


 

