#########################################################################################################
# Script that goes month by month and extracts all reddit posts related to work from home
#
# Antonio Luina
#########################################################################################################
import calendar
from time import sleep
from datetime import datetime, timezone
import urllib
import requests
import os
import re
import html
def CleanPost(sText, searchterm):
    sCleanText = sText
    sCleanText = html.unescape(sCleanText)
    sCleanText = re.sub(r'\s'," ",sCleanText)
    sCleanText = re.sub(r'http[s]?://.+\b',"",sCleanText)
    sCleanText = re.sub(r'"',"",sCleanText)
    sCleanText = re.sub(r'[^A-Za-z ]','',sCleanText)
    return sCleanText.strip()

def CreateTimeStamp(nYear, nMonth, nDay):
    return datetime(nYear,nMonth,nDay, 0, 0).timestamp()

def CreateQueryData(sQuery, nSize, tStart, tEnd):
    dData = {
        'q': sQuery,
        'size': nSize,
        'after': tStart,
        'before': tEnd
    }
    return dData

def GetResponse(sURL):
    res = requests.get(sURL)
    if "data" in res.json():
        if len(res.json()["data"]) > 0:
            return res.json()["data"]

def SaveData(lData, sFileName, sSearchTerm):
    sKey = 'body'
    sCreatedTimeStamp = 'created_utc'
    with open(sFileName, 'w')  as output_file:
        output_file.write('timestamp' + "," + 'body' + '\n')
        for dItem in lData:
            sTimeStamp = str(datetime.fromtimestamp(dItem["created_utc"], timezone.utc))
            sBody = CleanPost(dItem[sKey], sSearchTerm)
            sBody = sBody.encode()
            sBody = sBody.decode('utf-8')
            if sBody.strip() == "":
                continue
            output_file.write(sTimeStamp + "," + sBody + "\n")

# Query
lQueries = ['wfh', 'workingfromhome', 'WorkFromHome']
for sSearchTerm in lQueries:
    # Max Size, doesn't work that high but let's ask anyways
    nSize = 500
    sDirName = "RedditMonthByMonth"
    if not os.path.exists(sDirName):
        os.makedirs(sDirName)

    # Loop over year and months
    for nYear in range(2019, 2022, 1):
        print(nYear)
        for nMonth in range(1,13,1):
            print(nMonth)
            tRange = calendar.monthrange(nYear, nMonth)
            # Create the time stamp
            tStart = round(CreateTimeStamp(nYear, nMonth, 1))
            tEnd = round(CreateTimeStamp(nYear, nMonth, tRange[1]))
            dData = CreateQueryData(sSearchTerm, nSize, tStart, tEnd)
            print(dData)
            sURL = 'https://api.pushshift.io/reddit/search/comment/?' + urllib.parse.urlencode(dData)
            print(sURL)
            sleep(6)
            lData = GetResponse(sURL)
            if lData is None or len(lData) < 1:
                continue
            sFileName = sDirName + os.path.sep + sSearchTerm + str(nYear) + "-" + str(nMonth) + ".csv"
            SaveData(lData, sFileName, sSearchTerm)
