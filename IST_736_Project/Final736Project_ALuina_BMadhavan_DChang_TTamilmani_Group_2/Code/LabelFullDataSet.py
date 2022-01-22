import pickle
import pandas as pd
import os
from RunModel import *

def LabelFullDataset(sDirectory, sRedditFile, sTwitterFile, sModelPath, sOutputDir):
    # Load the pandas
    dfReddit = pd.read_csv(sDirectory + os.path.sep + sRedditFile)
    dfTwitter = pd.read_csv(sDirectory + os.path.sep + sTwitterFile)

    # Cleanup Reddit
    dfReddit = dfReddit[['timestamp','body', 'label']]
    dfReddit['source'] = 'reddit'
    # update the column names to match reddit
    # timestamp	body	sentiment	label	score
    # author_id	created_at	id	text	withheld	sentiment	label	score
    dfTwitter = dfTwitter[['created_at','text', 'label']].copy()
    dfTwitter.columns = ['timestamp','body', 'label']

    # Cleanup
    dfTwitter.dropna(axis=0, inplace=True)
    dfTwitter.drop_duplicates(inplace=True)
    dfTwitter.reset_index(drop=True, inplace=True)
    dfTwitter['source'] = 'twitter'

    dfAllPosts = pd.concat([dfReddit, dfTwitter])


    # load the model
    oModel = pickle.load(open(sModelPath, 'rb'))
    oResults = oModel['Results']['SVM linear Cost 1000'][0][1]

    # Create a Count Vectorizer for Term Frequencies
    cvFitTF = oResults.Vectorizer.transform(dfAllPosts['body'])

    oPredictedLabels = oResults.bestModel.predict(cvFitTF.toarray())

    dfAllPosts['predictedlabels'] = oPredictedLabels
    dfAllPosts['era'] = dfAllPosts['timestamp'].apply(func=SetEra)
    dfAllPosts.to_csv(sOutputDir + os.path.sep + "LabeledResults.csv",index=False)
    pass

def SetEra(sVal):
    if '2019' in sVal or '2020-01' in sVal or '2020-02' in sVal:
        return 'before'
    else:
        return 'after'
def main():
    sRedditFile = 'Redit_Sentiment.csv'
    sTwitterFile = 'Twitter_Sentiment.csv'
    sDirectory = 'Data'
    sOutputDir = 'SentimentResults-2515889939'
    sModelPath = sOutputDir + os.path.sep + 'Sentiment.pickle'

    LabelFullDataset(sDirectory, sRedditFile, sTwitterFile, sModelPath, sOutputDir)



if __name__ == '__main__':
    main()