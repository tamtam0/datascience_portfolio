#########################################################################################################
# Script that reads labeled/Redit_Sentiment...csv and builds different models to predict sentiment
#
# Antonio Luina
# 3/19/2021
#########################################################################################################

import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.naive_bayes import BernoulliNB, MultinomialNB
from sklearn.svm import SVC
from sklearn.metrics import plot_confusion_matrix, f1_score
from RunModel import *
import matplotlib.pyplot as plt
import numpy as np
import random
import pickle
import re
import os
import shutil
import threading
import random
import time

def main():
    # Whether it runs or not un multi threaded
    bMultiThread = True
    # Create a random seed
    nRandomSeed = random.randint(1,2**32 - 1)
    # nRandomSeed = 2106709061
    nRandomSeed = 2515889939
    print("Using Random Seed", nRandomSeed)
    nAnalysisSize = 5000
    dTrainDistribution = 0.7
    nFoldCount = 20

    PredictSentiment(bMultiThread, nRandomSeed, nAnalysisSize, dTrainDistribution, nFoldCount)

    return nRandomSeed

def PredictSentiment(bMultiThread, nRandomSeed, nAnalysisSize, dTrainDistribution, nFoldCount):
    start = time.time()

    # Setup output folder
    sOutputFolder = 'SentimentResults-' + str(nRandomSeed)
    os.makedirs(sOutputFolder, exist_ok=True)

    # Keyword to track threads.
    sThreadKeyWord='sentiment'

    sOutputFileName = sOutputFolder + os.path.sep + "Sentiment.pickle"

    # Reddit data filename
    sRedditSentimentInputFile = 'Data' + os.path.sep + 'Redit_Sentiment.csv'

    # Twitter data filename
    sTwitterSentimentInputFile = 'Data' + os.path.sep + 'Twitter_Sentiment.csv'

    # Load the data
    dfReddit = pd.read_csv(sRedditSentimentInputFile)

    # Load the data
    dfTwitter = pd.read_csv(sTwitterSentimentInputFile)

    # Body
    serPosts = dfReddit['body']
    # add twitter data
    serPosts = serPosts.append(dfTwitter['text']).reset_index(drop=True)

    # Labels
    serLabels = dfReddit['label']
    #add twitter labels
    serLabels = serLabels.append(dfTwitter['label']).reset_index(drop=True)

    # Create a data frame for cleanup
    pdPosts = pd.DataFrame({'posts': serPosts, 'label': serLabels})
    pdPosts.dropna(axis=0, inplace=True)
    pdPosts.drop_duplicates(inplace=True)
    pdPosts.reset_index(drop=True, inplace=True)

    # Reduced the dataset size.
    pdPosts.to_csv(sOutputFolder + os.path.sep + "AllPosts-allrecords.csv", index=False)
    pdPosts = ExtractBalancedRecords(pdPosts, nAnalysisSize, nRandomSeed)
    pdPosts.to_csv(sOutputFolder + os.path.sep + "AllPosts-5k.csv", index=False)

    # Separate again
    serPosts = pdPosts['posts']
    serLabels = pdPosts['label']

    print("Training Model using {0:d} records".format(nAnalysisSize ))
    print(pdPosts)

    #####################################################################################
    # Vectorize the text
    #####################################################################################
    # Create a list of tokenizers CountVectorizers for later
    dVectorizers = {}
    # Create a Count Vectorizer for Binary Count Vectorizer
    cvBinary = CountVectorizer(stop_words='english', tokenizer=WordCleaner, binary=True)
    cvFit = cvBinary.fit_transform(serPosts)
    dVectorizers["Binary"] = cvBinary, cvFit

    # Create a Count Vectorizer for Binary Count Vectorizer Stemmed
    cvBinaryPorterStemmer = CountVectorizer(stop_words='english', tokenizer=MyPorterStemmer, binary=True)
    cvFitBinaryPorterStemmer = cvBinaryPorterStemmer.fit_transform(serPosts)
    dVectorizers["Binary Porter Stemmer"] = cvBinaryPorterStemmer, cvFitBinaryPorterStemmer

    # Create a Count Vectorizer for Binary Count Vectorizer Stemmed
    cvBinarySnowballStemmer = CountVectorizer(stop_words='english', tokenizer=MySnowballStemmer, binary=True)
    cvFitBinarySnowballStemmer = cvBinarySnowballStemmer.fit_transform(serPosts)
    dVectorizers["Binary Snowball Stemmer"] = cvBinarySnowballStemmer, cvFitBinarySnowballStemmer

    # Create a Count Vectorizer for Binary Count Vectorizer Stemmed
    cvBinaryWordNetLemmer = CountVectorizer(stop_words='english', tokenizer=MyWordNetLemmer, binary=True)
    cvFitBinaryWordNetLemmer = cvBinaryWordNetLemmer.fit_transform(serPosts)
    dVectorizers["Binary WordNet Lemmer"] = cvBinaryWordNetLemmer, cvFitBinaryWordNetLemmer

    # Create a Count Vectorizer for Term Frequencies
    cvTF = CountVectorizer(stop_words='english', tokenizer=WordCleaner, binary=False)
    cvFitTF = cvTF.fit_transform(serPosts)
    dVectorizers["Term Frequency"] = cvTF, cvFitTF

    # Create a Count Vectorizer for Term Frequencies Stemmed
    cvTFPorterStemmer = CountVectorizer(stop_words='english', tokenizer=MyPorterStemmer)
    cvFitTFPorterStemmer = cvTFPorterStemmer.fit_transform(serPosts)
    dVectorizers["Term Frequency Porter Stemmer"] = cvTFPorterStemmer, cvFitTFPorterStemmer

    # Create a Count Vectorizer for Term Frequencies Stemmed
    cvTFSnowballStemmer = CountVectorizer(stop_words='english', tokenizer=MySnowballStemmer)
    cvFitTFSnowballStemmer = cvTFSnowballStemmer.fit_transform(serPosts)
    dVectorizers["Term Frequency SnowBall Stemmer"] = cvTFSnowballStemmer, cvFitTFSnowballStemmer

    # Create a Count Vectorizer for Term Frequencies Stmed
    cvTFWordNetLemmer = CountVectorizer(stop_words='english', tokenizer=MyWordNetLemmer)
    cvFitTFWordNetLemmer = cvTFWordNetLemmer.fit_transform(serPosts)
    dVectorizers["Term Frequency WordNet Lemmed"] = cvTFWordNetLemmer, cvFitTFWordNetLemmer

    # Create a Count Vectorizer normalized Term Frequency
    cvNormalizedTF = TfidfVectorizer(stop_words='english', tokenizer=WordCleaner, use_idf=False)
    cvFitNormalizedTF = cvNormalizedTF.fit_transform(serPosts)
    dVectorizers["Normalized Term Frequency"] = cvNormalizedTF, cvFitNormalizedTF

    # Create a Count Vectorizer normalized Term Frequency
    cvNormalizedTFPorterStemmer = TfidfVectorizer(stop_words='english', use_idf=False, tokenizer=MyPorterStemmer)
    cvFitNormalizedTFPorterStemmer = cvNormalizedTFPorterStemmer.fit_transform(serPosts)
    dVectorizers["Normalized Term Frequency - Porter Stemmer"] = cvNormalizedTFPorterStemmer,cvFitNormalizedTFPorterStemmer

    # Create a Count Vectorizer normalized Term Frequency
    cvNormalizedTFSnowballStemmer = TfidfVectorizer(stop_words='english', use_idf=False, tokenizer=MySnowballStemmer)
    cvFitNormalizedTFSnowballStemmer = cvNormalizedTFSnowballStemmer.fit_transform(serPosts)
    dVectorizers["Normalized Term Frequency - SnowBall Stemmer"] = cvNormalizedTFSnowballStemmer, cvFitNormalizedTFSnowballStemmer

    # Create a Count Vectorizer normalized Term Frequency
    cvNormalizedTFWordNetLemmer = TfidfVectorizer(stop_words='english', use_idf=False, tokenizer=MyWordNetLemmer)
    cvFitNormalizedTFWordNetLemmer = cvNormalizedTFWordNetLemmer.fit_transform(serPosts)
    dVectorizers["Normalized Term Frequency - WordNet Lemmer"] = cvNormalizedTFWordNetLemmer, cvFitNormalizedTFWordNetLemmer

    # Create a Count Vectorizer normalized Term Frequency
    cvTFIDF = TfidfVectorizer(stop_words='english', tokenizer=WordCleaner, use_idf=True)
    cvFitTFIDF = cvTFIDF.fit_transform(serPosts)
    dVectorizers["TF/IDF"] = cvTFIDF, cvFitTFIDF

    # Create a Count Vectorizer normalized Term Frequency
    cvTFIDFPorterStemmer = TfidfVectorizer(stop_words='english', use_idf=True, tokenizer=MyPorterStemmer)
    cvFitTFIDFPorterStemmer = cvTFIDFPorterStemmer.fit_transform(serPosts)
    dVectorizers["TF/IDF - Porter Stemmer"] = cvTFIDFPorterStemmer,cvFitTFIDFPorterStemmer

    # Create a Count Vectorizer normalized Term Frequency
    cvTFIDFSnowballStemmer = TfidfVectorizer(stop_words='english', use_idf=True, tokenizer=MySnowballStemmer)
    cvFitTFIDFSnowballStemmer = cvTFIDFSnowballStemmer.fit_transform(serPosts)
    dVectorizers["TF/IDF - SnowBall Stemmer"] = cvTFIDFSnowballStemmer, cvFitTFIDFSnowballStemmer

    # Create a Count Vectorizer normalized Term Frequency
    cvTFIDFWordNetLemmer = TfidfVectorizer(stop_words='english', use_idf=True, tokenizer=MyWordNetLemmer)
    cvFitTFIDFWordNetLemmer = cvTFIDFWordNetLemmer.fit_transform(serPosts)
    dVectorizers["TF/IDF - WordNet Stemmer"] = cvTFIDFWordNetLemmer, cvFitTFIDFWordNetLemmer
    #####################################################################################
    #####################################################################################

    # List of kernels to run
    lKernels = ['linear', 'poly', 'rbf']

    # Let's creeate a list of the analysis to run
    lModels = ['MultinomialNB', 'SVM', 'BernoulliNB']

    # Create a list of costs to run with
    lCost = [0.001, 0.01, 0.1, 1, 100, 1000, 10000]
    lCost = [0.001, 1, 1000]

    dResults = {}
    nMaxOfThreads = 12
    # Let's loop over the models to be create
    for sVectorizer in dVectorizers:
        print('Processing', sVectorizer)
        X = dVectorizers[sVectorizer][1]
        Vectorizer = dVectorizers[sVectorizer][0]
        lFeatureNames = dVectorizers[sVectorizer][0].get_feature_names()
        # Loop over the models
        for sModel in lModels:
            # if the model is SVM
            if sModel == 'SVM':
                if 'Binary' in sVectorizer:
                    continue
                for sKernel in lKernels:
                    for dCost in lCost:
                        sKey = sModel + ' ' + sKernel + sVectorizer + ' Cost ' + str(dCost)
                        if bMultiThread:
                            print('Running', sModel, 'with kernel', sKernel, 'with a cost of', dCost, ' ', sVectorizer, 'Parallel'); \
                            # Create a thread object
                            thrd = threading.Thread(group=None, target=CallSVM, name=sThreadKeyWord + sKey, args=(sModel, Vectorizer, sKernel, sVectorizer, dCost, X, nFoldCount, serLabels, lFeatureNames, nRandomSeed, dResults, nAnalysisSize, dTrainDistribution))
                            # lock until we have space
                            WaitForSapce(nMaxOfThreads, sThreadKeyWord)

                            # we got here there is space
                            thrd.start()
                        else:
                            # Create a key for saving the results
                            print('Running', sModel, 'with kernel', sKernel, 'with a cost of', dCost, ' ', sVectorizer); \
                            # # Create a SVM;
                            # svm = SVC(kernel=sKernel, C=dCost);
                            # res = RunModel(X=X, y=serLabels, K=20, featureNames=lFeatureNames, model=svm, nRandomSeed=nRandomSeed)
                            # dResults[sKey] = res
                            CallSVM(sModel, Vectorizer, sKernel, sVectorizer, dCost, X, nFoldCount, serLabels, lFeatureNames, nRandomSeed, dResults, nAnalysisSize, dTrainDistribution)


            # Multinomial Naive Bayes and Bernoulli
            elif sModel == 'MultinomialNB':
                if 'Binary' in sVectorizer:
                    continue
                # Create a key for multinomial
                sKey = sModel + ' ' + sVectorizer
                if bMultiThread:
                    print('Running',sModel, sVectorizer, 'Parallel')
                    # Create a thread object
                    thrd = threading.Thread(group=None, target=CallMultinomialNB, name=sThreadKeyWord + sKey, args=(sModel, Vectorizer, sVectorizer, X, nFoldCount, serLabels, lFeatureNames, nRandomSeed, dResults, nAnalysisSize, dTrainDistribution))

                    # lock until we have space
                    WaitForSapce(nMaxOfThreads, sThreadKeyWord)

                    # we got here there is space
                    thrd.start()
                else:
                    print('Running',sModel, sVectorizer)
                    # # Create a Multinomial Naive Bayes
                    # mNB = MultinomialNB()
                    # res = RunModel(X = X, y = serLabels,K=20,featureNames=lFeatureNames,model = mNB, nRandomSeed=nRandomSeed)
                    # # Save it
                    # dResults[sKey] = res
                    CallMultinomialNB(sModel, Vectorizer, sVectorizer, X, nFoldCount, serLabels, lFeatureNames, nRandomSeed, dResults, nAnalysisSize, dTrainDistribution)
            # Multinomial Naive Bayes and Bernoulli
            elif sModel == 'BernoulliNB':
                # only run binary models with Bernoulli
                if not 'Binary' in sVectorizer:
                    continue
                # Create a key for Binomial
                sKey = sModel + ' ' + sVectorizer
                if bMultiThread:
                    print('Running',sModel, sVectorizer, 'Parallel')
                    # Create a thread object
                    thrd = threading.Thread(group=None, target=CallBernoulliNB, name=sThreadKeyWord + sKey, args=(sModel, Vectorizer, sVectorizer, X, nFoldCount, serLabels, lFeatureNames, nRandomSeed,dResults, nAnalysisSize, dTrainDistribution))

                    # lock until we have space
                    WaitForSapce(nMaxOfThreads, sThreadKeyWord)

                    # we got here there is space
                    thrd.start()
                else:
                    print('Running',sModel, sVectorizer)
                    # # Create a Multinomial Naive Bayes
                    # mNB = BernoulliNB()
                    # res = RunModel(X = X, y = serLabels,K=20,featureNames=lFeatureNames,model = mNB, nRandomSeed=nRandomSeed)
                    # # Save it
                    # dResults[sKey] = res
                    CallBernoulliNB(sModel, Vectorizer , sVectorizer, X, nFoldCount, serLabels, lFeatureNames, nRandomSeed,dResults, nAnalysisSize, dTrainDistribution)

    # Let's wait for all to finish
    if bMultiThread:
        WaitForAll(sThreadKeyWord)

    end = time.time()

    difference = end - start

    print("Ran analysis in:", difference)

    dSavedData = {
        "RandomSeed": nRandomSeed
        , 'Results':dResults
        , 'Vectorizers': list(dVectorizers.keys())
        , 'Runtime': difference
    }

    # Delete any old results
    if os.path.isdir(sOutputFolder):
        print('Cleaning up results on',sOutputFolder)
        shutil.rmtree(sOutputFolder)
    os.makedirs(sOutputFolder, exist_ok=True)


    with open(sOutputFileName, 'wb') as fReddit:
        pickle.dump(dSavedData, fReddit)
def ExtractBalancedRecords(pdPosts, nNumberOfRecords, nRandomSeed):
    """
    Function that extract a balanced number of records out of the data frame
    It will, randomized nNumberOfRecords that have the same number of POSITIVE and NEGATIVE.
    nNumberOfRecords, if not even, will be (nNumberOfRecords - 1)
    :param pdPosts:
    :param nNumberOfRecords:
    :return:
    """

    # if even use the same number of records to extract
    if nNumberOfRecords % 2 == 0:
        nRecordsToExtract = int(nNumberOfRecords / 2)
    else: # Otherwise subtract one.
        nRecordsToExtract = int((nNumberOfRecords - 1) / 2)

    # if the number of records to be extracted is less than 2, all records will be extracted, so we can return
    if nRecordsToExtract < 2:
        return pdPosts

    # Get the labels
    lLabels = set(list(pdPosts['label']))

    # Create a list of temporary data frames.
    dfOut = None

    for sLabel in list(lLabels):
        if dfOut is None:
            dfOut = pdPosts[pdPosts['label'].isin([sLabel])].sample(nRecordsToExtract, random_state=nRandomSeed)
            print(dfOut)
        else:
            dfOut = dfOut.append(pdPosts[pdPosts['label'].isin([sLabel])].sample(nRecordsToExtract, random_state=nRandomSeed))
    dfOut.reset_index(drop=True, inplace=True)

    return dfOut

##################################################################################################################
# Function to call SVM
##################################################################################################################
def CallSVM(sModel, oVectorizer, sKernel, sVectorizer, dCost, X, nFoldCount, serLabels, lFeatureNames, nRandomSeed, dResults, nAnalysisSize, dTrainDistribution):
    sKey = sModel + ' ' + sKernel  + ' Cost ' + str(dCost)
    # Create a key for saving the results
    # print('Running', sModel, 'with kernel', sKernel, 'with a cost of', dCost, ' ', sVectorizer);
    svm = SVC(kernel=sKernel, C=dCost);
    res = RunModel(X=X, y=serLabels, K=nFoldCount, featureNames=lFeatureNames, model=svm, Vectorizer=oVectorizer, nRandomSeed=nRandomSeed, train_size=dTrainDistribution)
    if not sKey in dResults.keys():
        dResults[sKey] = []

    dResults[sKey].append((sVectorizer, res))
    pass
##################################################################################################################
##################################################################################################################
##################################################################################################################
# Function to call MultinomialNB
##################################################################################################################
def CallMultinomialNB(sModel, oVectorizer, sVectorizer, X, nFoldCount, serLabels, lFeatureNames, nRandomSeed, dResults, nAnalysisSize, dTrainDistribution):
    sKey = sModel
    # print('Running', sModel, sVectorizer)
    # Create a Multinomial Naive Bayes
    mNB = MultinomialNB()
    res = RunModel(X=X, y=serLabels, K=nFoldCount, featureNames=lFeatureNames, model=mNB, Vectorizer=oVectorizer, nRandomSeed=nRandomSeed, train_size=dTrainDistribution)
    # Save it
    if not sKey in dResults.keys():
        dResults[sKey] = []

    dResults[sKey].append((sVectorizer, res))

    pass
##################################################################################################################
##################################################################################################################
##################################################################################################################
# Function to call Binomial
##################################################################################################################
def CallBernoulliNB(sModel, oVectorizer, sVectorizer, X, nFoldCount, serLabels, lFeatureNames, nRandomSeed, dResults, nAnalysisSize, dTrainDistribution):
    # Create a key for Binomial
    sKey = sModel
    # print('Running', sModel, sVectorizer)
    # Create a Multinomial Naive Bayes
    mNB = BernoulliNB()
    res = RunModel(X=X, y=serLabels, K=nFoldCount, featureNames=lFeatureNames, model=mNB, Vectorizer=oVectorizer, nRandomSeed=nRandomSeed, train_size=dTrainDistribution)
    # Save it
    if not sKey in dResults.keys():
        dResults[sKey] = []

    dResults[sKey].append((sVectorizer, res))
    pass
##################################################################################################################
##################################################################################################################

if __name__ == '__main__':
    main()