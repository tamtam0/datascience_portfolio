####################################################################################################################
# This module, meant to be imported, runs the given a model, number of folds (K), Data (X), Labels (y),
# Feature Names, and the number of popular features to be extracted (TopN)
#
#
# Antonio Luina
# 2/27/2021
#
# 2/19/2021 - Antonio Luina - Added Stemmer and cleaner functions.
####################################################################################################################

# Imports
from sklearn.model_selection import StratifiedShuffleSplit
from sklearn.metrics import confusion_matrix, accuracy_score, f1_score

import pandas as pd
import numpy as np
import re
from nltk.stem import SnowballStemmer, PorterStemmer, WordNetLemmatizer
from sklearn.svm import SVC
import threading
import uuid
bMultiThread = True
nMaxOfThreads = 12
sFoldThreadKeyWord='fold'
# Functions for cleaning up the data
# Create a stemmer
ssSnowBallStemmer = SnowballStemmer('english', ignore_stopwords=True)
psPorterStemmer = PorterStemmer()
wnLemmer = WordNetLemmatizer()

class ModelResults:
    def __init__(self):
        self.AverageScore = None
        self.bestModel = None
        self.bestScore = None
        self.bestConfusionMatrix = None
        self.X_train_best = None
        self.y_train_best = None
        self.X_test_best = None
        self.y_test_best = None
        self.Accuracy = None
        self.F1Score = None
        self.FeatureNames = None
        self.TopNWords = None
        self.Vectorizer = None

    def __str__(self):

        sOut = ""
        sOut = sOut + "Average Score:" + str(self.AverageScore) + "\n"
        sOut = sOut + "Best Model:" + str(self.bestModel) + "\n"
        sOut = sOut + "Best Score:" + str(self.bestScore) + "\n"
        sOut = sOut + "Vectorizer:" + str(self.Vectorizer) + "\n"
        if not self.bestConfusionMatrix is None:
            sOut = sOut + "Confusion Matrix:" + "\n"
            for Row in self.bestConfusionMatrix:
                for Column in Row:
                    sOut = sOut + '{0:7s}'.format(str(Column))
                sOut += "\n"
        else:
            sOut = sOut + "Confusion Matrix:" + str(self.bestConfusionMatrix) + "\n"

        # if not self.bestTopDF is None:
        #     sOut = sOut + "bestTopDF:" + "\n"
        #     for Current in self.bestTopDF:
        #         sOut = sOut + str(Current) + "\n"
        # else:
        #     sOut = sOut + "bestTopDF:" + str(self.bestTopDF) + "\n"

        return sOut

    def __repr__(self):
        return self.__str__()


def RunModel(X, y, K, featureNames, model, Vectorizer, TopN=20, nRandomSeed=0, train_size=0.7):
    '''
    Function that calculates a Multi Binomial Naive Bayes based on given
    data and labels.  It peforms a K Fold.
    :param X: Data, in array format
    :param y: Labels, in array format
    :param K: Number of Folds
    :return: ModelResults Object
    '''

    # Create a K fold of K Size
    sss = StratifiedShuffleSplit(n_splits=K, train_size=train_size, random_state=nRandomSeed)

    # Initialize a list to keep the scores.
    lScore = []

    # Counter to keep track of current fold
    nCurrentFold = 1

    # Initialize results
    results = ModelResults()
    results.FeatureNames = featureNames
    sModelUID = str(uuid.uuid4())
    # Loop through the K folds
    for train_index, test_index in sss.split(X, y):
        if bMultiThread:
            # Create a unique ID for the thread
            sID = str(uuid.uuid4())

            # Create a thread object
            thrd = threading.Thread(group=None, target=RunFold, name=sModelUID + sFoldThreadKeyWord + sID, args=(X, y, featureNames, model, Vectorizer, lScore, results, train_index, test_index, TopN))

            # lock until we have space
            WaitForSapce(nMaxOfThreads, sFoldThreadKeyWord)

            # we got here there is space
            thrd.start()

        else:
            RunFold(X, y, featureNames, model, Vectorizer, lScore, results, train_index, test_index, TopN)

    WaitForAll(sModelUID)
    # Calculate the average score
    results.AverageScore = np.array(lScore).sum() / len(lScore)

    # Return the average score, best model and best score
    return results

def RunFold(X, y, featureNames, model, Vectorizer, lScore, results, train_index, test_index, TopN=20):
    # print(train_index, test_index)
    # print("Processing Fold", nCurrentFold)
    # nCurrentFold += 1
    # Create the train and test data based on the current fold split
    X_train, y_train = X[train_index], y[train_index]
    X_test, y_test = X[test_index], y[test_index]

    # fit the training data
    model.fit(X_train, y_train)

    # Do the prediction
    y_predict = model.predict(X_test)
    # Calculate F1_Score
    y_true = y_test.to_list()
    F1_Score = f1_score(y_true, y_predict, pos_label="POSITIVE")

    # Calculate the score
    score = accuracy_score(y_test, y_predict)

    # Save the score to calculate average at the end.
    lScore.append(score)

    # Calculate Top N for current fold.
    lColumnNames = ['FeatureName']
    lColumnNames.extend(list(set(y)))
    class_labels = set(y)

    lTopN = []
    # if type(model.coef_) is np.
    if not type(model) is SVC:
        Sorted = sorted(zip(model.coef_[0],featureNames))
        LowerHalf = Sorted[:TopN]
        TopHalf = Sorted[-TopN:]
        TopHalf.reverse()
        LowerHalf.reverse()
        lTopN = TopHalf + LowerHalf
    elif ( type(model) is SVC and 'linear' == model.kernel):
        Sorted = sorted(zip(model.coef_.toarray()[0],featureNames))
        LowerHalf = Sorted[:TopN]
        TopHalf = Sorted[-TopN:]
        TopHalf.reverse()
        LowerHalf.reverse()
        lTopN = TopHalf + LowerHalf
        # todo:add code here to get feature words for SVM, linear
        pass
    else:
        # todo:add next ones here as you find them
        pass

    # dfAllLogProbabilities = pd.DataFrame(lTopN, columns=lColumnNames)
    # # Now let's isolate each label into its own data frame
    # # and save it sorted
    # lTopDF = []
    # for j, sClass in enumerate(set(y)):
    #     dfTemp = dfAllLogProbabilities[['FeatureName', sClass]].copy()
    #     dfTemp.sort_values(by=[sClass], inplace=True)
    #     lTopDF.append(dfTemp.iloc[-TopN:].copy())

    # calculate the confusion matrix
    confusionMatrix = confusion_matrix(y_test, y_predict)

    # save the best score if this is the first fold or
    # if we have improved.
    if results.bestModel is None or (score > results.bestScore):
        # print("New Best Score:", score)

        results.bestModel = model
        results.bestScore = score
        results.bestConfusionMatrix = confusionMatrix
        results.X_train_best = X_train
        results.y_train_best = y_train
        results.X_test_best = X_test
        results.y_test_best = y_test
        results.F1Score = F1_Score
        results.TopNWords = lTopN
        results.Vectorizer = Vectorizer

# Cleans a word
def CleanWord(sWord):
    # Get a copy of the word
    sCleanWord = sWord.lower()
    # Remove websites address
    sCleanWord = re.sub(r"\.com", "dotcom", sCleanWord)
    # remove escaped characters
    sCleanWord = re.sub(r"\\[\w]{1}", " ", sCleanWord)
    # removing anything that is not a letter or a number
    sCleanWord = re.sub(r"[^A-Za-z ]|[0-9]", "", sCleanWord)
    sCleanWord = re.sub(r"wfh|workfromhome|workingfromhome", "", sCleanWord)


    return sCleanWord

def WordCleaner(sDocument):
    lDoc = sDocument.split()
    lCleanDoc = []
    for sWord in lDoc:
        sCleanWord = CleanWord(sWord)
        if len(sCleanWord) > 0:
            lCleanDoc.append(sCleanWord)
    return lCleanDoc

# stemms the word
def MyPorterStemmer(sDocument):
    lDoc = sDocument.split()
    lCleanDoc = []
    for sWord in lDoc:
        sCleanWord = psPorterStemmer.stem(CleanWord(sWord))
        if len(sCleanWord) > 0:
            lCleanDoc.append(sCleanWord)
    return lCleanDoc
# stemms the word
def MySnowballStemmer(sDocument):
    lDoc = sDocument.split()
    lCleanDoc = []
    for sWord in lDoc:
        sCleanWord = ssSnowBallStemmer.stem(CleanWord(sWord))
        if len(sCleanWord) > 0:
            lCleanDoc.append(sCleanWord)
    return lCleanDoc
# stemms the word
def MyWordNetLemmer(sDocument):
    lDoc = sDocument.split()
    lCleanDoc = []
    for sWord in lDoc:
        sCleanWord = wnLemmer.lemmatize(CleanWord(sWord))
        if len(sCleanWord) > 0:
            lCleanDoc.append(sCleanWord)
    return lCleanDoc

##################################################################################################################
# Functions to manage threads
##################################################################################################################
##################################################################################################################
# Function that waits for all.
##################################################################################################################
def WaitForAll(sThreadKeyWord):
    while GetThreadCount(sThreadKeyWord) > 0:
        pass
##################################################################################################################
# Function that locks until there is space to run.
##################################################################################################################
def GetThreadCount(sThreadKeyWord):
    nCount = 0
    for thrd in  threading.enumerate():
        if sThreadKeyWord in thrd.name:
            if thrd.is_alive():
                nCount += 1
    return nCount


# Function that locks until there is space to run.
##################################################################################################################
def WaitForSapce(nMaxOfThreads, sThreadKeyWord):
    while GetThreadCount(sThreadKeyWord) >= nMaxOfThreads:
        pass
##################################################################################################################
##################################################################################################################
