#######################################################################################################
# This script processes the results of Sentiment Reddit.
# It takes as an input a pickled file
#
# Antonio 3/20/021
#######################################################################################################
import pickle
import os
import shutil
from sklearn.metrics import plot_confusion_matrix
import matplotlib.pyplot as plt
import re
import csv
import pandas as pd
import threading
import PredictSentiment
import math
import seaborn as sns
import numpy as np

def main():
    TopN = 20
    bRunResults = False
    nRandomSeed = 2515889939
    # Results Key
    sResultsKey = 'Results'
    if bRunResults:
        nRandomSeed = PredictSentiment.main()

    sOutputFolder = 'SentimentResults-' + str(nRandomSeed)
    os.makedirs(sOutputFolder, exist_ok=True)

    sFileName = sOutputFolder + os.path.sep + 'Sentiment.pickle'
    sImagesOutputFolder = sOutputFolder + os.path.sep + 'Images'
    sSummaryOutputFolder = sOutputFolder + os.path.sep + 'Summary'
    sTablesOutputFolder = sOutputFolder + os.path.sep + 'Tables'
    # Process Results
    ProcessSentimentResults(sOutputFolder, sFileName, sImagesOutputFolder, sTablesOutputFolder, sSummaryOutputFolder, TopN, sResultsKey)

def ProcessSentimentResults(sOutputFolder, sFileName, sImagesOutputFolder, sTablesOutputFolder, sSummaryOutputFolder, TopN, sResultsKey):
    # Delete any old results
    if os.path.isdir(sImagesOutputFolder):
        print('Cleaning up results on',sImagesOutputFolder)
        shutil.rmtree(sImagesOutputFolder)
    # Check if the output directories exists
    if os.path.isdir(sTablesOutputFolder):
        print('Cleaning up results on',sTablesOutputFolder)
        shutil.rmtree(sTablesOutputFolder)
    # Check if the output directories exists
    if os.path.isdir(sSummaryOutputFolder):
        print('Cleaning up results on',sSummaryOutputFolder)
        shutil.rmtree(sSummaryOutputFolder)

    # Create output directories
    os.makedirs(sTablesOutputFolder, exist_ok=True)
    os.makedirs(sImagesOutputFolder, exist_ok=True)
    os.makedirs(sSummaryOutputFolder, exist_ok=True)

    # Load the pickled file
    dSentiment = pickle.load(open(sFileName, 'rb'))

    print("This results were run with a random seed of", dSentiment['RandomSeed'])

    # print all keys
    for sKey in dSentiment:
        print(sKey)
    # print number of models
    print('There are', len(dSentiment[sResultsKey]), 'in the results')

    ###############################################################################################
    # Print results for each model
    ###############################################################################################
    lResultsSummary = [['Combination', 'AverageScore','BestScore','F1 Score']]
    dResults = dSentiment[sResultsKey]

    dResultsMatrix = {}
    # Loop through all models and build a summary table
    for sKey in dResults:
        print('Processing',sKey)
        SaveConfussionMatrix(dResults, sKey, sImagesOutputFolder)
        for tResult in dResults[sKey]:
            sVectorizer = tResult[0]
            print(sVectorizer)
            # Get the results
            dResult = tResult[1]

            lResultsSummary.append([sKey + ' ' + sVectorizer,dResult.AverageScore, dResult.bestScore,dResult.F1Score])


    # Open a file to write Multinomial Results
    with open(sTablesOutputFolder + os.path.sep + 'SentimentResults.csv','w', newline='') as fMultinomialResults:
        # using csv.writer method from CSV package
        csvfMultinomialResults = csv.writer(fMultinomialResults)

        csvfMultinomialResults.writerow(lResultsSummary[0])
        csvfMultinomialResults.writerows(lResultsSummary[1:])

    PlotBarChartOfTopNModels(dResults, sSummaryOutputFolder, 5)
    # Print the best confusion matrices
    SaveTopNModelsConfusionMatrices(dResults, sSummaryOutputFolder, nTopN=5)
    PrintTopWords(dResults, sSummaryOutputFolder + os.path.sep + 'TopWords.png')

def PlotBarChartOfTopNModels(dResults, sSummaryOutputFolder, nTopN=10):
    lLabels = []
    lF1Scores = []
    for sModel in dResults:
        print(sModel)
        dMaxF1Score = 0
        sLabel = sModel
        for sVectorizer, dResult in dResults[sModel]:
            # if dResult.F1Score > dMaxF1Score:
            sLabel = sModel + " " + sVectorizer
            dMaxF1Score = dResult.F1Score
            lLabels.append(sLabel)
            lF1Scores.append(dMaxF1Score)
    height, X = [], []
    for tSortedVal in list(sorted(list(zip(lF1Scores,lLabels))))[-nTopN:]:
        height.append(tSortedVal[0])
        X.append(tSortedVal[1])
    minVal = min(height)
    maxVal = max(height)
    fig, axes = plt.subplots(figsize=(12, 6))
    axes.barh(X,height)
    axes.axvline(x=maxVal)
    plt.xlim([minVal - minVal * 0.25, 1.0])
    fig.suptitle("Top " + str(nTopN) + " F1-Scores")
    fig.tight_layout()
    fig.savefig(sSummaryOutputFolder + os.path.sep + "BestModels.png")

def SaveTopNModelsConfusionMatrices(dResults, sSummaryOutputFolder, nTopN=10):
    lLabels = []
    lF1Scores = []
    lMatricesUnsorted = []
    for sModel in dResults:
        # print(sModel)
        dMaxF1Score = 0
        sLabel = sModel
        for sVectorizer, dResult in dResults[sModel]:
            # if dResult.F1Score > dMaxF1Score:
            dMaxF1Score = dResult.F1Score
            lMatrix = dResult.bestConfusionMatrix.astype(int).tolist()

            lLabels.append(sLabel)
            lF1Scores.append(dMaxF1Score)
            sLabel = sModel + "\n" + sVectorizer + "\nF1-Score" + '{0:.2f}'.format(dMaxF1Score)
            lMatricesUnsorted.append((lMatrix, sLabel))
    height, X = [], []
    lMatrices = []
    for tSortedVal in sorted(list(zip(lF1Scores, lLabels, lMatricesUnsorted)),reverse=True)[:nTopN]:
        lMatrices.append(tSortedVal[2])

    lTickLabels = ['NEGATIVE', 'POSITIVE']
    sTitle = "Top " + str(nTopN) + " Confusion Matrices"

    confusion_matrix_subplots(sTitle, lMatrices, 5, lTickLabels, lTickLabels, sSummaryOutputFolder + os.path.sep + "BestModelsConfusionMatrices.png")

# Function that extract the confusion matrices and plots it into a PNG
def SaveConfussionMatrix(dResults, sKey, sOutputFolder):
    lMatrices = []
    for sVectorizer, dResult in dResults[sKey]:
        sTitle = sVectorizer + "\n F1 Score " + '{0:.2f}'.format(dResult.F1Score)
        lMatrix = dResult.bestConfusionMatrix.astype(int).tolist()
        lMatrices.append((lMatrix, sTitle))
        pass
    lTickLabels = ['NEGATIVE', 'POSITIVE']
    sFileName = sKey
    sFileName = re.sub(r'[^A-Za-z0-9]', "", sFileName)
    sFileName = sOutputFolder + os.path.sep + sFileName + ".png"
    confusion_matrix_subplots(sKey, lMatrices, 4, lTickLabels,lTickLabels, sFileName )
    pass
# for 9 fold cross validation
def confusion_matrix_subplots(main_title, lMatrices, nColumns, lXTickLabels, lYTickLabels, sFileName):
    # Calculate the number of rows
    nRows = math.ceil(len(lMatrices) / nColumns)
    # Create Subplots with 3 columns
    fig, axes = plt.subplots(nrows=nRows, ncols=nColumns, figsize=(15, 15), constrained_layout=True)
    # set some padding in between subplots
    # fig.subplots_adjust(wspace=2, hspace=2)
    # Flatten the axis to match the input matrices
    axes = axes.flatten()
    # Loop over each axis
    plt.suptitle(main_title)
    for nIndex, axis in enumerate(axes):
        if nIndex < len(lMatrices):
            svm = sns.heatmap(lMatrices[nIndex][0], annot=True, cmap='Blues', ax=axis, cbar=False, xticklabels=lXTickLabels, yticklabels=lYTickLabels, square=True, fmt="d")
            axis.set_title(lMatrices[nIndex][1])
            axis.set_ylabel('Expected')
            axis.set_xlabel('Predicted')
        else:
            fig.delaxes(axis)

    # fig.tight_layout()
    fig.savefig(sFileName, dpi=1200)
    # plt.show()

def PrintTopWords(dResults, sFileName):
    lLabels = []
    lF1Scores = []
    lModels = []
    for sModel in dResults:
        # print(sModel)
        dMaxF1Score = 0
        sLabel = sModel
        for sVectorizer, dResult in dResults[sModel]:
            # if dResult.F1Score > dMaxF1Score:
            sLabel = sModel + " " + sVectorizer
            dMaxF1Score = dResult.F1Score
            lLabels.append(sLabel)
            lF1Scores.append(dMaxF1Score)
            lModels.append(dResult);
    height, X = [], []
    for tSortedVal in sorted(list(zip(lF1Scores,lLabels,lModels)),reverse=True):
        # Check if we can plot it
        # if is multinomial binomial or linear SVM
        if 'Multinomial' in tSortedVal[1] or 'BernoulliNB' in tSortedVal[1] or ('SVM' in tSortedVal[1] and 'linear' in tSortedVal[1]):
            # print(tSortedVal[1])
            TopNWords = tSortedVal[2].TopNWords
            break

    X = []
    y = []
    [(y.append(dVal),X.append(sWord)) for dVal, sWord in TopNWords]
    fig, ax = plt.subplots(figsize=(12, 6),constrained_layout=True)
    fig.suptitle("Top Words", fontsize=24)
    colors = ['blue'] * int(len(y) / 2) + ['red'] * int(len(y) / 2)
    ax.bar(X, y, width=.5,  color=colors)
    plt.xticks([nIndex for nIndex, sLabel in enumerate(X)], X, rotation=67.5)
    # fig.tight_layout()
    fig.savefig(sFileName, dpi=1200)
    # plt.show()
    pass

if __name__ == '__main__':
    main()