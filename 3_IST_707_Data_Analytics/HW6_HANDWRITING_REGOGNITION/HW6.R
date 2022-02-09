library(FactoMineR)
library(dplyr)
library(tidyverse)
library(tidyr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(ggplot2)
library(ggthemes)
#library(Factoshiny)

setwd("~/Downloads/digit-recognizer")
filename <-"train.csv"
DigitTotalDF <- read.csv(filename, header = TRUE, stringsAsFactors = TRUE) 
DigitTotalDF$label<-as.factor(DigitTotalDF$label)
dim(DigitTotalDF)
colnames<- names(DigitTotalDF)
names(DigitTotalDF)<- sub("pixel","",colnames)


pixels_gathered <- DigitTotalDF %>%
  mutate(instance = row_number()) %>%
  gather(pixel, value, -label, -instance) %>%
  tidyr::extract(pixel, "pixel", "(\\d+)", convert = TRUE) %>%
  mutate(pixel = pixel - 2,
         x = pixel %% 28,
         y = 28 - pixel %/% 28)

pixel_summary <- pixels_gathered %>%
  group_by(x, y, label) %>%
  summarize(mean_value = mean(value)) %>%
  ungroup()

pixels_joined <- pixels_gathered %>%
  inner_join(pixel_summary, by = c("label", "x", "y"))

image_distances <- pixels_joined %>%
  group_by(label, instance) %>%
  summarize(euclidean_distance = sqrt(mean((value - mean_value) ^ 2)))


#EDA

ggplot(DigitTotalDF,aes(x=label,fill=label)) + geom_bar() + theme_few()

#Pixel by color , percent
ggplot(pixels_gathered, aes(value)) +geom_histogram(aes(y=..count../sum(..count..)),bins=5) + theme_clean()


ggplot(image_distances, aes(factor(label), euclidean_distance,color=label)) +
  geom_boxplot() +
  labs(x = "Digit",
       y = "Euclidean distance to the digit centroid") + theme_clean()


# Training
set.seed(1024)

library(doParallel)
cl <- makePSOCKcluster(9)
registerDoParallel(cl)

plotConfusionMatrix<- function(predicted,actual){
  table<-(table(Predicted=predicted, TrueDigits=actual))
  print(table)
  cf<-confusionMatrix(table)
  print(cf$overall)
  table <- table / rowSums(table)
  confusion_matrix <- as.data.frame(table)
  plot<-ggplot(data = confusion_matrix,
         aes(x = Predicted,y = TrueDigits)) +
    geom_tile(aes(fill = Freq)) +
    #geom_text(aes(label = sprintf("%.2f", (Freq/sum(Freq)*100))), vjust = 1) +
    geom_text(aes(label = scales::percent(Freq,accuracy = 2.2))) +
    scale_fill_gradient(low = "white",
                        high = "purple",
                        trans = "log") + theme_gdocs()
  print(plot)
  return(cf)
}



#Tree Model
plotTree<-function(treeModel){
  #summary(treeModel)
  #fancyRpartPlot(treeModel,type=1)
  plot(treeModel)
  #plot number of splits
  rsq.rpart(treeModel)
  plotcp(treeModel)
  #printcp(ptree)
  #confusion matrix to find correct and incorrect predictions
}


#Tree models

trainIndex <- createDataPartition(DigitTotalDF$label, p = .6, list = FALSE,  times = 1)
trainDF <- DigitTotalDF[ trainIndex,]
testDF  <- DigitTotalDF[-trainIndex,]

predictTreeModel<-function(tree_model){
  summary(tree_model)
  plotTree(tree_model)
  predicted_tree= predict(tree_model, testDF, type="class") 
  plotConfusionMatrix(predicted_tree,testDF$label)
  return(tree_model)
}

tree_model_1<-rpart(label ~., data = trainDF, method="class", control=rpart.control(cp=0))
predictTreeModel(tree_model_1)

 ptree<- prune(tree_model_1, 
               cp=tree_model_1$cptable[which.min(tree_model_1$cptable[,"xerror"]),"CP"])
 predictTreeModel(ptree)


#K-Fold Validation
N <- nrow(DigitTotalDF)
kfolds <- 3
holdout <- split(sample(1:N), 1:kfolds)

AllResults<-list() 
AllLabels<-list() 
for (k in 1:kfolds){
  kFold_Test <- DigitTotalDF[holdout[[k]], ] 
  kFold_Train<-DigitTotalDF[-holdout[[k]], ]

  tree_model<-rpart(label ~., data = kFold_Train, method="class", control=rpart.control(cp=0))
  purned_tree<- prune(tree_model, cp=tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])

  predicted <- predict(purned_tree, kFold_Test,type="class") 
  (plotConfusionMatrix(predicted, kFold_Test$label))
  # Accumulate results from each fold, if you like
  AllResults<- c(AllResults,predicted)
  AllLabels<- c(AllLabels, kFold_Test$label)
  ##Visualize
  plot(predicted, ylab = "Density", main = "Decision Tree Plot") 
}

table<-(table(Predicted=unlist(AllResults),TrueDigit=unlist(AllLabels)))
print(table)
plotConfusionMatrix(unlist(AllResults),unlist(AllLabels))




#NB

getTopPCAFeatures<- function(df,ncp){
  pca_digits = PCA(t(select(df,-label)),ncp = ncp,graph=FALSE)
  summary(pca_digits)
  #plot(pca_digits, select="contrib 1",choix = "var")
  #res <- Factoshiny(pca_digits)
  
  #select the pca variables only
  pca_df<-data.frame(df$label,pca_digits$var$coord)
  names(pca_df)[1]<-"label"
  return(pca_df)
}

runNB<-function(pca_df){
  trainIndex <- createDataPartition(pca_df$label, p = .6, list = FALSE,  
                                    times = 1)
  #head(trainIndex)
  trainDF <- pca_df[ trainIndex,]
  testDF  <- pca_df[-trainIndex,]
  model_nb =  naivebayes::naive_bayes(label ~., data = trainDF)
  summary(model_nb)
  predicted_nb= predict(model_nb, testDF, type="class") 
  plotConfusionMatrix(predicted_nb,testDF$label)
  return(model_nb)
}

model_nb_full<-runNB(DigitTotalDF)

pca_10<-getTopPCAFeatures(DigitTotalDF,10)
model_nb_10<-runNB(pca_10)
pca_50<-getTopPCAFeatures(DigitTotalDF,50)
model_nb_50<-runNB(pca_50)
pca_100<-getTopPCAFeatures(DigitTotalDF,100)
model_nb_100<-runNB(pca_100)
pca_75<-getTopPCAFeatures(DigitTotalDF,75)
model_nb_75<-runNB(pca_75)
pca_80<-getTopPCAFeatures(DigitTotalDF,80)
model_nb_80<-runNB(pca_80)
pca_150<-getTopPCAFeatures(DigitTotalDF,150)
model_nb_150<-runNB(pca_150)

pca_230<-getTopPCAFeatures(DigitTotalDF,230)
model_nb_230<-runNB(pca_230)


# k -fold
train.control <- trainControl(method = "cv", number=3)
# Train the model
nb_model <- train(label ~., data = pca_75, method = "naive_bayes",trControl = train.control)
# Summarize the results
print(nb_model)

summary(nb_model)

cf_nb<-confusionMatrix(nb_model)

str(cf_nb$table)

confusion_matrix_nb <- as.data.frame(cf_nb$table)
names(confusion_matrix_nb)<-c("Predicted","TrueDigits","Freq")
ggplot(data = confusion_matrix_nb,
             aes(x = Predicted,y = TrueDigits)) +
  geom_tile(aes(fill = Freq)) +
  #geom_text(aes(label = sprintf("%.2f", (Freq/sum(Freq)*100))), vjust = 1) +
  geom_text(aes(label = sprintf("%.2f",Freq))) +
  scale_fill_gradient(low = "white",
                      high = "purple",
                      trans = "log") + theme_gdocs()

#kaggle submission
kaggleTest <- read.csv("test.csv", header = TRUE, stringsAsFactors = TRUE)
colnames<- names(kaggleTest)
names(kaggleTest)<- sub("pixel","",colnames)

kaggle_dtree<- predict(ptree,kaggleTest,type="class")
kaggle_dtree<-data.frame(kaggle_dtree)
names(kaggle_dtree)<-c("Label")
kaggle_dtree$ImageId<- row.names(kaggle_dtree)
write.csv(kaggle_dtree,"kaggle_dtree.csv",row.names=FALSE)

kaggle_nb<- predict(model_nb_230,kaggleTest,type="class")
kaggle_nb<-data.frame(kaggle_nb)
names(kaggle_nb)<-c("Label")
kaggle_nb$ImageId<- row.names(kaggle_nb)
write.csv(kaggle_nb,"kaggle_nb.csv",row.names=FALSE)

stopCluster(cl)
