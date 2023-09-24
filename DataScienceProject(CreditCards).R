
#Introduction to project
# We embarked on an intriguing project involving a database brimming with 
#diverse individuals' credit card balances and their corresponding payment 
#histories over the course of a year. Within this treasure trove of data, we 
#were equipped with valuable insights into each individual's age, credit balance
#limit, marital status, education level, as well as multiple bill amounts and payment 
#records. Our ultimate goal was to harness the power of this data to predict whether
#a person would default on their payment in the subsequent month.

#The data we worked with originated from credit card statements in Taiwan, 
#spanning from April 2005 to September 2005. Our predictive model serves as a 
#vital tool for financial institutions, enabling them to anticipate potential 
#spikes in defaults in the upcoming month. With this foresight, banks can make 
#informed decisions and take preemptive measures to mitigate risks, ensuring they 
#are adequately prepared to address any deviations from the norm in payment 
#behavior.

UCI_Credit_Card <- read.csv("~/Downloads/UCI_Credit_Card.csv")
View(UCI_Credit_Card)

set.seed(1)

names(UCI_Credit_Card)[names(UCI_Credit_Card) == "default.payment.next.month"] <- "default"
data = UCI_Credit_Card

model = glm(default~ ., data=data)
summary(model)

step(model)

predict(model)[1:10]
fitted(model)[1:10]
model$fit[1:10]
Pred=(fitted(model)>.5)*1
Pred
ncol(data)
nrow(data)

sum(data$default!=Pred)/30000

# 0.2006667

####################################################################################

# BIC for Backwards selection

step(model)

model = glm(formula = default ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + 
              AGE + PAY_0 + PAY_2 + PAY_3 + PAY_5 + BILL_AMT1 + BILL_AMT2 + 
              PAY_AMT1 + PAY_AMT2 + PAY_AMT4 + PAY_AMT5, data = data)

Groups=sample(1:30000,replace=FALSE) ### randomly assigns observations to folds
Groups=matrix(Groups,ncol=2)
Groups

missclass_folds=c()
for(i in 1:2){
  model=glm(default ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + 
              AGE + PAY_0 + PAY_2 + PAY_3 + PAY_5 + BILL_AMT1 + BILL_AMT2 + 
              PAY_AMT1 + PAY_AMT2 + PAY_AMT4 + PAY_AMT5, family=binomial(), data=data[-Groups[,i],])
  prob=predict(model,newdata=data[Groups[,i],],type="response")
  predY=rep(0,15000)
  predY[prob>.5]=1 
  missclass=sum(data$default[Groups[,i]]!=predY)/15000
  missclass_folds=c(missclass_folds,missclass)
}
Avg_missclass=mean(missclass_folds)
Avg_missclass
#    [1] 0.1900667  this is the 2 fold cross validation 

####################################################################################
# Foward selection AIC
?step
step(glm(default~1,data=data),direction="forward",scope=list(lower=lm(default~1,data=data),upper=lm(default~.,data=data)))


model = glm(formula = default ~ PAY_0 + BILL_AMT1 + PAY_2 + AGE + PAY_3 + 
      PAY_AMT1 + MARRIAGE + LIMIT_BAL + EDUCATION + SEX + PAY_5 + 
      PAY_AMT2 + BILL_AMT2 + PAY_AMT5 + PAY_AMT4, data = data)


Groups=sample(1:30000,replace=FALSE) ### randomly assigns observations to folds
Groups=matrix(Groups,ncol=2)
Groups

missclass_folds=c()
for(i in 1:2){
  model=glm(default ~ PAY_0 + BILL_AMT1 + PAY_2 + AGE + PAY_3 + 
              PAY_AMT1 + MARRIAGE + LIMIT_BAL + EDUCATION + SEX + PAY_5 + 
              PAY_AMT2 + BILL_AMT2 + PAY_AMT5 + PAY_AMT4, family=binomial(), data=data[-Groups[,i],])
  prob=predict(model,newdata=data[Groups[,i],],type="response")
  predY=rep(0,15000)
  predY[prob>.5]=1 
  missclass=sum(data$default[Groups[,i]]!=predY)/15000
  missclass_folds=c(missclass_folds,missclass)
}
Avg_missclass=mean(missclass_folds)
Avg_missclass
#    [1] 0.1898   this is the 2 fold cross validation 
####################################################################################

####################################################################################
# BIC for backwards selection

# Load the necessary package
library(stats)
model = glm(default~ ., data=data)
# Fit a linear model


# Perform stepwise model selection using BIC
BIC_model <- step(model, k = log(30000))
nrow(data)

# Print the selected model
summary(BIC_model)

model = glm(formula = default ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + 
      PAY_0 + PAY_2 + PAY_3 + BILL_AMT1 + PAY_AMT1, data = data)


Groups=sample(1:30000,replace=FALSE) ### randomly assigns observations to folds
Groups=matrix(Groups,ncol=2)
Groups

missclass_folds=c()
for(i in 1:2){
  model=glm(formula = default ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE + 
              PAY_0 + PAY_2 + PAY_3 + BILL_AMT1 + PAY_AMT1, family=binomial(), data=data[-Groups[,i],])
  prob=predict(model,newdata=data[Groups[,i],],type="response")
  predY=rep(0,15000)
  predY[prob>.5]=1 
  missclass=sum(data$default[Groups[,i]]!=predY)/15000
  missclass_folds=c(missclass_folds,missclass)
}
Avg_missclass=mean(missclass_folds)
Avg_missclass
#    [1] [1] 0.1902  this is the 2 fold cross validation for BIC and Backwards selection

####################################################################################
?cv.glm
?step

model = glm(default~ ., data=data)

step(glm(default~1,data=data),direction="forward",scope=list(lower=lm(default~1,data=data),upper=lm(default~.,data=data)), k = log(30000))



step(model,direction = "forward", k = log(30000))

#Call:  glm(formula = default ~ PAY_0 + BILL_AMT1 + PAY_2 + AGE + PAY_3 + 
# PAY_AMT1 + LIMIT_BAL, data = data)

nrow(data)
Groups=sample(1:30000,replace=FALSE) ### randomly assigns observations to folds
Groups=matrix(Groups,ncol=2)
Groups

missclass_folds=c()
for(i in 1:2){
  model=glm(formula = default ~ PAY_0 + BILL_AMT1 + PAY_2 + AGE + PAY_3 + 
         PAY_AMT1 + LIMIT_BAL, family=binomial(), data=data[-Groups[,i],])
  prob=predict(model,newdata=data[Groups[,i],],type="response")
  predY=rep(0,15000)
  predY[prob>.5]=1 
  missclass=sum(data$default[Groups[,i]]!=predY)/15000
  missclass_folds=c(missclass_folds,missclass)
}
Avg_missclass=mean(missclass_folds)
Avg_missclass
# [1] 0.1914




####################################################################################

# PCA  ####################################################################################

data = UCI_Credit_Card[,c(2,6,7:25 )] 
qualitative = UCI_Credit_Card[,c(1,3:5 )] 
y = data[,21]
y
data
ncol(data)
dim(data)
pca=prcomp(data[,-21],scale=TRUE)
pca
summary(pca)
pca$x 
cor(pca$x)
pca$rotation

qualitative
screeplot(pca,type="lines")

PCdata=data.frame(pca$x[,1:3],y, qualitative)
PCdata

Groups=sample(1:30000,replace=FALSE) 
Groups=matrix(Groups,ncol=2)
Groups


errors=c()
for(i in 1:2){
  mod=lm(y~.,data=PCdata[-i,c(1:2,4)])  
  predicted=predict(mod,newdata=PCdata[i,c(1:2,4)])
  missclass=sum(data$default[Groups[,i]]!=predY)/15000
  missclass_folds=c(missclass_folds,missclass)
  
}
Avg_missclass=mean(missclass_folds)
Avg_missclass
# [1] 0.2406111

####################################################################################################################

#decision trees
data= UCI_Credit_Card
data$default=as.factor(data$default)



library(rpart)
library(rpart.plot)

tree=rpart(default~., data=data)
tree
plot(tree,margin=.1)
text(tree,use.n=TRUE, all=TRUE, cex=.6) 

tree=rpart(default~., data=data,control=rpart.control(cp=.001))
tree
plot(tree,margin=.1)
text(tree,use.n=TRUE, all=TRUE, cex=.6) 
## The graph is difficult to read. rpart.plot() is better for this:
rpart.plot(tree,digits=-3)

tree=rpart(default~., data=data,control=rpart.control(cp=.001)) 
rpart.plot(tree,digits=-3)
plot(tree,margin=.1)
text(tree,use.n=TRUE, all=TRUE, cex=.6) 



### Look at the row where xerror is minimized and set the control
tree=rpart(default~., data=data,control=rpart.control(cp=.0001))
tree=rpart(default~., data=data,control=rpart.control(cp=.001))

printcp(tree,digits=5) ## we observe that it's ideal to set cp= 0.00085 (since xerror is minimized right before this)
plotcp(tree)

tree=rpart(default~., data=data,control=rpart.control(cp=0.00085))
rpart.plot(tree,digits=-6)
predictions=predict(tree,type="class")
predictions[1:10]
predict(tree,type="prob")[1:10,]

sum(data$default!=predictions)/30000
#### Missclass rate:    [1] 0.1712333





############################################################################################
#optimal control parameter based on XERROR

g1=1:15000
g2=15001:30000

Groups=data.frame(g1,g2)
Groups

predictions=c()
for(i in 1:2){
  tree=rpart(default~., data=data[-Groups[,i],],control=rpart.control(cp=0.00085)) 
  predictions_per_fold=predict(tree,type="class",newdata=data[Groups[,i],]) ## 
  predictions=c(predictions,as.character(predictions_per_fold))
}
sum(data$default!=predictions)/length(predictions)

##[1] 0.1843333





############################################################################################
g1=1:15000
g2=15001:30000

Groups=data.frame(g1,g2)
Groups

predictions=c()
for(i in 1:2){
  tree=rpart(default~., data=data[-Groups[,i],],control=rpart.control(cp=.0001)) 
  predictions_per_fold=predict(tree,type="class",newdata=data[Groups[,i],]) ## 
  predictions=c(predictions,as.character(predictions_per_fold)) ## Note the need to change "predictions_per_fold" into character from factors. Otherwise, when we use c() function, we end up converting "LEAVE/STAY" to 0/1
}
sum(data$default!=predictions)/length(predictions)

##[1] 0.2317667

############################################################################################

.001

g1=1:15000
g2=15001:30000

Groups=data.frame(g1,g2)
Groups

predictions=c()
for(i in 1:2){
  tree=rpart(default~., data=data[-Groups[,i],],control=rpart.control(cp=.001)) 
  predictions_per_fold=predict(tree,type="class",newdata=data[Groups[,i],]) ## 
  predictions=c(predictions,as.character(predictions_per_fold)) ## Note the need to change "predictions_per_fold" into character from factors. Otherwise, when we use c() function, we end up converting "LEAVE/STAY" to 0/1
}
sum(data$default!=predictions)/length(predictions)

##[1] [1] 0.1835333
# SOMEHOW THIS IS A BIT BETTER THAN THE MODEL BASED ON THE BEST XERROR. BUT WHEN WE USED THE 
# RANDOM SAMPLING FOR THE CROSS  VALIDATION, IT CAME OUT WORSE THAN THE OPTIMAL VERSION. 


######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
### Support Vector Machines ###
library(e1071)


data= UCI_Credit_Card
data$default=as.factor(data$default)


x=sample(1:30000,15000) ## randomly choose players for the Train set.
g1=data[x,]
g2=data[-x,]
Groups=data.frame(g1,g2)
Groups

predictions=c()
?svm
for(i in 1:2){
  model=svm(default~.,data=data[-Groups[,i],],kernel="linear",scale=TRUE,type="C-classification",cost=1)
  predictions=predict(model,type="class",newdata=data[Groups[,i],])
  predictions=c(predictions,as.character(predictions_per_fold)) ## Note the need to change "predictions_per_fold" into character from factors. Otherwise, when we use c() function, we end up converting "LEAVE/STAY" to 0/1
}
sum(data$default!=predictions)/length(predictions)
# [1] 0.4960667


predictions=c()

for(i in 1:2){
  model=svm(default~.,data=data[-Groups[,i],],kernel="poly",scale=TRUE,type="C-classification",cost=20)
  predictions=predict(model,type="class",newdata=data[Groups[,i],])
  predictions=c(predictions,as.character(predictions_per_fold)) ## Note the need to change "predictions_per_fold" into character from factors. Otherwise, when we use c() function, we end up converting "LEAVE/STAY" to 0/1
}
sum(data$default!=predictions)/length(predictions)

# Missclass:  [1] 0.5183705


predictions=c()

for(i in 1:2){
  model=svm(default~.,data=data[-Groups[,i],],kernel="radial",scale=TRUE,type="C-classification",cost=30)
  predictions=predict(model,type="class",newdata=data[Groups[,i],])
  predictions=c(predictions,as.character(predictions_per_fold)) ## Note the need to change "predictions_per_fold" into character from factors. Otherwise, when we use c() function, we end up converting "LEAVE/STAY" to 0/1
}
sum(data$default!=predictions)/length(predictions)

# Missclass:  [1] 0.5234997



######################################################################################################################################

library(class)
library(class)
data = UCI_Credit_Card
data$default=as.factor(data$default)



x=sample(1:30000,15000) 
g1=data[x,]
g2=data[-x,]
Groups=data.frame(g1,g2)
Groups


Groups=sample(1:1000,replace=FALSE) ### randomly assigns observations to folds
Groups=matrix(Groups,ncol=2)

for(k in 1:2){
  data[,k]=(data[,k]-mean(data[,k]))/sd(data[,k])
}

misclass = c()
misclass_fold = c()
misclass_folds = c()

for(n in 1:50){
  misclass = c()
  misclass_fold = c()
  for(i in 1:2){ 
    Pred=knn(train=data[-Groups[,i],-25],test=data[Groups[,i],-25],cl=data[-Groups[,i],25],k=n) 
    misclass=c(misclass,1-(sum(data$default[Groups[,i]]==Pred))/30000)
  }
  misclass_folds = c(misclass_folds, misclass_fold)
}
misclass_folds


plot(1:50, misclass_folds,pch=16)
names(misclass_folds) = c(1:50)
misclass_folds[misclass_folds == min(misclass_folds)]

## Optimal k=22






knn(train=data[-Groups[,i],-25],test=data[Groups[,i],-25],cl=data[-Groups[,i],25],k=22)
misclass=c(misclass,1-(sum(data$default[Groups[,i]]==Pred))/30000)
missclass

#   [1] 0.1904667




data = UCI_Credit_Card[,c(2,6,7:25 )]   ##Lets take all the numeric columns of UCI_Credit_Card

Pred=knn(train=data[,-8],test=data[,-8],cl=data[,8],k=10) 




######################################################################################################################################
# Random Forest
# Obtain 1 Random Forest Model by creating 1000 trees where we randomly select 50% of the
# variables in each iteration. Compute the misclassification rate according to this Random Forest
# Model on the entire dataset (no need to do cross-validation here)
######################################################################################################################################

library(randomForest)
##### Note that your response variable has to be a factor (or character). If you pass it a column of 0s and 1s that are treated as numbers, it will NOT create a classification model.

data = UCI_Credit_Card
data$default=as.factor(data$default)


RF=randomForest(default~.,data=data) ## lets use the first 9 columns as the predictor variables (for no particular reason other than to save time).
## The argument mtry states how many variables to use at each node. The default is sqrt(# of columns).
## The argument ntree states how many trees to create. The default is 500.

?randomForest

RF
sum(data$default!=predictions)/30000


numpredictors=ncol(data)  

halfpredictors=floor(numpredictors / 2)

RF=randomForest(default~.,data = data, ntree=1000 , mtry = halfpredictors)
predict(RF)[1:10]
predictions=predict(RF)
sum(data$default!=predictions)/30000

##### misclassification rate according to this Random Forest
# [1] 0.1816


######################################################################################################################################
# Clustering
# Try 3 different hierarchical models based on varying the distance measure used between
# observations and distance measure used for the clusters. Compute the misclassification rate for
# each model on the entire dataset (no need to do cross-validation here)
######################################################################################################################################
?dist

data = UCI_Credit_Card
ncol(data)
data = data[,-25]
for(k in 1:ncol(data)){
  data[,k]=(data[,k]-mean(data[,k]))/sd(data[,k])
}
data


Eucl.Distances= dist(data) ## we can choose different distances here like "Euclidean" (default), or "Manhattan", etc.
Clusters=hclust(Eucl.Distances,method="complete")
Clusters
Clusters$merge[1:10,]
Groups=cutree(Clusters,k=2)

?cutree


sum(UCI_Credit_Card$default[1:30000][Groups==1]==1)  # [1] 6636
sum(UCI_Credit_Card$default[1:30000][Groups==1]==0) # [1] 23360
## Therefore Group 1 should be predicted as "STAY"
Pred=rep(NA,30000)

Pred[Groups==1]=0

sum(UCI_Credit_Card$default[1:30000][Groups==2]==1) #0
sum(UCI_Credit_Card$default[1:30000][Groups==2]==0) #4

## Therefore Group 2 should be predicted as "LEAVE"
Pred[Groups==2]= 0

missclass=sum(UCI_Credit_Card$default[1:30000]!=Pred)/30000
missclass
# [1] 0.2212



##

data = UCI_Credit_Card
ncol(data)
data = data[,-25]
for(k in 1:ncol(data)){
  data[,k]=(data[,k]-mean(data[,k]))/sd(data[,k])
}
data
?dist

?hclust

Manh.Distances= dist(data,method="manhattan") ## we can choose different distances here like "Euclidean" (default), or "Manhattan", etc.
Clusters=hclust(Eucl.Distances,method="complete")
Clusters
Groups=cutree(Clusters,k=2)

?cutree


sum(UCI_Credit_Card$default[1:30000][Groups==1]==1)  # [1] 6636
sum(UCI_Credit_Card$default[1:30000][Groups==1]==0) # [1] 23360
## Therefore Group 1 should be predicted as "STAY"
Pred=rep(NA,30000)
Pred[Groups==1]=0

sum(UCI_Credit_Card$default[1:30000][Groups==2]==1) #0
sum(UCI_Credit_Card$default[1:30000][Groups==2]==0) #4
## Therefore Group 2 should be predicted as "LEAVE"
Pred[Groups==2]= 0

missclass=sum(UCI_Credit_Card$default[1:30000]!=Pred)/30000
missclass
# [1] 0.2212






?dist


?hclust
Manh.Distances= dist(data,method="manhattan")
Clusters=hclust(Eucl.Distances,method="single")
Clusters
Groups=cutree(Clusters,k=2)

?cutree


sum(UCI_Credit_Card$default[1:30000][Groups==1]==1)  # [1] 6636
sum(UCI_Credit_Card$default[1:30000][Groups==1]==0) # [1] 23360
## Therefore Group 1 should be predicted as "STAY"
Pred=rep(NA,30000)
Pred[Groups==1]=0

sum(UCI_Credit_Card$default[1:30000][Groups==2]==1) #0
sum(UCI_Credit_Card$default[1:30000][Groups==2]==0) #4
## Therefore Group 2 should be predicted as "LEAVE"
Pred[Groups==2]= 0

missclass=sum(UCI_Credit_Card$default[1:30000]!=Pred)/30000
missclass
# [1] 0.2212

#Conclusion
# After an extensive exploration of various analytical methods and an in-depth 
#assessment of their respective outcomes, our analysis led us to a resounding 
#conclusion: Random Forest emerged as the most effective approach. This verdict 
#was driven by the remarkable achievement of a misclassification rate of just 
#18.16%. Notably, this exceptional performance was realized when we crafted a 
#forest of 1000 trees, employing a strategy that involved the random selection of
#50% of the variables in each iteration.

#Given this compelling result, it is evident that Random Forest analysis reigns 
#supreme in our quest for precision and accuracy. With this approach as our 
#cornerstone, we can confidently harness its capabilities to yield optimal 
#results in our modeling endeavors.
