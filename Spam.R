# install the following packages first.
library(ggplot2)
library(ISLR)
library(MASS)
library(klaR)  
library(knitr)
library(glmnet)
library(plyr)

set.seed(210)

# Spam data
spamdf <- read.csv("spamdf.csv")
varinfo <- read.csv("varinfo.csv",  stringsAsFactors = FALSE)
is.test <- read.csv("istest.csv", header=FALSE)
is.test <- as.integer(is.test[,1])

# log-transform the x variable
spamdf.log <- spamdf
spamdf.log[, 1:(ncol(spamdf) - 1)] <- log(0.1 + spamdf[, 1:(ncol(spamdf) - 1)])
# Add names
colnames(spamdf.log) <- c(varinfo[,1], "is.spam")

### Create training and testing sets
train.index <- sample(1:nrow(spamdf.log),3065)
spam.train <- spamdf.log[train.index,]
spam.test <- spamdf.log[-train.index,]
glm.fit = glm(is.test ~ ., data=spamdf.log, family=binomial)
glm.probs = predict(glm.fit, spam.test, type="response")
# Run the split by doing the default probs
glm.pred = rep(0,dim(spam.test)[1])
glm.pred[glm.probs>.5]=1
mean(glm.pred!=spam.test) 

