## Brain Cancer Data

str(BrainCancer)
library(Epi)
###
library(ISLR2)
library(tree)
###
names(BrainCancer)
###
attach(BrainCancer)
Surv(time, status)
table(sex)
table(diagnosis)
table(status)
###
par(mfrow = c(2, 2))
#목적 -> 여러 뇌질환 요소들로 인한 사망 여부 여측

#tree 선형이든 비선형이든 상관 x
set.seed(13)
a <- model.matrix(status ~ ., BrainCancer)
x <- data.frame(a[ ,2:9])
y <- factor(status[-13])
dat <- data.frame(x,y)
train <- sample(nrow(x),nrow(x)/2)
tree.BC <- tree(y ~ ., data = dat, subset = train)
title("Decision Tree")
text(tree.BC, pretty = 0)
tree.pred <- predict(tree.BC, x[-train, ], type = "class")
table(tree.pred, y[-train])
(17+11)/(17+7+9+11) #Accuracy


ROC(test=y[-train], stat = tree.pred, plot='ROC', AUC=T, main='Decision Tree')

y[-train]
tree.pred

#SVM -> 분류 선이 선형인지 비선형인지 해봐야앎
set.seed(3)
a <- model.matrix(status ~ ., BrainCancer)
x <- data.frame(a[ ,2:9])
y <- status[-13]
train <- sample(nrow(x),nrow(x)/2)
dat <- data.frame(x, y = as.factor(y))
library(e1071)
tune.out <- tune(svm, y ~ ., data = dat[train, ], 
                 kernel = "linear", 
                 ranges = list(
                   cost = c(0.1, 1, 10, 100, 1000),
                   gamma = c(0.5, 1, 2, 3, 4)
                 )
)
bestmod <- tune.out$best.model
summary(bestmod)
###
testdat <- dat[-train, ]
###
ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)
(20+10)/(20+7+7+10) #Accuracy

ROC(test=y[-train], stat = ypred, plot='ROC', AUC=T, main='SVM')


set.seed(13)
a <- model.matrix(status ~ ., BrainCancer)
x <- data.frame(a[ ,2:9])
y <- factor(status[-13])
dat <- data.frame(x,y)
train <- sample(nrow(x),nrow(x)/2)
dat <- data.frame(x, y = as.factor(y))
logfit <- glm(y ~ ., dat, family = "binomial")
summary(logfit)
testdat <- dat[-train, ]
phat <- predict(logfit, testdat,type = "response" )
ypred = as.numeric(phat > 0.5)
table(predict = ypred, truth = testdat$y)
(20+12)/(20+6+6+12) #Accuracy

ROC(test=y[-train], stat = ypred, plot='ROC', AUC=T, main='Logistic Regression')


library(plotrix)
library(dplyr)

#표 셀 생성
C11=paste0((17+11)/(17+7+9+11))
C21=paste0((20+10)/(20+7+7+10))
C31=paste0((20+12)/(20+6+6+12))


#표 생성
my_table=rbind(data.frame(A=C11),
               data.frame(A=C21),
               data.frame(A=C21))

colnames(my_table)=c("Accuracy")
rownames(my_table)=c("Decision Tree","SVM", "Logistic Regression")

#빈 plot 생성
plot.new()
plot.window(xlim=c(0,10),ylim=c(0,10))

#표출력
addtable2plot(0 ,5,my_table,bty="o",display.rownames=TRUE,hlines=TRUE,
              vlines=TRUE,title="The table",cex=2)
