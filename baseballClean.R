# mlb hall of fame classification analysis
# chua@wharton.upenn.edu

rm(list=ls())
par(mfrow=c(1,1)) 

if (!require("pacman")) install.packages("pacman")
pacman::p_load("Lahman", "dplyr", "ggplot2")

setwd("~/471") # wharton

# get career batting totals
batting <- summarize(group_by(Batting, playerID), numYears=length(unique(yearID)),
                     lastYear=max(yearID), g=sum(G, na.rm=TRUE), 
                     ab=sum(AB, na.rm=TRUE), r=sum(R, na.rm=TRUE), 
                     h=sum(H, na.rm=TRUE), x2b=sum(X2B, na.rm=TRUE), 
                     x3b=sum(X3B, na.rm=TRUE), hr=sum(HR, na.rm=TRUE), 
                     rbi=sum(RBI, na.rm=TRUE), sb=sum(SB, na.rm=TRUE), 
                     cs=sum(CS, na.rm=TRUE), bb=sum(BB, na.rm=TRUE), 
                     so=sum(SO, na.rm=TRUE), ibb=sum(IBB, na.rm=TRUE),
                     hbp=sum(HBP, na.rm=TRUE), sh=sum(SH, na.rm=TRUE),
                     sf=sum(SF, na.rm=TRUE), gidp=sum(GIDP, na.rm=TRUE))

# get career all-star game totals
asg <- summarize(group_by(AllstarFull, playerID), asgApp=sum(GP))

# get career awards: MVP, Gold Glove (fielding), Silver Slugger (batting),
#                    Triple Crown (BA, HR, RBI)
# we could get more, but these should be good
awards <- summarize(group_by(AwardsPlayers, playerID), 
                    mvp = sum(awardID == "Most Valuable Player"),
                    gg = sum(awardID == "Gold Glove"),
                    sisl = sum(awardID == "Silver Slugger"),
                    tc = sum(awardID == "Triple Crown"))

# bring in Wins Above Replacement metric (WAR) - FanGraphs data
# commented out for now: not enough historical "adjustment" data

warD <- read.csv("war_daily_bat.txt")
colnames(warD)[3] <- "playerID"
warD$WAR <- as.numeric(paste(warD$WAR))
warD$WAR_off <- as.numeric(paste(warD$WAR_off))
warD$Inn <- as.numeric(paste(warD$Inn))

warD2 <- summarize(group_by(warD, playerID), totalWAR=sum(WAR), 
                   totalInn = sum(Inn), playerName = first(name_common))
# warD2 <- summarize(group_by(warD, playerID), totalWAR=sum(WAR), 
#                    totalInn = sum(Inn), pitcher = first(pitcher))
warD<-warD2
rm(warD2)

# fix a typo in Lahman's db
data(HallOfFame)
HallOfFame$playerID[HallOfFame$playerID == 'glavito01'] <- "glavito02"

# find individuals elected to HOF
hof2 <- filter(HallOfFame, votedBy %in% c("BBWAA", "Special Election", "Veterans") &
                 category == "Player")
hof2 <- filter(HallOfFame, category == "Player")
# set induction to numeric; 0 = not inducted, 1 = inducted
hof2$inducted <- as.numeric(hof2$inducted) - 1

# sum(HallOfFame$inducted =="Y") # number of inducted members
# which.max(inducted) # number of years on ballot, if inducted

# find lifetime stats - if they were inducted in the end
hof <- summarize(group_by(hof2, playerID), inducted = max(inducted))

# merge columns into one data.frame
data = merge(batting, warD, by="playerID", all=TRUE)
data = merge(data, awards, by="playerID", all=TRUE)
data = merge(data, asg, by="playerID", all=TRUE)
data = merge(data, hof, by="playerID", all=TRUE)

data[is.na(data)] <- 0

# add PA, OBP, OPS, SLG, RC, BA
data$pa = (data$ab + data$bb+ data$hbp + data$sf + data$sh)
data$obp = ((data$h+data$bb+data$hbp)/(data$ab+data$bb+data$hbp+data$sf))
data$slg = ((2 * data$x2b) + (3 * data$x3b) + (4 * data$hr))/data$ab
data$ops = data$obp + data$slg
data$rc = ((data$h + data$bb) * (data$h + data$x2b + 2*data$x3b + 3*data$hr) / (data$ab + data$bb))
data$bAvg = data$h / data$ab
data$aby = data$ab / data$numYears

# qualitative dummy variables
data$threeK = as.numeric(data$h >= 3000) 
data$fiveHundo = as.numeric(data$hr >= 500)
data$threeHundo = as.numeric(data$bAvg >= 0.3)
data$tenYrs = as.numeric(data$numYears >= 10) 

# reorder columns so playerName is first
n <- which(names(data)=="playerName")
data <- data[, c(n, (1:(n-1)), (n+1):ncol(data))]

# NaN's introduced above - call these 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

data[is.nan(data)] <- 0
data[is.na(data)] <- 0

# cleanup 
rm(hof2, hof, asg, awards, batting, warD, HallOfFame, n)

# begin analysis
## classifying batters as having 120+ ab/year - see appendix for justification
data = filter(data, aby > 120, lastYear >= 1950, lastYear <=2000)
data.clean = data[, -c(1:2)]

set.seed(1) # fix seed
n=nrow(data.clean)
train.index=sample(n,n*3/4)
train=data.clean[train.index, ]
test=data.clean[-train.index, ]

##### LOGISTIC REGRESSION MODEL


### We start with the training data and make our initial model. 
fit1=glm(inducted ~ numYears + g + ab + r + h + x2b + x3b + hr +rbi + sb + cs + bb + so + ibb + hbp + sh + sf + gidp + totalWAR + totalInn + mvp + gg + sisl + tc + asgApp + pa + obp + slg + ops + rc + bAvg + aby, data=train, family=binomial(logit))
### Backwards Selection

fit2=glm(inducted ~ numYears + g + ab + r + h + x2b + x3b + hr +rbi + sb + cs + bb + so + ibb + hbp + sh + sf + gidp + totalWAR + totalInn + mvp + gg + sisl + tc + asgApp + obp + slg + rc + bAvg + aby, data=train, family=binomial(logit))
fit3=glm(inducted ~ hr + h + rbi + sb + totalWAR + mvp + gg + sisl + tc + asgApp + obp + slg + rc + bAvg, data=train, family=binomial(logit))
fit3=glm(inducted ~ hr + h + rbi + sb + totalWAR + mvp + gg + sisl + asgApp + obp + slg + rc + bAvg, data=train, family=binomial(logit))
fit4=glm(inducted ~ hr + h + rbi + sb + totalWAR + gg + sisl + asgApp + obp + slg + rc + bAvg, data=train, family=binomial(logit))
fit5=glm(inducted ~ hr + h + rbi + sb + totalWAR + gg + sisl + asgApp + obp + rc + bAvg, data=train, family=binomial(logit))
fit5=glm(inducted ~ hr + h + rbi + sb + totalWAR + gg + asgApp + obp + rc + bAvg, data=train, family=binomial(logit))
fit6=glm(inducted ~ hr + rbi + sb + totalWAR + gg + asgApp + obp + rc + bAvg, data=train, family=binomial(logit))
fit6=glm(inducted ~  rbi + sb + totalWAR + gg + asgApp + obp + rc , data=train, family=binomial(logit))
fit6=glm(inducted ~  rbi + sb + totalWAR + gg + asgApp + obp , data=train, family=binomial(logit))
fit6=glm(inducted ~  sb + totalWAR + gg + asgApp + obp , data=train, family=binomial(logit))
fit6=glm(inducted ~  sb + totalWAR + gg + asgApp , data=train, family=binomial(logit))

### Final GLM Model
fit7=glm(inducted ~  totalWAR + gg + asgApp, data=train, family=binomial(logit))
### Predicted
glm.probs=predict(fit7,test, type="response")
inducted1=as.factor(test$inducted)
contrasts(inducted1)
summary(inducted1)

glm.pred=rep("0",222)
hist(glm.probs)
glm.pred[glm.probs>.5]="1"

### CM
table1=table(glm.pred, inducted1)
mean(glm.pred==inducted1)


list(glm.probs>.5)

### The misclassification rate in this model is .023. Thus, this seems to be a very accurate model for predicting whether a hitter will be inducted into the Hall of Fame, with a correct prediction rate of .977. 

```{r}
library(boot)

log.fit=glm(inducted~., data.clean, family=binomial)
set.seed(1)

result=cv.glm(data.clean, log.fit, K=10)

result$delta

futures=filter(data, aby>120, lastYear > 2000, numYears>=10)
testf=predict(fit7, futures)

predTablef <- factor(ifelse(testf >= 0.5, "Y", "N"))
table(futures$inducted, predTablef)
futures[which(futures$inducted == 1), ]
futures[which(predTablef == 'Y'), ]

##### KNN (Exploration. Haven't included in report because subset of RandomForest)

library("klaR")
library("MASS")
library("class")
pacman::p_load("boot")

train.std <- scale(train)
test.std <- scale(test)

pca_train <- prcomp(train, scale.=TRUE)
data_train <- data.frame(y=train[, "inducted"], pca_train$x)
pca_test <- prcomp(test, scale.=TRUE)
data_test <- data.frame(y=test[, "inducted"], pca_test$x)
PC1 <- data_train$PC1
PC2 <- data_train$PC2
PC3 <- data_train$PC3
PC4 <- data_train$PC4
PC5 <- data_train$PC5
PC6 <- data_train$PC6
PC1.1 <- data_test$PC1
PC2.1 <- data_test$PC2
PC3.1 <- data_test$PC3
PC4.1 <- data_test$PC4
PC5.1 <- data_test$PC5
PC6.1 <- data_test$PC6

training <- cbind(PC1, PC2, PC3, PC4, PC5,PC6)
testing <- cbind(PC1.1,PC2.1,PC3.1,PC4.1,PC5.1,PC6.1)


knn.pred <- knn(train=training ,test=testing, cl=data_train$y,k=19)
m.confusion <- table(knn.pred, data_test$y)
sensitivity <- m.confusion[2,2]/sum(m.confusion[2,])
falsepositive <- m.confusion[1,2]/sum(m.confusion[1,])
training.error <- mean(data_test != knn.pred)

##### CROSS-VALIDATED GLM ON PRINCIPAL COMPONENTS
pacman::p_load("boot")
pca1 <- prcomp(data.clean, scale.=TRUE)
#summary(pca.train)
#screeplot(pca.train, type="lines")
#sum(pca.train$sdev^2 > 1) # check Kaiser's criterion

data1 <- data.frame(y=data[, "inducted"], pca1$x)
fit <- glm(y ~ PC1 + PC2 + PC3 + PC4 + PC5, data=data1, family="binomial")
# summary(fit)
set.seed(1)
result=cv.glm(data1, fit, K=10)
# result$delta

##### GLM ON PRINCIPAL COMPONENTS WITH 25% HOLDOUT
pca2 <- prcomp(train, scale.=TRUE)
data2 <- data.frame(y=train[, "inducted"], pca2$x)

fit2 <- glm(y ~ PC1 + PC2 + PC3 + PC4 + PC5, data=data2, family="binomial")
test.p <- predict(pca2, test)
pred <- predict(fit2, newdata = data.frame(test.p), type = "response")
predTable <- factor(ifelse(pred>=0.2,"Y","N"))
table(test$inducted,predTable)
fit2.error = sum((test[, "inducted"]-pred)^2)
sum(test$inducted!=(as.numeric(predTable)-1)) # num wrong

##### TREE WITH 25% HOLDOUT
pacman::p_load("tree")

fit3 <- tree(inducted ~ ., train)
#plot(fit3)
#text(fit3)
fit3.pred <- predict(fit3,test)
fit3.error <- sum((fit3.pred-test[,"inducted"])^2) 
fit3.error
## cross-validation
fit4 <- cv.tree(fit3, K=10)
# par(mfrow=c(3,1))
# plot(fit4$size, fit4$dev)
# plot(fit4$k,fit4$dev )
# plot(fit4$size, fit4$k)
## prune to 4
fit5=prune.tree(fit3, best=4)
par(mfrow=c(1,1))
fit5.pred=predict(fit5, test)
fit5.error=sum((test[, "inducted"]-fit5.pred)^2)
fit5.error

##### RANDOM FOREST WITH 25% HOLDOUT
pacman::p_load("randomForest")
fit6 = randomForest(as.factor(inducted) ~ ., train, mtry=10, importance=T)
# Confusion matrix:
#   0  1 class.error
# 0 1196  3 0.002502085
# 1   12 28 0.300000000
fit6.pred=as.numeric(predict(fit6, test))-1
test6.error = sum((test[, "inducted"]-fit6.pred)^2)
test6.error

##### RANDOM FOREST OF PCA'S
pacman::p_load("class")
train.s = scale(train)
test.s = scale(test)
fit7=knn(train=train.s,test=test.s,cl=train$inducted,k=2)
table(test$inducted,fit7)
fit7.error=mean(test$inducted != fit7)
