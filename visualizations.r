# mlb hall of fame classification analysis
# chua@wharton.upenn.edu

# attempt to follow https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml

rm(list=ls())
par(mfrow=c(1,1)) 

if (!require("pacman")) install.packages("pacman")
pacman::p_load("Lahman", "dplyr", "ggplot2")

#setwd("~/471") # wharton

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

warD <- read.csv("http://www.baseball-reference.com/data/war_daily_bat.txt")
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
batters = filter(data, aby > 120)

# modern: retiring between 1950 and 2000
## we use ASG metric, which only began in 1933
modern = filter(batters, lastYear >= 1950, lastYear <= 2000)

# old: retiring before 1950
# old = filter(batters, lastYear < 1950)

# pca
modern.clean <- modern[,c(-(1:2))]
batters.clean <- batters[,c(-(1:2))]

pca.one <- prcomp(modern.clean, scale.=TRUE)
pca.two <- prcomp(batters.clean, scale.=TRUE)

pacman::p_load("reshape2")
corB <- cor(batters.clean, use="complete.obs")
corM <- cor(modern.clean, use="complete.obs")
qplot(x=Var1, y=Var2, data=melt(corM), fill=value, geom="tile")

scores <- as.data.frame(pca.one$x)
scores2 <- as.data.frame(pca.two$x)

batters.clean$fInducted <- as.factor(batters.clean$inducted)
batters.subdata <- subset(batters.clean, inducted %in% c("0"))
modern.clean$fInducted <- as.factor(modern.clean$inducted)
modern.subdata <- subset(modern.clean, inducted %in% c("0"))

# #Create a custom color scale
pacman::p_load("RColorBrewer")
myColors <- brewer.pal(2,"Set1")
names(myColors) <- levels(modern.clean$induction)
colScale <- scale_colour_manual(name = "induction", values = myColors)

p1 <- ggplot(data = scores, aes(x = PC1, y = PC2, label = modern$playerName)) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(aes(colour = modern.clean$fInducted), size = 4) +
  ggtitle("PCA biplot of PC1 and PC2") +
  xlab("PC1") + ylab("PC2")
#p1

p2 <- ggplot(data = scores2, aes(x = PC1, y = PC2, label = batters$playerName)) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(aes(colour = batters.clean$fInducted), size = 4) +
  ggtitle("PCA biplot of PC1 and PC2") +
  xlab("PC1") + ylab("PC2")
p2

# add a dimension for shits and giggles
# pacman::p_load("rgl")
#plot3d(scores2[,1:3], main="3 dimensional PCA")
#with(scores2,text3d(scores2[,1],scores2[,2],scores2[,3],batters$playerName, col=as.integer(batters.clean$fInducted)))

# summary(pca.one)
# screeplot(pca.one, type="lines")

#analysis
pacman::p_load("rpart", "maptree", "pROC")
batters.clean <- batters[,c(-1,-2)]

y <- batters$inducted
set.seed(1) # fix seed
n=nrow(batters.clean)
train.index=sample(n,n*2/3)
batters.train=batters.clean[train.index, ]
batters.test=batters.clean[-train.index, ]

pca.batters<- prcomp(batters.train, scale.=TRUE)
#summary(pca.train)
screeplot(pca.batters, type="lines")
sum(pca.batters$sdev^2 > 1) # check Kaiser's criterion

data2 <- data.frame(y=batters.train[, "inducted"], pca.batters$x)
fit <- glm(y ~ PC1 + PC2 + PC3 + PC4 + PC5, data=data2, family="binomial")
summary(fit)

test.p <- predict(pca.batters, batters.test)
pred <- predict(fit, newdata = data.frame(test.p), type = "response")
predTable <- factor(ifelse(pred>=0.4,"Y","N"))
table(batters.test$inducted,predTable)

fit.roc=roc(batters.test$inducted, pred, plot=T)
auc(fit.roc)

test.all <- predict(pca.batters, batters)
pred.all <- predict(fit, newdata=data.frame(test.all), type="response")
predTable2 <- factor(ifelse(pred.all>=0.4,"Y","N"))

batters$pred <- as.numeric(predTable2) - 1
confusion <- table(batters$inducted,predTable2)
1 - sum(diag(confusion))/sum(confusion) # error rate vs 0.03629764
sum(batters$inducted)/nrow(batters) # 96.37% did not make the HOF

sum(batters$inducted!=batters$pred)
batters[which(batters$inducted!=batters$pred), ]

fit.all.roc=roc(batters$inducted, pred.all, plot=T)
auc(fit.all.roc)