# mlb hall of fame classification analysis chua@wharton.upenn.edu

rm(list = ls())
par(mfrow = c(1, 1))

if (!require("pacman")) install.packages("pacman")
pacman::p_load("Lahman", "dplyr", "ggplot2")

# get career batting totals
batting <- summarize(group_by(Batting, playerID), numYears = length(unique(yearID)), 
                     lastYear = max(yearID), g = sum(G, na.rm = TRUE), ab = sum(AB, na.rm = TRUE), 
                     r = sum(R, na.rm = TRUE), h = sum(H, na.rm = TRUE), x2b = sum(X2B, na.rm = TRUE), 
                     x3b = sum(X3B, na.rm = TRUE), hr = sum(HR, na.rm = TRUE), rbi = sum(RBI, na.rm = TRUE), 
                     sb = sum(SB, na.rm = TRUE), cs = sum(CS, na.rm = TRUE), bb = sum(BB, na.rm = TRUE), 
                     so = sum(SO, na.rm = TRUE), ibb = sum(IBB, na.rm = TRUE), hbp = sum(HBP, na.rm = TRUE), 
                     sh = sum(SH, na.rm = TRUE), sf = sum(SF, na.rm = TRUE), gidp = sum(GIDP, na.rm = TRUE))

# get career all-star game totals
asg <- summarize(group_by(AllstarFull, playerID), asgApp = sum(GP))

# get career awards: MVP, Gold Glove (fielding), Silver Slugger (batting), Triple
# Crown (BA, HR, RBI) we could get more, but these should be good
awards <- summarize(group_by(AwardsPlayers, playerID), mvp = sum(awardID == "Most Valuable Player"), 
                    gg = sum(awardID == "Gold Glove"), sisl = sum(awardID == "Silver Slugger"), tc = sum(awardID == 
                                                                                                           "Triple Crown"))

# bring in Wins Above Replacement metric (WAR) - FanGraphs data

warD <- read.csv("http://www.baseball-reference.com/data/war_daily_bat.txt")
colnames(warD)[3] <- "playerID"
warD$WAR <- as.numeric(paste(warD$WAR))
warD$WAR_off <- as.numeric(paste(warD$WAR_off))
warD$Inn <- as.numeric(paste(warD$Inn))

warD2 <- summarize(group_by(warD, playerID), totalWAR = sum(WAR), totalInn = sum(Inn), 
                   playerName = first(name_common))
warD <- warD2
rm(warD2)

# fix a typo in Lahman's db
data(HallOfFame)
HallOfFame$playerID[HallOfFame$playerID == "glavito01"] <- "glavito02"

# find individuals elected to HOF
hof2 <- filter(HallOfFame, votedBy %in% c("BBWAA", "Special Election", "Veterans") & 
                 category == "Player")
hof2 <- filter(HallOfFame, category == "Player")
# set induction to numeric; 0 = not inducted, 1 = inducted
hof2$inducted <- as.numeric(hof2$inducted) - 1

# sum(HallOfFame$inducted =='Y') # number of inducted members which.max(inducted)
# # number of years on ballot, if inducted

# find lifetime stats - if they were inducted in the end
hof <- summarize(group_by(hof2, playerID), inducted = max(inducted))

# merge columns into one data.frame
data <- merge(batting, warD, by = "playerID", all = TRUE)
data <- merge(data, awards, by = "playerID", all = TRUE)
data <- merge(data, asg, by = "playerID", all = TRUE)
data <- merge(data, hof, by = "playerID", all = TRUE)

data[is.na(data)] <- 0

# add PA, OBP, OPS, SLG, RC, BA
data$pa <- (data$ab + data$bb + data$hbp + data$sf + data$sh)
data$obp <- ((data$h + data$bb + data$hbp)/(data$ab + data$bb + data$hbp + data$sf))
data$slg <- ((2 * data$x2b) + (3 * data$x3b) + (4 * data$hr))/data$ab
data$ops <- data$obp + data$slg
data$rc <- ((data$h + data$bb) * (data$h + data$x2b + 2 * data$x3b + 3 * data$hr)/(data$ab + 
                                                                                     data$bb))
data$bAvg <- data$h/data$ab
data$aby <- data$ab/data$numYears

# qualitative dummy variables
data$threeK <- as.numeric(data$h >= 3000)
data$fiveHundo <- as.numeric(data$hr >= 500)
data$threeHundo <- as.numeric(data$bAvg >= 0.3)
# data$tenYrs = as.numeric(data$numYears >= 10)

# reorder columns so playerName is first
n <- which(names(data) == "playerName")
data <- data[, c(n, (1:(n - 1)), (n + 1):ncol(data))]

# NaN's introduced above - call these 0
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

data[is.nan(data)] <- 0
data[is.na(data)] <- 0

# cleanup
rm(hof2, hof, asg, awards, batting, warD, HallOfFame, n)

# visualization
pacman::p_load("reshape2")
corB <- cor(data.clean, use="complete.obs")
qplot(x=Var1, y=Var2, data=melt(corB), fill=value, geom="tile")

# remove objects for visualization

# begin analysis classifying batters as having 120+ ab/year - see appendix for
# justification
futures <- filter(data, aby>120, lastYear > 2000, numYears>=10)
data <- filter(data, aby > 120, lastYear >= 1950, lastYear <= 2000, numYears >= 10)
data.clean <- data[, -c(1:2)]

set.seed(1)  # fix seed
n <- nrow(data.clean)
train.index <- sample(n, n * 3/4)
train <- data.clean[train.index, ]
test <- data.clean[-train.index, ]
test.help <- data[-train.index, ]



##### CROSS-VALIDATED GLM ON PRINCIPAL COMPONENTS
pacman::p_load("boot")

pca1 <- prcomp(data.clean, scale. = TRUE)
# summary(pca.train) screeplot(pca.train, type='lines') sum(pca.train$sdev^2 > 1)
# # check Kaiser's criterion

data1 <- data.frame(y = data[, "inducted"], pca1$x)
fit <- glm(y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = data1, family = "binomial")
# summary(fit)
set.seed(1)
result <- cv.glm(data1, fit, K = 10)
# result$delta
test.p <- predict(pca1, test)
pred <- predict(fit, newdata = data.frame(test.p), type = "response")
predTable <- factor(ifelse(pred >= 0.2, "Y", "N"))
table(test$inducted, predTable)
fit2.error <- sum((test[, "inducted"] - pred)^2)
sum(test$inducted != (as.numeric(predTable) - 1))  # num wrong
vec <- which(test$inducted != (as.numeric(predTable) - 1))
(data[-train.index, ])[vec, ]


##### GLM ON PRINCIPAL COMPONENTS WITH 25% HOLDOUT
pca2 <- prcomp(train, scale. = TRUE)
data2 <- data.frame(y = as.factor(train[, "inducted"]), pca2$x)
screeplot(pca2, type="lines")
fit2 <- glm(y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = data2, family = "binomial")
AIC(fit2)
test2 <- predict(pca2, test)
pred2 <- predict(fit2, newdata = data.frame(test2), type = "response")
predTable2 <- factor(ifelse(pred2 >= 0.2, "Y", "N"))
table(test$inducted, predTable2)
fit2.error <- sum((test[, "inducted"] - pred2)^2)
sum(test$inducted != (as.numeric(predTable2) - 1))  # num wrong
vec2 <- which(test$inducted != (as.numeric(predTable2) - 1))
(data[-train.index, ])[vec2, ]
vec2
pred2[24]
pred2[114]
pred2[163]
testf <- predict(pca2, futures)
predf <- predict(fit2, newdata=data.frame(testf), type="response")  
predTablef <- factor(ifelse(predf >= 0.2, "Y", "N"))
table(futures$inducted, predTablef)
futures[which(futures$inducted == 1), ]
futures[which(predTablef == 'Y'), ]

p2 <- ggplot(data = as.data.frame(pca1$x), aes(x = PC1, y = PC2, label = data$playerName)) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(aes(colour = as.factor(data.clean$inducted)), size = 4) +
  ggtitle("PCA biplot of PC1 and PC2 - Modern Batter set") +
  xlab("PC1") + ylab("PC2")
p2

##### TREE WITH 25% HOLDOUT
pacman::p_load("tree")

fit3 <- tree(as.factor(inducted) ~ ., train)
# plot(fit3) text(fit3)
fit3.pred <- predict(fit3, test)
fit3.error <- sum((fit3.pred - test[, "inducted"])^2)
fit3.error
## cross-validation
fit4 <- cv.tree(fit3, K = 10)
# par(mfrow=c(3,1)) plot(fit4$size, fit4$dev) plot(fit4$k,fit4$dev )
# plot(fit4$size, fit4$k) prune to 4
fit5 <- prune.tree(fit3, best = 4)
par(mfrow = c(1, 1))
fit5.pred <- predict(fit5, test)
fit5.error <- sum((test[, "inducted"] - fit5.pred)^2)
fit5.error

##### RANDOM FOREST WITH 25% HOLDOUT
pacman::p_load("randomForest")
fit6 <- randomForest(as.factor(inducted) ~ ., train, mtry = 6, importance = T)
# Confusion matrix: 0 1 class.error 0 1196 3 0.002502085 1 12 28 0.300000000
fit6.pred <- as.numeric(predict(fit6, test)) - 1
test6.error <- sum((test[, "inducted"] - fit6.pred)^2)
sum(fit6.pred != test[, "inducted"])
table(test$inducted, fit6.pred)
test6.error
varImpPlot(fit6, main="Random Forest Feature Importance") # importance plot
imp <- importance(fit6, type=2) # importance table
as.matrix(imp[order(-imp), ])

##### RANDOM FOREST OF PCA'S
pacman::p_load("class")
train.s <- scale(train)
test.s <- scale(test)
fit7 <- knn(train = train.s, test = test.s, cl = train$inducted, k = 2)
table(test$inducted, fit7)
fit7.error <- mean(test$inducted != fit7)


mod <- rpart(as.factor(data$inducted) ~ ., data = data.clean)
draw.tree(mod)
