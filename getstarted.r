# mlb hall of fame classification analysis 
# chua@wharton.upenn.edu

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
