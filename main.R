# hall of fame elections
## identifying outliers in major league baseball

# chua@wharton.upenn.edu

# wharton lab convenience functions
if (!require("pacman")) install.packages("pacman")
pacman::p_load("Lahman", "dplyr", "ggplot2", "survival", "survMisc")
setwd("~/471-2")

# constants for analysis
MIN_YEAR = 1900

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Batting %>%
  group_by(playerID) %>%
  select(-(stint:lgID)) %>%
  mutate(lastYear = max(yearID),
         yearsPlayed = length(yearID)) %>%
  filter(lastYear >= MIN_YEAR) -> batting

Fielding %>%
  group_by(playerID) %>%
  select(-(stint:lgID)) %>%
  mutate(lastYear = max(yearID),
         yearsPlayed = length(yearID)) %>%
  filter(lastYear >= MIN_YEAR) -> fielding

AllstarFull %>%
  group_by(playerID) %>%
  select(-(gameNum:lgID)) %>%
  mutate(totalASG = length(GP) ) -> asg

AwardsPlayers %>%
  group_by(playerID) %>%
  select(-lgID, -(tie:notes)) -> awards
  
data(HallOfFame)
HallOfFame$playerID[HallOfFame$playerID == 'glavito01'] <- "glavito02"

HallOfFame %>%
  group_by(playerID) %>%
  filter(votedBy %in% c("BBWAA", "Special Election") &
           category == "Player") %>%
  mutate(yob = yearID - min(yearID) + 1)%>%
  select(-category, -needed_note) -> hof
hof$inducted <- as.numeric(hof$inducted)-1

hof.surv <- survfit(Surv(yob) ~ 1, data= hof)
h <- hof.surv$n.event/hof.surv$n.risk

# K-M plots
hof.plot <- survMisc::autoplot(survfit(Surv(yob) ~ 1, data = hof),
                               type = "CI",  alpha=0.7, title="Years on Ballot")
hof.plot

hof.facet <- survMisc::autoplot(survfit(Surv(yob) ~ inducted, data = hof),
                                type = "CI",  alpha=0.7, facet=T, title="Years on Ballot by Outcome")
hof.facet

# simple
hof %>% group_by(playerID) %>% summarize(maxYOB = max(yob)) %>% 
  ggplot(aes(x=maxYOB), stat="bin") + geom_histogram(binwidth=0.8) + ggtitle("Empirical years on ballot")



