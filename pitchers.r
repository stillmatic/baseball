# visualize demarcation between pitchers and batters over time
# chua@wharton.upenn.edu

if (!require("pacman")) install.packages("pacman")
pacman::p_load("Lahman", "dplyr")

batting <- summarize(group_by(Batting, playerID), 
                     ab=sum(AB, na.rm=TRUE), r=sum(R, na.rm=TRUE), 
                     h=sum(H, na.rm=TRUE), x2b=sum(X2B, na.rm=TRUE), 
                     x3b=sum(X3B, na.rm=TRUE), hr=sum(HR, na.rm=TRUE), 
                     rbi=sum(RBI, na.rm=TRUE), sb=sum(SB, na.rm=TRUE), 
                     cs=sum(CS, na.rm=TRUE), bb=sum(BB, na.rm=TRUE), 
                     so=sum(SO, na.rm=TRUE), ibb=sum(IBB, na.rm=TRUE),
                     hbp=sum(HBP, na.rm=TRUE), sh=sum(SH, na.rm=TRUE),
                     sf=sum(SF, na.rm=TRUE), gidp=sum(GIDP, na.rm=TRUE))

pitching <- summarize(group_by(Pitching, playerID), numYears=length(unique(yearID)),
                     lastYear=max(yearID), g=sum(G, na.rm=TRUE), 
                     w = sum(W,na.rm=TRUE), l=sum(L, na.rm=TRUE), 
                     gs=sum(GS, na.rm=TRUE), cg=sum(CG, na.rm=TRUE),
                    sho=sum(SHO, na.rm=TRUE), sv=sum(SV,na.rm=TRUE),
                    ipOuts=sum(IPouts, na.rm=TRUE), hAg=sum(H,na.rm=TRUE),
                    hrAg=sum(HR,na.rm=TRUE), bbAg=sum(BB,na.rm=TRUE),
                    era=sum(ERA,na.rm=TRUE), bfp=sum(BFP,na.rm=TRUE),
                    gf=sum(GF,na.rm=TRUE))

# you can also load with this to get what I used
# warD <- read.csv(https://gist.github.com/stillmatic/46d32989059817c39135/raw/war_daily_bat.txt)
warD <- read.csv("http://www.baseball-reference.com/data/war_daily_bat.txt")
colnames(warD)[3] <- "playerID"
warD$WAR <- as.numeric(paste(warD$WAR))
warD$WAR_off <- as.numeric(paste(warD$WAR_off))
warD$Inn <- as.numeric(paste(warD$Inn))

warD2 <- summarize(group_by(warD, playerID), totalWAR=sum(WAR), 
                   totalInn = sum(Inn), pitcher = first(pitcher))
warD <- warD2
rm(warD2)
m=merge(pitching, warD, by="playerID")
m=merge(m, batting,by="playerID")
m$aby = m$ab / m$numYears
m$ipy = m$ipOuts / m$numYears
modern = filter(m, lastYear >= 1950, lastYear <= 2000)
old = filter(m, lastYear < 1950)

par(mfrow=c(1,2)) 
plot(old$ipy ~ old$aby, xlab="At Bats", ylab="Outs Pitched")
title(main="Pre-1950 AB ~ Pitching")
abline(a=50,b=3.1,col="blue")
plot(modern$ipy ~ modern$aby, xlab="At Bats", ylab="Outs Pitched")
title(main="Post-1950 AB ~ Pitching")
abline(v=120,b=0,col="red")
