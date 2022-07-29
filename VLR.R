library(lubridate)
library(rvest) #look at functions
library(dplyr)
library(ggplot2)

# helpful stuff

## strip away the HTML tags
#gsub("<[^>]*>","",a[i])

## after finding the variables i care abt, create a df and then run across multiple urls 
#data0 <- data.frame(movie=gsub("<[^>]*>","",a[i]),
#gross=gsub("<[^>]*>|[,$]","",a[k]),
#date =ymd("2019-07-04")
#)

a <- scan("https://www.vlr.gg/69877/cloud9-vs-100-thieves-champions-tour-north-america-stage-1-challengers-w1",
          what="",sep="\n", encoding ="UTF-8")
a[1:10]
a[150:200]

grep("Economy",a) #839
a[800:900]


  b <- scan("https://www.vlr.gg/69877/cloud9-vs-100-thieves-champions-tour-north-america-stage-1-challengers-w1/?game=all&tab=economy",
          what="",sep="\n", encoding = "UTF-8")

grep("Eco \\(won\\)",b) #1005, 1699, 2306
b[c(1005,1699,2306)]

b[950:1050]

grep("All Maps",b) #867
b[867:900]
grep("Split",b)

# VLR is very particular in storing games. The original URL loaded into the variable "a" has the
# same output as adding /?game=all&tab=overview. However, when looking at individual maps, it 
# seems that VLR has an overall counter/ID for games. For example, when you leave the "All Maps"
# tab and click on Map 1, the URL changes to /?game=70808&tab=overview. 

# Because of this, I think I will have the most success finding the line where the "All Maps" eco
# table is stored and loading that into RStudio. Ill attempt that now.


grep("All Maps",b) #867
grep("Pistol Won", b) #1002 1696 2303

b[850:1050]
b[1020:1100]
b[1100:1300]
b[9900:10100]

# For some reason, the html doesnt have the raw input for the "All Maps" table. It has the 
# information inside the parentheses, but not the totals. 

# Go again, but storing "Pistol Won" all the way to "Eco: 0-5K". This should be the entire tbl.

eco.start1 <- grep("Pistol Won", b)[1]
eco.end1 <- grep("Eco: 0-5k", b)[1]
b[eco.start1:eco.end1]

eco.start2 <- grep("Pistol Won", b)[2]
eco.end2 <- grep("Eco: 0-5k", b)[2]
b[eco.start2:eco.end2]

eco.start3 <- grep("Pistol Won", b)[3]
eco.end3 <- grep("Eco: 0-5k", b)[3]
gsub("\t","",b[eco.start3:eco.end3])

# The "All Maps" table is the last occurrence of "Pistol Won" -> "Eco: 0-5k". However,
# it still doesnt include the un-parenthesized numbers :/
# trying to see if this is consistent in v1 game vs pk

c <- scan("https://www.vlr.gg/69878/version1-vs-knights-champions-tour-north-america-stage-1-challengers-w1/?game=all&tab=economy",
          what="",sep="\n", encoding ="UTF-8")

c1 <- grep("Pistol Won", c)[3]
c2 <- grep("Eco: 0-5k", c)[3]
c[c1:c2]

# Checking if the same exists for old games.

d <- scan("https://www.vlr.gg/51296/acend-vs-gambit-esports-valorant-champions-2021-gf/?game=all&tab=economy",
          what="",sep="\n", encoding ="UTF-8")

d1 <- grep("Pistol Won", d)[6]
d2 <- grep("Eco: 0-5k", d)[6]

d[d1:d2]

# Ask Professor Ridgeway/Alex if they have any idea where the non parenthesized numbers are



### thespike.gg is better at storing stats. lets scrape some.

a <- scan("https://www.thespike.gg/events/stats/players/north-america-stage-1-main-event/1214",
          what="",sep="\n", encoding ="UTF-8")
grep("wippie",a)
a[300:400]

#potential end of row
grep("/tr",a)
grep("a target", a) # has 60 happenings for 60 players
a[2297:2350] #mada

#conclusion: every row starts with "a target"

#lets try and just have "wippie" from line 311
wippieline <- grep("a target", a, value = T)[1]
wippieline

#table ends at mada, at line ~2300
grep("mada", a)

#singular row information
wippie1 <- grep("a target",a)[1]
wippie2 <- grep("a target",a)[2] - 1

a[wippie1:wippie2]

#checking input for SicK to see if it stores agents with "+1"
grep("SicK", a)
a[1258:1300]
#thespike doesnt seem to store the 3rd agent


## ACTUALLY DOWNLOADING THE TABLE

i <- grep("a target", a)
i <- cbind(i, c(i[-1], i[60]+34)-1)
i

tab <- apply(i, 1, function(j)
{
  b <- a[j[1]:j[2]]
  name <- grep("<p>",b,value=TRUE) 
  team <- grep("<small>",b,value=TRUE) 
  
  k <- grep('<td class="bold">', b)
  b <- gsub("<[^>]*>", "", b[k])

  return( c(name, team, b) )  
})
tab <- t(tab)

tab <- as.data.frame(tab)
tab

## TAB IS THE ENTIRE TABLE. NOW TO CLEAN IT

# rename columns
tab <- 
  tab %>% 
  rename(
    player = V1,
    team = V2,
    rating = V3,
    ACS = V4,
    roundsplayed = V5,
    ADR = V6,
    KillDeath = V7,
    KASTp = V8,
    KPR = V9,
    DPR = V10,
    APR = V11,
    HSp = V12,
    FB = V13,
    FD = V14,
    FBsuccess = V15,
    FBperR = V16, 
    ONEvXp = V17,
    ONEvXtry = V18,
    MultiKill = V19)

#clean player name
tab$player <- gsub("<p>","",tab$player)
tab$player <- gsub("</p>","",tab$player)
tab

#clean team name
tab$team <- gsub("<small>","",tab$team)
tab$team <- gsub("</small>","",tab$team)
tab


#checking columns 
lapply(tab, is) # everything is a character, thats wrong

#rating
tab$rating <- as.numeric(tab$rating)
tab$rating

#ACS
tab$ACS <- as.numeric(tab$ACS)
tab$ACS

#roundsplayed
tab$roundsplayed <- as.integer(tab$roundsplayed)

#ADR
tab$ADR <- as.numeric(tab$ADR)

#KillDeath
tab$KillDeath <- as.numeric(tab$KillDeath)

#KASTpercent
tab$KASTp <- gsub("%","",tab$KASTp)
tab$KASTp <- as.numeric(tab$KASTp)
tab$KASTp <- tab$KASTp * .01

#Kills, Deaths, and Assists per round
tab$KPR <- as.numeric(tab$KPR)
tab$DPR <- as.numeric(tab$DPR)
tab$APR <- as.numeric(tab$APR)

#headshot percentage
tab$HSp <- gsub("%","",tab$HSp)
tab$HSp <- as.numeric(tab$HSp)
tab$HSp <- tab$HSp * .01

#First Bloods and First Deaths
tab$FB <- as.integer(tab$FB)
tab$FD <- as.integer(tab$FD)

#FB success
tab$FBsuccess <- gsub("%","", tab$FBsuccess)
tab$FBsuccess <- as.numeric(tab$FBsuccess)
tab$FBsuccess <- tab$FBsuccess * .01

#FB per round
tab$FBperR <- as.numeric(tab$FBperR)

#ONE v X %
tab$ONEvXp <- gsub("%","",tab$ONEvXp)
tab$ONEvXp <- as.numeric(tab$ONEvXp)
tab$ONEvXp <- tab$ONEvXp * .01

#multikills
tab$MultiKill <- as.integer(tab$MultiKill)

#seperating onevxtry into 2 columns
grep("^[0-9]",tab$ONEvXtry, value = T)
substring(tab$ONEvXtry[2],0,1) #substring wont work cause could be single, double, or trip dig
?substr
sub(".*/ ", "", tab$ONEvXtry[2])  
tab$ONEvXtry


### AFTER CLEANING
tab

## quick plot
ggplot(tab) + geom_point(aes(x = ACS, y = FBsuccess))

