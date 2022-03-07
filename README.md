# cind-820
CIND 820 Final Project

###Code applied to download play by play files, convert them into csv files, create the matrix in the analyzed year, 
measure the success of a batting play, and the mean value of a homerun, single, and stolen base in a given year. Using 
the find key, quick search can be applied to select all values of the year, and replace all values with the next year, 
and the code can be run again to find the respective values for the next year. The R code used has been slightly modified
for my personal computer, as well as updated provided from the initial book where it was retrieved from. Additionally, 
an updated version of “parse.retrosheet2.pbp(2001)” was retrieved from a blog post from the author of the book.
###

**###*2001 values used as an example*####**

library(devtools)
source_gist("https://gist.github.com/bayesball/8892981", filename="parse.retrosheet2.pbp.R")
setwd("~/")
parse.retrosheet2.pbp(2001)


setwd("~/download.folder/unzipped")

data2001 <- read.csv("all2001.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(data2001) <- fields[, "Header"]

data2001$RUNS <- with(data2001, AWAY_SCORE_CT + HOME_SCORE_CT)
data2001$HALF.INNING <- with(data2001, 
                            paste(GAME_ID, INN_CT, BAT_HOME_ID))

data2001$RUNS.SCORED <- with(data2001, (BAT_DEST_ID > 3) +
  (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))

RUNS.SCORED.INNING <- aggregate(data2001$RUNS.SCORED, 
                        list(HALF.INNING = data2001$HALF.INNING), sum)

RUNS.SCORED.START <- aggregate(data2001$RUNS, 
                       list(HALF.INNING = data2001$HALF.INNING), "[", 1)

MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
data2001 <- merge(data2001, MAX)
N <- ncol(data2001)
names(data2001)[N] <- "MAX.RUNS"

data2001$RUNS.ROI <- data2001$MAX.RUNS - data2001$RUNS

##################################################
# 5.3  Creating the Matrix
##################################################

get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)                      
}

RUNNER1 <- ifelse(as.character(data2001[,"BASE1_RUN_ID"])=="", 0, 1)
RUNNER2 <- ifelse(as.character(data2001[,"BASE2_RUN_ID"])=="", 0, 1)
RUNNER3 <- ifelse(as.character(data2001[,"BASE3_RUN_ID"])=="", 0, 1)
data2001$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2001$OUTS_CT)

NRUNNER1 <- with(data2001, as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1))
NRUNNER2 <- with(data2001, as.numeric(RUN1_DEST_ID==2 | RUN2_DEST_ID==2 | BAT_DEST_ID==2))
NRUNNER3 <- with(data2001, as.numeric(RUN1_DEST_ID==3 | RUN2_DEST_ID==3 |
  RUN3_DEST_ID==3 | BAT_DEST_ID==3))
NOUTS <- with(data2001, OUTS_CT + EVENT_OUTS_CT)

data2001$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)

data2001 <- subset(data2001, (STATE!=NEW.STATE) | (RUNS.SCORED>0))

library(plyr)
data.outs <- ddply(data2001, .(HALF.INNING), summarize,
                  Outs.Inning = sum(EVENT_OUTS_CT))
data2001 <- merge(data2001, data.outs)
data2001C <- subset(data2001, Outs.Inning == 3)

RUNS <- with(data2001C, aggregate(RUNS.ROI, list(STATE), mean))
RUNS$Outs <- substr(RUNS$Group, 5, 5)
RUNS <- RUNS[order(RUNS$Outs), ]

RUNS.out <- matrix(round(RUNS$x, 2), 8, 3)
dimnames(RUNS.out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS.out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")

RUNS.2002 <- matrix(c(.51, 1.40, 1.14,  1.96, .90, 1.84, 1.51, 2.33,
               .27,  .94,  .68,  1.36, .54, 1.18,  .94, 1.51,
               .10,  .36,  .32,   .63, .23, .52,   .45, .78),
               8, 3)
dimnames(RUNS.2002) <- dimnames(RUNS.out)

cbind(RUNS.out, RUNS.2002)

##################################################
# 5.4  Measuring Success of a Batting Play
##################################################

RUNS.POTENTIAL <- matrix(c(RUNS$x, rep(0, 8)), 32, 1)
dimnames(RUNS.POTENTIAL)[[1]] <- c(RUNS$Group, "000 3","001 3",
                                   "010 3","011 3","100 3","101 3","110 3","111 3") 
data2001$RUNS.STATE <- RUNS.POTENTIAL[data2001$STATE,]
data2001$RUNS.NEW.STATE <- RUNS.POTENTIAL[data2001$NEW.STATE,]
data2001$RUNS.VALUE <- data2001$RUNS.NEW.STATE - data2001$RUNS.STATE + 
  data2001$RUNS.SCORED


##################################################
# 5.8 Run Values of Different Base Hits
##################################################

d.homerun <- subset(data2001, EVENT_CD==23)

table(d.homerun$STATE)

round(prop.table(table(d.homerun$STATE)), 3)

library(MASS)
truehist(d.homerun$RUNS.VALUE)

subset(d.homerun, RUNS.VALUE==max(RUNS.VALUE))[1, 
      c("STATE", "NEW.STATE", "RUNS.VALUE")]

mean.HR <- mean(d.homerun$RUNS.VALUE)
mean.HR

abline(v = mean.HR, lwd=3)
text(1.5, 5, "Mean Runs Value", pos=4)

d.single <- subset(data2001, EVENT_CD == 20)
#library(MASS)
truehist(d.single$RUNS.VALUE)

table(d.single$STATE)

subset(d.single, d.single$RUNS.VALUE==
  max(d.single$RUNS.VALUE))[, c("STATE", "NEW.STATE", "RUNS.VALUE")]

subset(d.single, d.single$RUNS.VALUE == min(d.single$RUNS.VALUE))[
  , c("STATE", "NEW.STATE", "RUNS.VALUE")]

mean.single <- mean(d.single$RUNS.VALUE)
mean.single
abline(v = mean.single, lwd=3)
text(.5, 5, "Mean Runs Value", pos=4)

##################################################
# 5.9  Value of Base Stealing
##################################################

stealing <- subset(data2001, EVENT_CD==6 | EVENT_CD==4)

table(stealing$EVENT_CD)

table(stealing$STATE)

#library(MASS)
truehist(stealing$RUNS.VALUE)

stealing.1001 <- subset(stealing, STATE=="100 1")

table(stealing.1001$EVENT_CD)

with(stealing.1001, table(NEW.STATE))

mean(stealing.1001$RUNS.VALUE)

#################################################

###Cleaning and Assesting the Data###

install.packages("ggplot2")
library(ggplot2)

setwd("~/")
RV_data <- read.csv("Runs value data.csv")
