# cind-820
CIND 820 Final Project

_###Code applied to download play by play files, convert them into csv files, create the matrix in the analyzed year, 
measure the success of a batting play, and the mean value of a homerun, single, and stolen base in a given year. Using 
the find key, quick search can be applied to select all values of the year, and replace all values with the next year, 
and the code can be run again to find the respective values for the next year. The R code used has been slightly modified
for my personal computer, as well as updated provided from the initial book where it was retrieved from. Additionally, 
an updated version of “parse.retrosheet2.pbp(2001)” was retrieved from a blog post from the author of the book. ###
_


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

#################################################
# Cleaning and Assesting the Data
#################################################

install.packages("ggplot2")
library(ggplot2)

setwd("~/")
RV_data <- read.csv("Runs value data.csv")

Year <- RV_data$ï..Year
HR_Run_Value <- RV_data$Home.Run.Runs.Value
HR_Percentage <- RV_data$Home.Run.Percentage
Single_Run_Value <- RV_data$Single.Runs.Value
Single_Percentage <- RV_data$Single.Percentage
SB_Run_Value <- RV_data$Stolen.Base.Runs.Value
SB_Percentage <- RV_data$Stolen.Base.Percentage
SB_APG <- RV_data$Stolen.Base.Attempts.Per.Game
Strikeout_Percentage <- RV_data$Stikeout.Percentage

```

Initial Assessment of data, to analyze univariate data of each attribute being assessed.


```{r}
library("stats")
library("psych")

summary(Year)

summary(HR_Run_Value)
describe(HR_Run_Value)

summary(HR_Percentage)
describe(HR_Percentage)

summary(Single_Run_Value)
describe(Single_Run_Value)

summary(Single_Percentage)
describe(Single_Percentage)

summary(SB_Run_Value)
describe(SB_Run_Value)

summary(SB_Percentage)
describe(SB_Percentage)

summary(SB_APG)
describe(SB_APG)

summary(Strikeout_Percentage)
describe(Strikeout_Percentage)

plot(RV_data)
```

Testing the correlation between the variables

```{r}
#Overall Correlation of entire data set
cor(RV_data)

#Correlation between the run value of a home run and the percentage of plate appearances that result in a home run. 
cor.test(HR_Run_Value, HR_Percentage)

#Correlation between the run value of a single and the percentage of hits that are singles.
cor.test(Single_Run_Value, Single_Percentage)

#Correlation between the run value of a stolen base and the percentage of stolen base attempts that are successful. 
cor.test(SB_Run_Value, SB_Percentage)

```

Assessing the change of each variable over time. 


```{r}
#Run Value of a Home run over time
plot(Year, HR_Run_Value, ylab = "HR Run Value", main= "Year vs HR Run Value") + abline(lm(HR_Run_Value~Year, data=RV_data))
summary(lm(Year~HR_Run_Value))

#Home Run Percentage over time
plot(Year, HR_Percentage, ylab = "HR Run Percentage", main= "Year vs HR Percentage") + abline(lm(HR_Percentage~Year, data=RV_data))
summary(lm(Year~HR_Percentage))

#Run Value of a single over time
plot(Year, Single_Run_Value, ylab = "Single Run Value", main= "Year vs Single Run Value") + abline(lm(Single_Run_Value~Year, data=RV_data))
summary(lm(Year~Single_Run_Value))

#Single percentage over time
plot(Year, Single_Percentage, ylab = "Single Percentage", main= "Year vs Single Percentage") + abline(lm(Single_Percentage~Year, data=RV_data))
summary(lm(Year~Single_Percentage))

#Run Value of a stolen base over time
plot(Year, SB_Run_Value, ylab = "Stolen Base Run Value", main= "Year vs Stolen Base Run Value") + abline(lm(SB_Run_Value~Year, data=RV_data))
summary(lm(Year~SB_Run_Value))

#Stolen Base percentage over time
plot(Year, SB_Percentage, ylab = "Stolen Base Percentage", main= "Year vs Stolen Base Percentage") + abline(lm(SB_Percentage~Year, data=RV_data))
summary(lm(Year~SB_Percentage))

#Stolen Base attempts per game over time
plot(Year, SB_APG, ylab = "SB Attempts Per Game", main= "Year vs SB Attempts Per Game") + abline(lm(SB_APG~Year, data=RV_data))
summary(lm(Year~SB_APG))

#Strikeout Percentage over time
plot(Year, Strikeout_Percentage, ylab = "Strikeout Percentage", main= "Year vs Strikeout Percentage") + abline(lm(Strikeout_Percentage~Year, data=RV_data))
summary(lm(Year~Strikeout_Percentage))

```


Predictive Analysis

```{r}
#building linear regression model
library(caTools)

sample.split(HR_Run_Value, SplitRatio = 0.70) -> split_tag
subset(RV_data, split_tag==T) -> train
subset(RV_data, split_tag==F) -> test

lm(HR_Run_Value~Year, data = train) -> model1
predict(model1, newdata = test) -> predicted_values
head(predicted_values)

cbind(Actual= test$Home.Run.Runs.Value, Predicted= predicted_values) -> final_data
as.data.frame(final_data)-> final_data

final_data$Actual - final_data$Predicted -> error
cbind(final_data,error) ->final_data
head(final_data)
rmse <- sqrt(mean(final_data$error)^2)
rmse
```

```{r}
# multi linear regression model
library(caTools)

sample.split(HR_Run_Value, SplitRatio = 0.75) -> split_model
subset(RV_data, split_model==T) -> train2
subset(RV_data, split_model==F) -> test2

lm(HR_Run_Value~Year+HR_Percentage+Single_Run_Value+Single_Percentage+SB_Run_Value+SB_Percentage+SB_APG+Strikeout_Percentage, data = train2) -> mod_multi_linear

predict(mod_multi_linear, newdata = test2) -> predicted_multi_linear
head(predicted_multi_linear)

cbind(Actual= test2$Home.Run.Runs.Value, Predicted = predicted_multi_linear) -> final_data2
as.data.frame(final_data2) ->final_data2
head(final_data2)

final_data2$Actual - final_data2$Predicted -> error2
head(error)

cbind(final_data2,error2) ->final_data2
head(final_data2)

rmse_ml<- sqrt(mean((final_data2$error2)^2))
rmse_ml
```
Lower model presented above is the first linear regression model when assesing the run value of a home run. 

