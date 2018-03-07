# Title: Make Shots Data Script
# Description: This script combines data about different shooters on
# the Golden State Warriors
# Input(s): andre-iguodala.csv, draymond-green.csv,kevin-durant.csv
#   klay-thompson.csv, stephen-curry.csv
# Output(s): shots-data.csv,stephen-curry-summary.txt, andre-iguodala-sumary.txt
#     draymond-green-summary.txt, kevin-durant-summary.txt,
#     klay-thompson-summary.txt,shots-data-summary.txt
# Author: Phoebe Abramowitz
# Date: 03-04-2018
#-------------------------------------------------------------------
library(dplyr)
#column types as characters, integers, and double/reals
#going to treat date as a character, could convert to date later if format changed
column_types <- c(
  rep('character',2),
  rep("integer",4),
  rep('character',3),
  'integer',
  'character',
  rep('integer',2)
)

#Ask why I'm getting errors here
curry <- read.csv("../data/stephen-curry.csv",stringsAsFactors=FALSE,colClasses=column_types)
iguodala <- read.csv("../data/andre-iguodala.csv",stringsAsFactors=FALSE,colClasses=column_types)
green <- read.csv("../data/draymond-green.csv",stringsAsFactors=FALSE,colClasses=column_types)
thompson <- read.csv("../data/klay-thompson.csv",stringsAsFactors=FALSE,colClasses=column_types)
durant <- read.csv("../data/kevin-durant.csv",stringsAsFactors=FALSE,colClasses=column_types)

#add column "name" to each data frame
curry <- mutate(curry,name="Stephen Curry")
iguodala <- mutate(iguodala,name="Andre Iguodala")
green <- mutate(green,name="Draymond Green")
thompson <- mutate(thompson,name="Klay Thompson")
durant <- mutate(durant,name="Kevin Durant")

#change values of shot_made_flag to be more descriptive
curry[curry$shot_made_flag=="y","shot_made_flag"] = "made shot"
curry[curry$shot_made_flag=="made_shot","shot_made_flag"] = "made shot"
curry[curry$shot_made_flag=="n","shot_made_flag"] = "missed shot"

iguodala[iguodala$shot_made_flag=="y","shot_made_flag"] = "made shot"
iguodala[iguodala$shot_made_flag=="n","shot_made_flag"] = "missed shot"

green[green$shot_made_flag=="y","shot_made_flag"] = "made shot"
green[green$shot_made_flag=="n","shot_made_flag"] = "missed shot"

thompson[thompson$shot_made_flag=="y","shot_made_flag"] = "made shot"
thompson[thompson$shot_made_flag=="n","shot_made_flag"] = "missed shot"

durant[durant$shot_made_flag=="y","shot_made_flag"] = "made shot"
durant[durant$shot_made_flag=="n","shot_made_flag"] = "missed shot"

#add column minute number where shot occured
curry <- mutate(curry,minute=(12*period)-minutes_remaining)
iguodala <- mutate(iguodala,minute=(12*period)-minutes_remaining)
green <- mutate(green,minute=(12*period)-minutes_remaining)
thompson <- mutate(thompson,minute=(12*period)-minutes_remaining)
durant <- mutate(durant,minute=(12*period)-minutes_remaining)

#use sink() to save text file summaries 
sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()

sink(file='../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink(file='../output/draymond-green-summary.txt')
summary(green)
sink()

sink(file='../output/kevin-durant-summary.txt')
summary(durant)
sink()

sink(file='../output/klay-thompson-summary.txt')
summary(thompson)
sink()

#ask about row names
#use rbind() to stack the tables into one single data frame
shots_data <- rbind(curry,iguodala,green,durant,thompson, make.row.names=FALSE,
                    stringsAsFactors=FALSE)
#write csv in data and send summary with sink() to output
write.csv(shots_data,'../data/shots-data.csv',row.names=FALSE)
sink(file='../output/shots-data-summary.txt')
summary(shots_data)
sink()
