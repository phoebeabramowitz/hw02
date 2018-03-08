# Title: Make Shot Charts Script
# Description: Create shot charts from 2016 season data set 
# Input(s): nba-court.jpg, shots-data.csv
# Output(s): gsw-shot-charts.pdf,stephen-curry-shot-chart.pdf
#   klay-thompson-shot-chart.pdf, draymond-green-shot-chart.pdf
#   kevin-durant-shot-chart.pdf, andre-iguodala-shot-chart.pdf,
#   gsw-shot-charts.pdf
# Author: Phoebe Abramowitz
# Date: 03-07-2018
#=================================================================================
#In Console, ran download.file to put nba-court in "images" folder
library(jpeg)
library(grid)
library(ggplot2)
library(dplyr)

court_file <- ('../images/nba-court.jpg')
court_image <- rasterGrob(
  readJPEG(court_file),
  width=unit(1,"npc"),
  height=unit(1,"npc")
)

#get data in here and sort by player for each chart
shots_data <- read.csv('../data/shots-data.csv')
curry <- filter(shots_data, name=="Stephen Curry")
iguodala <- filter(shots_data, name=="Andre Iguodala")
green <- filter(shots_data, name=="Draymond Green")
durant <- filter(shots_data, name=="Kevin Durant")
thompson <- filter(shots_data, name=="Klay Thompson")

#make charts 
curry_shot_chart <- ggplot(data=curry)+
  annotation_custom(court_image, -250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle("Shot Chart: Steph Curry")+
  theme_minimal()
iguodala_shot_chart <- ggplot(data=iguodala)+
  annotation_custom(court_image, -250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle("Shot Chart: Andre Iguodala")+
  theme_minimal()
green_shot_chart <- ggplot(data=green)+
  annotation_custom(court_image, -250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle("Shot Chart: Draymond Green")+
  theme_minimal()
durant_shot_chart <- ggplot(data=durant)+
  annotation_custom(court_image, -250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle("Shot Chart: Kevin Durant")+
  theme_minimal()
thompson_shot_chart <- ggplot(data=thompson)+
  annotation_custom(court_image, -250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle("Shot Chart: Klay Thompson")+
  theme_minimal()
#show shot charts
# curry_shot_chart
# iguodala_shot_chart
# durant_shot_chart
# green_shot_chart
# thompson_shot_chart

#save each as a pdf in images with a sink
pdf(file="../images/stephen-curry-shot-chart.pdf",width=6.5,height=5)  
  curry_shot_chart
dev.off()

pdf(file="../images/andre-iguodala-shot-chart.pdf",width=6.5,height=5)  
  iguodala_shot_chart
dev.off()

pdf(file="../images/draymond-green-shot-chart.pdf",width=6.5,height=5)  
green_shot_chart
dev.off()

pdf(file="../images/klay-thompson-shot-chart.pdf",width=6.5,height=5)  
thompson_shot_chart
dev.off()

pdf(file="../images/kevin-durant-shot-chart.pdf",width=6.5,height=5)  
durant_shot_chart
dev.off()

#create facetted shot chart c
gsw_shot_chart <- ggplot(data=shots_data)+
  annotation_custom(court_image, -250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  facet_wrap(~name)+
  ggtitle("Shot Charts: GSW(2016 Season")+
  theme_minimal()


pdf(file="../images/gsw-shot-charts.pdf",width=8,height=7)  
gsw_shot_chart
dev.off()

