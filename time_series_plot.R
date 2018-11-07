#For running time series analyses on important areas and produce plots
#Pollutants: PM2.5, PM10, SO2, NO2, O3, CO
#Chinese Air Quality Standard HJ663-2013
#Outputs are air pollution time series analyses
#Run daily_assessment.R first to get daily assessment

library(ggplot2) 
library(reshape2)
library(tidyverse)
library(xts)

setwd("/Users/sherry/Desktop/china_air/data")

#Function for plotting multiple plots
#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#read policy related daily data
#read provincial daily data
policy_files <- list.files(pattern = ".*(_policy_daily)+.csv")
province_files <- list.files(pattern = ".*(_province_daily)+.csv")

#Need to reformat to short table
policy_var <- data.frame()
province_var <- list()

a = 1
for (a in 1:length(city_files)){
  city_var[[a]] <- read.csv(policy_files[a])
  a = a+1
}

a = 1
for (a in 1:length(province_files)){
  province_var[[a]] <- read.csv(province_files[a])
  a = a+1
}

#calculate


#line plot

#bar plot

#time-series

#maps


