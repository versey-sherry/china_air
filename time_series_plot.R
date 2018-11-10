#For running time series analyses on important areas and produce plots
#Pollutants: PM2.5, PM10, SO2, NO2, O3, CO
#Chinese Air Quality Standard HJ663-2013
#Outputs are air pollution time series analyses
#Run daily_assessment.R first to get daily assessment

library(ggplot2) 
library(reshape2)
library(tidyverse)
library(tidyquant)
library(ggpubr)
library(wesanderson)

setwd("/Users/sherry/Desktop/china_air/data")

#Function for generating plot date
plot_date <- function(x){
  return(paste(month(x), day(x), sep = "/"))
}

#read policy related and provincial daily data
files <- c(list.files(pattern = ".*(_policy_daily)+.csv"), list.files(pattern = ".*(_province_daily)+.csv"))
#set up pollutant scopes
pollutants <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")
winter_action_plan <- data.frame(x1 = , x2 = , y1 = -Inf, y2 = +Inf,
                                 period = "Winter Action Plan")


#Read all tables in as variables for further subsetting
a = 1
for (a in 1:length(files)){
  assign(gsub("_daily.csv", "", files[a]), read.csv(files[a]))
  a = a+1
}

#calculate 30-day rolling average
#calculate 365-day rolling average

a = 1
for (a in 1:length(files)){
  temp <- eval(parse(text = gsub("_daily.csv", "", files[a]))) %>%
    melt(., id = "X")
  #format date-time
  temp$X <- as.Date(as.character(temp$X), "%Y%m%d")
  temp <- temp %>%
    tibble::as.tibble() %>%
    group_by(variable) %>%
    #get 30-day rolling average
    tq_mutate(
      select = value,
      mutate_fun = rollapply,
      #rollapply agrs
      width = 30,
      align = "right",
      #Function args
      FUN = mean,
      na.rm = TRUE,
      #tq_mutate arg,
      col_rename = "mean_month") %>%
    #get 365 rolling average
    tq_mutate(
      select = value,
      mutate_fun = rollapply,
      #rollapply agrs
      width = 365,
      align = "right",
      #Function args
      FUN = mean,
      na.rm = TRUE,
      #tq_mutate arg,
      col_rename = "mean_year")
  assign(gsub("_daily.csv", "", files[a]), temp)

}

####Monthly running average analysis###
#repeat for years
#Line plots for running average
temp <- eval(parse(text = paste(pollutants[1], "_policy", sep = "")))
month_moving <- temp %>% select(X, variable, mean_month)

a = 2
for (a in 2:length(pollutants)){
  temp <- eval(parse(text = paste(pollutants[a], "_policy", sep = "")))
  month_moving <- temp %>% select(X, variable, mean_month) %>% left_join(., month_moving, by = c("X", "variable"))
}
names(month_moving) <- c("Date", "Area", rev(pollutants))

#change it to long format for plotting
#ggplot doesn't support pipe inside ggplot arguement so subsetting data happens in data manipulation
month_moving <- melt(month_moving, id.vars = c("Date", "Area"), measure.vars = pollutants) %>%
  tibble::as.tibble() %>% 
  #subset the needed plots
  filter(., Area %in% c("JJJ", "PRD", "YRD", "Fenwei") & !is.na(value))

#Set up the periodic highlights according to policies
highlight <- data.frame(x1 = as.Date("2017-11-15"), x2 = as.Date("2018-03-15"), 
                         y1 = -Inf, y2 = +Inf, period = "Winter Action Plan")

#plots with period highlight
ggplot() +
  #plotting pollution line
  geom_line(data = month_moving,
            aes(x = Date, y = value, color = Area)) +
  #plotting policy period
  geom_rect(data = highlight, 
            mapping = aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill = period), 
            color = NA, alpha = 0.5) +
  labs(title = "JJJ PRD YRD Fenwei", subtitle = "30 Day Moving Average") +
  scale_color_manual(values = wes_palette("Darjeeling1", 6, type = "continuous")) + 
  theme_tq() + 
  facet_wrap(~ variable, scales = "free") +
  theme(legend.position = "bottom")

#year on year monthly moving average for PM2.5
month_moving <- month_moving %>% 
  filter(., variable == "PM2.5") %>% 
  select(Date, Area, value) %>%
  mutate(year = year(Date),
         plotdate = strftime(Date, format = "%j")) %>%
  select(plotdate, year, Area, value)

  ggplot(data = month_moving, aes(x = plotdate, y = value, color = year)) + 
  geom_line() +
  labs(title = "JJJ PRD YRD Fenwei", subtitle = "PM2.5 30 Day Moving Average") +
  #scale_color_manual(values = wes_palette("Darjeeling1", 5, type = "continuous")) + 
  #theme_tq() + 
  facet_wrap(~ Area, scales = "free") +
  theme(legend.position = "bottom")
  
