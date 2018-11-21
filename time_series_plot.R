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

#Function to generate digit in percent form
percent <- function(x){
  return(paste(round(x*100, 2), "%", sep = ""))
}

#read policy related and provincial daily data
files <- c(list.files(pattern = ".*(_policy_daily)+.csv"), list.files(pattern = ".*(_province_daily)+.csv"))
#set up pollutant scopes
pollutants <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")


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
highlight <- data.frame(x1 = c(as.Date("2017-11-15"), as.Date("2018-10-01")), 
                        x2 = c(as.Date("2018-03-15"), as.Date("2019-03-31")), 
                         y1 = c(-Inf, -Inf), y2 = c(+Inf, +Inf), period = "Winter Action Plan")

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

####Yearly running average analysis###
temp <- eval(parse(text = paste(pollutants[1], "_policy", sep = "")))
year_moving <- temp %>% select(X, variable, mean_year)

a = 2
for (a in 2:length(pollutants)){
  temp <- eval(parse(text = paste(pollutants[a], "_policy", sep = "")))
  year_moving <- temp %>% select(X, variable, mean_year) %>% left_join(., year_moving, by = c("X", "variable"))
}
names(year_moving) <- c("Date", "Area", rev(pollutants))

#change it to long format for plotting
#ggplot doesn't support pipe inside ggplot arguement so subsetting data happens in data manipulation
year_moving <- melt(year_moving, id.vars = c("Date", "Area"), measure.vars = pollutants) %>%
  tibble::as.tibble() %>% 
  #subset the needed plots
  filter(., Area %in% c("JJJ", "PRD", "YRD", "Fenwei") & !is.na(value))

#plots with period highlight
ggplot() +
  #plotting pollution line
  geom_line(data = year_moving,
            aes(x = Date, y = value, color = Area)) +
  #plotting policy period
  geom_rect(data = highlight, 
            mapping = aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill = period), 
            color = NA, alpha = 0.5) +
  labs(title = "JJJ PRD YRD Fenwei", subtitle = "365 Day Moving Average") +
  scale_color_manual(values = wes_palette("Darjeeling1", 6, type = "continuous")) + 
  theme_tq() + 
  facet_wrap(~ variable, scales = "free") +
  theme(legend.position = "bottom")

#year on year monthly moving average for PM2.5
month_moving %>% 
  filter(., variable == "PM2.5") %>% 
  select(Date, Area, value) %>%
  mutate(year = factor(year(Date)),
         plotdate = as.Date(paste("2014", month(Date), day(Date), sep = "-"))) %>%
  select(plotdate, year, Area, value) %>% 
  ggplot(., aes(x = plotdate, y = value, color = year)) + 
  geom_line() +
  scale_x_date(date_labels = "%b") + 
  labs(title = "JJJ PRD YRD Fenwei", subtitle = "PM2.5 30 Day Moving Average by year") +
  scale_color_manual(values = wes_palette("Darjeeling1", 5, type = "continuous")) + 
  theme_tq() + 
  facet_wrap(~ Area, scales = "free") +
  theme(legend.position = "bottom")

#subset Ozone for April to August analysis
temp <- eval(parse(text = paste(pollutants[1], "_policy", sep = "")))
ozone_month_moving <- temp %>% select(X, variable, mean_month)

a = 2
for (a in 2:length(pollutants)){
  temp <- eval(parse(text = paste(pollutants[a], "_policy", sep = "")))
  ozone_month_moving <- temp %>% select(X, variable, mean_month) %>% left_join(., ozone_month_moving, by = c("X", "variable"))
}
names(ozone_month_moving) <- c("Date", "Area", rev(pollutants))

#change it to long format for plotting
#ggplot doesn't support pipe inside ggplot arguement so subsetting data happens in data manipulation
ozone_month_moving <- melt(ozone_month_moving, id.vars = c("Date", "Area"), measure.vars = pollutants) %>%
  tibble::as.tibble() %>% 
  #subset the needed plots
  filter(., variable == "O3", Area %in% c("JJJ", "PRD", "YRD", "Fenwei") & !is.na(value))

#Set up the periodic highlights according to policies
highlight <- data.frame(x1 = c(as.Date("2016-04-01"), as.Date("2017-04-01"), as.Date("2018-04-01")), 
                        x2 = c(as.Date("2016-09-30"), as.Date("2017-09-30"), as.Date("2018-09-30")), 
                        y1 = c(-Inf, -Inf, -Inf), y2 = c(+Inf, +Inf, +Inf), period = "High Ozone")

#plots with period highlight
ggplot() +
  #plotting pollution line
  geom_line(data = ozone_month_moving,
            aes(x = Date, y = value, color = Area)) +
  #plotting policy period
  geom_rect(data = highlight, 
            mapping = aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill = period), 
            color = NA, alpha = 0.5) +
  labs(title = "JJJ PRD YRD Fenwei", subtitle = "Ozone 30 Day Moving Average") +
  scale_color_manual(values = wes_palette("Darjeeling1", 6, type = "continuous")) + 
  theme_tq() + 
  theme(legend.position = "bottom")

#bar plot for pollutant year on year change
temp <- eval(parse(text = paste(pollutants[1], "_policy", sep = "")))
year_year <- temp %>% mutate(year = year(X)) %>% 
  group_by(variable, year) %>% 
  summarise(year_mean = mean(value, na.rm = TRUE)) %>%
  group_by(variable) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(p_change = (year_mean/lag(year_mean)) -1) %>%
  select(variable, year, p_change)

a = 2
for (a in 2:length(pollutants)){
  temp <- eval(parse(text = paste(pollutants[a], "_policy", sep = "")))
  year_year <- temp %>% mutate(year = year(X)) %>% 
    group_by(variable, year) %>% 
    summarise(year_mean = mean(value, na.rm = TRUE)) %>%
    group_by(variable) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(p_change = (year_mean/lag(year_mean)) -1) %>%
    select(variable, year, p_change) %>%
    left_join(., year_year, by = c("variable", "year"))
}
names(year_year) <- c("Area", "Year", rev(pollutants))

#plot bar chart
#select needed year
year_year %>% group_by(Area, Year) %>% 
  filter(., Year == "2017") %>% 
  melt(., id.vars = c("Area", "Year"), measure.vars = pollutants) %>%
  mutate(vjust = ifelse(value >0, -0.25, 0.25)) %>%
  ggplot(., aes(x = variable, y = value)) +
  geom_bar(aes(fill = value < 0), stat = "identity") + 
  scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), 
                    values = wes_palette("Darjeeling1", 2, type = "continuous")) +
  labs(title = "All Special Areas", subtitle = "year on year bar plot") +
  geom_text(aes(label = percent(value), vjust = ifelse(value > 0, -0.25, 1.25)), position =position_dodge(width=0.9)) +
  theme_tq() + 
  facet_wrap(~ Area, scales = "free") +
  theme(legend.position = "bottom")
  



