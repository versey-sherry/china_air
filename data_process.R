#For processing air pollution data downloaded from MEE.
#Pollutant: PM2.5, PM10, SO2, NO2, CO, O3_8h
#Chinese Air Quality Standard HJ663-2013
#Site data download from http://beijingair.sinaapp.com/
#Sort data in folder by date

setwd("/Users/sherry/Desktop/china_air/data")
files <- list.files(pattern = "(china_sites_)+.*.csv")
stations <- read.csv("/Users/sherry/Desktop/china_air/match1.csv", stringsAsFactors = FALSE)

#generate city/province reference to propergate data frame from station list
pm25_city <- data.frame(id = unique(stations$cityid))
pm10_city <- data.frame(id = unique(stations$cityid))
so2_city <- data.frame(id = unique(stations$cityid))
no2_city <- data.frame(id = unique(stations$cityid))
o3_city <- data.frame(id = unique(stations$cityid))
co_city <- data.frame(id = unique(stations$cityid))
pm25_province <- data.frame(id = unique(stations$provinceid))
pm10_province <- data.frame(id = unique(stations$provinceid))
so2_province <- data.frame(id = unique(stations$provinceid))
no2_province <- data.frame(id = unique(stations$provinceid))
o3_province <- data.frame(id = unique(stations$provinceid))
co_province <- data.frame(id = unique(stations$provinceid))

#read air pollution data and process to day level
a = 1
for (a in 1:length(files)){
  air_data <- read.csv(files[a], stringsAsFactors = FALSE)
  date <- as.character(unique(air_data$date))
  #pm2.5
  pollution <- air_data[air_data$type == "PM2.5", ]
  pollution <- pollution[c(-1, -2, -3)]
  pollution_mean <- apply(pollution, 2, mean, na.rm = TRUE)
  #Check if there are more than 21 valid data entry per day.
  # check_21 <- pollution
  # check_21 <- !is.na(check_21)
  # check_21 <- apply(check_21, 2, sum)
  # check_21 <- check_21 >= 21
  # pollution_mean <- data.frame(pollution_mean * check_21)
  pollution_mean <- data.frame(pollution_mean)
  names(pollution_mean) <- date
  pollution_mean$id <- row.names(pollution_mean)
  pollution_mean <- merge(stations, pollution_mean, by.x = "id", by.y = "id", all.x = FALSE, all.y = TRUE)
  pollution_city <- aggregate(pollution_mean[6], by = list(pollution_mean$cityid), mean, na.rm = TRUE)
  pollution_province <- aggregate(pollution_mean[6], by = list(pollution_mean$provinceid), mean, na.rm = TRUE)
  pm25_city <- merge(pm25_city, pollution_city, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  pm25_province <- merge(pm25_province, pollution_province, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  #pm10
  pollution <- air_data[air_data$type == "PM10", ]
  pollution <- pollution[c(-1, -2, -3)]
  pollution_mean <- apply(pollution, 2, mean, na.rm = TRUE)
  # check_21 <- pollution
  # check_21 <- !is.na(check_21)
  # check_21 <- apply(check_21, 2, sum)
  # check_21 <- check_21 >= 21
  # pollution_mean <- data.frame(pollution_mean * check_21)
  pollution_mean <- data.frame(pollution_mean)
  names(pollution_mean) <- date
  pollution_mean$id <- row.names(pollution_mean)
  pollution_mean <- merge(stations, pollution_mean, by.x = "id", by.y = "id", all.x = FALSE, all.y = TRUE)
  pollution_city <- aggregate(pollution_mean[6], by = list(pollution_mean$cityid), mean, na.rm = TRUE)
  pollution_province <- aggregate(pollution_mean[6], by = list(pollution_mean$provinceid), mean, na.rm = TRUE)
  pm10_city <- merge(pm10_city, pollution_city, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  pm10_province <- merge(pm10_province, pollution_province, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  #so2
  pollution <- air_data[air_data$type == "SO2", ]
  pollution <- pollution[c(-1, -2, -3)]
  pollution_mean <- apply(pollution, 2, mean, na.rm = TRUE)
  # check_21 <- pollution
  # check_21 <- !is.na(check_21)
  # check_21 <- apply(check_21, 2, sum)
  # check_21 <- check_21 >= 21
  # pollution_mean <- data.frame(pollution_mean * check_21)
  pollution_mean <- data.frame(pollution_mean)
  names(pollution_mean) <- date
  pollution_mean$id <- row.names(pollution_mean)
  pollution_mean <- merge(stations, pollution_mean, by.x = "id", by.y = "id", all.x = FALSE, all.y = TRUE)
  pollution_city <- aggregate(pollution_mean[6], by = list(pollution_mean$cityid), mean, na.rm = TRUE)
  pollution_province <- aggregate(pollution_mean[6], by = list(pollution_mean$provinceid), mean, na.rm = TRUE)
  so2_city <- merge(so2_city, pollution_city, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  so2_province <- merge(so2_province, pollution_province, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  #no2
  pollution <- air_data[air_data$type == "NO2", ]
  pollution <- pollution[c(-1, -2, -3)]
  pollution_mean <- apply(pollution, 2, mean, na.rm = TRUE)
  # check_21 <- pollution
  # check_21 <- !is.na(check_21)
  # check_21 <- apply(check_21, 2, sum)
  # check_21 <- check_21 >= 21
  # pollution_mean <- data.frame(pollution_mean * check_21)
  pollution_mean <- data.frame(pollution_mean)
  names(pollution_mean) <- date
  pollution_mean$id <- row.names(pollution_mean)
  pollution_mean <- merge(stations, pollution_mean, by.x = "id", by.y = "id", all.x = FALSE, all.y = TRUE)
  pollution_city <- aggregate(pollution_mean[6], by = list(pollution_mean$cityid), mean, na.rm = TRUE)
  pollution_province <- aggregate(pollution_mean[6], by = list(pollution_mean$provinceid), mean, na.rm = TRUE)
  no2_city <- merge(no2_city, pollution_city, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  no2_province <- merge(no2_province, pollution_province, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  #co
  pollution <- air_data[air_data$type == "CO", ]
  pollution <- pollution[c(-1, -2, -3)]
  pollution_mean <- apply(pollution, 2, mean, na.rm = TRUE)
  # check_21 <- pollution
  # check_21 <- !is.na(check_21)
  # check_21 <- apply(check_21, 2, sum)
  # check_21 <- check_21 >= 21
  # pollution_mean <- data.frame(pollution_mean * check_21)
  pollution_mean <- data.frame(pollution_mean)
  names(pollution_mean) <- date
  pollution_mean$id <- row.names(pollution_mean)
  pollution_mean <- merge(stations, pollution_mean, by.x = "id", by.y = "id", all.x = FALSE, all.y = TRUE)
  pollution_city <- aggregate(pollution_mean[6], by = list(pollution_mean$cityid), mean, na.rm = TRUE)
  pollution_province <- aggregate(pollution_mean[6], by = list(pollution_mean$provinceid), mean, na.rm = TRUE)
  co_city <- merge(co_city, pollution_city, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  co_province <- merge(co_province, pollution_province, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  #O3
  pollution <- air_data[air_data$type == "O3_8h", ]
  pollution <- pollution[c(-1, -2, -3)]
  pollution[is.na(pollution)] <- 0
  pollution_mean <- apply(pollution, 2, max)
  # check_21 <- pollution
  # check_21 <- !is.na(check_21)
  # check_21 <- apply(check_21, 2, sum)
  # check_21 <- check_21 >= 21
  # pollution_mean <- data.frame(pollution_mean * check_21)
  pollution_mean <- data.frame(pollution_mean)
  names(pollution_mean) <- date
  pollution_mean$id <- row.names(pollution_mean)
  pollution_mean <- merge(stations, pollution_mean, by.x = "id", by.y = "id", all.x = FALSE, all.y = TRUE)
  pollution_city <- aggregate(pollution_mean[6], by = list(pollution_mean$cityid), mean, na.rm = TRUE)
  pollution_province <- aggregate(pollution_mean[6], by = list(pollution_mean$provinceid), mean, na.rm = TRUE)
  o3_city <- merge(o3_city, pollution_city, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  o3_province <- merge(o3_province, pollution_province, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  a = a + 1
}

#There are functions for generating date from %Y%m%d date type but since special month period is required
#date is processed through string operations and new functions

#generate quater
quater <- function(x){
  if (x == "01" | x == "02" | x == "03"){
    b = "Q1"
  } else if (x == "04" | x == "05" | x == "06"){
    b = "Q2"
  } else if (x == "07" | x == "08" | x == "09"){
    b = "Q3" 
  } else {
    b = "Q4"
  }
  return(b) 
}

#generate half year
half_year <- function(x){
  if (x =="01" | x == "02" | x == "03" | x == "04" | x == "05" | x == "06"){
    b = "H1"
  } else{
    b = "H2"
  }
  return(b)
}

#generage special ozone periods
ozone58 <- function(x){
  if (x == "05" | x == "06" | x == "07" | x == "08"){
    b = "Ozone High Season"
  } else {
    b = "Not Ozone High Season"
  }
  return(b)
}

ozone49 <- function(x){
  if (x == "04" | x == "05" | x == "06" | x == "07" | x == "08" | x == "09"){
    b = "Ozone Health Impact Season"
  } else {
    b = "Not Ozone Health Impact Season"
  }
  return(b)
}

all_pollutant_city <- list(pm25_city, pm10_city, so2_city, no2_city, co_city, o3_city)
all_pollutant_province <- list(pm25_province, pm10_province, so2_province, no2_province, co_province, o3_province)
all_pollutant_str <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")
pollutant_city_year <- data.frame(id = unique(stations$cityid))
pollutant_city_month <- data.frame(id = unique(stations$cityid))
pollutant_city_quarter <- data.frame(id = unique(stations$cityid))
pollutant_city_half <- data.frame(id = unique(stations$cityid))
pollutant_province_year <- data.frame(id = unique(stations$provinceid))
pollutant_province_month <- data.frame(id = unique(stations$provinceid))
pollutant_province_quarter <- data.frame(id = unique(stations$provinceid))
pollutant_province_half <- data.frame(id = unique(stations$provinceid))

#loop through to get the city data
a = 1
for (a in 1:length(all_pollutant_city)){
  air_data <- all_pollutant_city[[a]]
  trans <- data.frame(t(air_data[2:length(air_data)]))
  names(trans) <- air_data$id
  date <- data.frame(rownames(trans))
  date$year <- apply(date[1], 1, substr, 1, 4)
  date$month <- apply(date[1],1, substr, 5, 6)
  date$quarter <- apply(date[3], 1, quater)
  date$half <- apply(date[3], 1, half_year)
  date$month <- paste(date$year, date$month, sep = "")
  date$quarter <- paste(date$year, date$quarter, sep = "")
  date$half <- paste(date$year, date$half, sep = "")
  date <- date[-1]
  trans <- cbind(date, trans)
  
  #year
  year <- aggregate(trans[5:length(trans)], list(trans$year), mean, na.rm = TRUE)
  date <- paste(year$Group.1, all_pollutant_str[a], sep = "_")
  year <- year[-1]
  year <- data.frame(t(year))
  colnames(year) <- date
  year$id <- rownames(year)
  pollutant_city_year <- merge(pollutant_city_year, year, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  
  #month
  month <- aggregate(trans[5:length(trans)], list(trans$month), mean, na.rm = TRUE)
  date <- paste(month$Group.1, all_pollutant_str[a], sep = "_")
  month <- month[-1]
  month <- data.frame(t(month))
  colnames(month) <- date
  month$id <- rownames(month)
  pollutant_city_month <- merge(pollutant_city_month, month, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  
  #quarter
  quarter <- aggregate(trans[5:length(trans)], list(trans$quarter), mean, na.rm = TRUE)
  date <- paste(quarter$Group.1, all_pollutant_str[a], sep = "_")
  quarter <- quarter[-1]
  quarter <- data.frame(t(quarter))
  colnames(quarter) <- date
  quarter$id <- rownames(quarter)
  pollutant_city_quarter <- merge(pollutant_city_quarter, quarter, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  
  #half
  half <- aggregate(trans[5:length(trans)], list(trans$half), mean, na.rm = TRUE)
  date <- paste(half$Group.1, all_pollutant_str[a], sep = "_")
  half <- half[-1]
  half <- data.frame(t(half))
  colnames(half) <- date
  half$id <- rownames(half)
  pollutant_city_half <- merge(pollutant_city_half, half, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  a = a + 1
}

#loop through to get the province data
a = 1
for (a in 1:length(all_pollutant_province)){
  air_data <- all_pollutant_province[[a]]
  trans <- data.frame(t(air_data[2:length(air_data)]))
  names(trans) <- air_data$id
  date <- data.frame(rownames(trans))
  date$year <- apply(date[1], 1, substr, 1, 4)
  date$month <- apply(date[1],1, substr, 5, 6)
  date$quarter <- apply(date[3], 1, quater)
  date$half <- apply(date[3], 1, half_year)
  date$month <- paste(date$year, date$month, sep = "")
  date$quarter <- paste(date$year, date$quarter, sep = "")
  date$half <- paste(date$year, date$half, sep = "")
  date <- date[-1]
  trans <- cbind(date, trans)
  
  #year
  year <- aggregate(trans[5:length(trans)], list(trans$year), mean, na.rm = TRUE)
  date <- paste(year$Group.1, all_pollutant_str[a], sep = "_")
  year <- year[-1]
  year <- data.frame(t(year))
  colnames(year) <- date
  year$id <- rownames(year)
  pollutant_province_year <- merge(pollutant_province_year, year, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  
  #month
  month <- aggregate(trans[5:length(trans)], list(trans$month), mean, na.rm = TRUE)
  date <- paste(month$Group.1, all_pollutant_str[a], sep = "_")
  month <- month[-1]
  month <- data.frame(t(month))
  colnames(month) <- date
  month$id <- rownames(month)
  pollutant_province_month <- merge(pollutant_province_month, month, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  
  #quarter
  quarter <- aggregate(trans[5:length(trans)], list(trans$quarter), mean, na.rm = TRUE)
  date <- paste(quarter$Group.1, all_pollutant_str[a], sep = "_")
  quarter <- quarter[-1]
  quarter <- data.frame(t(quarter))
  colnames(quarter) <- date
  quarter$id <- rownames(quarter)
  pollutant_province_quarter <- merge(pollutant_province_quarter, quarter, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  
  #half
  half <- aggregate(trans[5:length(trans)], list(trans$half), mean, na.rm = TRUE)
  date <- paste(half$Group.1, all_pollutant_str[a], sep = "_")
  half <- half[-1]
  half <- data.frame(t(half))
  colnames(half) <- date
  half$id <- rownames(half)
  pollutant_province_half <- merge(pollutant_province_half, half, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  a = a + 1
}

#Special Ozone treatment for city data
air_data <- o3_city
trans <- data.frame(t(air_data[2:length(air_data)]))
names(trans) <- air_data$id
date <- data.frame(rownames(trans))
date$year <- apply(date[1], 1, substr, 1, 4)
date$month <- apply(date[1],1, substr, 5, 6)
date$sp1 <- apply(date[3], 1, ozone58)
date$sp2 <- apply(date[3], 1, ozone49)
date$sp1 <- paste(date$year, date$sp1, sep = "")
date$sp2 <- paste(date$year, date$sp2, sep = "")
date <- date[c(-1, -2, -3)]
trans <- cbind(date, trans)

#special onzone month period May through August (sp1) for city
sp1_city <- aggregate(trans[3:length(trans)], list(trans$sp1), mean, na.rm = TRUE)
sp1_city <- data.frame(t(sp1_city))
colnames(sp1_city) <- as.character(unlist(sp1_city[1, ]))
sp1_city <- sp1_city[-1, ]

#special onzone month period April through September (sp2) for city
sp2_city <- aggregate(trans[3:length(trans)], list(trans$sp2), mean, na.rm = TRUE)
sp2_city <- data.frame(t(sp2_city))
colnames(sp2_city) <- as.character(unlist(sp2_city[1, ]))
sp2_city <- sp2_city[-1, ]

#Special Ozone treatment for province data
air_data <- o3_province
trans <- data.frame(t(air_data[2:length(air_data)]))
names(trans) <- air_data$id
date <- data.frame(rownames(trans))
date$year <- apply(date[1], 1, substr, 1, 4)
date$month <- apply(date[1],1, substr, 5, 6)
date$sp1 <- apply(date[3], 1, ozone58)
date$sp2 <- apply(date[3], 1, ozone49)
date$sp1 <- paste(date$year, date$sp1, sep = "")
date$sp2 <- paste(date$year, date$sp2, sep = "")
date <- date[c(-1, -2, -3)]
trans <- cbind(date, trans)

#special onzone month period May through August (sp1) for province
sp1_province <- aggregate(trans[3:length(trans)], list(trans$sp1), mean, na.rm = TRUE)
sp1_province <- data.frame(t(sp1_province))
colnames(sp1_province) <- as.character(unlist(sp1_province[1, ]))
sp1_province <- sp1_province[-1, ]

#special onzone month period April through September (sp2) for province
sp2_province <- aggregate(trans[3:length(trans)], list(trans$sp2), mean, na.rm = TRUE)
sp2_province <- data.frame(t(sp2_province))
colnames(sp2_province) <- as.character(unlist(sp2_province[1, ]))
sp2_province <- sp2_province[-1, ]

#write out all the csv
write.csv(pollutant_city_year, "pollutant_city_year.csv")
write.csv(pollutant_city_month, "pollutant_city_month.csv")
write.csv(pollutant_city_quarter, "pollutant_city_quarter.csv")
write.csv(pollutant_city_half, "pollutant_city_half.csv")
write.csv(sp1_city, "sp1_city.csv")
write.csv(sp2_city, "sp2_city.csv")
write.csv(pollutant_province_year, "pollutant_province_year.csv")
write.csv(pollutant_province_month, "pollutant_province_month.csv")
write.csv(pollutant_province_quarter, "pollutant_province_quarter.csv")
write.csv(pollutant_province_half, "pollutant_province_half.csv")
write.csv(sp1_province, "sp1_province.csv")
write.csv(sp2_province, "sp2_province.csv")
