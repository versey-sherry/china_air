#For processing air pollution data downloaded from MEE.
#Pollutant: PM2.5, PM10, SO2, NO2, CO, O3_8h
#Chinese Air Quality Standard HJ663-2013
#Outputs are yearly, quarterly, semiannually, and other special periodic assessement
#Run daily_assessment.R first to get daily assessment

setwd("/Users/sherry/Desktop/china_air/data")

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

files <- list.files(pattern = ".*(_daily)+.csv")
a = 1
for (a in 1:length(files)){
  assign(gsub("_daily.csv", "", files[a]), read.csv(files[a]))
  a = a+1
}
stations <- read.csv("/Users/sherry/Desktop/china_air/match1.csv", stringsAsFactors = FALSE)
pollutants <- c("PM2.5_city", "PM10_city", "SO2_city", "NO2_city", "CO_city", "O3_city")

all_pollutant_city <- list(PM2.5_city, PM10_city, SO2_city, NO2_city, CO_city, O3_city)
all_pollutant_province <- list(PM2.5_province, PM10_province, SO2_province, NO2_province, CO_province, O3_province)
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
  trans <- all_pollutant_city[[a]]
  date <- data.frame(trans$X)
  trans <- trans[-1]
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
  date <- paste(year$Group.1, pollutants[a], sep = "_")
  year <- year[-1]
  year <- data.frame(t(year))
  colnames(year) <- date
  year$id <- rownames(year)
  pollutant_city_year <- merge(pollutant_city_year, year, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  
  #month
  month <- aggregate(trans[5:length(trans)], list(trans$month), mean, na.rm = TRUE)
  date <- paste(month$Group.1, pollutants[a], sep = "_")
  month <- month[-1]
  month <- data.frame(t(month))
  colnames(month) <- date
  month$id <- rownames(month)
  pollutant_city_month <- merge(pollutant_city_month, month, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  
  #quarter
  quarter <- aggregate(trans[5:length(trans)], list(trans$quarter), mean, na.rm = TRUE)
  date <- paste(quarter$Group.1, pollutants[a], sep = "_")
  quarter <- quarter[-1]
  quarter <- data.frame(t(quarter))
  colnames(quarter) <- date
  quarter$id <- rownames(quarter)
  pollutant_city_quarter <- merge(pollutant_city_quarter, quarter, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  
  #half
  half <- aggregate(trans[5:length(trans)], list(trans$half), mean, na.rm = TRUE)
  date <- paste(half$Group.1, pollutants[a], sep = "_")
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
  date <- paste(year$Group.1, pollutants[a], sep = "_")
  year <- year[-1]
  year <- data.frame(t(year))
  colnames(year) <- date
  year$id <- rownames(year)
  pollutant_province_year <- merge(pollutant_province_year, year, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  
  #month
  month <- aggregate(trans[5:length(trans)], list(trans$month), mean, na.rm = TRUE)
  date <- paste(month$Group.1, pollutants[a], sep = "_")
  month <- month[-1]
  month <- data.frame(t(month))
  colnames(month) <- date
  month$id <- rownames(month)
  pollutant_province_month <- merge(pollutant_province_month, month, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  
  #quarter
  quarter <- aggregate(trans[5:length(trans)], list(trans$quarter), mean, na.rm = TRUE)
  date <- paste(quarter$Group.1, pollutants[a], sep = "_")
  quarter <- quarter[-1]
  quarter <- data.frame(t(quarter))
  colnames(quarter) <- date
  quarter$id <- rownames(quarter)
  pollutant_province_quarter <- merge(pollutant_province_quarter, quarter, by.x ="id", by.y = "id", all.x = TRUE, all.y = TRUE)
  
  #half
  half <- aggregate(trans[5:length(trans)], list(trans$half), mean, na.rm = TRUE)
  date <- paste(half$Group.1, pollutants[a], sep = "_")
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
