#For processing air pollution data downloaded from MEE.
#Pollutant: PM2.5, PM10, SO2, NO2, CO, O3_8h
#Chinese Air Quality Standard HJ663-2013
#Outputs are daily assessments of pollutants
#Site data download from http://beijingair.sinaapp.com/
#Sort data in folder by date

setwd("/Users/sherry/Desktop/china_air/data")
files <- list.files(pattern = "(china_sites_)+.*.csv")
stations <- read.csv("/Users/sherry/Desktop/china_air/match1.csv", stringsAsFactors = FALSE)

#PM2.5, PM10, SO2, NO2, CO are assessed on daily mean
#O3 is assessed on max of daily 8 hour moving average
pollutants <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")

#create pollutant list to propogate data
b = 1
for (b in 1:length(pollutants)){
  assign(paste(pollutants[b], "city", sep = "_"), data.frame(id = unique(stations$cityid)))
  assign(paste(pollutants[b], "province", sep = "_"), data.frame(id = unique(stations$provinceid)))
}

#Generate pollutant daily assessment by city and by province
a = 1
for (a in 1:10){
  air_data <- read.csv(files[a], stringsAsFactors = FALSE)
  date <- as.character(unique(air_data$date))
  for (b in 1:(length(pollutants)-1)){
    pollution <- air_data[air_data$type == pollutants[b], ]
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
    temp_city <- eval(parse(text = paste(pollutants[b], "city", sep = "_")))
    temp_province <- eval(parse(text = paste(pollutants[b], "province", sep = "_")))
    assign(paste(pollutants[b], "city", sep = "_"), 
           merge(temp_city, pollution_city, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE))
    assign(paste(pollutants[b], "provincetemp", sep = "_"), 
           merge(temp_province, pollution_province, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE))
    b = b+1
  }
  #Evlaute O3_8h max
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
  O3_city <- merge(O3_city, pollution_city, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  O3_province <- merge(O3_province, pollution_province, by.x = "id", by.y = "Group.1", all.x = TRUE, all.y = TRUE)
  a = a+1
}

#Output for plotting running average
b = 1
for (b in 1:length(pollutants)){
  temp_city <- eval(parse(text = paste(pollutants[b], "city", sep = "_")))
  rownames(temp_city) <- temp_city$id
  temp_city <- data.frame(t(temp_city[-1]))
  temp_province <- eval(parse(text = paste(pollutants[b], "province", sep = "_")))
  rownames(temp_province) <- temp_province$id
  temp_province <- data.frame(t(temp_province[-1]))
  write.csv(temp_city,
            paste(pollutants[b], "_city_daily.csv", sep = ""))
  write.csv(temp_province,
            paste(pollutants[b], "_province_daily.csv", sep = ""))
  
}
