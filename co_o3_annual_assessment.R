#For getting the annual assessment for ozone and carbon monoxide
#Pollutants: O3, CO
#Chinese Air Quality Standard HJ663-2013
#Annual assessment for O3 is the 90th percentile of annual 8 hour running average max
#Annual assessment for CO is the 95th percentile of daily average

setwd("/Users/sherry/Desktop/china_air/data")

files <- list.files(pattern = "(china_sites_)+.*.csv")
stations <- read.csv("/Users/sherry/Desktop/china_air/match1.csv", stringsAsFactors = FALSE)

#generate city/province reference to propergate data frame from city
o3_city <- data.frame(id = unique(stations$cityid))
o3_province <- data.frame(id = unique(stations$provinceid))
co_city <- data.frame(id = unique(stations$cityid))
co_province <- data.frame(id = unique(stations$provinceid))

a = 1
for (a in 1:length(files)){
  air_data <- read.csv(files[a], stringsAsFactors = FALSE)
  date <- as.character(unique(air_data$date))
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
  
  a = a + 1
}

#function that gets the nth percentile
#NAs are excluded from the list
nthmax <- function(array, n = 90){
  place = round((100-n)/100 * length(array[!is.na(array)])) + 1
  return(sort(array[2:length(array)], decreasing = TRUE)[place])
}

#Transform data frame and get nth max from the list
raw_data <- list(o3_city, o3_province, co_city, co_province)
names <- c("o3_city", "o3_province", "co_city", "co_province")
percentile <- c(90, 90, 95, 95)
a = 1
for (a in 1:length(raw_data)){
  trans <- raw_data[[a]]
  id <- trans$id
  trans <- data.frame(t(trans[2:length(trans)]))
  names(trans) <- id
  trans[trans == 0] <- NA
  date <- substr(rownames(trans), 1, 4)
  
  #subset by year
  trans_2015 <- trans[date == "2015", ]
  trans_2016 <- trans[date == "2016", ]
  trans_2017 <- trans[date == "2017", ]
  trans_2015 <- apply(trans_2015, 2, function(x) {nthmax(x, percentile[a])})
  trans_2016 <- apply(trans_2016, 2, function(x) {nthmax(x, percentile[a])})
  trans_2017 <- apply(trans_2017, 2, function(x) {nthmax(x, percentile[a])})
  
  trans <- data.frame(t(rbind(trans_2015, trans_2016, trans_2017)))
  write.csv(trans, paste(names[a], ".csv", sep = ""))
  }
