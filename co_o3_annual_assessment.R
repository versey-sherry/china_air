#For getting the annual assessment for ozone and carbon monoxide
#Pollutants: O3, CO
#Chinese Air Quality Standard HJ663-2013
#Outputs are annual assessment for O3 and CO
#Annual assessment for O3 is the 90th percentile of annual 8 hour running average max
#Annual assessment for CO is the 95th percentile of daily average
#Run daily_assessment.R first to get daily assessment

setwd("/Users/sherry/Desktop/china_air/data")

files <- c(list.files(pattern = "(CO)+.*.csv"), list.files(pattern = "(O3)+.*.csv"))
a = 1
for (a in 1:length(files)){
  assign(gsub("_daily.csv", "", files[a]), read.csv(files[a]))
  a = a+1
}

#function that gets the nth percentile
#NAs are excluded from the list
nthmax <- function(array, n = 90){
  place = round((100-n)/100 * length(array[!is.na(array)])) + 1
  return(sort(array[2:length(array)], decreasing = TRUE)[place])
}

#Transform data frame and get nth max from the list
raw_data <- list(O3_city, O3_province, CO_city, CO_province)
names <- c("O3_city", "O3_province", "CO_city", "CO_province")
percentile <- c(90, 90, 95, 95)
a = 1
for (a in 1:length(raw_data)){
  trans <- raw_data[[a]]
  trans[trans == 0] <- NA
  date <- substr(trans$X, 1, 4)
  trans <- trans[-1]
  years <- unique(date)
  annual <- data.frame()
  #Calculate annual assessment
  b = 1
  for (b in 1:length(years)){
    temp <- trans[date == years[b], ]
    temp <- apply(temp, 2, function(x) {nthmax(x, percentile[a])})
    annual <- rbind(annual, temp)
    b = b+1
  }
  names(annual) <- names(trans)
  annual <- data.frame(t(annual))
  write.csv(annual, paste(names[a], ".csv", sep = ""))
  }
