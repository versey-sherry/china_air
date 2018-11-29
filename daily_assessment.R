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
for (a in 1:length(files)){
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
    assign(paste(pollutants[b], "province", sep = "_"), 
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

#Output daily assessment
a = 1
for (a in 1:length(pollutants)){
  temp_city <- eval(parse(text = paste(pollutants[a], "city", sep = "_")))
  rownames(temp_city) <- temp_city$id
  temp_city <- data.frame(t(temp_city[-1]))
  assign(paste(pollutants[a], "city", sep = "_"), temp_city)
  temp_province <- eval(parse(text = paste(pollutants[a], "province", sep = "_")))
  rownames(temp_province) <- temp_province$id
  temp_province <- data.frame(t(temp_province[-1]))
  assign(paste(pollutants[a], "province", sep = "_"), temp_province)
  write.csv(temp_city,
            paste(pollutants[a], "_city_daily.csv", sep = ""))
  write.csv(temp_province,
            paste(pollutants[a], "_province_daily.csv", sep = ""))
  
}

#Calculate daily mean according to policy analytic scopes
#Set up policy analytic scope
#2+26 Jingjinji 大气十条
g_jjj <- c("邯郸", "保定", "安阳", "石家庄", "邢台", "衡水", "焦作",
           "聊城", "郑州", "菏泽", "濮阳", "德州", "开封", "滨州",
           "唐山", "新乡", "沧州", "淄博", "济南", "鹤壁", "太原",
           "晋城", "天津", "阳泉", "长治", "廊坊", "济宁", "北京")
prd <- c("深圳", "惠州", "珠海", "中山", "广州" ,"江门", "东莞", "肇庆", "佛山")
yrd <- c("宿州", "阜阳", "蚌埠", "滁州", "亳州", "淮北", "合肥", "安庆",
         "芜湖", "淮南", "宣城", "铜陵", "马鞍山", "六安", "池州", "黄山", 
         "徐州", "宿迁", "泰州", "淮安", "常州", "无锡", "扬州", "镇江", 
         "南京", "连云港", "苏州", "南通", "盐城", "上海", "杭州", "金华",
         "湖州", "绍兴", "嘉兴", "衢州", "温州", "宁波", "台州", "丽水", 
         "舟山")
fenwei <- c("铜川", "吕梁", "宝鸡", "三门峡", "晋中", "渭南",
            "运城", "西安", "洛阳", "咸阳", "临汾")
imp_city <- c("邯郸", "保定", "石家庄", "邢台", "衡水", "乌鲁木齐", "西安",
              "郑州", "徐州", "唐山", "沧州", "济南", "太原", "天津",
              "廊坊", "宿迁", "哈尔滨", "北京", "合肥", "镇江", "扬州",
              "成都", "长沙", "武汉", "泰州", "淮安", "沈阳", "常州",
              "兰州", "长春", "银川", "连云港", "无锡", "绍兴", "盐城",
              "重庆", "杭州", "秦皇岛", "呼和浩特", "苏州", "湖州", "金华",
              "衢州", "嘉兴", "南京", "南昌", "佛山", "肇庆", "西宁",
              "南通", "上海", "青岛", "温州", "东莞", "江门", "宁波",
              "南宁", "广州", "大连", "承德", "中山", "丽水", "台州",
              "张家口", "贵阳", "珠海", "惠州", "深圳", "昆明", "厦门",
              "福州", "舟山", "海口", "拉萨")
a = 1
for (a in 1:length(pollutants)){
  daily_data <- eval(parse(text = paste(pollutants[a], "city", sep = "_")))
  temp_city <- data.frame(t(rbind(apply(daily_data[g_jjj], 1, mean, na.rm = TRUE),
                     apply(daily_data[prd], 1, mean, na.rm = TRUE),
                     apply(daily_data[yrd], 1, mean, na.rm = TRUE),
                     apply(daily_data[fenwei], 1, mean, na.rm = TRUE),
                     apply(daily_data[imp_city], 1, mean, na.rm = TRUE),
                     apply(daily_data, 1, mean, na.rm = TRUE))))
  names(temp_city) <- c("JJJ", "PRD", "YRD", "Fenwei", "Imp_city", "National")
  write.csv(temp_city, paste(pollutants[a], "_policy_daily.csv", sep = ""))
}


