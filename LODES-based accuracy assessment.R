library(pacman)
p_load(data.table, tibble, ggplot2, Metrics, sf, dplyr)

# #Set to working directory
# setwd(paste0("C:/Users/wanh535/OneDrive - PNNL/Desktop/GODEEEP",
#              "/Income Distribution Reconciliation/LODES/Version 8",
#              "/RAC_job_by_income"))

####Block-level accuracy assessment
#Read in LODES data between 2002 and 2019
first = TRUE
for(i in 1:19){
  target_files <- list.files(pattern = as.character(2001+i))
  data1 <- fread(target_files[1])
  data1 <- data1[, c("h_geocode", "CE01")]
  data2 <- fread(target_files[2])
  data2 <- data2[, c("h_geocode", "CE02")]
  data3 <- fread(target_files[3])
  data3 <- data3[, c("h_geocode", "CE03")]
  
  data <- merge(data1, data2, by = "h_geocode", all.x = TRUE, all.y = TRUE)
  data <- merge(data, data3, by = "h_geocode", all.x = TRUE, all.y = TRUE)
  data[is.na(data), ] <- 0
  
  data$CE01wp <- data$CE01 / sum(data$CE01)*100
  data$CE02wp <- data$CE02 /sum(data$CE02)*100
  data$CE03wp <- data$CE03 /sum(data$CE03)*100
  data$year <- as.numeric(i+2001)
  
  if(first == TRUE){
    LODES <- data
  } else{
    LODES <- rbind(LODES, data)
  }
  first = FALSE
}

LODES$h_geocode <- as.character(LODES$h_geocode)


#Calculate temporal mean block-level weight using LODES data 2002 - 2015
LODES2 <- LODES[LODES$year < 2016, ]
weight <- LODES2[, .(mean(CE01wp), mean(CE02wp), mean(CE03wp)), by = .(h_geocode)]
colnames(weight) <- c('h_geocode', 'CE01wp', 'CE02wp', 'CE03wp')

#Normalize the weights to make sure their sum adds up to 100%
weight$CE01wp <- weight$CE01wp/sum(weight$CE01wp)*100
weight$CE02wp <- weight$CE02wp/sum(weight$CE02wp)*100
weight$CE03wp <- weight$CE03wp/sum(weight$CE03wp)*100

#Aggregate block-level LODES to state-level.
for(validation_year in c(2016, 2017, 2018, 2019)){
  lode <- LODES[LODES$year == validation_year, ]
  state_ce01 <- sum(lode$CE01)
  state_ce02 <-sum(lode$CE02)
  state_ce03 <- sum(lode$CE03)
  
  #Calculate downscaled block-level population by 3 income bins
  weight$pop_ce01 <- round(weight$CE01wp * state_ce01/100)
  weight$pop_ce02 <- round(weight$CE02wp * state_ce02/100)
  weight$pop_ce03 <- round(weight$CE03wp * state_ce03/100)
  
  #Compare downscaled income data with actual LODES data
  validate <- merge(lode[, c("h_geocode", "CE01", "CE02", "CE03")], 
                    weight[, c("h_geocode", "pop_ce01", "pop_ce02", 
                               "pop_ce03")], by = "h_geocode") 
  
  validate <- as.data.frame(validate)
  metric <- matrix(0, 3, 4)
  for(i in 1:3){
    metric[i, 1] <- cor(validate[, i+1], validate[, i+4])^2
    APE <- abs(validate[, i+1] - validate[, i+4])/validate[, i+1]*100
    APE[is.infinite(APE)] <- NA
    metric[i, 2] <- round(median(APE, na.rm = T))
    
    PPI <- sum(abs(validate[, i+1] - validate[, i+4]))/ 2
    metric[i, 3] <- round(PPI)
    metric[i, 4] <- round(PPI/sum(validate[i+1]) * 100, digits = 2)
  }
  
  colnames(metric) <- c("R-squared","MdAPE%", "PPI", "PPPI%")
  rownames(metric) <- c("income1", "income2", "income3")
  print(validation_year)
  print(metric)
}

####Block group-level accuracy assessment
#Aggregate weight to block group level
weight <- weight[, c(1:4)]
weight$h_geocode_bg <- substr(weight$h_geocode, 1, 12)
weight <- weight[, c(2:5)]
weight_bg <- weight[, lapply(.SD, sum), by = .(h_geocode_bg)]

#Aggregate lode data to block group level for comparison
for(validation_year in c(2016, 2017, 2018, 2019)){
  lode <- LODES[LODES$year == validation_year, ]
  lode$h_geocode_bg <- substr(lode$h_geocode, 1, 12)
  lode <- lode[, c("h_geocode_bg", "CE01", "CE02", "CE03")]
  lode <- as.data.table(lode)
  lode_bg <- lode[, lapply(.SD, sum), by = .(h_geocode_bg)]
  
  state_ce01 <- sum(lode_bg$CE01)
  state_ce02 <-sum(lode_bg$CE02)
  state_ce03 <- sum(lode_bg$CE03)
  
  weight_bg$pop_ce01 <- round(weight_bg$CE01wp * state_ce01/100)
  weight_bg$pop_ce02 <- round(weight_bg$CE02wp * state_ce02/100)
  weight_bg$pop_ce03 <- round(weight_bg$CE03wp * state_ce03/100)
  
  validate2 <- merge(lode_bg[, c("h_geocode_bg", "CE01", "CE02", "CE03")],
                     weight_bg[, c("h_geocode_bg", "pop_ce01", "pop_ce02",
                                   "pop_ce03")], by = "h_geocode_bg")
  
  validate2 <- as.data.frame(validate2)
  
  metric2 <- matrix(0, 3, 4)
  
  for(i in 1:3){
    metric2[i, 1] <- cor(validate2[, i+1], validate2[, i+4])^2
    APE <- abs(validate2[, i+1] - validate2[, i+4])/validate2[, i+1]*100
    APE[is.infinite(APE)] <- NA
    metric2[i, 2] <- round(median(APE, na.rm = T))
    
    PPI <- sum(abs(validate2[, i+1] - validate2[, i+4]))/ 2
    metric2[i, 3] <- round(PPI)
    metric2[i, 4] <- round(PPI/sum(validate2[i+1]) * 100, digits = 2)
  }
  
  colnames(metric2) <- c("R-squared","MdAPE%", "PPI", "PPPI%")
  rownames(metric2) <- c("income1", "income2", "income3")
  print(validation_year)
  print(metric2)
}
