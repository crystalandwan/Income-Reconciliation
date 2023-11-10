################################################################################
#############County-level accuracy assessment based on ACS dataset for year 2020

##Set to correct working directory
# setwd(paste0("C:/Users/wanh535/OneDrive - PNNL/Desktop/GODEEEP/",
#              "Income Distribution Reconciliation/CountyACS/"))


#Because ACS 1-year estimate data for 2020 is not available due to covid, 
#we use the mean between ACS 2019 and ACS2021 to represent ACS2020.

#Read in ACS2019
ACS2019 <- fread("nhgis0107_ds243_2019_county.csv")
ACS2019 <- ACS2019[ACS2019$STATE == "Washington", ]
income_indices <- grep("ALAJE", colnames(ACS2019))

#Aggregate 16 income categories into 3 bins (This aggregation process requires
#to split one income category into two different income bins, and this split is
#assuming that the population is uniformly distributed within each category)
ACS2019$low_income <- round(rowSums(ACS2019[, income_indices[2:3], with = FALSE]) + 
                              ACS2019[, income_indices[4], with = FALSE] * 0.63)

ACS2019$med_income <- round(ACS2019[, income_indices[4], with = FALSE] * 0.37 + 
                              rowSums(ACS2019[, income_indices[5:9], with = FALSE]) + 
                              ACS2019[, income_indices[10], with = FALSE] * 0.6792)

ACS2019$high_income <- round(ACS2019[, income_indices[10], with = FALSE] * 0.3208 + 
                               rowSums(ACS2019[, income_indices[11:17], with = FALSE]))

#Read in ACS2021
ACS2021 <- fread("nhgis0107_ds253_2021_county.csv")
ACS2021 <- ACS2021[ACS2021$STATE == "Washington", ]
income_indices2 <- grep("ANY7E", colnames(ACS2021))

ACS2021$low_income <- round(rowSums(ACS2021[, income_indices2[2:3], with = FALSE]) + 
                              ACS2021[, income_indices2[4], with = FALSE] * 0.63)

ACS2021$med_income <- round(ACS2021[, income_indices2[4], with = FALSE] * 0.37 + 
                              rowSums(ACS2021[, income_indices2[5:9], with = FALSE]) + 
                              ACS2021[, income_indices2[10], with = FALSE] * 0.6792)

ACS2021$high_income <- round(ACS2021[, income_indices2[10], with = FALSE] * 0.3208 + 
                               rowSums(ACS2021[, income_indices2[11:17], with = FALSE]))

#Use the mean of ACS2019 and ACS2021 to calculate ACS2020
ACS2020 <- merge(ACS2019[, c("GISJOIN", "low_income", "med_income", "high_income")], 
                 ACS2021[, c("GISJOIN", "low_income", "med_income", "high_income")], 
                 by = "GISJOIN")
ACS2020$low <- (ACS2020$low_income.x+ ACS2020$low_income.y) / 2
ACS2020$med <- (ACS2020$med_income.x+ ACS2020$med_income.y) / 2
ACS2020$high <- (ACS2020$high_income.x+ ACS2020$high_income.y) / 2


#Read in projected block group-level income data
bg <- fread("bg_binned_income_proj_rounded.csv")

#Aggregate to county level
bg$GISJOIN_County <- substr(bg$GISJOIN, 1, 8)
bg <- bg[, c(2:4, 155)]

County <- bg[, lapply(.SD, sum), by = .(GISJOIN_County)]


#Merge the county-level income projection data with groundtruth ACS data
validate <- merge(ACS2020[, c("GISJOIN", "low", "med", "high")],
                   County, by.x = "GISJOIN", by.y = "GISJOIN_County")
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
print(metric)
