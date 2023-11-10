library(pacman)
p_load(data.table, binsmooth, pracma, sf, dplyr)
###############################################################################
###Update state-level income distribution for the projected years & SSPs

# #Set path to access to ACS and GDP data
# setwd(paste0("C:/Users/wanh535/OneDrive - PNNL/Desktop/GODEEEP",
#              "/Income Distribution Reconciliation"))

#Read in state-level ACS 2013-2017 data
ACS <- fread("./WA_state_ACS2013-2017/nhgis0100_ds233_20175_state.csv")
WA_ACS <- ACS[ACS$STATE == "Washington", ]

#Select desired columns and then rename column names
WA_ACS <- WA_ACS[, c("AHY1E001", "AHZZE001", "AH1QE001", "AH1OE002",
                     "AH1OE003", "AH1OE004", "AH1OE005", "AH1OE006", 
                     "AH1OE007", "AH1OE008", "AH1OE009", "AH1OE010", 
                     "AH1OE011", "AH1OE012", "AH1OE013", "AH1OE014", 
                     "AH1OE015", "AH1OE016", "AH1OE017")]
colnames(WA_ACS) <- c("population", "household_number", "aggregate_income", 
                      "Income1", "Income2", "Income3", "Income4", "Income5", 
                      "Income6", "Income7", "Income8", "Income9", "Income10", 
                      "Income11", "Income12", "Income13", "Income14", "Income15", 
                      "Income16") #Income1-16 represents 16 different income 
                                  #categories in ACS.

#Set the state-level mean household size as 2.62 (calculated as the temporal 
#mean between 2011 and 2019)
hh_size <- 2.62

#Calculate state-level mean household income
mean_income <- WA_ACS$aggregate_income / WA_ACS$household_number

#Fit a spline based on 16-binned income data (using mean_income as constraint)
bCounts <- as.numeric(WA_ACS[, c(4:19)])
bEdges <- c(10000, 15000, 20000, 25000, 30000, 35000, 
            40000, 45000, 50000, 60000, 75000, 100000,
            125000, 150000, 200000)
splineFit <- splinebins(bEdges, bCounts, mean_income)
plot(splineFit$splinePDF, 0, splineFit$E)


#Caculate the decile household incomes based on the fitted income pdf.
percentiles = sb_percentiles(splineFit, p = seq(0,100,10))
decile_incomes_hhs <-rep(0, 10)
for(i in 1:10){
  decile_incomes_hhs[i] <- integral(
    function(x){splineFit$splinePDF(x)*x},percentiles[i], percentiles[i+1]
  )/integral(splineFit$splinePDF, percentiles[i], percentiles[i+1])
}

#Read in projected gdp per capita
gdp <- read.csv(paste0('./deciles income distribution/version1.1.0/', 
                       'state_decile_shares_gdp_per_capita_2011_2100.csv'))
#Extract data for Washington state
gdp <- gdp[gdp$state == "WA", ]
#Set the GDP-to-income ratio as 0.84 (calculated as the temporal mean 
#between 2011 and 2019)
gdp_to_income_ratio <- 0.84 
#Convert decile gpd per capita to decile income per household
gdp$decile_income_hhs <- gdp$decile_gdp_pc * hh_size * gdp_to_income_ratio

#Read in projected state-level population 
year <- sort(unique(gdp$year))
year <- as.character(year)
pop_ssp2 <- fread("./SSP2/SSP2/53-WA/53-WA_proj_pop.csv")
pop_ssp2 <- apply(pop_ssp2[, ..year], 2, sum)

pop_ssp3 <- fread(paste0("./SSP3/SSP3/53-WA/53-WA_proj_pop.csv"))
pop_ssp3 <- apply(pop_ssp3[, ..year], 2, sum)

pop_ssp5 <- fread(paste0("./SSP5/SSP5/53-WA/53-WA_proj_pop.csv"))
pop_ssp5 <- apply(pop_ssp5[, ..year], 2, sum)

pop_proj <- rbind(pop_ssp2, pop_ssp3)
pop_proj <- rbind(pop_proj, pop_ssp5)

###Construct small income intervals to approxiamte the income distribution and 
#then adjust the income by projected decile mean.Finally calculate the state-
#level population by 3 income bins based on the updated income distribution.

SSP <- c("SSP2", "SSP3", "SSP5")
year <- sort(unique(gdp$year))[6:22]
decile_income_est <- c()
binned_income <- as.data.frame(matrix(0, length(year) * length(SSP), 16))
colnames(binned_income) <- c("<10k", "10k-15k", "15k-20k", "20k-25k", "25k-30k",
                             "30k-35k", "35k-40k", "40k-45k", "45-50k", 
                             "50k-60k", "60k-75k", "75k-100k", "100k-125k", 
                             "125k-150k", "150-200k", ">200k")
binned_income_3 <- as.data.frame(matrix(0, length(year) * length(SSP), 3))
colnames(binned_income_3) <- c("Income1", "Income2", "Income3")

for(i in 1:3){
  for(j in 1:length(year)){
    x <- seq(0, splineFit$E, 100)
    y <- rep(0, length(x))
    for (p in 1:length(x)){
      y[p] <- integral(splineFit$splinePDF, x[p], x[p]+100)
    }
    
    gdp3 <- gdp[gdp$sce == SSP[i] & gdp$year == year[j], ]
    gdp3 <- gdp3[order(as.numeric(substr(gdp3$category, 2, 
                                         nchar(gdp3$category)))), ]
    
    x_new <- c()
    y_new <- c()
    all_ratio <- c()
    for(k in 1:10){
      ratio <- gdp3[k, 'decile_income_hhs']/decile_incomes_hhs[k]
      all_ratio <- c(all_ratio, ratio)
      ind <- which(x + 100 < percentiles[k+1])
      x_sub <- x[ind]
      x <- x[-ind]
      y_sub <- y[ind]
      y <- y[-ind]
      x_sub <- (x_sub+50) * ratio
      x_new <- c(x_new, x_sub)
      y_new <- c(y_new, y_sub)
    }
    
    pdf_new <- splinefun(x_new, y_new)
    y_new <- y_new/integral(pdf_new, 0, splineFit$E * all_ratio[10])
    pdf_new <- splinefun(x_new, y_new)
    
    y_sum <- rep(0, length(y_new))
    for (m in 1:length(y_new)){
      y_sum[m] <- sum(y_new[1:m]) / sum(y_new)
    }
    inv <- splinefun(y_sum, x_new)
    
    percentiles2 = inv(seq(0,100,10) / 100)
    percentiles2[1] = 0
    percentiles2[11] = splineFit$E * all_ratio[10]
    
    #Calculate the estimated decile means
    decile_incomes_hhs2 <-rep(0, 10)
    for(l in 1:10){
      decile_incomes_hhs2[l] <- integral(function(x){pdf_new(x)*x},
                                         percentiles2[l], percentiles2[l+1])/
        integral(pdf_new, percentiles2[l], 
                 percentiles2[l+1])
    }
    decile_income_est <- cbind(decile_income_est, decile_incomes_hhs2)
    colnames(decile_income_est)[(i-1) * length(year) + j] <- paste0(year[j],
                                                                    SSP[i]) 
    
    #Calculate the 16-binned income hhs number
    population <- pop_proj[i, j+5]
    bins <- c(0, bEdges, splineFit$E * all_ratio[10])
    for(w in 1:(length(bins)-1)){
      binned_income[(i-1)*length(year)+j, w] <- 
        round(integral(pdf_new, bins[w], bins[w+1]) * population/ hh_size)
    }
    rownames(binned_income)[(i-1) * length(year) + j] <- paste0(year[j], SSP[i])
    
    #Calculate the 3-binned income hhs number
    bins_3 <- c(0, 18150, 48396, splineFit$E * all_ratio[10])
    for(m in 1:(length(bins_3)-1)){
      binned_income_3[(i-1)*length(year)+j, m] <- 
        round(integral(pdf_new, bins_3[m], bins_3[m+1])*population/hh_size)
    }
    rownames(binned_income_3)[(i-1)*length(year) + j] <- paste0(year[j], SSP[i])
    
  }
}

decile_income_est <- as.data.frame(decile_income_est)
rownames(decile_income_est) <- c("d1", "d2",  "d3",  "d4",  "d5", "d6",  "d7", 
                                 "d8", "d9", "d10")

###############################################################################
###Downscaling income from state-level to block/block group level

# #Set path to access to LODES data
# setwd(paste0("C:/Users/wanh535/OneDrive - PNNL/Desktop/GODEEEP",
#              "/Income Distribution Reconciliation/LODES/Version 8",
#              "/RAC_job_by_income"))

#Read in LODES data for all years
first = TRUE
for(i in 1:18){
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

#Calculate the temporal-mean weights
weight <- LODES[, .(mean(CE01wp), mean(CE02wp), mean(CE03wp)), by = .(h_geocode)]
colnames(weight) <- c('h_geocode', 'CE01wp', 'CE02wp', 'CE03wp')


#Normalize the weights to make sure their sum adds up to 100%
weight$CE01wp <- weight$CE01wp/sum(weight$CE01wp)*100
weight$CE02wp <- weight$CE02wp/sum(weight$CE02wp)*100
weight$CE03wp <- weight$CE03wp/sum(weight$CE03wp)*100
weight <- as.data.frame(weight)

#Downscale
for(i in 1:nrow(binned_income_3)){
  scenario <- rownames(binned_income_3)[i]
  for (j in 1:3){
    name <- paste0(scenario, colnames(binned_income_3)[j])
    weight$v <- weight[, j+1]*binned_income_3[i, j]/100
    colnames(weight)[dim(weight)[2]] <- name
  }
}

#Create GISJOIN ID based on geocode ID
weight$GISJOIN <- paste0("G", substr(weight$h_geocode, 1, 2), 0, 
                         substr(weight$h_geocode, 3, 5), 0, 
                         substr(weight$h_geocode, 6, 15))
bk_proj <- weight[, c(158, 5:157)]

#The number of blocks in the previously-generated bk_proj is less than the
#actual total blocks in Washington because blocks of 0 population are not 
#included in the LODES dataset. These blocks need to be added to the bk_proj. 
Wa_block <- st_read("WA_block_2020.shp")
Wa_block <- left_join(Wa_block, bk_proj, by = "GISJOIN")
Wa_block <- Wa_block[, c(1, 19:172)]
Wa_block[is.na(Wa_block)] <- 0 #If the block has no LODES info, its pop is 0
bk_proj <- st_drop_geometry(Wa_block)
bk_proj[, c(2:154)] <- round(bk_proj[, c(2:154)])

#Write out projected block-level income data
fwrite(bk_proj, "bk_binned_income_proj_rounded.csv")

#Aggregate the block level projection to block group level
bk_proj$GISJOIN_BG <- substr(bk_proj$GISJOIN, 1, 15)
bk_proj <- as.data.table(bk_proj)
bk_proj <- bk_proj[, -1] #Remove GISJOIN id for blocks

bg_proj <- bk_proj[, lapply(.SD, sum), by = .(GISJOIN_BG)]
colnames(bg_proj)[1] <- "GISJOIN"
bg_proj[, c(2:154)] <- round(bg_proj[, c(2:154)])

#Write out projected block group-level income data
fwrite(bg_proj, "bg_binned_income_proj_rounded.csv")
