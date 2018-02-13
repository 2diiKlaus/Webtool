rm(list=ls())

#Load packages
library(grid)
library(plyr)
library(reshape2)
library(gridExtra)
library(scales)
library(stringr)
library(ggplot2)
library(png)
library(tidyr)
library(zoo)

# Version - Control

# --- DATE ---|--- Editor ---| --- Version Name --- | --- Edits / Adds / Changes / Bugfixes ---
#  2017-11-14 |      KH      |           0          | Adjust Batch-code for single files and simplify the code - target will be to enable it to work as a function / stand-alone tool that can be used by other companies and for the website

# get location of the code (code and portfolio data should be in the same folder in this case)
#Location <- dirname(rstudioapi::getActiveDocumentContext()$path)
Location <- getwd()
Location <- paste0(Location, "/")
Location <- gsub("2°","2�",Location)

# - - - - -
# I) Set workdrive ----
# - - - - -
setwd(Location)

# - - - - -
# II) Read in global functions ----
# - - - - -
source(paste0(Location,"GlobalPortCheckFunctions.R"))

# - - - - -
# III) Read parameter file----
# - - - - -
ParameterFile <- read.csv(paste0(Location,"Investor_ParameterFile.csv"), stringsAsFactors = FALSE, strip.white = TRUE)
Startyear <- as.numeric(format(Sys.time(), "%Y"))

# # - - - - -
# # IV) initiate log file----
# # - - - - -
# initiateLogFile()

# - - - - -
# |------------------------------|
# 1) Data Loading------
# |------------------------------|
# a) Read in Asset Level Data / MasterData
# b) Read in Asset level Data Bridge
# c) Read in financial data (Data retrieved from BBG PORT function)
# d) Read in regions data
# e) Read in market data
# f) Read in Scenario data
# g) Read in all lists used
# h) Read in Portfolio data
# - - - - -

# - - - - -
# 1a) Read in asset level data----
# - - - - -
MasterData <- readRDS(paste0(Location,"MasterData.Rda"))

#Trim Masterdata to startyear
MasterData<-subset(MasterData, MasterData$Year >= Startyear)

# - - - - -
# 1b) Read in Asset level Data Bridge----
# - - - - -
ALDBridge <- readRDS(paste0(Location,"ALDEquityBridge.Rda"))
EquityBridge <- readRDS(paste0(Location,"EquityBridge.Rda"))


# - - - - -
# 1c) Read in financial data (Data retrieved from BBG PORT function)----
# - - - - -
BBGPORTOutput <- readRDS(paste0(Location,"FinancialData.Rda"))

# - - - - -
# 1d) Read in regions data ----
# - - - - -
# Create a List of all existing Benchmark-Region and all assessed CompanyLocation-Regions
BenchRegionLists <- readRDS(paste0(Location,"BenchRegions.Rda"))
BenchRegionLists[is.na(BenchRegionLists)] <- ""
BenchRegionLists <- plyr::rename(BenchRegionLists, c("BenchRegions" = "BenchmarkRegions", "BenchRegions_ISO_colnames" = "BenchmarkRegions_ISO_colnames"))
BenchmarkRegionList <- data.frame(BenchmarkRegion = BenchRegionLists$BenchmarkRegions[!is.na(BenchRegionLists$BenchmarkRegions) & BenchRegionLists$BenchmarkRegions != ""], BenchmarkRegionColname = BenchRegionLists$BenchmarkRegions_ISO_colnames[!is.na(BenchRegionLists$BenchmarkRegions_ISO_colnames) & BenchRegionLists$BenchmarkRegions_ISO_colnames != ""])
CompanyDomicileRegion <- readRDS(paste0(Location,"IndexRegions.Rda"))
CompanyDomicileRegion <- plyr::rename(CompanyDomicileRegion, c("IndexUniverse" = "CompanyDomicileRegion", "IndexUniverseColname" = "CompanyDomicileRegionColname")) 
CompanyDomicileRegionList <- data.frame(CompanyDomicileRegion = CompanyDomicileRegion$CompanyDomicileRegion[!is.na(CompanyDomicileRegion$CompanyDomicileRegion) & CompanyDomicileRegion$CompanyDomicileRegion != ""], CompanyDomicileRegionColname = CompanyDomicileRegion$CompanyDomicileRegionColname[!is.na(CompanyDomicileRegion$CompanyDomicileRegionColname) & CompanyDomicileRegion$CompanyDomicileRegionColname != ""])

# Read countryname-conversion file to abbreviation
CountryISOList <- readRDS(paste0(Location, "CountryISOCodes.Rda"))
CountryISOList <- subset(CountryISOList, CountryISOList$COUNTRY != "#N/A")
CountryISOList$GDPlantLocation<-as.character(CountryISOList$GDPlantLocation)


# - - - - -
# 1e) Read in market production data (for starting point calculation of the benchmark) & market size data----
# - - - - -
# Reference Production of the market
MarketRef <- readRDS(paste0(Location,"MarketReference.Rda"))
#Calculate technology share in the the starting year (This will be scaled to portfolio size later on to calculate the starting point of the 2°C benchmark)
MarketRef <- subset(MarketRef, Year == Startyear, select = c("BenchmarkRegion","Sector","Technology", "CompanyDomicileRegion", "RefProdMarketTech", "RefProdMarketSector" )) #CompanyDomicileRegion == EvalRegion,
MarketRef$MarketTechShare[MarketRef$Sector == "Fossil Fuels"] <- 1
MarketRef$MarketTechShare[MarketRef$Sector != "Fossil Fuels"] <- MarketRef$RefProdMarketTech[MarketRef$Sector != "Fossil Fuels"] / MarketRef$RefProdMarketSector[MarketRef$Sector != "Fossil Fuels"]
MarketRef <- subset(MarketRef)
#MarketRef <- rename(MarketRef, c("Production" = "RefMarketProduction"))


# Get market size information ($USD) for Listed markets
MarketSizeData <- readRDS(paste0(Location,"ListedMarketSizeAUM.Rda"))

# - - - - -
# 1f) Read in scenario data----
# - - - - -
IEATargets <-  readRDS(paste0(Location,"IEATargets2016_AllRegions.Rda"))
IEATargetssub <- subset(IEATargets, Year <= (Startyear + 10)) 

# - - - - -
# 1g) Read in all input lists such as technology-list, sector-list, benchmark-region list, etc.----
# - - - - -
AllLists <- readRDS(paste0(Location,"ListInput.Rda"))


# - - - - -
# 1h) Read in portfolio data----
# - - - - -
Portfolio <- readRDS(paste0(Location,"Portfolio.Rda"))

InvestorName <- ParameterFile$InvestorName
PortfolioName <- ParameterFile$PortfolioName
# - - - - -
# |------------------------------|
# 2) Portfolio Analysis -----
# |------------------------------|
# - - - - -
Portfolio <- merge(Portfolio,unique(subset(BBGPORTOutput, select = c("ISIN","CNTRY_OF_DOMICILE"))), by = "ISIN", all.x = TRUE)
Portfolio <- subset(Portfolio, select = c("EQY_FUND_TICKER" , "Position", "Subgroup" , "ICB.Subsector.Name", "Ticker", "ISIN", "SharePrice", "CNTRY_OF_DOMICILE"))

#Sum over same ISIN in one Brand
ISINCount <- as.data.frame(table(Portfolio$ISIN))

if (dim(Portfolio)[1]>0){
Portfolio <- aggregate(Portfolio["Position"], by = Portfolio[, c("EQY_FUND_TICKER" , "Subgroup" , "ICB.Subsector.Name", "Ticker", "ISIN", "SharePrice", "CNTRY_OF_DOMICILE")], FUN=sum)
}

#Calculate assets under management and total number of shares if there is no given toal number of shares
#Clean price list
Portfolio$SharePrice[Portfolio$SharePrice == "#N/A N/A"] <- 0

##if there is no price information or if the asset is outside fo the region
Portfolio$SharePrice <- as.numeric(Portfolio$SharePrice)
Portfolio$Position <- as.numeric(Portfolio$Position)

if (sum(Portfolio$Position[Portfolio$SharePrice != 0 & !is.na(Portfolio$SharePrice)], na.rm = TRUE) != 0) {
  # Subset the portfolio by securities that cannot be assessed, i.e. with missing price or country information
  PortMissingInfo <- subset(Portfolio, SharePrice == "#N/A N/A" | CNTRY_OF_DOMICILE == "#N/A Invalid Security")
  Portfolio <-  subset(Portfolio, SharePrice != "#N/A N/A" & CNTRY_OF_DOMICILE != "#N/A Invalid Security")
  Portfolio$AUM <- Portfolio$Position * Portfolio$SharePrice
  PortAUM <- sum(Portfolio$AUM, na.rm = TRUE)
  Portfolio$PortWeight <- Portfolio$AUM / PortAUM
  
  # Introduce regional split up of Portfolio AUM
  AUMmixInput <-     ddply(Portfolio,.(CNTRY_OF_DOMICILE), summarize, AUM = sum(AUM, na.rm = TRUE))
  for (f in 1:length(CompanyDomicileRegionList$CompanyDomicileRegion)) {
    AUMsub <-
      data.frame(AUM = sum(AUMmixInput$AUM[AUMmixInput$CNTRY_OF_DOMICILE %in% CompanyDomicileRegion[, names(CompanyDomicileRegion) == CompanyDomicileRegion$CompanyDomicileRegionColname[f]]]))
    AUMsub$Region <-
      CompanyDomicileRegionList$CompanyDomicileRegion[f]
    if (exists("AUMmixOutput") == FALSE) {
      AUMmixOutput <- AUMsub
    } else{
      AUMmixOutput <- rbind(AUMmixOutput, AUMsub)
    }
  }
  AUMmix <- AUMmixOutput
  rm(AUMsub, AUMmixOutput)
  
  # Add sector to the Portfolio
  SectorProduction <- unique(subset(MasterData, select = c("EQY_FUND_TICKER","Sector")))
  Portfolio <- merge(Portfolio, SectorProduction, by = "EQY_FUND_TICKER", all.x = TRUE, all.y = FALSE)
  
  #Meta-Analysis for piechart & Moodys Risk map
  # Sector exposure merging
  Portfolio$piesector <- "Not Assessed" #label non-benchmarked sectors
  Portfolio$piesector[Portfolio$ICB.Subsector.Name %in% AllLists$OilGasICB] <- "Fossil Fuels"
  Portfolio$piesector[Portfolio$ICB.Subsector.Name %in% AllLists$AutoICB] <- "Automotive"
  Portfolio$piesector[which(Portfolio$ICB.Subsector.Name %in% AllLists$UtilitiesICB & Portfolio$Sector %in% "Power")] <- "Utility Power"
  Portfolio$piesector[which(!Portfolio$ICB.Subsector.Name %in% AllLists$UtilitiesICB & Portfolio$Sector %in% "Power")] <- "NonUtility Power"
  
  # set weighting of companies that are duplicates (ff or auto and non-utility) to 0 for the non-utility part
  temp <- subset(as.data.frame(table(Portfolio$ISIN)),Freq > 1)
  temp2 <- subset(Portfolio, ISIN %in% temp$Var1 & piesector %in% c("NonUtility Power","Not Assessed"))
  temp2 <- subset(as.data.frame(table(temp2$ISIN)),Freq > 1)
  if(dim(temp)[1]>0){
    Portfolio$PortWeight[Portfolio$piesector != Portfolio$Sector & Portfolio$ISIN %in% temp$Var1 & !(Portfolio$piesector == "Utility Power" & Portfolio$Sector == "Power") & !(Portfolio$piesector == "NonUtility Power" & Portfolio$ISIN %in% temp2$Var1)] <- 0
    Portfolio$AUM[Portfolio$piesector != Portfolio$Sector & Portfolio$ISIN %in% temp$Var1 & !(Portfolio$piesector == "Utility Power" & Portfolio$Sector == "Power") & !(Portfolio$piesector == "NonUtility Power" & Portfolio$ISIN %in% temp2$Var1)] <- 0
    Portfolio$Position[Portfolio$piesector != Portfolio$Sector & Portfolio$ISIN %in% temp$Var1 & !(Portfolio$piesector == "Utility Power" & Portfolio$Sector == "Power") & !(Portfolio$piesector == "NonUtility Power" & Portfolio$ISIN %in% temp2$Var1)] <- 0
    }
  
  Portfolio$piesector[Portfolio$ICB.Subsector.Name %in% AllLists$FuturesecsICB] <- Portfolio$ICB.Subsector.Name[Portfolio$ICB.Subsector.Name %in% AllLists$FuturesecsICB]
  
  PortfolioSub <- aggregate(Portfolio["Position"], by = Portfolio[,c("EQY_FUND_TICKER","CNTRY_OF_DOMICILE")], FUN = sum, na.rm = TRUE)
    # ddply(Portfolio,.(EQY_FUND_TICKER,CNTRY_OF_DOMICILE),summarize, Position = sum(Position))
  
  # Merge with Asset level Data
  ReducedList <- merge(MasterData, PortfolioSub, by.x = c("EQY_FUND_TICKER","CNTRY_OF_DOMICILE"), by.y = c("EQY_FUND_TICKER","CNTRY_OF_DOMICILE"), all.x=FALSE, all.y=FALSE)
  
  # Calculate portfolio production
  if(dim(ReducedList)[1]>0){
    ReducedList$ShareProduction[is.na(ReducedList$ShareProduction)] <- 0
    ReducedList$Position<-as.numeric(ReducedList$Position)
    # Adjust number of shares if it's an American Depository Share.
    ReducedList$Position[!ReducedList$ADR_ADR_PER_SH == "#N/A Field Not Applicable"]<-ReducedList$Position[!ReducedList$ADR_ADR_PER_SH == "#N/A Field Not Applicable"]/as.numeric(ReducedList$ADR_ADR_PER_SH[!ReducedList$ADR_ADR_PER_SH == "#N/A Field Not Applicable"])
    # Calculate portfolio production
    ReducedList$Production = as.numeric(ReducedList$ShareProduction) * ReducedList$Position #if it's a portfolio, production is from the total number of owned shares, if and index or market it from the total free floating shares
    # Minimise data frame size by restricting results to only a 10 year forcast
    ReducedList <- subset (ReducedList, Year <= (Startyear + 10))
    ReducedList <- merge(ReducedList, CountryISOList, by.x = "PlantLocation", by.y = "GDPlantLocation", all.x = TRUE)
    ReducedList <- plyr::rename(ReducedList, c("COUNTRY_ISO" = "PlantLocation_ISO"))
    # If there is no plant location for the fossil fuel production the production is considered to have come the country of doimicle of the owner
    ReducedList$PlantLocation_ISO[is.na(ReducedList$PlantLocation_ISO) & ReducedList$Sector == "Fossil Fuels"] <-ReducedList$CNTRY_OF_DOMICILE[is.na(ReducedList$PlantLocation_ISO) & ReducedList$Sector == "Fossil Fuels"]
    
    Portfoliomix <- aggregate(ReducedList["Production"], by=ReducedList[,c("Sector","Technology", "Year", "PlantLocation_ISO", "CNTRY_OF_DOMICILE")], FUN=sum)
    
    
    for(k in 1:length(CompanyDomicileRegionList$CompanyDomicileRegion)){
      # print(k)
      PortfoliomixSub <- subset(Portfoliomix, CNTRY_OF_DOMICILE %in% CompanyDomicileRegion[,names(CompanyDomicileRegion) == CompanyDomicileRegionList$CompanyDomicileRegionColname[k]])
      if(dim(PortfoliomixSub)[1] > 0) {
        for (j in 1:length(BenchmarkRegionList$BenchmarkRegion)){
          # print(j)
          PortfolioMixBM <- subset(PortfoliomixSub, PlantLocation_ISO %in% BenchRegionLists[,names(BenchRegionLists) == BenchmarkRegionList$BenchmarkRegionColname[j]])
          
          if(dim(PortfolioMixBM)[1] > 0) {
            PortfolioMixBM$BenchmarkRegion <- BenchmarkRegionList$BenchmarkRegion[j]
            if(exists("PortfolioData") == FALSE){
              PortfolioData <- PortfolioMixBM
              rm(PortfolioMixBM)
            }else{
              PortfolioData <- rbind(PortfolioData, PortfolioMixBM)
              rm(PortfolioMixBM)
            }
          }
        }
        PortfolioData$CompanyDomicileRegion <- CompanyDomicileRegionList$CompanyDomicileRegion[k]
        if(exists("PortfolioDataAll") == FALSE){
          PortfolioDataAll <- PortfolioData
          rm(PortfolioData)
        }else{
          PortfolioDataAll <- rbind(PortfolioDataAll, PortfolioData)
          rm(PortfolioData)
        }
      }
    }
    
    Portfoliomix <- aggregate(PortfolioDataAll["Production"], by=PortfolioDataAll[,c("Sector","Technology", "Year", "BenchmarkRegion", "CompanyDomicileRegion")], FUN=sum)
    rm(PortfolioDataAll)
    Portmix <- Portfoliomix
    Portmix <- datacompletion(Portmix)
    
  }
}

if(sum(Portfolio$Position[Portfolio$SharePrice != 0 & !is.na(Portfolio$SharePrice)] & dim(ReducedList)[1]>0 , na.rm = TRUE) != 0 ){
  
  # Calculate the reference values (for the technology as well as for the sector in the start year/initial year) and merge it with the portfolio production mix data
  Sectorref <- ddply(subset(Portmix, Year == Startyear & Sector %in% c("Automotive","Power")),.(BenchmarkRegion, CompanyDomicileRegion,Sector),summarize,RefSectorProd = sum(Production,na.rm=TRUE))
  techlist2 <- unique(subset(Portmix, Year == Startyear & Sector %in% c("Automotive","Power"), select = c("Sector","Technology")))
  Sectorref <- merge(Sectorref,techlist2, by = "Sector", all.x = TRUE, all.y = TRUE)
  Portmix <- merge(Portmix,Sectorref, by = c("BenchmarkRegion", "CompanyDomicileRegion", "Sector", "Technology"), all.x=TRUE, all.y=FALSE)
  RefTechProd <- subset(Portmix, Year == Startyear, select = c("BenchmarkRegion", "CompanyDomicileRegion", "Sector", "Technology","Production"))
  RefTechProd <- plyr::rename(RefTechProd, c("Production" = "RefTechProd"))
  Portmix <- merge(Portmix,RefTechProd, by = c("BenchmarkRegion", "CompanyDomicileRegion", "Sector", "Technology"), all.x=TRUE, all.y=FALSE)
  Portmix$RefSectorProd[Portmix$Sector == "Fossil Fuels"] <- Portmix$RefTechProd[Portmix$Sector == "Fossil Fuels"]
  
  Combin <- merge(Portmix,MarketRef, by = c("BenchmarkRegion", "CompanyDomicileRegion","Sector", "Technology"), all = TRUE)
  
  # Calculate scaled reference production
  Combin$RefMarketScaledProd <- Combin$RefSectorProd * Combin$MarketTechShare
  
  for (l in 1:length(CompanyDomicileRegionList$CompanyDomicileRegion)){
    Combin$PortAUM[Combin$CompanyDomicileRegion == CompanyDomicileRegionList$CompanyDomicileRegion[l]] <- AUMmix$AUM[AUMmix$Region == CompanyDomicileRegionList$CompanyDomicileRegion[l]]
  }
  
  #Merge with IEA targets
  Combin <- merge(Combin,IEATargetssub, by = c("BenchmarkRegion","Sector","Technology","Year"), all.x = TRUE)
  Combin <- subset(Combin, !is.na(Direction)) 
  
  ### Calculate benchmark production values
  ## Relative to the market and including current assets and future plans 
  Combin$TargetProductionAlignment <- Combin$RefMarketScaledProd + Combin$RefSectorProd * Combin$FairSharePerc
  Combin$TargetProductionAlignment[Combin$Direction == "declining"] <- Combin$RefMarketScaledProd[Combin$Direction == "declining"] * (1+Combin$FairSharePerc[Combin$Direction == "declining"])
  
  ## AUM approach, Get market AUM for each CompanyDomicileRegion (which is each investment universe)
  for (l in 1:length(CompanyDomicileRegionList$CompanyDomicileRegion)){
    Combin$MarketAUM[Combin$CompanyDomicileRegion == CompanyDomicileRegionList$CompanyDomicileRegion[l]] <- MarketSizeData$MarketSize[MarketSizeData$MarketRegion == CompanyDomicileRegionList$CompanyDomicileRegion[l]]
  }
  
  Combin$TargetProductionAUMIntensity <- (Combin$RefProdMarketTech + (Combin$RefProdMarketSector * Combin$FairSharePerc)) * (Combin$PortAUM / Combin$MarketAUM)
  Combin$TargetProductionAUMIntensity[Combin$Direction == "declining"] <- Combin$RefProdMarketTech[Combin$Direction == "declining"] * (1 + Combin$FairSharePerc[Combin$Direction == "declining"]) * (Combin$PortAUM[Combin$Direction == "declining"] / Combin$MarketAUM[Combin$Direction == "declining"])
  Combin$TargetProductionAUMIntensity[Combin$Sector == "Fossil Fuels"] <- Combin$RefProdMarketTech[Combin$Sector == "Fossil Fuels"] * (1 + Combin$FairSharePerc[Combin$Sector == "Fossil Fuels"]) * (Combin$PortAUM[Combin$Sector == "Fossil Fuels"] / Combin$MarketAUM[Combin$Sector == "Fossil Fuels"])
  
  ## Aggregate benchmark production for Global Aggregate
  #Subset Combin the mutual exclusive benchmarking regions for each sector
  GlobalAggregate <- subset(Combin, CompanyDomicileRegion %in% c("Global", AllLists$MutualExclusiveCompanyDomicileRegions) & 
                              (Sector == "Power" & BenchmarkRegion %in% AllLists$PowerBenchmarkRegionGlobal) | 
                              (Sector == "Automotive" & BenchmarkRegion == "Global") | 
                              (Sector == "Fossil Fuels" & Technology != "Coal" & BenchmarkRegion %in% AllLists$FossilFuelBenchmarkRegions) | 
                              (Sector == "Fossil Fuels" & Technology == "Coal" & BenchmarkRegion %in% "Global"))
  GlobalAggregate$BenchmarkRegion2 = "GlobalAggregate"
  
  AggregatedResults <- GlobalAggregate
  
  #Sum production
  GlobalAggregate <- ddply(AggregatedResults,.(Sector, Technology, Scenario, Year, CompanyDomicileRegion, PortAUM, BenchmarkRegion2), summarize, Production = sum(Production, na.rm = TRUE), TargetProductionAlignment= sum(TargetProductionAlignment, na.rm = TRUE),TargetProductionAUMIntensity = sum(TargetProductionAUMIntensity, na.rm = TRUE))
  GlobalAggregate <- plyr::rename(GlobalAggregate, c("BenchmarkRegion2" = "BenchmarkRegion"))
  GlobalAggregateSave <- GlobalAggregate
  
  MissingCols <- (setdiff(names(Combin), names(GlobalAggregate)))# Find names of missing columns
  tempdf <- data.frame(matrix(ncol = length(MissingCols), nrow = nrow(GlobalAggregate)))
  names(tempdf) <- MissingCols
  GlobalAggregate <- cbind(GlobalAggregate,tempdf)
  
  #Add with Combin
  CombinSave<-Combin
  Combin <- rbind(Combin, GlobalAggregate)
  
  # Calculate Exposure percentages 
  Combin$MarketExposure <- (Combin$Production - Combin$TargetProductionAlignment) / Combin$TargetProductionAlignment
  Combin$AUMExposure <- (Combin$Production - Combin$TargetProductionAUMIntensity) / Combin$TargetProductionAUMIntensity
  
  #implement AUM in sector values
  Combin$PortName <- PortfolioName
  # Combin$Type <- "TestPort"
  Combin$InvestorName <- InvestorName

  Combin <- subset(Combin, BenchmarkRegion == "GlobalAggregate"  & Scenario == "450S" & CompanyDomicileRegion == "Global" , select = c("InvestorName","PortName", "Year",	"Sector",	"Technology",	"Scenario",	"CompanyDomicileRegion",	"BenchmarkRegion",	"PortAUM",	"MarketAUM",	"Production",		"FairSharePerc",	"Direction",	"TargetProductionAlignment",	"TargetProductionAUMIntensity",		"MarketExposure",	"AUMExposure"))

  ReducedListSub <- subset(ReducedList,Year %in% c(Startyear,Startyear+5))
  rm(AUMmix)
}

# Order the variables in CombinAll
# RegionalSubset <- subset(Combin, Scenario == ParameterFile$Scenario & BenchmarkRegion %in% ParameterFile$BenchmarkRegion & CompanyDomicileRegion %in% ParameterFile$CompanyDomicileRegion)
RegionalSubset<- subset(Combin, select = c("InvestorName", "PortName",  "Year",	"Sector",	"Technology",		"PortAUM",	"Production",	"TargetProductionAlignment",	"TargetProductionAUMIntensity",	"MarketExposure",	"AUMExposure"))

# - - - - -
# |------------------------------|
# 3) Save Analysis Reults -----
# |------------------------------|
# - - - - -
saveRDS(ReducedListSub,"CompanysProduction_Snapshot.Rda")
saveRDS(Portfolio,"PortfolioData_Snapshot.Rda")
saveRDS(Combin,file="EquityAnalysisResults.Rda")
