rm(list=ls())

#Load packages
# library(grid)
library(dplyr)
library(reshape2)
# library(gridExtra)
# library(scales)
# library(stringr)
# library(ggplot2)
# library(png)
# library(tidyr)
# library(zoo)

# Version - Control

# --- DATE ---|--- Editor ---| --- Version Name --- | --- Edits / Adds / Changes / Bugfixes ---
#  2017-11-14 |      KH      |           0          | Adjust Batch-code for single files and simplify the code - target will be to enable it to work as a function / stand-alone tool that can be used by other companies and for the website

# - - - - -
# I) Get location of the code ----
# - - - - -
# code and portfolio data should be in the same folder in this case
# Location <- dirname(rstudioapi::getActiveDocumentContext()$path)
Location <- getwd()
Location <- paste0(Location, "/")
Location <- gsub("2Â°","2°",Location)

# - - - - -
# II) Set workdrive ----
# - - - - -
setwd(Location)

# - - - - -
# III) Read in functions ----
# - - - - -
source("GlobalPortCheckFunctions.R")

# - - - - -
# IV) Reset Error and Warning count ----
# - - - - -
#TODO: 1 error log file and one warning log file
ErrorCount = 0
WarningCount = 0
initiateLogFile("Error")
initiateLogFile("Warning")

# - - - - -
# |------------------------------|
# 0) Read Investor and Portfolio Name -----
# |------------------------------|
# - - - - -
# TODO: Read in PortfolioName and InvestorName
NameInput <- read.csv("Investor_ParameterFile.csv", strip.white = TRUE, stringsAsFactors = FALSE)
PortfolioName <- NameInput$PortfolioName
InvestorName <- NameInput$InvestorName

# - - - - -
# |------------------------------|
# 1) Read & check in Portfolio Input -----
# |------------------------------|
# - - - - -
Portfolio <- read.csv("Portfolio_Input.csv", strip.white = TRUE, stringsAsFactors = FALSE)
# Portfolio <- read.csv("PortfolioInput_error.csv", strip.white = TRUE, stringsAsFactors = FALSE)
if(dim(Portfolio)[2] != 2 & colnames(Portfolio)[1] == "ISIN.Position"){
  Portfolio <- read.csv("Portfolio_Input.csv", strip.white = TRUE, stringsAsFactors = FALSE, sep = ";", dec = ",")
}

# check if all headers are given
MissingColumns <-setdiff(c("Position","ISIN"),colnames(Portfolio))
if(length(MissingColumns) > 0){
  addMessageToLogFile("Error",paste0("The input file is missing the following datapoints: ", MissingColumns))
  ErrorCount <- ErrorCount + 1
}

if (ErrorCount == 0){
  # Check for positions without Identifier
  MissingISIN <- subset(Portfolio, ISIN == "")
  if(nrow(MissingISIN) > 0){
    percMissingISINs <- round(nrow(MissingISIN) / (100 * nrow(Portfolio)), digits = 2)
    addMessageToLogFile("Warning",paste0(nrow(MissingISIN),"(equal to ",percMissingISINs,"%) of the positions are missing the unique identifier (ISIN) input"))
    WarningCount = WarningCount + 1
    Portfolio <- subset(Portfolio, ISIN != "")
  }
  
  # Check for positions without position
  Portfolio$Position <- as.numeric(Portfolio$Position)
  MissingPosition <- subset(Portfolio, is.na(Portfolio$Position))
  if(nrow(MissingPosition) > 0){
    percMissingPositions <- round(nrow(MissingPosition) / (100 * nrow(Portfolio)), digits = 2)
    addMessageToLogFile("Warning",paste0(nrow(MissingPosition),"(<=> ",percMissingPositions,"% of the positions are missing the position input. Make sure you set the field to general in excel as the Application cannot treat the number-format"))
    WarningCount = WarningCount + 1
    Portfolio <- subset(Portfolio, !is.na(Portfolio$Position))
  }
  
  # Check for positions with negative or 0 positions
  Portfolio <- group_by(Portfolio, ISIN)
  Portfolio <- dplyr::summarise(Portfolio, Position = sum(Position))
  
  NegativePosition <- subset(Portfolio, Position <= 0)
  if(nrow(NegativePosition) >= 0){
    addMessageToLogFile("Warning",paste0(nrow(NegativePosition)," of the positions have a 0 as value or a negative value. Those Positions will not be analysed."))
    WarningCount = WarningCount + 1
    Portfolio <- subset(Portfolio, !is.na(Portfolio$Position))
  }
  
  
  # - - - - -
  # |------------------------------|
  # 2) Merge Portfolio Input with Bloomberg-Data & perform further checks -----
  # |------------------------------|
  # - - - - -
  #Load Bloomberg data
  BBGPORTOutput <- readRDS(paste0(Location,"FinancialData.Rda"))
  
  # check for Positions without existing BBG-Data in our universe <- those need to be collected!
  Portfolio <- merge(Portfolio, BBGPORTOutput, by = c("ISIN"),all.x = TRUE)
  Portfolio$ValueUSD <- Portfolio$Position * Portfolio$SharePrice
  PortfolioData_wo_BBG <- unique(subset(Portfolio,is.na(ValueUSD), select = c("ISIN")))
  
  PortSizeCheck1 <- sum(Portfolio$ValueUSD, na.rm = TRUE)
  
  # - - - - -
  # |------------------------------|
  # 3) Perform Fund Look Through & perform further checks -----
  # |------------------------------|
  # - - - - -
  
  # 3a) Read in fund data----
  Fund_Data <- readRDS(paste0(Location,"FundLookThroughData.Rda")) 
  
  # 3b) Merge Portfolio Data with fund data for the lookthrough and calculate owned holdings of Portfolios----
  #All Instruments (R-pull with Port-Weight for both EQY & CBonds):
  Portfolio_LookThrough <- merge(Fund_Data, subset(Portfolio, select = c("ISIN", "ValueUSD")),  by.y = "ISIN", by.x = "FundISIN")
  if (exists("Portfolio_LookThrough") & nrow(Portfolio_LookThrough)>0){
  Portfolio_LookThrough <- merge(Portfolio_LookThrough, unique(subset(BBGPORTOutput, select = c("ISIN", "SharePrice", "Group"))),by.x = "HoldingISIN", by.y = "ISIN", all.x = TRUE, all.y = FALSE)
  Portfolio_LookThrough$Position <- Portfolio_LookThrough$ValueUSD * Portfolio_LookThrough$value / 100
  Portfolio_LookThrough$Position[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] <- Portfolio_LookThrough$ValueUSD[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] * Portfolio_LookThrough$value[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] * Portfolio_LookThrough$SharePrice[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"]
  Portfolio_LookThrough <- subset(Portfolio_LookThrough, FundCoverage <= 100)
  Portfolio_LookThroughCovered <- subset(Portfolio_LookThrough, !is.na(SharePrice))
  Portfolio_LookThroughCovered <- aggregate(Portfolio_LookThroughCovered["Position"], by = Portfolio_LookThroughCovered[,c("FundISIN", "ValueUSD", "FundCoverage")],FUN = sum, na.rm = TRUE)
  Portfolio_LookThroughCovered <- dplyr::rename(Portfolio_LookThroughCovered, FundCoverageMS = FundCoverage)
  
  Portfolio_LookThroughCovered$FundCoverageBBG <- Portfolio_LookThroughCovered$Position / Portfolio_LookThroughCovered$ValueUSD
  FundCoveragePortfolioLevel <- sum(Portfolio_LookThroughCovered$Position,na.rm = TRUE) / sum(Portfolio_LookThroughCovered$ValueUSD,na.rm = TRUE)
  
  PortfolioData_wo_BBG <- subset(PortfolioData_wo_BBG, !ISIN %in% Portfolio_LookThroughCovered$FundISIN)
  
  PortfolioData_Funds <- subset(Portfolio, ISIN %in% Portfolio_LookThrough$FundISIN)
  # EQFundGroups <- c("Equity Fund", "Closed-end Funds","Asset Allocation Fund")
  # AllFundGroups <- grep("Fund",unique(BBG_Data$Group), value =TRUE)
  # FundsBBG <- subset(Portfolio, Group %in% AllFundGroups, select = c("ISIN", "SharePrice","Position", "ValueUSD", "ICB.Subsector.Name", "Group"))  
  FundsCovered <- subset(PortfolioData_Funds, select = c("ISIN", "SharePrice","Position", "ValueUSD", "ICB.Subsector.Name"))
  
  FundsWithMissingBBGData <- subset(FundsCovered, is.na(ValueUSD), select = c("ISIN"))
  if(nrow(FundsWithMissingBBGData) > 0){
    FundsWithMissingBBGData$QTY <- 1
    FundsWithMissingBBGData$Date <- "31-12-2016"
    # write.csv(FundsWithMissingBBGData, "BBG-Look-up-needed.csv",row.names = FALSE)
  }
  
  PortfolioData_w_BBG_test <- subset(Portfolio,!is.na(Group) & !ISIN %in% Portfolio_LookThrough$FundISIN)
  
  if(is.null(length(setdiff(Portfolio$ISIN,c(PortfolioData_Funds$ISIN,PortfolioData_wo_BBG$ISIN,PortfolioData_w_BBG_test$ISIN))))){
    print("ISINS GETTING LOST!! CHECK LOSTISINS")
    LOSTISINS <- setdiff(Portfolio$ISIN,c(PortfolioData_Funds$ISIN,PortfolioData_wo_BBG$ISIN,PortfolioData_w_BBG_test$ISIN))
  }
  
  Portfolio <- subset(Portfolio, !is.na(Group) & !ISIN %in% Portfolio_LookThrough$FundISIN)
  }
  
  # Create the equity portfolio input files for the fund analysis
  Portfolio <- subset(Portfolio, select = c("ISIN", "Group", "ValueUSD"))
  Portfolio$HoldingType <- "Direct Holding"
  
  if (exists("Portfolio_LookThrough") & nrow(Portfolio_LookThrough)>0){
  Portfolio_Funds <- subset(Portfolio_LookThrough, select = c("HoldingISIN", "Group", "Position"))
  Portfolio_Funds <- dplyr::rename(Portfolio_Funds, ISIN = HoldingISIN, ValueUSD = Position)
  Portfolio_Funds_summed <- aggregate(Portfolio_Funds["ValueUSD"], by=Portfolio_Funds[,c("ISIN", "Group")], FUN=sum, na.rm = TRUE)
  Portfolio_Funds_summed$HoldingType <- "Fund Holding"
  }
  
  TotalPortfolio <- Portfolio
  if (exists("Portfolio_Funds_summed")==TRUE){
    TotalPortfolio <- rbind(TotalPortfolio,Portfolio_Funds_summed)}
  
  PortSizeCheck2 <- sum(TotalPortfolio$ValueUSD, na.rm = TRUE)
  
  # USE PortSizeCheck2 - PortSizeCheck1 to show how much is "lost" due to coverage of funds != 100%
  # also show number of funds that are looked through and funds that are missing, either not in our data base yet or no data existing 
  
  
  # - - - - -
  # |------------------------------|
  # 4) Perform Meta-Analysis -----
  # |------------------------------|
  # - - - - -
  TotalPortfolio <- merge(TotalPortfolio, subset(BBGPORTOutput, select = c("ISIN","ICB.Subsector.Name", "Name", "Ticker", "Subgroup")), by = "ISIN", all.x = TRUE, all.y = FALSE)
  Groups_notEQY <- c("Sovereign", "Agency CMBS", "Automobile ABS Other","CMBS Other","CMBS Subordinated" ,"Municipal-City" ,"Municipal-County","Debt Fund","Multi-National","Commodity Fund", "Real Estate Fund","Alternative Fund","Money Market Fund", "","Other ABS","Sovereign","Sovereign Agency","WL Collat CMO Mezzanine","WL Collat CMO Other","WL Collat CMO Sequential")
  TotalPortfolio_EQY <- subset(TotalPortfolio, (!is.na(ICB.Subsector.Name) & ICB.Subsector.Name != "") | (Name != Ticker & !Group %in% Groups_notEQY & !is.na(Group)))
  TotalPortfolio_EQY <- merge(TotalPortfolio_EQY, subset(BBGPORTOutput, select = c("ISIN","SharePrice")), by = "ISIN", all.x = TRUE, all.y = FALSE)
  
  # Filter for funds
  EQPortfolio <- subset(TotalPortfolio_EQY, !Group %in% grep("Fund",unique(TotalPortfolio_EQY$Group), value = TRUE) & SharePrice != "" & Group != "Alternative Investment")
  
  # Bind with ALD - Equity - Bridge
  EquityBridge <- readRDS(paste0(Location,"EquityBridge.Rda"))
  EquityBridgeSub <- unique(EquityBridge)
  EQPortfolio <- merge(EQPortfolio,EquityBridgeSub, by.x = "Ticker", by.y = "TICKER_AND_EXCH_CODE",all.x = TRUE)
  EQPortfolio$Position <- EQPortfolio$ValueUSD / EQPortfolio$SharePrice
  
  # EQPortfolio <- subset(Portfolio, select = c("EQY_FUND_TICKER" , "Position", "Subgroup" , "ICB.Subsector.Name", "Ticker", "ISIN", "SharePrice", "CNTRY_OF_DOMICILE"))
  EQPortfolio <- subset(EQPortfolio, !is.na(ValueUSD) & ValueUSD > 0)
  EQPortfolio$PortfolioName <- PortfolioName
  EQPortfolio$InvestorName <- InvestorName
  EQPortSize <- sum(EQPortfolio$ValueUSD)
  
  TotalPortfolio$InstrumentType <- "Others"
  TotalPortfolio$InstrumentType[TotalPortfolio$ISIN  %in% TotalPortfolio_EQY$ISIN] <- "Equity"
  
  PortSizeCheck3 <- sum(TotalPortfolio$ValueUSD, na.rm = TRUE)
  
  #------------
  # Data for the Overview Pie Chart
  #------------
  TotalPortfolio$ValueUSD <- as.numeric(TotalPortfolio$ValueUSD)
  OverviewPiechartData <- aggregate(TotalPortfolio["ValueUSD"], by = TotalPortfolio[,c("HoldingType", "InstrumentType")], FUN = sum, na.rm = TRUE)
  OverviewPiechartDatawide <- dcast(OverviewPiechartData, HoldingType  ~ InstrumentType, value.var = "ValueUSD")
  
  MissingColumns <-setdiff(c("Equity","Others"),colnames(OverviewPiechartDatawide))
  if(length(MissingColumns) > 0){OverviewPiechartDatawide[,MissingColumns] <- 0}
  
  OverviewPiechartDatawideTotal <- data.frame(Equity = sum(OverviewPiechartDatawide$Equity, na.rm = TRUE), 
                                              Others = sum(OverviewPiechartDatawide$Others, na.rm = TRUE),
                                              HoldingType = "All")
  
  OverviewPiechartDatawide <- rbind(OverviewPiechartDatawide,OverviewPiechartDatawideTotal)
  OverviewPiechartDatawide$PortfolioSizeUSD <- PortSizeCheck1
  OverviewPiechartDataFinal <- dplyr::rename(OverviewPiechartDatawide, PositionsWithValue_Ignore4piechart = Others )
  OverviewPiechartDataFinal$Others <- OverviewPiechartDataFinal$PortfolioSizeUSD - OverviewPiechartDataFinal$Equity
  OverviewPiechartDataFinal <- subset(OverviewPiechartDataFinal, HoldingType == "All")
  
  # - - - - -
  # |------------------------------|
  # 5) Save Portfolio files that are used as input for the Alignment test & output text for the initial analysis (Warnings & Errors) -----
  # |------------------------------|
  # - - - - -
  saveRDS(EQPortfolio,"Portfolio.Rda")
  saveRDS(OverviewPiechartDataFinal,"Portfolio_Overview_Piechart.Rda")
}