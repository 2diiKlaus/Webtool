# # Report Maker 2000
# 
# # Libraries
# 
# # Location <- getSrcDirectory()[1] # probably need to use this line instead of the following when running on the server
# Location <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(Location)
# 
# source(WebGraphFunctions_v1.R)
# 
# InvestorParameterFile <- read.csv()
# 
# readInputFile <- (GraphResultsFile.R)
# 
# 
# 
# ReportCall(ReportParameterFile, WorkingDirectory)




# Website basic Report Code

rm(list=ls())

# Libraries
library(grid)
library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape2)
library(gridExtra)
library(scales)
library(stringr)
library(extrafont)
library(tidyr)
library(knitr)
# library(png)
library(RColorBrewer)
library(matrixStats)


# Location <- dirname(rstudioapi::getActiveDocumentContext()$path)
Location <- getwd()
Location <- paste0(Location, "/")                 # I don't know whether these two lines will still be necessary
Location <- gsub("2Â°","2°",Location)
setwd(Location)

source("WebGraphFunctions_v0.R")

# This should be written from the website - although is it even necessary?
ParameterFile <- read.csv("Global_ParameterFile.csv")
SetParameters(ParameterFile)
PortfolioName <- "WebPortfolio"

# Variables needed for plotting - colour, text specs etc. 
GraphValues()

BatchTest <- readRDS("ComparativeBatchResults_Combin.Rda")   # Comparative funds Combin File 
BatchTest_PortSnapshots<- readRDS("ComparativeBatchResults_PortSS.Rda") # Comparative funds PortSnapshot
Results_combin_EQ <- readRDS("EquityAnalysisResults.Rda")
PortSnapshot <- readRDS("PortfolioData_Snapshot.Rda")
IndexUniverses <- readRDS("IndexRegions.Rda")
BBGData_CompanyLevel <- readRDS("CompanylvlBBGData.Rda") # This needs to be updated (ie quarterly)
AllCompanyData <- readRDS("CompanyLevelData.Rda")
IndexData <- readRDS("IndexData.Rda")
CleanedBBGData <- readRDS("CompanylvlBBGData.Rda")
UtilityCompanies <- readRDS("UtilityCompanies.Rda")
AutoCompanies <- readRDS("AutoCompanies.Rda")
OGCarbonBudget <- readRDS("OGCarbonBudget.Rda") # Carbon Tracker budget data
OGData <- readRDS("OGData.Rda")           # Fuel Type Data
Companies <- readRDS("Companies.Rda")
BenchmarkRegionList <- readRDS("BenchmarkRegionList.Rda")
MasterData_Power <- readRDS("MasterData_Power.Rda")
AllIEATargets <- readRDS("AllIEATargets.Rda")

piechartdata <- readRDS("Portfolio_Overview_Piechart.Rda")
ShippingData <- readRDS("ShippingData.Rda")
OSTargets <- readRDS("OSTargets.Rda")
OSData <- readRDS("OSData.Rda")
PortSnapshot$PortName <- PortfolioName
EQ_OS_WEM <- matchOS(OSData, PortSnapshot)

BatchTest <- BatchTest[,colnames(Results_combin_EQ)]
WeightedResults <- WeightedResultsPrep(BatchTest_PortSnapshots,BatchTest)
ComparisonResults <- RankingResultsPrep(BatchTest,Results_combin_EQ)
Ranks <- ComparisonResults[[1]]
Exposures <- ComparisonResults[[2]]
AUMData <- ComparisonResults[[3]]


###############
# Check for sector production
###############
EQSectorProd <- production_check(Results_combin_EQ)


# #################
# ### REPORTING ###
# #################

template <- (readLines("WebTemplate.tex",encoding="UTF-8"))
# ReportTranslation <- readRDS("ReportTranslation.Rda")

ReportTranslation <- read.csv("ReportTranslation.csv", strip.white = T,stringsAsFactors = F)

RT <- preptranslations("Report",ReportTranslation, "EN", Startyear)

EQReportData <- ReportData()

# ReportFigures()
ReportGeneration()





