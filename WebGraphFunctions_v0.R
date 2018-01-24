

#----------- Set Parameters
SetParameters <- function(ParameterFile){
  BenchmarkRegionchoose <<- as.character(ParameterFile$BenchmarkRegion)
  CompanyDomicileRegionchoose <<- as.character(ParameterFile$CompanyDomicileRegion)
  Scenariochoose <<- as.character(ParameterFile$Scenario)
  Startyear <<- as.numeric(format(Sys.time(), "%Y")) #as.numeric(ParameterFile$Startyear)
  Indexchoose <<- "MSCI World"
  Languagechoose <<- "EN"
  # BatchName <<- as.character(ParameterFile$BatchName)
}

# ------------ Production Check ------------- #
production_check <- function(combin){
  Results <- subset(combin,Scenario %in% Scenariochoose & Year == Startyear+5 & BenchmarkRegion == BenchmarkRegionchoose & CompanyDomicileRegion == CompanyDomicileRegionchoose,select= c("Sector","Production")) 
  SectorProduction <- ddply(Results, .(Sector),summarise, Production  =sum(Production))
  return(SectorProduction)
}

# ----------- Graph Print Check -------------
SectorPrint <- function(SectorToPlot,SectorProd){
  
  SectorProduction <- SectorProd$Production[SectorProd$Sector %in% SectorToPlot]
  
  PlotFlag <- 1
  if (SectorToPlot %in% c("Automotive","Power") & SectorProduction == 0){PlotFlag <- 0}
  
  return(PlotFlag)
  
}

#-------- Call any graph
GraphCall <- function(GraphType, TechToPlot){
  
  SectorToPlot <- SectorSelect(TechToPlot)
  ChartType <- "EQ"
  
  if (GraphType == "LineChart"){
    mini_line_chart(ChartType,TechToPlot,SectorToPlot)
  }
  
  if (GraphType == "StackedBar"){                #Not Ready yet
    stacked_bar_chart(ChartType,SectorToPlot)
  }
  
  if (GraphType == "PortfolioPie"){
    pie_chart(ChartType)
  }
  
  if (GraphType == "RankingChart"){
    ranking_chart(ChartType, SectorToPlot)
  }
  
  if (GraphType == "OverviewPie"){
    port_pie()
  }
  
  if (GraphType == "OtherSectorChart"){
    if (TechToPlot == "Shipping"){
      shipping_chart()
    }else{
      other_sector_chart( SectorToPlot)
    }
  }
  
  if (GraphType == "CompanyChart"){
    CompanyChart(ChartType, SectorToPlot)
  }
  
  if (GraphType =="RenewAdditions"){
    renewAdditions(ChartType)
  }
  
}

#-------- Report Figures
ReportFigures <- function(){
  
  plot_0 <- GraphCall("OverviewPie","")
  plot_1 <- GraphCall("PortfolioPie","")
  
  if (SectorPrint("Power",EQSectorProd)==1){
    plot_3 <- GraphCall("StackedBar","Power")
    plot_4 <- GraphCall("LineChart","RenewablesCap")
    plot_5 <- GraphCall("LineChart","CoalCap")
    plot_6 <- GraphCall("LineChart","GasCap")
    plot_00 <- GraphCall("LineChart","NuclearCap")
    plot_01 <- GraphCall("LineChart","HydroCap")
    plot_7 <- GraphCall("RankingChart","Power")
  }
        
  if (SectorPrint("Automotive",EQSectorProd)==1){
    plot_13 <- GraphCall("StackedBar","Automotive")
    plot_14 <- GraphCall("LineChart","ICE")
    plot_15 <- GraphCall("LineChart","Electric")
    plot_16 <- GraphCall("LineChart","Hybrid")
    plot_17 <- GraphCall("RankingChart","Automotive")
  }  
  
  plot_23 <- GraphCall("StackedBar","Fossil Fuels")
  plot_24 <- GraphCall("LineChart","Oil")
  plot_25 <- GraphCall("LineChart","Gas")
  plot_26 <- GraphCall("LineChart","Coal")
  plot_27 <- GraphCall("RankingChart","Fossil Fuels")
  
  plot_33 <- GraphCall("RankingChart","All")
  
  OtherSectors <<- data.frame("Cement"=0,"Steel"=0,"Aviation"=0,"Shipping"=0)
  plot_36 <- GraphCall("OtherSectorChart", "Cement")
  OtherSectors$Cement <<- InPort
  plot_37 <- GraphCall("OtherSectorChart", "Steel")
  OtherSectors$Steel <<- InPort
  plot_38 <- GraphCall("OtherSectorChart", "Aviation")
  OtherSectors$Aviation <<- InPort
  plot_39 <- GraphCall("OtherSectorChart", "Shipping")
  OtherSectors$Shipping <<- InPort
  
  plot_43 <- GraphCall("CompanyChart", "Power")
  plot_44 <- GraphCall("CompanyChart", "Automotive")
  plot_46 <- GraphCall("CompanyChart", "OG")  
  
  plot_51 <- GraphCall("RenewAdditions", "")

  # figurelist <- list.files(getwd(),pattern=c("\\.png$"), full.names = FALSE)
  # writeLines(figurelist,"FigureList.txt")
  
  }

#-------- Report Data
ReportData <- function(){
  
  combin <- Results_combin_EQ
 
  ChartType = "EQ"
  
  
  if (nrow(combin)>0){
    
    PortSnapshot <- rename(PortSnapshot, c("IssLvlPortWeight"="PortWeight"),warn_missing = FALSE)
    # Pie Share Data
    PortSnapshotSub <- subset(PortSnapshot, CNTRY_OF_DOMICILE %in% IndexUniverses[,names(IndexUniverses) == eval(paste0(CompanyDomicileRegionchoose,"_ISO"))])
    piesub_tech <- unique(subset(PortSnapshotSub,select=c("ISIN","piesector","PortWeight")))
    piesub_tech$piesector<-gsub("NonUtility Power", "Non-Utility Power", piesub_tech$piesector)
    piesub_tech$piesector[is.na(piesub_tech$piesector)] <- "Not Assessed"
    pieshares <- ddply(piesub_tech, .(piesector),summarize,Portfolio_weight=sum(PortWeight, na.rm=TRUE))
    # Numbers to print
    PieAssessedShare <- round((1-pieshares$Portfolio_weight[pieshares$piesector %in% "Not Assessed"]),2)*100
    if(length(pieshares$Portfolio_weight[pieshares$piesector%in% "Not Assessed"])==0){PieAssessedShare<-100}
    
    # Line Chart Data
    if (ChartType == "EQ"){
      LineData <- subset(combin, BenchmarkRegion %in% BenchmarkRegionchoose & CompanyDomicileRegion %in% CompanyDomicileRegionchoose & Scenario %in% Scenariochoose & Year %in% (Startyear+5))  
      LineData <- subset(LineData, select = c("Sector","Technology","Year","Production","TargetProductionAlignment","TargetProductionAUMIntensity"))
      LineData$Check <- LineData$Production-LineData$TargetProductionAlignment
      LineData$Check[LineData$Sector %in% "Fossil Fuels"] <- LineData$Production[LineData$Sector %in% "Fossil Fuels"]-LineData$TargetProductionAUMIntensity[LineData$Sector %in% "Fossil Fuels"]
      
    }else{
      LineData <- subset(combin, Year %in% (Startyear+5) & BenchmarkRegion %in% BenchmarkRegionchoose  & Scenario %in% Scenariochoose)    
      LineData <- subset(LineData, select = c("Sector","Technology","Year","WtTechShareTechShare","Benchmark_WtTechShareTechShare","Benchmark_OGC","OGCMetrik_Portfolio")) 
      LineData$Check <- LineData$WtTechShareTechShare - LineData$Benchmark_WtTechShareTechShare
      LineData$Check[LineData$Sector %in% c("Oil&Gas","Coal")]<- LineData$OGCMetrik_Portfolio[LineData$Sector %in% c("Oil&Gas","Coal")] - LineData$Benchmark_OGC[LineData$Sector %in% c("Oil&Gas","Coal")]
      LineData$Production <- LineData$WtTechShareTechShare
      LineData$Production[LineData$Sector %in% c("Oil&Gas","Coal")] <-LineData$OGCMetrik_Portfolio[LineData$Sector %in% c("Oil&Gas","Coal")]
    } 
    
    LineData$Technology <- revalue(LineData$Technology, c("Coal"="CoalProd","Gas"="GasProd","Oil"="OilProd"))
    
    # 1 indicates it is aligned, 0 is misaligned
    # # Rating = to 1 if the Production is higher than the target for Good Techs
    goodtech <- c("RenewablesCap","HydroCap","NuclearCap","Hybrid","Electric")
    badtech <- c("ICE","OilProd","GasProd","CoalProd","GasCap","CoalCap")
    # 
    LineData$Rating <- "Check"
    LineData$Rating[LineData$Check < 0] <- 0
    LineData$Rating[LineData$Check > 0] <- 1
    LineData$Rating[is.na(LineData$Production)] <- NA
    LineData <- subset(LineData,!Technology %in% "OilCap", select = c("Technology","Rating"))
    LD <- setNames(data.frame(t(LineData[,-1])), LineData[,1]) 
    
    
    # Ranking Chart
    df <- Exposures
    df <- merge(df,AUMData, by= "PortName")
    df <- rename(x = df, c("PortAUM"="AUM"),warn_missing = FALSE)
    
    WM<- as.data.frame(lapply(df[ , 2:12], weighted.mean, na.rm=TRUE,  w = df$AUM))
    WM$PortName <- "WeightedMean"
    df$AUM <- NULL
    df <- df[df$PortName %in% PortfolioName,]
    df <- rbind(df,WM)
    
    df <- setNames(data.frame(t(df[,-1])), df[,1]) 
    df$Check <- df$WeightedMean - df[,1]
    df$Rating <- 1
    df$Rating[df$Check > 0] <- 0
    df$Rating[is.na(df[,1])] <- NA
    
    TechsAboveAlignment <- as.integer(sum(as.numeric(LineData$Rating), na.rm = TRUE))
    TechsAboveMean <- as.integer(sum(as.numeric(df$Rating), na.rm = TRUE))
    TechsInPort <- as.integer(length(which(!is.na(LineData$Rating))))
    
    
    ReportData <- data.frame(row.names=c("PieAssessedShare","TechsAboveAlignment", "TechsAboveMean", "TechsInPort"))
    ReportData$Values <- c(PieAssessedShare,TechsAboveAlignment,TechsAboveMean,TechsInPort)
    ReportData <- data.frame(t(ReportData))
    ReportData <- cbind(ReportData,LD)
  }else{
    
    ReportData <- data.frame()
  }
  
  return(ReportData)
}

#-------- Generate the report
ReportGeneration <- function(){
  
  PORTFOLIONAME <- toupper(PortfolioName)
  
  # Copy in the template for the report
  text <- as.data.frame(template,stringsAsFactors = FALSE)  
  colnames(text) <- "text"
  
  removetextlines <- function(handlename){
    startpage <- which(grepl(paste0(handlename,"S"),text$text))
    endpage <- which(grepl(paste0(handlename,"E"),text$text))
    
    if (length(startpage) >0 ){
      
      removelist <- lapply(1:length(startpage), function(x) c(startpage[c(x)]:endpage[c(x)]))
      removelist <- melt(removelist[1:length(startpage)])
      text <- as.data.frame(text[-removelist$value,],stringsAsFactors =FALSE)
      colnames(text) <- "text"
    }else{
      removeline <- which(grepl(handlename,text$text))
      text <- as.data.frame(text[-removeline,],stringsAsFactors =FALSE)
      colnames(text) <- "text"
    }
    return(text)
  }
  
  # Add in numerics/conditionals
  # Changes the more or less for each Technology
  if (nrow(EQReportData)>0){
    EQTechList <- as.data.frame(paste0("EQCaption",colnames(EQReportData)[5:length(EQReportData)]))  
    colnames(EQTechList) <- "CaptionTitle" 
    EQTechList$Test <- t(EQReportData[5:length(EQReportData)])
    EQTechList$Caption <- RT["CaptionMore"][[1]]
    EQTechList$Caption[EQTechList$Test == 0] <- RT["CaptionLess"][[1]]
    EQTechList$Test <- NULL
    EQTechList<- setNames(data.frame(t(EQTechList[,-1])), EQTechList[,1])
    
    RT$EQCoverage <- paste(EQReportData$PieAssessedShare,as.character(" \\\\\\%"))
    
    RT$EQTechsAlign <- EQReportData$TechsAboveAlignment
    RT$EQTechsWM <- EQReportData$TechsAboveMean
    RT$EQTechsPort <- EQReportData$TechsInPort  
    RT <- cbind(RT,EQTechList)  
  }
  
  # Update the template to reflect figure names
  # FigNames<-as.data.frame(readLines("FigureList.txt",skipNul = TRUE))
  # colnames(FigNames) <- "Name"
  # FigNames$Name <- gsub("\"","",as.character(FigNames$Name))
  # FigNames$Fig <- substring(FigNames$Name,1,2)
  # FigNames$Fig <- paste0("Fig",FigNames$Fig)
  # 
  # for (f in 1:nrow(FigNames)){
  #   text$text <- gsub(FigNames$Fig[f],FigNames$Name[f],text$text,fixed = TRUE)
  # }
  
  # RenewAdds<-0
  # if (length(grep("Fig51", FigNames$Fig))>0){RenewAdds <- 1}
  
  # Check for each technology, 

    techpageremoval <- data.frame("PowerEQ"=EQSectorProd$Production[EQSectorProd$Sector == "Power"],
                                "AutomotiveEQ"=EQSectorProd$Production[EQSectorProd$Sector == "Automotive"],
                                "FossilFuelsEQ"=1)
  removesectors <- colnames(techpageremoval[which(techpageremoval == 0)]) 
  
  # removes the sectors  
  if(length(removesectors)>0){
    for (i in 1:length(removesectors)){
      text <- removetextlines(removesectors[i])
    }}
  
  # removes equity pages
  if (nrow(EQReportData)==0){
    # pages <- c(9,11,13,15)
    text <- removetextlines("EQPage")
    text <- removetextlines("EQPie")
    text <- removetextlines("RenewAddsOut")
    
    renewvspace<- which(grepl("renewspacingworkaround",text$text))
    text$text[renewvspace] <- "\t\\vspace{-2.9cm} %renewspacingworkaround"
  }
  
  # removes renewable chart 
  if (!file.exists("RenewableAdditions.png") & nrow(EQReportData)>0){
    text <- removetextlines("RenewAddsOut")
    renewvspace<- which(grepl("renewspacingworkaround",text$text))
    text$text[renewvspace] <- gsub(".9cm","2.9cm",text$text[renewvspace])
  }
  
  OtherSectors <- data.frame("Cement"=0,"Steel"=0,"Aviation"=0,"Shipping"=0)
  if (file.exists("TechnologyGraphs_TechnologyAlignment_Cement.png")){OtherSectors$Cement <- 1}
  if (file.exists("TechnologyGraphs_TechnologyAlignment_Steel.png")){OtherSectors$Steel <- 1}
  if (file.exists("TechnologyGraphs_TechnologyAlignment_Aviation.png")){OtherSectors$Aviation <- 1}
  if (file.exists("TechnologyGraphs_TechnologyAlignment_Shipping.png")){OtherSectors$Shipping <- 1}
  # if (length(grep("Cement", FigNames$Fig))>0){OtherSectors$Cement <- 1}
  # if (length(grep("Steel", FigNames$Fig))>0){OtherSectors$Steel <- 1}
  # if (length(grep("Aviation", FigNames$Fig))>0){OtherSectors$Aviation <- 1}
  # if (length(grep("Shipping", FigNames$Fig))>0){OtherSectors$Shipping <- 1}

  
  # removes Other Sector Pages - materials
  if ((OtherSectors$Steel+OtherSectors$Cement==0)){
    text <- removetextlines("OtherSectorsMaterial")
  }
  
  # removes Other Sector Pages - transportation
  if ((OtherSectors$Aviation+OtherSectors$Shipping==0)){
    text <- removetextlines("OtherSectorsTransport")
  }  
  
  # Set Report Language
  replacelist <- colnames(RT)
  for (i in 1:length(replacelist)){
    text$text <- gsub(replacelist[i],RT[replacelist[i]][[1]], text$text)
  }
  
  text$text <- gsub("Languagechoose",Languagechoose, text$text)
  
  text$text <- gsub("SamplePort",PortfolioName,text$text)
  text$text <- gsub("SAMPLEPORT",PORTFOLIONAME,text$text)
  text$text <- gsub("CO2","CO\\\\textsubscript{2}",text$text)
  text$text <- gsub("Â°","°",text$text)
  
  # Copy in the graphics folder for the report
  # originalloc <- paste0(TemplatePath,"ReportGraphics/")
  # graphicsloc <- paste0(LanguageDirectory ,"/","ReportGraphics/")
  # flist <- list.files(originalloc, full.names = TRUE)
  # 
  # if(!dir.exists(file.path(graphicsloc))){
  #   dir.create(file.path(graphicsloc), showWarnings = TRUE, recursive = FALSE, mode = "0777")
  #   for (file in flist){file.copy(file, graphicsloc)}
  # }  
  
  # Save the template file
  TemplateNameNew <- paste0("Template_",PortfolioName,"_",Languagechoose)
  write.table(text, paste0(TemplateNameNew,".Rnw"),col.names = FALSE,row.names = FALSE,quote=FALSE,fileEncoding = "UTF-8")  
  
  # Create the PDF
  knit2pdf(paste0(TemplateNameNew,".Rnw"),compiler = "xelatex", encoding = 'UTF-8')
  # Delete remaining files and ReportGraphics Folder
  # unlink("ReportGraphics",recursive = TRUE)
  excessfileendings <- c(".log",".rnw",".tex",".aux")
  file.remove(paste0(TemplateNameNew,excessfileendings))
  # file.remove("FigureList.txt")
  
  # Rename output file
 
  file.rename(paste0(TemplateNameNew,".pdf"),paste0("AlignmentReport_",PortfolioName,".pdf"))
  
  return()
  
  
  
  
  
}

#-------- Prepare report text
preptranslations <- function(TranslationType,GraphTranslation, Languagechoose, Startyear){
  GT <- subset(GraphTranslation, select = c("TextLabel",Languagechoose))
  
  # GT <- subset(ReportTranslation, select = c("TextLabel",Languagechoose))
  
  GT <- setNames(data.frame(t(GT[,-1])), GT[,1])
  
  # a <- cat("\\%")
  GT <- as.data.frame(lapply(GT,function(x) if(is.character(x)|is.factor(x)) gsub("Startyear\\+5",as.character(Startyear+5),x) else x))  
  GT <- as.data.frame(lapply(GT,function(x) if(is.character(x)|is.factor(x)) gsub("Startyear",Startyear,x) else x))
  
  if (TranslationType == "Report"){
    GT <- as.data.frame(lapply(GT,function(x) if(is.character(x)|is.factor(x)) gsub("%",as.character("\\\\\\%"),x,fixed = TRUE) else x))
    GT <- as.data.frame(lapply(GT,function(x) if(is.character(x)|is.factor(x)) gsub("&",as.character("\\\\\\&"),x,fixed = TRUE) else x))
  }
  
  GT <- data.frame(lapply(GT, str_trim), stringsAsFactors = FALSE)
  GT <- data.frame(lapply(GT, as.character), stringsAsFactors=FALSE)
  
  
  return(GT)
  
}

#---------- Set Graphing Values-----
GraphValues <- function(){
  
  #Saturated colours
  RenewablesColour <<- "#b3de69"
  HydroColour <<- "#428bbd"
  NuclearColour <<- "#827ab8"
  GasCapColour<<-"grey75"
  CoalCapColour <<- "#252525"
  ElectricColour<<- "#69c454"
  HybridColour<<- "#00b7be"
  ICEColour<<- "#2F4F4F"   #"#ed1c24" #"#f93620"
  GasProdColour <<- "#ffb861"
  OilProdColour <<- "#c88450"
  CoalProdColour <<- "#835834"
  
  YourportColour <<- "#265b9b"   #"#2e4b6e"  #"#17224D"
  IndexColour <<-  "grey85"
  Tar2DColourBar <<- "#b3de69"
  Tar2DColour <<- "#a1c75e"
  goodexpColour <<- "#1F891F"
  badexpColour <<- "#ed1c24" #"#fb8072"
  ReqCapColour <<- "grey55"
  CurrCapColour <<- "grey75"
  AxisColour <<- "#17375e" #"#274F80"
  
  textsize <<- 8
  ppi <<- 600 #resolution of plots
  
}

# ------------ Sector Selector-----
SectorSelect <- function(TechToPlot){
  SectorToPlot <- "None"
  if (TechToPlot %in% c("RenewablesCap","HydroCap", "NuclearCap","CoalCap","GasCap")){
    SectorToPlot <- "Power" 
  }
  if (TechToPlot %in% c("ICE","Hybrid", "Electric")){
    SectorToPlot <- "Automotive" 
  }
  if (TechToPlot %in% c("Coal","Oil","Gas")){
    SectorToPlot <- "Fossil Fuels"
  }
  if(TechToPlot %in% c("Fossil Fuels","Automotive","Power", "Cement","Steel","Aviation","Shipping","OG","All")){
    SectorToPlot <- TechToPlot
  }
  
  return(SectorToPlot) 
}

# --------- Weighted Results----
WeightedResultsPrep <- function(BatchTest_PortSnapshots,BatchTest){
  
  Results <- subset(BatchTest, Year == Startyear+5 )#,select= c("PortName","Sector","Technology","MarketExposure","AUMExposure","Production","PortAUM"))
  Results <- subset(Results, !Technology %in% c("OilCap"))
  
  piesub_tech <- unique(subset(BatchTest_PortSnapshots,select=c("PortName","ISIN","piesector","PortWeight")))
  piesub_tech$piesector<-gsub("NonUtility Power", "Non-Utility Power", piesub_tech$piesector)
  piesub_tech$piesector[is.na(piesub_tech$piesector)] <- "Not Assessed"
  pieshares <- ddply(piesub_tech, .(PortName,piesector),summarize,Portfolio_weight=sum(PortWeight, na.rm=TRUE))
  pieshares$piesector[pieshares$piesector == "Utility Power"]<- "Power"
  pieshares <- subset(pieshares, pieshares$piesector %in% c("Fossil Fuels", "Automotive","Power"))
  
  df <- merge(Results,pieshares, by.x = c("PortName", "Sector"), by.y = c("PortName","piesector"), all.x = TRUE, all.y = FALSE)
  df$Portfolio_weight[is.na(df$Portfolio_weight)] <- 0
  
  df$SectorAUM <- df$PortAUM*df$Portfolio_weight
  
  SectorTotAUM <- unique(subset(df, select = c("PortName","Sector","SectorAUM")))
  SectorTotAUM <- ddply(SectorTotAUM, . (PortName),summarise, SectorTotAUM = sum(SectorAUM,na.rm = TRUE))
  df<-merge(df,SectorTotAUM, by="PortName")
  
  df$SectorCoverage <- df$SectorAUM/df$SectorTotAUM
  
  df$Production[df$Technology == "Coal"]<- df$Production[df$Technology == "Coal"]*24
  df$Production[df$Technology == "Oil"]<- df$Production[df$Technology == "Oil"]*6.12
  df$Production[df$Technology == "Gas"]<- df$Production[df$Technology == "Gas"]*0.0372
  
  ProdTotals <- ddply(df, .(PortName,Sector), summarise, SecProd = sum(Production, na.rm=TRUE))
  df <- merge(df, ProdTotals, by = c("PortName", "Sector"))
  
  df$TechShare <- df$Production/df$SecProd
  df$TechAUM <- df$TechShare*df$SectorAUM
  df$CoverageWeight <- df$TechAUM/df$SectorTotAUM
  
  df[is.na(df)]<- 0
  
  Results <- subset(df, select =c("PortName","Technology","Sector","SectorTotAUM","PortAUM","CoverageWeight"))
  
  WeightedResults <- ddply(Results, .(Technology),summarise, CoverageWeight= weighted.mean(CoverageWeight,PortAUM,na.rm = TRUE))
  WeightedResults$CoverageWeight <- WeightedResults$CoverageWeight/sum(WeightedResults$CoverageWeight)  
  WeightedResults$PortName <- "WeightedResults" 
  
  # ResultsDF <- subset(Results, select = c("PortName","Technology","CoverageWeight"))
  # ResultsDF <- rbind(ResultsDF,WeightedResults)
  
  
  ResultsDF <- WeightedResults
  return(ResultsDF)
}

#--------Ranking Results----
RankingResultsPrep <- function(BatchTest,Results_combin_EQ){
  
  Results <- BatchTest
  Results <- rbind(BatchTest,Results_combin_EQ)
  
  # AUMData<-Results[Results$PortName %in% Results$PortName,]
  AUMs <- ddply(Results, .(PortName),summarise, AUM = sum(PortAUM, na.rm=FALSE))
  
  Heatmap <- subset(Results, Results$Year == Startyear+5  ,select = c("PortName","Technology","BenchmarkRegion","MarketExposure", "AUMExposure"))
  
  Heatmap$MarketExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")] <- Heatmap$AUMExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")]
  # After getting the AUM values, remove that vector from the dataframe
  Heatmap <- Heatmap[,names(Heatmap) != 'AUMExposure']
  Heatmap$MarketExposure <- as.numeric(Heatmap$MarketExposure)
  
  Heatmap$MarketExposure <- Heatmap$MarketExposure*100
  
  Heatmap <- subset(Heatmap, select = c("PortName","Technology","MarketExposure"))
  
  ComparisonTable <- reshape(Heatmap, v.names = "MarketExposure",idvar=c("PortName"),timevar="Technology", direction= "wide")
  colnames(ComparisonTable) <- gsub("MarketExposure.","",colnames(ComparisonTable))
  ComparisonTable$OilCap <- NULL
  
  # issues with rank with positive and negative values, compare a positive table only
  ComparisonPositive <- ComparisonTable
  ComparisonPositive[2:12] <- ComparisonPositive[2:12]+max(abs(ComparisonPositive[2:12]),na.rm = TRUE)+1
  
  # Order the table for Green vs Brown Tech
  goodtech <- c("RenewablesCap","HydroCap","NuclearCap","Hybrid","Electric")  
  badtech <- c("CoalCap","GasCap","ICE","Oil","Gas","Coal")
  
  CompGood <- ComparisonPositive[colnames(ComparisonPositive) %in% c("PortName",goodtech)]
  CompBad <- ComparisonPositive[colnames(ComparisonPositive) %in% c("PortName","CoalCap","GasCap","ICE")]
  CompFF <- ComparisonPositive[colnames(ComparisonPositive) %in% c("PortName","Oil","Gas","Coal")]
  
  rownames(CompGood)<- CompGood$PortName
  rownames(CompBad)<- CompBad$PortName
  rownames(CompFF)<- CompFF$PortName
  
  rankingbad <- data.frame(apply(CompBad[2:4],2,rank, ties.method="min",na.last="keep"))
  rankingff <- data.frame(apply(CompFF[2:4],2,rank, ties.method="min",na.last="keep"))
  rankinggood <- data.frame(apply(-CompGood[2:6],2,rank, ties.method="min",na.last="keep"))
  
  rankingbad$PortName <- row.names(rankingbad)
  rankinggood$PortName <- row.names(rankinggood)
  rankingff$PortName <- row.names(rankingff)
  
  rankingtable <- merge(rankingbad,rankingff, by="PortName")
  rankingtable <- merge(rankingtable, rankinggood,by="PortName")
  
  rankingtable <- subset(rankingtable, select = colnames(ComparisonTable))
  
  results <- list(rankingtable,ComparisonTable, AUMs)
}

#--------OS Data Prep
matchOS <- function(OSdata, PortSnapshot){
  
  OSdatared <- unique(subset(OSdata, select = c("Issuer","Sector","ISIN","Weighted.Emissions.Factor","Weighted.Target.Emissions.Factor")) )
  
  PortSnapshot$Sector <- NULL
  
  OS <- merge(PortSnapshot,OSdatared,by = "ISIN")
  
  # OSset <- subset(OS, Sector == Sectorchoose)
  
  WEF <- ddply(OS,.(PortName,Sector),summarise,
               EmissionsFactor =weighted.mean(Weighted.Emissions.Factor,AUM),
               TargetEmissionsFactor =weighted.mean(Weighted.Target.Emissions.Factor,AUM))
  # Returns the WEF for each company by sector
  return(WEF)    
}

# ------------ Other Sector Plots------------ 
other_sector_chart <- function(SectorToPlot){
  
  theme_linecharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(), 
          axis.text.x=element_text(face="bold",colour="black",size=textsize),
          axis.text.y=element_text(face="bold",colour="black",size=textsize),
          axis.title.x=element_text(face="bold",colour="black",size=textsize),
          axis.title.y=element_text(face="bold",colour="black",size=textsize),
          axis.line = element_line(colour = "black",size=1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          #panel.background = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          # legend.position=c(0.5,0),#legend.position = "none",
          legend.position = "none",
          legend.direction="horizontal",
          legend.text = element_text(face="bold",size=textsize,colour="black"),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.key.size=unit(0.4,"cm"),
          #legend.title=element_blank(),
          legend.title = element_text(colour = "black", size = textsize),
          legend.key = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = unit(c(1,1, 0, 0), "lines")
          # plot.margin = unit(c(1,1, 5, 2), "lines")
    )
  }    
  
  
  
  check = 0
  EQPlotData <- subset(EQ_OS_WEM, EQ_OS_WEM$PortName == PortfolioName & EQ_OS_WEM$Sector == SectorToPlot)
  if(nrow(EQPlotData) == 1){
    EQPlotData$ChartType <- "EQ"
    check = check+0.5}
  
  # CBPlotData <- subset(CB_OS_WEM, CB_OS_WEM$PortName == PortfolioName & CB_OS_WEM$Sector == SectorToPlot)
  # if(nrow(CBPlotData) == 1){
  #   CBPlotData$ChartType <- "CB"
  #   check = check+1.5}
  
  if (check == 2){PlotData <- rbind(EQPlotData,CBPlotData)} else{
    if (check == 0.5){PlotData <- EQPlotData}else{
      if (check == 1.5){PlotData <-CBPlotData}}}
  
  if (check >0){
    
    PlotData<- merge(PlotData,OSTargets, by="Sector")
    PlotData <- PlotData[,!colnames(PlotData) %in% c("Sector","PortName", "TargetEmissionsFactor")]
    
    df <- melt(PlotData, id.vars = c("ChartType", "EmissionsFactor"))
    df <- df[with(df,order(ChartType)),]
    
    df$Year <- Startyear:(Startyear+5)
    df$value <- df$EmissionsFactor*df$value
    
    OtherSectorUnits <- data.frame("Sector" = c("Cement","Steel","Aviation"), "Units"= c("t CO2 / t Cement","t CO2 / t Steel","CO2/passenger km"))
    
    year_lab <- "Year" #GT["Year"][[1]]
    # ylabel <- paste0(GT["OtherSectorLabel"][[1]]," (",GT[paste0(SectorToPlot,"Units")][[1]],")")#paste0(GT["OtherSectorLabel"][[1]]," (",GT[paste0(SectorToPlot,"Units")][[1]],")")
    ylabel <- paste0("Emissions Intensity (",OtherSectorUnits$Units[OtherSectorUnits$Sector == SectorToPlot],")")
    
    df$ChartType<-factor(df$ChartType)
    # df <- df[with(df,order(Year)),]
    
    # dfCB <- df[df$ChartType == "CB",]
    dfEQ <- df[df$ChartType == "EQ",]
    
    outputplot <- ggplot()
    
    if (exists("dfCB")){if (nrow(dfCB)>0){outputplot<-outputplot+
      geom_line(data=dfCB,aes(x=Year,y=value,colour=Tar2DColour,group=1),size=1.5,linetype=1)+
      annotate(geom = "point",y=dfCB$value[dfCB$Year==Startyear],x=dfCB$Year[dfCB$Year==Startyear],size=5,colour=YourportColour,fill=YourportColour, shape=22)}}
    if (nrow(dfEQ)>0){outputplot<-outputplot+geom_line(data=dfEQ,aes(x=Year,y=value,colour=Tar2DColour,group=1),size=1.5,linetype=2)+
      annotate(geom = "point",y=dfEQ$value[dfEQ$Year==Startyear],x=dfEQ$Year[dfEQ$Year==Startyear],size=5,colour=YourportColour,fill=YourportColour, shape=22)}
    
    outputplot<-outputplot+
      scale_fill_identity(name = "", guide = 'legend',labels = c("Exposure gap","Current capacity + planned additions")) +
      scale_colour_manual(name="",guide='legend',values= c(Tar2DColour),labels=c(PortfolioName,"2°C Benchmark"))  +
      xlab(year_lab) + ylab(ylabel) + # Set axis labels
      # legend(values=legelabels)+
      scale_x_continuous(breaks=seq(Startyear,max(df$Year),1),expand=c(0,0))+
      scale_y_continuous(expand=c(0,0))+
      expand_limits(x=c(Startyear-0.5,Startyear+4.5),y= c(.95*min(df[,c(4)], na.rm=TRUE),1.05*max(df[,c(4)], na.rm=TRUE)))+
      theme_linecharts()  
    
    
    
    ggsave(filename=paste0("TechnologyGraphs_TechnologyAlignment_",SectorToPlot,'.png'),bg="transparent",height=3.6,width=3.6,plot=outputplot,dpi=ppi)
    InPort<<-1
  }else{
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    Label = "There are no holdings of this technology in this portfolio"
      
    
    outputplot <- ggplot()+
      annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,30), size=4)+
      geom_blank()+
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "white",colour = NA))
    
    # ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",SectorToPlot,'_OtherSectors.png', sep=""),bg="transparent",height=3.6,width=3.6,plot=outputplot,dpi=ppi)
    ggsave(filename=paste0("TechnologyGraphs_TechnologyAlignment_",SectorToPlot,'.png'),bg="transparent",height=3.6,width=3.6,plot=outputplot,dpi=ppi)
   
    InPort<<-0
  }    
  
  return()
}

# ------------ Shipping Plots------------ 
shipping_chart <- function(SectorToPlot= "Shipping"){
  
  EQPortSnapshot <- PortSnapshot
  
  theme_barcharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(), 
          axis.text.x=element_text(face="bold",colour="black",size=textsize),
          axis.text.y=element_text(face="bold",colour="black",size=textsize),
          axis.title.x=element_blank(),
          axis.title.y=element_text(face="bold",colour="black",size=textsize),#element_text(face="bold",colour="black",size=textsize),
          axis.line = element_line(colour = "black",size=1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          legend.position=c(0.5,-.3),
          legend.direction="horizontal",
          legend.text = element_text(face="bold",size=textsize,colour="black"),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.key.size=unit(0.4,"cm"),
          legend.title=element_blank(),
          legend.key = element_blank(),
          plot.margin = unit(c(0.6,0, 2.5, 0), "lines"),
          plot.background = element_rect(fill = "transparent",colour = NA)
    )
  }
  
  EQShipsPort<- subset(EQPortSnapshot, EQPortSnapshot$ISIN %in% ShippingData$ISIN)
  
  CBShipsPort <- data.frame()
  noships <- nrow(EQShipsPort) +nrow(CBShipsPort)
  
  if(noships >0){
    
    EQShips <- ShippingData[ShippingData$ISIN %in% EQShipsPort$ISIN,]
    CBShips <- ShippingData[ShippingData$ISIN %in% CBShipsPort$ISIN,]
    EQShipsinPort <- merge(EQShips, EQShipsPort, by="ISIN")
    if(nrow(CBShips) > 0){CBShipsinPort <- merge(CBShips, CBShipsPort, by="ISIN")}else{CBShipsinPort <- data.frame()}
    
    ship_summary <- function(ShipsinPort,ClassificationName){
      if(ClassificationName == "CB"){
        # Weighted Approach (CB and potentially EQY)
        ShipAUM <- sum(CBShipsinPort$AUM[ShipsinPort$Year == Startyear],na.rm = TRUE)
        ShipsinPort <- CBShipsinPort
        ShipsinPort$ShipShare <- ShipsinPort$AUM/ShipAUM
        ShipsinPortlong <- melt(ShipsinPort[,c("Year","ShipShare",grep("Perc",colnames(ShipsinPort), value = TRUE))], id.var=c("Year","ShipShare"))
        ShipsinPortlong$TechShare <- ShipsinPortlong$ShipShare * ShipsinPortlong$value
        ShipsinPort <- aggregate(ShipsinPortlong["TechShare"], by = ShipsinPortlong[,c("Year","variable")], FUN = sum)
        ShipsinPort$variable <- strtrim(ShipsinPort$variable,5)
        ShipsinPort$Type <- "Corporate Bonds"
        return(ShipsinPort)
      }else{
        #Ownership Approach
        EQShipsinPort <- aggregate(EQShipsinPort["Position"], by = EQShipsinPort[,c("Issuer","GHG_A", "GHG_B", "GHG_C", "GHG_D", "GHG_E", "GHG_F", "GHG_G", "Year", "TotalShares")], FUN = sum)
        ShipsinPort <- EQShipsinPort
        ShipsinPort$ShipShare <- ShipsinPort$Position / ShipsinPort$TotalShares
        ShipsinPortlong <- melt(ShipsinPort[,c("Year","ShipShare",grep("GHG_",colnames(ShipsinPort), value = TRUE))], id.var=c("Year","ShipShare"))
        ShipsinPortlong$PortfolioProduction <- ShipsinPortlong$ShipShare * as.numeric(ShipsinPortlong$value)
        ShipsinPort <- aggregate(ShipsinPortlong["PortfolioProduction"], by = ShipsinPortlong[,c("Year","variable")], FUN = sum)
        ShipsinPortRef <- ddply(ShipsinPortlong,.(Year),summarize, TotalShips = sum(PortfolioProduction, na.rm = TRUE))
        ShipsinPort <- merge(ShipsinPort,ShipsinPortRef, by = "Year", all.x = TRUE)
        ShipsinPort$TechShare <- ShipsinPort$PortfolioProduction / ShipsinPort$TotalShips
        ShipsinPort <- ShipsinPort[,c("Year", "variable", "TechShare")]
        ShipsinPort$Type <- "Equity"
        return(ShipsinPort)
      }
    }
    
    #Market
    ShipsListedMarket <- subset(ShippingData, Company == "ListedMarket", select = c("Year",grep("Perc",colnames(ShippingData), value = TRUE)))
    ShipsListedMarket <- melt(ShipsListedMarket, id.var=c("Year"))
    ShipsListedMarket$variable <- strtrim(ShipsListedMarket$variable,5)
    ShipsListedMarket$Type <- "Equity Market"
    ShipsListedMarket <- rename(ShipsListedMarket, c("value" = "TechShare"))
    
    ShipsSummary <- ShipsListedMarket
    
    #CB
    if(dim(CBShipsinPort)[1]>0){
      ShipsCB <- ship_summary(CBShipsinPort, "CB")
      ShipsSummary <- rbind(ShipsSummary, ShipsCB)
    }
    
    #EQ
    if(dim(EQShipsinPort)[1]>0){
      ShipsEQ <- ship_summary(EQShipsinPort, "EQ")
      ShipsSummary <- rbind(ShipsSummary, ShipsEQ)
    }
    
    if(length(unique(ShipsSummary$Type)) > 2){
      ShipsSummary <- subset(ShipsSummary, Year == Startyear+5)
    }
    
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    
    ShipsSummary$Order <- str_sub(ShipsSummary$variable,-1,-1)
    ShipsSummary$variable <- paste0("GHG ",str_sub(ShipsSummary$variable,-1,-1)," Score")
    ShipsSummary$Name <- paste0(ShipsSummary$Type," ",ShipsSummary$Year)
    ShipColourPalette <- c("#D73027", "#FC8D59", "#FEE08B", "#FFFFBF", "#D9EF8B", "#91CF60", "#1A9850")
    ShipsSummary$Order <- factor(ShipsSummary$Order, levels=rev(c("A","B","C","D","E","F","G")))
    ShipsSummary <- ShipsSummary[order(ShipsSummary$Order),]
    ShipsSummary$Name <- wrap.labels(ShipsSummary$Name,8)
    
    ylabel <- "% Share in this Portfolio"
    
    shippingchart<- ggplot(ShipsSummary, aes(Name, TechShare,fill=Order))+
      geom_bar(stat = "identity",width = .6, show.legend = TRUE)+
      scale_fill_manual(labels=unique(ShipsSummary$Order),values=ShipColourPalette)+
      scale_y_continuous(expand=c(0,0), limits = c(0,1.0001), labels=percent)+
      expand_limits(0,0)+
      guides(fill=guide_legend(nrow = 1))+
      ylab(ylabel)+
      theme_barcharts()
    
    # print(PlotData)
    # ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_Shippingbar.png"),bg="transparent",height=3.6,width=3.6,plot=shippingchart,dpi=ppi)
    ggsave(filename=paste0("TechnologyGraphs_TechnologyAlignment_",SectorToPlot,".png"),bg="transparent",height=3.6,width=3.6,plot=shippingchart,dpi=ppi)
    InPort <<- 1
  }else{
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    Label = "Your portfolio contains no holdings in this sector"
    
    
    shippingchart <- ggplot()+
      annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,30), size=4)+
      geom_blank()+
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA))
    
    ggsave(filename=paste0("TechnologyGraphs_TechnologyAlignment_Shipping.png"),bg="transparent",height=3.6,width=3.6,plot=shippingchart,dpi=ppi)
    InPort <<-0
  }
  
  return()}

# ------------- PORT DATA PIE --------------- 
port_pie <- function(){
  
  Port <- piechartdata
  Port <- subset(Port, select = c("Equity","Others"))
  
  if(nrow(Port)>0){
    SumPort <- sum(Port[1:2,1],na.rm = TRUE)
    Port<- melt(Port)
    colnames(Port)<- c("Classification","value")
    Port$perc <- round(Port$value/SumPort,2)*100
    
    Palette <- data.frame(Classification = c("Equity","Others"),Colour=c("dodgerblue1","grey"))
    Palette$Colour <- as.character(Palette$Colour)
    Port <- merge(Port,Palette, by="Classification")
    
    # Port$Label <- lapply(Port$Classification, function(x) GT[paste0(x,"Title")][[1]])
    
    
    PieChart<- ggplot(Port, aes(x="", y=perc, fill=Classification))+
      geom_bar(stat = "identity",color=NA, width = 0.5)+
      geom_bar(stat = "identity",color='white',show.legend = FALSE, lwd = .25,width = 1)+
      scale_fill_manual(values= Port$Colour,labels=paste(Port$Classification,": ",Port$perc,"%",sep=""))+
      guides(fill = guide_legend(override.aes = list(colour = NULL)))+
      theme(axis.ticks=element_blank(), axis.text.y=element_blank(),axis.title=element_blank(),
            axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.line = element_blank(), plot.margin = unit(c(0,0, 0, 0), "lines"),
            plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA),
            legend.background = element_rect(fill = "transparent",colour = NA),
            legend.text = element_text(size=textsize,family = "Arial",colour="black"),
            legend.key.size=unit(0.4,"cm"),legend.title=element_blank())
    
    PieChart <- PieChart + coord_polar("y", start=0, direction=-1)#+ xlab('') #+  ylab('')
    
    # ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",'Portpie.png',sep=""),bg="transparent",height=2,width=4,plot=PieChart,dpi=ppi)
    ggsave(filename=paste0("PortfolioOverviewGraphs_Portfolio_InvestmentClassAssessment.png"),bg="transparent",height=2,width=4,plot=PieChart,dpi=ppi)
    
      }
  
}

# ------------- PIE CHART ------------------- 
pie_chart <- function(ChartType){
  
  if (nrow(PortSnapshot)>0){
    
    colnames(PortSnapshot)[colnames(PortSnapshot) %in% "IssLvlPortWeight"] <- "PortWeight"
    PortSnapshotSub <- subset(PortSnapshot, CNTRY_OF_DOMICILE %in% IndexUniverses[,names(IndexUniverses) == eval(paste0(CompanyDomicileRegionchoose,"_ISO"))])
    piesub_tech <- unique(subset(PortSnapshotSub,select=c("ISIN","piesector","PortWeight")))
    
    piesub_tech$piesector<-gsub("NonUtility Power", "Non-Utility Power", piesub_tech$piesector)
    piesub_tech$piesector[is.na(piesub_tech$piesector)] <- "Not Assessed"
    # piesub_tech$piesector[piesub_tech$piesector] <- "Not Assessed"
    
    
    #OUT OF REGION <- Anti-PortSnapshotSub if Region != Global/GLobalAgg
    piesub_tech$piesector <- revalue(piesub_tech$piesector,c("Metal-Iron" = "Iron & Steel","NonOG Production" = "Fossil Fuels","Bldg Prod-Cement/Aggreg" = "Building Materials & Fixtures", "Oil&Gas"= "Fossil Fuels","Coal"="Fossil Fuels", "Transport-Marine" = "Marine Transportation","Metal-Aluminum"="Aluminum", "Steel-Producers" = "Iron & Steel", "Transport-Air Freight"= "Airlines"),warn_missing = FALSE)
    
    pieshares <- ddply(piesub_tech, .(piesector),summarize,Portfolio_weight=sum(PortWeight, na.rm=TRUE))
    pieshares$label <- "Total Portfolio"
    
    # Create a sub dataframe for plotting
    secfull <- c("Utility Power", "Automotive", "Fossil Fuels", "Non-Utility Power", "Airlines", "Building Materials & Fixtures","Aluminum", "Iron & Steel", "Marine Transportation","Not Assessed")
    secsmiss <- setdiff(secfull,unique(pieshares$piesector))
    
    weights <- rep(0,length(secsmiss))
    label <- rep("Total Portfolio",length(secsmiss))
    missingdf <- data.frame(secsmiss,weights,label)
    names(missingdf) <- names(pieshares)
    pieshares <- rbind(pieshares,missingdf)
    pieshares <- within(pieshares,piesector <- factor(piesector, levels=secfull))
    pieshares$piesector <- revalue(pieshares$piesector,c("Building Materials & Fixtures" = "Building Materials"))
    pieshares <- pieshares[with(pieshares,order(label,piesector)),]
    pieshares <- pieshares[pieshares$label %in% "Total Portfolio",]
    pieshares$perc <- round(pieshares$Portfolio_weight*100,1)
    Palette <- c("#274f80","#30629e", "#3974bc", "#5288cA", "#934d1d","#D26E2A", "#ED7D31", "#F1A78A","#F5C7B8", "#E5E5E5", "#b7b7b7") #blue #b7b7b7
    
    PieChart<- ggplot(pieshares, aes(x="", y=Portfolio_weight, fill=piesector))+
      geom_bar(stat = "identity",color=NA, width = 0.5)+
      geom_bar(stat = "identity",color='white',show.legend = FALSE, lwd = .25,width = 1)+
      scale_fill_manual(values= Palette,labels=paste(pieshares$piesector," ",pieshares$perc,"%",sep=""))+
      guides(fill = guide_legend(override.aes = list(colour = NULL)))+
      theme(axis.ticks=element_blank(), axis.text.y=element_blank(),axis.title=element_blank(),
            axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.line = element_blank(), plot.margin = unit(c(0,0, 0, 0), "lines"),
            plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA),
            legend.background = element_rect(fill = "transparent",colour = NA),
            legend.text = element_text(size=textsize,family = "Arial",colour="black"),
            legend.key.size=unit(0.4,"cm"),legend.title=element_blank())
    
    PieChart <- PieChart + coord_polar("y", start=0, direction=-1)+ xlab('') +  ylab('')
    
    # ggsave(filename=paste0(plotnumber,"_",ChartType,'_pie.png',sep=""),bg="transparent",height=2,width=4,plot=PieChart,dpi=ppi)
    ggsave(filename=paste0("PortfolioOverviewGraphs_Portfolio_SectorWeights.png"),bg="transparent",height=2,width=4,plot=PieChart,dpi=ppi)
    
  }else{
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    if (ChartType == "CB"){
      Label <- "This Portfolio has no Corporate Debt Holdings"
    }else{Label <- "This Portfolio has no Equity Holdings"}
    
    
    outputplot <- 
      ggplot()+
      annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,15), size=5)+
      geom_blank()+
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA))
    
    ggsave(filename=paste0("PortfolioOverviewGraphs_Portfolio_SectorWeights.png"),bg="transparent",height=2.6,width=4,plot=outputplot,dpi=ppi)
  }
  
  return()
}

# ------------- STACKED BAR CHARTS ---------- 
stacked_bar_chart <- function(ChartType,SectorToPlot){
  
  # combin <- CBCombin
  # ChartType <- "CB"
  # WeightedResults <- CBWMCoverageWeight
  # SectorToPlot <- "Fossil Fuels"
  # plotnumber=99
  combin <- Results_combin_EQ 
  
  theme_barcharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(), 
          axis.text.x=element_text(face="bold",colour="black",size=textsize),
          axis.text.y=element_text(face="bold",colour="black",size=textsize),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),#element_text(face="bold",colour="black",size=textsize),
          axis.line = element_line(colour = "black",size=1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          legend.position=c(0.5,-.3),
          legend.direction="horizontal",
          legend.text = element_text(face="bold",size=textsize,colour="black"),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.key.size=unit(0.4,"cm"),
          legend.title=element_blank(),
          legend.key = element_blank(),
          plot.margin = unit(c(0.6,1.0, 2.5, 0), "lines"),
          plot.background = element_rect(fill = "transparent",colour = NA)
    )
  }
  
  wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
  wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
  
  WeightedResults$PortName <- NULL
  
  # if(SectorToPlot == "All"){cbondsgo <-nrow(combin)}
  if(SectorToPlot == "Fossil Fuels"){combin <- subset(combin, combin$Sector %in% c("Fossil Fuels","Oil&Gas","Coal"))}else{combin <- subset(combin, combin$Sector == SectorToPlot)}
  
  combin <- combin[!combin$Technology %in% "OilCap",]
  
  ProductionMix_5yrs <- subset(combin, Year==Startyear+5 )#& BenchmarkRegion==BenchmarkRegionchoose & CompanyDomicileRegion == CompanyDomicileRegionchoose & Scenario == Scenariochoose & Sector == SectorToPlot)
  
  if (sum(ProductionMix_5yrs$Production,na.rm = T) > 0){
    
    
    if (SectorToPlot == "Fossil Fuels"){
      ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Coal"]<- ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Coal"]*24
      ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Oil"]<- ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Oil"]*6.12
      ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Gas"]<- ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Gas"]*0.0372
      ProductionMix_5yrs$RefTechProd[ProductionMix_5yrs$Technology == "Coal"]<- ProductionMix_5yrs$TargetProductionAUMIntensity[ProductionMix_5yrs$Technology == "Coal"]*24
      ProductionMix_5yrs$RefTechProd[ProductionMix_5yrs$Technology == "Oil"]<- ProductionMix_5yrs$TargetProductionAUMIntensity[ProductionMix_5yrs$Technology == "Oil"]*6.12
      ProductionMix_5yrs$RefTechProd[ProductionMix_5yrs$Technology == "Gas"]<- ProductionMix_5yrs$TargetProductionAUMIntensity[ProductionMix_5yrs$Technology == "Gas"]*0.0372
      
      ProductionMix_5yrs <- ddply(ProductionMix_5yrs, .(Year, Sector, Technology,Scenario), summarise,
                                  PortProduction= sum(Production),
                                  RefProduction = sum(RefTechProd))
    }else{
      ProductionMix_5yrs <- ddply(ProductionMix_5yrs, .(Year, Sector, Technology,Scenario), summarise,
                                  PortProduction= sum(Production),
                                  RefProduction = sum(TargetProductionAlignment))}
    
    ProductionMix_5yrs <- merge(ProductionMix_5yrs,WeightedResults, by="Technology")
    
    ProductionMix_5yrs <- melt(ProductionMix_5yrs, id = c( "Year","Technology","Scenario","Sector"))
    SectorTotals <- ddply(ProductionMix_5yrs,.(Year,Sector,variable), summarise,SectorTotal = sum(value))
    ProductionMix_5yrs <- merge(ProductionMix_5yrs,SectorTotals)
    
    ProductionMix_5yrs$TechShare <- ProductionMix_5yrs$value/ProductionMix_5yrs$SectorTotal
    
    ProductionMix_5yrs <- subset(ProductionMix_5yrs, select= c("Sector","Technology","variable","TechShare"))
    ProductionMix_5yrs$Technology <- gsub("Cap","",ProductionMix_5yrs$Technology)
    ProductionMix_5yrs$variable <- as.character(ProductionMix_5yrs$variable)
    ProductionMix_5yrs$variable[ProductionMix_5yrs$variable %in% "PortProduction"] <- "Portfolio"
    ProductionMix_5yrs$variable[ProductionMix_5yrs$variable %in% "RefProduction"] <- "2° Target"
    ProductionMix_5yrs$variable[ProductionMix_5yrs$variable %in% "CoverageWeight"] <- "Average Portfolio"
    
    if (SectorToPlot == "Automotive"){ 
      technologyorder <-c("Electric","Hybrid","ICE")
      techorder <- data.frame(order=c(1,2,3),Technology= technologyorder)
      colours <- factor(c(ICEColour,HybridColour,ElectricColour))
      # ylabel <- GT["StackedBarYLabel_Automotive"][[1]]
    }
    
    if (SectorToPlot == "Power"){
      # ylabel <- GT["StackedBarYLabel_Power"][[1]]
      technologyorder <- c("Coal","Gas","Nuclear","Hydro","Renewables")
      techorder <- data.frame(order=c(1,2,4,3,5),Technology= technologyorder)
      colours <- factor(c(RenewablesColour,HydroColour,NuclearColour,GasCapColour,CoalCapColour))}
    
    if (SectorToPlot == "Fossil Fuels"){
      # ylabel <- GT["StackedBarYLabel_FF"][[1]]
      technologyorder <- c("Coal","Gas","Oil")
      techorder <- data.frame(order=c(1,2,3),Technology= technologyorder)
      colours <- factor(c(CoalProdColour,GasProdColour,OilProdColour))
    }  
    colourdf <- data.frame(colours, Technology = technologyorder)
    
    PlotData <- ProductionMix_5yrs
    
    PlotData <- subset(PlotData, !PlotData$variable == "SamplePort")
    
    PlotData <- merge(PlotData,colourdf, by="Technology")
    orderofchart <- c("2° Target","Portfolio","Average Portfolio")
    PlotData$variable <- factor(PlotData$variable, levels=orderofchart)
    PlotData$Technology <- factor(PlotData$Technology, levels=technologyorder)
    PlotData <- PlotData[order(PlotData$Technology,PlotData$variable),]
    PlotData$variable <- wrap.labels(PlotData$variable,20)
    
    PlotData$Sector <- NULL
    
    # LanguageLabels <- GT[unique(paste0("T_",PlotData$Technology))]
    if (SectorToPlot == "Fossil Fuels"){PlotData$Label <- paste0(PlotData$Technology,"Prod")}else{PlotData$Label<- PlotData$Technology}
    if (SectorToPlot == "Power"){PlotData$Label <- paste0(PlotData$Label,"Cap")}
    
    # PlotData$Language <- t(GT[paste0("T_",PlotData$Label)])[,1]
    
    stackedbarchart_plot<- ggplot(PlotData, aes(variable, TechShare,fill=rev(Technology)))+
      geom_bar(stat = "identity",width = .6)+
      scale_fill_manual(labels=unique(rev(PlotData$Label)),values=unique(as.character((PlotData$colours))))+
      scale_y_continuous(expand=c(0,0), limits = c(0,1.0001), labels=percent)+
      expand_limits(0,0)+
      guides(fill=guide_legend(nrow = 1))+
      theme_barcharts()+ 
      coord_flip()
    
    if(SectorToPlot == "Fossil Fuels"){SectorToPlot<- "FossilFuels"}
    
    # print(PlotData)
    # ggsave(filename=paste0(plotnumber,"_EQ_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=1.8,width=7.5,plot=stackedbarchart_plot,dpi=ppi)
    
    
  }else{
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    Label <- "Your Portfolio contains no holdings in this sector" 
    
    stackedbarchart_plot <- ggplot()+
      annotate(geom = "text", x=0,y=0, label=Label, size=4)+
      geom_blank()+
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA))
    
    if(SectorToPlot == "Fossil Fuels"){SectorToPlot<- "FossilFuels"}
    # ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=1.8,width=7.5,plot=outputplot,dpi=ppi)
    
  }
  
  
  # ggsave(filename=paste0(plotnumber,"_EQ_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=1.8,width=7.5,plot=stackedbarchart_plot,dpi=ppi)
  ggsave(filename=paste0("SectorGraphs_",SectorToPlot,'Sector_TechnologyMix.png'),bg="transparent",height=1.8,width=7.5,plot=stackedbarchart_plot,dpi=ppi)
  return() 
}

# ------------- MINI LINE CHARTS ------------ 
mini_line_chart <- function(ChartType, TechToPlot, SectorToPlot){
  
  combin <- Results_combin_EQ
  # print(SectorToPlot)
  
  # combin <- CBCombin
  # TechToPlot <- "Oil"
  # SectorToPlot <- "Fossil Fuels"
  # ChartType <- "CB"
  
  
  theme_linecharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(), 
          axis.text.x=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.text.y=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.title.x=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.title.y=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.line = element_line(colour = AxisColour,size=1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          #panel.background = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          # legend.position=c(0.5,-.4),#legend.position = "none", 
          legend.position = "none", 
          legend.direction="horizontal",
          legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.key.size=unit(0.4,"cm"),
          #legend.title=element_blank(),
          legend.title = element_text(colour = AxisColour, size = textsize),
          legend.key = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = unit(c(1,1, 0, 0), "lines")
    )
  }    
  
  production <- subset(combin, Technology %in% TechToPlot)
  production <- rename(production, c("WtProduction"="Production"),warn_missing = FALSE)
  
  if ((sum(production$Production, na.rm = TRUE)>0 | SectorToPlot == "Fossil Fuels") & nrow(combin)>0){
    
    if (ChartType == "EQ"){
      LineData <- subset(combin, Technology %in% TechToPlot )#& BenchmarkRegion %in% BenchmarkRegionchoose & CompanyDomicileRegion %in% CompanyDomicileRegionchoose & Scenario %in% Scenariochoose)  
      if (SectorToPlot == "Fossil Fuels"){
        LineData <- subset(LineData, select = c("Sector","Year","Production","TargetProductionAUMIntensity"))
        names(LineData)[names(LineData)=="TargetProductionAUMIntensity"] <- "TargetProductionAlignment"
      } else{
        LineData <- subset(LineData, select = c("Sector","Year","Production","TargetProductionAlignment"))
        
      }
      
      names(LineData)[names(LineData)=="TargetProductionAlignment"] <- "Target"
      names(LineData)[names(LineData)== "Production"] <- "Portfolio"
      
      sectors <- c("Automotive", "Fossil Fuels", "Power")
      axislabels <- c("Cars Produced", "Production", "Capacity") #c(GT["Cars"][[1]], GT["FossilFuels_Unit"][[1]], GT["Power_Unit"][[1]])
      lookup <- data.frame(sectors,axislabels)
      TechLabel <- gsub("Cap","",TechToPlot)   
      axislabel <- paste(TechLabel,lookup$axislabels[grep(SectorToPlot, lookup$sectors)])
      
      if(SectorToPlot == "Automotive"){axislabel <- TechLabel}
      
      # Scaling and Labelling the Y axis
      maxval <- max(LineData[,4],LineData[,3],na.rm=TRUE)
      
      magnitude_scale <- c(1,1,1e3,1e6,1e9)
      power_units <- c("kW","MW","GW","TW","Error_powertoohigh")
      car_units <- c("","","thousand","million","billion")
      ff_units <- c("","","thousand","million","billion")
      ff_units <- paste0(ff_units," ","barrels")
      coal_units <- c("","t","kt","MT","GT")
      oil_units <- c("","","thousand","million","billion")
      oil_units <- paste0(oil_units," ","barrels")
      gas_units <- c("","","thousand","million","billion")
      gas_units <- paste0(gas_units, " m²")
      unit_lookup <- data.frame(car_units,ff_units,power_units,coal_units,oil_units,gas_units)
      # ff_sectors <- c(GT["T_Coal"][[1]],GT["T_Oil"][[1]],GT["T_Gas"][[1]])
      ff_sectors <- c("Coal","Oil","Gas")
      sectors <- cbind(sectors, ff_sectors)
      unit_lookup <- setNames(unit_lookup,sectors)
      
      # Scales the Data to the correct units based on the maximum value.
      max_magnitude <- findInterval(maxval,magnitude_scale)
      if(max_magnitude == 0){max_magnitude <- 2}
      LineData$Portfolio <- LineData$Portfolio /magnitude_scale[max_magnitude]
      LineData$Target <- LineData$Target/magnitude_scale[max_magnitude]
      
      # Looks up the units within the correct line in the unit_lookup dataframe and sets the labels
      if (SectorToPlot == "Fossil Fuels")  {unit_search <- TechToPlot} else{
        unit_search <- SectorToPlot
      }
      
      unitlabel <- paste("(",unit_lookup[unit_search][max_magnitude,],")",sep="")  
      if (unitlabel == "()" & SectorToPlot == "Automotive"){unitlabel <- "(Cars)"}else if (unitlabel =="()"){unitlabel<-""}
      
      
      
    }else{  # "CB"
      LineData <- subset(combin, Technology %in% TechToPlot)#  BenchmarkRegion %in% BenchmarkRegionchoose & & Scenario %in% Scenariochoose)    
      
      if (SectorToPlot == "Fossil Fuels"){
        LineData <- subset(LineData, select = c("Sector","Year","OGCMetrik_Portfolio","Benchmark_OGC"))
        names(LineData)[names(LineData)=="Benchmark_OGC"] <- "Target"
        names(LineData)[names(LineData)== "OGCMetrik_Portfolio"] <- "Portfolio"      
        
        LineData$Portfolio <- LineData$Portfolio/100
        LineData$Target <- LineData$Target/100  
      }else{
        
        LineData <- subset(LineData, select = c("Sector","Year","WtTechShareTechShare","Benchmark_WtTechShareTechShare"))
        names(LineData)[names(LineData)=="Benchmark_WtTechShareTechShare"] <- "Target"
        names(LineData)[names(LineData)== "WtTechShareTechShare"] <- "Portfolio"}
      
      max_magnitude <- 100
      if(SectorToPlot == "Fossil Fuels"){TechLabel <- gsub("Cap "," ",TechToPlot)   } #GT[paste0("T_",TechToPlot,"Prod")][[1]]}else{TechLabel <- GT[paste0("T_",TechToPlot)][[1]] }       # Removes "Cap " from the Power labels
      
      if (SectorToPlot == "Fossil Fuels"){unitlabel <- paste0(TechLabel," (",Startyear, " = 100)")}else{
        unitlabel <- paste0(TechLabel," (%)")}
      axislabel <- ""
      
      LineData$Portfolio <- LineData$Portfolio*100
      LineData$Target <- LineData$Target*100  
    }
    
    LineData <- subset(LineData, LineData$Year >= Startyear)
    LineData$Portfolio[!LineData$Year %in% c(Startyear:(Startyear+5))]<- NA
    
    goodtech <- c("Renewables","Hydro","Nuclear","Hybrid","Electric")  
    badtech <- c("ICE","Oil","Gas","Coal","GasCap","CoalCap")
    
    year_lab <-  "Year"#GT["Year"][[1]]
    ylabel <- paste(axislabel,unitlabel)
   
    # bad ones - ie coal, oil, ice
    if (TechToPlot %in% badtech){
      outputplot <- ggplot(data=LineData)+
        geom_ribbon(aes(x=Year,ymin=Target,ymax=pmax(Target,Portfolio),fill=badexpColour)) +
        #geom_ribbon(aes(x=Year,ymin=0,ymax=`2Â°C Benchmark`,fill=ReqCapColour))+
        geom_ribbon(aes(x=Year,ymin=pmin(Target,Portfolio),ymax=Target,fill=goodexpColour)) +
        geom_ribbon(aes(x=Year,ymin=0,ymax=pmin(Target,Portfolio),fill=CurrCapColour))+
        geom_line(aes(x=Year,y=Portfolio,colour=YourportColour),size=1.5,linetype=1) +
        geom_line(aes(x=Year,y=Target,colour=Tar2DColour),size=1.5,linetype=2) +
        
        scale_fill_identity(name = "", guide = 'legend',labels = c("Exposure gap","Current capacity + planned additions")) +
        scale_colour_manual(name="",guide='legend',values= c(YourportColour,Tar2DColour))+#,labels=c(PortfolioName,"2Â°C Benchmark"))  +
        xlab(year_lab) + ylab(ylabel) + # Set axis labels
        scale_x_continuous(breaks=seq(Startyear,max(LineData$Year),5),expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        expand_limits(y= 1.1*max(LineData[,c(3,4)], na.rm=TRUE))+
        theme_linecharts()
    }else{
      # good ones - ie renewables
      outputplot <- ggplot(data=LineData)+
        geom_ribbon(aes(x=Year,ymin=Target,ymax=pmax(Target,Portfolio),fill=goodexpColour)) +
        # geom_ribbon(aes(x=Year,ymin=0,ymax=Target,fill=ReqCapColour))+
        geom_ribbon(aes(x=Year,ymin=pmin(Target,Portfolio),ymax=Target,fill=badexpColour)) +
        geom_ribbon(aes(x=Year,ymin=0,ymax=pmin(Target,Portfolio),fill=CurrCapColour))+
        geom_line(aes(x=Year,y=Portfolio,colour=YourportColour),size=1.5,linetype=1) +
        geom_line(aes(x=Year,y=Target,colour=Tar2DColour),size=1.5,linetype=2) +
        scale_fill_identity(name = "", guide = 'legend',labels = c("Exposure gap","Current capacity + planned additions")) +
        scale_colour_manual(name="",guide='legend',values= c(YourportColour,Tar2DColour))+#,labels=c(PortfolioName,"2°C Benchmark"))  +
        xlab(year_lab) + ylab(ylabel) + # Set axis labels
        scale_x_continuous(breaks=seq(Startyear,max(LineData$Year),5),expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        expand_limits(y= 1.1*max(LineData[,c(3,4)], na.rm=TRUE))+
        theme_linecharts()
    }
    outputplot <- outputplot +
      guides(colour=guide_legend(keywidth = 4, keyheight = 1,order=1,override.aes = list(linetype=c(1,2),colour=c(YourportColour,Tar2DColour),size=1.5)))    
    
    # ggsave(filename=paste0(plotnumber,"_",ChartType,"_",TechToPlot,'_LinePlot.png', sep=""),bg="transparent",height=2.2,width=2.4,plot=outputplot,dpi=ppi)
    
    TechName <- ifelse(SectorToPlot == "Automotive", paste0(TechToPlot,"CarProduction"),ifelse(SectorToPlot == "Power",paste0(sub("Cap","Power",TechToPlot)),paste0(TechToPlot,"Production")))
    ggsave(filename=paste0("TechnologyGraphs_TechnologyAlignment_",TechName,'.png'),bg="transparent",height=2.2,width=2.4,plot=outputplot,dpi=ppi)
    
    InPort=1
  }else{
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    techlabel <- gsub("Cap "," ",TechToPlot)   
    replacename <- paste0("NoSector",ChartType)
    
    Label <- "There is no production of this Technology in your portfolio"
    
    outputplot <- ggplot()+
      annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,20), size=4)+
      geom_blank()+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA))
    
    
    ggsave(filename=paste0("TechnologyGraphs_TechnologyAlignment_",TechName,'.png'),bg="transparent",height=2.2,width=2.4,plot=outputplot,dpi=ppi)
    # ggsave(filename=paste0(plotnumber,"_",ChartType,"_",TechToPlot,'_MiniLinePlot.png', sep=""),bg="transparent",height=2.2,width=2.4,plot=outputplot,dpi=ppi)
    InPort=0
  }
  return(InPort)    
}

#------------ Ranking Chart -----------------
ranking_chart <- function(ChartType, SectorToPlot){
    
    # Plotting Exposure
    sectors <- data.frame(Sector = c("Automotive","Automotive","Automotive","Fossil Fuels","Fossil Fuels","Fossil Fuels","Power","Power","Power","Power","Power"),Technology = c("Electric","Hybrid","ICE","Coal","Gas","Oil","CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap"), order =1:11,TechnologyNames=c("Electric\nVehicles", "Hybrid\nVehicles", "ICE\nVehicles", "Coal\nProduction", "Gas\nProduction", "Oil\nProduction","Renewable\nCapacity","Hydro\nCapacity", "Nuclear\nCapacity",  "Gas\nCapacity", "Coal\nCapacity"))
    
    Technology<-c("Electric", "Hybrid", "ICE","Gas","Oil", "Coal","RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap")
    badtech <- c("CoalCap","GasCap","ICE","Oil","Gas","Coal")
    goodtech <- Technology[!Technology %in% badtech]
    
    df <- Exposures
    
    df[colnames(df) %in% goodtech] <- df[colnames(df) %in% goodtech]+100
    df[colnames(df) %in% badtech] <- 100-df[colnames(df) %in% badtech]
   
    
    df <- merge(df,AUMData, by= "PortName")
    df <- rename(df, c("PortAUM"="AUM"),warn_missing = FALSE)
    
    WM<- as.data.frame(lapply(df[ , 2:12], weighted.mean, na.rm=TRUE,  w = df$AUM))
    WM$PortName <- "WeightedMean" 
    
    Rank <- Ranks[Ranks$PortName %in% PortfolioName,]
    maxrank <- colMaxs(as.matrix(Ranks[2:12]),na.rm = TRUE )
    autorank <- max(maxrank[1:3],na.rm = TRUE)
    ffrank <- max(maxrank[4:6])
    powerrank <- max(maxrank[7:11])
    if(SectorToPlot== "All"){maxrank <- c(rep(autorank,3),rep(ffrank,3),rep(powerrank,5))}
    if(SectorToPlot== "Automotive"){maxrank <- c(rep(autorank,3))}
    if(SectorToPlot== "Fossil Fuels"){maxrank <- c(rep(ffrank,3))}
    if(SectorToPlot== "Power"){maxrank <- c(rep(powerrank,3))}
    
    #nrow(Ranks)
    Rank$PortName <- "Rank"
    
    
    
    df$AUM <- NULL
    
    Mins <- colMins(as.matrix(df[2:12]),na.rm = TRUE)
    Maxs <- colMaxs(as.matrix(df[2:12]),na.rm = TRUE)
    MinMax <- as.data.frame(t(cbind(Mins,Maxs)))
    colnames(MinMax) <- colnames(df[2:12])
    MinMax$PortName <- c("Minimum","Maximum")
    df <- rbind(df,MinMax,WM,Rank)
    df <- df[df$PortName %in% c(PortfolioName,"Minimum","Maximum","WeightedMean","Rank"),]
    
    PlotData <- setNames(data.frame(t(df[,-1])), df[,1]) 
    PlotData$Technology <- rownames(PlotData)
    PlotData <- merge(PlotData,sectors,by="Technology")
    
    PlotData$PortLoc <- PlotData[,PortfolioName]/100
    
    # Factorise and Order by Technology  
    PlotData <- PlotData[(order(PlotData$order)),]
    PlotData$order <- factor(PlotData$order, levels = PlotData$order)
    
    # Reduce chart to values to plot 
    
    if (SectorToPlot != "All"){
      PlotData <- subset(PlotData, PlotData$Sector %in% SectorToPlot)
      if (SectorToPlot == "Power"){PlotData <- subset(PlotData, PlotData$Technology %in% c("RenewablesCap", "GasCap", "CoalCap"))}
      locations <- c(1:nrow(PlotData))
    }else{
      locations <- c(1:3,4.5:6.5,8:12)
    }
    
    # Chart variables
    barwidth <- .03
    bh <-0.6
    tbwid <- .25
    
    # Label Wrapping Functions  
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    # PlotData$a <- paste0(gsub(" ","",PlotData$Sector),"_Unit")
    # PlotData$b <- paste0("T_",PlotData$Technology)
    # PlotData$b[PlotData$Sector %in% "Fossil Fuels"] <- paste0("T_",PlotData$Technology[PlotData$Sector %in% "Fossil Fuels"],"Prod")
    
    # # Line Labels
    # PlotData$TechTitle <- paste0(t(GT[PlotData$b])," ",t(GT[PlotData$a]))
    # PlotData$TechTitle[PlotData$Sector %in% "Automotive"] <- paste0(t(GT[PlotData$b[PlotData$Sector %in% "Automotive"] ]))
    PlotData$TechLabel <- PlotData$TechnologyNames
    
    PlotData <- PlotData[order(PlotData$order),]
    PlotData$order <- factor(PlotData$order, levels = PlotData$order)
    
    PlotData$Locations <- locations
    
    PlotData$WMloc <- PlotData$WeightedMean/100
    PlotData$WMloc <- t(as.data.frame(lapply(1:nrow(PlotData),function(x) max(0, PlotData$WMloc[x]))))
    PlotData$WMloc <- t(as.data.frame(lapply(1:nrow(PlotData),function(x) min(2, PlotData$WMloc[x]))))    
    
    PlotData$UppLim <- 200 #100
    PlotData$UppLim <- rowMins(as.matrix(PlotData[,colnames(PlotData) %in% c("Maximum","UppLim")]))/100
    PlotData$LowLim <- 0#-100
    PlotData$LowLim <- rowMaxs(as.matrix(PlotData[,colnames(PlotData) %in% c("Minimum","LowLim")]))/100
    
    PlotData$xlowloc <- PlotData$LowLim
    PlotData$xupploc <- PlotData$UppLim
    PlotData$comploc <- PlotData[,PortfolioName]/100
    PlotData$comploc[PlotData$comploc < 0] <- 0
    PlotData$comploc[PlotData$comploc > 2] <- 2
    
    PlotData$complabel<-PlotData[,PortfolioName]
    PlotData$complabel[PlotData$complabel>200]<-200
    PlotData$complabel[PlotData$complabel<0]<-0    
    
    PlotData$complabel <- paste0(round(PlotData$complabel,0),"%")
    PlotData$minlabel<- 0 #round(PlotData$LowLim*100,0)
    PlotData$maxlabel<- 200 #round(PlotData$UppLim*100,0)        
    
    PlotData$minlabel <- paste0(PlotData$minlabel, " %")
    PlotData$maxlabel <- paste0(PlotData$maxlabel, " %")
    
    PlotData$Rank[!is.na(PlotData$Rank)]<- round(PlotData$Rank[!is.na(PlotData$Rank)],0)
    PlotData$Rank[is.na(PlotData$Rank)]<- "-"
    
    repval = 200
    redgreen<- colorRampPalette(c("red","white", "darkgreen"))(repval) 
    xvals <- rep(seq(0,2,2/(repval-1)),length(locations))
    yvals <- sort(rep(locations,repval))
    plotdf <- data.frame(x=xvals,y=yvals,w=2.05/repval,h=bh, colbar=rep(redgreen,length(locations)))
    
    outputplot <-    ggplot()+
      geom_tile(data=plotdf, aes(x=x,y=y),height=plotdf$h,width=plotdf$w,fill=plotdf$colbar) +
      scale_x_continuous()+
      scale_y_discrete()+
      
      # error lines
      geom_segment(data=PlotData,aes(x=xlowloc, xend=xupploc,y=Locations,yend=Locations), linetype="dashed",colour="black")+
      geom_point(data=PlotData,aes(x=xlowloc,y=Locations), fill="black",colour="black", size=2)+
      geom_point(data=PlotData,aes(x=xupploc,y=Locations),  fill="black",colour="black",size=2)+
      
      # centre alignment line    
      annotate(geom="rect",xmin = 0,xmax=1,ymin = locations-bh/2,ymax=locations+bh/2,colour=Tar2DColour ,fill = "transparent")+ #linetype="dashed",
      annotate(geom="rect",xmin =0,xmax=2,ymin=(locations-bh/2),ymax=(locations+bh/2), fill="transparent",colour="black")+ # Box around the bars
      
      # Weighted Mean
      # annotate(xmin=PlotData$WMloc-barwidth/2,xmax=PlotData$WMloc+barwidth/2,ymin=-bh/2+locations,ymax=bh/2+locations,geom = "rect", fill="darkgrey")+
      
      # Company Circles
      geom_point(data=PlotData,aes(x=comploc,y=Locations),  fill=YourportColour,colour=YourportColour,size=10)+
      annotate(geom="text",label=PlotData$complabel, x= PlotData$comploc, y= PlotData$Locations, colour="white",size=rel(3))+ 
      
      # Distribution Range 
      annotate(geom="text",x= -.1, hjust=1 , y= locations,label=PlotData$minlabel,size=rel(3),colour="black")+     # Minimum
      annotate(geom="text",x= 2.1, hjust=0 , y= locations,label=PlotData$maxlabel,size=rel(3),colour="black")+     # Maximum
      
      # Ranking box and label
      annotate("text", label = "Rank", x= 2.35+tbwid/2,y = max(locations)+ 0.5, size=rel(3),fontface = "bold",colour="black")+ # Rank Heading
      annotate("text", label = paste0(PlotData$Rank," of ",maxrank), x= 2.35+tbwid/2,hjust=0.5, y = locations,size=rel(3),fontface = "bold",colour="black")+ # Company Ranking
      
      theme(panel.background = element_rect(fill="transparent"),
            panel.grid.major.x = element_blank() ,
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x=element_text(face="bold",colour="black", size=12),
            axis.title.y=element_text(face="bold",colour="black", size=12, vjust = 1),
            plot.margin = (unit(c(0.2, 0.6, 0, 0), "lines")))
    
    
    if (SectorToPlot == "All"){
      
      leafloc <- c(11,12,2,3)
      
      outputplot<-    outputplot+
        labs(x=NULL,y= NULL)+
        annotate(geom="text",x=-0.8,y=PlotData$Locations[PlotData$Technology %in% badtech],label=wrap.labels(PlotData$TechLabel[PlotData$Technology %in% badtech],12), size=rel(3), hjust=0, fontface = "bold",colour="black")+  # Technology Label - Black
        annotate(geom="text",x=-0.8,y=PlotData$Locations[PlotData$Technology %in% goodtech],label=wrap.labels(PlotData$TechLabel[PlotData$Technology %in% goodtech],12), size=rel(3), hjust=0, fontface = "bold",colour="darkgreen")+ 
        geom_hline(yintercept = c(3.75,7.25))
      
      # write.csv(PlotData,paste0("RankingChartData_",ChartType,"_",PortfolioName,".csv"),row.names = F)
      
      graphheight <- 7.2
    }else {
    
    # if (SectorToPlot != "All"){
      
      if (SectorToPlot == "Power"){leafloc <- c(3,-10); ymax = 5.7; graphheight <- 2.3}
      if (SectorToPlot == "Automotive"){leafloc <- c(3,2); ymax = 3.7; graphheight <- 2.3}
      if (SectorToPlot == "Fossil Fuels"){leafloc <- c(-10,-10); ymax = 3.7; graphheight <- 2.3}
      
      outputplot<-    outputplot+
        labs(x=NULL,y= NULL,  title= NULL)+
        annotate(geom="text",x=-0.8,y=PlotData$Locations[PlotData$Technology %in% badtech],
                 label=wrap.labels(PlotData$TechLabel[PlotData$Technology %in% badtech],12), 
                 size=rel(3), hjust=0, fontface = "bold",colour="black")
      
      if (SectorToPlot != "Fossil Fuels"){
        outputplot <-outputplot+
          annotate(geom="text",x=-0.8,y=PlotData$Locations[PlotData$Technology %in% goodtech],
                   label=wrap.labels(PlotData$TechLabel[PlotData$Technology %in% goodtech],12), 
                   size=rel(3), hjust=0, fontface = "bold",colour="darkgreen")
      }
    }
    
    outputplot <- ggplot_gtable(ggplot_build(outputplot))
    outputplot$layout$clip[outputplot$layout$name == "panel"] <- "off"
    grid.draw(outputplot)  
    
    if (SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
    if(SectorToPlot == "All"){
      ggsave(filename=paste0("SectorGraphs_Overview_PortfolioRanking.png"),bg="transparent",height=graphheight,width=7,plot=outputplot)
      }else{
      ggsave(filename=paste0("SectorGraphs_",SectorToPlot,'_PortfolioRanking.png', sep=""),bg="transparent",height=graphheight,width=7,plot=outputplot)
      }
    
  }
  

##############################
### Not Printed Seperately ###
##############################

#------------ Company Composition Chart -----
CompanyChart <- function(ChartType, SectorToPlot){
  
  if (SectorToPlot== "Automotive"){AlloftheCompanies <- AutoCompanies}
  if (SectorToPlot == "Power"){AlloftheCompanies <- UtilityCompanies}
  if (SectorToPlot == "OG"){AlloftheCompanies <- OGCarbonBudget}
  if (SectorToPlot == "Fossil Fuels"){AlloftheCompanies <- OGData}
  
  combin <- Results_combin_EQ
  companiestoprint = 10
  
  WheelofFortune<-function(df, othercompanies = TRUE ,family = NULL, columnNames = NULL, binSize = 1, spaceItem = 0.2,techorder,PortFirmY=18,OtherFirmY=5,
                           spaceFamily = 1.2, innerRadius = 0.3, outerRadius = 1, guides = seq(100,0,by = -25),
                           alphaStart = -0.3, circleProportion = 0.8, direction = "inwards", familyLabels = FALSE, normalised = TRUE)
  {
    # 
    # df<-AllCompanies
    # family = NULL
    # columnNames = NULL
    # binSize = 1
    # spaceItem = 0.2
    # spaceFamily = 1.2 #1.2
    # innerRadius = 0.3 #0.3
    # outerRadius = 1
    # guides =seq(100,0,by = -25)
    # alphaStart = -0.3 #-0.3
    # circleProportion = .8
    # direction = "inwards"
    # familyLabels = FALSE
    # normalised = TRUE
    
    if (!is.null(columnNames)) {
      namesColumn <- names(columnNames)
      names(namesColumn) <- columnNames
      df <- rename(df, namesColumn)
    }
    
    applyLookup <- function(groups, keys, unassigned = "unassigned") {
      lookup <- rep(names(groups), sapply(groups, length, USE.NAMES = FALSE))
      names(lookup) <- unlist(groups, use.names = FALSE)
      p <- lookup[as.character(keys)]
      p[is.na(p)] <- unassigned
      p
    }
    
    df$score <- factor(df$score, levels=techorder)
    
    if (!is.null(family)) {
      df$family <- applyLookup(family, df$item)}
    df <- arrange(df, family, item, score) # original sort 
    
    
    
    if (normalised) 
    {df <- ddply(df, .(family, item), transform, value = cumsum(value/(sum(value))))
    }else {
      maxFamily <- max(plyr::ddply(df, .(family, item), summarise, 
                                   total = sum(value))$total)
      df <- ddply(df, .(family, item), transform, value = cumsum(value))
      df$value <- df$value/maxFamily
    }
    
    df <- ddply(df, .(family, item), transform, previous = c(0, head(value, length(value) - 1)))
    
    df2 <- ddply(df, .(family, item), summarise, indexItem = 1)
    df2$indexItem <- cumsum(df2$indexItem)
    df3 <- ddply(df, .(family), summarise, indexFamily = 1)
    df3$indexFamily <- cumsum(df3$indexFamily)
    df <- merge(df, df2, by = c("family", "item"))
    df <- merge(df, df3, by = "family")
    df <- arrange(df, family, item, score)
    affine <- switch(direction, inwards = function(y) (outerRadius - innerRadius) * y + innerRadius, outwards = function(y) (outerRadius - innerRadius) * (1 - y) + innerRadius, stop(paste("Unknown direction")))
    
    df <- within(df, {
      xmin <- (indexItem - 1) * binSize + (indexItem - 1) *
        spaceItem + (indexFamily - 1) * (spaceFamily - spaceItem)
      xmax <- xmin + binSize
      ymin <- affine(1 - previous)
      ymax <- affine(1 - value)
    })
    if (normalised) {
      guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)),
                             y = rep(1 - guides/100, 1, each = nrow(df)))
    } else {
      guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)),
                             y = rep(1 - guides/maxFamily, 1, each = nrow(df)))}
    guidesDF <- within(guidesDF, {
      xend <- xmin + binSize
      y <- affine(y)
    })
    totalLength <- tail(df$xmin + binSize + spaceFamily, 1)/circleProportion - 0
    p <- ggplot(df) + geom_rect(aes(xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax, fill = score))
    readableAngle <- function(x) {
      angle <- x * (-360/totalLength) - alphaStart * 180/pi + 90
      angle + ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == -2, 180, 0)
    }
    readableJustification <- function(x) {
      angle <- x * (-360/totalLength) - alphaStart * 180/pi + 90
      ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == -2, 1, 0)
    }
    
    dfItemLabels <- ddply(df, .(family, item), summarize, xmin = xmin[1])
    
    dfItemLabels <- within(dfItemLabels, {
      x <- xmin + binSize/2
      angle <- readableAngle(xmin + binSize/2)
      hjust <- 1
    })
    # new
    
    if (othercompanies == TRUE){
      # LABELS ARE INCLUDED
      typelabel <- data.frame(labelname = c("Portfolio Companies","Other Listed Companies"),x=c(PortFirmY,OtherFirmY),y=0.0,hjust=0.5, angle=90,labelcolours=c( AxisColour,"grey50"))
      if (PortFirmY == 0){typelabel$labelname[1]<-""}
      
      #Company Labels
      p <- p + geom_text(aes(x = x+1.8, label = item, #angle = angle,
                             hjust = hjust,colour = family, fontface=ifelse(family=="Portfolio","bold","plain")), y = 0.16, size = 2.5, show.legend = FALSE,vjust = 3, data = dfItemLabels) +
        scale_colour_manual(values = c("grey50", AxisColour, "black")) #guide=FALSE,
      
      # Sector Labels
      p <- p + geom_text(aes(x = x,hjust=hjust, y=y,label = labelname, angle=angle),size = 3, colour=typelabel$labelcolours, data=typelabel)
      
    }else{
      
      p <- p + geom_text(aes(x = x+1.8, label = item, #angle = angle, 
                             hjust = hjust,colour = family, fontface=ifelse(family=="Portfolio","bold","plain")), y = 0.16, size = 2.5, show.legend = FALSE,vjust = 3, data = dfItemLabels) +
        scale_colour_manual(values = c("black", AxisColour, "black")) #guide=FALSE,
    }
    
    p <- p + geom_segment(aes(x = xmin, xend = xend, y = y, yend = y), 
                          colour = "white", data = guidesDF) #+geom_segment(aes(x = xmin, xend = .75, y = y, yend = y), colour = "grey50", data = guidesDF) #complete lines
    
    if (normalised) {
      guideLabels <- data.frame(x = 0, y = seq(0.2,1.0, by= 0.2),#affine(1 - guides/100), 
                                label = paste(guides, "% ", sep = ""))
    }else{ guideLabels <- data.frame(x = 0, y = affine(1 - guides/maxFamily), 
                                     label = paste(guides, "% ", sep = ""))}
    p <- p + geom_text(aes(x = x-1, y = y, label = label), data = guideLabels,
                       angle = 0, hjust = .5, size = 3)
    if (familyLabels) {
      familyLabelsDF <- aggregate(xmin ~ family, data = df, 
                                  FUN = function(s) mean(s + binSize))
      familyLabelsDF <- within(familyLabelsDF, {
        x <- xmin})
      
    }
    p <- p + theme(panel.background = element_blank(), axis.title.x = element_blank(), 
                   axis.title.y = element_blank(), panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(), axis.text.x = element_blank(), 
                   axis.text.y = element_blank(), axis.ticks = element_blank(),
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   legend.title = element_blank(),legend.position = "bottom")
    # p <- p + ylim(0, outerRadius)
    
  }    
  
  if (SectorToPlot == "OG"){
    OG <-AlloftheCompanies # OGCarbonbudget
    CompProdSnapshot <- combin
    OG$InPort <- "AllCompanies"
    
    if (ChartType == "EQ"){AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "DebtTicker"]
    }else{
      AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "EquityTicker"]
    }
    
    colnames(PortSnapshot)[colnames(PortSnapshot) %in% c("COMPANY_CORP_TICKER","EQY_FUND_TICKER")] <- "TICKER"
    colnames(AlloftheCompanies)[colnames(AlloftheCompanies) %in% c("COMPANY_CORP_TICKER","EQY_FUND_TICKER","EquityTicker","DebtTicker")] <- "TICKER"
    
    OG$InPort[OG$EQY_FUND_TICKER %in% CompProdSnapshot$EQY_FUND_TICKER] <- "PortCompanies"
    
    OGCompanies <- AllCompanyData[AllCompanyData$EQY_FUND_TICKER %in% OG$EQY_FUND_TICKER,]
    OGCompanies <- subset(OGCompanies, Year %in% (Startyear+5) & BenchmarkRegion %in% "Global" & CompanyDomicileRegion %in% CompanyDomicileRegionchoose)
    
    OGCompanies<- subset(OGCompanies, !Technology %in%  "Coal")
    OGCompanies$Production[OGCompanies$Technology == "Oil"]<- OGCompanies$Production[OGCompanies$Technology == "Oil"]*6.12
    OGCompanies$Production[OGCompanies$Technology == "Gas"]<- OGCompanies$Production[OGCompanies$Technology == "Gas"]*0.0372
    
    OGCompanies <- ddply(OGCompanies, . (EQY_FUND_TICKER),summarise, Size = sum(Production))
    
    OG <- merge(OG,OGCompanies, by = "EQY_FUND_TICKER",all.x = TRUE, all.y = FALSE)
    
    OG <- OG[!is.na(OG$Size),]
    
    OG <- subset(OG, select = c("Company","InPort","Size","TotalCarbonBudget","OutsideCarbonBudget"))
    
    # limit data
    OG <- OG[order(-OG$Size),]
    OGPort <- subset(OG, OG$InPort %in% "PortCompanies")
    OGOut <- subset(OG, OG$InPort %in% "AllCompanies")
    
    NoInPort <- nrow(OGPort)
    NoOutPort <- nrow(OGOut)
    
    # NoInPort <- 10
    # NoOutPort <-10
    
    if (NoInPort < 10){NoOutPort <- 20 -NoInPort}else
      if(NoOutPort < 10){NoInPort <- 20 -NoOutPort}else 
        if(NoOutPort>10 & NoInPort>10){NoOutPort<-NoInPort<-10}
    
    
    
    OG <- rbind(OGPort[1:NoInPort,],OGOut[1:NoOutPort,])
    OG <- subset(OG, select=-Size)
    
    PlotData <- melt(OG, id.vars = c("Company","InPort"))
    colnames(PlotData) <- c("item","family","score","value")
    techorder <- c("OutsideCarbonBudget","TotalCarbonBudget")
    
    # scale_fill_manual(values = c("ICE" = ICEColour,"Hybrid" = HybridColour, "Electric"= ElectricColour), labels = TechLabels, name = "Technology")
    Colours <- data.frame("variable"=unique(PlotData$score), "Colour"=c("firebrick","darkgrey"), labels=c("Outside Carbon Budget","Within Carbon Budget"))
    Colours$Colour <- as.character(Colours$Colour)
    
    circleProportion = 1
    alphaStart = 0.02
    spaceFamily = .8
    
    if(NoInPort == 0){PortFirmY <-0}else{PortFirmY <- 18}
    
    # PlotData
    
    Plot<- WheelofFortune(PlotData, family = NULL, columnNames = NULL, binSize = 1, spaceItem = 0.22,techorder=techorder,PortFirmY=PortFirmY,OtherFirmY=5,
                          spaceFamily = spaceFamily, innerRadius = 0.18, outerRadius = 1, guides = seq(0,100,by = 25), alphaStart = alphaStart,
                          circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)+
      scale_fill_manual(values = Colours$Colour, labels=Colours$labels)+
      coord_flip()
    
  }
  else{
    
    if (SectorToPlot == "Power"){techorder <- c("Coal","Gas","Nuclear","Hydro","Renewables")} 
    if (SectorToPlot == "Automotive"){techorder <- c("ICE","Hybrid","Electric")}
    if (SectorToPlot == "Fossil Fuels"){techorder <- c("Conventional Oil","Heavy Oil","Oil Sands", "Unconventional Oil","Other")
    AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "Technology"]
    AlloftheCompanies <- rename(AlloftheCompanies, c("Resource.Type" = "Technology"),warn_missing = FALSE)
    
    if (ChartType == "EQ"){AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "DebtTicker"]}
    else{
      AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "EquityTicker"]
    }
    }
    
    colnames(PortSnapshot)[colnames(PortSnapshot) %in% c("COMPANY_CORP_TICKER","EQY_FUND_TICKER")] <- "TICKER"
    colnames(AlloftheCompanies)[colnames(AlloftheCompanies) %in% c("COMPANY_CORP_TICKER","EQY_FUND_TICKER","EquityTicker","DebtTicker")] <- "TICKER"
    
    CompaniesInPort <- subset(PortSnapshot, select = c("TICKER"), AUM>0)
    CompaniesInPort <- unique(CompaniesInPort)
    
    AllCompanies <- ddply(AlloftheCompanies, .(Technology, TICKER, Name), summarise, Production =sum(Production,na.rm = TRUE)) #Country, 
    colnames(AllCompanies)[colnames(AllCompanies)=="Production"] <- "Capacity"
    AllCompanies$Capacity[is.na(AllCompanies$Capacity)] <-0
    AllCompanies <- subset(AllCompanies, !AllCompanies$Technology %in% "OilCap")
    
    # Classify the Companies
    AllCompanies$Classification <- "AllCompanies"
    AllCompanies$Classification[AllCompanies$TICKER %in% CompaniesInPort$TICKER] <- "PortCompanies"
    
    # Portfolio Average
    Portfoliomix <- ddply(AllCompanies, .(Technology, Classification), summarize, Capacity = sum(Capacity))
    Portfoliomix <- subset(Portfoliomix, Portfoliomix$Classification == "PortCompanies")
    if(dim(Portfoliomix)[1] != 0){
      Portfoliomix$Classification <- "Portfolio"
      # Portfoliomix <- subset(Portfoliomix, !Portfoliomix$Technology %in% c("Oil","Diesel","LPGCNG","Petrol"))
      Portfoliomix$Name <- PortfolioName
      Portfoliomix <- subset(Portfoliomix, select =c("Name","Classification","Technology","Capacity"))
      colnames(Portfoliomix) <- c("item", "family", "score", "value")
      Portfoliomix$value <- as.numeric(Portfoliomix$value)
      Portfoliomix$value <- (Portfoliomix$value/sum(Portfoliomix$value))*100
    }
    
    Targetmix <- subset(combin, Sector == SectorToPlot & Scenario == Scenariochoose & BenchmarkRegion == BenchmarkRegionchoose & Year == Startyear+5)
    if (ChartType == "EQ"){ 
      Targetmix <- subset(Targetmix,  CompanyDomicileRegion == CompanyDomicileRegionchoose , select = c("Technology", "TargetProductionAlignment"))
      }else{
      Targetmix <- subset(Targetmix, select = c("Technology","Benchmark_WtTechShare"))
      Targetmix <- rename(Targetmix, c("Benchmark_WtTechShare" = "TargetProductionAlignment"))
    }
    
    Targetmix$Classification<-"Portfolio"
    Targetmix$Name<-"2° Target"
    Targetmix<-rename(Targetmix, c("TargetProductionAlignment"="Capacity"))
    Targetmix <- subset(Targetmix, select =c("Name","Classification","Technology","Capacity"))
    colnames(Targetmix) <- c("item", "family", "score", "value")
    Targetmix$value <- as.numeric(as.character(Targetmix$value))
    
    
    # Add Index
    Indexmix <- ddply(IndexData, .(CompanyDomicileRegion,Technology), summarize, Capacity = sum(Production))
    Indexmix$Classification <- "Portfolio"
    Indexmix <- subset(Indexmix, select =c("CompanyDomicileRegion","Classification","Technology","Capacity"))
    colnames(Indexmix) <- c("item", "family", "score", "value")  
    Indexmix$value <- as.numeric(as.character(Indexmix$value))
    Indexmix$item <- Indexchoose
    
    # Percentage share of each technology  
    CompanyTotal <- ddply(AllCompanies, .(TICKER,Name), summarise, CompanyTotalCapacity=sum(Capacity))
    AllCompanies <- merge(AllCompanies,CompanyTotal)
    AllCompanies$TechShare <- (AllCompanies$Capacity/AllCompanies$CompanyTotalCapacity)*100
    
    TopPortCompanies <- CompanyTotal[CompanyTotal$TICKER %in% CompaniesInPort$TICKER,]
    TopPortCompanies <- TopPortCompanies[rev(order(TopPortCompanies$CompanyTotalCapacity)),]
    TopPortCompanies <- TopPortCompanies[1:companiestoprint,]
    
    TopPortCompanies<-na.omit(TopPortCompanies)
    if(nrow(TopPortCompanies)>0){    TopPortCompanies$totorder <- seq(1,nrow(TopPortCompanies))}
    
    indexcomptoprint <- companiestoprint+ (companiestoprint - nrow(TopPortCompanies))
    TopIndexCompanies <- CompanyTotal[!CompanyTotal$TICKER %in% CompaniesInPort$TICKER,]
    TopIndexCompanies <- TopIndexCompanies[rev(order(TopIndexCompanies$CompanyTotalCapacity)),]
    TopIndexCompanies <- TopIndexCompanies[1:indexcomptoprint,]
    TopIndexCompanies$totorder <- seq(1,indexcomptoprint)
    TopIndexCompanies <- TopIndexCompanies[1:(2*companiestoprint-nrow(TopPortCompanies)),]
    
    if (SectorToPlot != "Fossil Fuels"){
      AllTopCompanies <- rbind(TopPortCompanies, TopIndexCompanies)
    }else{
      AllTopCompanies <- TopPortCompanies
    }  
    
    AllCompanies <- subset(AllCompanies, AllCompanies$TICKER %in% AllTopCompanies$TICKER)
    AllCompanies <- subset(AllCompanies, Name != "NA")
    
    # Clean Company Names
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, "LIMITED", "LTD.")
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, "COMPANY", "CO.")
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, "CORPORATION", "CORP.")
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, ",", "")
    AllCompanies$Name<-strtrim(AllCompanies$Name, 16)
    
    oldnames <- c("BAYERISCHE MOTOREN WERKE AG","FIAT CHRYSLER AUTOMOBILES NV","FUJI HEAVY INDUSTRIES LTD","HONDA MOTOR CO LTD","MITSUBISHI MOTORS CORP","BRILLIANCE CHINA AUTOMOTIVE")
    newnames <- c("BMW AG","FIAT CHRYSLER NV","FUJI HEAVY IND LTD","HONDA MOTOR CO","MITSUBISHI MOTORS","BRILLIANCE CN AUTO")
    for (i in c(1:length(oldnames))){AllCompanies$Name[AllCompanies$Name %in% oldnames[i]] <- newnames[i]}
    
    # Rearrange to be ready for WheelofFortune Function
    AllCompanies <- subset(AllCompanies, select = c("Name","Classification","Technology","TechShare"))
    colnames(AllCompanies) <- c("item", "family", "score", "value") #item = component, family = portfolio, score  = technology, value = capacity mix
    
    # Bind the remaining Lines (IEAmix comes in each section)
    
    AllCompanies[AllCompanies$item == "NA"] <- "NoName"
    
    AllCompanies <- as.data.frame(sapply(AllCompanies, function(x) gsub("Cap", "", x)))
    
    # # REMOVE COMPANY NAMES
    # nocompanies <- length(unique(AllCompanies$item))
    # names <- data.frame(item=unique(AllCompanies$item),number=paste0("Company ",LETTERS[1:nocompanies]))
    # AllCompanies <- merge(AllCompanies,names, by="item")
    # AllCompanies$item<-NULL
    # AllCompanies$item <- AllCompanies$number
    # AllCompanies$number<-NULL
    
    
    if(nrow(TopPortCompanies)>0){PortFirmY=(companiestoprint*2-3)}else{PortFirmY <-0}
    OtherFirmY=5
    
    if (SectorToPlot == "Power"){  
      # Portfoliomix <- as.data.frame(sapply(Portfoliomix, function(x) gsub("Cap", "", x)))
      Portfoliomix$score <- gsub("Cap","",Portfoliomix$score)
      Portfoliomix$value <- as.numeric(as.character(Portfoliomix$value))
      
      Targetmix <- subset(Targetmix, score  %in% c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap"))
      Targetmix <- as.data.frame(sapply(Targetmix, function(x) gsub("Cap", "", x)))
      Targetmix$value <- as.numeric(as.character(Targetmix$value))
      Targetmix$value <- (Targetmix$value/sum(Targetmix$value))*100
      
      Indexmix <- subset(Indexmix, score  %in% c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap"))
      Indexmix <- as.data.frame(sapply(Indexmix, function(x) gsub("Cap", "", x)))
      Indexmix$value <- as.numeric(as.character(Indexmix$value))
      Indexmix$value <- (Indexmix$value/sum(Indexmix$value))*100
      
      AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
      AllCompanies <- rbind(AllCompanies, Portfoliomix, Targetmix, Indexmix)  
      AllCompanies <- subset(AllCompanies, AllCompanies$score != "Oil")
      
      circleProportion = 1
      alphaStart = 0.02
      spaceFamily = .8
      
      # TechLabels <- c(paste0("% ", GT["T_CoalCap"][[1]]),paste0("% ", GT["T_GasCap"][[1]]),paste0("% ", GT["T_NuclearCap"][[1]]),paste0("% ", GT["T_HydroCap"][[1]]),paste0("% ", GT["T_RenewablesCap"][[1]]))
      TechLabels <- c("% Coal Capacity","% Gas Capacity","% Nuclear Capacity","% Hydro Capacity","% Renewable Capacity")
      
            
      labelling <- data.frame(values = c(CoalCapColour,GasCapColour,NuclearColour, HydroColour,RenewablesColour), labels = TechLabels, name = techorder)
      labelling$values <- as.character(labelling$values)
      labelling$name <- factor(labelling$name, techorder)
      
      Plot<- WheelofFortune(AllCompanies, family = NULL, columnNames = NULL, binSize = 1, spaceItem = 0.22,techorder=techorder,PortFirmY=PortFirmY,OtherFirmY=OtherFirmY,
                            spaceFamily = spaceFamily, innerRadius = 0.18, outerRadius = 1, guides = seq(0,100,by = 25), alphaStart = alphaStart,
                            circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)+
        scale_fill_manual(values = labelling$values, labels = labelling$labels)+
        
        coord_flip()
    }
    
    if (SectorToPlot == "Automotive"){
      Targetmix <- subset(Targetmix, score  %in% c("ICE","Hybrid","Electric"))
      Targetmix$value <- as.numeric(as.character(Targetmix$value))
      Targetmix$value <- (Targetmix$value/sum(Targetmix$value))*100
      
      Indexmix <- subset(Indexmix, score  %in% c("ICE","Hybrid","Electric"))
      Indexmix$value <- as.numeric(as.character(Indexmix$value))
      Indexmix$value <- (Indexmix$value/sum(Indexmix$value))*100
      
      AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
      AllCompanies <- rbind(AllCompanies, Portfoliomix, Targetmix, Indexmix)  
      
      circleProportion = 1
      alphaStart = 0
      spaceFamily = 1
      
      # TechLabels <- c(paste0("% ", GT["T_ICE"][[1]]),paste0("% ", GT["T_Hybrid"][[1]]),paste0("% ", GT["T_Electric"][[1]]))
      TechLabels <- c("% ICE Vehicles","% Hybrid Vehicles ","% Electric Vehicles")
      
      
      labelling <- data.frame(values = c(ICEColour,HybridColour,ElectricColour), labels = TechLabels, name = techorder)
      labelling$values <-    as.character(labelling$values)
      labelling$name <- factor(labelling$name, techorder)
      
      # AllCompanies[is.na(AllCompanies$value)]<-NULL
      AllCompanies <- subset(AllCompanies,!is.na(AllCompanies$value))
      
      Plot<- WheelofFortune(AllCompanies, family = NULL, columnNames = NULL, binSize = 1.0, spaceItem = 0.2,techorder=techorder,PortFirmY=PortFirmY,OtherFirmY=OtherFirmY,
                            spaceFamily = spaceFamily, innerRadius = 0.18, outerRadius = 1., guides = seq(0,100,by = 25), alphaStart = alphaStart,
                            circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)+
        scale_fill_manual(values = labelling$values, labels = labelling$labels)+
        coord_flip()
      # Plot
      
    }
    
    if (SectorToPlot == "Fossil Fuels"){
      
      # Portfoliomix <- as.data.frame(sapply(Portfoliomix, function(x) gsub("Cap", "", x)))
      Portfoliomix$value <- as.numeric(as.character(Portfoliomix$value))
      
      AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
      
      AllCompanies <- AllCompanies[rev(order(AllCompanies$item)),]
      AllCompanies$item <- factor(AllCompanies$item, levels=AllCompanies$item)
      
      AllCompanies <- rbind(AllCompanies, Portfoliomix)
      
      circleProportion = 1
      alphaStart = 0
      spaceFamily = 1
      
      oilcolours = brewer.pal(9, "YlGnBu")[5:9]
      
      # TechLabels <- c(paste0("% ", GT["Conv_Oil"][[1]]),paste0("% ", GT["Heavy_Oil"][[1]]),paste0("% ", GT["Oil_Sands"][[1]]), paste0(GT["Unconv_Oil"][[1]]), paste0(GT["Other_Oil"][[1]]))
      TechLabels <- c("% Conventional Oil","% Heavy Oil","% Oil Sands", "% Unconventional Oil", "% Other Oil")
      
      Plot<- WheelofFortune(AllCompanies,othercompanies = FALSE , family = NULL, columnNames = NULL, binSize = 1.0, spaceItem = 0.2,techorder = techorder,PortFirmY=PortFirmY,OtherFirmY=OtherFirmY,
                            spaceFamily = spaceFamily, innerRadius = 0.18, outerRadius = 1., guides = seq(0,100,by = 25), alphaStart = alphaStart,
                            circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)+
        scale_fill_manual(values=oilcolours,labels = TechLabels, name = "Technology")+
        coord_flip()
      
    } 
    
  }
  
  Plot <- ggplot_gtable(ggplot_build(Plot))
  Plot$layout$clip[Plot$layout$name == "panel"] <- "off"
  
  # PortfolioName <- gsub("_.*", "\\1", PortfolioName)
  if (SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
  
  png(paste0("CompanyComposition_",SectorToPlot,'.png'), height = 2600, width = 5500,res=ppi,bg="transparent") 
  grid.draw(Plot)
  dev.off()  
  
  # return(png(paste(PortfolioName,"_",SectorToPlot,'_WheelofFortune.png'), height = 3.300, width = 3300,res=ppi,bg="transparent") )
  return()  
}

#------------ Renewable Additions -----------
renewAdditions <- function(ChartType){
  
  combin <- Results_combin_EQ
  
  if(ChartType=="EQ"){
    # Definition of Regions
    PowerRegionExclusion <- c("Global", "OECD", "NonOECD","EU", "OECDAmericas" , "LatinAmerica", "Africa", "EEurope_Eurasia", "NonOECDAsia", "OECDAsiaOceania", "NonOECDRest","OECDAggregate","NonOECDAggregate")
    GlobalAggregate <- subset(combin, CompanyDomicileRegion %in% CompanyDomicileRegionchoose & Scenario == Scenariochoose & Year %in% c(Startyear,Startyear+5))# & !BenchmarkRegion %in% PowerRegionExclusion)
    GlobalAggregate <- subset(GlobalAggregate, select = c("BenchmarkRegion","Year","Direction","FairSharePerc"))
    regions <- data.frame(unique(GlobalAggregate$BenchmarkRegion[!GlobalAggregate$BenchmarkRegion %in% c("GlobalAggregate","OECDAggregate","NonOECDAggregate")]))
    
    RegionCountries <- data.frame(BenchmarkRegionList$Global, BenchmarkRegionList$OECD)
    RegionCountries <- rename(RegionCountries, c("BenchmarkRegionList.Global"="Global","BenchmarkRegionList.OECD"="OECD"))
    
    Countries <- data.frame(BenchmarkRegion=character(),Country=character())
    if (BenchmarkRegionchoose != "GlobalAggregate"){for (j in 1:(nrow(regions))){
      countries <- data.frame(BenchmarkRegion=regions[j,],Country=unique(BenchmarkRegionList[[as.character(regions[j,])]]))
      Countries <- rbind(Countries,countries)}}else{
        Countries<- data.frame(BenchmarkRegion="Global",Country=RegionCountries$Global)
      }
  }else{
    PortSnapshot <- rename(PortSnapshot, c("COMPANY_CORP_TICKER"="EQY_FUND_TICKER"),warn_missing = FALSE)
  }
  # Companies in Port
  
  #### DELETE THIS NEXT LINE - the name really needs to be read in in the output generation not later!
  PortSnapshot$Name <- "COMPANY NAME"
  #####
  
  
  PortCompanies <- unique(subset(PortSnapshot, select = c("EQY_FUND_TICKER","Name","piesector"), PortSnapshot$AUM >0))
  
  # IEA Renewables Targets
  IEATargetsRenew <- subset(AllIEATargets,Year %in% c(Startyear, Startyear+5) & Scenario == Scenariochoose & Technology =="RenewablesCap", select = c("BenchmarkRegion","Year","Direction","FairSharePerc","AnnualvalIEAtech"))
  
  # Power Capacities
  PowerCapacity <- subset(MasterData_Power,  Sector =="Power" & Year %in% c(Startyear,Startyear+5) & EQY_FUND_TICKER != "NonListedProduction" & Technology != "OilCap")
  
  # Cut down to power companies
  PowerCapacity <- merge(PortCompanies, PowerCapacity, by = "EQY_FUND_TICKER")
  renewcap <- subset(PowerCapacity, PowerCapacity$Technology %in% "RenewablesCap")
  
  if (nrow(renewcap)>0 ){
    
    # Calculate the company power total for all technology 
    PowerTotals <- ddply(PowerCapacity, .(EQY_FUND_TICKER,Year,PlantLocation), summarise,LocalCompanyCap = sum(CompLvlProduction))
    PowerTotals <- merge(PowerTotals, Countries, by.x ="PlantLocation", by.y="Country")
    PowerTotals <- merge(PowerTotals, IEATargetsRenew, by = c("Year","BenchmarkRegion"))
    PowerTotals$LocalTargetAdditions <- PowerTotals$LocalCompanyCap*(PowerTotals$FairSharePerc)
    
    CompanyPowerTotals <- ddply(PowerTotals, .(EQY_FUND_TICKER,Year),summarise, TargetAdditions =sum(LocalTargetAdditions), CompanyCap = sum(LocalCompanyCap))
    CompanyPowerTotals <- subset(CompanyPowerTotals, Year == Startyear+5, select = c("EQY_FUND_TICKER","TargetAdditions","CompanyCap"))
    
    RenewCapacity <- subset(PowerCapacity,  Technology =="RenewablesCap", select = c("EQY_FUND_TICKER","Name","Year","piesector","Technology","PlantLocation","CompLvlProduction") )
    RenewCapacity <- ddply(RenewCapacity, .(EQY_FUND_TICKER,Name,piesector,Year),summarise, RenewableCap = sum(CompLvlProduction))
    RenewAdditions <- dcast(RenewCapacity, EQY_FUND_TICKER+Name+piesector~Year, fun=sum, value.var = "RenewableCap")
    RenewAdditions$Additions <- RenewAdditions[[as.character(Startyear+5)]]-RenewAdditions[[as.character(Startyear)]]
    RenewAdditions <- subset(RenewAdditions, select = c("EQY_FUND_TICKER","Name","piesector","Additions"))
    
    Renewables <- merge(RenewAdditions,CompanyPowerTotals, by = c("EQY_FUND_TICKER"))
    
    Renewables <- subset(Renewables, piesector == "Utility Power")
    if(dim(Renewables)[1] != 0 ){
      AllUtilities <- ddply(Renewables,.(piesector),summarise, TargetAdditions=sum(TargetAdditions), CompanyCap=sum(CompanyCap), Additions =sum(Additions))  
      AllUtilities$Name <- "All Portfolio Utilities"
      AllUtilities$piesector <- "Combined"
      AllUtilities$EQY_FUND_TICKER <- "All Portfolio Utilities"
      
      Renewables<- rbind(Renewables,AllUtilities)
      
      Renewables$AddPerc <- Renewables$Additions/Renewables$TargetAdditions
      Renewables$AddPerc[Renewables$AddPerc > 1]<- 1
      Renewables$Remaining <- 1-Renewables$AddPerc
      Renewables$StillRequired <- Renewables$TargetAdditions-Renewables$Additions
      Renewables<- subset(Renewables, Renewables$TargetAdditions != 0)
      
      Renewables <- Renewables[order(Renewables$CompanyCap),]
      
      nocompanies <- nrow(Renewables)
      companiestokeep <- 20
      if (nocompanies > companiestokeep){
        RenewablesToPlot <- Renewables[((nocompanies-companiestokeep)+1):nocompanies,]
      }else{
        RenewablesToPlot <- Renewables
      }
      
      RenewablesBar <- subset(RenewablesToPlot, select = c("Name","piesector","AddPerc","Remaining","StillRequired"))
      RenewablesBar <- melt(RenewablesBar, id.vars = c("Name","piesector"))
      RenewablesBar <- RenewablesBar[order(RenewablesBar$piesector,RenewablesBar$Name),]
      RenewablesBar$Name <- factor(RenewablesBar$Name, levels = unique(RenewablesBar$Name))
      
      RenewablesStillRequired <- subset(RenewablesBar, RenewablesBar$variable == "StillRequired")
      RenewablesStillRequired$value[RenewablesStillRequired$value < 0 ] <-0
      # RenewablesStillRequired <- RenewablesStillRequired[order(RenewablesStillRequired$piesector,RenewablesStillRequired$Name),]
      RenewablesBar <- RenewablesBar[!(RenewablesBar$variable == "StillRequired"),]
      
      # RenewablesStillRequired$value <- round(RenewablesStillRequired$StillRequired,0)
      RenewablesStillRequired$Name <- factor(RenewablesStillRequired$Name, levels = (RenewablesStillRequired$Name))
      
      Yaxislabel <- paste0("Renewable capacity additions (",Startyear, " - ", Startyear+5,")")
      stillreq <- "Remaining Required Additions"  # red
      remainlabel <- "Progress" #GT["StillReq"][[1]]
      progresslabel <- "Remaining Required Additions"  #CORRECT #GT["StillReq"][[1]]#GT["X2Target"][[1]]
      Target <- "Progress"
      
      theme_barcharts <- function(base_size = textsize, base_family = "") {
        theme(axis.ticks=element_blank(), 
              axis.text.x=element_text(face="bold",colour=AxisColour,size=textsize),
              axis.text.y=element_text(face="bold",colour=AxisColour,size=textsize),
              axis.title.x=element_blank(),
              axis.title.y=element_text(face="bold",colour=AxisColour,size=textsize),
              axis.line = element_line(colour = AxisColour,size=1),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              legend.position=c(0.5,-.2),
              legend.direction="horizontal",
              legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
              legend.background = element_rect(fill = "transparent",colour = NA),
              legend.key.size=unit(0.4,"cm"),
              legend.title=element_blank(),
              legend.key = element_blank(),
              plot.margin = unit(c(.4,0, 2.2, 0), "lines"),
              plot.background = element_rect(fill = "transparent",colour = NA)
        )
      }
      
      
      # # REMOVE COMPANY NAMES
      # names <- data.frame(Name=unique(RenewablesBar$Name[RenewablesBar$piesector == "Utility Power"]),number=paste0("Company ",LETTERS[1:19]),order =as.numeric(as.character(seq(1:19))))
      # RenewablesBar <- merge(RenewablesBar,names, by="Name")
      # RenewablesBar$Name<-NULL
      # RenewablesBar$Name <- RenewablesBar$number
      
      
      RenAddBar <-ggplot(RenewablesBar, aes(x=rev(Name),y=rev(value), fill = variable))+
        geom_bar(stat = "identity", width=.8, colour = NA)+
        geom_segment(aes(x=0, xend = 0 , y=0, yend = 1), size=1, colour = AxisColour,  arrow = arrow(length = unit(0.4,"cm")))+
        geom_hline(yintercept = 1, colour=Tar2DColour, linetype = "longdash", size = 1)+
        scale_fill_manual(breaks=c("AddPerc","Remaining"),values = c("AddPerc"= badexpColour, "Remaining" = YourportColour), labels = c("AddPerc"=progresslabel, "Remaining" = remainlabel))+
        annotate(hjust = 1,"text", x = c(1:(length(RenewablesStillRequired$Name))) , y = .98, label = paste((round(RenewablesStillRequired$value,0)), "MW",stillreq,sep=" "), colour = "white",size = 3)+
        scale_y_continuous(breaks = c(0,.25,.5,.75,1),label = c("0%","25%","50%","75%",Target),expand=c(0,0), limits=c(0,1))+
        scale_x_discrete(expand=c(0,0))+
        theme_barcharts()+
        coord_flip()
      RenAddBar <- RenAddBar  +
        theme(  axis.title.x=element_text(face="bold",colour=AxisColour,size=textsize),
                axis.title.y=element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                # legend.position=c(0.5,-.28),
                legend.position = "bottom",
                legend.direction="horizontal",
                legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
                legend.background = element_rect(fill = "transparent",colour = NA),
                legend.key.size=unit(0.4,"cm"),
                legend.title=element_blank(),
                legend.key = element_blank(),
                plot.margin = unit(c(.4,1.5, 0, 0), "lines"))+
        ylab(Yaxislabel)
      
      
      RenAddValues <- c(4.8,8)
      
      NoCompanies <- nrow(RenewablesToPlot)
      
      plotheight <- .2*NoCompanies +.9
      plotwidth <- .1*NoCompanies +7.1
      
      
      ggsave(filename=paste0("RenewableAdditions.png"),bg="transparent",height=plotheight,width=plotwidth,plot=RenAddBar,dpi=ppi)
      RenewAdds<<-1
    }else{
      
      Label <- "No Renewable Energy Additions within this Portfolio"
      
      
      RenAddBar <-ggplot()+
        annotate("text", label = Label,x = 0, y = 0, size = 4)+
        theme(  axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                # legend.position=c(0.5,-.28),
                # legend.position = "bottom",
                # legend.direction="horizontal",
                # legend.text = element_text(face="bold",size=textsize,colour="black"),
                # legend.background = element_rect(fill = "transparent",colour = NA),
                # legend.key.size=unit(0.4,"cm"),
                legend.title=element_blank(),
                legend.key = element_blank(),
                plot.margin = unit(c(.4,1.5, 0, 0), "lines"))
      
      RenewAdds<<-0
      # RenAddBar <- ggplot() + annotate("text", label = paste0("No Renewable Additions in Portfolio"),x = 0, y = 0, size = 4)
      # ggsave(filename=paste0("RenewableAdditions.png"),bg="transparent",height=2,width=7.5,plot=RenAddBar,dpi=ppi)
    }
  }
  
}
  
