GraphTranslation <- read.csv(paste0(TEMPLATE.PATH,"/GraphTranslation_V4.csv"), stringsAsFactors = FALSE)
ReportTranslation <- read.csv(paste0(TEMPLATE.PATH,"/GeneralReportTranslation_V1.csv"), stringsAsFactors = FALSE)

Languagechoose <- "EN"


### Function ###

preptranslations <- function(TranslationType,GraphTranslation, Languagechoose, Startyear){
  GT <- subset(GraphTranslation, select = c("TextLabel",Languagechoose))
  
  GT <- setNames(data.frame(t(GT[,-1])), GT[,1])
  
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

GT <- preptranslations("Graph",GraphTranslation, Languagechoose,Startyear)
RT <- preptranslations("Report",ReportTranslation, Languagechoose, Startyear)


