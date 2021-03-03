viewTable <- function(){
  View(diamondsExport[,c("O2Sats_FRB","TIME_POINT_57=O2_SATURATION","DATE_TIME_INVESTIGATIONS","DATETIME_FRB")])
}


diamondsExport$O2Sats_FRB <- "NULL"

for (i in 1:nrow(diamondsExport)){
  
  if((!sub(".*=","",diamondsExport$DATE_TIME_INVESTIGATIONS[i]) %in%  c("NULL","NULL=NULL")) & diamondsExport$DATETIME_FRB[i] != "NULL"){
    
    investigationDate <- stri_split(diamondsExport$DATE_TIME_INVESTIGATIONS[i], regex = ";")[[1]]
    investigationDate <- investigationDate[investigationDate!="NULL"]
    investigationDate <- sub(".*=","",investigationDate)
    investigationDate <- (as.Date(investigationDate))  #separate the dates by the ; and make the times NA so you are left with the dates
    investigationDate <- investigationDate[!is.na(investigationDate)] #remove the NAs to just leave the dates of investigations
    
    
    
    timePoint <- stri_split(diamondsExport$TIME_POINT[i], regex = ";")[[1]][
      which(between(investigationDate -as.Date(diamondsExport$DATETIME_FRB[i]),-1,0))]  #find where the investigation date is on the date of or the day before first research bloods and extract the relevant time point
    
    if(length(timePoint)!=0){
       if(is.na(timePoint)){
     diamondsExport$O2Sats_FRB[i] <- NA 
    } else { 
    
    sats <- stri_split(diamondsExport$`TIME_POINT_57=O2_SATURATION`[i], regex = ";")[[1]]
    
    if (any(grepl(timePoint,sats))){
    sats <- sats[grepl(timePoint,sats)]
    diamondsExport$O2Sats_FRB[i] <- sub(".*=","",sats)
    }
    }
    }
  }
}

viewTable()
