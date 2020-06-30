best <- function(stado, outcome) {
  setwd("~/Programm Assignment/data")
  bd<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  bd[,11]<- as.numeric(bd[,11]) # 30-day Mortality Heart Attack
  bd[,17]<- as.numeric(bd[,17]) # 30-day Mortality Heart failture
  bd[,23]<- as.numeric(bd[,23]) # 30-day Mortality Pnuemonia
  nbd<-bd[,c(2,7,11,17,23)]
  head(nbd)
  names(nbd)<-c("Hospital","state","heart attack","heart failture",
                "pneumonia")
  ## Check that state and outcome are valid
  if(!stado %in% nbd[, "state"]){
    stop("Invalid state")
  } else if(!outcome %in% colnames(nbd[,3:5])){
    stop("Invalid outcome")
  } else { 
    a<-subset(nbd,state==stado)
    min<-min(a[,outcome],na.rm=T)
    x<-r<-a[which(a[,outcome]==min),]
    y<-r[order(r$Hospital),1]
  }
  return(y)
}

