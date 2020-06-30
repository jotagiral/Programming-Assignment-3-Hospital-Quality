rankhospital <- function(stado, outcome, num=1) {
  setwd("~/Programm Assignment/data")
  bd<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  bd[,11]<- as.numeric(bd[,11]) # 30-day Mortality Heart Attack
  bd[,17]<- as.numeric(bd[,17]) # 30-day Mortality Heart failure
  bd[,23]<- as.numeric(bd[,23]) # 30-day Mortality Pnuemonia
  nbd<-bd[,c(2,7,11,17,23)]
  head(nbd)
  names(nbd)<-c("Hospital","state","heart attack","heart failure",
                "pneumonia")
  ## Check that state and outcome are valid
  if(!stado %in% nbd[, "state"]){
    stop("Invalid state")
  } else if(!outcome %in% colnames(nbd[,3:5])){
    stop("Invalid outcome")
  } else { 
    x<-subset(nbd,state==stado,select=c("Hospital",outcome))
    head(x)
    y <- x[order(x[, outcome], x[, "Hospital"], decreasing = F), ]
    y2<-cbind(y,rank=1:dim(y)[1])}
  if (is.numeric(num)) {
    out<- y2[num,1]
  } else if (num=="best") {
      out<-y2[1,1]}
    else if (num=="worst"){
      out<- y2[dim(na.omit(y2))[1],1]
    }
    else {
      stop("Invalid rank")
      }
  return(out)  
  }

