source("rankhospital.R")

rankall <- function(outcome, num = "best") {
  setwd("~/Programm Assignment/data")
  bd<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  s<-(unique(bd[,7]))
  s2<-s[order(s)]
  ra<-data.frame()
  for (i in 1:length(s)) {
    r<-rankhospital(stado=s2[i], outcome,num)
    ra<-c(ra,r)
    out<-cbind(ra,s2)
    out<-as.data.frame(out)
    rownames(out)<-s2
    colnames(out)<-c("hospital", "state")
    
  }
  return(out)
}


