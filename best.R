outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
best<-function(state,outcome){
  a<-unlist(strsplit(outcome,"\\ "))
  for(i in 1:length(a)){
    b<-NULL
    d<-unlist(strsplit(a[i],""))
    d[1]<-toupper(d[1])
    for(j in 1:length(d)){
      if(is.null(b)){
        b<-paste(d[j])
      }
      else{
        b<-paste(b,d[j],sep = "")
      }
    }
    a[i]<-b
  }
  a
}