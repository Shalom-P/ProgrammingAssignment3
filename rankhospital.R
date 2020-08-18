outcome <- read.csv("outcome-of-care-measures.csv")
rankhospital<-function(state,outcom,num = "best"){
  if(outcom == "heart failure"){
    if(num == "best"){
      num<-1
    }
    a<-tapply(as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),outcome$State,sort)
    b<-a[names(a) == state]
    o<-unlist(b)
    if(num == "worst"){
      num<-length(o) 
    }
    n<-outcome$Hospital.Name[(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == o[num]) & (outcome$State == state)]
  }else if(outcom == "heart attack"){
    if(num == "best"){
      num<-1
    }
    a<-tapply(as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),outcome$State,sort)
    b<-a[names(a) == state]
    o<- unlist(b)
    if(num == "worst"){
      num<- length(o)
    }
    n<-outcome$Hospital.Name[(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == o[num]) & (outcome$State==state)]
  }
  n[1]
}