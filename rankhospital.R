outcome <- read.csv("outcome-of-care-measures.csv")
rankhospital<-function(state,outcom,num = "best"){
  y<-unique(outcome$State)
  if((outcom == "heart failure") & (state %in% y)){
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
  }else if((sum(outcom != c("heart attack","heart failure", "pneumonia"))) | !(state %in% y)){
    if(sum(outcom != c("heart attack","heart failure", "pneumonia")) & (state %in% y)){
      n<-"invalid outcome"
    }else if(!(state %in% y) & sum(outcom == c("heart attack","heart failure", "pneumonia"))){
      n<-"invalid state"
    }else {
      n<-"invalid outcome and invalid state"
    }
  }else if(outcom == "pneumonia"){
    if(num == "best"){
      num<-1
    }
    a<-tapply(as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),outcome$State,sort)
    b<-a[names(a) == state]
    o<-unlist(b)
    if(num == "worst"){
      num<-length(o) 
    }
    n<-outcome$Hospital.Name[(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == o[num]) & (outcome$State == state)]
  }
  n[1]
}