outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
best<-function(State,outcom){
  y<-unique(outcome$State)
  if(outcom == "heart attack"){
    a<-tapply(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcome$State,min)
    b<-a[names(a)== State]
    n<-outcome$Hospital.Name[(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == b) & (outcome$State == State)]
  }
  else if(outcom == "heart failure"){
    a<-tapply(as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),outcome$State,min,na.rm = TRUE)
    b<-a[names(a)== State]
    n<-outcome$Hospital.Name[(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == b) & (outcome$State == State)]
  }
  else if(outcom == "pneumonia"){
    a<-tapply(as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),outcome$State,min,na.rm = TRUE)
    b<-a[names(a)==State]
    n<-outcome$Hospital.Name[(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == b) & (outcome$State == State)]
  }
  for(i in 0){
    if(!(State %in% y)){
      n<-paste('Error in best(',State,', ',outcom,') : invalid state')
    }
    else if((outcom != "heart attack") & (outcom != "heart failure") & (outcom != "pneumonia")){
      n<-paste("Error in best(",State,", ",outcom,") : invalid outcome")
    }
  }
  n
}