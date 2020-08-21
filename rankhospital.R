outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
a<-data.frame(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,State = outcome$State,Hospital.Name = outcome$Hospital.Name)
b<-data.frame(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,State = outcome$State,Hospital.Name = outcome$Hospital.Name)
c<-data.frame(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,State = outcome$State,Hospital.Name = outcome$Hospital.Name)
rankhospital<-function(state,outcome,num= "best"){
  if(!sum((outcome %in% c("heart attack","heart failure","pneumonia")))){
    stop("invalid outcome")
  }
  if(outcome == "heart attack"){
    heart<-a %>% arrange(a$Hospital.Name)
    heart<-heart %>% arrange(as.numeric(heart$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    heart<-heart %>% arrange(heart$State)
    rnk<-data.frame(State = heart$State[heart$State == state],hn = heart$Hospital.Name[heart$State == state],har = as.numeric(heart$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[heart$State == state]))
    if(!(state %in% unique(rnk$State))){
      stop("invalid state")
    }
    if(num == "best"){
      fn<-rnk$Hospital.Name[1]
    }else if(num == "worst"){
      fn<-rnk$hn[length(rnk$har[complete.cases(rnk$har)])]
    }else{
      fn<-rnk$hn[num]
    }
  }else if(outcome == "heart failure"){
    heart<-b %>% arrange(b$Hospital.Name)
    heart<-heart %>% arrange(as.numeric(heart$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    heart<-heart %>% arrange(heart$State)
    rnk<-data.frame(State = heart$State[heart$State == state],hn = heart$Hospital.Name[heart$State == state],har = as.numeric(heart$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[heart$State == state]))
    if(!(state %in% unique(rnk$State))){
      stop("invalid state")
    }
    if(num == "best"){
      fn<-rnk$Hospital.Name[1]
    }else if(num == "worst"){
      fn<-rnk$hn[length(rnk$har[complete.cases(rnk$har)])]
    }else{
      fn<-rnk$hn[num]
    }
  }else if(outcome == "pneumonia"){
    heart<-c %>% arrange(c$Hospital.Name)
    heart<-heart %>% arrange(as.numeric(heart$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    heart<-heart %>% arrange(heart$State)
    rnk<-data.frame(State = heart$State[heart$State == state],hn = heart$Hospital.Name[heart$State == state],har = as.numeric(heart$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[heart$State == state]))
    if(!(state %in% unique(rnk$State))){
      stop("invalid state")
    }
    if(num == "best"){
      fn<-rnk$Hospital.Name[1]
    }else if(num == "worst"){
      fn<-rnk$hn[length(rnk$har[complete.cases(rnk$har)])]
    }else{
      fn<-rnk$hn[num]
    }
  }
  fn
}
