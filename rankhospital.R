outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
c<-outcome %>% arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
p<-outcome %>% arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
pn<-outcome %>% arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
rankhospital<-function(state,outcome,num = "best"){
  b<-tapply(as.numeric(c$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),c$State,sort)
  a<-tapply(as.numeric(p$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),p$State,sort)
  pnt<-tapply(as.numeric(pn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),pn$State,sort)
  if(outcome == "heart attack"){
    bs<-unlist(b[names(b)== state])
    if(num == "best"){
      n<-c$Hospital.Name[(c$State==state) & (c$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == bs[1])]
    }else if(num== "worst"){
      n<-c$Hospital.Name[(c$State == state) & (c$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == max(bs))]
    }else{
      n<-c$Hospital.Name[(c$State == state) & (c$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == bs[num])]
    }
  }else if(outcome == "heart failure"){
    as<-unlist(a[names(a) == state])
    if(num == "best"){
      n<-p$Hospital.Name[(p$State==state) & (p$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == as[1])]
    }else if(num== "worst"){
      n<-p$Hospital.Name[(p$State == state) & (p$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == max(as))]
    }else{
      n<-p$Hospital.Name[(p$State == state) & (p$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == as[num])]
    }
  }else if(outcome == "pneumonia"){
    pnts<-unlist(pnt[names(pnt)== state])
    if(num == "best"){
      n<-pn$Hospital.Name[(pn$State==state) & (pn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == pnts[1])]
    }else if(num== "worst"){
      n<-pn$Hospital.Name[(pn$State == state) & (pn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == max(pnts))]
    }else{
      n<-pn$Hospital.Name[(pn$State == state) & (pn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == pnts[num])]
    }
  }
  n[1]
}