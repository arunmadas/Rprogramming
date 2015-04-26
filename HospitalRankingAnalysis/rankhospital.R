rankhospital<-function(state,outcome,num="best") {
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        validOutcomeList<-list("heart attack","heart failure","pneumonia")
        if(!(outcome %in% validOutcomeList)) {
                isValidOutcome<-FALSE
                errorStr<-paste("Error in best(\"",state,"\",\"",outcome,"\") : invalid outcome", sep="")
                stop(errorStr)
        }
        
        if(outcome=="heart attack") {
                
                heartattackdata<-outcomedata[,c(2,7,11)]
                heartattackdata<-heartattackdata[heartattackdata$State==state,]
                heartattackdata[,3]<-suppressWarnings(as.numeric(heartattackdata[,3]))
                heartattackdata<-heartattackdata[!is.na(heartattackdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                orh<-order(heartattackdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,heartattackdata$Hospital.Name)
                if(num=="best") {
                        return(heartattackdata[orh[1],1])
                }
                else if(num=="worst") {
                        return(heartattackdata[orh[length(orh)],1])
                }
                else {
                        return(heartattackdata[orh[num],1])
                }
        }
        else if (outcome == "heart failure") {
                heartfailuredata<-outcomedata[,c(2,7,17)]
                heartfailuredata<-heartfailuredata[heartfailuredata$State==state,]
                heartfailuredata[,3]<-suppressWarnings(as.numeric(heartfailuredata[,3]))
                heartfailuredata<-heartfailuredata[!is.na(heartfailuredata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                orh<-order(heartfailuredata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,heartfailuredata$Hospital.Name)
                if(num=="best") {
                        return(heartfailuredata[orh[1],1])
                }
                else if(num=="worst") {
                        return(heartfailuredata[orh[length(orh)],1])
                }
                else {
                        return(heartfailuredata[orh[num],1])
                }
        }
        else if(outcome == "pneumonia") {
                pneumoniadata<-outcomedata[,c(2,7,23)]
                pneumoniadata<-pneumoniadata[pneumoniadata$State==state,]
                pneumoniadata[,3]<-suppressWarnings(as.numeric(pneumoniadata[,3]))
                pneumoniadata<-pneumoniadata[!is.na(pneumoniadata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                orh<-order(pneumoniadata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,pneumoniadata$Hospital.Name)
                if(num=="best") {
                        return(pneumoniadata[orh[1],1])
                }
                else if(num=="worst") {
                        return(pneumoniadata[orh[length(orh)],1])
                }
                else {
                        return(pneumoniadata[orh[num],1])
                }
        }
}