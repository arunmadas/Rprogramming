rankall<-function(outcome,num="best") {
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        validOutcomeList<-list("heart attack","heart failure","pneumonia")
        if(!(outcome %in% validOutcomeList)) {
                isValidOutcome<-FALSE
                errorStr<-paste("Error in best(\"",state,"\",\"",outcome,"\") : invalid outcome", sep="")
                stop(errorStr)
        }
        
        dframe<-data.frame(NULL)
        
        if(!is.na(as.numeric(num)) && as.numeric(num)>length(states)) {
                return(suppressWarnings(as.numeric("Y")))
        }
        
        if(outcome=="heart attack") {
                
                heartattackdata<-outcomedata[,c(2,7,11)]
                heartattackdata[,3]<-suppressWarnings(as.numeric(heartattackdata[,3]))
                states<-unique(heartattackdata[,2])
                states<-sort(states)
                dframe<-data.frame(NULL)
                for(eachstate in states) 
                {                        
                        eachstateheartattackdata<-heartattackdata[heartattackdata$State==eachstate,]
                        if(num=="best") {
                                orh<-order(eachstateheartattackdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,eachstateheartattackdata$Hospital.Name)
                                if(is.null(dframe))
                                {
                                        dframe<-cbind(" "=eachstate,eachstateheartattackdata[orh[1],1:2])
                                } 
                                else 
                                {       
                                        dframe<-rbind(dframe, cbind(" "=eachstate,eachstateheartattackdata[orh[1],1:2]))
                                }
                        }
                        else if(num=="worst") {
                                orh<-order(eachstateheartattackdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,eachstateheartattackdata$Hospital.Name,decreasing = FALSE)                                
                                if(is.null(dframe))
                                {
                                        dframe<-cbind(" "=eachstate,eachstateheartattackdata[orh[length(orh)],1:2])
                                }
                                else
                                {
                                        dframe<-rbind(dframe, cbind(" "=eachstate,eachstateheartattackdata[orh[length(orh)],1:2]))
                                }
                        }
                        else {
                                orh<-order(eachstateheartattackdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,eachstateheartattackdata$Hospital.Name)
                                intnum<-as.numeric(num);
                                if(!is.na(intnum)) {
                                        if(is.null(dframe) || length(dframe)==0)
                                        {
                                                dframe<-cbind(" "=eachstate,eachstateheartattackdata[orh[intnum],1:2])
                                        }
                                        else
                                        {
                                                dframe<-rbind(dframe, cbind(" "=eachstate,eachstateheartattackdata[orh[intnum],1:2]))
                                        }
                                }
                        }
                }   
                dframe[,3]<-dframe[,1]                
                colnames(dframe) <- c(" ","hospital", "state")                
                rownames(dframe) <- dframe[,1]
                dframe<-dframe[2:3]
                return(dframe)
        }
        else if (outcome == "heart failure") {
                
                
                heartfailuredata<-outcomedata[,c(2,7,17)]
                heartfailuredata[,3]<-suppressWarnings(as.numeric(heartfailuredata[,3]))
                states<-unique(heartfailuredata[,2])
                states<-sort(states)
                dframe<-data.frame(NULL)
                for(eachstate in states) 
                {                        
                        eachstateheartfailuredata<-heartfailuredata[heartfailuredata$State==eachstate,]
                        if(num=="best") {
                                orh<-order(eachstateheartfailuredata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,eachstateheartfailuredata$Hospital.Name)
                                if(is.null(dframe))
                                {
                                        dframe<-cbind(" "=eachstate,eachstateheartfailuredata[orh[1],1:2])
                                } 
                                else 
                                {       
                                        dframe<-rbind(dframe, cbind(" "=eachstate,eachstateheartfailuredata[orh[1],1:2]))
                                }
                        }
                        else if(num=="worst") {
                                orh<-order(eachstateheartfailuredata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,eachstateheartfailuredata$Hospital.Name,decreasing=FALSE)
                                if(is.null(dframe))
                                {
                                        dframe<-cbind(" "=eachstate,eachstateheartfailuredata[orh[length(orh)],1:2])
                                }
                                else
                                {
                                        dframe<-rbind(dframe, cbind(" "=eachstate,eachstateheartfailuredata[orh[length(orh)],1:2]))
                                }
                        }
                        else {
                                orh<-order(eachstateheartfailuredata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,eachstateheartfailuredata$Hospital.Name)
                                intnum<-as.numeric(num);
                                if(!is.na(intnum)) {
                                        if(is.null(dframe) || length(dframe)==0)
                                        {
                                                dframe<-cbind(" "=eachstate,eachstateheartfailuredata[orh[intnum],1:2])
                                        }
                                        else
                                        {
                                                dframe<-rbind(dframe, cbind(" "=eachstate,eachstateheartfailuredata[orh[intnum],1:2]))
                                        }
                                }
                        }
                }   
                dframe[,3]<-dframe[,1]
                colnames(dframe) <- c(" ","hospital", "state")                
                rownames(dframe) <- dframe[,1]
                dframe<-dframe[2:3]
                return(dframe)                

        }
        else if(outcome == "pneumonia") {
                
                
                pneumoniadata<-outcomedata[,c(2,7,23)]
                pneumoniadata[,3]<-suppressWarnings(as.numeric(pneumoniadata[,3]))
                states<-unique(pneumoniadata[,2])
                states<-sort(states)
                dframe<-data.frame(NULL)
                for(eachstate in states) 
                {                        
                        eachstatepneumoniadata<-pneumoniadata[pneumoniadata$State==eachstate,]
                        if(num=="best") {
                                orh<-order(eachstatepneumoniadata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,eachstatepneumoniadata$Hospital.Name)
                                if(is.null(dframe))
                                {
                                        dframe<-cbind(" "=eachstate,eachstatepneumoniadata[orh[1],1:2])
                                } 
                                else 
                                {       
                                        dframe<-rbind(dframe, cbind(" "=eachstate,eachstatepneumoniadata[orh[1],1:2]))
                                }
                        }
                        else if(num=="worst") {
                                orh<-order(eachstatepneumoniadata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,eachstatepneumoniadata$Hospital.Name,decreasing=FALSE)
                                if(is.null(dframe))
                                {
                                        dframe<-cbind(" "=eachstate,eachstatepneumoniadata[orh[length(orh)],1:2])
                                }
                                else
                                {
                                        dframe<-rbind(dframe, cbind(" "=eachstate,eachstatepneumoniadata[orh[length(orh)],1:2]))
                                }
                        }
                        else {
                                orh<-order(eachstatepneumoniadata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,eachstatepneumoniadata$Hospital.Name)
                                intnum<-as.numeric(num);
                                if(!is.na(intnum)) {
                                        if(is.null(dframe) || length(dframe)==0)
                                        {
                                                dframe<-cbind(" "=eachstate,eachstatepneumoniadata[orh[intnum],1:2])
                                        }
                                        else
                                        {
                                                dframe<-rbind(dframe, cbind(" "=eachstate,eachstatepneumoniadata[orh[intnum],1:2]))
                                        }
                                }
                        }
                }   
                dframe[,3]<-dframe[,1]
                colnames(dframe) <- c(" ","hospital", "state")                
                rownames(dframe) <- dframe[,1]
                dframe<-dframe[2:3]
                return(dframe)                
        }
}