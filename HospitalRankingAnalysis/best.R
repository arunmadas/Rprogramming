best<-function(state,outcome) {
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #validation boolean values
  isValidState<-TRUE
  isValidOutcome<-TRUE
  
  #Master set of valid state
  validStateList<-c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","AS","DC","FM","GU","MH","MP","PW","PR","VI","AE","AA","AP")
  
  #Validate if state is valid
  if(!is.element(state,validStateList)){
    isValidState<-FALSE
    errorStr<-paste("Error in best(\"",state,"\",\"",outcome,"\") : invalid state", sep="")
    stop(errorStr)
  }
  
  #if state is valid, validate outcome
  if(isValidState) {
      validOutcomeList<-list("heart attack","heart failure","pneumonia")
      if(!(outcome %in% validOutcomeList)) {
        isValidOutcome<-FALSE
        errorStr<-paste("Error in best(\"",state,"\",\"",outcome,"\") : invalid outcome", sep="")
        stop(errorStr)
      }
  }
  
  if(isValidState && isValidOutcome) {
   # cat("state is =", state)
    
    bestHospitalHeartAttack<-0
    hospitalName<-NULL
    minHeartAttack<-NULL
    
    if(outcome=="heart attack") {
      #col 11
      outcomedata[,11]<-suppressWarnings(as.numeric(outcomedata[,11]))
      #bestHospitalHeartAttack<-as.numeric(min(outcomedata[outcomedata$state==state,][,11],na.rm=TRUE))
      #cat("bestHospitalHeartAttack=",bestHospitalHeartAttack)
      for(indx in 1:nrow(outcomedata)) {
             
              
          if(outcomedata[[indx,7]] %in% state && !is.na(outcomedata[[indx,11]])) {     
           
                  #cat("matching state")
                  
                  if(is.null(minHeartAttack)){
                          minHeartAttack<-outcomedata[indx,11]
                          hospitalName<-outcomedata[indx,2]
                          next
                  }
                  
                  #if values of min is equal 
                  if(outcomedata[indx,11]==minHeartAttack) {
                         if(outcomedata[indx,2]<hospitalName) {
                                 hospitalName<-outcomedata[indx,2]
                                 #cat("same score, lowering hospital")
                         } 
                          
                  }
                           
                  if(outcomedata[indx,11]<minHeartAttack) {
                          #cat("assign again")
                          minHeartAttack<-outcomedata[indx,11]
                          hospitalName<-outcomedata[indx,2]
                          #cat(hospitalName)
                  }                  
          }              
      }
      return(hospitalName)
    }
    else if (outcome == "heart failure") {
      #17
            bestHospitalHeartAttack<-0
            hospitalName<-NULL
            minHeartAttack<-NULL
            indx<-1
            
            outcomedata[,17]<-suppressWarnings(as.numeric(outcomedata[,17]))
            
            for(indx in 1:nrow(outcomedata)) {
                    
                    
                    if(outcomedata[[indx,7]] %in% state && !is.na(outcomedata[[indx,17]])) {     
                            
                            #cat("matching state")
                            
                            if(is.null(minHeartAttack)){
                                    minHeartAttack<-outcomedata[indx,17]
                                    hospitalName<-outcomedata[indx,2]
                                    next
                            }
                            
                            #if values of min is equal 
                            if(outcomedata[indx,17]==minHeartAttack) {
                                    if(outcomedata[indx,2]<hospitalName) {
                                            hospitalName<-outcomedata[indx,2]
                                            #cat("same score, lowering hospital")
                                    } 
                                    
                            }
                            
                            if(outcomedata[indx,17]<minHeartAttack) {
                                    #cat("assign ..")
                                    minHeartAttack<-outcomedata[indx,17]
                                    hospitalName<-outcomedata[indx,2]
                                    #cat(hospitalName,"--",minHeartAttack)
                            }                  
                    }              
            }
            return(hospitalName)
    }
    else if(outcome == "pneumonia") {
      #23
            bestHospitalHeartAttack<-0
            hospitalName<-NULL
            minHeartAttack<-NULL
            indx<-1
            outcomedata[,23]<-suppressWarnings(as.numeric(outcomedata[,23]))
            
            for(indx in 1:nrow(outcomedata)) {
                    
                    
                    if(outcomedata[[indx,7]] %in% state && !is.na(outcomedata[[indx,23]])) {     
                            
                            #cat("matching state")
                            
                            if(is.null(minHeartAttack)){
                                    minHeartAttack<-outcomedata[indx,23]
                                    hospitalName<-outcomedata[indx,2]
                                    next
                            }
                            
                            #if values of min is equal 
                            if(outcomedata[indx,23]==minHeartAttack) {
                                    if(outcomedata[indx,2]<hospitalName) {
                                            hospitalName<-outcomedata[indx,2]
                                            #cat("same score, lowering hospital")
                                    } 
                                    
                            }
                            
                            if(outcomedata[indx,23]<minHeartAttack) {
                                    #cat("assign again")
                                    minHeartAttack<-outcomedata[indx,23]
                                    hospitalName<-outcomedata[indx,2]
                                    #cat(hospitalName)
                            }                  
                    }              
            }
            return(hospitalName)
    }
  }
}
