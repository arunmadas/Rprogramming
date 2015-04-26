tapply(mtcars$mpg, mtcars$cyl, mean)pollutantmean <- function(directory, pollutant, id=1:332) {
  fileList<-list.files(directory)
  fileCount<-length(fileList)
  valuesMatrix<-matrix(nrow=2,ncol=fileCount)
  
  for(fileIndex in id) {
     #construct the file name
     csvFile<-paste(getwd(),"/",directory,"/",fileList[fileIndex],sep="")
     #print(csvFile)
     
     # read the file contents
     fileContent <- read.csv(csvFile)
     
     #declare variables
     sumValue<-0
     position<-2
     
     if(pollutant=="sulfate") {
       #cat("calculating sulfate mean for fileIndex",fileIndex)
       position<-2
     } 
     else if (pollutant == "nitrate") {
       #cat("calculating nitrate mean for fileIndex",fileIndex)
       position<-3
     }

     #Calculate the sum values
     content<-as.matrix(fileContent[position])     
     sumValue<- sum(content,na.rm=TRUE)
     #cat("sum of index", fileIndex,"=",as.numeric(sumValue))       
     valuesMatrix[1,fileIndex]<-as.numeric(sumValue)
     
     #exclude the NA values and get the count
     naContent<-is.na(content)                             
     nonNaContent<- content[!naContent]
     valuesMatrix[2,fileIndex]<-length(nonNaContent)
     #cat("count of non-na values",length(nonNaContent))           
  }
  #valuesMatrix
  
  totalsum<-sum(valuesMatrix[1,],na.rm=TRUE)
  totalcount<-sum(valuesMatrix[2,],na.rm=TRUE)
  #cat("totalsum = ",totalsum,";totalcount = ",totalcount)
  
  meanval<-totalsum/totalcount  
  round(meanval,3)
}