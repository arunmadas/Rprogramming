complete <- function(directory, id=1:332) {
  fileList<-list.files(directory)
  fileCount<-length(fileList)
  valuesMatrix<-matrix(nrow=length(id),ncol=2,dimnames=list(NULL,c("id","nobs")))
  
  index<-1
  for(fileIndex in id) {
     #construct the file name
     csvFile<-paste(getwd(),"/",directory,"/",fileList[fileIndex],sep="")
     #print(csvFile)
     
     # read the file contentsas.
     fileContent <- read.csv(csvFile)
     
     #Calculate the sum values
     matrixContent<-as.matrix(fileContent)   
     nobs <- 0
     for(matIndex in 1:dim(matrixContent)[1]) {
        sulphateVal <- matrixContent[matIndex,2]
        nitrateVal <- matrixContent[matIndex,3]
        
        if(!is.na(sulphateVal) && !is.na(nitrateVal)) {
          nobs <- nobs+1
        }
     }
     
     #nobs<-dim(mdata)[1]
     #cat("sum of index", fileIndex,"=",as.numeric(sumValue))       
     valuesMatrix[index,1]<-fileIndex
     valuesMatrix[index,2]<-nobs
     
     index<-index+1
     #cat("count of non-na values",length(nonNaContent))           
  }
  as.data.frame(valuesMatrix)
  #dimnames(valuesMatrix)
}