source("complete.R")

corr <- function(directory, threshold = 0) {
        fileList<-list.files(directory)
        fileCount<-length(fileList)
                
        #get the total number of valid observations in each file by monitor id
        countObs<-complete(directory, 1:fileCount)
        
        #convert to matrix
        matrixCountObs<-as.matrix(countObs) 
        outputVector<-vector("numeric",length=0)
        
        mcount<-1
        isOneElemAdded<-0
        #iterate over the monitorid-vs-observationCount to exclude thresholds
        for(mcount in 1:dim(matrixCountObs)[1]) {
            
                monitorId<-matrixCountObs[mcount,1]
                monitorThreshold<-matrixCountObs[mcount,2]
                
                #only for those observations greater than threshold
                
                if(monitorThreshold > threshold) {
                        #compute corr for that id
                        
                        #read the file
                        csvFile<-paste(getwd(),"/",directory,"/",fileList[monitorId],sep="")
                        fileContent <- read.csv(csvFile)
                        
                        #new trials
#                         fileContentMatrix<-as.matrix(fileContent)
#                         nonNaMatrix<-
#                         
#                         for(matIndex in 1:dim(fileContentMatrix)[1]) {
#                                 sulphate<-fileContentMatrix[matIndex,2]
#                                 nitrate<-fileContentMatrix[matIndex,3]
#                                 
#                                 if(is.na(sulphate) || is.na(nitrate)) {
#                                         next
#                                 } else {
#                                         
#                                 }
#                                         
#                         }
                        #new trials
                        
                        
                        complCase<-fileContent[complete.cases(fileContent[,2:3]),]
                        outputVector[mcount] <-cor(complCase$sulfate, complCase$nitrate)

                        #corrVal
                        
                        #corrVal<-cor(fileContent$sulfate, fileContent$nitrate, use = "complete.obs")
                        #corrVal<-round(corrVal,5)
                        
#                         if(isOneElemAdded == 0) {
#                                 outputVector<-corrVal 
#                                 isOneElemAdded=isOneElemAdded+1
#                         } else {
#                                 outputVector<-c(outputVector,corrVal)        
#                                 isOneElemAdded=isOneElemAdded+1
#                         }
                        
                }
                else {
                        next
                }

                #print(monitorCount[2])
        }
        outputVector<-outputVector[!is.na(outputVector)]
        outputVector
        
}

# 
# summary <- function (object, ..., digits = 4) 
# {
#         if (is.factor(object)) 
#                 return(summary.factor(object, ...))
#         else if (is.matrix(object)) 
#                 return(summary.matrix(object, digits = digits, ...))
#         value <- if (is.logical(object)) 
#                 c(Mode = "logical", {
#                         tb <- table(object, exclude = NULL)
#                         if (!is.null(n <- dimnames(tb)[[1L]]) && any(iN <- is.na(n))) dimnames(tb)[[1L]][iN] <- "NA's"
#                         tb
#                 })
#         else if (is.numeric(object)) {
#                 nas <- is.na(object)
#                 object <- object[!nas]
#                 qq <- stats::quantile(object)
#                 qq <- ceiling(c(qq[1L:3L], mean(object), qq[4L:5L]), 4)
#                 names(qq) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", 
#                                "Max.")
#                 if (any(nas)) 
#                         c(qq, `NA's` = sum(nas))
#                 else qq
#         }
#         else if (is.recursive(object) && !is.language(object) && 
#                          (n <- length(object))) {
#                 sumry <- array("", c(n, 3L), list(names(object), c("Length", 
#                                                                    "Class", "Mode")))
#                 ll <- numeric(n)
#                 for (i in 1L:n) {
#                         ii <- object[[i]]
#                         ll[i] <- length(ii)
#                         cls <- oldClass(ii)
#                         sumry[i, 2L] <- if (length(cls)) 
#                                 cls[1L]
#                         else "-none-"
#                         sumry[i, 3L] <- mode(ii)
#                 }
#                 sumry[, 1L] <- format(as.integer(ll))
#                 sumry
#         }
#         else c(Length = length(object), Class = class(object), Mode = mode(object))
#         class(value) <- c("summaryDefault", "table")
#         value
# }