# support functions for shiny AACR report generation app
library(openxlsx)
library(readxl)
library(tm)
library(RTextTools)
library(quanteda)
library(tools)
library(RColorBrewer)
library(digest)
library(stringr)
#library(gmailr)

source(file.path("functions","homogenizeNumbers.R")) #load support functions
source(file.path("functions","printConfusionMatrix2.R")) #load support functions


preLoadxlsx <- function(fileIn){
        if(tolower(file_ext(fileIn$name))=="csv"){
                junk <- read.csv(fileIn$datapath,stringsAsFactors = FALSE)
                nSheets <- 1
        }else{
                nSheets <- length(excel_sheets(fileIn$datapath))         #count number of sheets
                junk <- read_excel(fileIn$datapath,sheet=nSheets)
        }
        nRows <- nrow(junk)
        nCols <- ncol(junk)
        c(nSheets,nRows,nCols)
}


readDataIn <- function(fileIn,sheetIn=NULL,startRow=0,header=TRUE){
        #------------------ Read in data ------------------------------------------------
        if(tolower(file_ext(fileIn$name))=="csv"){
                datIn <- read.csv(fileIn$datapath,header=header,skip=startRow,stringsAsFactors = FALSE)
                nSheets <- 1
        }else{
                if(is.null(sheetIn)){
                        nSheets <- length(excel_sheets(fileIn$datapath))         #count number of sheets
                        datIn <- read_excel(fileIn$datapath,sheet=nSheets,skip=startRow,col_names=header) #load last worksheet to table
                        if (length(datIn) == 0){datIn <- read_excel(fileIn$datapath,sheet=1,skip=startRow,col_names=header)} #load worksheet to table
                        
                } else {
                        datIn <- read_excel(fileIn$datapath,sheet=as.numeric(sheetIn),skip=startRow,col_names=header)
                }
        }
        
        if (exists("datIn$Score")){datIn$Score <- as.numeric(datIn$Score)}
        as.data.frame(datIn)
}

stemText <- function(x){
        tmp <- strsplit(x,"[[:space:]]")
        tmp <- lapply(tmp,function(x) paste(stemDocument(x[x!=""]),collapse=" "))
        unlist(tmp)
}
removeHTMLtags <- function(x){
        tmp <- gsub("<.*?>", "", x)
        tmp
}
stemCompleteText <- function(x,stemDictionary){
        tmp <- strsplit(x,"[[:space:]]")
        tmp <- lapply(tmp,function(x) paste(stemCompletion(x[x!=""],stemDictionary),collapse=" "))
        unlist(tmp)
}

preprocessData <- function(datIn,question,stemming=TRUE,numberFormat="remove",preserveSymbol=""){
        localStopwords <- c("the","a","in","and")
        tmp <- iconv(datIn[,question],to="utf-8",sub="") #force character encoding, remove non-comformat characters
        tmp <- tolower(tmp)
        tmp <- removeHTMLtags(tmp)
        tmp <- removeWords(tmp,localStopwords)
        if (nchar(preserveSymbol)[1]>0){
                pSymbol <- gsub("[[:space:]]","",preserveSymbol)
                pSymbol <- unlist(strsplit(pSymbol,split=""))
                tmp <- strip(tmp,char.keep=pSymbol,digit.remove=FALSE) #remove punctuation        
        }else{
                tmp <- removePunctuation(tmp)
        }
        
        if(numberFormat=="remove"){
                tmp <- removeNumbers(tmp)
        }else if(numberFormat=="digitsToWords"){
                tmp <- sapply(tmp,homogenizeNumbers)
                tmp <- unname(tmp)
        }
        if(stemming){tmp <- stemText(tmp)}
        
        tmp
}

estimateProb <- function(rowIn){
        lrIn <- length(rowIn)
        tmpIndex <- which(rowIn[1:(lrIn-1)]==rowIn[lrIn])+1
        if(is.na(mean(as.numeric(rowIn[tmpIndex])))){print(rowIn)}
        mean(as.numeric(rowIn[tmpIndex]))
}

predictScoring <- function(docTermMatIn,models,ensembleMethod="Simple",stackingModel=NULL,codeList=NULL){
        #create data container
        contTest <- create_container(docTermMatIn,NULL,testSize=1:nrow(docTermMatIn),virgin=TRUE)
        #run classifcations
        classTmp <- classify_models(contTest,models)
        rm(models) #remove
        predOut <- list()
        classIndex <- seq(1,ncol(classTmp),by=2)
        if(ensembleMethod=="CVPWeight"){
                #make tidy prediction data structure such that prediction probability reflects a single code (positive case)
                tmpPredicts <- classTmp
                if(ifelse(is.null(codeList),TRUE,is.na(codeList))){
                        scoreLevels <- 0:1
                }else{
                        scoreLevels <- codeList
                }
                negativeCaseIndex <- tmpPredicts[,classIndex]==scoreLevels[1]
                tmpPredicts[,classIndex+1][negativeCaseIndex] <- 1-tmpPredicts[,classIndex+1][negativeCaseIndex]
                #select FPR for negative codes, TPR for postitve codes
                cvR <- sapply(seq_along(classIndex),function(x) ifelse(tmpPredicts[,classIndex[x]]==scoreLevels[2],stackingModel[2,x],stackingModel[1,x]))
                
                predOut$rubricLevel <-ifelse(rowSums(tmpPredicts[,classIndex+1],na.rm=TRUE)>(length(classIndex)-rowSums(cvR,na.rm=TRUE)),
                                             scoreLevels[2],
                                             scoreLevels[1])
                
        }else if(ensembleMethod=="Simple"){
                predOut$rubricLevel <- apply(classTmp[,classIndex],1,function(x) names(which.max(table(x))))
        }
        predOut$probability <- apply(cbind(classTmp,predOut$rubricLevel),1,estimateProb)
        predOut
}

calcOA <- function(termsIn,scoresIn,testScore){
        #calculate overabundance of specific term-score pair
        index <- scoresIn==testScore
        if(sum(index)==0){
                out <- -1
        }else{
                out <- sum(termsIn[index])*length(scoresIn)/(sum(index)*sum(termsIn))-1
        }
        
        out
}

overAbundance <- function(dtmIn,scoresIn){
        #         Calculate overabundance of ngram terms based on scoring
        #         dtmIn - input document term matrix
        #         scoreIn - input scoring assignment vectors
        
        out <- numeric()
        for(i in levels(scoresIn)){
                out <- rbind(out,apply(dtmIn,2,function(x) calcOA(x,scoresIn,i)))
        }
        out
}

mostOverabundant <- function(dtmIn,scoresIn,nMost){
        #find the n most overabundant terms for each score
        oAbun <- overAbundance(dtmIn,scoresIn)
        outNames <- apply(oAbun,1,function(x) names(sort(x,decreasing=TRUE)[1:nMost]))
        outValues <- apply(oAbun,1,function(x) format(round(sort(x,decreasing=TRUE)[1:nMost],2),nsmall=2))
        completeIndex <- !is.na(outNames[,1])
        list(names=outNames[completeIndex,],values=outValues[completeIndex,])        
}

modelNames <- function(modelListIn){
        modelNameDictionary <- list(SVM="Support Vector Machines",
                                    SLDA="Scaled Linear Discriminant Analysis", #Corrected on 9-28-2020
                                    BOOSTING="LogitBoost",
                                    BAGGING="Bagging Classifcation Trees",
                                    RF="Random Forest",
                                    GLMNET="Penalized Generalized Linear Model",
                                    TREE="Classifcation Tree",
                                    NNET="Feed-Forward Neural Network",
                                    MAXENT="Maximum Entropy")
        outDF <- data.frame(unlist(modelNameDictionary[names(modelListIn)]))
        colnames(outDF) <- "Ensemble Models"
        outDF
}

ResortDtm <- function(working.dtm) {
        # sorts a sparse matrix in triplet format (i,j,v) first by i, then by j.
        # Args:
        #   working.dtm: a sparse matrix in i,j,v format using $i $j and $v respectively. Any other variables that may exist in the sparse matrix are not operated on, and will be returned as-is.
        # Returns:
        #   A sparse matrix sorted by i, then by j.
        # SOURCE: https://groups.google.com/forum/#!topic/rtexttools-help/VILrGoRpRrU
        
        working.df <- data.frame(i = working.dtm$i, j = working.dtm$j, v = working.dtm$v)  # create a data frame comprised of i,j,v values from the sparse matrix passed in.
        working.df <- working.df[order(working.df$i, working.df$j), ] # sort the data frame first by i, then by j.
        working.dtm$i <- working.df$i  # reassign the sparse matrix' i values with the i values from the sorted data frame.
        working.dtm$j <- working.df$j  # ditto for j values.
        working.dtm$v <- working.df$v  # ditto for v values.
        return(working.dtm) # pass back the (now sorted) data frame.
}  # end function

findBinSize <- function(x,nBins){
        binSizes <- c(0.01,0.02,0.05,0.1,0.2)
        tmp <- abs(nBins-ceiling(x/binSizes))
        binSizes[which.min(tmp)]
}

guessResponseCol <- function(datIn){
        nCharsCol <-sapply(1:ncol(datIn),function(x) sum(stri_length(datIn[,x]),na.rm=TRUE))
        which.max(nCharsCol)
}

colorPicker <- function(n,name){
        colorSet <- brewer.pal(n,name)
        if(length(colorSet!=n)){
                colorSet <- rep_len(colorSet,n)
        }
        colorSet
}

checkPID <- function(idIn){
        #Test if all IDs are in MSU PID format
        testList <- (nchar(idIn) == 9) & (substr(idIn,1,1) == "a") & (!is.na(suppressWarnings(as.numeric(substring(idIn,2)))))
        sum(testList) == length(idIn)
}

checkHash <- function(idIn){
        # check if ids are already hashed
        testList <- nchar(idIn) == 32
        sum(testList) == length(idIn)
}

hashIDs <- function(datIn,IDfield=1){
        tmpIDs <- datIn[,IDfield]
        tmpIDs <- gsub("@.*","",tmpIDs) #remove email domains
        tmpIDs <- tolower(tmpIDs) #move to lower case
        tmpIDs <- str_trim(tmpIDs) #trim leading/trailing whitespace
        if(checkPID(tmpIDs)){tmpIDs <- gsub("a","A",tmpIDs)} #for MSU PIDs capitalize leading A
        if(!checkHash(tmpIDs)){ #check if already hashed
                tmpIDs <- unlist(lapply(tmpIDs,function(x) digest(x,serialize=FALSE))) #hash netIDs        
        }
        datIn[,IDfield] <- tmpIDs
        datIn
}

updateDataIn <- function(fileIn,dataIn,IDcolumn,sheetIn=NULL,startRow=0,header=TRUE){
        #------------------ Read in data ------------------------------------------------
        if(tolower(file_ext(fileIn$name))=="csv"){
                dataOut <- hashIDs(dataIn,IDcolumn)
                write.csv(dataOut,file=fileIn$datapath,row.names=FALSE)
        }else{
                dataOut <- hashIDs(dataIn,IDcolumn)
                
                write.xlsx(dataOut,file=fileIn$datapath)
        }
}

sendReportEmail <- function(reportID,question,emailString,outAddress,serverAddress,secretFile){
        #generate and send emails with links to saved reports
        #use_secret_file(secretFile)
        #emailList <- trimws(unlist(strsplit(emailString,",")))
        #subjectOut <- "Your AACR Instructor Feedback Report"
        #bodyOut <- "The AACR Instructor Feedback Report for your data on the %s item is now ready and may be accessed from the following link:
        
        #%s/AutoReport/?reportID=%s
        
        #For help with the report document please see:
        
        #https://apps.beyondmultiplechoice.org/Using%%20the%%20AACR%%20Reports%%20V3.pdf
        #"
        
        #emailOut <- mime(From=outAddress,
        #                Subject=subjectOut,
        #                body=sprintf(bodyOut,question,serverAddress,reportID))
        #for(toAddress in emailList){
        #        emailOut$header <- c(emailOut$header,To=toAddress)
        #}
       # send_message(emailOut)
}
isValidEmail <- function(x) {
        #function by Felix Schoenbrodt (http://www.nicebread.de/validating-email-adresses-in-r/)
        grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

