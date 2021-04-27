#AACR classifier training support functions

library(openxlsx)
library(readxl)
library(RTextTools)
library(tm)
library(caret)
library(SnowballC)
library(quanteda)
library(stringi)
library(tools)
library(ggplot2)
library(plyr)

source(file.path("functions","homogenizeNumbers.R")) #load support functions
source(file.path("functions","printConfusionMatrix2.R")) #load support functions


preLoadxlsx <- function(fileIn){
        if(tolower(file_ext(fileIn$name))=="csv"){
                junk <- read.csv(fileIn$datapath,stringsAsFactors = FALSE)
                nSheets <- 1
        }else{
                nSheets <- length(excel_sheets(fileIn$datapath))         #count number of sheets
                junk <- read.xlsx(fileIn$datapath,sheet=nSheets)
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

removeHTMLtags <- function(x){
        tmp <- gsub("<.*?>", "", x)
        tmp
}

stemText <- function(x){
        tmp <- strsplit(x,"[[:space:]]")
        tmp <- lapply(tmp,function(x) paste(stemDocument(x[x!=""]),collapse=" "))
        unlist(tmp)
}

stemReplaceNAs <- function(stemmedText,completedText,dtmIn,datIn,question){
        completedTokens <- unlist(strsplit(completedText,"[[:space:]]")) #tokenize stem completed text
        stemmedTokens <- unlist(strsplit(stemmedText,"[[:space:]]")) #tokens stemmed text
        naIndex <- grep("NA",completedTokens) #find completed tokens that are "NA"
        problemWords <- unique(stemmedTokens[naIndex]) #select problem words
        for(pWord in problemWords){
                tmpIndex <- which(as.vector(dtmIn[,pWord])==1)[1] #adoption for quanteda dfm object
                # problemResponse <- datIn[tmpIndex,question]
                problemResponse <- iconv(datIn[tmpIndex,question],to="utf-8",sub="") #force character encoding, remove non-comformat characters
                problemResponse <- tolower(problemResponse)
                problemResponse <- unlist(strsplit(problemResponse,"[[:space:]]"))
                responseDistVector <- adist(pWord,problemResponse,costs=list("i"=1,"d"=3,"s"=2)) #calculate edit distances
                pWordIndex <- which.min(responseDistVector) #find minimum edit distance index
                originalWord <- problemResponse[pWordIndex] #determine original word
                lineIndex <- grep(pWord,stemmedText)
                if(is.integer(pWordIndex) && length(pWordIndex) != 0L){
                        completedText[lineIndex] <- gsub("NA",originalWord,completedText[lineIndex]) #replace "NA" with or        
                }else{
                        completedText[lineIndex] <- "NA"
                }
                
        }
        completedText
}

stemCompleteText <- function(x,stemDictionary,type="first"){
        tmp <- strsplit(x,"[[:space:]]")
        tmp <- lapply(tmp,function(x) paste(stemCompletion(x[x!=""],stemDictionary,type=type),collapse=" "))
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
        mean(as.numeric(rowIn[tmpIndex]))
}

makeFirstStemDictionary <- function(datIn,question){
        localStopwords <- c("the","a","in","and")
        
        tmp <- iconv(datIn[,question],to="utf-8",sub="") #force character encoding, remove non-comformat characters
        tmp <- tolower(tmp)
        tmp <- removeWords(tmp,localStopwords)
        tmp <- removeNumbers(tmp)
        tmp <- removePunctuation(tmp)
        tmp <- unlist(strsplit(tmp,"[[:space:]]"))
        tmp <- tmp[tmp!=""]
        tmp <- names(sort(table(tmp),decreasing = TRUE))
        tmp
}

makeFinalStemDictionary <- function(x,dictIn){
        tmp <- stemCompleteText(x,dictIn,type="first")
        # tmp <- stemCompleteText(x,dictIn,type="shortest")
        tmp <- unlist(strsplit(tmp,"[[:space:]]"))
        tmp <- unique(tmp)
        tmp <- tmp[tmp!=""]
        tmp
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

tryTrainModels <- function(containerIn,algorithms){
        outModels <- list()
        for(alg in algorithms){
                modelTmp <- try(train_model(containerIn,alg))
                if(class(modelTmp)!="try-error"){
                        outModels[[alg]] <- modelTmp
                }
        }
        outModels
}

modelNames <- function(modelListIn){
        modelNameDictionary <- list(SVM="Support Vector Machines",
                                    SLDA="Supervised Latent Dirichlet Allocation",
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
guessResponseCol <- function(datIn){
        nCharsCol <-sapply(1:ncol(datIn),function(x) sum(stri_length(datIn[,x]),na.rm=TRUE))
        which.max(nCharsCol)
}
makeReportPDF <- function(fileName,input,params,values){
        pdfFile <- file.path("reporting",paste0("cv",input$rubricType,"PDF.Rmd"))
        out <- rmarkdown::render(pdfFile,output_file=paste0(stri_rand_strings(1,20),".pdf"),
                                 output_dir="/tmp",intermediates_dir = "/tmp")
}
agreementPlot <- function(histDF,fontSize=18){
        ggplot(histDF, aes(x=Probability, fill=Responses)) +
                geom_histogram(binwidth=.05, position="identity")+
                labs(y="Count",x="Prediction Probability")+
                ggtitle("Prediction probability for all classes")+ 
                theme_grey(base_size = fontSize)
}
agreementPlotLevels <- function(histDF,rubricType,fontSize=18){
        tmpTitle <- ifelse(rubricType=="Typed","Prediction probability by human score","Prediction probability by rubric level")
        ggplot(histDF, aes(x=Probability, fill=Responses))+
                geom_histogram(binwidth=.05, position="identity")+
                facet_wrap(~Type,nrow=ceiling(length(unique(histDF$Type))/3))+
                labs(y="Number of Responses",x="Prediction Probability")+
                ggtitle(tmpTitle)+ 
                theme_grey(base_size = fontSize)
}
expectedActualDisagreement <- function(histDF){
        cuts <- seq(0.9,0.5,-0.1)
        predProb <- histDF[histDF$Responses=="All","Probability"]
        misClassProb <- histDF[histDF$Responses=="Disagree","Probability"]
        expected <- sapply(cuts,function(x) round(sum(1-predProb[predProb>=x]),2))
        actual <- sapply(cuts,function(x) sum(misClassProb>=x))
        cbind(expected,actual)
}

agreementTable <- function(histDF){
        cuts <- seq(0.9,0.5,-0.1)
        cutNames <- paste0("P>",cuts)
        typeNames <- levels(as.factor(histDF$Type))

        emDF <- expectedActualDisagreement(histDF)
        tmpDF <- ddply(histDF,.(Type),expectedActualDisagreement)
        tmpDF <- as.data.frame(split(tmpDF,as.factor(tmpDF$Type)))
        noTypeIndex <- grepl("(expected|actual)",colnames(tmpDF))
        tmpDF <- tmpDF[,noTypeIndex]
        emDF <- cbind(emDF,tmpDF)
        emDF <- t(emDF)
        colnames(emDF) <- cutNames
        rownames(emDF) <- paste0(rep(c("All",paste("Rubric bin", typeNames)),1,each=2),":",
                                                 c(" expected disagreements"," actual disagreements"))
        round(emDF)
        
}

replaceDuplicate <- function(stemmedIn,completedIn){
        #replace duplicate entries in re-stemmed n-gram list due to incorrect restemming of terms
        completedOut <- completedIn
        iterationCount <- 1
        while(any(duplicated(completedOut)) & iterationCount < 10){
                dupIndex <- which(completedOut %in% completedOut[duplicated(completedOut)])
                
                tmp <- cbind(stemmedIn[dupIndex],completedOut[dupIndex],dupIndex)
                tmp <- as.data.frame(tmp[order(tmp[,2]),])
                tmp <- merge(tmp[seq(1,nrow(tmp),2),],tmp[seq(2,nrow(tmp),2),],by="V2")
                tmp <- unname(as.matrix(tmp))
                if(nrow(tmp)>1){
                        tmp2 <- apply(tmp[,c(2,4)],2,nchar)
                        minIndex <- apply(tmp2,1,which.min)
                        minTerm <-matrix(unlist(lapply(seq_along(minIndex),function(x) tmp[x,c(2*minIndex[x],2*minIndex[x]+1)])),ncol=2,byrow=TRUE)        
                }else if(nrow(tmp)==1){
                        tmp2 <- nchar(tmp[,c(2,4)])
                        minIndex <- which.min(tmp2)
                        minTerm <-matrix(unlist(tmp[1,c(2*minIndex,2*minIndex+1)]),ncol=2,byrow=TRUE)        
                }
                completedOut[as.numeric(as.character(minTerm[,2]))] <- as.character(minTerm[,1])
                
                iterationCount <- iterationCount+1
        }
        
        completedOut
}

whichShorter <- function(charVectorIn){
        charVectorIn[which.min(nchar(charVectorIn))]
}

resave <- function(..., list = character(), file) {
        ##add/replace objects in saved Rdata file
        ## solution by flodel
        ## http://stackoverflow.com/questions/11813096/updating-an-existing-rdata-file
        previous  <- load(file)
        var.names <- c(list, as.character(substitute(list(...)))[-1L])
        for (var in var.names) assign(var, get(var, envir = parent.frame()))
        save(list = unique(c(previous, var.names)), file = file)
}
cvRates <- function(predictIn,scoreIn,levelsIn){
        predictTmp <- factor(predictIn,levels=levelsIn)
        scoreTmp <- factor(scoreIn,levels=levelsIn)
        ratesOut <- table(predictTmp,scoreTmp) #calculate TN,FN,FP,TP
        ratesOut <- ratesOut/rowSums(ratesOut) # convert to TNR,FNR,FPR,TPR
        
        ratesOut
}
stripBaggingCruft <- function(models){
        if("BAGGING" %in% names(models)){
                for(i in 1:25){
                        attr(models[["BAGGING"]][["mtrees"]][[i]][["btree"]][["terms"]],".Environment") <- NULL
                        if(!is.null(models[["BAGGING"]][["mtrees"]][[i]][["btree"]][["call"]][["data"]])){
                                models[["BAGGING"]][["mtrees"]][[i]][["btree"]][["call"]][["data"]] <- NULL
                        }
                }        
        }
        models
}