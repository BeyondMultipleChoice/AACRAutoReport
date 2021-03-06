#run cross validation testing
library(caret)
library(data.table)

if(input$rubricType=="Typed"){
        scoreColumnVector <- input$scoreColumn
}else{
        scoreColumnVector <- seq(input$scoreColumn[1],input$scoreColumn[2])
}

#write temp datIn from reactive data
datInTmp <- datIn()


#check for rubric bins with 0 or 1 positive codings and exclude from analysis
if(input$rubricType=="Binary"){
        checkCoding <- sapply(datInTmp[,scoreColumnVector],function(x) (min(table(x))<2)|(length(table(x))==1))
        if(sum(checkCoding)>0){
                
                scoreColumnVector <-scoreColumnVector[!checkCoding] #remove rubric bins with insufficient codings
                values$warningMessages <- c(values$warningMessages,
                                            paste("The following rubric bins do not have sufficent represenation in the data set and have been excluded from this analysis:",
                                                  gsub('[.]'," ",names(checkCoding[checkCoding]))))
        }
}

respColTmp <- as.numeric(input$responseColumn)
rubricLevelNames <- gsub('[.]'," ",colnames(datInTmp)[scoreColumnVector])

#preprocess data
datInTmp[,respColTmp] <- preprocessData(datInTmp,respColTmp,input$stemming,input$numberFormat,
                                        preserveSymbol=input$preserveSymbol,
                                        removeStopwords=params$stopwords)

nrowIn <- nrow(datInTmp)

nFolds <- 10

rubricLevelIndex <- 0
nBins <- length(scoreColumnVector)
progressStepSize <- 1/(nBins*nFolds)
progressStep <- 0
confusionMatrixOut <- list()
predictOutTmp <- numeric()
histDF <- data.frame(Probability=numeric(),Responses=character(),Type=character())

modelsUsed <- c("SVM","SLDA","BOOSTING","BAGGING","RF","GLMNET","TREE","MAXENT") #list of classifier algorithms to use
modColIndex <- seq(1,length(modelsUsed)*2,2)

for(scoreColumn in scoreColumnVector){
        completeIndex <- complete.cases(datInTmp[,scoreColumn]) #index to remove missing scores
        score <- datInTmp[completeIndex,scoreColumn] #remove responses with missing scores
        rubricLevelIndex <- rubricLevelIndex +1
        scoreLevels <- levels(as.factor(score))

        set.seed(837293) #set seed for folding
        testFolds <- createFolds(factor(score),k=nFolds)
        
        predictOut <- numeric()
        probOut <- numeric()
        predDF <- data.frame()
        
        #--------------- n-fold cross validation ----------------------------------------------
        for(i in 1:nFolds){
                progressStep <- progressStep+1
                progress$set(message=paste("Rubric Bin",paste0(rubricLevelIndex,"/",nBins,",")),
                             detail = paste("fold",paste0(i,"/",nFolds)), 
                             value = progressStep*progressStepSize)
                #use quanteda::dfm for robustness
                dmTrain <- dfm(corpus(iconv(datInTmp[-testFolds[[i]],respColTmp],to="utf-8",sub="")), 
                               ngrams=as.numeric(input$ngramDepth),verbose=FALSE)  
                dmTrain <- dfm_trim(dmTrain,sparsity=input$sparsity,verbose=FALSE) #remove sparse terms
                trainDictionary <- as.list(colnames(dmTrain)) #build term dictionary
                names(trainDictionary) <- colnames(dmTrain)
                trainDictionary <- dictionary(trainDictionary)
                dmTest <- dfm(corpus(iconv(datInTmp[testFolds[[i]],respColTmp],to="utf-8",sub="")),
                              dictionary=trainDictionary,ngrams=as.numeric(input$ngramDepth),verbose=FALSE)
                
                dmTrain <- convert(dmTrain,to="tm") 
                dmTest <- convert(dmTest,to="tm")
                
                # resort to sparse matrix convention ordering
                dmTrain <- ResortDtm(dmTrain) 
                dmTest <- ResortDtm(dmTest)
                
                #create containers
                testLength <- length(testFolds[[i]])
                trainLength <- length(score)-testLength
                contTrain <- create_container(dmTrain,score[-testFolds[[i]]],trainSize=1:trainLength,virgin=FALSE)
                contTest <- create_container(dmTest,NULL,testSize=1:testLength,virgin=TRUE)
                
                #train models
                set.seed(37811)
                models <- tryTrainModels(contTrain,algorithms=modelsUsed) #error handling
                
                #classify models
                predicts <- classify_models(contTest,models)
                #remove large objects from memory
                rm(models)
                rm(dmTrain)
                rm(dmTest)
                rm(contTrain)
                rm(contTest)
                
                modIndex <- seq(1,length(predicts),2)
                
                #make tidy prediction data structure such that prediction probability reflects a single code (positive case)
                tmpPredicts <- predicts
                negativeCaseIndex <- tmpPredicts[,modIndex]==scoreLevels[1]
                tmpPredicts[,modIndex+1][negativeCaseIndex] <- 1-tmpPredicts[,modIndex+1][negativeCaseIndex]
                
                predDF <- rbindlist(list(predDF,tmpPredicts),fill=TRUE)
                
        }
        unfoldIndex <- order(unlist(testFolds))
        predDF <- as.data.frame(predDF[unfoldIndex,]) #reorder intermediate predictions to match input ordering
        # save(predDF,file=file.path("/tmp",paste0("tmpScoring",Sys.time(),".Rdata"))) # save individual model scoring for examination
        
        #cv metrics and ensembling by fold
        for(i in 1:nFolds){
                
                #cv weighing, determine true/false positive/negative rates
                cvR <- apply(predDF[-testFolds[[i]],modColIndex],2,function(x) cvRates(x,score[-testFolds[[i]]],scoreLevels)) #returns columnwise TN,FN,FP,TP
                #select FPR for negative codes, TPR for postitve codes
                # cvR <- sapply(seq_along(modColIndex),function(x) ifelse(predDF[testFolds[[i]],modColIndex[x]]==scoreLevels[2],cvR[4,x],cvR[3,x]))
                
                positiveCodes <- sapply(seq_along(modColIndex),function(x) ifelse(predDF[testFolds[[i]],modColIndex[x]]==scoreLevels[2],1,0))
                
                #construct ensemble prediction
                tmpPred <- sapply(seq_along(testFolds[[i]]),
                                  function(x) ifelse(sum(predDF[testFolds[[i]][x],modIndex+1]*diag(data.matrix(cvR[3+positiveCodes[x,],])),na.rm=TRUE)>sum((1-predDF[testFolds[[i]][x],modIndex+1])*diag(data.matrix(cvR[1+positiveCodes[x,],])),na.rm=TRUE),
                                                     scoreLevels[2],scoreLevels[1]))

                
                
                # tmpProb <- apply(cbind(predicts,tmpPred),1,estimateProb)
                tmpProb <- apply(cbind(predDF[testFolds[[i]],],tmpPred),1,estimateProb)
                negativeCaseIndex2 <- tmpPred==scoreLevels[1]
                tmpProb[negativeCaseIndex2] <- 1-tmpProb[negativeCaseIndex2]
                
                predictOut <- c(predictOut,tmpPred)
                probOut <- c(probOut,tmpProb)
                
        }
        
        
        
        
        predictOut2 <- rep(NA,nrowIn)
        probOut2 <- rep(NA,nrowIn)
        predictOut2[completeIndex] <- predictOut[unfoldIndex]
        probOut2[completeIndex] <- probOut[unfoldIndex]
        
        predictOutTmp <- cbind(predictOutTmp,predictOut2,probOut2)
        confMat <-confusionMatrix(factor(predictOut2[completeIndex],scoreLevels),factor(datInTmp[completeIndex,scoreColumn],scoreLevels))
        confusionMatrixOut[[length(confusionMatrixOut)+1]] <- confMat 

        #agreement/probability data frame
        disagreeIndex <- predictOut2[completeIndex]!=datInTmp[,scoreColumn]
        disagreeIndex[is.na(disagreeIndex)] <- FALSE
        if(input$rubricType=="Typed"){
                typeTmp <- score[disagreeIndex]        
        }else{
                typeTmp <- rep(rubricLevelNames[which(scoreColumn==scoreColumnVector)],sum(disagreeIndex,na.rm=TRUE))
        }
        tmpHistDF <- data.frame(Probability=probOut2[completeIndex][disagreeIndex],
                                Responses=rep("Disagree",sum(disagreeIndex)),
                                Type=typeTmp
                                )
        histDF <- rbind(histDF,tmpHistDF)
}

#agreement/probability data frame

if(input$rubricType=="Typed"){
        typeTmp <- as.vector(datInTmp[,scoreColumnVector])
}else{
        typeTmp <- rep(rubricLevelNames,1,each=nrow(predictOutTmp))
}
tmpHistDF <- data.frame(Probability=as.vector(predictOutTmp[,seq(nBins)*2]),
                        Responses=rep("All",nrow(predictOutTmp)),
                        Type=typeTmp
)
histDF <- rbind(histDF,tmpHistDF)
histDF$Responses <- factor(histDF$Responses,levels=c("All","Disagree"))
histDF$Probability <- as.numeric(histDF$Probability)

#assign reactive values
values$responseIn <-datIn()[,respColTmp]
values$predict <- predictOutTmp
values$cvOut <- confusionMatrixOut
values$trainLength <- trainLength
values$scoreLength <- nRrow()
values$rubricLevelNames <- rubricLevelNames
values$histDF <- histDF
gc()