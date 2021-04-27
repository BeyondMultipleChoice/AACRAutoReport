#run cross validation testing
library(caret)
library(data.table)
library(smotefamily)
library(slam)

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
datInTmp[,respColTmp] <- preprocessData(datInTmp,respColTmp,input$stemming,input$numberFormat,preserveSymbol=input$preserveSymbol)

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

minMajRatio <- 1 #ratio of minority class to majority class for SMOTE

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
                               ngrams=1:2,verbose=FALSE)
                #---------- handle SMOTE oversampling ---------------
                scoreCounts <- table(score[-testFolds[[i]]])
                print(scoreCounts)
                
                dup_size <- minMajRatio*max(scoreCounts)/min(scoreCounts)-1 # set minority/majority class ratio to 2/3
                                print("hi")
                print(dup_size)
                if(dup_size>0){
                        dmTrain <- as.data.frame(dmTrain) # convert to data frame
                        nTermsTmp <- ncol(dmTrain)
                        dmTrain <- SMOTE(X=dmTrain,                  
                                         target=score[-testFolds[[i]]],
                                         dup_size = dup_size) # conduct SMOTE resampling
                        print("there")
                        trainScores <- dmTrain$data[,nTermsTmp+1] #extract scores from smotefamily data structure
                        dmTrain <- dmTrain$data[,1:nTermsTmp] #extract dtm data frame from smotefamily data structure
                        dmTrain <- as.simple_triplet_matrix(dmTrain) #convert to slam simple triplet matrix
                        dmTrain <- tm::as.DocumentTermMatrix(dmTrain,weighting = weightTf) #convert to tm DocumentTermMatrix
                        
                }else{
                        dmTrain <- convert(dmTrain,to="tm")
                        trainScores <- score[-testFolds[[i]]]
                }
                #------ end SMOTE processing ---------------
                
                dmTrain <- removeSparseTerms(dmTrain,sparse=0.99) #remove sparse terms
                # dmTrain <- dfm_trim(dmTrain,sparsity=0.99,verbose=FALSE) #remove sparse terms
                # dmTrain <- trim(dmTrain,sparsity=0.99,verbose=FALSE) #remove sparse terms
                trainDictionary <- as.list(colnames(dmTrain)) #build term dictionary
                names(trainDictionary) <- colnames(dmTrain)
                trainDictionary <- dictionary(trainDictionary)
                dmTest <- dfm(corpus(iconv(datInTmp[testFolds[[i]],respColTmp],to="utf-8",sub="")),
                              dictionary=trainDictionary,ngrams=1:2,verbose=FALSE)
                
                # dmTrain <- convert(dmTrain,to="tm") 
                dmTest <- convert(dmTest,to="tm")
                
                # resort to sparse matrix convention ordering
                dmTrain <- ResortDtm(dmTrain) 
                dmTest <- ResortDtm(dmTest)
                
                #create containers
                testLength <- length(testFolds[[i]])
                trainLength <- length(trainScores)
                # trainLength <- length(score)-testLength
                contTrain <- create_container(dmTrain,trainScores,trainSize=1:trainLength,virgin=FALSE)
                # contTrain <- create_container(dmTrain,score[-testFolds[[i]]],trainSize=1:trainLength,virgin=FALSE)
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
                # print(colnames(predicts))
                
                #make tidy prediction data structure such that prediction probability reflects a single code (positive case)
                tmpPredicts <- predicts
                negativeCaseIndex <- tmpPredicts[,modIndex]==scoreLevels[1]
                tmpPredicts[,modIndex+1][negativeCaseIndex] <- 1-tmpPredicts[,modIndex+1][negativeCaseIndex]
                
                predDF <- rbindlist(list(predDF,tmpPredicts),fill=TRUE)
                
                #predictOut <- c(predictOut,tmpPred)
                #probOut <- c(probOut,tmpProb)
        }
        unfoldIndex <- order(unlist(testFolds))
        predDF <- as.data.frame(predDF[unfoldIndex,]) #reorder intermediate predictions to match input ordering
        save(predDF,file=file.path("/tmp",paste0("tmpScoring",Sys.time(),".Rdata"))) # save individual model scoring for examination
        
        
        # print(mem_used())
        # print(sapply(ls(),object_size))
        
        #cv metrics and ensembling by fold
        for(i in 1:nFolds){
                
                #cv weighing, determine true/false positive/negative rates
                cvR <- apply(predDF[-testFolds[[i]],modColIndex],2,function(x) cvRates(x,score[-testFolds[[i]]],scoreLevels)) #returns columnwise TN,FN,FP,TP
                #select FPR for negative codes, TPR for postitve codes
                # cvR <- sapply(seq_along(modColIndex),function(x) ifelse(predDF[testFolds[[i]],modColIndex[x]]==scoreLevels[2],cvR[4,x],cvR[3,x]))
                
                positiveCodes <- sapply(seq_along(modColIndex),function(x) ifelse(predDF[testFolds[[i]],modColIndex[x]]==scoreLevels[2],1,0))
                
                # print(positiveCodes[3,])
                # print(cvR[3:4,])
                # print(cvR[3+positiveCodes[3,],])#this is wrong, want a single row of CV+ values
                # print(diag(data.matrix(cvR[3+positiveCodes[3,],])))#this is wrong, want a single row of CV+ values
                # print(sum(predDF[testFolds[[i]][3],modIndex+1]*cvR[3+positiveCodes[3,],],na.rm=TRUE))
                #construct ensemble prediction
                # tmpPred <- sapply(seq(nrow(tmpPredicts)),
                #                   function(x) ifelse(sum(tmpPredicts[x,modIndex+1])>(length(modIndex)-sum(cvR[x,])),
                #                                      scoreLevels[2],scoreLevels[1]))

                # tmpPred <- sapply(seq_along(testFolds[[i]]),
                #                   function(x) ifelse(sum(predDF[testFolds[[i]][x],modIndex+1],na.rm=TRUE)>(length(modIndex)-sum(cvR[x,],na.rm=TRUE)),
                #                                      scoreLevels[2],scoreLevels[1]))
                # 
                tmpPred <- sapply(seq_along(testFolds[[i]]),
                                  function(x) ifelse(sum(predDF[testFolds[[i]][x],modIndex+1]*diag(data.matrix(cvR[3+positiveCodes[x,],])),na.rm=TRUE)>sum((1-predDF[testFolds[[i]][x],modIndex+1])*diag(data.matrix(cvR[1+positiveCodes[x,],])),na.rm=TRUE),
                                                     scoreLevels[2],scoreLevels[1]))

                
                
                # tmpProb <- apply(cbind(predicts,tmpPred),1,estimateProb)
                tmpProb <- apply(cbind(predDF[testFolds[[i]],],tmpPred),1,estimateProb)
                negativeCaseIndex2 <- tmpPred==scoreLevels[1]
                tmpProb[negativeCaseIndex2] <- 1-tmpProb[negativeCaseIndex2]
                
                predictOut <- c(predictOut,tmpPred)
                probOut <- c(probOut,tmpProb)
                
                # print(mem_used())
                # print(sapply(ls(),object_size))
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

print(head(histDF))

#assign reactive values
values$predict <- predictOutTmp
values$cvOut <- confusionMatrixOut
values$trainLength <- trainLength
values$scoreLength <- nRrow()
values$rubricLevelNames <- rubricLevelNames
values$histDF <- histDF
gc()