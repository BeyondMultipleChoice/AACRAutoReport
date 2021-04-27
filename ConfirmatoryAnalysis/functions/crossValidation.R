#run cross validation testing
library(caret)

if(input$rubricType=="Typed"){
        scoreColumnVector <- input$scoreColumn
}else{
        scoreColumnVector <- seq(input$scoreColumn[1],input$scoreColumn[2])
}

#write temp datIn from reactive data
datInTmp <- datIn()

# datInTmp <- datInTmp[complete.cases(datInTmp[,scoreColumnVector]),] #remove responses with missing scores

#check for rubric bins with 0,1 positive codings and exclude from analysis
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

for(scoreColumn in scoreColumnVector){
        completeIndex <- complete.cases(datInTmp[,scoreColumn]) #index to remove missing scores
        score <- datInTmp[completeIndex,scoreColumn] #remove responses with missing scores
        rubricLevelIndex <- rubricLevelIndex +1
        scoreLevels <- levels(as.factor(score))

        set.seed(837293) #set seed for folding
        testFolds <- createFolds(factor(score),k=nFolds)
        
        predictOut <- numeric()
        probOut <- numeric()
        #--------------- n-fold cross validation ----------------------------------------------
        for(i in 1:nFolds){
                progressStep <- progressStep+1
                progress$set(message=paste("Rubric Bin",paste0(rubricLevelIndex,"/",nBins,",")),
                             detail = paste("fold",paste0(i,"/",nFolds)), 
                             value = progressStep*progressStepSize)
                #use quanteda::dfm for robustness
                dmTrain <- dfm(corpus(iconv(datInTmp[-testFolds[[i]],respColTmp],to="utf-8",sub="")), 
                               ngrams=1:2,verbose=FALSE)  
                dmTrain <- dfm_trim(dmTrain,sparsity=0.99,verbose=FALSE) #remove sparse terms
                # dmTrain <- trim(dmTrain,sparsity=0.99,verbose=FALSE) #remove sparse terms
                
                trainDictionary <- as.list(colnames(dmTrain)) #build term dictionary
                names(trainDictionary) <- colnames(dmTrain)
                trainDictionary <- dictionary(trainDictionary)
                
                dmTest <- dfm(corpus(iconv(datInTmp[testFolds[[i]],respColTmp],to="utf-8",sub="")),
                              dictionary=trainDictionary,ngrams=1:2,verbose=FALSE)
                
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
                #models <- train_models(contTrain,algorithms=c("SVM","SLDA","BOOSTING","BAGGING","RF","GLMNET","TREE","NNET","MAXENT"))
                # models <- train_models(contTrain,algorithms=c("SVM","SLDA","BOOSTING","BAGGING","RF","GLMNET","TREE","MAXENT"))
                # models <- train_models(contTrain,algorithms=rownames(values$modNameDFList[[rubricLevelIndex]])) #use only models used in stored ensemble object
                models <- tryTrainModels(contTrain,algorithms=c("SVM","SLDA","BOOSTING","BAGGING","RF","GLMNET","TREE","MAXENT")) #error handling
                
                #classify models
                predicts <- classify_models(contTest,models)
                modIndex <- seq(1,length(predicts),2)
                # tmpPred <- apply(predicts[,modIndex],1,function(x) as.numeric(names(which.max(table(x)))))
                tmpPred <- apply(predicts[,modIndex],1,function(x) names(which.max(table(x))))
                tmpProb <- apply(cbind(predicts,tmpPred),1,estimateProb)
#                 predOut$rubricLevel <- apply(classTmp[,classIndex],1,function(x) as.numeric(names(which.max(table(x)))))
#                 predOut$probability <- apply(cbind(classTmp,predOut$rubricLevel),1,estimateProb)
                
                
                predictOut <- c(predictOut,tmpPred)
                probOut <- c(probOut,tmpProb)
        }
        
        predictOut2 <- rep(NA,nrowIn)
        probOut2 <- rep(NA,nrowIn)
        predictOut2[completeIndex] <- predictOut[order(unlist(testFolds))]
        probOut2[completeIndex] <- probOut[order(unlist(testFolds))]
        
        predictOutTmp <- cbind(predictOutTmp,predictOut2,probOut2)
        confMat <-confusionMatrix(factor(predictOut2[completeIndex],scoreLevels),factor(datInTmp[completeIndex,scoreColumn],scoreLevels))
        confusionMatrixOut[[length(confusionMatrixOut)+1]] <- confMat 

        #agreement/probability data frame
        # disagreeIndex <- predictOut2!=score
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
# save(histDF,file="/tmp/histDF.Rdata")

#assign reactive values
# values$predict <- list(predictOut2,probOut2)
values$responseIn <-datIn()[,respColTmp]
values$predict <- predictOutTmp
values$cvOut <- confusionMatrixOut
values$trainLength <- trainLength
values$scoreLength <- nRrow()
# values$rubricLevelNames <- gsub('[.]'," ",colnames(datIn()[,scoreColumnVector]))
values$rubricLevelNames <- rubricLevelNames
values$histDF <- histDF