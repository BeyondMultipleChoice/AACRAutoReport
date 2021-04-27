#ngram and model

#write temp datIn from reactive data
datInTmp <- datIn()
respColTmp <- as.numeric(input$responseColumn)

#score columns
if(input$rubricType=="Typed"){
        scoreColumnVector <- input$scoreColumn
}else{
        scoreColumnVector <- seq(input$scoreColumn[1],input$scoreColumn[2])
}
#make first stem dictionary
tmpStem <- makeFirstStemDictionary(datInTmp,respColTmp)

#preprocess data
responseText <- preprocessData(datInTmp,respColTmp)

#construct document term matrix
#use quanteda::dfm for robustness
dmTrain <- dfm(corpus(iconv(responseText,to="utf-8",sub="")),ngrams=1:2,verbose=FALSE) 
dmTrain <- trim(dmTrain,sparsity=0.99,verbose=FALSE) #remove sparse terms
# dmTrain <- as.DocumentTermMatrix(dmTrain)            # convert to tm DTM structure
dmTrain <- convert(dmTrain,to="tm")            # convert to tm DTM structure
dmTrain <- ResortDtm(dmTrain)                        # resort dtm to standard sparse matrix convention

questionDictionary <- as.list(colnames(dmTrain)) #build quanteda::dfm format term dictionary
names(questionDictionary) <- colnames(dmTrain)
questionDictionary <- dictionary(questionDictionary)

# dmTrain <- DocumentTermMatrix(Corpus(VectorSource(iconv(responseText,to="utf-8",sub=""))), #tm Version
#                               control=list(tokenize=newBigramTokenizer))
# dmTrain <- removeSparseTerms(dmTrain,0.99)

# questionDictionary <- Terms(dmTrain)

#make final stem dictionary
tmpQuestionDictionary <- unname(unlist(questionDictionary)) #unlist questionDictionary from quanteda format
tmpQuestionDictionary <- gsub("_"," ",tmpQuestionDictionary) #replace underscore notation with whitespace

# stemDictionary <- makeFinalStemDictionary(questionDictionary,tmpStem)
stemDictionary <- makeFinalStemDictionary(tmpQuestionDictionary,tmpStem) 

#stem complete n-grams
# completedNgrams <- stemCompleteText(questionDictionary,stemDictionary)
# completedNgrams <- stemReplaceNAs(questionDictionary,completedNgrams,dmTrain,datInTmp,respColTmp)
completedNgrams <- stemCompleteText(tmpQuestionDictionary,stemDictionary) 
completedNgrams <- stemReplaceNAs(tmpQuestionDictionary,completedNgrams,dmTrain,datInTmp,respColTmp) 

trainLength <- nrow(datInTmp)
models <- list()
values$modNameDFList <- list()

for(i in seq_along(scoreColumnVector)){
        #create container
        contTrain <- create_container(dmTrain,datInTmp[,scoreColumnVector[i]],trainSize=1:trainLength,virgin=FALSE)
        
        #train models
        set.seed(37811) #seed set for reproducibility
        # models[[i]] <- train_models(contTrain,algorithms=c("SVM","SLDA","BOOSTING","BAGGING","RF","GLMNET","TREE","MAXENT"))
        models[[i]] <- tryTrainModels(contTrain,algorithms=c("SVM","SLDA","BOOSTING","BAGGING","RF","GLMNET","TREE","MAXENT")) #error handling
        values$modNameDFList[[i]] <- modelNames(models[[i]])
        }

#prepare metadata for saving
questionRubric <- values$hot
questionText <- input$questionText
rubricType <- input$rubricType
groupLevels <- as.character(values$hot[,1])


#save models and question metadata
outputDir <- file.path("..","AutoReport","questionData",input$questionName)
dir.create(outputDir)
modFileOut <- file.path(outputDir,paste0(input$questionName,"Models.Rdata"))
save(models,questionDictionary,completedNgrams,file=modFileOut)
metaFileOut <- file.path(outputDir,paste0(input$questionName,"Metadata.Rdata"))
save(questionText,rubricType,groupLevels,questionRubric,file=metaFileOut)

#update question list
load(file.path("..","AutoReport","questionData","questionList.Rdata")) #load question list
questionList <- c(questionList,input$questionName)
save(questionList, file=file.path("..","AutoReport","questionData","questionList.Rdata")) #load question list
