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
#check for rubric bins with 0,1 positive codings and exclude from analysis
if(input$rubricType=="Binary"){
        checkCoding <- sapply(datInTmp[,scoreColumnVector],function(x) (min(table(x))<2)|(length(table(x))==1))
        if(sum(checkCoding)>0){
                scoreColumnVector <-scoreColumnVector[!checkCoding] #remove rubric bins with insufficient codings
        }
}

#make first stem dictionary
tmpStem <- makeFirstStemDictionary(datInTmp,respColTmp)

#preprocess data
responseText <- preprocessData(datInTmp,respColTmp,input$stemming,input$numberFormat,preserveSymbol=input$preserveSymbol)

#construct document term matrix
#use quanteda::dfm for robustness
dmTrain <- dfm(corpus(iconv(responseText,to="utf-8",sub="")),ngrams=1:2,verbose=FALSE) 
# dmTrain <- trim(dmTrain,sparsity=0.99,verbose=FALSE) #remove sparse terms
dmTrain <- dfm_trim(dmTrain,sparsity=0.99,verbose=FALSE) #remove sparse terms
dmTrain <- convert(dmTrain,to="tm")            # convert to tm DTM structure
dmTrain <- ResortDtm(dmTrain)                        # resort dtm to standard sparse matrix convention

questionDictionary <- as.list(colnames(dmTrain)) #build quanteda::dfm format term dictionary
names(questionDictionary) <- colnames(dmTrain)
questionDictionary <- dictionary(questionDictionary)

#make final stem dictionary
tmpQuestionDictionary <- unname(unlist(questionDictionary)) #unlist questionDictionary from quanteda format
tmpQuestionDictionary <- gsub("_"," ",tmpQuestionDictionary) #replace underscore notation with whitespace

# stemDictionary <- makeFinalStemDictionary(questionDictionary,tmpStem)
stemDictionary <- makeFinalStemDictionary(tmpQuestionDictionary,tmpStem) 

#stem complete n-grams
completedNgrams <- stemCompleteText(tmpQuestionDictionary,stemDictionary) 
completedNgrams <- stemReplaceNAs(tmpQuestionDictionary,completedNgrams,dmTrain,datInTmp,respColTmp) 
completedNgrams <- replaceDuplicate(tmpQuestionDictionary,completedNgrams)#replace duplicated stem complete n-grams

models <- list()
values$modNameDFList <- list()

for(i in seq_along(scoreColumnVector)){
        completeIndex <- complete.cases(datInTmp[,scoreColumnVector[i]]) #index to remove missing scores
        trainLength <- length(datInTmp[completeIndex,scoreColumnVector[i]])
        #create container
        contTrain <- create_container(dmTrain[completeIndex,],datInTmp[completeIndex,scoreColumnVector[i]],trainSize=1:trainLength,virgin=FALSE)
        
        #train models
        set.seed(37811) #seed set for reproducibility
        # models[[i]] <- train_models(contTrain,algorithms=c("SVM","SLDA","BOOSTING","BAGGING","RF","GLMNET","TREE","MAXENT"))
        models[[i]] <- tryTrainModels(contTrain,algorithms=c("SVM","SLDA","BOOSTING","BAGGING","RF","GLMNET","TREE","MAXENT"))#error handling
        print(str(models[[i]],max.level=2))
        models[[i]] <- stripBaggingCruft(models[[i]]) #remove large extraneous data structures from bagging model
        print(str(models[[i]],max.level=2))
        values$modNameDFList[[i]] <- modelNames(models[[i]])
        }

#===================== prepare metadata for saving ===========================================
#---------- preprocessing and model parameters ---------------------------------------
preproc <- list()
preproc$stemming <- input$stemming
preproc$numberFormat <- input$numberFormat
preproc$preserveSymbol <- input$preserveSymbol

#set question name parameter to acommidate different querry inputs
if(input$newQuestionQuerry=="1"){
        questionName <- input$questionName
}else{
        questionName <- input$question
}

#define output file locations
outputDir <- file.path("..","AutoReport","questionData",questionName)
dir.create(outputDir,showWarnings = FALSE)
modFileOut <- file.path(outputDir,paste0(questionName,"Models",params$version,".Rdata"))
metaFileOut <- file.path(outputDir,paste0(questionName,"Metadata.Rdata"))
minorFileOut <- file.path(outputDir,paste0(questionName,"MinorMetadata",trunc(as.numeric(params$version)),".Rdata"))

#---------------- load existing data structures or initialize new structures -----------------
if(input$newQuestionQuerry=="1"){
        #initalize data structures for new items
        ## major version data structures
        questionRubric <- list()
        questionText <- NULL
        rubricType <- NULL
        groupLevels <- list()
        majorVersionDescription <- NULL
        numMinorVersions <- NULL
        
        ## minor version data structures
        minorVersionDescription <- NULL
        versionDate <- NULL
        codeList <- NULL
}else if(input$newMajorQuerry=="1"){
        #for new major versions
        ##load major version sturcture
        load(metaFileOut)
        
        ## initialize minor version data structures
        minorVersionDescription <- NULL
        versionDate <- NULL
        codeList <- NULL
}else{
        #for new minor versions of existing major versions
        ##load major & minor version data structures
        load(metaFileOut)
        load(minorFileOut)
}

#------------ concatenate new metadata -------------------
minorVersionDescription <- c(input$minorVersionDescription,minorVersionDescription)
versionDate <- c(Sys.Date(),versionDate)

if(!is.null(input$positiveCode)){
        codeList <- c(list(c(params$codeList[params$codeList!=input$positiveCode],input$positiveCode)),codeList)
}else{
        codeList <- c(NA,codeList)
}

ensembleProcedureList <- c("CVPWeight","PWeight","Simple","PLR_L1","PLR_L2")
ensembleProcedure <- ifelse(input$rubricType=="Typed","Simple",ensembleProcedureList[as.numeric(input$ensembleMethod)])

#=================== save models and question metadata ============================================
#update major version metadata
if(input$newQuestionQuerry=="1" |params$newMajorQuerry=="1"){ 
        questionRubric <- c(list(values$hot),questionRubric)
        questionText <- c(input$questionText,questionText)
        rubricType <- c(input$rubricType,rubricType)
        groupLevels <- c(list(as.character(values$hot[,"Rubric Level"])),groupLevels)
        majorVersionDescription <- c(input$majorVersionDescription,majorVersionDescription)
        numMinorVersions <- c(1,numMinorVersions)
        
}else{
        # increment the number of minor versions of the appropriate major version
        numMinorVersions[[selectedMV()]] <- numMinorVersions[[selectedMV()]]+1 
}
save(questionText,rubricType,groupLevels,questionRubric,majorVersionDescription,numMinorVersions,file=metaFileOut)

#save models and minor version metadata files
save(models,questionDictionary,completedNgrams,preproc,ensembleProcedure,file=modFileOut)
save(codeList,minorVersionDescription,versionDate,file=minorFileOut)


#update question list
if(input$newQuestionQuerry==1){
        load(file.path("..","AutoReport","questionData","questionList.Rdata")) #load question list
        questionList <- unique(c(questionList,input$questionName))
        save(questionList, file=file.path("..","AutoReport","questionData","questionList.Rdata")) #load question list
}

