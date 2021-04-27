# ngram and predict on input
library(tm)

#load question model and dictionary

modelFileIn <- file.path("questionData",params$question,paste0(params$question,"Models",params$version,".Rdata"))
if(!file.exists(modelFileIn)){
        load(file.path("questionData",params$question,paste0(params$question,"Models.Rdata")))#load scoring model
}else{
        load(modelFileIn)#load scoring model
}

if(!is.dictionary(questionDictionary)){
        # questionDictionary <- dictionary(questionDictionary) # reformat for revised quanteda dictionary class
        questionDictionary <- dictionary(unclass(questionDictionary)) # reformat for revised quanteda dictionary class
}

#preprocess responses
responseIn <- preprocessData(datIn(),as.numeric(input$responseColumn),preproc$stemming,preproc$numberFormat,preserveSymbol=preproc$preserveSymbol)

#define corpus
responseIn <- corpus(iconv(responseIn,to="utf-8",sub="")) #quanteda version

#create quanteda document frequency matrix
dmTest <- dfm(responseIn,dictionary=questionDictionary,ngrams=1:2,verbose=FALSE)
#convert to tm::DocumentTermMatrix structure
dmTest <- convert(dmTest,to="tm")

# resort to sparse matrix convention ordering
values$docTermMat <- ResortDtm(dmTest)

#predict rubric level scoring
values$predict <- list()
values$modNameDFList <- list()
for(i in seq_along(models)){
        if(!exists("ensembleProcedure")){
                ensembleProcedure <- "Simple"
        }
        if(!exists("stackingModel")){
                tmp <- predictScoring(values$docTermMat,
                                      models[[i]],
                                      ensembleMethod=ensembleProcedure,
                                      codeList=params$codeList)
        }else{
                tmp <- predictScoring(values$docTermMat,
                                      models[[i]],
                                      ensembleMethod=ensembleProcedure,
                                      stackingModel=stackingModel[[i]],
                                      codeList=params$codeList)
        }
        
        print(str(tmp))
        values$predict[[i]] <- tmp
        values$modNameDFList[[i]] <- modelNames(models[[i]])
}
print(values$predict[[1]]$rubricLevel)
#complete stems in document term matrix
values$docTermMat$dimnames$Terms <- completedNgrams

#set parameters
params$rubricType <- ifelse(length(models)==1,"Typed","Binary")
values$responseIn <-datIn()[,as.numeric(input$responseColumn)]

#write out example data if flag set
print(query$read$writeExample)
print(!is.null(query$read$writeExample))
print(query$read$writeExample=="TRUE")
print(ifelse(!is.null(query$read$writeExample),query$read$writeExample=="TRUE",FALSE))
if(ifelse(!is.null(query$read$writeExample),query$read$writeExample=="TRUE",FALSE)){
        paramsEx <- reactiveValuesToList(params)
        valuesEx <- reactiveValuesToList(values)
        pExIndex <- !(names(paramsEx) %in% c("numMinorVersions","fileDimensions")) # parameters to exclude from example file
        vExIndex <- !(names(valuesEx) %in% c("fileIn","sheetIn")) # values to exclude from example file
        paramsEx <- paramsEx[pExIndex]
        valuesEx <- valuesEx[vExIndex]
        exampleFile <- file.path("questionData",params$question,paste0(params$question,"Example.Rdata"))
        save(paramsEx,valuesEx,file=exampleFile)
}
if(input$emailString!="" & query$testStatus==FALSE){
        paramsEx <- reactiveValuesToList(params)
        valuesEx <- reactiveValuesToList(values)
        pExIndex <- !(names(paramsEx) %in% c("numMinorVersions","fileDimensions")) # parameters to exclude from example file
        vExIndex <- !(names(valuesEx) %in% c("fileIn","sheetIn")) # values to exclude from example file
        paramsEx <- paramsEx[pExIndex]
        valuesEx <- valuesEx[vExIndex]
        params$reportIDout <- stri_rand_strings(1,20)
        saveOutFile <- file.path(savedReportPath,paste0(params$reportIDout,".Rdata"))
        save(paramsEx,valuesEx,file=saveOutFile)
        
        #MODIF - NEW FUNCTION TO SEND E-MAILS 8-24-2020
        sendEmail(params$reportIDout,params$question,input$emailString)
       #sendReportEmail(params$reportIDout,params$question,input$emailString,outAddress,serverAddress,secretFile)
}
