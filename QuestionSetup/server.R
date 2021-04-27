#AACR question setup app server
library(shiny)
library(rhandsontable)
library(DT)

source(file.path("functions","supportFunctions.R")) #load support functions

shinyServer(function(input,output){
#====================== initialize reactive values objects ======================================
        runStage <- reactiveValues()
        values <- reactiveValues()
        params <- reactiveValues()
        setHot <- function(x){ values[["hot"]] <- x}
        
        #read question list
        load(file.path("..","AutoReport","questionData","questionList.Rdata")) #load question list
        params$questionList <- questionList
        
#===================== Reactive observers ========================================================       
        #read in data
        observe({
                if(is.null(runStage$fileIn)){return()} # check if
                params$fileDimensions <- preLoadxlsx(values$fileIn)
        })
        observe({
                if(is.null(input$sheeetIn)){return()} # check if
        })
        observe({
                if(!is.null(values$fileIn)){runStage$fileIn <- TRUE} #file input has processed
        })
        observe({
                if(!is.null(params$fileDimensions)){runStage$preRead <- TRUE} #preread has processed
        })
        observe({
                if(!is.null(input$responseColumn)){
                        runStage$runRead <- TRUE
                } #post read has processed
        })
        observeEvent(input$fileIn,{
                values$fileIn <- input$fileIn
                fExt <- tools::file_ext(values$fileIn$name)
                if(fExt %in% c("xlsx","xls")){
                        file.rename(values$fileIn$datapath,paste(values$fileIn$datapath,fExt,sep="."))
                        values$fileIn$datapath <- paste(values$fileIn$datapath,fExt,sep=".")
                }
        })
        
        observe({
                if(is.null(runStage$runRead) | (input$rubricType=="Typed")){return()} # check if
                if(!runStage$runRead | (length(input$scoreColumn) < 2)){return()}
                scoresTmp <- unlist(datIn()[,input$scoreColumn[1]:input$scoreColumn[2]],use.names=FALSE)
                params$codeList <- names(sort(table(scoresTmp),decreasing = TRUE)[1:2])
                
        })
        observe({
                if(is.null(runStage$runRead)){return()}
                if((input$rubricType=="Typed") & (length(input$scoreColumn)>1)){return()}
                if(!(input$rubricType=="Typed") & (length(input$scoreColumn)==1)){return()}
                if(is.null(input$rubricLevelTable)){
                                if (input$rubricType=="Typed"){
                                nrowTMP <- length(unique(datIn()[,input$scoreColumn]))
                                DF <- as.data.frame(matrix("",nrow=nrowTMP,ncol=3))
                                colnames(DF) <- c("Code","Rubric Level","Description")
                                codeTmp <- sort(unique(datIn()[,as.numeric(input$scoreColumn)]))
                                if(length(codeTmp)>0){DF$Code <- codeTmp}
                        }else{
                                nrowTMP <- diff(input$scoreColumn)+1
                                if(length(nrowTMP)>0){
                                        DF <- as.data.frame(matrix("",nrow=nrowTMP,ncol=2))
                                        colnames(DF) <- c("Rubric Level","Description")
                                        
                                }
                                                        }        
                }else{
                        DF <- hot_to_r(input$rubricLevelTable)
                        if (input$rubricType=="Typed"){
                                DF[,"Code"] <- sort(unique(datIn()[,as.numeric(input$scoreColumn)]))
                        }else{
                                DF <- DF[,c("Rubric Level","Description")]
                        }
                }
                
                if(!is.null(input$headerCheck)){
                        if(input$headerCheck){
                                if(input$rubricType=="Typed"){
                                        scoreColumnVectorTmp <- input$scoreColumn
                                }else{
                                        scoreColumnVectorTmp <- seq(input$scoreColumn[1],input$scoreColumn[2])
                                }
                                
                                DF["Rubric Level"] <- gsub('[.]'," ",colnames(datIn()[,scoreColumnVectorTmp]))
                        }
                }
                values$DF <- DF
        })
        
        #observe question status querries and read in data as necessary & set version number
        observe({ 
                if(is.null(input$newQuestionQuerry)){return()}
                # if(is.null(input$question)){return()}
                if(input$newQuestionQuerry=="2"){ # if item exists
                        if(is.null(input$question)){return()}
                        questionInDir <- file.path("..","AutoReport","questionData",input$question)
                        metaFileIn <- file.path(questionInDir,paste0(input$question,"Metadata.Rdata"))
                        #load question metadata
                        load(metaFileIn)
                        #assign to reactive data structures
                        params$questionText <- questionText
                        params$questionRubric <- questionRubric
                        params$majorVersionDescription <- majorVersionDescription
                        params$numMinorVersions <- numMinorVersions
                        if(input$newMajorQuerry=="1"){
                                params$version <- paste(length(params$questionText)+1,"0",sep=".")
                        }else{
                                if(is.null(input$selectMajorVersion)){return()}
                                params$version <- paste(length(params$questionText)-selectedMV()+1,
                                                        params$numMinorVersions[[selectedMV()]],
                                                        sep=".")
                        }
                }else{
                        params$version <- "1.0"
                }
        })
        
        
#------------- train scoring model, run cross validation, save models -------------------------------------
        observeEvent(input$goButton,{
                if(is.null(input$newMajorQuerry)){
                        params$newMajorQuerry <- "0"
                }else{
                        params$newMajorQuerry <- input$newMajorQuerry
                }
                withProgress(message="Calculating", value=0,{
                        incProgress(0.10,detail="Training Models")
                        # source(file.path("functions","trainModel.R"),local=TRUE)
                        if(input$rubricType=="Typed"){
                                source(file.path("functions","trainModel.R"),local=TRUE)
                        }else{
                                if(input$ensembleMethod %in% 1:3){
                                        source(file.path("functions","trainModel.R"),local=TRUE)
                                }else if(input$ensembleMethod==4){
                                        source(file.path("functions","trainModelPLRensembleL1.R"),local=TRUE)
                                }else if(input$ensembleMethod==5){
                                        source(file.path("functions","trainModelPLRensemble.R"),local=TRUE)
                                }
                        }
                        incProgress(0.25,detail="Cross Validation")
                        progress <- shiny::Progress$new()
                        progress$set(message = "Rubric Bin", value = 0)
                        on.exit(progress$close())
                        if(input$rubricType=="Typed"){
                                source(file.path("functions","crossValidation.R"),local=TRUE)
                        }else{
                                if(input$ensembleMethod==1){
                                        source(file.path("functions","crossValidationCVProbWeight.R"),local=TRUE)
                                }else if(input$ensembleMethod==2){        
                                        source(file.path("functions","crossValidationProbWeight.R"),local=TRUE)
                                }else if(input$ensembleMethod==3){
                                        source(file.path("functions","crossValidation.R"),local=TRUE)
                                }else if(input$ensembleMethod==4){
                                        source(file.path("functions","crossValidationPLRensembleL1.R"),local=TRUE)
                                }else if(input$ensembleMethod==5){
                                        source(file.path("functions","crossValidationPLRensemble.R"),local=TRUE)
                                }
                        }
                })
        })
        
#======================== reactive functions ==============================================================       
        datIn <- reactive({
                readDataIn(values$fileIn,sheetIn=input$sheetIn,header=input$headerCheck)
        })
        
        nRCol <- reactive({ncol(datIn())})
        nRrow <- reactive({nrow(datIn())})
        selectedMV <- reactive({which(params$majorVersionDescription==input$selectMajorVersion)})
#======================== UI elements =====================================================================        
        #preview table
        output$preview_datIn <- renderDataTable({
                if(is.null(runStage$runRead)){return()} # check if
                if((input$rubricType=="Binary")&(length(input$scoreColumn)<2)){return()}
                cn <- colnames(datIn())
                if(input$rubricType=="Typed"){
                        scoreCols <- input$scoreColumn
                }else{
                        scoreCols <- seq(input$scoreColumn[1],input$scoreColumn[2])
                }
                datatable(head(datIn()),options = list(dom = 't'),rownames=FALSE) %>%
                        formatStyle(cn[as.numeric(input$responseColumn)],backgroundColor="#99FF99") %>%
                        formatStyle(cn[scoreCols],backgroundColor="#FF944D")
        })
        output$rowCount <- renderUI({
                if(is.null(runStage$runRead)){return()} # check if
                helpText(paste(nRrow(),"Rows Read"))
        })
        #dynamic UI elements
        output$chooseWorksheet <- renderUI({
                if(is.null(params$fileDimensions)){return()}
                nWorksheets <- params$fileDimensions[1]
                list(
                        if(nWorksheets > 1){selectInput("sheetIn","Select Worksheet",as.list(seq(nWorksheets)))},
                        checkboxInput("headerCheck","First Row is Header",value=TRUE)
                )
        })
        
        output$choose_response_col <- renderUI({
                if(is.null(input$headerCheck)){return()} # check if
                selectInput("responseColumn","Student Response Column",as.list(seq(nRCol())),selected=guessResponseCol(datIn()))
        })
        
        output$choose_score_cols <- renderUI({
                if(is.null(input$headerCheck)){return()} # check if
                if (input$rubricType=="Typed"){
                        sliderDelta <- 0
                }else{
                        sliderDelta <- c(guessResponseCol(datIn())-nRCol()+1,0)
                }
                sliderInput("scoreColumn","Response Score Columns",min=1,max=nRCol(),value=nRCol()+sliderDelta,step=1)
        })
        output$report_button <- renderUI({
                if(is.null(runStage$runRead)){return()} # check if
                        actionButton("goButton",label="Run Analysis")
                
        })
        
#-------------------- Question status sequence -----------------------------------------------
        output$questionStatusQuerry1 <- renderUI({
                if(is.null(runStage$runRead)){return()} # check if
                radioButtons("newQuestionQuerry",
                             label = "Is this a new item?",
                             choices=list("Yes"=1,"No"=2),
                             selected=1)
        })
        output$questionStatusQuerry2 <- renderUI({
                if(is.null(input$newQuestionQuerry)){return()} # check if
                if(input$newQuestionQuerry=="2"){
                        list(
                                selectInput("question","Select Question",params$questionList,selected=params$questionList[[1]]),
                                radioButtons("newMajorQuerry",
                                             label = "Does this item have a new prompt or rubric?",
                                             choices=list("Yes"=1,"No"=2),
                                             selected=1)
                        )
                }
        })
        output$questionStatusQuerry3 <- renderUI({
                if(is.null(input$newMajorQuerry)){return()} # check if
                if(input$newMajorQuerry=="2"){
                        selectInput("selectMajorVersion","Select Item Version",params$majorVersionDescription,selected = params$majorVersionDescription[[1]])
                        
                }
        })
        
        output$select_cvLevel <- renderUI({
                if(is.null(values$cvOut) | (input$rubricType=="Typed")){return()} # check if
                cvMenuChoice <- seq_along(values$rubricLevelNames)
                names(cvMenuChoice) <- values$rubricLevelNames
                selectInput("cvLevel","Rubric Level",as.list(cvMenuChoice))
        })
        output$cvSubtitle <- renderUI({
                if(is.null(values$trainLength)){return()}
                h4(paste("Training Set N=",values$scoreLength))
        })
        output$originalText <- renderUI({
                if(is.null(runStage$runRead)){return()} # check if
                as.character(datIn()[1,as.numeric(input$responseColumn)])
                
        })
        output$preProText <- renderUI({
                if(is.null(runStage$runRead)){return()} # check if
                tmpText <- preprocessData(datIn()[1,],as.numeric(input$responseColumn),
                                          input$stemming,input$numberFormat,preserveSymbol=input$preserveSymbol)
                
        })
        #Hands on table handling
        output$rubricLevelTable = renderRHandsontable({
                if (!is.null(input$rubricLevelTable)) {
                        values$DF <- hot_to_r(input$rubricLevelTable)
                }
                setHot(values$DF)
                if(input$rubricType=="Typed"){
                        rhandsontable(values$DF,rowHeaders = FALSE) %>%
                                hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
                                hot_col("Code", readOnly = TRUE) %>%
                                hot_cols(type="autocomplete")        
                }else{
                        rhandsontable(values$DF,rowHeaders = FALSE) %>%
                                hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
                                hot_cols(type="autocomplete")
                }
                
                
        })
        output$choosePositiveCode <- renderUI({
                if (input$rubricType=="Typed"){return()}
                if(sum(params$codeList %in% 0:1)!=2){
                        selectInput("positiveCode","Select Positive Code",params$codeList,selected=params$codeList[[2]])
                }else{return()}
        })
        output$warningMessages <- renderUI({
                if(is.null(values$warningMessages)){return()}
                paste("Warning:",values$warningMessages)
        })
        #cross validation figures
        # output$cvDataOut <- renderPrint({
        #                         if(is.null(input$cvLevel) & is.null(values$cvOut)){
        #                                 return()
        #                         }else if(is.null(input$cvLevel) & !is.null(values$cvOut)){
        #                                 values$cvOut[[1]]
        #                         }else{
        #                                 values$cvOut[[as.numeric(input$cvLevel)]]
        #                         }
        # })
        output$cvDataOut <- renderPrint({
                if(is.null(input$cvLevel) & is.null(values$cvOut)){
                        return()
                }else if(is.null(input$cvLevel) & !is.null(values$cvOut)){
                        tmpCVdat <-values$cvOut[[1]]
                        
                        # print(values$cvOut[[1]])
                }else{
                        tmpCVdat <- values$cvOut[[as.numeric(input$cvLevel)]]
                        # values$cvOut[[as.numeric(input$cvLevel)]]
                }
                class(tmpCVdat) <- "confusionMatrix2"
                print(tmpCVdat)
        })
        
        output$modelListTable <- renderTable({
                if(is.null(input$cvLevel) & is.null(values$cvOut)){
                        return()
                }else if(is.null(input$cvLevel) & !is.null(values$cvOut)){
                        values$modNameDFList[[1]]
                }else{
                        values$modNameDFList[[as.numeric(input$cvLevel)]]
                }
        })
        output$agreementAll <- renderPlot({
                if(is.null(values$cvOut)){return()} # check if
                agreementPlot(values$histDF)
        })
        output$agreementByType <- renderPlot({
                if(is.null(values$cvOut)){return()} # check if
                agreementPlotLevels(values$histDF,input$rubricType)
        })
        output$expectAgreement <- renderTable({
                if(is.null(values$cvOut)){return()} # check if
                agreementTable(values$histDF)
        },digits=0)
#--------------------------- Downloaders ------------------------------------------------------------
        output$downloadPDF <- downloadHandler(
                filename = function(){
                        paste0(strsplit(values$fileIn$name,"[.]")[[1]][1],".pdf")
                },
                content = function(file){
                        out <- makeReportPDF(file,input,params,values)
                        file.copy(out,file)
                }
        )
        # output$downloadExcel <- downloadHandler(
        #         filename = function(){
        #                 gsub("\\.xls","Scored\\.xls",input$fileIn$name)
        #         },
        #         content = function(file){
        #                 wkbk <- loadWorkbook(input$fileIn$datapath)               #load Excel file
        #                 sheetNames <- getSheets(wkbk)
        #                 nSheets <- length(sheetNames)         #count number of sheets
        #                 scoreOut <- as.data.frame(values$predict)     #make prediction & probability data frame
        #                 
        #                 if(input$rubricType=="Typed"){        #rename prediction& probability data frame
        #                         names(scoreOut) <- c("AACRScore","Probability")
        #                 }else{
        #                         tmpNames <- paste(values$rubricLevelNames,"Score")
        #                         tmpNames <- as.vector(rbind(tmpNames,rep("Probability",length(tmpNames))))
        #                         names(scoreOut) <- tmpNames
        #                 }
        #                 writeWorksheet(wkbk,data=scoreOut,sheet=sheetNames[1],startCol=(nRCol()+1))
        #                 saveWorkbook(wkbk)
        #                 file.copy(input$fileIn$datapath,file)
        #         }
        # )
        output$downloadExcel <- downloadHandler(
                filename = function(){
                        gsub("\\.xls","Scored\\.xls",values$fileIn$name)
                },
                content = function(file){
                        wkbk <- openxlsx::loadWorkbook(values$fileIn$datapath)               #load Excel file
                        # sheetNames <- getSheetNames(wkbk)
                        sheetNames <- wkbk$sheet_names
                        nSheets <- length(sheetNames)         #count number of sheets
                        scoreOut <- as.data.frame(values$predict)     #make prediction & probability data frame
                        
                        if(input$rubricType=="Typed"){        #rename prediction& probability data frame
                                names(scoreOut) <- c("AACRScore","Probability")
                        }else{
                                # tmpNames <- paste(params$groupFreq[,params$groupType],"Score")
                                tmpNames <- paste(values$rubricLevelNames,"Score")
                                tmpNames <- as.vector(rbind(tmpNames,rep("Probability",length(tmpNames))))
                                names(scoreOut) <- tmpNames
                        }
                        writeData(wb=wkbk,sheet=sheetNames[1],x=scoreOut,startCol=(nRCol()+1))
                        openxlsx::saveWorkbook(wb=wkbk,file=values$fileIn$datapath,overwrite=TRUE)
                        file.copy(values$fileIn$datapath,file)
                }
        )
        output$downloadCSV <- downloadHandler(
                filename = function(){
                        gsub("\\.(?i)(csv)","Scored\\.csv",values$fileIn$name)
                },
                content = function(file){
                        scoreOut <- as.data.frame(values$predict)     #make prediction & probability data frame
                        
                        if(input$rubricType=="Typed"){        #rename prediction& probability data frame
                                names(scoreOut) <- c("AACRScore","Probability")
                        }else{
                                tmpNames <- paste(values$rubricLevelNames,"Score")
                                tmpNames <- as.vector(rbind(tmpNames,rep("Probability",length(tmpNames))))
                                names(scoreOut) <- tmpNames
                        }
                        tmpOut <- cbind(datIn(),scoreOut)
                        write.csv(tmpOut,file=file,row.names = FALSE)
                }
        )
        output$downloadData <- renderUI({
                if(tolower(file_ext(values$fileIn$name))=="csv"){
                        downloadButton("downloadCSV","Download CSV File")
                }else{
                        downloadButton("downloadExcel","Download Excel File")
                }
        })
#------------------------ metadata panels ----------------------------------------------------------------
        output$metadataInputPanel <- renderUI({
                if(is.null(input$newQuestionQuerry)){return()}
                if(input$newQuestionQuerry=="1"){
                        # new item
                        list(
                                textInput("questionName",
                                          label = "Name of Question"),
                                textInput("questionText", label = "Input Question Text",width="100%"),
                                h4("Input Rubric Level Names and Descriptions"),
                                rHandsontableOutput("rubricLevelTable"),
                                textInput("majorVersionDescription",label = "Brief description of Prompt and Rubric"),
                                textInput("minorVersionDescription",label = "Brief description of Scoring Model"),
                                uiOutput("choosePositiveCode"),
                                as.character(paste("Version:",params$version)),
                                uiOutput("warningMessages")
                        )
                }else{
                        if(is.null(input$newMajorQuerry)){return()}
                        if(input$newMajorQuerry=="1"){
                                # existing question, new major version
                                list(
                                        h4("Name of Question"),
                                        p(as.character(input$question)),
                                        textInput("questionText", label = "Input Question Text",width="100%",value=params$questionText[[1]]),
                                        h4("Input Rubric Level Names and Descriptions"),
                                        rHandsontableOutput("rubricLevelTable"),
                                        textInput("majorVersionDescription",label = "Brief description of Prompt and Rubric",placeholder="Ex. 2016 prompt with  2 level analytic rubric"),
                                        textInput("minorVersionDescription",label = "Brief description of Scoring Model",placeholder="Ex. 2014-15 data set from MSU, UGA, and UCB"),
                                        uiOutput("choosePositiveCode"),
                                        as.character(paste("Version:",params$version)),
                                        uiOutput("warningMessages")
                                )
                        }else{
                                # existing question & major version, new minor version
                                list(
                                        h4("Name of Question"),
                                        p(as.character(input$question)),
                                        h4("Question Text"),
                                        p(as.character(params$questionText[[selectedMV()]])),
                                        h4("Input Rubric Level Names and Descriptions"),
                                        renderTable(params$questionRubric[[selectedMV()]]),
                                        h4("Brief description of Prompt and Rubric"),
                                        p(params$majorVersionDescription[[selectedMV()]]),
                                        textInput("minorVersionDescription",label = "Brief description of Scoring Model",placeholder="Ex. 2014-15 data set from MSU, UGA, and UCB"),
                                        uiOutput("choosePositiveCode"),
                                        as.character(paste("Version:",params$version)),
                                        uiOutput("warningMessages")
                                )
                        }
                }
        })
        
})
