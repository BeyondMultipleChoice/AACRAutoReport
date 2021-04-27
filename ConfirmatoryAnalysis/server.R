#AACR confirmatory analysis app server
library(shiny)
library(rhandsontable)
library(DT)

source(file.path("functions","supportFunctions.R")) #load support functions
source(file.path("functions","reportFunctions.R")) #load report functions

shinyServer(function(input,output){
        #initialize reactive values objects
        runStage <- reactiveValues()
        values <- reactiveValues()
        valuesHS <- reactiveValues()
        params <- reactiveValues()
        paramsHS <- reactiveValues()
        setHot <- function(x){ values[["hot"]] <- x}
        
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
        observe({
                if(is.null(input$chooseStopwords)){return()}
                if(input$chooseStopwords=="CUSTOM"){
                        tmp <- tolower(input$stopwordList)
                        tmp <- gsub("[[:space:]]","",tmp)
                        params$stopwords <- unlist(strsplit(tmp,","))
                }else{
                        params$stopwords <- input$chooseStopwords        
                }
                        
        })
        observeEvent(input$fileIn,{
                values$fileIn <- input$fileIn
                fExt <- tools::file_ext(values$fileIn$name)
                if(fExt %in% c("xlsx","xls")){
                        file.rename(values$fileIn$datapath,paste(values$fileIn$datapath,fExt,sep="."))
                        values$fileIn$datapath <- paste(values$fileIn$datapath,fExt,sep=".")
                }
        })
        datIn <- reactive({
                readDataIn(values$fileIn,sheetIn=input$sheetIn,header=input$headerCheck)
        })
        
        nRCol <- reactive({ncol(datIn())})
        nRrow <- reactive({nrow(datIn())})
        
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
                if(is.null(runStage$preRead)){return()} # check if
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
        output$customStopwords <- renderUI({
                if(input$chooseStopwords!="CUSTOM"){return()}
                textInput("stopwordList",label="Custom Stopwords",value=paste(c("the","a","in","and"),collapse=", "))
        })
        output$preProText <- renderUI({
                if(is.null(runStage$runRead)){return()} # check if
                tmpText <- preprocessData(datIn()[1,],as.numeric(input$responseColumn),
                                          input$stemming,input$numberFormat,
                                          preserveSymbol=input$preserveSymbol,
                                          removeStopwords=params$stopwords)
                
        })
        output$warningMessages <- renderUI({
                if(is.null(values$warningMessages)){return()}
                paste("Warning:",values$warningMessages)
        })
        #text and predictions
        observeEvent(input$goButton,{
                withProgress(message="Calculating", value=0,{
                        incProgress(0.25,detail="Cross Validation")
                        progress <- shiny::Progress$new()
                        progress$set(message = "Rubric Bin", value = 0)
                        on.exit(progress$close())
                        if(input$rubricType=="Typed"){
                                source(file.path("functions","crossValidation.R"),local=TRUE)
                        }else{
                                if(input$ensembleMethod==1){
                                        source(file.path("functions","crossValidation.R"),local=TRUE)
                                }else if(input$ensembleMethod==2){
                                        source(file.path("functions","crossValidationProbWeight.R"),local=TRUE)
                                }else if(input$ensembleMethod==3){
                                        source(file.path("functions","crossValidationPLRensembleL1.R"),local=TRUE)
                                }else if(input$ensembleMethod==4){
                                        source(file.path("functions","crossValidationPLRensemble.R"),local=TRUE)
                                }else if(input$ensembleMethod==5){
                                        source(file.path("functions","crossValidationCVProbWeight.R"),local=TRUE)
                                }else if(input$ensembleMethod==6){
                                        source(file.path("functions","crossValidationCVProbWeightCaret.R"),local=TRUE)
                                }else if(input$ensembleMethod==7){
                                        source(file.path("functions","crossValidationPLRensembleCVP.R"),local=TRUE)
                                }else if(input$ensembleMethod==8){
                                        source(file.path("functions","crossValidationCVProbWeightSMOTE.R"),local=TRUE)
                                }else if(input$ensembleMethod==9){
                                        source(file.path("functions","crossValidationCVProbWeight3gram.R"),local=TRUE)
                                }
                        }
                        incProgress(0.75,detail="Preparing For Language Analysis")
                        source(file.path("functions","makeDocTermMat.R"),local = TRUE)
                        incProgress(0.80,detail="Calculating Human Statistics")
                        source(file.path("functions",paste0("humanReportSetup",input$rubricType,".R")),local=TRUE)
                        incProgress(0.90,detail="Calculating Machine Statistics")
                        source(file.path("functions",paste0("machineReportSetup",input$rubricType,".R")),local=TRUE)
                                
                })
        })
        
        output$cvDataOut <- renderPrint({
                if(is.null(input$cvLevel) & is.null(values$cvOut)){
                        return()
                }else if(is.null(input$cvLevel) & !is.null(values$cvOut)){
                        tmpCVdat <-values$cvOut[[1]]
                }else{
                        tmpCVdat <- values$cvOut[[as.numeric(input$cvLevel)]]
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
        output$downloadPDF <- downloadHandler(
                filename = function(){
                        paste0(strsplit(values$fileIn$name,"[.]")[[1]][1],".pdf")
                },
                content = function(file){
                        out <- makeReportPDF(file,input,params,values)
                        file.copy(out,file)
                }
        )
        output$downloadExcel <- downloadHandler(
                filename = function(){
                        gsub("\\.xls","Scored\\.xls",values$fileIn$name)
                },
                content = function(file){
                        wkbk <- openxlsx::loadWorkbook(values$fileIn$datapath)               #load Excel file
                        sheetNames <- wkbk$sheet_names
                        nSheets <- length(sheetNames)         #count number of sheets
                        scoreOut <- as.data.frame(values$predict)     #make prediction & probability data frame

                        if(input$rubricType=="Typed"){        #rename prediction& probability data frame
                                names(scoreOut) <- c("AACRScore","Probability")
                        }else{
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

#------------------------ Report objects ---------------------------------------------------------------------
        source(file.path("serverObjects","humanReportObjects.R"),local=TRUE)
        source(file.path("serverObjects","machineReportObjects.R"),local=TRUE)
        
        
#------------------ UI handling ------------------------------------------------------------------------------
        output$humanReportPanelSet <- renderUI({
                # if(query$exampleStatus & is.null(params$rubricType)){return()}
                source(file.path("UIPanels",paste0("humanReportPanels",input$rubricType,".R")),local=TRUE)[['value']]
        })
        output$machineReportPanelSet <- renderUI({
                # if(query$exampleStatus & is.null(params$rubricType)){return()}
                source(file.path("UIPanels",paste0("machineReportPanels",input$rubricType,".R")),local=TRUE)[['value']]
        })
})