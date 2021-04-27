#server file for shiny AACR Report Generation App
library(shiny)
library(DT)
library(caret)
library(plotly)
library(visNetwork)

source(file.path("functions","supportFunctions.R")) #load support functions
source(file.path("functions","serverFunctions.R")) #load functions for server
source(file.path("functions","sendEmail.R"))  #MODIF - new function to send e-mails 8-24-2020

if(file.exists(file.path("config","config.R"))){source(file.path("config","config.R"))}

#shiny server
shinyServer(function(input,output,session){
        load(file.path("questionData","questionList.Rdata")) #load question list
        
#====================== initialize reactive values objects ======================================
        
        #initialize reactive values objects
        values <- reactiveValues()
        params <- reactiveValues()
        paramsInitial <- reactiveValues()
        query <- reactiveValues()
        
#===================== Reactive observers ========================================================       
        
        observe({
                query$read <- parseQueryString(session$clientData$url_search) # read query string from URL
                query$exampleStatus <- ifelse(is.null(query$read$exampleReport),FALSE,query$read$exampleReport=="TRUE")
                query$testStatus <- ifelse(is.null(query$read$test),FALSE,query$read$test=="TRUE")
                # print(query$read)
        })
        
        #read in data
        observeEvent(input$exampleButton,{
                values$fileIn <- list(datapath=file.path("questionData","babbledReplication.xlsx"),
                                      name="babbledReplication.xlsx") 
                # input$question <- "Holistic Example"
                # input$responseColumn <- 2
                values$example <- TRUE
        })
        observeEvent(input$exampleAnalyticButton,{
                values$fileIn <- list(datapath=file.path("questionData","analyticExample.xlsx"),
                                      name="analyticExample.xlsx")
                # input$question <- "Analytic Example"
                # input$responseColumn <- 2
                values$example <- TRUE
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
                if(is.null(values$fileIn)){return()} # check if
                params$fileDimensions <- preLoadxlsx(values$fileIn)
        })
        observe({
                values$sheetIn <- input$sheetIn
        })
        observeEvent(query$read$question,{
                params$question <- query$read$question
        })
        observeEvent(input$question,{
                params$question <- input$question
        })
#---------------- observe question status querries and read in data as necessary & set version number -----------------
        ##load major version scoring model data
        observe({
                if(is.null(params$question)){return()}
                questionInDir <- file.path("..","AutoReport","questionData",params$question)
                metaFileIn <- file.path(questionInDir,paste0(params$question,"Metadata.Rdata"))
                #load question metadata
                load(metaFileIn)
                #assign to reactive data structures
                paramsInitial$questionText <- questionText
                paramsInitial$questionRubric <- questionRubric
                if(exists("majorVersionDescription")){
                        paramsInitial$majorVersionDescription <- majorVersionDescription
                        paramsInitial$numMinorVersions <- numMinorVersions
                }else{
                        paramsInitial$majorVersionDescription <- "First Version"
                        paramsInitial$numMinorVersions <- 1
                        paramsInitial$codeList <- codeList
                }
                paramsInitial$groupLevels <- groupLevels
                paramsInitial$rubricType <- rubricType
        })
        ##load minor version scoring model data
        observe({
                if(is.null(paramsInitial$groupLevels)){return()}
                params$majorSMversion <- length(paramsInitial$questionText)-selectedMajV()+1 
                questionInDir <- file.path("..","AutoReport","questionData",params$question)
                minorMetaFileIn <- file.path(questionInDir,paste0(params$question,"MinorMetadata",params$majorSMversion,".Rdata"))
                #load question metadata
                if(file.exists(minorMetaFileIn)){
                        load(minorMetaFileIn)
                        #assign to reactive data structures
                        paramsInitial$codeList <- codeList
                        paramsInitial$minorVersionDescription <- minorVersionDescription
                        paramsInitial$versionDate <- versionDate
                        params$oldMetaFileFormat <- FALSE
                }else{
                        params$oldMetaFileFormat <- TRUE
                        paramsInitial$minorVersionDescription <- "First Version"
                        paramsInitial$versionDate <- ""
                }
                                
        })
        ##set version numbers
        observe({
                if(is.null(paramsInitial$versionDate)){return()}
                if(length(selectedMajV())==0){return()}
                if(is.na(selectedMajV())){return()}
                params$minorSMversion <- paramsInitial$numMinorVersions[[selectedMajV()]]-selectedMinV()
                params$version <- paste(params$majorSMversion,params$minorSMversion,sep=".")
        })
        
#----------------------------- Text processing and predictions --------------------------------------------------
        #primary report generation
        observeEvent(input$goButton,{
                if(!colClassCheck()){return()}
                if(input$idColumn!=0 & is.null(values$example)){
                        updateDataIn(fileIn=values$fileIn,
                                     dataIn=datIn(),
                                     IDcolumn=as.numeric(input$idColumn),
                                     sheetIn=values$sheetIn,
                                     header=input$headerCheck)
                }
                # setup data structures for selected scoring model version
                ## major version indexed data structures
                params$questionText <- paramsInitial$questionText[[selectedMajV()]]
                params$majorVersionDescription <- paramsInitial$majorVersionDescription[[selectedMajV()]]
                params$numMinorVersions <- paramsInitial$numMinorVersions[[selectedMajV()]]
                params$rubricType <- paramsInitial$rubricType[[selectedMajV()]]
                ## minor version indexed data structures
                params$minorVersionDescription <- paramsInitial$minorVersionDescription[[selectedMinV()]]
                params$versionDate <- paramsInitial$versionDate[[selectedMinV()]]
                if(!params$oldMetaFileFormat){
                        params$questionRubric <- paramsInitial$questionRubric[[selectedMajV()]]
                        params$groupLevels <- paramsInitial$groupLevels[[selectedMajV()]]
                        params$codeList <- paramsInitial$codeList[[selectedMinV()]]
                }else{
                        #handle pre-versioning scoring model metadata correctly
                        params$questionRubric <- paramsInitial$questionRubric
                        params$groupLevels <- paramsInitial$groupLevels
                        params$codeList <- paramsInitial$codeList
                }
                
                withProgress(message="Processing Data", value=0,{
                        incProgress(0.25,detail="Generating Predictions")
                        source(file.path("functions","ngramAndPredict.R"),local=TRUE)
                        incProgress(0.75,detail="Calculating Statistics")
                        source(file.path("functions",paste0("reportSetup",params$rubricType,".R")),local=TRUE)
                })

                # write data to database
                if(exists("dbname") & is.null(values$example) & query$testStatus==FALSE){
                        source(file.path("functions","writeToDB.R"),local=TRUE)
                }
        })
        #example report generation
        observeEvent(query$read$exampleReport,{
                if(ifelse(is.null(query$read$exampleReport),FALSE,query$read$exampleReport=="TRUE")& !is.null(query$read$question)){
                        exampleFile <- file.path("questionData",query$read$question,paste0(query$read$question,"Example.Rdata"))
                        load(exampleFile)
                        for(tmpIndex in names(paramsEx)){params[[tmpIndex]] <- paramsEx[[tmpIndex]]}
                        for(tmpIndex in names(valuesEx)){values[[tmpIndex]] <- valuesEx[[tmpIndex]]}
                        params$version <- paste(params$majorSMversion,params$minorSMversion,sep=".")
                        source(file.path("functions",paste0("reportSetup",params$rubricType,".R")),local=TRUE)
                }
        })
        #saved report generation
        observeEvent(query$read$reportID,{
                if(!is.null(query$read$reportID)){
                        saveInFile <- file.path(savedReportPath,paste0(query$read$reportID,".Rdata"))
                        load(saveInFile)
                        for(tmpIndex in names(paramsEx)){params[[tmpIndex]] <- paramsEx[[tmpIndex]]}
                        for(tmpIndex in names(valuesEx)){values[[tmpIndex]] <- valuesEx[[tmpIndex]]}
                        source(file.path("functions",paste0("reportSetup",params$rubricType,".R")),local=TRUE)
                }
        })
#======================== reactive functions ==============================================================       
        
        datIn <- reactive({
                if(is.null(values$sheetIn)){values$sheetIn <- 1}
                readDataIn(values$fileIn,sheetIn=values$sheetIn,header=input$headerCheck)
        })
        
        nRCol <- reactive({ncol(datIn())})
        nRrow <- reactive({nrow(datIn())})
        
        colClassCheck <- reactive({
                if(!is.null(input$responseColumn)){
                        class(datIn()[,as.numeric(input$responseColumn)])=="character"
                        # print(class(datIn()[,as.numeric(input$responseColumn)]))
                }
        })
        emailCheck <- reactive({
                if(!is.null(input$emailString)){
                        tmpEmail <- trimws(unlist(strsplit(input$emailString,",")))[1]
                        isValidEmail(tmpEmail)
                }
        })

        selectedMajV <- reactive({
                ifelse(input$useMostRecent,
                       1,
                       which(paramsInitial$majorVersionDescription==input$selectMajorVersion)
                )
        })
        selectedMinV <- reactive({
                ifelse(input$useMostRecent,
                       1,
                       which(paramsInitial$minorVersionDescription==input$selectMinorVersion)
                )
        })
        
#======================== UI elements =====================================================================        

        output$choose_question <- renderUI({
                if(ifelse(!is.null(query$read$question),query$read$question %in% questionList,FALSE)){return()}
                selectInput("question","Select Question",questionList)#,selected=selected)
        })
        
        #preview table
        output$rowCount <- renderUI({
                if(is.null(input$responseColumn)){return()} # check if
                helpText(paste(nRrow(),"Rows Read"))
        })
        
        output$preview_datIn <- renderDataTable({
                if(is.null(input$responseColumn)){return()} # check if
                datPreview <- head(datIn())
                cn <- colnames(datPreview)
                if(as.numeric(input$idColumn)!=0){
                        datPreview <- hashIDs(datPreview,as.numeric(input$idColumn))
                        datatable(datPreview,options = list(dom = 't'),rownames=FALSE) %>%
                                formatStyle(cn[as.numeric(input$responseColumn)],backgroundColor="#99FF99") %>%
                                formatStyle(cn[as.numeric(input$idColumn)],backgroundColor="#FF944D")        
                }else{
                        datatable(datPreview,options = list(dom = 't'),rownames=FALSE) %>%
                                formatStyle(cn[as.numeric(input$responseColumn)],backgroundColor="#99FF99")
                }
                
        })

        #dynamic UI elements
        output$chooseIDCol <- renderUI({
                if(is.null(input$headerCheck)){return()} # check if

                columnList <- c(list(None=0),as.list(seq(nRCol())))
                initialIDC <- ifelse(guessResponseCol(datIn())==1,columnList[1],columnList[2])
                selectInput("idColumn","Student Identifier Column",columnList,selected=initialIDC)
        })

        output$chooseWorksheet <- renderUI({
                if(is.null(params$fileDimensions)){return()}
                nWorksheets <- params$fileDimensions[1]
                list(
                        if(nWorksheets > 1){selectInput("sheetIn","Select Worksheet",as.list(seq(nWorksheets)))},
                        checkboxInput("headerCheck","First Row is Header",value=TRUE)
                )
        })
        
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #MODIF 8-18-2020: drop-down menus and date field to add optional information into database table question_administrations
        output$instrStatus <- renderUI({
                selectInput("instrStatus", "When was it administered? (optional)", c("N/A", "Pre-Instruction", "Post-Instruction"), selected="N/A")
                
        })
        
        output$adminIncentive <- renderUI({
                selectInput("adminIncentive", "What was the students' incentive? (optional)", 
                            c("N/A", "Low stakes graded", "Completion credit", "High stakes graded", "Extra credit", "No credit"), selected="N/A")
                
        })
        
        output$adminCondition <- renderUI({
                selectInput("adminCondition", "Where was the question administered? (optional)", c("N/A", "In classroom activity", "Online homework", "Exam question"), selected="N/A")
                
        })
        
        output$adminDate  <- renderUI({
                
                
        })
        #~~~~END Of MODIF section 8-18-2020~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        
        output$choose_response_col <- renderUI({
                if(is.null(input$headerCheck)){return()} # check if
                selectInput("responseColumn","Student Response Column",as.list(seq(nRCol())),selected=guessResponseCol(datIn()))
        })
        output$reportAddress <- renderText({
                if(is.null(params$reportIDout)){return()}
                paste0("<b>Report Address:</b>",serverAddress,"/AutoReport/?reportID=",params$reportIDout)
                
        })
                
        output$respColError <- renderUI({
                if(!is.null(input$responseColumn)){
                        if(!colClassCheck()){
                                helpText("Note: The input student response column is not text.",
                                         "Please input a column number that includes the",
                                         "student responses in a text format.")
                        }else if(!emailCheck()){
                                helpText("Please input a valid instructor email address")
                        }else{
                                actionButton("goButton",label="Make Report")
                        }
                }
        })
#---------------------- Versions Querries and Display -------------------------------------------
        output$majorVersionQuerry <- renderUI({
                if(is.null(input$useMostRecent)){return()} # do not display if default value hasn't been set
                if(input$useMostRecent){return()} # do not display if "Use Most Recent" box is checked
                selectInput("selectMajorVersion",
                            "Which Rubric/Prompt should be used?",
                            paramsInitial$majorVersionDescription,
                            selected = paramsInitial$majorVersionDescription[[1]])
                
                
        })
        output$minorVersionQuerry <- renderUI({
                if(is.null(input$useMostRecent)){return()} # do not display if default value hasn't been set
                if(input$useMostRecent){return()} # do not display if "Use Most Recent" box is checked
                selectInput("selectMinorVersion",
                            "Which scoring Model should be used?",
                            paramsInitial$minorVersionDescription,
                            selected = paramsInitial$minorVersionDescription[[1]])
        })
        output$scoringModelMetadataPreview <- renderUI({
                if(is.null(input$useMostRecent)){return()}
                list(
                        h4("Name of Question"),
                        p(as.character(params$question)),
                        h4("Question Text"),
                        p(as.character(paramsInitial$questionText[[selectedMajV()]])),
                        h4("Input Rubric Level Names and Descriptions"),
                        renderTable(paramsInitial$questionRubric[[selectedMajV()]]),
                        h4("Brief description of Prompt and Rubric"),
                        p(paramsInitial$majorVersionDescription[[selectedMajV()]]),
                        h4("Brief description of Scoring Model"),
                        p(paramsInitial$minorVersionDescription[[selectedMinV()]]),
                        as.character(paste("Version:",params$version,"      Date:",paramsInitial$versionDate[[selectedMinV()]]))
                )
                
        })
        
        output$select_cv_level <- renderUI({
                if(params$rubricType=="Typed"){return()} # check if
                selectInput("cvLevel","Select Rubric Level",params$wdMenuList)
        })
        output$exampleButton_ui <- renderUI({
                if(!is.null(input$fileIn)){return()} # check if
                if(ifelse(!is.null(query$read$question),query$read$question %in% questionList,FALSE)){return()}
                list(
                actionButton("exampleButton",label="Use Holistic Example Data"),
                actionButton("exampleAnalyticButton",label="Use Analytic Example Data")
                )
        })
        output$chooseMinFreq <- renderUI({
                if(is.null(values$docTermMat)){return()} # check if
                list(
                        sliderInput("minFreq","Minimum Term Frequency",
                                    min=params$mapControlLimits[[as.numeric(input$mapLevel)]][1],
                                    max=params$mapControlLimits[[as.numeric(input$mapLevel)]][3],
                                    value=params$mapControlLimits[[as.numeric(input$mapLevel)]][2],
                                    step=1),
                        sliderInput("corThreshold", label = "Correlation Threshold", min = 0, max = 1, 
                                    value=params$mapControlLimits[[as.numeric(input$mapLevel)]][4] 
                                    ),        
                        sliderInput("probThreshold", label = "Minimum Classification Probability", min = 0, max = 1, value = 0)
                )
                
        })
        
#--------------------- interactive reports  UI and figures ----------------------------------------------
        output$typedDistributionPlot <- renderPlotly({
                if(is.null(params$groupFreq)){return()} # check if
                pdf(NULL)
                makeTypedScoringPlot(params)
        })

        output$summaryStatement <- renderText({
                paste("For this question, your students fall into",length(params$groupLevels),"rubric bins. Students with similar ideas fall into the same rubric bin. The", length(params$groupLevels), "rubric bins and the percentage of students in each rubric bin are shown in the figure below.",ifelse(params$rubricType=="Typed","","The bins in this rubric are not mutually exclusive. A single student response can be positively scored in multiple rubric bins."))
        })
        output$scoringTypedStatement <- renderText({
                "The following table contains the student responses sorted by rubric bin. The column labeled % contains the scoring probability. Each score produced by the AACR automated system has an associated score probability. These score probabilities are estimates of the systems confidence that a human and the automated system would assign the same score to a given student response. Please note that these are probabilistic estimates; a score probability of 0.90 means that of 100 student responses similar to the considered response a human expert and the machine system are expected to agree on the scoring of 90 of the responses."
        })
        output$scoringBinaryStatement <- renderText({
                "The following table contains the student responses sorted by rubric bin. Note that the rubric bins are not mutually exclusive, so a single response may appear in multiple bins. The column labeled % contains the scoring probability.Each score produced by the AACR automated system has an associated score probability. These score probabilities are estimates of the systems confidence that a human and the automated system would assign the same score to a given student response. Please note that these are probabilistic estimates; a score probability of 0.90 means that of 100 student responses similar to the considered response a human expert and the machine system are expected to agree on the scoring of 90 of the responses."
        })
        output$probStatement <- renderText({
                "The above figure displays a histogram of the scoring probabilities for the selected rubric bin. Each score produced by the AACR automated system has an associated score probability. These score probabilities are estimates of the systems confidence that a human and the automated system would assign the same score to a given student response. Please note that these are probabilistic estimates; a score probability of 0.90 means that of 100 student responses similar to the considered response a human expert and the machine system are expected to agree on the scoring of 90 of the responses."
        })
        output$importantTermsStatement <- renderText({
                "The above figure displays the importance of terms for being a response being scored in the select rubric bin. The metric used to evaluate the importance of a term is the relative frequency of term in the set of responses scored in a bin normalized to that terms relative in the complete set of student responses and centered about zero. Using this metric a term that appears with the same relative frequency in the set of responses that are positively scored for an individual rubric bin as it does complete response set has an importance value of 0. If a term appears with a relative frequency 25% greater in a rubric bin than it does in the complete set, it's importance value is 0.25. If the term occurs 30% less frequently in a rubric bin than the complete set it's importance value is -0.30. For plots with only a single rubric bin the points are dispersed stochastically on the vertical to aid visibility."        
        })
        output$coWDStatement <- renderText({
                "The circular nodes in this plot correspond with the item rubric bins. The edges (lines) connecting the nodes correspond to co-occurence rates bewtween the rubric bins in the scored sample. In this diagram both nodes and edges are scaled in size and color to represent the range of rubric bin and co-occurence frequencies as shown in the figure keys."
                })
        output$coFigStatement <- renderText({
                "The figures in the follow section display the probabilities of co-occurences between rubric bins. Each figure considers all the responses that have been scored for a given rubric bin and displays the proabilities that those bins have also been coded for the other rubric bins, relative to the occurance of those bins in the complete response set. The metric used here is refered to as the conditional probability. A conditional probability of 0.5 indicates that the analytic rubric category responses represented by the labled bar is half as likely to appear in responses which also are scored for the co-occurant analytic rubric category as they do in the full response set. For a value of 1.3 the relevantly score responses occur 30 percent likely to appear with the co-occurant rubric level than is a given response drawn from the full response set."
        })
        output$rlCoStatement <- renderText({
                "The figures in the follow section display the frequencies of co-occurences between rubric bins. Each figure considers all the responses that have been scored for a given rubric bin and displays the frequencies for which those bins have also been coded for the other rubric bins, relative to the occurance of those bins in the complete response set. The metric used here is refered to as the conditional frequency. A conditional frequncy of 0.5 indicates that the rubric level responses repressented by the labled bar occurs half as often in responses which also are scored for the co-occurant rubric level as they do in the full response set. For a value of 1.3 the relevantly score responses occur 30 percent more frequently with the co-occurant analytic rubric category than they do in the full response set."
        })
        output$ngOverStatement <- renderText({
                "The term importance (overabundance) provides a measure of the relative prevalence of a term in a the set of reposes texts in a rubric bin. The metric used to evaluate the importance of a term is the relative frequency of term in the set of responses scored in a bin normalized to that terms relative in the complete set of student responses and centered about zero. Using this metric a term that appears with the same relative frequency in the set of responses that are positively scored for an individual rubric bin as it does complete response set has an importance value of 0. If a term appears with a relative frequency 25% greater in a rubric bin than it does in the complete set, it's importance value is 0.25. If the term occurs 30% less frequently in a rubric bin than the complete set it's importance value is -0.30."
        })
        output$wdStatement <- renderText({
                "The web diagrams shown here display the frequency and co-occurence rates of the most important terms in each rubric bin. The circular node symbols display the frequency of term occurence as a fraction of responses scored in the rubric bin which contain a given term. The lines connecting the nodes indicate the frequency for which the two connected terms appear together as a fraction of responses in the rubric bin. Highly correlated groups or clusters of terms represent common phrases, sentances, and concepts appearing in student responses."
        })
        output$mapStatement <- renderText({
                "The following correlation maps show the most common terms for each rubric bin and their mutual correlations represented as a network graph. Each term is displayed as a node, with highly correlated terms being connected with lines. The thicker the line connecting two terms the stronger they are correlated in the rubric bins response set. Highly correlated groups or clusters of terms represent common phrases, sentances, and concepts appearing in student responses."
        })
        output$binaryDistributionPlot <- renderPlotly({
                if(is.null(params$groupFreq)){return()} # check if
                pdf(NULL)
                makeBinaryScoringPlot(params)
        })
        output$displayRubric <- renderTable({
                if(is.null(params$questionRubric)){return()}
                dispRubTmp <- params$questionRubric[,c("Rubric Level","Description")]
                colnames(dispRubTmp) <- c("Rubric Bin","Description")
                dispRubTmp
        },include.rownames=FALSE)
        output$displayQuestion <- renderUI({
                if(is.null(params$questionText)){return()}
                h4(HTML(params$questionText))
        })
        
        
        #MODIF - 9-22-2020  *** This is the only function you need to update in the original document ***
        #Add a way to show a link to an extended question
        output$rubricURL <- renderText({
                
                if(is.null(params$questionText)){return()}
                
                else{
                        #Only show this link if the question is Photosynthesis - Biomass
                        if(params$question=="Photosynthesis - Biomass"){
                                return(paste('<h5><br><a href=\"photosynthesis_questions.pdf\" target=\"_blank\"><b>Click here to see all versions of this question.</b></a><br></h4>'))
                        }
                }
        })
        
        
        output$chooseHistogram <- renderUI({
                if(is.null(params$oAbDF)){return()}
                selectInput("histogramLevel","Select Rubric Bin",params$wdMenuList)
        })
        output$probHistogram <- renderPlotly({
                if(is.null(params$oAbDF)){return()}
                makeHistogramPlot(values,params,as.numeric(input$histogramLevel))
        })
        output$assocFracPlot <- renderPlotly({
                if(is.null(params$oAbDF)){return()}
                ftTmp <-applyFilter(params,input$includeFilter2,input$excludeFilter2)
                filteredSize <- nrow(ftTmp)
                totalSize <- length(values$responseIn)
                makeAssocFracPlot(filteredSize,totalSize)
        })
        output$exampleResponses <- renderDataTable({
                if(is.null(params$responseByGroup)){return()} # check if
                datatable(params$responseByGroup,rownames=FALSE,options=list(pageLength=3))
        },options=list(pageLength=5))
        output$responseFilters <- renderUI({
                if(is.null(params$responseByGroup)){return()} # check if
                list(
                        checkboxGroupInput("includeFilter","Must Include",params$wdMenuList,inline=TRUE),
                        checkboxGroupInput("excludeFilter","Must Not Include",params$wdMenuList,inline=TRUE)
                )
        })
        output$responseFilters2 <- renderUI({
                if(is.null(params$responseByGroup)){return()} # check if
                list(
                        checkboxGroupInput("includeFilter2","Must Include",params$wdMenuList,inline=TRUE),
                        checkboxGroupInput("excludeFilter2","Must Not Include",params$wdMenuList,inline=TRUE)
                )
        })
        output$filteredResponses <- renderDataTable({
                if(is.null(params$responseByGroup)){return()}
                outTable <-applyFilter(params,input$includeFilter,input$excludeFilter)
                datatable(outTable,rownames = FALSE,options=list(pageLength=5))
        })
        output$chooseCondLev <- renderUI({
                if(is.null(params$oAbDF)){return()}
                selectInput("condLevel","Conditional Level",params$wdMenuList)
        })
        output$chooseCooccurLev <- renderUI({
                if(is.null(params$oAbDF)){return()}
                selectInput("cooccurLevel","Co-occurant with:",params$wdMenuList)
        })
        output$cooccurFreqPlot <- renderPlotly({
                if(is.null(input$cooccurLevel)){return()} # check if
                makeCooccurFreqPlot(values,params,as.numeric(input$cooccurLevel))
        })
        output$condFreqPlot <- renderPlot({
                if(is.null(input$condLevel)){return()} # check if
                makeCondFreqPlot(values,params,as.numeric(input$condLevel))
        })
        output$chooseImportant <- renderUI({
                fluidRow(
                        column(3,
                               selectInput("firstImportantLevel","First Rubric Bin",params$wdMenuList)
                               ),
                        column(3,
                               selectInput("secondImportantLevel","Second Rubric Bin",c("---"=0,params$wdMenuList))
                               )
                )
        })
        output$importantTermsPlot <- renderPlotly({
                if(is.null(input$firstImportantLevel)){return()} # check if
                makeImportantPlot(params,as.numeric(input$firstImportantLevel),as.numeric(input$secondImportantLevel))
        })
        output$overabundanceTable <- renderDataTable({
                if(is.null(params$oAbDF)){return()} # check if
                datatable(params$oAbDF,rownames=FALSE)
        },options=list(pageLength=10,searching=FALSE))

        output$chooseWebDiagram <- renderUI({
                if(is.null(params$oAbDF)){return()}
                selectInput("diagramLevel","Select Diagram",params$wdMenuList)
        })
        output$webDiagramPredConf <- renderUI({
                if(is.null(params$oAbDF)){return()}
                sliderInput("wdProbThreshold", label = "Minimum Classification Probability", min = 0, max = 1, value = 0)
        })
        output$webDiagramPlot <- renderPlot({
                if(is.null(params$oAbDF)){return()} # check if
                makeWebDiagram(params,values,as.integer(input$diagramLevel),input$wdProbThreshold)
        })
        # output$cooccurWDPlot <- renderPlot({
        #         if(is.null(params$oAbDF)){return()} # check if
        #         makeVisNetworkPlot2(values,params)
        #         # makeCooccurWD(params,values)
        # })
        output$cooccurWDPlot <- renderVisNetwork({
                if(is.null(params$oAbDF)){return()} # check if
                makeVisNetworkPlot2(values,params,arrows = "to",width=3)
                # makeCooccurWD(params,values)
        })
        
        
        output$chooseTermMap <- renderUI({
                if(is.null(values$docTermMat)){return()}
                selectInput("mapLevel","Select Rubric Level",params$wdMenuList)
        })
        
        output$termMapPlot <- renderPlot({
                if(is.null(values$docTermMat)){return()}
                if(params$rubricType=="Typed"){
                        rubricLevelIndex <- values$predict[[1]]$rubricLevel==as.numeric(input$mapLevel) &
                                values$predict[[1]]$probability>input$probThreshold
                } else {
                        rubricLevelIndex <- values$predict[[as.numeric(input$mapLevel)]]$rubricLevel==1 &
                                values$predict[[as.numeric(input$mapLevel)]]$rubricLevel>input$probThreshold
                }
                dtmTmp <- values$docTermMat[rubricLevelIndex,]
                makeTermMap(dtmTmp,input$minFreq,input$corThreshold)
        })
        output$cvDisclaimer <- renderUI({
                HTML("<b> Please Note:</b> The following cross validation metric pertain to the training data set used to produce the automated scoring model, and not the student response set scored in this report. As such it is representative of the general performance of the scoring model and not descriptive of the uploaded student responses.")
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
        
#----------------------- set up report downloads ---------------------------------------------------------------
        output$downloadHTML <- downloadHandler(
                filename = function(){
                        paste0(file_path_sans_ext(values$fileIn$name),".html")
                },
                content = function(file){
                        out <- makeReportHTML(file,input,params,values)
                        file.copy(out,file)
                        
                }
        )
        output$downloadIndex <- downloadHandler(
                filename = "index.html",
                content = function(file){
                        out <- makeIndexFile(file,input,values)
                        file.copy(out,file)
                }
        )
        output$downloadPDF <- downloadHandler(
                filename = function(){
                        paste0(ifelse(is.null(values$fileIn$name),params$question,file_path_sans_ext(values$fileIn$name)),".pdf")
                },
                content = function(file){
                        out <- makeReportPDF(file,input,params,values)
                        file.copy(out,file)
                }
        )
        output$downloadExcel <- downloadHandler(
                filename = function(){
                        paste0(ifelse(is.null(values$fileIn$name),params$question,file_path_sans_ext(values$fileIn$name)),"Scored.xlsx")
                },
                content = function(file){
                        scoreOut <- as.data.frame(values$predict)     #make prediction & probability data frame
                        if(params$rubricType=="Typed"){
                                scoreOut$rubricLevel <- params$questionRubric$Code[scoreOut$rubricLevel]
                        }
                        if(ifelse(!is.null(params$codeList),!is.na(params$codeList),FALSE)){
                                scoreOut <- data.frame(apply(scoreOut,2,function(x) params$codeList[x+1]))
                        }
                        if(params$rubricType=="Typed"){        #rename prediction& probability data frame
                                names(scoreOut) <- c("AACRScore","Probability")
                        }else{
                                tmpNames <- paste(unlist(params$questionRubric["Rubric Level"]),"Score")
                                tmpNames <- as.vector(rbind(tmpNames,rep("Probability",length(tmpNames))))
                                names(scoreOut) <- tmpNames
                        }
                        if(input$idColumn != "0"){
                                scoreOut <- cbind(hashIDs(datIn(),as.numeric(input$idColumn))[,as.numeric(input$idColumn)],
                                                  datIn()[,as.numeric(input$responseColumn)],
                                                  scoreOut)
                                colnames(scoreOut)[1:2] <- c("Hashed Identifier","Student Response")
                        }else{
                                scoreOut <- cbind(datIn()[,as.numeric(input$responseColumn)],scoreOut)
                                colnames(scoreOut)[1] <- "Student Response"
                        }
                        
                        write.xlsx(scoreOut,file=file)
                }
        )
        output$downloadCSV <- downloadHandler(
                filename = function(){
                        paste0(ifelse(is.null(values$fileIn$name),params$question,file_path_sans_ext(values$fileIn$name)),"Scored.csv")
                },
                content = function(file){
                        scoreOut <- as.data.frame(values$predict)     #make prediction & probability data frame
                        if(params$rubricType=="Typed"){
                                scoreOut$rubricLevel <- params$questionRubric$Code[scoreOut$rubricLevel]
                        }else if(ifelse(!is.null(params$codeList),!is.na(params$codeList),FALSE)){
                                scoreOut <- data.frame(apply(scoreOut,2,function(x) params$codeList[x+1]))
                        }
                        
                        if(params$rubricType=="Typed"){        #rename prediction& probability data frame
                                names(scoreOut) <- c("AACRScore","Probability")
                        }else{
                                tmpNames <- paste(unlist(params$questionRubric["Rubric Level"]),"Score")
                                tmpNames <- as.vector(rbind(tmpNames,rep("Probability",length(tmpNames))))
                                names(scoreOut) <- tmpNames
                        }
                        
                        if(ifelse(is.null(input$idColumn),FALSE,input$idColumn != "0")){
                                # if(input$idColumn != "0"){
                                        # scoreOut <- cbind(hashIDs(datIn(),as.numeric(input$idColumn))[,as.numeric(input$idColumn)],
                                #                   datIn()[,as.numeric(input$responseColumn)],
                                #                   scoreOut)
                                scoreOut <- cbind(hashIDs(datIn(),as.numeric(input$idColumn))[,as.numeric(input$idColumn)],
                                                  values$responseIn,
                                                  scoreOut)
                                colnames(scoreOut)[1:2] <- c("Hashed Identifier","Student Response")
                        }else{
                                # scoreOut <- cbind(datIn()[,as.numeric(input$responseColumn)],scoreOut)
                                scoreOut <- cbind(values$responseIn,scoreOut)
                                colnames(scoreOut)[1] <- "Student Response"
                        }
                        
                        write.csv(scoreOut,file=file,row.names=FALSE)
                }
        )
        output$downloadData <- renderUI({
                if(ifelse(is.null(values$fileIn$name),TRUE,tolower(file_ext(values$fileIn$name))=="csv")){
                        downloadButton("downloadCSV","Download CSV File")
                }else{
                        downloadButton("downloadExcel","Download Excel File")
                }
        })

#----------------------- Data Upload Panel UI -----------------------------------------------------------------------
        output$topLevelPanelSet <- renderUI({
                if(ifelse(is.null(query$read$exampleReport),FALSE,query$read$exampleReport=="TRUE")){
                        source(file.path("UIPanels","examplePanelSet.R"),local=TRUE)[['value']]
                }else if(!is.null(query$read$reportID)){
                        source(file.path("UIPanels","savedPanelSet.R"),local=TRUE)[['value']]
                }else{
                        source(file.path("UIPanels","primaryPanelSet.R"),local=TRUE)[['value']]
                }
        })
#---------------------------- Report Panels -------------------------------------------------------------------------
        output$reportPanelSet <- renderUI({
                if(query$exampleStatus & is.null(params$rubricType)){return()}
                source(file.path("UIPanels",paste0("reportPanels",params$rubricType,".R")),local=TRUE)[['value']]
        })
})