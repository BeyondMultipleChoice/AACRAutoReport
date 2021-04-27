#shiny objects for Confirmatory Analysis machine scoring report

#---------------------- statement objects -------------------------------------
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
        "The figures in the follow section display the probabilities of co-occurences between rubric bins. Each figure considers all the responses that have been scored for a given rubric bin and displays the proabilities that those bins have also been coded for the other rubric bins, relative to the occurance of those bins in the complete response set. The metric used here is refered to as the conditional probability. A conditional probability of 0.5 indicates that the rubric level responses repressented by the labled bar is half as likely to appear in responses which also are scored for the co-occurant rubric level as they do in the full response set. For a value of 1.3 the relevantly score responses occur 30 percent likely to appear with the co-occurant rubric level than is a given response drawn from the full response set."
})
output$rlCoStatement <- renderText({
        "The figures in the follow section display the frequencies of co-occurences between rubric bins. Each figure considers all the responses that have been scored for a given rubric bin and displays the frequencies for which those bins have also been coded for the other rubric bins, relative to the occurance of those bins in the complete response set. The metric used here is refered to as the conditional frequency. A conditional frequncy of 0.5 indicates that the rubric level responses repressented by the labled bar occurs half as often in responses which also are scored for the co-occurant rubric level as they do in the full response set. For a value of 1.3 the relevantly score responses occur 30 percent more frequently with the co-occurant rubric level than they do in the full response set."
})
output$ngOverStatement <- renderText({
        "The term importance (overabundance) provides a measure of the relative prevalence of a term in a the set of reposes texts in a rubric bin. The metric used to evaluate the importance of a term is the relative frequency of term in the set of responses scored in a bin normalized to that terms relative in the complete set of student responses and centered about zero. Using this metric a term that appears with the same relative frequency in the set of responses that are positively scored for an individual rubric bin as it does complete response set has an importance value of 0. If a term appears with a relative frequency 25% greater in a rubric bin than it does in the complete set, it's importance value is 0.25. If the term occurs 30% less frequently in a rubric bin than the complete set it's importance value is -0.30."
})
output$wdStatement <- renderText({
        "The web diagrams shown here display the frequency and co-occurence rates of the most important terms in each rubric bin. The circular node symbols display the frequency of term occurence as a fraction of responses scored in the rubric bin which contain a given term. The lines connecting the nodes indicate the frequency for which the two connected terms appear together as a fraction of responses in the rubric bin. Highly correlated groups or clusters of terms represent common phrases, sentances, and concepts appearing in student responses."
})
output$mapStatement <- renderText({
        "The following correlation maps show the most common terms for each rubric bin and there mutual correlations represented as a network graph. Each term is displayed as a node, with highly correlated terms being connected with lines. The thicker the line connecting two terms the stronger they are correlated in the rubric bins response set. Highly correlated groups or clusters of terms represent common phrases, sentances, and concepts appearing in student responses."
})


#-------------- typed objects ------------------------------------------

output$typedDistributionPlot <- renderPlotly({
        if(is.null(params$groupFreq)){return()} # check if
        pdf(NULL)
        makeTypedScoringPlot(params)
})
output$exampleResponses <- renderDataTable({
        if(is.null(params$responseByGroup)){return()} # check if
        datatable(params$responseByGroup,rownames=FALSE,options=list(pageLength=3))
},options=list(pageLength=5))
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
output$webDiagramPlot <- renderPlot({
        print(str(params$oAbDF))
        
        if(is.null(params$oAbDF)){return()} # check if
        makeWebDiagram(params,values,as.integer(input$diagramLevel),input$wdProbThreshold)
})
output$webDiagramPredConf <- renderUI({
        if(is.null(params$oAbDF)){return()}
        sliderInput("wdProbThreshold", label = "Minimum Classification Probability", min = 0, max = 1, value = 0)
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

#--------------- binary objects ------------------------------------
output$binaryDistributionPlot <- renderPlotly({
        if(is.null(params$groupFreq)){return()} # check if
        pdf(NULL)
        makeBinaryScoringPlot(params)
})
output$chooseHistogram <- renderUI({
        if(is.null(params$oAbDF)){return()}
        selectInput("histogramLevel","Select Rubric Bin",params$wdMenuList)
})
output$probHistogram <- renderPlotly({
        if(is.null(params$oAbDF)){return()}
        makeHistogramPlot(values,params,as.numeric(input$histogramLevel))
})
output$cooccurWDPlot <- renderVisNetwork({
        if(is.null(params$oAbDF)){return()} # check if
        makeVisNetworkPlot2(values,params,arrows = "to",width=3)
        # makeCooccurWD(params,values)
})
output$assocFracPlot <- renderPlotly({
        if(is.null(params$oAbDF)){return()}
        ftTmp <-applyFilter(params,input$includeFilter2,input$excludeFilter2)
        filteredSize <- nrow(ftTmp)
        totalSize <- length(values$responseIn)
        makeAssocFracPlot(filteredSize,totalSize)
})
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
output$cooccurFreqPlot <- renderPlotly({
        if(is.null(input$cooccurLevel)){return()} # check if
        makeCooccurFreqPlot(values,params,as.numeric(input$cooccurLevel))
})
output$chooseCooccurLev <- renderUI({
        if(is.null(params$oAbDF)){return()}
        selectInput("cooccurLevel","Co-occurant with:",params$wdMenuList)
})
output$condFreqPlot <- renderPlot({
        if(is.null(input$condLevel)){return()} # check if
        makeCondFreqPlot(values,params,as.numeric(input$condLevel))
})
output$chooseCondLev <- renderUI({
        if(is.null(params$oAbDF)){return()}
        selectInput("condLevel","Conditional Level",params$wdMenuList)
})
output$filteredResponses <- renderDataTable({
        if(is.null(params$responseByGroup)){return()}
        outTable <-applyFilter(params,input$includeFilter,input$excludeFilter)
        datatable(outTable,rownames = FALSE,options=list(pageLength=5))
})
