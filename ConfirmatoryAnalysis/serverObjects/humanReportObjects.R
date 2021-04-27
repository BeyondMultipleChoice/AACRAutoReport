#shiny objects for Confirmatory Analysis machine scoring report

#---------------------- statement objects -------------------------------------
output$scoringTypedStatementHS <- renderText({
        "The following table contains the student responses sorted by rubric bin. The column labeled % contains the scoring probability. Each score produced by the AACR automated system has an associated score probability. These score probabilities are estimates of the systems confidence that a human and the automated system would assign the same score to a given student response. Please note that these are probabilistic estimates; a score probability of 0.90 means that of 100 student responses similar to the considered response a human expert and the machine system are expected to agree on the scoring of 90 of the responses."
})
output$scoringBinaryStatementHS <- renderText({
        "The following table contains the student responses sorted by rubric bin. Note that the rubric bins are not mutually exclusive, so a single response may appear in multiple bins. The column labeled % contains the scoring probability.Each score produced by the AACR automated system has an associated score probability. These score probabilities are estimates of the systems confidence that a human and the automated system would assign the same score to a given student response. Please note that these are probabilistic estimates; a score probability of 0.90 means that of 100 student responses similar to the considered response a human expert and the machine system are expected to agree on the scoring of 90 of the responses."
})
output$probStatementHS <- renderText({
        "The above figure displays a histogram of the scoring probabilities for the selected rubric bin. Each score produced by the AACR automated system has an associated score probability. These score probabilities are estimates of the systems confidence that a human and the automated system would assign the same score to a given student response. Please note that these are probabilistic estimates; a score probability of 0.90 means that of 100 student responses similar to the considered response a human expert and the machine system are expected to agree on the scoring of 90 of the responses."
})
output$importantTermsStatementHS <- renderText({
        "The above figure displays the importance of terms for being a response being scored in the select rubric bin. The metric used to evaluate the importance of a term is the relative frequency of term in the set of responses scored in a bin normalized to that terms relative in the complete set of student responses and centered about zero. Using this metric a term that appears with the same relative frequency in the set of responses that are positively scored for an individual rubric bin as it does complete response set has an importance value of 0. If a term appears with a relative frequency 25% greater in a rubric bin than it does in the complete set, it's importance value is 0.25. If the term occurs 30% less frequently in a rubric bin than the complete set it's importance value is -0.30. For plots with only a single rubric bin the points are dispersed stochastically on the vertical to aid visibility."        
})
output$coWDStatementHS <- renderText({
        "The circular nodes in this plot correspond with the item rubric bins. The edges (lines) connecting the nodes correspond to co-occurence rates bewtween the rubric bins in the scored sample. In this diagram both nodes and edges are scaled in size and color to represent the range of rubric bin and co-occurence frequencies as shown in the figure keys."
})
output$coFigStatementHS <- renderText({
        "The figures in the follow section display the probabilities of co-occurences between rubric bins. Each figure considers all the responses that have been scored for a given rubric bin and displays the proabilities that those bins have also been coded for the other rubric bins, relative to the occurance of those bins in the complete response set. The metric used here is refered to as the conditional probability. A conditional probability of 0.5 indicates that the rubric level responses repressented by the labled bar is half as likely to appear in responses which also are scored for the co-occurant rubric level as they do in the full response set. For a value of 1.3 the relevantly score responses occur 30 percent likely to appear with the co-occurant rubric level than is a given response drawn from the full response set."
})
output$rlCoStatementHS <- renderText({
        "The figures in the follow section display the frequencies of co-occurences between rubric bins. Each figure considers all the responses that have been scored for a given rubric bin and displays the frequencies for which those bins have also been coded for the other rubric bins, relative to the occurance of those bins in the complete response set. The metric used here is refered to as the conditional frequency. A conditional frequncy of 0.5 indicates that the rubric level responses repressented by the labled bar occurs half as often in responses which also are scored for the co-occurant rubric level as they do in the full response set. For a value of 1.3 the relevantly score responses occur 30 percent more frequently with the co-occurant rubric level than they do in the full response set."
})
output$ngOverStatementHS <- renderText({
        "The term importance (overabundance) provides a measure of the relative prevalence of a term in a the set of reposes texts in a rubric bin. The metric used to evaluate the importance of a term is the relative frequency of term in the set of responses scored in a bin normalized to that terms relative in the complete set of student responses and centered about zero. Using this metric a term that appears with the same relative frequency in the set of responses that are positively scored for an individual rubric bin as it does complete response set has an importance value of 0. If a term appears with a relative frequency 25% greater in a rubric bin than it does in the complete set, it's importance value is 0.25. If the term occurs 30% less frequently in a rubric bin than the complete set it's importance value is -0.30."
})
output$wdStatementHS <- renderText({
        "The web diagrams shown here display the frequency and co-occurence rates of the most important terms in each rubric bin. The circular node symbols display the frequency of term occurence as a fraction of responses scored in the rubric bin which contain a given term. The lines connecting the nodes indicate the frequency for which the two connected terms appear together as a fraction of responses in the rubric bin. Highly correlated groups or clusters of terms represent common phrases, sentances, and concepts appearing in student responses."
})
output$mapStatementHS <- renderText({
        "The following correlation maps show the most common terms for each rubric bin and there mutual correlations represented as a network graph. Each term is displayed as a node, with highly correlated terms being connected with lines. The thicker the line connecting two terms the stronger they are correlated in the rubric bins response set. Highly correlated groups or clusters of terms represent common phrases, sentances, and concepts appearing in student responses."
})

#-------------- typed objects ------------------------------------------

output$typedDistributionPlotHS <- renderPlotly({
        if(is.null(paramsHS$groupFreq)){return()} # check if
        pdf(NULL)
        makeTypedScoringPlot(paramsHS)
})
output$exampleResponsesHS <- renderDataTable({
        if(is.null(paramsHS$responseByGroup)){return()} # check if
        datatable(paramsHS$responseByGroup,rownames=FALSE,options=list(pageLength=3))
},options=list(pageLength=5))
output$chooseImportantHS <- renderUI({
        fluidRow(
                column(3,
                       selectInput("firstImportantLevelHS","First Rubric Bin",paramsHS$wdMenuList)
                ),
                column(3,
                       selectInput("secondImportantLevelHS","Second Rubric Bin",c("---"=0,paramsHS$wdMenuList))
                )
        )
})
output$importantTermsPlotHS <- renderPlotly({
        if(is.null(input$firstImportantLevelHS)){return()} # check if
        makeImportantPlot(paramsHS,as.numeric(input$firstImportantLevelHS),as.numeric(input$secondImportantLevelHS))
})
output$overabundanceTableHS <- renderDataTable({
        if(is.null(paramsHS$oAbDF)){return()} # check if
        datatable(paramsHS$oAbDF,rownames=FALSE)
},options=list(pageLength=10,searching=FALSE))
output$chooseWebDiagramHS <- renderUI({
        if(is.null(paramsHS$oAbDF)){return()}
        selectInput("diagramLevelHS","Select Diagram",paramsHS$wdMenuList)
})
output$webDiagramPlotHS <- renderPlot({
        if(is.null(paramsHS$oAbDF)){return()} # check if
        makeWebDiagram(paramsHS,valuesHS,as.integer(input$diagramLevelHS),input$wdProbThresholdHS)
})
output$webDiagramPredConfHS <- renderUI({
        if(is.null(paramsHS$oAbDF)){return()}
        sliderInput("wdProbThresholdHS", label = "Minimum Classification Probability", min = 0, max = 1, value = 0)
})
output$chooseTermMapHS <- renderUI({
        if(is.null(values$docTermMat)){return()}
        selectInput("mapLevelHS","Select Rubric Level",paramsHS$wdMenuList)
})
output$termMapPlotHS <- renderPlot({
        if(is.null(values$docTermMat)){return()}
        if(paramsHS$rubricType=="Typed"){
                # rubricLevelIndexHS <- valuesHS$handScored==as.numeric(input$mapLevelHS) &
                #         valuesHS$hsProbability>input$probThresholdHS
                rubricLevelIndexHS <- valuesHS$predict[[1]]$rubricLevel==as.numeric(input$mapLevelHS) &
                        valuesHS$predict[[1]]$probability>input$probThresholdHS
                print(str(rubricLevelIndexHS))
        } else {
                # rubricLevelIndexHS <- valuesHS$handScored[[as.numeric(input$mapLevelHS)]]==1 &
                #         valuesHS$hsProbability[[as.numeric(input$mapLevelHS)]]>input$probThresholdHS
                rubricLevelIndexHS <- valuesHS$predict[[as.numeric(input$mapLevelHS)]]$rubricLevel==1 &
                        valuesHS$predict[[as.numeric(input$mapLevelHS)]]$rubricLevel>input$probThresholdHS
        }
        dtmTmp <- values$docTermMat[rubricLevelIndexHS,]
        makeTermMap(dtmTmp,input$minFreqHS,input$corThresholdHS)
})
output$chooseMinFreqHS <- renderUI({
        if(is.null(values$docTermMat)){return()} # check if
        list(
                sliderInput("minFreqHS","Minimum Term Frequency",
                            min=paramsHS$mapControlLimits[[as.numeric(input$mapLevelHS)]][1],
                            max=paramsHS$mapControlLimits[[as.numeric(input$mapLevelHS)]][3],
                            value=paramsHS$mapControlLimits[[as.numeric(input$mapLevelHS)]][2],
                            step=1),
                sliderInput("corThresholdHS", label = "Correlation Threshold", min = 0, max = 1, 
                            value=paramsHS$mapControlLimits[[as.numeric(input$mapLevelHS)]][4] 
                ),        
                sliderInput("probThresholdHS", label = "Minimum Classification Probability", min = 0, max = 1, value = 0)
        )
        
})

#--------------- binary objects ------------------------------------
output$binaryDistributionPlotHS <- renderPlotly({
        print("hi bdphs")
        print(str(paramsHS$groupFreq))
        if(is.null(paramsHS$groupFreq)){return()} # check if
        print("there")
        pdf(NULL)
        makeBinaryScoringPlot(paramsHS)
})
output$cooccurWDPlotHS <- renderVisNetwork({
        if(is.null(paramsHS$oAbDF)){return()} # check if
        makeVisNetworkPlot2(valuesHS,paramsHS,arrows = "to",width=3)
        # makeCooccurWD(paramsHS,valuesHS)
})
output$assocFracPlotHS <- renderPlotly({
        if(is.null(paramsHS$oAbDF)){return()}
        ftTmp <-applyFilter(paramsHS,input$includeFilter2HS,input$excludeFilter2HS)
        filteredSize <- nrow(ftTmp)
        totalSize <- length(valuesHS$responseIn)
        makeAssocFracPlot(filteredSize,totalSize)
})
output$responseFiltersHS <- renderUI({
        if(is.null(paramsHS$responseByGroup)){return()} # check if
        list(
                checkboxGroupInput("includeFilterHS","Must Include",paramsHS$wdMenuList,inline=TRUE),
                checkboxGroupInput("excludeFilterHS","Must Not Include",paramsHS$wdMenuList,inline=TRUE)
        )
})
output$responseFilters2HS <- renderUI({
        if(is.null(params$responseByGroup)){return()} # check if
        list(
                checkboxGroupInput("includeFilter2HS","Must Include",paramsHS$wdMenuList,inline=TRUE),
                checkboxGroupInput("excludeFilter2HS","Must Not Include",paramsHS$wdMenuList,inline=TRUE)
        )
})
output$cooccurFreqPlotHS <- renderPlotly({
        if(is.null(input$cooccurLevelHS)){return()} # check if
        makeCooccurFreqPlot(valuesHS,paramsHS,as.numeric(input$cooccurLevelHS))
})
output$chooseCooccurLevHS <- renderUI({
        if(is.null(paramsHS$oAbDF)){return()}
        selectInput("cooccurLevelHS","Co-occurant with:",paramsHS$wdMenuList)
})
output$condFreqPlotHS <- renderPlot({
        if(is.null(input$condLevelHS)){return()} # check if
        makeCondFreqPlot(valuesHS,paramsHS,as.numeric(input$condLevelHS))
})
output$chooseCondLevHS <- renderUI({
        if(is.null(paramsHS$oAbDF)){return()}
        selectInput("condLevelHS","Conditional Level",paramsHS$wdMenuList)
})
output$filteredResponsesHS <- renderDataTable({
        if(is.null(paramsHS$responseByGroup)){return()}
        outTable <-applyFilter(paramsHS,input$includeFilterHS,input$excludeFilterHS)
        datatable(outTable,rownames = FALSE,options=list(pageLength=5))
})
