#functions for AACR report gerneration shiny server
library(rmarkdown)
library(igraph)
library(stringr)
library(stringi)
library(ggplot2)
library(plotly)
library(visNetwork)
library(grDevices)

#set definitions
osName <- Sys.info()['sysname']

#generate HTML report
makeReportHTML <- function(fileName,input,params,values){
        htmlFile <- file.path("reporting",paste0(params$rubricType,"Rmd"),paste0(params$rubricType,"HTML.Rmd"))
        rmarkdown::render(htmlFile,output_file=paste0(stri_rand_strings(1,20),".html"),
                          output_dir="/tmp",intermediates_dir = "/tmp",envir=parent.frame())
}
makeIndexFile <- function(fileName,input,values){
        rmarkdown::render("reporting/index.Rmd",output_file=paste0(stri_rand_strings(1,20),".html"),
                          output_dir="/tmp",intermediates_dir = "/tmp",envir=parent.frame())
}
#generate PDF report
makeReportPDF <- function(fileName,input,params,values){
        pdfFile <- file.path("reporting",paste0(params$rubricType,"Rmd"),paste0(params$rubricType,"PDF.Rmd"))
        out <- rmarkdown::render(pdfFile,output_file=paste0(stri_rand_strings(1,20),".pdf"),
                                 output_dir="/tmp",intermediates_dir = "/tmp")
}

#generate web diagram
radian.rescale <- function(x, start=0, direction=1) {              #shift positions for labels
        c.rotate <- function(x) (x + start) %% (2 * pi) * direction
        c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}


makeCooccurWD <- function(params,values,inset=0){
        edgeScaleFactor <- 5      #scaling parameter for edges
        nodeScaleFactor <- 5
        
        tmp <- as.data.frame(values$predict)
        tmp <- tmp[,seq(1,ncol(tmp),2)]

        adjMat1 <- (t(tmp) %*% as.matrix(tmp)) #construct adjacency matrix
        
        amTmp <- adjMat1
        diag(amTmp) <- 0
        edgeCutThreshold <- findBinSize(max(amTmp)/nrow(tmp),10) #Threshold cut for discrete edge sizes (as percent of sample)
        edgeUpperBound <- floor(max(amTmp)/nrow(tmp)/edgeCutThreshold)*edgeCutThreshold #maximum edge bin
        nodeCutThreshold <- findBinSize(max(adjMat1)/nrow(tmp),10) #Threshold cut for discrete edge sizes (as percent of sample)
        nodeUpperBound <- floor(max(adjMat1)/nrow(tmp)/nodeCutThreshold)*nodeCutThreshold #maximum edge bin
        
        
        edgeLevels <- seq(edgeCutThreshold,edgeUpperBound-edgeCutThreshold,edgeCutThreshold) #set edge threshold levels
        nodeLevels <- seq(nodeCutThreshold,nodeUpperBound-nodeCutThreshold,nodeCutThreshold) #set node threshold levels
        
        nNodes <- length(nodeLevels)+1
        nEdges <- length(edgeLevels)+1
        
        edgeNames <-c(paste0(">",edgeLevels*100,"%"),paste0(">",100*edgeUpperBound,"%"))
        nodeNames <-c(paste0(">",nodeLevels*100,"%"),paste0(">",100*nodeUpperBound,"%"))
        
        edgeColors <- colorPicker(nEdges,"Set2")
        nodeColors <- colorPicker(nNodes,"Set3")
        
        adjMat1 <- floor(adjMat1/nrow(tmp)/edgeCutThreshold)*edgeScaleFactor #scale edge sizes in adjacency matrix
        edgeThresholdIndex <- adjMat1 > nEdges*edgeScaleFactor
        adjMat1[edgeThresholdIndex] <- nEdges*edgeScaleFactor #clip values above max edge size

        dimnames(adjMat1) <- list(str_wrap(params$groupLevels,15),str_wrap(params$groupLevels,15))        
        
        nodeSizes <- floor(colSums(tmp)/nrow(tmp)/nodeCutThreshold)*nodeScaleFactor              #calculate node sizes
        nodeThresholdIndex <- nodeSizes > nNodes*nodeScaleFactor 
        nodeSizes[nodeThresholdIndex] <- nNodes*nodeScaleFactor #clip values above max node size
        
        g <- graph.adjacency(adjMat1,weighted=TRUE,mode='undirected',diag=FALSE) #make web graph
        V(g)$name[nodeSizes==0] <- " "          #remove nodes of size zero
        frameColor <- rep("darkgrey",length(V(g)))
        frameColor[nodeSizes==0] <- "NA"
        vertColor <- rep("NA",length(V(g)))
        vertColor[nodeSizes/nodeScaleFactor>0] <-nodeColors[nodeSizes/nodeScaleFactor]
        
        lab.locs <- radian.rescale(x=1:length(params$groupLevels), direction=-1, start=0) #position node labels
        par(mar=c(5,4,6,2)+.01)
        plot(g,                                 #display graph  
             layout=layout.circle,
             vertex.size=nodeSizes,
             vertex.color=vertColor,
             vertex.frame.color=frameColor,
             vertex.label.family="sans",
             vertex.label.degree=lab.locs,
             vertex.label.dist=1,
             edge.label.family="sans",
             edge.color=edgeColors[E(g)$weight/edgeScaleFactor],
             edge.width=E(g)$weight,
             rescale=FALSE)
        legend("topleft",legend=edgeNames, col=edgeColors, lwd=(1:nEdges)*edgeScaleFactor,bty="n",y.intersp=1.3,
               title="Co-occurrence",inset=inset)#,inset=-0.07)
        legend("topright",legend=nodeNames, pt.bg=nodeColors, col="darkgrey",pch=21,pt.cex=(1:nNodes)*0.92,bty="n",y.intersp=1.7,
               title="Category",inset=inset)#,inset=-0.07)
        
}
makeWebDiagram <- function(params,values,diagramLevel,minClassProb=0,inset=0){
        #set only presence/absence
        if(params$rubricType=="Typed"){
                tmp2 <- values$docTermMat[values$predict[[1]]$rubricLevel==diagramLevel & values$predict[[1]]$probability >= minClassProb,
                                          params$oAb[[1]][1:params$maxCategoryN,diagramLevel]]
        } else {
                tmp2 <- values$docTermMat[values$predict[[as.numeric(diagramLevel)]]$rubricLevel==1 &
                                                  values$predict[[as.numeric(diagramLevel)]]$probability >= minClassProb,
                                          params$oAb[[as.numeric(diagramLevel)]][[1]][1:params$maxCategoryN,2]]
                
        }
        tmp2$v[tmp2$v > 1] <- rep(1,length(tmp2$v[tmp2$v>1]))
        tmp2 <- as.matrix(tmp2)       
        adjMat <- (t(tmp2) %*% as.matrix(tmp2)) #construct adjacency matrix
        
        amTmp <- adjMat
        diag(amTmp) <- 0
        edgeCutThreshold <- findBinSize(max(amTmp)/nrow(tmp2),10) #Threshold cut for discrete edge sizes (as percent of sample)
        edgeUpperBound <- floor(max(amTmp)/nrow(tmp2)/edgeCutThreshold)*edgeCutThreshold #maximum edge bin
        nodeCutThreshold <- findBinSize(max(adjMat)/nrow(tmp2),10) #Threshold cut for discrete edge sizes (as percent of sample)
        nodeUpperBound <- floor(max(adjMat)/nrow(tmp2)/nodeCutThreshold)*nodeCutThreshold #maximum edge bin
        
        edgeScaleFactor <- 5      #scaling parameter for edges
        nodeScaleFactor <- 5
        
        if(edgeUpperBound < 3*edgeCutThreshold){edgeUpperBound <- 3*edgeCutThreshold}
        if(nodeUpperBound < 3*nodeCutThreshold){nodeUpperBound <- 3*nodeCutThreshold}
        edgeLevels <- seq(edgeCutThreshold,edgeUpperBound-edgeCutThreshold,edgeCutThreshold) #set edge threshold levels
        nodeLevels <- seq(nodeCutThreshold,nodeUpperBound-nodeCutThreshold,nodeCutThreshold) #set node threshold levels

        nNodes <- length(nodeLevels)+1
        nEdges <- length(edgeLevels)+1
        
        edgeNames <-c(paste0(">",edgeLevels*100,"%"),paste0(">",100*edgeUpperBound,"%"))
        nodeNames <-c(paste0(">",nodeLevels*100,"%"),paste0(">",100*nodeUpperBound,"%"))
        edgeColors <- colorPicker(nEdges,"Set2")
        nodeColors <- colorPicker(nNodes,"Set3")
        
        lab.locs <- radian.rescale(x=1:params$maxCategoryN, direction=-1, start=0)
        
        adjMat <- floor(adjMat/nrow(tmp2)/edgeCutThreshold)*edgeScaleFactor #scale adjacency for edge weights matrix
        edgeThresholdIndex <- adjMat > nEdges*edgeScaleFactor
        adjMat[edgeThresholdIndex] <- nEdges*edgeScaleFactor #clip values above max edge size
        
        dimnames(adjMat) <- lapply(dimnames(adjMat), function(x) 
                str_wrap(gsub("[.]"," ",x),15)) #format dimension names
        nodeSizes <- floor(colSums(tmp2)/nrow(tmp2)/nodeCutThreshold)*nodeScaleFactor              #calculate node sizes
        nodeThresholdIndex <- nodeSizes > nNodes*nodeScaleFactor 
        nodeSizes[nodeThresholdIndex] <- nNodes*nodeScaleFactor #clip values above max node size

        g <- graph.adjacency(adjMat,weighted=TRUE,mode='undirected',diag=FALSE) #make web graph
        
        V(g)$name[nodeSizes==0] <- " "          #remove nodes of size zero
        frameColor <- rep("darkgrey",length(V(g)))
        frameColor[nodeSizes==0] <- "NA"
        vertColor <- rep("NA",length(V(g)))
        vertColor[nodeSizes/nodeScaleFactor>0] <-nodeColors[nodeSizes/nodeScaleFactor]
        par(mar=c(5,4,6,2)+.01)
        
        plot(g,                                 #display graph  
             layout=layout.circle,
             vertex.size=nodeSizes,
             vertex.color=vertColor,
             vertex.frame.color=frameColor,
             edge.width=E(g)$weight,
             edge.color=edgeColors[E(g)$weight/edgeScaleFactor],
             vertex.label.family="sans",
             edge.label.family="sans",
             vertex.label.dist=1,
             vertex.label.degree=lab.locs,
             rescale=FALSE
        )
        
        title(  main=params$groupFreq[,params$groupType][diagramLevel])
        legend("topleft",legend=edgeNames, col=edgeColors, lwd=(1:nEdges)*edgeScaleFactor, bty="n",y.intersp=1.3,
                                                        title="Co-occurrence",inset=inset)#inset=-0.07)
        legend("topright",legend=nodeNames, pt.bg=nodeColors, col="darkgrey",pch=21, pt.cex=(1:nNodes)*0.92, bty="n",y.intersp=1.4,
               title="Frequency",inset=inset)#inset=-0.07)
}

# generate binary scoring bar plot

makeBinaryScoringPlot <- function(params,static=FALSE){
        tmpWD <-getwd()
        setwd("/tmp")
        if(!is.null(params$noneOfAbove)){
                # print(str(params$noneOfAbove))
                # print(str(params$groupFreq))
                figData <- rbind(params$groupFreq,params$noneOfAbove)
                figData[,1] <- factor(figData[,1],levels=rev(figData[,1]))
                # print(str(figData))
        }else{
                figData <- params$groupFreq
        }
        if(static){
                # p <- ggplot(params$groupFreq,aes(x=`Rubric Level`,y=Responses,fill=`Rubric Level`))+
                p <- ggplot(figData,aes(x=`Rubric Level`,y=Responses,fill=`Rubric Level`))+
                        geom_bar(stat="identity",show.legend = FALSE)+
                        coord_flip()+
                        scale_fill_manual(values=params$groupCols,guide="none")+
                        xlab(NULL)+
                        theme(text=element_text(family="Times"))
        }else{
                g <- ggplot(figData,aes(x=`Rubric Level`,y=Responses,fill=`Rubric Level`,z=Percentage))+
                        # g <- ggplot(params$groupFreq,aes(x=`Rubric Level`,y=Responses,fill=`Rubric Level`,z=Percentage))+
                        geom_bar(stat="identity",show.legend = FALSE)+
                        coord_flip()+
                        scale_fill_manual(values=rev(params$groupCols),guide="none")+
                        xlab(NULL)
                p <- ggplotly(g,tooltip = c("x","y","z"))
                p <- p %>% layout(showlegend = FALSE)        
        }
        
        setwd(tmpWD)
        p
}

makeTypedScoringPlot <- function(params,static=FALSE){
        tmpDF <- cbind(params$groupFreq,data.frame(dummy=rep("dummy",nrow(params$groupFreq))))
        Percentage <- paste(round(tmpDF$Documents/sum(tmpDF$Documents)*100),"%")
        # cPosition <- sapply(seq_along(Percentage),function(x) sum(tmpDF[1:x,"Documents"])-0.5*tmpDF[x,"Documents"])
        # tmpDF <- cbind(tmpDF,data.frame(Percentage=Percentage,cPosition=cPosition))
        tmpDF <- cbind(tmpDF,data.frame(Percentage=Percentage))
        
        tmpDF[,"Rubric Level"] <- factor(tmpDF[,"Rubric Level"],levels=tmpDF[,"Rubric Level"])
        if(static){
                # p <- ggplot(tmpDF,aes(x=dummy,y=Documents,fill=`Rubric Level`,z=Percentage,label=Percentage))+
                # p <- ggplot(tmpDF,aes(x=dummy,y=Documents,fill=`Rubric Level`,label=Percentage))+
                #         geom_bar(stat = "identity")+
                #         geom_text(aes(y=cPosition),check_overlap = TRUE)+
                #         scale_x_discrete(name="",breaks=NULL,labels=NULL)+
                #         ylab("Student Responses")+
                #         coord_flip()+
                #         scale_fill_manual(values=params$groupCols)
                
                p <- ggplot(tmpDF,aes(x=dummy,y=Documents,fill=`Rubric Level`,label=Percentage,group=`Rubric Level`))+
                        geom_bar(stat = "identity")+
                        geom_text(position=position_stack(vjust = .5),check_overlap = TRUE)+
                        scale_x_discrete(name="",breaks=NULL,labels=NULL)+
                        ylab("Student Responses")+
                        coord_flip()+
                        scale_fill_manual(values=params$groupCols)
                
                        
        }else{
                # g<- ggplot(tmpDF,aes(x=dummy,y=Documents,fill=`Rubric Level`,z=Percentage))+
                #         geom_bar(stat = "identity",show.legend=FALSE)+
                #         scale_x_discrete(name="",breaks=NULL,labels=NULL)+
                #         ylab("Student Responses")+
                #         coord_flip()+
                #         scale_fill_manual(values=params$groupCols)#+
                g<- ggplot(tmpDF,aes(x=dummy,y=Documents,fill=`Rubric Level`,label=Percentage))+
                        geom_bar(stat = "identity",show.legend=FALSE)+
                        #scale_x_discrete(name="",breaks=NULL,labels=NULL)+
                        ylab("Student Responses")+
                        coord_flip()+
                        scale_fill_manual(values=params$groupCols)+
                        theme(axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.title.y = element_blank())
                
                p <- ggplotly(g,tooltip = c("fill","y","label"))
        }
p
}



#construct term correlation map
makeTermMap <- function(tdm,minFreq,corThreshold){
        freq.terms <- findFreqTerms(tdm, lowfreq = as.numeric(minFreq))
        nTerms <- length(freq.terms)
        if(nTerms>0){
                plot(tdm, term = freq.terms, corThreshold = corThreshold, weighting = T,
                     attrs=list(
                             node=list(fontsize="20",shape="box",color="gray70",fixedsize=FALSE),
                             edge=list(color="gray70")))        
        }else{
                plot.new()
                text(0.5,0.5,"No terms above specified thresholds:\n try lowering constraints below",cex=2)
        }
        
}

#cooccurence plot
makeCooccurFreqPlot <- function(values,params,condLevel,static=FALSE){
        tmp <- as.data.frame(values$predict)
        tmp <- tmp[seq(1,ncol(tmp),2)]
        tmp[tmp>1] <- 1
        
        adjMat1 <- (t(tmp) %*% as.matrix(tmp)) #construct adjacency matrix
        cooccurCounts <- adjMat1[condLevel,]
        tmpIndex <- !(seq_along(cooccurCounts) %in% condLevel)
        ccNames <- params$groupLevels[tmpIndex]
        cooccurFreq <- cooccurCounts[tmpIndex]/cooccurCounts[condLevel]
        maxCF <- max(cooccurFreq,na.rm=TRUE)
        Percentage <- paste(round(cooccurFreq*100),"%")
        cPosition <- sapply(cooccurFreq,function(x) ifelse(x > (0.25*maxCF),0.5*x,x+0.1*maxCF))
        ccDF <- data.frame(ccNames,cooccurFreq,Percentage,cPosition)
        colnames(ccDF) <- c("Rubric Level","Co-occurrence Frequency","Percentage","cPosition")
        ccDF <- ccDF[!is.na(ccDF[,2]),]
        
        if(static){
                p2<- ggplot(ccDF,aes(x=`Rubric Level`,y=`Co-occurrence Frequency`,fill=`Rubric Level`,label=Percentage))+
                        geom_bar(stat="identity",show.legend=FALSE)+
                        geom_text(aes(y=cPosition),check_overlap = TRUE)+
                        coord_flip()+
                        scale_fill_manual(values=params$groupCols[tmpIndex])+
                        xlab(NULL)+
                        ylab("Co-occurrence Freqency")+
                        ggtitle(paste("Analytic Rubric Categories Co-occurrent with",params$groupLevels[condLevel]))+
                        theme(text=element_text(family="Times"))
                
        }else{
                g2<- ggplot(ccDF,aes(x=`Rubric Level`,y=`Co-occurrence Frequency`,fill=`Rubric Level`,z=`Co-occurrence Frequency`))+
                                        geom_bar(stat="identity",show.legend=FALSE)+
                                        coord_flip()+
                                        scale_fill_manual(values=params$groupCols[tmpIndex])+
                                        xlab(NULL)+
                                        ylab("Co-occurrence Frequency")+
                                        ggtitle(paste("Analytic Rubric Categories Co-occurrent with",params$groupLevels[condLevel]))
                p2 <- ggplotly(g2,tooltip=c("x","z"))
                p2 <- p2 %>% layout(showlegend=FALSE)        
        }
        p2
}

#make conditional frequency plot
makeCondFreqPlot <- function(values,params,condLevel,static=FALSE){
        tmpWD <-getwd()
        setwd("/tmp")
        rlNames <- params$groupFreq[,params$groupType]
        condIndex <- values$predict[[condLevel]]$rubricLevel == 1
        rlIndex <- 1:length(values$predict)
        rlIndex <- rlIndex[rlIndex!=condLevel]
        condFreq <- sapply(rlIndex,function(x) (sum(values$predict[[x]]$rubricLevel[condIndex])/
                                   sum(condIndex))/
                                   (sum(values$predict[[x]]$rubricLevel)/
                                   length(values$predict[[condLevel]]$rubricLevel))
                           )
                        
        cpLabel <- round(condFreq,2)
        maxCP <- max(abs(condFreq-1),na.rm=TRUE)
        cPosition <- sapply(condFreq-1,function(x) ifelse(abs(x) > (0.25*maxCP),0.5*x,NA))
        cPosition[is.na(cPosition)] <- sapply(condFreq[is.na(cPosition)]-1,function(x) ifelse(x>=0,x+0.1*maxCP,x-0.1*maxCP))
        cPosition <- unlist(cPosition)

        condFreq <- data.frame(rlNames[rlIndex],condFreq,condFreq-1,cPosition,cpLabel)
        colnames(condFreq) <- c("Rubric Level","Conditional Probability","cpMinusOne","cPosition","cpLabel")
        condFreq <- condFreq[!is.na(condFreq[,2]),]
        if(static){
                p2<- ggplot(condFreq,aes(x=`Rubric Level`,y=cpMinusOne,fill=`Rubric Level`,label=cpLabel))+
                        geom_bar(stat="identity",show.legend=FALSE)+
                        geom_text(aes(y=cPosition),check_overlap = TRUE)+
                        coord_flip()+
                        scale_fill_manual(values=params$groupCols[rlIndex])+
                        scale_y_continuous(labels=function(x)x+1)+
                        xlab(NULL)+
                        ylab("Conditional Probability")+
                        ggtitle(paste("Analytic Rubric Categories Co-occurant with",rlNames[condLevel]))+
                        theme(text=element_text(family="Times"))
        }else{
                p2<- ggplot(condFreq,aes(x=`Rubric Level`,y=cpMinusOne,fill=`Rubric Level`,label=cpLabel))+
                        geom_bar(stat="identity",show.legend=FALSE)+
                        geom_text(aes(y=cPosition),check_overlap = TRUE)+
                        coord_flip()+
                        scale_fill_manual(values=params$groupCols[rlIndex])+
                        scale_y_continuous(labels=function(x)x+1)+
                        xlab(NULL)+
                        ylab("Conditional Probability")+
                        ggtitle(paste("Analytic Rubric Categories Co-occurant with",rlNames[condLevel]))
        }
        
        setwd(tmpWD)
        p2
}

findMinFreq <- function(tdm,terms=5){
        maxMinFreq <- min(nrow(tdm),500)
        tmpFreq <- sapply(1:maxMinFreq, function(x) length(findFreqTerms(tdm,lowfreq=x)))
        freqOut <- which.min(abs(tmpFreq-terms))
        freqOut
}


findCorThreshold <- function(tdm,targetRate=3){
        tdm <- as.matrix(tdm)
        tmp <- cor(tdm)
        nNodes <- ncol(tdm)
        tmp <- tmp[upper.tri(tmp)]
        cRates <- seq(0,1,0.05)
        tmpRate <- sapply(cRates, function(x) 2*length(tmp[tmp>x])/nNodes)
        corThresholdOut <- which.min(abs(tmpRate-targetRate))
        cRates[corThresholdOut]
}

applyFilter <- function(params,includeList,excludeList){
        includeIndex <- params$groupLevels %in% names(params$wdMenuList)[as.numeric(includeList)]
        excludeIndex <- params$groupLevels %in% names(params$wdMenuList)[as.numeric(excludeList)]
        
        includeIndex <- seq_along(includeIndex)[includeIndex]*2
        excludeIndex <- seq_along(excludeIndex)[excludeIndex]*2
        
        outTable <- params$responseTable
        for (index in includeIndex){
                outTable <- outTable[outTable[,index]==1,]
        }
        for(index in excludeIndex){
                outTable <- outTable[outTable[,index]==0,]
        }
        outTable 
}

makeHistogramPlot <- function(values,params,histogramLevel){
        probIn <- data.frame(Probability=params$probTable[,histogramLevel])
        g <- ggplot(probIn,aes(Probability))+
                geom_histogram(binwidth=0.05,na.rm=TRUE,fill=params$groupCols[histogramLevel])+
                xlim(0,1)+
                xlab("Score Probability")+
                ylab("Number of Responses")+
                labs("Histogram of Score Probabilites")
        p <- ggplotly(g)
        p
                      
}

makeImportantPlot <- function(params,firstLevel,secondLevel){
        nBins <- (ncol(params$importanceTable)-1)/2
        binNames <- names(params$wdMenuList)
        if(secondLevel==0){
                datIn <- data.frame(Importance=params$importanceTable[,firstLevel+1],
                                    Frequency=params$importanceTable[,firstLevel+nBins+1],
                                    zeros=rep(0,nrow(params$importanceTable)),
                                    Term=params$importanceTable$Term
                )
                
                g <- ggplot(datIn,aes(x=Importance,y=zeros,size=Frequency,z=Term))+
                        geom_jitter(alpha=0.5,
                                    height=0.2,
                                    width=0,
                                    color="blue")+
                        scale_y_continuous(name="",labels="",breaks=c(),limits=c(-.1,.1))+
                        scale_x_continuous(breaks=c(min(datIn$Importance),0,max(datIn$Importance)),
                                           labels=c("Absence \n Important","Neutral","Presence \n Important"),
                                           name=paste("Term Importance: \n",binNames[firstLevel]))+
                        geom_vline(xintercept = 0)+
                        theme_minimal()+
                        theme(legend.position="none")
                p <- ggplotly(g,tooltip=c("z","x","size"))
                p <- p %>% layout(showlegend=FALSE)
                
        }else{
                datIn <- data.frame(`First Term`=params$importanceTable[,firstLevel+1],
                                    `Second Term`=params$importanceTable[,secondLevel+1],
                                    Frequency=params$importanceTable[,firstLevel+nBins+1]+params$importanceTable[,secondLevel+nBins+1],
                                    Term=params$importanceTable$Term
                )
                colnames(datIn) <- c("First Term","Second Term","Frequency","Term")
                
                g <- ggplot(datIn,aes(x=`First Term`,y=`Second Term`,size=Frequency,z=Term))+
                        geom_jitter(alpha=0.5,
                                    color="blue")+
                        scale_x_continuous(breaks=c(min(datIn[,"First Term"]),0,max(datIn[,"First Term"])),
                                           labels=c("Abscence \n Important","Neutral","Presence \n Important"),
                                           name=paste("Term Importance: \n",binNames[firstLevel]))+
                        scale_y_continuous(breaks=c(min(datIn[,"Second Term"]),0,max(datIn[,"Second Term"])),
                                           labels=c("Abscence \n Important","Neutral","Presence \n Important"),
                                           name=paste("Term Importance: \n",binNames[secondLevel]))+
                        geom_hline(yintercept = 0)+
                        geom_vline(xintercept = 0)+
                        theme_minimal()+
                        theme(legend.position="none")
                p <- ggplotly(g,tooltip=c("z","x","y","size"))
                p <- p %>% layout(showlegend=FALSE)
                
        }
}
makeAssocFracPlot <- function(filteredSize,totalSize){
        dfTmp <- data.frame(Responses=c(filteredSize,totalSize-filteredSize),
                            Percentage=paste(round(c(filteredSize,totalSize-filteredSize)/totalSize*100),"%"),
                            Subset=c("Selected Rubric Bins","Others"))
        g <- ggplot(dfTmp,aes(fill=Subset,y=Responses,x=1,z=Percentage))+
                geom_bar(stat="identity",show.legend = FALSE)+
                coord_flip()+
                scale_fill_manual(values=c("grey","red"),guide="none")+
                scale_x_continuous(name="",labels="",breaks=c())+
                xlab(NULL)+
                theme_minimal()
        p <- ggplotly(g,tooltip = c("fill","y","z"))
        # p <- ggplotly(g,tooltip = c("y","z"))
        p <- p %>% layout(showlegend = FALSE)
        
}

makeVisNetworkPlot2 <- function(values,params,width=NULL,arrows=NULL){
        #creates visNetwork plot for data frame of binary input scores
        tmp <- as.data.frame(values$predict)
        scoresIn <- tmp[,seq(1,ncol(tmp),2)]
        
        nBins <- ncol(scoresIn)
        binNames <- params$groupFreq[,params$groupType]
        names(scoresIn) <- binNames
        # binNames <- names(scoresIn)
        
        roundness <- max(c(0.05,0.28-0.02*nBins)) #determine curve of edges
        
        condProb <- data.frame(bin=c(),given=c(),prob=c())
        for(i in 1:nBins){
                condIndex <- scoresIn[,i] == 1
                rlIndex <- seq(ncol(scoresIn))
                rlIndex <- rlIndex[rlIndex!=i]
                condFreq <- sapply(rlIndex,function(x) (sum(scoresIn[condIndex,x])/sum(condIndex)))
                condProb <- rbind(condProb,
                                  data.frame(given=rep(i,length(rlIndex)),
                                             bin=rlIndex,
                                             prob=condFreq)
                )
        }
        
        nodePalette <- colorRampPalette(brewer.pal(9,"Greens")) 
        nodeColors <- nodePalette(101)
        
        edgePalette <- colorRampPalette(brewer.pal(9,"Blues"))
        # edgeColors <- edgePalette(101)
        edgeColors <- edgePalette(21)
        
        sumScores <- colSums(scoresIn)
        meanScores <- round(colMeans(scoresIn)*100)
        
        nodes <- data.frame(id=seq(ncol(scoresIn)),
                            value=sumScores,
                            # label="",
                            label=str_wrap(binNames,15),
                            title=paste("<b>",binNames,"</b><br> Percentage:",meanScores,"%<br> Students:",sumScores),
                            color=nodeColors[meanScores+1]
        )
        
        edges <- data.frame(to=condProb$bin,
                            from=condProb$given,
                            title=paste("<b>",binNames[condProb$given],"&rarr;",binNames[condProb$bin]," </b><br>",round(condProb$prob*100),"%"),
                            color=edgeColors[round(condProb$prob/0.05)+1]#,
        )
        if(!is.null(arrows)){
                edges <- cbind(edges,data.frame(arrows=rep(arrows,nrow(condProb))))
        }
        if(!is.null(width)){
                edges <- cbind(edges,data.frame(width=rep(width,nrow(condProb))))
        }else{
                edges <- cbind(edges,data.frame(value=condProb$prob))
        }
        
        visNetwork(nodes,edges) %>% 
                visOptions(highlightNearest=list(enabled=TRUE,
                                                 degree=0,
                                                 labelOnly=FALSE)) %>%
                visIgraphLayout(layout="layout_in_circle") %>% #,nodeIdSelection=TRUE)
                visInteraction(dragNodes=FALSE,
                               dragView=FALSE,
                               zoomView=FALSE) %>% 
                visEdges(smooth=list(enabled=TRUE,
                                     type="curvedCCW",
                                     roundness=roundness)
                )
        
}

