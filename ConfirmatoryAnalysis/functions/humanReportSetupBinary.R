#setup report values and statistics
library(plyr)
library(RColorBrewer)


#load question model and dictionary
# cvDataFile <- file.path("questionData",input$question,paste0(input$question,"CrossValidation",params$version,".Rdata"))
# cvDataFile <- file.path("questionData",params$question,paste0(params$question,"CrossValidation",params$version,".Rdata"))
# if(!file.exists(cvDataFile)){
#         # cvDataFile <- file.path("questionData",input$question,paste0(input$question,"CrossValidation.Rdata"))
#         cvDataFile <- file.path("questionData",params$question,paste0(params$question,"CrossValidation.Rdata"))
# }
# load(cvDataFile)#load cross validation data
# 
# values$cvOut <- confusionMatrixOut
# 

# paramsHS <- isolate(reactiveValuesToList(params))
# valuesHS <- isolate(reactiveValuesToList(values))

print("h1")
valuesHS$rubricLevelNames <- gsub('[.]'," ",colnames(datInTmp)[scoreColumnVector])
valuesHS$responseIn <-datIn()[,as.numeric(input$responseColumn)]

# print(str(valuesHS$predict))


scoreColVector <- seq(input$scoreColumn[1],input$scoreColumn[2])
checkCoding <- sapply(datIn()[,scoreColVector],function(x) (min(table(x))<2)|(length(table(x))==1)) #filter bins with insufficient count
if(sum(checkCoding)>0){
        scoreColVector <-scoreColVector[!checkCoding] #remove rubric bins with insufficient codings
}

nrTmp <- nRrow()
valuesHS$predict <- list()
for(i in seq_along(scoreColVector)){
        tmpList <- list()
        tmpList$rubricLevel <- datIn()[,scoreColVector[i]]
        tmpList$probability <- rep(1,nrTmp)
        # valuesHS$predict[[i]]$rubricLevel <- datIn()[,scoreColVector[i]]
        # valuesHS$predict[[i]]$probability <- rep(1,nrTmp)
        valuesHS$predict[[i]] <- tmpList
}

# valuesHS$predict$rubricLevel <-datIn()[,seq(input$scoreColumn[1],input$scoreColumn[2])]
# valuesHS$predict$probability <- matrix(1,nrow=nrow(valuesHS$predict$rubricLevel),ncol=ncol(valuesHS$predict$rubricLevel))
print("h2")

paramsHS$rubricType <- "Binary"
paramsHS$groupType <- "Rubric Level"
paramsHS$groupLevels <- valuesHS$rubricLevelNames
# paramsHS$groupCols <- colorPicker(length(paramsHS$groupLevels),"Set1")
paramsHS$groupCols <- rev(colorPicker(length(valuesHS$rubricLevelNames),"Set1"))
paramsHS$groupCols <- c(paramsHS$groupCols, "#909497" ) 
# paramsHS$groupCols <- colorPicker(length(paramsHS$groupLevels)+1,"Set1")
paramsHS$maxCategoryN <- 10
# valuesHS$scoreLength <- scoreLength
# valuesHS$trainLength <- round(0.9*scoreLength) 

print("h3")


# for(i in seq_along(valuesHS$predict)){
#         if(ifelse(!is.null(paramsHS$codeList),!is.na(paramsHS$codeList),FALSE)){
#                 valuesHS$predict[[i]]$rubricLevel <- as.numeric(factor(valuesHS$predict[[i]]$rubricLevel,levels=paramsHS$codeList))-1
#         }else{
#                 valuesHS$predict[[i]]$rubricLevel <- as.numeric(valuesHS$predict[[i]]$rubricLevel)
#         }
# }

#select Group and Confidence variable names

#----------------- from page 1 ----------------------------------------------------------------
#group frequency count
nGroups <- length(valuesHS$predict)
# print(str(valuesHS$predict))
rubricLevelSums <- sapply(1:nGroups,function (x) sum(valuesHS$predict[[x]]$rubricLevel))
# paramsHS$groupFreq <- data.frame(Group=paramsHS$groupLevels,
#                                Responses=rubricLevelSums,
#                                Color=seq_along(paramsHS$groupLevels),
#                                Percentage=paste(round(rubricLevelSums/length(valuesHS$predict[[1]]$rubricLevel)*100,1),"%"))
# print(paramsHS$groupLevels)
# print(rubricLevelSums)

print(valuesHS$rubricLevelNames)
print(str(as.numeric(rubricLevelSums)))
print(str(seq_along(valuesHS$rubricLevelNames)))
print(str(paste(round(rubricLevelSums/length(valuesHS$predict[[1]]$rubricLevel)*100,1),"%")))

paramsHS$groupFreq <- data.frame(Group=valuesHS$rubricLevelNames,
                               Responses=as.numeric(rubricLevelSums),
                               Color=seq_along(valuesHS$rubricLevelNames),
                               Percentage=paste(round(rubricLevelSums/length(valuesHS$predict[[1]]$rubricLevel)*100,1),"%"),
                               stringsAsFactors = FALSE)
names(paramsHS$groupFreq) <- c(paramsHS$groupType,"Responses","Color","Percentage")

print("h4")
#add none of the above summary
# 
noneOfAboveIndex <- rowSums(sapply(valuesHS$predict,function(x) x$rubricLevel))==0
if(sum(noneOfAboveIndex)>0){
        # paramsHS$noneOfAbove <- c("None of the above",
        #                         sum(noneOfAboveIndex),
        #                         nrow(paramsHS$groupFreq)+1,
        #                         paste(round(sum(noneOfAboveIndex)/length(valuesHS$predict[[1]]$rubricLevel)*100,1),"%"))
        paramsHS$noneOfAbove <- data.frame(Group="None of the above",
                                         Responses=sum(noneOfAboveIndex),
                                         Color=nrow(paramsHS$groupFreq)+1,
                                         Percentage=paste(round(sum(noneOfAboveIndex)/length(valuesHS$predict[[1]]$rubricLevel)*100,1),"%"),
                                         stringsAsFactors = FALSE)
}
names(paramsHS$noneOfAbove)[names(paramsHS$noneOfAbove)=="Group"] <- paramsHS$groupType

print("h5")

maxGroupCount <- max(rubricLevelSums)    #find membership of largest group
responseByGroup <- data.frame(row.names=1:maxGroupCount)  #initialize empty array
for(i in 1:nGroups){                        #for each binary group
        # tmp <- data.frame(datIn()[,as.numeric(input$responseColumn)], #make temporary data frame with response
        tmp <- data.frame(valuesHS$responseIn, #make temporary data frame with response
                                            valuesHS$predict[[i]]$rubricLevel,              #predicted rubric level
                          valuesHS$predict[[i]]$probability,             # and probabilty
                          stringsAsFactors = FALSE)      #
        tmp <- tmp[tmp[,2]==1,c(1,3)]      #subset group members
        tmp <- tmp[order(tmp[,2],decreasing=(paramsHS$groupType=="Rubric Level")),] #sort by probabilty
        padding <- rep("",maxGroupCount-rubricLevelSums[i])
        padDF <- data.frame(padding,padding)
        names(padDF) <- names(tmp)
        tmp <- rbind(tmp,padDF) #pad student response vector to maximum possible length
        responseByGroup <- cbind(responseByGroup,tmp)   #append group's responses
}
names(responseByGroup) <- as.vector(rbind(paramsHS$groupLevels,rep("%",nGroups)))       #add correct group names

print("h6")


ptTmp <- responseByGroup[,seq(2,2*nGroups,2)] #write probabilty table for histograms
paramsHS$probTable <- apply(ptTmp,2,as.numeric)

responseByGroup[,seq(2,2*nGroups,2)] <- apply(responseByGroup[,seq(2,2*nGroups,2)],2,function(x) strtrim(x,4)) #format Probabilitys
paramsHS$responseByGroup <- responseByGroup
#----------------------- response table --------------------------------------------
scoreOut <- as.data.frame(valuesHS$predict)     #make prediction & probability data frame
# if(ifelse(!is.null(paramsHS$codeList),!is.na(paramsHS$codeList),FALSE)){
#                 scoreOut <- data.frame(apply(scoreOut,2,function(x) paramsHS$codeList[x+1]))
# }

tmpNames <- paste(unlist(valuesHS$rubricLevelNames),"Score")
# tmpNames <- paste(unlist(paramsHS$questionRubric["Rubric Level"]),"Score")
tmpNames <- as.vector(rbind(tmpNames,rep("%",length(tmpNames))))
# paramsHS$responseTable <- cbind(datIn()[,as.numeric(input$responseColumn)],round(scoreOut,2))
paramsHS$responseTable <- cbind(valuesHS$responseIn,round(scoreOut,2))
names(paramsHS$responseTable) <- c("Student Response",tmpNames)

#------------------ n-gram overabundance -------------------------------------------

paramsHS$oAb <- lapply(1:nGroups,function(x) mostOverabundant(values$docTermMat,
                                                            factor(valuesHS$predict[[x]]$rubricLevel,levels=0:1),
                                                            ncol(values$docTermMat)))
paramsHS$oAbDF <- sapply(seq(nGroups),function(x) data.frame(paramsHS$oAb[[x]][[1]][,1],paramsHS$oAb[[x]][[2]][,1]))
paramsHS$oAbDF <- as.data.frame(matrix(unlist(paramsHS$oAbDF),ncol=nGroups*2),stringsAsFactors=FALSE)

tmpDF <- as.data.frame(as.matrix(values$docTermMat))
freqTable <- lapply(valuesHS$predict,function(x) colSums(tmpDF[x$rubricLevel==1,]))
freqTable <- data.frame(matrix(unlist(freqTable),nrow=nGroups))

oAb2 <- lapply(1:nGroups,function(x) overAbundance(values$docTermMat,
                                                     factor(valuesHS$predict[[x]]$rubricLevel,levels=0:1))[2,])
oAb2 <- as.data.frame(matrix(unlist(oAb2),ncol=nGroups))

importanceTable <- cbind(oAb2,t(freqTable))
importanceTable <- data.frame(Term=Terms(values$docTermMat),importanceTable)
importanceTable <- importanceTable[complete.cases(importanceTable),]
paramsHS$importanceTable <- importanceTable

names(paramsHS$oAbDF) <- c(rbind(paramsHS$groupLevels,rep("OA",length(paramsHS$groupLevels))))

print("h7")
#------------------------- web diagram list --------------------------------------------------

wdMenuNames <- paramsHS$groupFreq[paramsHS$groupFreq$Responses>0,paramsHS$groupType]
wdMenuList <- as.list(seq(nrow(paramsHS$groupFreq))[paramsHS$groupFreq$Responses>0])
names(wdMenuList) <- wdMenuNames
paramsHS$wdMenuList <-wdMenuList
print(wdMenuList)

print("h8")

#------------------------- n-gram correlation map limits -------------------------------------
probThreshold <- 0
corThreshold <- 0.3
minNumTerms <- 5
startNumTerms <- 15
maxNumTerms <- 50

tmpMapControlLimits <- list()
for(gLevel in paramsHS$wdMenuList){
        rubricLevelIndex <- valuesHS$predict[[as.numeric(gLevel)]]$rubricLevel==1 &
                valuesHS$predict[[as.numeric(gLevel)]]$probability>probThreshold
        # print(gLevel)
        # print(table(valuesHS$predict[[as.numeric(gLevel)]]$rubricLevel,useNA = "ifany"))
        # print(table(valuesHS$predict[[as.numeric(gLevel)]]$probability,useNA = "ifany"))
        # print(table(rubricLevelIndex,useNA = "ifany"))
        print("h8.1")
        dtmTmp <- values$docTermMat[rubricLevelIndex,]
        print("h8.2")
        minMinFreq <- findMinFreq(dtmTmp,maxNumTerms)
        startMinFreq <- findMinFreq(dtmTmp,startNumTerms)
        maxMinFreq <- findMinFreq(dtmTmp,minNumTerms)
        print("h8.3")
        #print(startMinFreq)
        #print(table(findFreqTerms(dtmTmp,startMinFreq),useNA="ifany"))
        dtmTmp2 <- dtmTmp[,unique(findFreqTerms(dtmTmp,startMinFreq))]
        startCorThreshold <- findCorThreshold(dtmTmp2,targetRate = 3.5)
        print("h8.4")
        

        tmpMapControlLimits[[gLevel]] <- c(minMinFreq,startMinFreq,maxMinFreq,startCorThreshold)        
        print("h8.5")
}
paramsHS$mapControlLimits <- tmpMapControlLimits
print("h9")

print(str(paramsHS$groupFreq))
