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

params$groupLevels <- values$rubricLevelNames
params$groupType <- "Rubric Level"
params$rubricType <- "Binary"
params$groupCols <- colorPicker(length(params$groupLevels),"Set1")
params$groupCols <- c(params$groupCols, "#909497" )
# params$groupCols <- colorPicker(length(params$groupLevels)+1,"Set1")
params$maxCategoryN <- 10
# values$scoreLength <- scoreLength
# values$trainLength <- round(0.9*scoreLength) 

# for(i in seq_along(values$predict)){
#         if(ifelse(!is.null(params$codeList),!is.na(params$codeList),FALSE)){
#                 values$predict[[i]]$rubricLevel <- as.numeric(factor(values$predict[[i]]$rubricLevel,levels=params$codeList))-1
#         }else{
#                 values$predict[[i]]$rubricLevel <- as.numeric(values$predict[[i]]$rubricLevel)
#         }
# }

print("m1")
print(scoreColVector)
print(str(values$predict))

print(str(values$predict[[1]]))

tmpPredict <- isolate(values$predict)
values$predict <- list()
for(i in seq_along(scoreColVector)){
        tmpList <- list()
        tmpList$rubricLevel <- as.numeric(tmpPredict[,2*i-1])
        tmpList$probability <- as.numeric(tmpPredict[,2*i])
        # tmpList$rubricLevel <- as.numeric(values$predict[,2*i-1])
        # tmpList$probability <- as.numeric(values$predict[,2*i])
        #
        # values$predictHS[[i]]$rubricLevel <- datIn()[,scoreColVector[i]]
        # values$predictHS[[i]]$probability <- rep(1,nrTmp)
        values$predict[[i]] <- tmpList
}


#select Group and Confidence variable names
print("m2")

#----------------- from page 1 ----------------------------------------------------------------
#group frequency count
# nGroups <- length(values$predict)
# print(str(values$predict))
rubricLevelSums <- sapply(1:nGroups,function (x) sum(values$predict[[x]]$rubricLevel))
# params$groupFreq <- data.frame(Group=params$groupLevels,
#                                Responses=rubricLevelSums,
#                                Color=seq_along(params$groupLevels),
#                                Percentage=paste(round(rubricLevelSums/length(values$predict[[1]]$rubricLevel)*100,1),"%"))
print(params$groupLevels)
print(rubricLevelSums)
params$groupFreq <- data.frame(Group=params$groupLevels,
                               Responses=as.numeric(rubricLevelSums),
                               Color=seq_along(params$groupLevels),
                               Percentage=paste(round(rubricLevelSums/length(values$predict[[1]]$rubricLevel)*100,1),"%"),
                               stringsAsFactors = FALSE)
names(params$groupFreq) <- c(params$groupType,"Responses","Color","Percentage")

print("m3")

#add none of the above summary
# 
noneOfAboveIndex <- rowSums(sapply(values$predict,function(x) x$rubricLevel))==0
if(sum(noneOfAboveIndex)>0){
        # params$noneOfAbove <- c("None of the above",
                                # sum(noneOfAboveIndex),
                                # nrow(params$groupFreq)+1,
                                # paste(round(sum(noneOfAboveIndex)/length(values$predict[[1]]$rubricLevel)*100,1),"%"))
        params$noneOfAbove <- data.frame(Group="None of the above",
                                         Responses=sum(noneOfAboveIndex),
                                         Color=nrow(params$groupFreq)+1,
                                         Percentage=paste(round(sum(noneOfAboveIndex)/length(values$predict[[1]]$rubricLevel)*100,1),"%"),
                                         stringsAsFactors = FALSE)        
}
names(params$noneOfAbove)[names(params$noneOfAbove)=="Group"] <- params$groupType

print("m4")


maxGroupCount <- max(rubricLevelSums)    #find membership of largest group
responseByGroup <- data.frame(row.names=1:maxGroupCount)  #initialize empty array
for(i in 1:nGroups){                        #for each binary group
        # tmp <- data.frame(datIn()[,as.numeric(input$responseColumn)], #make temporary data frame with response
        tmp <- data.frame(values$responseIn, #make temporary data frame with response
                                            values$predict[[i]]$rubricLevel,              #predicted rubric level
                          values$predict[[i]]$probability,             # and probabilty
                          stringsAsFactors = FALSE)      #
        tmp <- tmp[tmp[,2]==1,c(1,3)]      #subset group members
        tmp <- tmp[order(tmp[,2],decreasing=(params$groupType=="Rubric Level")),] #sort by probabilty
        padding <- rep("",maxGroupCount-rubricLevelSums[i])
        padDF <- data.frame(padding,padding)
        names(padDF) <- names(tmp)
        tmp <- rbind(tmp,padDF) #pad student response vector to maximum possible length
        responseByGroup <- cbind(responseByGroup,tmp)   #append group's responses
}
names(responseByGroup) <- as.vector(rbind(params$groupLevels,rep("%",nGroups)))       #add correct group names

print("m5")


ptTmp <- responseByGroup[,seq(2,2*nGroups,2)] #write probabilty table for histograms
params$probTable <- apply(ptTmp,2,as.numeric)

responseByGroup[,seq(2,2*nGroups,2)] <- apply(responseByGroup[,seq(2,2*nGroups,2)],2,function(x) strtrim(x,4)) #format Probabilitys
params$responseByGroup <- responseByGroup
#----------------------- response table --------------------------------------------
scoreOut <- as.data.frame(values$predict)     #make prediction & probability data frame
# scoreOut <- as.data.frame(values$predict)     #make prediction & probability data frame
if(ifelse(!is.null(params$codeList),!is.na(params$codeList),FALSE)){
                scoreOut <- data.frame(apply(scoreOut,2,function(x) params$codeList[x+1]))
}

print("m6")


# tmpNames <- paste(unlist(params$questionRubric["Rubric Level"]),"Score")
tmpNames <- paste(values$rubricLevelNames,"Score")
tmpNames <- as.vector(rbind(tmpNames,rep("%",length(tmpNames))))
# params$responseTable <- cbind(datIn()[,as.numeric(input$responseColumn)],round(scoreOut,2))
params$responseTable <- cbind(values$responseIn,round(scoreOut,2))
names(params$responseTable) <- c("Student Response",tmpNames)

#------------------ n-gram overabundance -------------------------------------------

params$oAb <- lapply(1:nGroups,function(x) mostOverabundant(values$docTermMat,
                                                            factor(values$predict[[x]]$rubricLevel,levels=0:1),
                                                            ncol(values$docTermMat)))
params$oAbDF <- sapply(seq(nGroups),function(x) data.frame(params$oAb[[x]][[1]][,1],params$oAb[[x]][[2]][,1]))
params$oAbDF <- as.data.frame(matrix(unlist(params$oAbDF),ncol=nGroups*2),stringsAsFactors=FALSE)

print(str(params$oAbDF))
print("m7")


tmpDF <- as.data.frame(as.matrix(values$docTermMat))
freqTable <- lapply(values$predict,function(x) colSums(tmpDF[x$rubricLevel==1,]))
# freqTable <- lapply(values$predict,function(x) colSums(tmpDF[x$rubricLevel==1,]))
freqTable <- data.frame(matrix(unlist(freqTable),nrow=nGroups))

oAb2 <- lapply(1:nGroups,function(x) overAbundance(values$docTermMat,
                                                     factor(values$predict[[x]]$rubricLevel,levels=0:1))[2,])
oAb2 <- as.data.frame(matrix(unlist(oAb2),ncol=nGroups))

importanceTable <- cbind(oAb2,t(freqTable))
importanceTable <- data.frame(Term=Terms(values$docTermMat),importanceTable)
importanceTable <- importanceTable[complete.cases(importanceTable),]
params$importanceTable <- importanceTable

names(params$oAbDF) <- c(rbind(params$groupLevels,rep("OA",length(params$groupLevels))))

print("m8")
#------------------------- web diagram list --------------------------------------------------

wdMenuNames <- params$groupFreq[params$groupFreq$Responses>0,params$groupType]
wdMenuList <- as.list(seq(nrow(params$groupFreq))[params$groupFreq$Responses>0])
names(wdMenuList) <- wdMenuNames
params$wdMenuList <-wdMenuList

#------------------------- n-gram correlation map limits -------------------------------------
probThreshold <- 0
corThreshold <- 0.3
minNumTerms <- 5
startNumTerms <- 15
maxNumTerms <- 50

tmpMapControlLimits <- list()
for(gLevel in params$wdMenuList){
        rubricLevelIndex <- values$predict[[as.numeric(gLevel)]]$rubricLevel==1 &
                values$predict[[as.numeric(gLevel)]]$probability>probThreshold
        # print(gLevel)
        # print(table(values$predict[[as.numeric(gLevel)]]$rubricLevel,useNA = "ifany"))
        # print(table(values$predict[[as.numeric(gLevel)]]$probability,useNA = "ifany"))
        # print(table(rubricLevelIndex,useNA = "ifany"))
        dtmTmp <- values$docTermMat[rubricLevelIndex,]
        minMinFreq <- findMinFreq(dtmTmp,maxNumTerms)
        startMinFreq <- findMinFreq(dtmTmp,startNumTerms)
        maxMinFreq <- findMinFreq(dtmTmp,minNumTerms)
        dtmTmp2 <- dtmTmp[,findFreqTerms(dtmTmp,startMinFreq)]
        startCorThreshold <- findCorThreshold(dtmTmp2,targetRate = 3.5)
        

        tmpMapControlLimits[[gLevel]] <- c(minMinFreq,startMinFreq,maxMinFreq,startCorThreshold)        
}
params$mapControlLimits <- tmpMapControlLimits
print("m9")
