#setup report values and statistics
library(plyr)
library(RColorBrewer)

# # cvDataFile <- file.path("questionData",input$question,paste0(input$question,"CrossValidation",params$version,".Rdata"))
# cvDataFile <- file.path("questionData",params$question,paste0(params$question,"CrossValidation",params$version,".Rdata"))
# if(!file.exists(cvDataFile)){
#         # cvDataFile <- file.path("questionData",input$question,paste0(input$question,"CrossValidation.Rdata"))
#         cvDataFile <- file.path("questionData",params$question,paste0(params$question,"CrossValidation.Rdata"))
# }
# 
# load(cvDataFile)#load cross validation data
# 
# values$cvOut <- confusionMatrixOut

# values$handScores <-datIn()[,as.numeric(input$scoreColumn)]
# values$hsProbability <- rep(1,length(values$handScores))

# values$scoreLength <- scoreLength
# values$trainLength <- round(0.9*scoreLength) 

# values$predict[[1]]$rubricLevel <- as.numeric(factor(values$predict[[1]]$rubricLevel,levels=params$questionRubric$Code)) #to numeric coding
# values$predict[[1]]$rubricLevel <- factor(values$predict[[1]]$rubricLevel,levels=seq_along(params$questionRubric$Code)) #to numeric factor

# valuesHS$rubricLevelNames <- gsub('[.]'," ",colnames(datInTmp)[scoreColumnVector])
valuesHS$responseIn <-datIn()[,as.numeric(input$responseColumn)]

valuesHS$predict <- list()
tmpList <- list()
tmpList$rubricLevel <- datIn()[,as.numeric(input$scoreColumn)]
tmpList$probability <- rep(1,nRrow())
valuesHS$predict[[1]] <- tmpList
valuesHS$predict[[1]]$rubricLevel <- factor(valuesHS$predict[[1]]$rubricLevel)
# print(str(valuesHS$predict[[1]]))

valuesHS$rubricLevelNames <- levels(valuesHS$predict[[1]]$rubricLevel)
# valuesHS$rubricLevelNames <- sort(unique(valuesHS$predict[[1]]$rubricLevel))


paramsHS$rubricType <- "Typed"
paramsHS$groupType <- "Rubric Level"
paramsHS$groupLevels <- valuesHS$rubricLevelNames
paramsHS$groupCols <- rev(colorPicker(length(valuesHS$rubricLevelNames),"Set1"))
# params$groupCols <- rev(colorPicker(length(params$groupLevels),"Set1"))
paramsHS$maxCategoryN <- 10

print("h1")
#----------------- from page 1 ----------------------------------------------------------------
#group frequency count

# params$groupFreqHS <- plyr::count(values$handScores)
paramsHS$groupFreq <- plyr::count(valuesHS$predict[[1]]$rubricLevel)
# params$groupFreq <- count(values$predict[[1]]$rubricLevel)
names(paramsHS$groupFreq) <- c(paramsHS$groupType,"Documents")
paramsHS$groupFreq[,paramsHS$groupType] <- paramsHS$groupLevels[paramsHS$groupFreq[,paramsHS$groupType]]


print("h2")

#examples by group
groupLvlIDs <- valuesHS$rubricLevelNames

# groupLvlIDs <- params$questionRubric[,"Rubric Level"]
nGroups <- length(groupLvlIDs)

# tmp <- data.frame(datIn()[,as.numeric(input$responseColumn)],
tmp <- data.frame(valuesHS$responseIn,
                                    valuesHS$predict[[1]]$rubricLevel,
                  prob=valuesHS$predict[[1]]$probability,
                  stringsAsFactors = FALSE)      #select relevant variables

tmp <- tmp[order(tmp[,3],decreasing=(paramsHS$groupType=="Rubric Level")),]    #sort by probability/distance
tmp <- split(tmp,tmp[,2])                      #split by group ID
maxGroup <- max(unlist(lapply(tmp,nrow)))      #find maximum group membership
paramsHS$responseByGroup <- as.data.frame(array("",c(maxGroup,nGroups*2)),stringsAsFactors=FALSE) #initialize response data frame

names(paramsHS$responseByGroup) <- as.vector(rbind(paramsHS$groupLevels,rep("%",nGroups)))       #add correct group names

print("h3")


for(i in 1:length(groupLvlIDs)){               #populate response data frame
        if(nrow(tmp[[i]])>0){
                paramsHS$responseByGroup[1:nrow(tmp[[i]]),2*i-1] <- tmp[[i]][,1]
                paramsHS$responseByGroup[1:nrow(tmp[[i]]),2*i] <- tmp[[i]][,3]        
        }
}
# print("h3.5")
# print(str(paramsHS$responseByGroup))

ptTmp <- paramsHS$responseByGroup[,seq(2,2*nGroups,2)] #write probabilty table for histograms
# print(str(ptTmp))
paramsHS$probTable <- apply(ptTmp,2,as.numeric)
# print(values$predict[[1]]$rubricLevel)
# print(params$groupFreq)
print(str(paramsHS$responseByGroup))

paramsHS$responseByGroup[,seq(2,2*nGroups,2)] <- apply(
        paramsHS$responseByGroup[,seq(2,2*nGroups,2)],2,function(x) strtrim(x,4)) #format Probabilitys

print("h4")

#------------------ n-gram overabundance -------------------------------------------
# paramsHS$oAb <- mostOverabundant(valuesHS$docTermMat,values$handScores,ncol(values$docTermMat))
paramsHS$oAb <- mostOverabundant(valuesHS$docTermMat,valuesHS$predict[[1]]$rubricLevel,ncol(valuesHS$docTermMat))

paramsHS$oAbDF <- as.data.frame(matrix(rbind(paramsHS$oAb[[1]],paramsHS$oAb[[2]]),nrow=nrow(paramsHS$oAb[[1]])),stringsAsFactors=FALSE)

# tmpDF <- cbind(as.data.frame(as.matrix(valuesHS$docTermMat)),data.frame(SCOREOUT=values$handScores),stringsAsFactors=FALSE)
tmpDF <- cbind(as.data.frame(as.matrix(valuesHS$docTermMat)),data.frame(SCOREOUT=valuesHS$predict[[1]]$rubricLevel),stringsAsFactors=FALSE)
freqTable <- aggregate(.~SCOREOUT,data=tmpDF,sum)
freqTable <- freqTable[,2:ncol(freqTable)]  #remove label column

print("h5")

print(str(valuesHS$docTermMat))

# oAb2 <- overAbundance(values$docTermMat,values$handScores)
oAb2 <- overAbundance(valuesHS$docTermMat,valuesHS$predict[[1]]$rubricLevel)

# print(str(oAb2))
# print(str(freqTable))

tmpCNames1 <- colnames(oAb2)
tmpCNames2 <- colnames(freqTable)

print(length(tmpCNames1))
print(length(tmpCNames2))

print(tmpCNames1[duplicated(tmpCNames1)])

print(sum(!(tmpCNames1 %in% tmpCNames2)))

print(tmpCNames1[!(tmpCNames1 %in% tmpCNames2)])

importanceTable <- rbind(oAb2,freqTable)
importanceTable <- data.frame(Term=colnames(oAb2),t(importanceTable))
importanceTable <- importanceTable[complete.cases(importanceTable),]
paramsHS$importanceTable <- importanceTable
print("h5.5")
# print(str(params$importanceTable))

print(str(paramsHS$groupLevels))
print(str(rep("OA",length(paramsHS$groupLevels))))
names(paramsHS$oAbDF) <- c(rbind(paramsHS$groupLevels,rep("OA",length(paramsHS$groupLevels))))

print("h6")

#------------------------- web diagram list --------------------------------------------------

wdMenuNames <- paramsHS$groupFreq[,paramsHS$groupType]
wdMenuList <- as.list(seq(nGroups)[groupLvlIDs %in% wdMenuNames])
names(wdMenuList) <- wdMenuNames
paramsHS$wdMenuList <-wdMenuList
# print(params$wdMenuList)
#------------------------- n-gram correlation map limits -------------------------------------
probThreshold <- 0
corThreshold <- 0.3
minNumTerms <- 5
startNumTerms <- 15
maxNumTerms <- 50

print("h7")

tmpMapControlLimits <- list()
for(gLevel in paramsHS$wdMenuList){
        # rubricLevelIndex <- values$handScores==gLevel &
        #         values$hsProbability>probThreshold
        rubricLevelIndex <- valuesHS$predict[[1]]$rubricLevel==gLevel &
                valuesHS$predict[[1]]$probability>probThreshold
        dtmTmp <- values$docTermMat[rubricLevelIndex,]
        minMinFreq <- findMinFreq(dtmTmp,maxNumTerms)
        startMinFreq <- findMinFreq(dtmTmp,startNumTerms)
        maxMinFreq <- findMinFreq(dtmTmp,minNumTerms)
        dtmTmp2 <- dtmTmp[,findFreqTerms(dtmTmp,startMinFreq)]
        startCorThreshold <- findCorThreshold(dtmTmp2,targetRate = 3.5)
        
        tmpMapControlLimits[[gLevel]] <- c(minMinFreq,startMinFreq,maxMinFreq,startCorThreshold)        
}
paramsHS$mapControlLimits <- tmpMapControlLimits
print("h8")

