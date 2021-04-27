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

# values$cvOut <- confusionMatrixOut

# values$scoreLength <- scoreLength
# values$trainLength <- round(0.9*scoreLength) 

# values$predict[[1]]$rubricLevel <- as.numeric(factor(values$predict[[1]]$rubricLevel,levels=params$questionRubric$Code)) #to numeric coding
# values$predict[[1]]$rubricLevel <- factor(values$predict[[1]]$rubricLevel,levels=seq_along(params$questionRubric$Code)) #to numeric factor

print("m0")
tmpPredict <- isolate(values$predict)
values$predict <- list()
tmpList <- list()
print("m0.1")
tmpList$rubricLevel <- as.numeric(tmpPredict[,1])
tmpList$probability <- as.numeric(tmpPredict[,2])
values$predict[[1]] <- tmpList
print("m0.2")
values$predict[[1]]$rubricLevel <- factor(values$predict[[1]]$rubricLevel) #to numeric factor
print("m0.3")
values$rubricLevelNames <- levels(values$predict[[1]]$rubricLevel)

print("m1")

params$groupLevels <- values$rubricLevelNames
params$groupType <- "Rubric Level"
params$rubricType <- "Typed"

# params$groupType <- "Rubric Level"
# params$groupCols <- rev(colorPicker(length(values$rubricLevelNames),"Set1"))

params$groupCols <- rev(colorPicker(length(params$groupLevels),"Set1"))

params$maxCategoryN <- 10

#----------------- from page 1 ----------------------------------------------------------------
#group frequency count

params$groupFreq <- plyr::count(values$predict[[1]]$rubricLevel)
# params$groupFreq <- count(values$predict[[1]]$rubricLevel)
names(params$groupFreq) <- c(params$groupType,"Documents")
params$groupFreq[,params$groupType] <- params$groupLevels[params$groupFreq[,params$groupType]]

print("m2")


#examples by group
groupLvlIDs <- values$rubricLevelNames
# groupLvlIDs <- params$questionRubric[,"Rubric Level"]
nGroups <- length(groupLvlIDs)
print(groupLvlIDs)
print(nGroups)
# tmp <- data.frame(datIn()[,as.numeric(input$responseColumn)],
# print(str(values$responseIn))
# print(str(values$predict[[1]]$rubricLevel))
# print(str(values$predict[[1]]$probability))
tmp <- data.frame(values$responseIn,
                  values$predict[[1]]$rubricLevel,
                  prob=values$predict[[1]]$probability,
                  stringsAsFactors = FALSE)      #select relevant variables

tmp <- tmp[order(tmp[,3],decreasing=(params$groupType=="Rubric Level")),]    #sort by probability/distance
tmp <- split(tmp,tmp[,2])                      #split by group ID
maxGroup <- max(unlist(lapply(tmp,nrow)))      #find maximum group membership
params$responseByGroup <- as.data.frame(array("",c(maxGroup,nGroups*2)),stringsAsFactors=FALSE) #initialize response data frame

names(params$responseByGroup) <- as.vector(rbind(params$groupLevels,rep("%",nGroups)))       #add correct group names

print("m3")
# print(str(params$responseByGroup))

for(i in 1:length(groupLvlIDs)){               #populate response data frame
        if(nrow(tmp[[i]])>0){
                params$responseByGroup[1:nrow(tmp[[i]]),2*i-1] <- tmp[[i]][,1]
                params$responseByGroup[1:nrow(tmp[[i]]),2*i] <- tmp[[i]][,3]        
        }
}
print(str(params$responseByGroup))
ptTmp <- params$responseByGroup[,seq(2,2*nGroups,2)] #write probabilty table for histograms
print(str(ptTmp))
params$probTable <- apply(ptTmp,2,as.numeric)
print(values$predict[[1]]$rubricLevel)
print(params$groupFreq)
print(str(params$responseByGroup))
params$responseByGroup[,seq(2,2*nGroups,2)] <- apply(
        params$responseByGroup[,seq(2,2*nGroups,2)],2,function(x) strtrim(x,4)) #format Probabilitys

print("m4")

#------------------ n-gram overabundance -------------------------------------------
params$oAb <- mostOverabundant(values$docTermMat,values$predict[[1]]$rubricLevel,ncol(values$docTermMat))

params$oAbDF <- as.data.frame(matrix(rbind(params$oAb[[1]],params$oAb[[2]]),nrow=nrow(params$oAb[[1]])),stringsAsFactors=FALSE)

tmpDF <- cbind(as.data.frame(as.matrix(values$docTermMat)),data.frame(SCOREOUT=values$predict[[1]]$rubricLevel),stringsAsFactors=FALSE)
freqTable <- aggregate(.~SCOREOUT,data=tmpDF,sum)
freqTable <- freqTable[,2:ncol(freqTable)]  #remove label column

oAb2 <- overAbundance(values$docTermMat,values$predict[[1]]$rubricLevel)

importanceTable <- rbind(oAb2,freqTable)
importanceTable <- data.frame(Term=colnames(oAb2),t(importanceTable))
importanceTable <- importanceTable[complete.cases(importanceTable),]
params$importanceTable <- importanceTable

names(params$oAbDF) <- c(rbind(params$groupLevels,rep("OA",length(params$groupLevels))))

print("m5")

#------------------------- web diagram list --------------------------------------------------

wdMenuNames <- params$groupFreq[,params$groupType]
wdMenuList <- as.list(seq(nGroups)[groupLvlIDs %in% wdMenuNames])
names(wdMenuList) <- wdMenuNames
params$wdMenuList <-wdMenuList
# print(params$wdMenuList)
#------------------------- n-gram correlation map limits -------------------------------------
probThreshold <- 0
corThreshold <- 0.3
minNumTerms <- 5
startNumTerms <- 15
maxNumTerms <- 50

print("m6")

tmpMapControlLimits <- list()
for(gLevel in params$wdMenuList){
        rubricLevelIndex <- values$predict[[1]]$rubricLevel==gLevel &
                values$predict[[1]]$probability>probThreshold
        dtmTmp <- values$docTermMat[rubricLevelIndex,]
        minMinFreq <- findMinFreq(dtmTmp,maxNumTerms)
        startMinFreq <- findMinFreq(dtmTmp,startNumTerms)
        maxMinFreq <- findMinFreq(dtmTmp,minNumTerms)
        dtmTmp2 <- dtmTmp[,findFreqTerms(dtmTmp,startMinFreq)]
        startCorThreshold <- findCorThreshold(dtmTmp2,targetRate = 3.5)
        
        tmpMapControlLimits[[gLevel]] <- c(minMinFreq,startMinFreq,maxMinFreq,startCorThreshold)        
}
params$mapControlLimits <- tmpMapControlLimits
print("m7")

