#---- make DocTermMatrix for reports ------------------------------------
dmTrain <- dfm(corpus(iconv(datInTmp[,respColTmp],to="utf-8",sub="")), 
               ngrams=as.numeric(input$ngramDepth),verbose=FALSE)  
dmTrain <- dfm_trim(dmTrain,sparsity=input$sparsity,verbose=FALSE) #remove sparse terms
dmTrain <- convert(dmTrain,to="tm")            # convert to tm DTM structure
dmTrain <- ResortDtm(dmTrain)                        # resort dtm to standard sparse matrix convention

questionDictionary <- as.list(colnames(dmTrain)) #build quanteda::dfm format term dictionary
names(questionDictionary) <- colnames(dmTrain)
questionDictionary <- dictionary(questionDictionary)

#make final stem dictionary
tmpQuestionDictionary <- unname(unlist(questionDictionary)) #unlist questionDictionary from quanteda format
tmpQuestionDictionary <- gsub("_"," ",tmpQuestionDictionary) #replace underscore notation with whitespace

#make first stem dictionary
tmpStem <- makeFirstStemDictionary(datInTmp,respColTmp)


# stemDictionary <- makeFinalStemDictionary(questionDictionary,tmpStem)
stemDictionary <- makeFinalStemDictionary(tmpQuestionDictionary,tmpStem) 

#stem complete n-grams
completedNgrams <- stemCompleteText(tmpQuestionDictionary,stemDictionary) 
completedNgrams <- stemReplaceNAs(tmpQuestionDictionary,completedNgrams,dmTrain,datInTmp,respColTmp) 
completedNgrams <- replaceDuplicate(tmpQuestionDictionary,completedNgrams)#replace duplicated stem complete n-grams

#replace presistant duplicates
if (sum(duplicated(completedNgrams))>0){
        dupIndex <- duplicated(completedNgrams)
        completedNgrams[dupIndex] <- dmTrain$dimnames$Terms[dupIndex]
}

values$docTermMat <- dmTrain
values$docTermMat$dimnames$Terms <- completedNgrams

#print("****************************")
#junk <- duplicated(completedNgrams)
#print(completedNgrams[junk])
#print(dmTrain$dimnames$Terms[junk])
#print("****************************")

valuesHS$docTermMat <- dmTrain
valuesHS$docTermMat$dimnames$Terms <- completedNgrams

rm(dmTrain)
#------------------------------------------------------
