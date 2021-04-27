#library(qdap)
library(textclean)

# numbers2words <- function(x){
#         ## From John Fox
#         ## https://stat.ethz.ch/pipermail/r-help/2005-April/069647.html
#         x <- as.numeric(x)
#         helper <- function(x){
#                 digits <- rev(strsplit(as.character(x), "")[[1]])
#                 nDigits <- length(digits)
#                 if (nDigits == 1) as.vector(ones[digits])
#                 else if (nDigits == 2)
#                         if (x <= 19) as.vector(teens[digits[1]])
#                 else trim(paste0(tens[digits[2]],
#                                 Recall(as.numeric(digits[1]))))
#                 else if (nDigits == 3) trim(paste0(ones[digits[3]], "hundred", 
#                                                   Recall(makeNumber(digits[2:1]))))
#                 else {
#                         nSuffix <- ((nDigits + 2) %/% 3) - 1
#                         if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
#                         trim(paste0(Recall(makeNumber(digits[
#                                 nDigits:(3*nSuffix + 1)])),
#                                 suffixes[nSuffix],  
#                                 Recall(makeNumber(digits[(3*nSuffix):1]))))
#                 }
#         }
#         trim <- function(text){
#                 gsub("^\ ", "", gsub("\ *$", "", text))
#         }      
#         makeNumber <- function(...) as.numeric(paste0(..., collapse=""))
#         opts <- options(scipen=100)
#         on.exit(options(opts))
#         ones <- c("", "one", "two", "three", "four", "five", "six", "seven", 
#                   "eight", "nine")
#         names(ones) <- 0:9 
#         teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
#                    "sixteen", " seventeen", "eighteen", "nineteen")
#         names(teens) <- 0:9
#         tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy",
#                   "eighty", "ninety")
#         names(tens) <- 2:9 
#         x <- round(x)
#         suffixes <- c("thousand", "million", "billion", "trillion")
#         if (length(x) > 1) return(sapply(x, helper))
#         helper(x)
# }

condenseNumberWords <- function(stringVector){
        numberWords <- c("one", "two", "three", "four", "five", "six", "seven", 
                                 "eight", "nine","ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
                                 "sixteen", "seventeen", "eighteen", "nineteen","twenty", "thirty", "forty", "fifty", "sixty", "seventy",
                                 "eighty", "ninety","hundred","thousand", "million", "billion", "trillion")
        searchPattern <- paste0("(",paste(numberWords,collapse="|"),")")
        numWordIndex <- grepl(searchPattern,stringVector)
        stringVectorOut <- c()
        tmpWord <-""
        i<-1
        for(indx in numWordIndex){
                if(indx){
                        tmpWord <- paste0(tmpWord,stringVector[i])
                }else if(tmpWord==""){
                        stringVectorOut <- c(stringVectorOut,stringVector[i])
                }else{
                        stringVectorOut <- c(stringVectorOut,tmpWord,stringVector[i])
                        tmpWord <- ""
                }
                i<-i+1
        }
        if(tmpWord!=""){stringVectorOut <- c(stringVectorOut,tmpWord)}
        stringVectorOut
}

homogenizeNumbers <- function(stringIn){
        stringTmp <- strsplit(stringIn,"\\s+")[[1]]
        stringTmp <- condenseNumberWords(stringTmp)
        stringTmp <- paste(stringTmp,collapse=" ")
        stringTmp <- replace_ordinal(stringTmp,num.paste=TRUE)
        stringTmp <- replace_number(stringTmp,num.paste=FALSE)
        stringTmp
#         digitIndex <- grepl("[0-9]",stringTmp)
#         stringTmp[digitIndex] <- sapply(stringTmp[digitIndex],numbers2words)
#         paste(stringTmp,collapse=" ")
}
