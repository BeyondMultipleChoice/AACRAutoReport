library(DBI)
library(pool)
library(dplyr)


#This script was modified on 1/27/2020 for AutoReport in production server. 
#It has a new function to add a new entry into Questions Table if it's the first time that a user uploads responses

questionObject <- list(name=params$question,
                       majVersion=params$majorSMversion,
                       rubric=params$rubricType,   #MODIF -- add rubric type in the Questions Table record
                       context=params$questionText)   #MOdif -- add questionText in the Questions Table record


writeStudents <- function(DBconnection,hashedIDs){
        #writes new hashed student IDs to student tables, returns an ordered list of primary keys
        
        uniqueHID <- unique(hashedIDs)
        
        #check which IDs exist
        existingHIDs <- DBconnection %>% tbl("students") %>% filter(student_hashed_id %in% uniqueHID) %>% 
                select(student_hashed_id) %>% as.data.frame()
        existUniqueHID <- unique(existingHIDs$student_hashed_id)
        #add IDs that do not exist
        if(length(uniqueHID) > length(existUniqueHID)){
                tmpHashedIDs <- data.frame(student_hashed_id =uniqueHID[!(uniqueHID %in% existUniqueHID)])
                dbWriteTable(DBconnection,"students",tmpHashedIDs,append=TRUE,row.names=FALSE)
        }
        
        #retrieve student table primary keys
        tmpIDs <- DBconnection %>% tbl("students") %>% filter(student_hashed_id %in% uniqueHID) %>%
                select(id,student_hashed_id) %>% as.data.frame()
        uniqueIndex <- duplicated(tmpIDs$student_hashed_id)
        primaryKey <- full_join(x=data.frame(student_hashed_id=hashedIDs),y=tmpIDs[!uniqueIndex,],by="student_hashed_id")["id"] #####
        
        unname(unlist(primaryKey))
}

writeInstructor <- function(DBconnection,instructorEmail){
        #writes instructor email to instructor table, returns instructor primary key
        
        #check if email exist
        existingEmails <- DBconnection %>% tbl("instructors") %>% filter(email == instructorEmail) %>% 
                select(email) %>% as.data.frame()
        
        #add email if does not exist
        if(nrow(existingEmails) == 0){
                tmpEmails <- data.frame(email = instructorEmail)
                dbWriteTable(DBconnection,"instructors",tmpEmails,append=TRUE,row.names=FALSE)
        }
        
        #retrieve instructor primary key
        primaryKey <- DBconnection %>% tbl("instructors") %>% filter(email==instructorEmail) %>% select(id) %>% as.data.frame()
        unname(unlist(primaryKey))
}

writeCourse <- function(DBconnection,instructorID,question){
        #write placeholder course description, return course primary key
        
        tmpDescription <- paste("Data set for", question$name,"submitted",Sys.Date())
        
        #check if course exist
        existingCourse <- DBconnection %>% tbl("courses") %>% filter(instructor_id==instructorID) %>% 
                filter(description==tmpDescription) %>% select(id) %>% as.data.frame()
        
        #write placeholder description
        if(nrow(existingCourse) == 0){
                tmpCourseEntry <- data.frame(instructor_id=instructorID, description = tmpDescription)
                dbWriteTable(DBconnection,"courses",tmpCourseEntry,append=TRUE,row.names=FALSE)
                
        }
        
        #retrieve course primary key
        primaryKey <- DBconnection %>% tbl("courses") %>% filter(instructor_id==instructorID) %>% 
                filter(description==tmpDescription) %>% select(id) %>% as.data.frame()
        unname(unlist(primaryKey))
}


#MODIF, ADDED: writes question if not available already in the database. 
writeQuestion <- function(DBconnection, question)  
{
     
        #check if question AND its version exist
        existingQuestions <- DBconnection %>% tbl("questions") %>% filter(question_name == question$name && question_version == question$majVersion) %>% 
                select(question_name) %>% as.data.frame()
        
        
        #add question if does not exist
        if(nrow(existingQuestions) == 0){
            
                tmpQuestion <- data.frame(question_name = question$name,
                                          question_text = question$context,
                                          question_version = question$majVersion,
                                          rubric_type = question$rubric,
                                          development_status = "production")
                
                dbWriteTable(DBconnection,"questions",tmpQuestion,append=TRUE,row.names=FALSE)
        }
        
        #retrieve questions primary key
        primaryKeyQ <- DBconnection %>% tbl("questions") %>% filter(question_name == question$name) %>% select(id) %>% as.data.frame()
        
        unname(unlist(primaryKeyQ))
}

writeQuestionAdmin <- function(DBconnection,question,courseID){
        #write question administrations entry, return primary key
        currentDate <- Sys.Date()

        #lookup question id
        questionID <- DBconnection %>% tbl("questions") %>% filter(question_name == question$name) %>%
                filter(question_version == question$majVersion) %>%
                select(id) %>% as.data.frame()
        questionID <- unname(unlist(questionID))

        #write question adminstration entry
        QAdminEntry <- data.frame(course_id=courseID,question_id=questionID,date_uploaded=currentDate)
        dbWriteTable(DBconnection,"question_administrations",QAdminEntry,append=TRUE,row.names=FALSE)

        #retrieve primary key
        primaryKey <- DBconnection %>% tbl("question_administrations") %>% filter(course_id==courseID) %>%
                filter(question_id==questionID) %>% filter(date_uploaded==currentDate) %>% select(id) %>% as.data.frame()
        primaryKey <- unname(unlist(primaryKey))
        primaryKey[length(primaryKey)]
}


writeResponses <- function(DBconnection,hashedIDs,studentResponses,instructorEmail,question){
        #write student responses to response table
    
        #write instructor
        instructorID <- writeInstructor(DBconnection,instructorEmail)
        
        #MODIF- write question if it's not in the database...
        questionID <- writeQuestion(DBconnection, question)  #questionID is not used so far  
        
        #write course
        courseID <- writeCourse(DBconnection,instructorID, question)
        
        #write question administration
        QAdminID <- writeQuestionAdmin(DBconnection,question,courseID)
        
        #write hashedIDs & retrieve student
        print(c("hi",length(hashedIDs)))
        
        if(length(hashedIDs>0)){
                studentID <- writeStudents(DBconnection,hashedIDs)        
        }
        
        
        #write responses
        if(length(hashedIDs>0)){
                responsesEntry <- data.frame(student_id=studentID,
                                             question_administration_id=rep(QAdminID,length(studentID)),
                                             constructed_response=studentResponses)
        }else{
                responsesEntry <- data.frame(question_administration_id=rep(QAdminID,length(studentResponses)),
                                             constructed_response=studentResponses)
        }
        dbWriteTable(DBconnection,"responses",responsesEntry,append=TRUE,row.names=FALSE)
}

pool <- dbPool(
        drv = RMySQL::MySQL(),
        dbname = dbname,
        host = host,
        username = config::get("Database",file="config/config.yml")$user,
        password = config::get("Database",file="config/config.yml")$pw
)



if(input$idColumn!=0){
        hashedIDs <- hashIDs(datIn(),as.numeric(input$idColumn))[,as.numeric(input$idColumn)]
}else{
        hashedIDs <- rep(NULL,nRrow())
}

writeResponses(DBconnection=pool,
               hashedIDs=hashedIDs,
               studentResponses=datIn()[,as.numeric(input$responseColumn)],
               instructorEmail=trimws(unlist(strsplit(input$emailString,",")))[1],
               question=questionObject)
