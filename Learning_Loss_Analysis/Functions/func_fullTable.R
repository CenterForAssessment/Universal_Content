fullParticipationTable <- function(long_data, params){
  #Create a data.frame that examines a prioritized number of data subsets
  #long_data <-  Report_Data$ELP_Assessment 
  
  #Setup -----------------------------------------------------------------------
  #A. Overall, State Level Data ------------------------------------------------
  p.state <- list() 
  
  #A1. "One" Way Splits 
  #A1.A Overall
  p.state[[length(p.state)+1]] <- long_data[,
                             list(LEVEL             = "State",
                                  GRADE             = "All",
                                  SUBGROUP          = "All",
                                  ACHIEVEMENT_LEVEL_PRIOR = "All",
                                  NUMBER_STUDENTS   = length(SCALE_SCORE),
                                  NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                  PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                            by  = list(CONTENT_AREA, YEAR)]
  #A2.B Grade Level
  #table(long_data[YEAR == 2021]$GRADE)
  p.state[[length(p.state)+1]] <- long_data[,
                             list(LEVEL             = "State",
                                  #GRADE             = "All",
                                  SUBGROUP          = "All",
                                  ACHIEVEMENT_LEVEL_PRIOR = "All",
                                  NUMBER_STUDENTS   = length(SCALE_SCORE),
                                  NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                  PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                            by  = list(CONTENT_AREA, YEAR, GRADE)]
  
  #A3.C Subgroup
  loop.start <- length(p.state)+1
  loop.stop  <- length(p.state)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.state[[j]] <- long_data[,list(LEVEL           = "State",
                                    GRADE             = "All",
                                    #SUBGROUP          = "All",
                                    ACHIEVEMENT_LEVEL_PRIOR = "All",
                                    NUMBER_STUDENTS   = length(SCALE_SCORE),
                                    NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                    PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                              by  = list(CONTENT_AREA, YEAR, 
                                         eval(parse(text = params$demos$names[j - loop.start +1])))]   
    
    names(p.state[[j]])[names(p.state[[j]]) == "parse"] <- "SUBGROUP"
  }
  #lapply(p.state, function(x) unique(x$SUBGROUP))
  
  #A3.C ACHIEVEMENT_LEVEL 
  p.state[[length(p.state)+1]] <- long_data[,
                                            list(LEVEL             = "State",
                                                 GRADE             = "All",
                                                 SUBGROUP          = "All",
                                                 #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                                 NUMBER_STUDENTS   = length(SCALE_SCORE),
                                                 NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                                 PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                            by  = list(CONTENT_AREA, YEAR, ACHIEVEMENT_LEVEL_PRIOR)]
  
  #B. Two Way Interactions 
  #B1. Subgroup x Grade Level 
  loop.start <- length(p.state)+1
  loop.stop  <- length(p.state)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.state[[j]] <- long_data[,list(LEVEL           = "State",
                                    #GRADE             = "All",
                                    #SUBGROUP          = "All",
                                    ACHIEVEMENT_LEVEL_PRIOR = "All",
                                    NUMBER_STUDENTS   = length(SCALE_SCORE),
                                    NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                    PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                              by  = list(CONTENT_AREA, YEAR, GRADE,
                                         eval(parse(text = params$demos$names[j - loop.start +1])))]   
    
    names(p.state[[j]])[names(p.state[[j]]) == "parse"] <- "SUBGROUP"
  }
  
  #B2. Subgroup x Achievement Level
  loop.start <- length(p.state)+1
  loop.stop  <- length(p.state)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.state[[j]] <- long_data[,list(LEVEL           = "State",
                                    GRADE             = "All",
                                    #SUBGROUP          = "All",
                                    #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                    NUMBER_STUDENTS   = length(SCALE_SCORE),
                                    NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                    PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                              by  = list(CONTENT_AREA, YEAR, ACHIEVEMENT_LEVEL_PRIOR,
                                         eval(parse(text = params$demos$names[j - loop.start +1])))]   
    
    names(p.state[[j]])[names(p.state[[j]]) == "parse"] <- "SUBGROUP"
  }
  
  #B3. Achievement Level x Grade
  p.state[[length(p.state)+1]] <- long_data[,
                                            list(LEVEL             = "State",
                                                 #GRADE             = "All",
                                                 SUBGROUP          = "All",
                                                 #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                                 NUMBER_STUDENTS   = length(SCALE_SCORE),
                                                 NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                                 PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                            by  = list(CONTENT_AREA, YEAR, GRADE, ACHIEVEMENT_LEVEL_PRIOR)]
  
  #C. Three Way Interactions 
  #C1. Grade x Subgroup x Achievement Level 
  loop.start <- length(p.state)+1
  loop.stop  <- length(p.state)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.state[[j]] <- long_data[,list(LEVEL           = "State",
                                    #GRADE             = "All",
                                    #SUBGROUP          = "All",
                                    #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                    NUMBER_STUDENTS   = length(SCALE_SCORE),
                                    NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                    PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                              by  = list(CONTENT_AREA, GRADE, YEAR, ACHIEVEMENT_LEVEL_PRIOR,
                                         eval(parse(text = params$demos$names[j - loop.start +1])))]   
    
    names(p.state[[j]])[names(p.state[[j]]) == "parse"] <- "SUBGROUP"
  }

  #D. Bind into a Single List 
  #lapply(p.state, function(x) x <- x[,names(p.state[[1]])])
  p.state <- do.call(rbind, p.state)
  #Check NAs
  

  #B. District Level Data ------------------------------------------------------
  p.district <- list() 
  
  #A1. "One" Way Splits 
  #A1.A Overall
  p.district[[length(p.district)+1]] <- long_data[,
                                            list(LEVEL             = "District",
                                                 GRADE             = "All",
                                                 SUBGROUP          = "All",
                                                 ACHIEVEMENT_LEVEL_PRIOR = "All",
                                                 NUMBER_STUDENTS   = length(SCALE_SCORE),
                                                 NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                                 PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                            by  = list(CONTENT_AREA, YEAR, DISTRICT_NUMBER)]
  #A2.B Grade Level
  p.district[[length(p.district)+1]] <- long_data[,
                                            list(LEVEL             = "District",
                                                 #GRADE             = "All",
                                                 SUBGROUP          = "All",
                                                 ACHIEVEMENT_LEVEL_PRIOR = "All",
                                                 NUMBER_STUDENTS   = length(SCALE_SCORE),
                                                 NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                                 PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                            by  = list(CONTENT_AREA, YEAR, DISTRICT_NUMBER,  GRADE)]
  
  #A3.C Subgroup
  loop.start <- length(p.district)+1
  loop.stop  <- length(p.district)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.district[[j]] <- long_data[,list(LEVEL           = "District",
                                    GRADE             = "All",
                                    #SUBGROUP          = "All",
                                    ACHIEVEMENT_LEVEL_PRIOR = "All",
                                    NUMBER_STUDENTS   = length(SCALE_SCORE),
                                    NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                    PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                              by  = list(CONTENT_AREA, YEAR, DISTRICT_NUMBER,  
                                         eval(parse(text = params$demos$names[j - loop.start +1])))]   
    
    names(p.district[[j]])[names(p.district[[j]]) == "parse"] <- "SUBGROUP"
  }
  #lapply(p.district, function(x) unique(x$SUBGROUP))
  
  #A3.C ACHIEVEMENT_LEVEL 
  p.district[[length(p.district)+1]] <- long_data[,
                                            list(LEVEL             = "District",
                                                 GRADE             = "All",
                                                 SUBGROUP          = "All",
                                                 #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                                 NUMBER_STUDENTS   = length(SCALE_SCORE),
                                                 NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                                 PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                            by  = list(CONTENT_AREA, YEAR, DISTRICT_NUMBER, ACHIEVEMENT_LEVEL_PRIOR)]
  
  #B. Two Way Interactions 
  #B1. Subgroup x Grade Level 
  loop.start <- length(p.district)+1
  loop.stop  <- length(p.district)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.district[[j]] <- long_data[,list(LEVEL           = "District",
                                    #GRADE             = "All",
                                    #SUBGROUP          = "All",
                                    ACHIEVEMENT_LEVEL_PRIOR = "All",
                                    NUMBER_STUDENTS   = length(SCALE_SCORE),
                                    NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                    PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                              by  = list(CONTENT_AREA, YEAR, DISTRICT_NUMBER, GRADE,
                                         eval(parse(text = params$demos$names[j - loop.start +1])))]   
    
    names(p.district[[j]])[names(p.district[[j]]) == "parse"] <- "SUBGROUP"
  }
  
  #B2. Subgroup x Achievement Level
  loop.start <- length(p.district)+1
  loop.stop  <- length(p.district)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.district[[j]] <- long_data[,list(LEVEL           = "District",
                                    GRADE             = "All",
                                    #SUBGROUP          = "All",
                                    #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                    NUMBER_STUDENTS   = length(SCALE_SCORE),
                                    NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                    PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                              by  = list(CONTENT_AREA, YEAR, DISTRICT_NUMBER, ACHIEVEMENT_LEVEL_PRIOR,
                                         eval(parse(text = params$demos$names[j - loop.start +1])))]   
    
    names(p.district[[j]])[names(p.district[[j]]) == "parse"] <- "SUBGROUP"
  }
  
  #B3. Achievement Level x Grade
  p.district[[length(p.district)+1]] <- long_data[,
                                            list(LEVEL             = "District",
                                                 #GRADE             = "All",
                                                 SUBGROUP          = "All",
                                                 #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                                 NUMBER_STUDENTS   = length(SCALE_SCORE),
                                                 NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                                 PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                            by  = list(CONTENT_AREA, YEAR, DISTRICT_NUMBER, GRADE, ACHIEVEMENT_LEVEL_PRIOR)]
  
  #C. Three Way Interactions 
  #C1. Grade x Subgroup x Achievement Level 
  loop.start <- length(p.district)+1
  loop.stop  <- length(p.district)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.district[[j]] <- long_data[,list(LEVEL           = "District",
                                    #GRADE             = "All",
                                    #SUBGROUP          = "All",
                                    #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                    NUMBER_STUDENTS   = length(SCALE_SCORE),
                                    NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                    PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                              by  = list(CONTENT_AREA, GRADE, YEAR, DISTRICT_NUMBER,  ACHIEVEMENT_LEVEL_PRIOR,
                                         eval(parse(text = params$demos$names[j - loop.start +1])))]   
    
    names(p.district[[j]])[names(p.district[[j]]) == "parse"] <- "SUBGROUP"
  }
  
  #D. Bind into a Single List 
  #lapply(p.district, function(x) x <- x[,names(p.district[[1]])])
  p.district <- do.call(rbind, p.district)
  #Check NAs
  
  
  #Combine into an overall table
  
  
  #C. School Level Data ------------------------------------------------------
  p.school <- list() 
  
  #A1. "One" Way Splits 
  #A1.A Overall
  p.school[[length(p.school)+1]] <- long_data[,
                                                  list(LEVEL             = "School",
                                                       DISTRICT_NUMBER   = unique(DISTRICT_NUMBER),
                                                       GRADE             = "All",
                                                       SUBGROUP          = "All",
                                                       ACHIEVEMENT_LEVEL_PRIOR = "All",
                                                       NUMBER_STUDENTS   = length(SCALE_SCORE),
                                                       NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                                       PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                                  by  = list(CONTENT_AREA, YEAR, SCHOOL_NUMBER)]
  #A2.B Grade Level
  p.school[[length(p.school)+1]] <- long_data[,
                                                  list(LEVEL             = "School",
                                                       DISTRICT_NUMBER   = unique(DISTRICT_NUMBER),
                                                       #GRADE             = "All",
                                                       SUBGROUP          = "All",
                                                       ACHIEVEMENT_LEVEL_PRIOR = "All",
                                                       NUMBER_STUDENTS   = length(SCALE_SCORE),
                                                       NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                                       PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                                  by  = list(CONTENT_AREA, YEAR, SCHOOL_NUMBER,  GRADE)]
  
  #A3.C Subgroup
  loop.start <- length(p.school)+1
  loop.stop  <- length(p.school)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.school[[j]] <- long_data[,list(LEVEL           = "School",
                                     DISTRICT_NUMBER   = unique(DISTRICT_NUMBER),
                                       GRADE             = "All",
                                       #SUBGROUP          = "All",
                                       ACHIEVEMENT_LEVEL_PRIOR = "All",
                                       NUMBER_STUDENTS   = length(SCALE_SCORE),
                                       NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                       PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                 by  = list(CONTENT_AREA, YEAR, SCHOOL_NUMBER,  
                                            eval(parse(text = params$demos$names[j - loop.start +1])))]   
    
    names(p.school[[j]])[names(p.school[[j]]) == "parse"] <- "SUBGROUP"
  }
  #lapply(p.school, function(x) unique(x$SUBGROUP))
  
  #A3.C ACHIEVEMENT_LEVEL 
  p.school[[length(p.school)+1]] <- long_data[,
                                                  list(LEVEL             = "School",
                                                       DISTRICT_NUMBER   = unique(DISTRICT_NUMBER),
                                                       GRADE             = "All",
                                                       SUBGROUP          = "All",
                                                       #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                                       NUMBER_STUDENTS   = length(SCALE_SCORE),
                                                       NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                                       PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                                  by  = list(CONTENT_AREA, YEAR, SCHOOL_NUMBER, ACHIEVEMENT_LEVEL_PRIOR)]
  
  #B. Two Way Interactions 
  #B1. Subgroup x Grade Level 
  loop.start <- length(p.school)+1
  loop.stop  <- length(p.school)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.school[[j]] <- long_data[,list(LEVEL           = "School",
                                     DISTRICT_NUMBER   = unique(DISTRICT_NUMBER),
                                       #GRADE             = "All",
                                       #SUBGROUP          = "All",
                                       ACHIEVEMENT_LEVEL_PRIOR = "All",
                                       NUMBER_STUDENTS   = length(SCALE_SCORE),
                                       NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                       PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                 by  = list(CONTENT_AREA, YEAR, SCHOOL_NUMBER, GRADE,
                                            eval(parse(text = params$demos$names[j - loop.start +1])))]   
    
    names(p.school[[j]])[names(p.school[[j]]) == "parse"] <- "SUBGROUP"
  }
  
  #B2. Subgroup x Achievement Level
  loop.start <- length(p.school)+1
  loop.stop  <- length(p.school)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.school[[j]] <- long_data[,list(LEVEL           = "School",
                                     DISTRICT_NUMBER   = unique(DISTRICT_NUMBER),
                                       GRADE             = "All",
                                       #SUBGROUP          = "All",
                                       #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                       NUMBER_STUDENTS   = length(SCALE_SCORE),
                                       NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                       PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                 by  = list(CONTENT_AREA, YEAR, SCHOOL_NUMBER, ACHIEVEMENT_LEVEL_PRIOR,
                                            eval(parse(text = params$demos$names[j - loop.start +1])))]   
    
    names(p.school[[j]])[names(p.school[[j]]) == "parse"] <- "SUBGROUP"
  }
  
  #B3. Achievement Level x Grade
  p.school[[length(p.school)+1]] <- long_data[,
                                                  list(LEVEL             = "School",
                                                       DISTRICT_NUMBER   = unique(DISTRICT_NUMBER),
                                                       #GRADE             = "All",
                                                       SUBGROUP          = "All",
                                                       #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                                       NUMBER_STUDENTS   = length(SCALE_SCORE),
                                                       NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                                       PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                                  by  = list(CONTENT_AREA, YEAR, SCHOOL_NUMBER, GRADE, ACHIEVEMENT_LEVEL_PRIOR)]
  
  #C. Three Way Interactions 
  #C1. Grade x Subgroup x Achievement Level 
  loop.start <- length(p.school)+1
  loop.stop  <- length(p.school)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.school[[j]] <- long_data[,list(LEVEL           = "School",
                                     DISTRICT_NUMBER   = unique(DISTRICT_NUMBER),
                                       #GRADE             = "All",
                                       #SUBGROUP          = "All",
                                       #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                       NUMBER_STUDENTS   = length(SCALE_SCORE),
                                       NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                       PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                 by  = list(CONTENT_AREA, GRADE, YEAR, SCHOOL_NUMBER,  ACHIEVEMENT_LEVEL_PRIOR,
                                            eval(parse(text = params$demos$names[j - loop.start +1])))]   
    
    names(p.school[[j]])[names(p.school[[j]]) == "parse"] <- "SUBGROUP"
  }
  
  #D. Bind into a Single List 
  #lapply(p.school, function(x) x <- x[,names(p.school[[1]])])
  p.school <- do.call(rbind, p.school)
  #Check NAs
  
  
  #Combine into an overall table
  p.state$DISTRICT_NUMBER <- "All"
  p.state$SCHOOL_NUMBER <- "All"
  
  p.district$SCHOOL_NUMBER <- "All"
  
  p <- rbind(p.state, p.district, p.school)
  
  return(p)
}#end of function