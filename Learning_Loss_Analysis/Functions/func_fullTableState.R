stateParticipationTable <- function(long_data, subject=c("ELA", "MATHEMATICS"), params){
  #Create a data.frame that examines a prioritized number of data subsets
  #long_data <-  Report_Data$ELP_Assessment

  #Setup -----------------------------------------------------------------------
  #A. Overall, State Level Data ------------------------------------------------
  long_data <- data.table(long_data)
  p.state <- list()

  #A1. "One" Way Splits
  p.state[[length(p.state)+1]] <- long_data[CONTENT_AREA == subject,
                                  list(LEVEL                = "State",
                                       GRADE                = "All",
                                       SUBGROUP             = "All",
                                       PRIOR_ACHIEVEMENT_LEVEL = "All",
                                       NUMBER_STUDENTS      = length(SCALE_SCORE),
                                       NUMBER_TESTED        = sum(!is.na(SCALE_SCORE)),
                                       PERCENT_TESTED       = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100
                                       #PERCENT_PROF         =
                                  ),
                            by  = list(YEAR)]
  #A2.B Grade Level
  #table(long_data[YEAR == 2021]$GRADE)
  p.state[[length(p.state)+1]] <- long_data[CONTENT_AREA ==subject,
                             list(LEVEL             = "State",
                                  #GRADE             = "All",
                                  SUBGROUP          = "All",
                                  PRIOR_ACHIEVEMENT_LEVEL  = "All",
                                  NUMBER_STUDENTS   = length(SCALE_SCORE),
                                  NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                  PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                            by  = list(YEAR, GRADE)]

  if(FALSE){
  #A3.C Subgroup
  loop.start <- length(p.state)+1
  loop.stop  <- length(p.state)+length(params$demos$names)
  for(j in loop.start:loop.stop){
    p.state[[j]] <- long_data[CONTENT_AREA ==subject,
                              list(LEVEL           = "State",
                                    GRADE             = "All",
                                    #SUBGROUP          = "All",
                                    PRIOR_ACHIEVEMENT_LEVEL = "All",
                                    NUMBER_STUDENTS   = length(SCALE_SCORE),
                                    NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                    PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                              by  = list(YEAR,
                                         eval(parse(text = params$demos$names[j - loop.start +1])))]

    names(p.state[[j]])[names(p.state[[j]]) == "parse"] <- "SUBGROUP"
  }
  #lapply(p.state, function(x) unique(x$SUBGROUP))
  }

  #A3.C ACHIEVEMENT_LEVEL
  p.state[[length(p.state)+1]] <- long_data[CONTENT_AREA ==subject,
                                            list(LEVEL             = "State",
                                                 GRADE             = "All",
                                                 SUBGROUP          = "All",
                                                 #ACHIEVEMENT_LEVEL_PRIOR = "All",
                                                 NUMBER_STUDENTS   = length(SCALE_SCORE),
                                                 NUMBER_TESTED     = sum(!is.na(SCALE_SCORE)),
                                                 PERCENT_TESTED    = (length(SCALE_SCORE) - sum(is.na(SCALE_SCORE)))/length(SCALE_SCORE)*100),
                                            by  = list(YEAR, PRIOR_ACHIEVEMENT_LEVEL)]

  p.state <- do.call(rbind.fill, p.state)

  return(p.state)
}#end of function
