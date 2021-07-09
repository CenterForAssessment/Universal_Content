func_SummaryTable <- function(p, subject, value.var="PERCENT_TESTED"){ #table produced via func_fullTable.R
  #may want a dt table in the appendix for review
  #right now, this table exlcudes subject
  #Many want to include the prior achievement level, after creating the actual
  #right achievement level
  
  #A. Overall ------------------------------------------------------------------
  p.subset <- p[LEVEL        == "State" & 
                CONTENT_AREA == subject &
                GRADE        == "All" & 
                SUBGROUP     == "All" & 
                ACHIEVEMENT_LEVEL_PRIOR == "All" &
                DISTRICT_NUMBER       == "All" &
                SCHOOL_NUMBER           == "All", ]
  p.table1 <- dcast(data=p.subset, GRADE + SUBGROUP ~ YEAR, value.var=value.var)
  p.table1$variable <- "Overall"
  p.table1$variable.name <- "Overall"  
  
  #B. Grade Levels -------------------------------------------------------------
  p.subset <- p[LEVEL        == "State" & 
                CONTENT_AREA == subject &
                GRADE %in% unique(p$GRADE)[unique(p$GRADE) != "All"] &
                SUBGROUP     == "All" & 
                ACHIEVEMENT_LEVEL_PRIOR == "All" &
                DISTRICT_NUMBER         == "All" &
                SCHOOL_NUMBER           == "All", ]
  
  p.subset <- dcast(p.subset, GRADE + SUBGROUP ~ YEAR, 
                    value.var=value.var)
  p.subset$GRADE <- as.numeric(p.subset$GRADE)
  p.subset <- p.subset[order(p.subset$GRADE),]
  p.subset$variable <- "Grade Level"
  p.subset$variable.name <- p.subset$GRADE
  p.table1 <- rbind(p.table1, p.subset)
  
  #C. Demographics  ------------------------------------------------------------
  for(i in params$demos$names){
    p.subset <- p[LEVEL        == "State" & 
                  CONTENT_AREA == subject &
                  GRADE        == "All" & 
                  SUBGROUP %in% params$demos$values[[i]] & #code out unkowns
                  ACHIEVEMENT_LEVEL_PRIOR == "All" &
                  DISTRICT_NUMBER       == "All" &
                  SCHOOL_NUMBER           == "All", ]
    p.subset <- p.subset[p.subset$NUMBER_TESTED >= params$nsize_rule ,]
    p.subset <- dcast(p.subset, GRADE + SUBGROUP ~ YEAR, value.var=value.var)
    p.subset$variable <- i
    p.subset$variable.name <-  p.subset$SUBGROUP
    p.table1 <- rbind.fill(p.table1, p.subset)
  }
  
  #E. Format Table -------------------------------------------------------------
  row.names(p.table1) <- NULL
  
  p.table1.rows <- table(p.table1$variable)
  p.table1.rows <- p.table1.rows[unique(p.table1$variable)]
  names(p.table1.rows) <- gsub("_STATUS", "", names(p.table1.rows))
  names(p.table1.rows) <- gsub("_", " ", names(p.table1.rows))
  names(p.table1.rows)[grepl("FREE", names(p.table1.rows))] <- "Free & Reduced Lunch"
  names(p.table1.rows)[grepl("ELL", names(p.table1.rows))]  <- "English Language Learners"
  names(p.table1.rows)[grepl("IEP", names(p.table1.rows))]  <- "Individualized Education Plan"
  
  names(p.table1.rows) <- tolower(names(p.table1.rows))
  names(p.table1.rows) <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", 
                               names(p.table1.rows), perl = TRUE)
  
  return(list(p.table1, p.table1.rows))
}