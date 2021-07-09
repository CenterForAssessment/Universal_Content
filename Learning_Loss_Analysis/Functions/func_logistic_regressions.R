logisticModels <- function(long_data, params, 
                           assessment_type= c("State_Assessment", "ELP_Assessment", "Interim_Assessment"), 
                           extra_interactions=FALSE){
  #I. Single Predictors ----------------------------------------------------------
  #A. Define "Exploration" dataset, using just the current year 
  #tapply(long_data$SCALE_SCORE, long_data$YEAR, summary)
  #years <- params$years[(length(params$years)-1):length(params$years)]
  years <- params$years[length(params$years)]
  explore.data <- long_data[YEAR %in% years,]
  explore.data$NON_TESTED <- ifelse(is.na(explore.data$SCALE_SCORE), 1, 0)
  #explore.data <- dcast(data=explore.data, formula=...~YEAR)
  #need to add in prior scores 
  
  #B. Convert Categorical Variables to Factors
  explore.data$NON_TESTED <- as.factor(explore.data$NON_TESTED)
  explore.data$GRADE      <- as.factor(explore.data$GRADE)
  for(i in params$demos$names){
    explore.data[[i]] <- factor(explore.data[[i]],
                                 levels = params$demos$values[[i]])
  }
  
  #C. Define Demographic factors 
    #C1. Get Demographic Factors 
    fit.demos <- c("GRADE", params$demos$names)#, "SCALE_SCORE_PRIOR_STANDARDIZED")
    if(assessment_type %in% c("State_Assessment", "Interim_Assessment")){
      fit.demos <- c("CONTENT_AREA", fit.demos)
    }
    #C2. Exclude Factors that have no variability 
    for(i in fit.demos){
      demo.length.check <- unique(explore.data[,..i])
      if(sum(!is.na(demo.length.check)) <= 1){
        fit.demos <-   fit.demos[fit.demos != i]
      }
    }
  
  #May actually want to do grade-level regressions
  fit.lvl1  <- vector("list", length=length(fit.demos)+1) #individual predictors plus full set
  for(i in 1:(length(fit.lvl1)-1)){
    formula.text <- paste0("NON_TESTED ~ ", fit.demos[i])
    fit.lvl1[[i]] <- glm(formula=formula.text, family="binomial", data=explore.data)
  }
  #lapply( fit.lvl1, summary)
  
  formula.text <- paste0("NON_TESTED ~", paste0(fit.demos, collapse = " + "))
  fit.lvl1[[length(fit.lvl1)]] <- glm(formula.text, family="binomial", data=explore.data)
  
  #II. Interaction Models --------------------------------------------------------
  if(extra_interactions){
    #A. All 2 and 3 way interactions 
    formula.text <- paste0("NON_TESTED ~", "(", paste0(fit.demos, collapse = " + "), ")^2")
    fit.lvl1[[length(fit.lvl1)+1]] <- glm(formula=formula.text, family="binomial", data=explore.data)
    
    formula.text <- paste0("NON_TESTED ~", "(", paste0(fit.demos, collapse = " + "), ")^3")
    fit.lvl1[[length(fit.lvl1)+1]]   <- glm(formula=formula.text, family="binomial", data=explore.data)
  }

  return(fit.lvl1)
}
#fit.lvl2to3 
#https://cran.r-project.org/web/packages/glmmLasso/glmmLasso.pdf