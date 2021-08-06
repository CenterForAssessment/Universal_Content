## Various Helper Functions for Propensity Score Weighting
##
## AUTHOR: Allie Cooperman 
## DATE CREATED: 7/9/2021
## DATE LAST EDITED: 7/29/2021

####-----dynamic_header-----####

## Creates header for grouping columns or rows in kableExtra tables
## Arguments:
##    header.type = Character indicating whether the header is for grouping columns or rows
##    group.labels = Character vector of grouping characteristic (e.g., grades)
##    num.columns = If grouping columns, number of columns that should be grouped 
##                  within each element of group.labels
##    group.levels = If grouping rows, vector for number of rows that should be grouped
##                   within each element of group.labels

# Begin function
dynamic_header <- function(header.type = c("Columns", "Rows"), 
                           group.labels,
                           num.columns = NULL,
                           group.levels = NULL) {
  
  # Set number of groups
  n <- length(group.labels)
  
  # Create column header
  if(header.type == "Columns") {
    
    # Begin header with one for the left-hand column
    header.vector <- setNames(1, " ")
    
    # Iteratively add column labels for groups
    for(i in 1:n) {
      
      header.vector <- c(header.vector, 
                         setNames(num.columns, group.labels[i]))
      
    }
    
  } # end if header.type == "Columns"
  
  # Create row header
  if(header.type == "Rows") {
    
    # Begin header
    header.vector <- NULL
    
    # Iteratively add labels for groups
    for(i in 1:n) {
      
      header.vector <- c(header.vector,
                         setNames(group.levels[i], group.labels[i]))
      
    }
    
  } # end if header.type == "Rows"
  
  # Return header
  return(header.vector)

} # end dynamic_header function


####-----compute_weights-----####

## Compute propensity score weights for logistic or hierarchical models
## References: Bishop et al. (2018); Leite et al. (2015)
## Arguments:
##    pscores = Vector of propensity scores
##    group = Binary vector differentiating two groups (e.g., 1 = treatment, 0 = control)
##    estimand = Average treatment on the treated (ATT) or average treatment effect (ATT)

# Begin function
compute_weights <- function(pscores, group, estimand = c("ATT", "ATE")) {
  
  # Compute weights
  if(estimand == "ATT") {
    
    # Weights_{group = 1} = 1
    # Weights_{group = 0} = p/(1-p)
    pweights <- group + (1 - group)*(pscores / (1 - pscores))
    
  } else {
    
    # Weights_{group = 1} = 1/p
    # Weights_{group = 0} = 1/(1-p)
    pweights <- (group / pscores) + (1 - group) / (1 - pscores)
    
  }
  
  # Return weights
  return(pweights)
  
} # end compute_weights function


####-----compute_es-----####

## Compute standardized mean differences between groups with and without weights
## Applicable for propensity scores estimated using parametric 
## (logistic or hierarchical) models
## Effect sizes for propensity scores estimated using decision trees 
## are computed directly from `twang` package
## References: MatchIt R package; Leite et al. (2015)
## Arguments:
##    x = Covariate (categorical variables should be dummy- or contrast-coded)
##    group = Binary vector differentiating two groups (e.g., 1 = treatment, 0 = control)
##    w = Propensity score weights
##    method = Character string differentiating logistic or hierarchical method
##             used to estimate propensity scores
##    estimand = Average treatment on the treated (ATT) or average treatment effect (ATT)


# Begin function
compute_es <- function(x, group, w, 
                       method = c("logistic", "hierarchical"), 
                       estimand = c("ATT", "ATE")) {
  
  # Logistic regression: Use standardized effect sizes from MatchIt
  if(method == "logistic") {
    
    # Specify standard deviation for denominator of effect size
    sdtype <- if(estimand == "ATT") "treated" else "pooled"
    
    # Subset to complete cases only
    obs.complete <- which(!is.na(w))
    x.complete <- x[obs.complete]
    group.complete <- group[obs.complete]
    w.complete <- w[obs.complete]
    
    # Compute weighted effect size
    es.w <- MatchIt:::qoi(xx = x.complete, 
                          tt = group.complete,
                          s.weights = w.complete, 
                          s.d.denom = sdtype, 
                          standardize = T)[[3]]
    
    # Compute unweighted effect size
    es.uw <- MatchIt:::qoi(xx = x.complete, 
                           tt = group.complete,
                           s.weights = rep(1, length(x.complete)), 
                           s.d.denom = sdtype, 
                           standardize = T)[[3]]
    
  }
  
  # Hierarchical: Use standardized effect sizes from Leite et al. (2015)
  if(method == "hierarchical") {
    
    # Compute weighted effect size
    # Divide by weighted standard deviation for full sample
    wm.g1 <- weighted.mean(x[group == 1], w[group == 1])
    wm.g2 <- weighted.mean(x[group == 0], w[group == 0])
    wsd <- sqrt(MatchIt:::wvar(x = x, w = w))
    es.w <- (wm.g2 - wm.g1) / wsd
    
    # Compute unweighted effect size
    # Divide by standard deviation for full sample
    es.uw <- (mean(x[group == 0]) - mean(x[group == 1])) / sd(x)
    
  }
  
  # Return vector of unweighted and weighted effect sizes
  return(c(es.uw, es.w))
  
} # end compute_es function


####----cov_table-----####

## Function to create table with covariate distributions by year and grade
## References: https://stackoverflow.com/questions/33185982/how-to-use-data-table-to-efficiently-calculate-allele-frequencies-proportions
##             https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
## Arguments:
##    data = Data table 
##    covlist = List of covariates to compute group-level distributions
##    covnames = List of covariate names (for naming continuous variables)
##    grades = Vector of grades

# Begin function
cov_table <- function(data, covlist, covnames, grades) {
  
  # Allocate memory for table
  ctable <- NULL
  
  # For-loop across selected covariates
  for(iCov in 1:length(covlist)) {
    
    # Select covariate
    cov.name <- covlist[iCov]
    
    # Subset data
    cov.columns <- c("GRADE", "YEAR", cov.name)
    cov.data <- data[, ..cov.columns]
    
    # Compute proportions/means and reshape wide
    if(class(cov.data[, eval(parse(text = cov.name))]) == "factor") {
      
      aggtab <- cov.data[, .N, keyby = cov.columns][, meanvar := N/sum(N), by = .(GRADE, YEAR)]
      tabwide <- dcast(aggtab[, !c("N")], 
                       eval(parse(text = cov.name)) ~ GRADE + YEAR, 
                       value.var = "meanvar")
      
    } else {
      
      aggtab <- cov.data[, .(meanvar = lapply(.SD, mean, na.rm = T)), keyby = .(GRADE, YEAR)]
      aggtab[, paste(cov.name) := cov.name]
      aggtab$meanvar <- as.numeric(aggtab$meanvar)
      aggtab$meanvar <- round(aggtab$meanvar, 3)
      tabwide <- dcast(aggtab, 
                       eval(parse(text = cov.name)) ~ GRADE + YEAR, 
                       value.var = "meanvar")
      tabwide[1, 1] <- covnames[iCov]
      
    }
    
    # Update column name for rbind
    names(tabwide)[1] <- "COVARIATE"
    
    # Add covariate proportions to full table
    ctable <- rbind(ctable, tabwide)
    
  }
  
  # Return table
  return(ctable)
  
} # end cov_table function


####-----psw_commonsupport-----####

## Function to create tables evaluating common support regions
## Arguments:
##    data = Data table with propensity scores and propensity score weights
##    grades = Vector of grades by which to group results
##    groupvar = Character value indicting the grouping variable

# Begin function
psw_commonsupport <- function(data, grades, groupvar) {
  
  # Number of grades
  numgrades <- length(grades)
  
  # Key columns
  keycol <- c("GRADE", groupvar)
  
  # Create table of score ranges by grade
  psrange.sub <- data[, .(minps = min(PS, na.rm = T),
                          maxps = max(PS, na.rm = T),
                          prop0 = mean(PS == 0, na.rm = T),
                          prop1 = mean(PS == 1, na.rm = T)), 
                      keyby = keycol]
  psrange.sub.wide <- reshape(psrange.sub, 
                              timevar = "GRADE", 
                              idvar = groupvar, 
                              direction = "wide")
  
  # Create table of weight ranges by grade
  pswrange.sub <- data[, .(minps = min(PSWEIGHTS, na.rm = T),
                           maxps = max(PSWEIGHTS, na.rm = T),
                           originalN = .N,
                           effss = MatchIt:::ESS(PSWEIGHTS)),
                       keyby = keycol]
  pswrange.sub.wide <- reshape(pswrange.sub, 
                               timevar = "GRADE", 
                               idvar = groupvar, 
                               direction = "wide")
  
  # Compute lower bound of common support range
  mincol <- paste0("minps.", grades)
  mincs <- apply(psrange.sub.wide[, ..mincol], 2, max)
  
  # Compute upper bound of common support range
  maxcol <- paste0("maxps.", grades)
  maxcs <- apply(psrange.sub.wide[, ..maxcol], 2, min)
  
  # Allocate memory to save proportion of 
  # students within common support range
  propincs <- rep(NA, numgrades)
  
  # For-loop across grades
  for(iGrade in 1:numgrades) {
    
    # Subset grade-level data
    tmp.data <- data[GRADE == grades[iGrade],]
    
    # Compute proportion
    propincs[iGrade] <- sum(tmp.data$PS >= mincs[iGrade] & 
                              tmp.data$PS <= maxcs[iGrade]) / nrow(tmp.data)
    
  }
  
  # Append variables to create table
  csupport.table <- data.table(
    
    "Grade" = params$grades,
    "CommonSupport.Min" = mincs,
    "CommonSupport.Max" = maxcs,
    "CommonSupport.Prop" = round(propincs*100, 2)
    
  )
  
  # Return output
  return(list(PSrange = psrange.sub.wide,
              PSWrange = pswrange.sub.wide,
              CommonSupport = csupport.table))
  
} # end psw_commonsupport function


####------psw_analysis-----####

## Function to compute estimated mean differences with and without propensity score weights
## Arguments:
##    data = Data table 
##    outcome = Outcome variable
##    group = Binary vector differentiating two groups (e.g., 1 = treatment, 0 = control)
##    cov.include = Character vector of covariates to include for doubly-robust model
##    w = Propensity score weights
##    type = Analysis type, if hierarchical then include random intercept and slope for `random.var`
##    random.var = Variable for random effects

# Function to compute estimated mean differences with and without weights
psw_analysis <- function(data, outcome, group, cov.include = NULL, w, 
                         type = c("non-hierarchical", "hierarchical"),
                         random.var = NULL) {
  
  # Set model formula
  mod.formula <- paste0(outcome, " ~ ", group)
  
  # If doubly-robust model, add covariates as fixed effects
  if(!is.null(cov.include)) {
    
      mod.formula <- paste(mod.formula, paste(cov.include, collapse = " + "), sep = " + ")
    
  }

  # Fit models, non-hierarchical
  if(type == "non-hierarchical") {

      # Fit OLS models
      fitmod.noweights <- lm(as.formula(mod.formula), data = data)
      fitmod.weighted <- lm(as.formula(mod.formula), data = data, weights = w)
      
      # Save estimated mean differences
      mdiff.unweighted <- fitmod.noweights$coefficients[[2]]
      mdiff.weighted <- fitmod.weighted$coefficients[[2]]
    
  } # end if type == "non-hierarchical"
  
  # If hierarchical analysis
  if(type == "hierarchical") {
    
      # Add random intercept and slope
      randomeffect <- paste0("(1 + ", group, " | ", random.var, ")")
      mod.formula <- paste(mod.formula, randomeffect, sep = " + ")
      
      # Append weights
      data$w <- w
      
      # Add school-level weights for WeMix
      # Unity vector
      data$WL2 <- 1
      
      # Add individual weights for unweighted model
      # Unity vector
      data$WL1 <- 1
   
      # Fit OLS models
      fitmod.noweights <- WeMix::mix(as.formula(mod.formula), data = data, 
                                     weights = c("WL1", "WL2"))
      fitmod.weighted <- WeMix::mix(as.formula(mod.formula), data = data, 
                                    weights = c("w", "WL2"))
      
      # Save estimated mean differences
      mdiff.unweighted <- fitmod.noweights$coef[[2]]
      mdiff.weighted <- fitmod.weighted$coef[[2]]
    
  }
  
  # Return list
  return(list(Unweighted = mdiff.unweighted,
              Weighted = mdiff.weighted,
              ModUnweighted = fitmod.noweights,
              ModWeighted = fitmod.weighted))
  
}