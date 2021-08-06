#' Propensity score wrapper function
#' 
#' `psw_wrapper` estimates propensity scores and computes inverse propensity score weights
#' 
#' This function allows users to estimate propensity scores using logistic regression (use `lm`),
#' gradient boosting (with the `twang` package), or a two-level, random intercept model (using the `lme4` package). 
#' Propensity scores are computed for two cross-sectional samples, and weights are computed using inverse
#' probability weighting. 
#' 
#' @param psformula Character string for formula to be used when estimating the propensity scores.
#' @param data Data for score estimation, can be of class data.frame or data.table. Categorical variables should be coded using dummy or contrast coding beforehand.
#' @param method Propensity score estimation method; options are "logistic" for logistic regression, 
#'               "gradient" for gradient boosted decision trees, and "hierarchical" for multilevel model.
#' @param estimand Propensity score estimand; options are "ATT" for average treatment on the treated, or
#'                 "ATE" for average treatment effect.
#' @param thresholds Lower and upper bounds for standardized mean difference (i.e., effect size) after weighting; 
#'                   if effect size greater in magnitude than `thresholds[2]` and `method = "logistic` or `method = hierarchical`, 
#'                   propensity score estimation is re-run using fixed-effect interaction terms. 
#' @param twangStop Stopping method for gradient boosting method with the `twang` package
#'                 
#' @return A list with estimated propensity scores, weights, and effect sizes to evaluate covariate balance.
#' \itemize{
#'    \item PS: Vector of estimated propensity scores
#'    \item PSWeights: Vector of inverse probability weights
#'    \item ES: Table of standardized mean differences on the selected covariates
#'    \item DoublyRobust: Character string of covariates to be included in a doubly-robust model, having effect sizes within the given thresholds
#'    \item UnbalancedCov: Character string of covariates with effect sizes greater than `thresholds[2]` after weighting 
#'    \item ModObject: Model object for estimating propensity scores
#' }
#' 
#' @references Methods courtesy of
#' \itemize{
#'    \item Cefalu, M., Ridgeway, G., McCaffrey, D., Morral, A., Griffin, B. A., & Burgette, L. (2021). twang: Toolkit for weighting and analysis of nonequivalent groups. R package version 2.1. https://CRAN.R-project.org/package<-twang
#'    \item Bishop, C. D., Leite, W. L., & Snyder, P. A. (2018). Using propensity score weighting to reduce selection bias in large-scale data sets. \textit{Journal of Early Intervention, 40}(4), 347-362. https://doi.org/10.1177/1053815118793430
#'    \item Ho, D. E., Imai, K., King, G., & Stuart, E. A. (2011). MatchIt: Nonparametric preprocessing for parametric causal inference. \textit{Journal of Statistical Software, 42}(8), 1-28. 
#'    \item Leite, W. L., Jimenez, F., Kaya, Y., Stapleton, L. M., MacInnes, J. W., & Sandbach, R. (2015). An evaluation of weighting methods based on propensity scores to reduce selection bias in multilevel observational studies. \textit{Multivariate Behavioral Research, 50}(3), 265-284. https://doi.org/10.1080/00273171.2014.991018
#'    \item Rosenbaum, P. R., & Rubin, D. B. (1983). The central role of the propensity score in observational studies for causal effects. \textit{Biometrika, 70}, 41-55.
#' }
#' 

# Begin psw_wrapper function
psw_wrapper <- function(psformula, 
                        data, 
                        method = c("logistic", "gradient", "hierarchical"), 
                        estimand = c("ATT", "ATE"),
                        thresholds,
                        twangStop) {
  
  ### Data Cleaning
  
  # Create formula for R
  psf <- as.formula(psformula)
  
  # Specify grouping/treatment variable
  groupvar <- strsplit(psformula, " ")[[1]][1]
  
  # Specify covariates
  # https://stackoverflow.com/questions/21330633/get-the-right-hand-side-variables-of-an-r-formula
  covvar <- labels(terms(psf))
  
  # Allocate vector for potentially unbalanced covariates
  DoublyRobust <- "None"
  UnbalancedCov <- "None"
  
  ### Estimate Propensity Scores and Weights
  
  # Logistic regression
  if(method == "logistic") {
    
    # Estimate propensity scores
    ps.mod <- glm(as.formula(psformula), data = data, family = "binomial")
    ps.prob <- predict(ps.mod, newdata = data, type = "response")
    
    # Extract vector for grouping variable
    group <- data[, groupvar]
    
    # Estimate propensity score weights
    ps.weights <- compute_weights(pscores = ps.prob, group = group, estimand = estimand)
    
    # Estimate effect sizes with and without weighting
    estable <- matrix(NA, nrow = length(covvar), ncol = 2)
    for(iCov in 1:length(covvar)) {
      
      estable[iCov, ] <- compute_es(x = data[, covvar[iCov]],
                                    group = group,
                                    w = ps.weights,
                                    method = "logistic",
                                    estimand = estimand)

    }
    
    # Check covariates for doubly robust model
    numdoublyrobustcov <- which(abs(estable[,2]) > thresholds[1] & abs(estable[,2]) < thresholds[2])
    
    # Check if any covariates have weighted effect sizes greater than `threshold`
    numunbalanced <- sum(abs(estable[,2]) > thresholds[2])
    
    # If any unbalanced covariate terms, re-run with additional interaction terms
    if(numunbalanced > 0) {
      
      # Set interaction formula
      psformula.int <- paste(groupvar, paste0("(", paste(covvar, collapse = "+"), ")^2"), sep = "~")
      
      # Estimate propensity scores
      ps.mod <- glm(as.formula(psformula), data = data, family = "binomial")
      ps.prob <- predict(ps.mod, newdata = data, type = "response")

      # Estimate propensity score weights
      ps.weights <- compute_weights(pscores = ps.prob, group = group, estimand = estimand)
      
      # Estimate effect sizes with and without weighting
      estable <- matrix(NA, nrow = length(covvar), ncol = 2)
      for(iCov in 1:length(covvar)) {
        
        estable[iCov, ] <- compute_es(x = data[, covvar[iCov]],
                                      group = group,
                                      w = ps.weights,
                                      method = "logistic",
                                      estimand = estimand)
        
      }
      
      # Check covariates for doubly robust model
      numdoublyrobustcov <- which(abs(estable[,2]) > thresholds[1] & abs(estable[,2]) < thresholds[2])
      
      # Check if any covariates still unbalanced
      still.unbalanced <- which(abs(estable[,2]) > thresholds[2])
      UnbalancedCov <- if(length(still.unbalanced) > 0) paste(covvar[still.unbalanced], collapse = ", ") else UnbalancedCov
      
    } # end if unbalanced covariates after additive model
    
    # Update doubly robust character string
    DoublyRobust <- if(length(numdoublyrobustcov) > 0) paste(covvar[numdoublyrobustcov], collapse = ", ") else DoublyRobust
    
  } # end if method == "logistic"
  
  
  # Gradient boosting
  if(method == "gradient") {
    
    # Use while-loop to ensure that n.trees is large enough
    accept <- FALSE
    try.ntrees <- 10000; k <- 0
    while(!accept) {

      k <- k + 1
      try.ntrees <- try.ntrees + (k*10000)
      ps.mod <- tryCatch(expr = {

                   ps.mod <- ps(psf, data = data,
                                n.trees = try.ntrees, interaction.depth = 2,
                                shrinkage = 0.01, verbose = F,
                                estimand = estimand,
                                stop.method = twangStop)

               },
               warning = function(w) {

                   ps.mod <- "increase n.trees"

               })

      accept <- if(ps.mod == "increase n.trees") TRUE else FALSE

    }
    
    # Extract propensity scores
    ps.prob <- ps.mod$ps[,1]
    
    # Extract propensity score weights
    ps.weights <-  get.weights(ps.mod, stop.method = twangStop)
    
    # Estimate effect sizes with and without weighting
    es.baltable <- bal.table(ps.mod)
    estable <- cbind(es.baltable[[1]]$std.eff.sz, 
                     es.baltable[[2]]$std.eff.sz)
    
    # Check covariates for doubly robust model
    numdoublyrobustcov <- which(abs(estable[,2]) > thresholds[1] & abs(estable[,2]) < thresholds[2])
    DoublyRobust <- if(length(numdoublyrobustcov) > 0) paste(covvar[numdoublyrobustcov], collapse = ", ") else DoublyRobust
    
    # Check if any covariates unbalanced
    # `twang` automatically considers interactions and polynomial effects, so re-running
    # with a more complex model unnecessary
    numunbalanced <- sum(abs(estable[,2]) > thresholds[2])
    still.unbalanced <- which(abs(estable[,2]) > thresholds[2])
    UnbalancedCov <- if(numunbalanced > 0) covvar[still.unbalanced] else UnbalancedCov
    
  } # end if method == "gradient"
  
  
  # Multilevel modeling
  if(method == "hierarchical") {
    
    # Estimate propensity scores
    ps.mod <- glmer(as.formula(psformula), data = data, family = "binomial")
    ps.prob <- predict(ps.mod, newdata = data, type = "response")
    
    # Extract vector for grouping variable
    group <- data[, groupvar]
    
    # Remove random effects from covariate list
    covvar.fixed <- covvar[-grep("1 | ", covvar)]
    covvar.random <- paste0("(",  covvar[grep("1 | ", covvar)], ")")
    
    # Estimate propensity score weights
    ps.weights <- compute_weights(pscores = ps.prob, group = group, estimand = estimand)
    
    # Estimate effect sizes with and without weighting
    # Using full sample SD in denominator (not pooled; Leite et al., 2015)
    estable <- matrix(NA, nrow = length(covvar.fixed), ncol = 2)
    for(iCov in 1:length(covvar.fixed)) {
      
      estable[iCov, ] <- compute_es(x = data[, covvar.fixed[iCov]],
                                    group = group,
                                    w = ps.weights,
                                    method = "hierarchical",
                                    estimand = estimand)
      
    }
    
    # Check covariates for doubly robust model
    numdoublyrobustcov <- which(abs(estable[,2]) > thresholds[1] & abs(estable[,2]) < thresholds[2])
    
    # Check if any covariates have weighted effect sizes greater than `threshold`
    numunbalanced <- sum(abs(estable[,2]) > thresholds[2])
    
    # If any unbalanced covariate terms, re-run with additional interaction terms
    if(numunbalanced > 0) {
      
      # Set interaction formula
      psformula.int <- paste(groupvar, paste0("(", paste(covvar.fixed, collapse = "+"), ")^2", " + ", covvar.random), sep = "~")
      
      # Estimate propensity scores
      ps.mod <- glm(as.formula(psformula), data = data, family = "binomial")
      ps.prob <- predict(ps.mod, newdata = data, type = "response")
      
      # Estimate propensity score weights
      ps.weights <- compute_weights(pscores = ps.prob, group = group, estimand = estimand)
      
      # Estimate effect sizes with and without weighting
      estable <- matrix(NA, nrow = length(covvar.fixed), ncol = 2)
      for(iCov in 1:length(covvar.fixed)) {
        
        estable[iCov, ] <- compute_es(x = data[, covvar.fixed[iCov]],
                                      group = group,
                                      w = ps.weights,
                                      method = "logistic",
                                      estimand = estimand)
        
      }
      
      # Check covariates for doubly robust model
      numdoublyrobustcov <- which(abs(estable[,2]) > thresholds[1] & abs(estable[,2]) < thresholds[2])
      
      # Check if any covariates still unbalanced
      still.unbalanced <- which(abs(estable[,2]) > thresholds[2])
      UnbalancedCov <- if(length(still.unbalanced) > 0) paste(covvar.fixed[still.unbalanced], collapse = ", ") else UnbalancedCov
      
    } # end if unbalanced covariates after additive model
    
    # Update doubly robust character string
    DoublyRobust <- if(length(numdoublyrobustcov) > 0) paste(covvar[numdoublyrobustcov], collapse = ", ") else DoublyRobust
    
  } # end if method == "hierarchical"
  
  ### Warning if cannot find balance for one or more covariates
  
  # Add warning message
  if(UnbalancedCov != "None") {
    
    warning(paste0("NOTE: ", length(UnbalancedCov), " covariates had an effect size greater than ", thresholds[2], " after weighting."))
    
  }
  
  ### Clean Results
  
  # Add covariate names to effect size tables
  rownames(estable) <- if(method == "hierarchical") covvar.fixed else covvar
  colnames(estable) <- c("Unweighted", "Weighted")
  
  # Clean number of doubly robust covariates
  numdoublyrobustcov <- if(length(numdoublyrobustcov) == 0) 0 else length(numdoublyrobustcov)
  
  ### Return Values
  
  return(list(PS = ps.prob,
              PSWeights = ps.weights,
              ES = estable,
              DoublyRobust = DoublyRobust,
              NumDoublyRobust = numdoublyrobustcov,
              UnbalancedCov = UnbalancedCov,
              NumUnbalanced = numunbalanced,
              ModObject = ps.mod))
  
} # end psw_wrapper function
