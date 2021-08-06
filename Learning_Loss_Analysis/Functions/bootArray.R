bootArray <- function (
    data,
    statistic,
    R,
    sim = "ordinary",
    stype = c("i", "f", "w"),
    strata = rep(1, n),
    L = NULL,
    m = 0,
    weights = NULL,
    ran.gen = function(d, p) d,
    mle = NULL,
    simple = FALSE,
    ...,
    parallel = c("no", "multicore", "snow"),
    ncpus = getOption("boot.ncpus", 1L), cl = NULL)
  {
    call <- match.call()

    stype <- match.arg(stype)

    ###  AVI - utilities from boot
    isMatrix <- function (x) length(dim(x)) == 2L

    index.array <- function (n, R, sim, strata = rep(1, n), m = 0, L = NULL, weights = NULL) {
      indices <- NULL
      if (is.null(weights)) {
        if (sim == "ordinary") {
          indices <- ordinary.array(n, R, strata)
          if (sum(m) > 0)
            indices <- cbind(indices, extra.array(n, R, m, strata))
        }
        else if (sim == "balanced")
          indices <- balanced.array(n, R, strata)
        else if (sim == "antithetic")
          indices <- antithetic.array(n, R, L, strata)
        else if (sim == "permutation")
          indices <- permutation.array(n, R, strata)
      }
      else {
        if (sim == "ordinary")
          indices <- importance.array(n, R, weights, strata)
        else if (sim == "balanced")
          indices <- importance.array.bal(n, R, weights, strata)
      }
      indices
    }

    ordinary.array <- function (n, R, strata) {
      inds <- as.integer(names(table(strata)))
      if (length(inds) == 1L) {
        output <- sample.int(n, n * R, replace = TRUE)
        dim(output) <- c(R, n)
      }
      else {
        output <- matrix(as.integer(0L), R, n)
        for (is in inds) {
          gp <- seq_len(n)[strata == is]
          output[, gp] <- if (length(gp) == 1)
            rep(gp, R)
          else bsample(gp, R * length(gp))
        }
      }
      output
    }

    boot.return <- function (sim, t0, t, strata, R, data, stat, stype, call, seed, L, m, pred.i, weights, ran.gen, mle) {
      out <- list(t0 = t0, t = t, R = R, # data = data,  ##  AVI - Don't return data!
        seed = seed, statistic = stat, sim = sim, call = call)
      if (sim == "parametric")
        out <- c(out, list(ran.gen = ran.gen, mle = mle))
      else if (sim == "antithetic")
        out <- c(out, list(stype = stype, strata = strata, L = L))
      else if (sim == "ordinary") {
        if (sum(m) > 0)
          out <- c(out, list(stype = stype, strata = strata, weights = weights, pred.i = pred.i))
        else out <- c(out, list(stype = stype, strata = strata, weights = weights))
      }
      else if (sim == "balanced")
        out <- c(out, list(stype = stype, strata = strata, weights = weights))
      else out <- c(out, list(stype = stype, strata = strata))
      class(out) <- "boot"
      out
    }

    bsample <- function (x, ...) x[sample.int(length(x), replace = TRUE, ...)]

    ###  END - utilities from boot


    if (missing(parallel))
        parallel <- getOption("boot.parallel", "no")
    parallel <- match.arg(parallel)
    have_mc <- have_snow <- FALSE
    if (parallel != "no" && ncpus > 1L) {
        if (parallel == "multicore")
            have_mc <- .Platform$OS.type != "windows"
        else if (parallel == "snow")
            have_snow <- TRUE
        if (!have_mc && !have_snow)
            ncpus <- 1L
        loadNamespace("parallel")
    }
    if (simple && (sim != "ordinary" || stype != "i" || sum(m))) {
        warning("'simple=TRUE' is only valid for 'sim=\"ordinary\", stype=\"i\", n=0', so ignored")
        simple <- FALSE
    }
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        runif(1)
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
     n <- NROW(data)
     if ((n == 0) || is.null(n))
         stop("no data in call to 'boot'")
     temp.str <- strata
     strata <- tapply(seq_len(n), as.numeric(strata))
     t0 <- if (sim != "parametric") {
         if ((sim == "antithetic") && is.null(L))
             L <- empinf(data = data, statistic = statistic, stype = stype,
                 strata = strata, ...)
         if (sim != "ordinary")
             m <- 0
         else if (any(m < 0))
             stop("negative value of 'm' supplied")
         if ((length(m) != 1L) && (length(m) != length(table(strata))))
             stop("length of 'm' incompatible with 'strata'")
         if ((sim == "ordinary") || (sim == "balanced")) {
             if (isMatrix(weights) && (nrow(weights) != length(R)))
                 stop("dimensions of 'R' and 'weights' do not match")
         }
         else weights <- NULL
         if (!is.null(weights))
             weights <- t(apply(matrix(weights, n, length(R),
                 byrow = TRUE), 2L, normalize, strata))
         if (!simple)
             i <- index.array(n, R, sim, strata, m, L, weights)
         original <- if (stype == "f")
             rep(1, n)
         else if (stype == "w") {
             ns <- tabulate(strata)[strata]
             1/ns
         }
         else seq_len(n)
         t0 <- if (sum(m) > 0L)
             statistic(data, original, rep(1, sum(m)), ...)
         else statistic(data, original, ...)
         rm(original)
         t0
     }
     else statistic(data, ...)

     pred.i <- NULL
     fn <- if (sim == "parametric") {
         ran.gen
         data
         mle
         function(r) {
             dd <- ran.gen(data, mle)
             statistic(dd, ...)
         }
     }
  else {
    if (!simple && ncol(i) > n) {
        pred.i <- as.matrix(i[, (n + 1L):ncol(i)])
        i <- i[, seq_len(n)]
    }
    if (stype %in% c("f", "w")) {
        f <- freq.array(i)
        rm(i)
        if (stype == "w")

            f <- f/ns
        if (sum(m) == 0L)
            function(r) statistic(data, f[r, ], ...)
        else function(r) statistic(data, f[r, ], pred.i[r,
            ], ...)
    }
    else if (sum(m) > 0L)
        function(r) statistic(data, i[r, ], pred.i[r, ],
            ...)
    else if (simple)
        function(r) statistic(data, index.array(n, 1, sim,
            strata, m, L, weights), ...)
    else function(r) statistic(data, i[r, ], ...)
}
RR <- sum(R)
res <- if (ncpus > 1L && (have_mc || have_snow)) {
    if (have_mc) {
        parallel::mclapply(seq_len(RR), fn, mc.cores = ncpus)
    }
    else if (have_snow) {
        list(...)
        if (is.null(cl)) {
            cl <- parallel::makePSOCKcluster(rep("localhost",
              ncpus))
            if (RNGkind()[1L] == "L'Ecuyer-CMRG")

              parallel::clusterSetRNGStream(cl)
            res <- parallel::parLapply(cl, seq_len(RR), fn)
            parallel::stopCluster(cl)
            res
        }
        else parallel::parLapply(cl, seq_len(RR), fn)
    }
} else lapply(seq_len(RR), fn)
# AVI - format results in an array instead of matrix to allow for table-like results
# t.star <- matrix(, RR, length(t0))
# for (r in seq_len(RR)) t.star[r, ] <- res[[r]]
t.star <- array(, dim = RR)
for (r in seq_len(RR)) t.star[r] <- res[r]
if (is.null(weights))
    weights <- 1/tabulate(strata)[strata]
boot0 <- boot.return(sim, t0, t.star, temp.str, R, data,
    statistic, stype, call, seed, L, m, pred.i, weights,
    ran.gen, mle)
attr(boot0, "boot_type") <- "boot"
# boot0$data <- NULL  ##  AVI - Don't return data!
boot0
}


###   Bias Corrected (Accelerated) Percentile Confidence Intervals (from `coxed` package with edits/improvments)
BCa <-  function (theta, conf.level = 0.95, accelerate = TRUE) {
    theta <- na.omit(theta)
    if (length(theta) == 0) return(as.numeric(c(NA, NA)))
    low <- (1 - conf.level)/2
    high <- 1 - low
    sims <- length(theta)
    z.inv <- length(theta[theta < mean(theta)])/sims
    z <- qnorm(z.inv)

    if (accelerate) {
      U <- (sims - 1) * (mean(theta, na.rm = TRUE) - theta)
      top <- sum(U^3)
      under <- 6 * (sum(U^2))^{3/2}
      a <- top/under
    } else a <- 0

    lower.inv <- pnorm(z + (z + qnorm(low))/(1 - a * (z + qnorm(low))))
    lower <- quantile(theta, lower.inv, names = FALSE, na.rm = TRUE)
    upper.inv <- pnorm(z + (z + qnorm(high))/(1 - a * (z + qnorm(high))))
    upper <- quantile(theta, upper.inv, names = FALSE, na.rm = TRUE)
    return(c(lower, upper))
}
