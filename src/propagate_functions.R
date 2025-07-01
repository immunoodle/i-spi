## https://github.com/anspiess/propagate Propagate was removed from CRAN.

propagate <- function(
    expr,
    data,
    second.order = TRUE,
    do.sim = TRUE,
    cov = TRUE,
    df = NULL,
    nsim = 1000000,
    alpha = 0.05,
    ...
)
{
  op <- options(warn = -1)
  on.exit(options(op))

  ## version 1.0-4: convert function to expression
  if (is.function(expr)) {
    ARGS <- as.list(args(expr))
    ARGS <- ARGS[-length(ARGS)]
    VARS <- names(ARGS)
    expr <- body(expr)
    class(expr) <- "expression"
    isFun <- TRUE
  } else isFun <- FALSE

  ## check for correct expression and number of simulations
  if (!is.expression(expr)) stop("propagate: 'expr' must be an expression")
  if (nsim < 10000) stop("propagate: 'nsim' should be >= 10000 !")

  ## check for matching variable names
  if (!isFun) VARS <- all.vars(expr)
  m <- match(VARS, colnames(data))

  if (any(is.na(m))) stop("propagate: variable names of input dataframe and expression do not match!")
  if (length(unique(m)) != length(m)) stop("propagate: some variable names are repetitive!")

  DATA <- data
  EXPR <- expr

  ## create variables from input data
  ## version 1.0-5: check for simulated data => large nrow(data)
  if (nrow(data) > 3) {
    meanVAL <- apply(DATA, 2, function(x) mean(x, na.rm = TRUE))
    sdVAL <- apply(DATA, 2, function(x) sd(x, na.rm = TRUE))
    dfVAL <- NULL
    isRaw <- TRUE
  } else {
    meanVAL <- DATA[1, ]
    sdVAL <- DATA[2, ]
    dfVAL <- if (nrow(DATA) == 3) DATA[3, ] else NULL
    isRaw <- FALSE
  }

  ## stat data: if no covariance matrix is supplied, create one with diagonal variances
  if (is.logical(cov) & !isRaw) {
    SIGMA <- diag(sdVAL^2, nrow = length(VARS), ncol = length(VARS))
    colnames(SIGMA) <- rownames(SIGMA) <- colnames(DATA)
  }

  ## raw data: if no covariance matrix is supplied, create one with off-diagonals or not
  if (is.logical(cov) & isTRUE(cov) & isRaw) {
    SIGMA <- cov(data)
  }
  if (is.logical(cov) & isFALSE(cov) & isRaw) {
    SIGMA <- cov(data)
    SIGMA[upper.tri(SIGMA)] <- SIGMA[lower.tri(SIGMA)] <- 0
  }

  ## if covariance matrix is supplied, check for symmetry and matching names
  if (is.matrix(cov)) {
    if (NCOL(cov) != NROW(cov)) stop("propagate: 'cov' is not a symmetric matrix!")
    m <- match(colnames(cov), colnames(DATA))
    if (any(is.na(m))) stop("propagate: names of input dataframe and var-cov matrix do not match!")
    if (length(unique(m)) != length(m)) stop("propagate: some names of the var-cov matrix are repetitive!")
    SIGMA <- cov
  }

  ## version 1.0-5: replace possible NA's in covariance matrix with 0's
  SIGMA[is.na(SIGMA)] <- 0

  ## version 1.0-5: No diagonals with 0,
  ## otherwise tmvtnorm:::checkSymmetricPositiveDefinite throws an error!
  if (any(diag(SIGMA) == 0)) {
    DIAG <- diag(SIGMA)
    DIAG[DIAG == 0] <- 2E-16
    diag(SIGMA) <- DIAG
  }

  ## This will bring the variables in 'data' and 'expr' in the same
  ## order as in the covariance matrix
  m1 <- match(colnames(SIGMA), colnames(DATA))
  meanVAL <- meanVAL[m1]
  m2 <- match(colnames(SIGMA), VARS)

  ############ first- and second-order Taylor expansion-based error propagation ################
  ## first-order mean: eval(EXPR)
  ## version 1.0-4: continue with NA's when differentiation not possible
  MEAN1 <- try(eval(EXPR, envir = as.list(meanVAL)), silent = TRUE)
  if (!is.numeric(MEAN1)) {
    message("propagate: there was an error in calculating the first-order mean")
    MEAN1 <- NA
  }

  ## evaluate gradient vector
  GRAD <- try(makeGrad(EXPR, m2), silent = TRUE)
  if (!inherits(GRAD, "try-error")) evalGRAD <- try(sapply(GRAD, eval, envir = as.list(meanVAL)), silent = TRUE)
  if (inherits(GRAD, "try-error")) evalGRAD <- try(numGrad(EXPR, as.list(meanVAL)), silent = TRUE)
  if (!inherits(evalGRAD, "try-error")) evalGRAD <- as.vector(evalGRAD) else evalGRAD <- NA

  ## first-order variance: g.S.t(g)
  VAR1 <- try(as.numeric(t(evalGRAD) %*% SIGMA %*% matrix(evalGRAD)), silent = TRUE)
  if (inherits(VAR1, "try-error")) {
    message("propagate: there was an error in calculating the first-order variance")
    VAR1 <- NA
  }

  ## second-order mean: firstMEAN + 0.5 * tr(H.S)
  if (second.order) {
    HESS <- try(makeHess(EXPR, m2), silent = TRUE)
    if (!inherits(HESS, "try-error")) evalHESS <- try(sapply(HESS, eval, envir = as.list(meanVAL)), silent = TRUE)
    if (inherits(HESS, "try-error")) evalHESS <- try(numHess(EXPR, as.list(meanVAL)), silent = TRUE)
    if (!inherits(evalHESS, "try-error")) evalHESS <- matrix(evalHESS, ncol = length(meanVAL), byrow = TRUE) else evalHESS <- NA

    valMEAN2 <- try(0.5 * tr(evalHESS %*% SIGMA), silent = TRUE)
    if (!inherits(valMEAN2, "try-error")) {
      MEAN2 <- MEAN1 + valMEAN2
    } else {
      message("propagate: there was an error in calculating the second-order mean")
      MEAN2 <- NA
    }

    ## second-order variance: firstVAR + 0.5 * tr(H.S.H.S)
    valVAR2 <- try(0.5 * tr(evalHESS %*% SIGMA %*% evalHESS %*% SIGMA), silent = TRUE)
    if (!inherits(valVAR2, "try-error")) {
      VAR2 <- VAR1 + valVAR2
    } else {
      message("propagate: there was an error in calculating the second-order variance")
      VAR2 <- NA
    }
  } else MEAN2 <- VAR2 <- HESS <- evalHESS <- NA

  ## total mean and variance
  if (second.order) totalVAR <- VAR2 else totalVAR <- VAR1
  if (second.order) totalMEAN <- MEAN2 else totalMEAN <- MEAN1
  errorPROP <- sqrt(totalVAR)

  ## sensitivity index/contribution/relative contribution
  if (is.numeric(evalGRAD)) {
    sensitivity <- evalGRAD
    contribution <- outer(sensitivity, sensitivity, "*") * SIGMA
    rel.contribution <- abs(contribution)/sum(abs(contribution), na.rm = TRUE)
  } else sensitivity <- contribution <- rel.contribution <- NA

  ## WS degrees of freedom, coverage factor and expanded uncertainty
  if (!is.null(dfVAL)) dfVAL[is.na(dfVAL)] <- 1E6
  if (is.null(dfVAL)) dfVAL <- rep(1E6, ncol(DATA))
  ws <- WelchSatter(ui = sqrt(diag(SIGMA)), ci = sensitivity, df = dfVAL, dftot = df, uc = errorPROP, alpha = alpha)

  ## confidence interval based on either first- or second-order mean
  if (is.na(MEAN2)) confMEAN <- MEAN1 else confMEAN <- MEAN2
  confPROP <- confMEAN + c(-1, 1) * ws$u.exp
  names(confPROP) <- paste(c(alpha/2, 1 - alpha/2) * 100, "%", sep = "")

  ################## Monte-Carlo simulation using multivariate t-distribution #####################
  if (do.sim) {
    if (is.na(ws$ws.df) | is.infinite(ws$ws.df)) DF <- 1E6 else DF <- ws$ws.df
    if (is.numeric(df)) DF <- df

    ## if raw data, don't create Monte Carlo data
    if (!isRaw) {
      datSIM <- rtmvt(nsim, mean = meanVAL, sigma = SIGMA, df = floor(DF))
      colnames(datSIM) <- colnames(DATA)
    } else datSIM <- DATA

    ## try vectorized evaluation, which is much faster
    resSIM <- try(eval(EXPR, as.data.frame(datSIM)), silent = TRUE)

    ## use 'row-wise' method if 'vectorized' throws an error
    if (inherits(resSIM, "try-error")) {
      message("propagate: using 'vectorized' evaluation gave an error. Switching to 'row-wise' evaluation...")
      resSIM <- apply(datSIM, 1, function(x) eval(EXPR, envir = as.list(x)))
    }

    ## alpha-based confidence interval of MC simulations
    confSIM <- quantile(resSIM, c(alpha/2, 1 - (alpha/2)), na.rm = TRUE)

    ## warning in case of single evaluated result
    if (length(unique(resSIM)) == 1) message("propagate: Monte Carlo simulation gave unique repetitive values! Are all derivatives constants?")
  } else resSIM <- datSIM <- confSIM <- allSIM <- NA


  outPROP <- c(Mean.1 = MEAN1, Mean.2 = MEAN2, sd.1 = sqrt(VAR1), sd.2 = sqrt(VAR2),
               confPROP[1], confPROP[2])

  outSIM <- c(Mean = mean(resSIM, na.rm = TRUE), sd = sd(resSIM, na.rm = TRUE),
              Median = median(resSIM, na.rm = TRUE), MAD = mad(resSIM, na.rm = TRUE),
              confSIM[1], confSIM[2])

  OUT <- list(gradient = GRAD, evalGrad = evalGRAD,
              hessian = HESS, evalHess = evalHESS,
              rel.contr  = rel.contribution, covMat = SIGMA, ws.df = floor(ws$ws.df),
              k = ws$k, u.exp = ws$u.exp, resSIM = resSIM, datSIM = datSIM,
              prop = outPROP, sim = outSIM, expr = EXPR, data = DATA, alpha = alpha)

  class(OUT) <- "propagate"
  return(OUT)
}

WelchSatter <- function(ui, ci = NULL, df = NULL, dftot = NULL, uc = NULL, alpha = 0.05)
{
  if (is.null(ci)) ci <- rep(1, length(ui))
  if (is.null(df)) stop("WelchSatter: Please supply 'df's!")
  if (length(ui) != length(df)) stop("WelchSatter: Different number of values in 'ui' and 'df'!")
  if (!is.null(dftot) & length(dftot) != 1) stop("WelchSatter: Total degrees of freedom must be a single number!")
  if (is.null(uc)) uc <- sqrt(sum((ci * ui)^2))
  ws.df <- (uc^4)/sum(((ci * ui)^4)/df)
  if (is.numeric(dftot)) ws.df <- dftot
  k <- qt(1 - alpha/2, floor(ws.df))
  u.exp <- k * uc
  return(list(ws.df = ws.df, k = k, u.exp = u.exp))
}

predictNLS <- function(
    model,
    newdata,
    newerror,
    interval = c("confidence", "prediction", "none"),
    alpha = 0.05,
    ...
)
{
  interval <- match.arg(interval)

  ## get right-hand side of formula
  RHS <- as.list(eval(model$call$formula))[[3]]
  EXPR <- as.expression(RHS)

  ## all variables in model
  VARS <- all.vars(EXPR)

  ## coefficients
  COEF <- coef(model)

  ## extract predictor variable
  predVAR <- setdiff(VARS, names(COEF))

  ## version 1.0-6: take original predictor values, if 'newdata' is missing,
  ## as in classical 'predict.nls'
  if (missing(newdata)) {
    newdata <- try(eval(model$data)[, predVAR, drop = FALSE], silent = TRUE)
    if (inherits(newdata, "try-error")) {
      newdata <- sapply(predVAR, function(i) get(i))
      names(newdata) <- predVAR
    }
  }

  ## check that 'newdata' and 'newerror' has same name as predVAR
  if (length(setdiff(colnames(newdata), predVAR)) != 0) stop("predictNLS: 'newdata' should have column name(s): ", predVAR, "!\n")
  if (!missing(newerror)) {
    if (length(setdiff(colnames(newerror), predVAR)) != 0) stop("predictNLS: 'newerror' should have column name(s): ", predVAR, "!\n")
  }

  ## get variance-covariance matrix
  VCOV <- vcov(model)

  ## create and check 'newerror' for same dimension as 'newdata'
  if (missing(newerror)) {
    newerror <- newdata
    newerror[] <- 0
  } else {
    if (!identical(dim(newdata), dim (newerror))) stop("'newdata' and 'newerror' should have the same dimensions!")
    if (!identical(colnames(newdata), colnames(newerror))) stop("'newdata' and 'newerror' should have the same column names!")
  }

  ## iterate over all entries in 'newdata' and 'newerror' as in usual 'predict.' functions
  NR <- NROW(newdata)
  outMAT <- matrix(nrow = NR, ncol = 12)
  propLIST <- vector("list", length = NR)

  ## version 1.06: if interval = "prediction" add "eps" variable for residual standard error
  if (interval == "prediction") {
    form <- formula(model)
    form <- as.formula(paste(form[2], form[1], paste(form[3], " + rv", sep = ""), sep = " "))
    EXPR <- as.expression(form[[3]])
  }

  for (i in 1:NR) {
    message("predictNLS: ", paste0("Propagating predictor value #", i, "..."))
    tempDATA <- newdata[i, , drop = FALSE]
    errorDATA <- newerror[i, , drop = FALSE]

    ## create dataframe of variables for 'propagate'
    ## version 1.0-6: if interval = "prediction", add zero mean for residual variance
    DAT <- as.numeric(c(COEF, tempDATA))
    names(DAT) <- c(names(COEF), predVAR)
    DAT <- rbind(DAT, 0)
    row.names(DAT) <- NULL
    if (interval == "prediction") DAT <- cbind(DAT, rv = c(0, 0))

    ## create (augmented) covariance matrix
    if (interval == "confidence") COV <- mixCov(VCOV, errorDATA^2)
    else {
      r <- residuals(model)
      w <- weights(model)
      rss <- sum(if (is.null(w)) r^2 else r^2 * w)
      n <- length(residuals(model))
      p <- length(coef(model))
      rv <- rss/(n - p)
      COV <- mixCov(VCOV, errorDATA^2, rv)
    }

    ## version 1.06: include degrees of freedom
    DF <- df.residual(model)

    # DAT_v <<- DAT
    # cov_v <<- COV
    if( all(colnames(DAT) == colnames(COV)) == FALSE) {
      colnames(COV) <- colnames(DAT)
    }
    ## call 'propagate'
    PROP <- propagate(expr = EXPR, data = DAT, cov = COV, alpha = alpha, df = DF, ...)
    propLIST[[i]] <- PROP

    ## populate outMAT
    outPROP <- PROP$prop
    outSIM <- PROP$sim
    OUT <- c(outPROP, outSIM)
    outMAT[i, ] <- OUT
  }

  outMAT <- as.data.frame(outMAT)
  colnames(outMAT) <- c(paste("Prop.", names(outPROP), sep = ""),
                        paste("Sim.", names(outSIM), sep = ""))
  return(list(summary = outMAT, prop = propLIST))
}

########## visible ########################
makeGrad <- function(expr, order = NULL)
{
  VARS <- all.vars(expr)
  if (!is.null(order)) VARS <- VARS[order]
  FUN <- function(x) D(expr, x)
  vecGRAD <- sapply(VARS, FUN)
  vecGRAD <- matrix(vecGRAD, nrow = 1)
  return(vecGRAD)
}

makeHess <- function(expr, order = NULL)
{
  VARS <- all.vars(expr)
  if (!is.null(order)) VARS <- VARS[order]
  GRID <- expand.grid(VARS, VARS)
  FUN <- function(x) D(D(expr, x[1]), x[2])
  vecHESS <- apply(GRID, 1, FUN)
  matHESS <- matrix(vecHESS, ncol = length(VARS), byrow = TRUE)
  return(matHESS)
}

evalDerivs <- function(deriv, envir)
{
  if (missing(envir)) envir <- .GlobalEnv
  DIM <- dim(deriv)
  evalVEC <- sapply(deriv, eval, envir = envir)
  dim(evalVEC) <- DIM
  return(evalVEC)
}

kurtosis <- function (x, na.rm = FALSE)
{
  if (is.matrix(x))
    apply(x, 2, kurtosis, na.rm = na.rm)
  else if (is.vector(x)) {
    if (na.rm)
      x <- x[!is.na(x)]
    n <- length(x)
    n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2) - 3
  }
  else if (is.data.frame(x))
    sapply(x, kurtosis, na.rm = na.rm)
  else kurtosis(as.vector(x), na.rm = na.rm)
}

skewness <- function (x, na.rm = FALSE)
{
  if (is.matrix(x))
    apply(x, 2, skewness, na.rm = na.rm)
  else if (is.vector(x)) {
    if (na.rm)
      x <- x[!is.na(x)]
    n <- length(x)
    (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
  }
  else if (is.data.frame(x))
    sapply(x, skewness, na.rm = na.rm)
  else skewness(as.vector(x), na.rm = na.rm)
}

counter <- function (i)
{
  if (i%%10 == 0)
    cat(i)
  else cat(".")
  if (i%%50 == 0)
    cat("\n")
  flush.console()
}

tr <- function(mat) sum(diag(mat), na.rm = TRUE)

rescale <- function (x, tomin, tomax)
{
  if (missing(x) | missing(tomin) | missing(tomax)) {
    stop(paste("rescale: rescale(x, tomin, tomax)\n", "\twhere x is a numeric object and tomin and tomax\n is the range to rescale into",
               sep = "", collapse = ""))
  }
  if (is.numeric(x) && is.numeric(tomin) && is.numeric(tomax)) {
    xrange <- range(x, na.rm = TRUE)
    if (xrange[1] == xrange[2])
      return(x)
    mfac <- (tomax - tomin)/(xrange[2] - xrange[1])
    return(tomin + (x - xrange[1]) * mfac)
  }
  else {
    warning("rescale: only numeric objects can be rescaled")
    return(x)
  }
}

print.interval <- function(x, ...)
{
  cat("[", x[1], ", ", x[2], "]", sep = "")
}

print.propagate <- function(x, ...)
{
  object <- x

  ## print error propagation results
  message("Results from uncertainty propagation:")
  print(object$prop)

  ## print simulation results
  if (length(x$resSIM) > 1) {
    message("Results from Monte Carlo simulation:")
    print(object$sim)
  }
}

print.fitDistr <- function(x, ...)
{
  message("Best fit is ", names(x$fit)[[1]], " Distribution.")
  message("Parameters:")
  print(x$par[[1]])
  message("Standard errors:")
  print(x$se[[1]])
  message("Goodness of fit:")
  cat("BIC =", x$stat[1, "BIC"])
}

isFALSE <- function (x) is.logical(x) && length(x) == 1L && !is.na(x) && !x
isTRUE <- function (x) is.logical(x) && length(x) == 1L && !is.na(x) && x

mixCov <- function(...)
{
  ## convert arguments to named list
  covLIST <- list(...)

  ## extract function call names
  allVars <- all.vars(sys.call(0))

  ## get dimensions
  dimLIST <- lapply(covLIST, function(x) NCOL(x))

  ## get columns names
  nameVec <- NULL

  ## create final covariance matrix
  DIMS <- do.call("sum", dimLIST)
  covMAT <- matrix(NA, nrow = sum(DIMS), ncol = sum(DIMS))

  ## populate matrix and augment name vector
  counter <- 1
  for (i in 1:length(covLIST)) {
    if (!is.null(colnames(covLIST[[i]]))) nameVec <- c(nameVec, colnames(covLIST[[i]]))
    else nameVec <- c(nameVec, allVars[i])
    POS <- counter:(counter + dimLIST[[i]] - 1)
    INS <- covLIST[[i]]
    if (NCOL(INS) > NROW(INS)) INS <- diag(as.numeric(INS)) ## check if vector or covariance matrix
    if (NCOL(INS) == 1) INS <- as.numeric(INS)
    covMAT[POS, POS] <- INS
    counter <- counter + dimLIST[[i]]
  }

  ## fill with 0's
  covMAT[is.na(covMAT)] <- 0

  ## create colnames/rownames
  rownames(covMAT) <- colnames(covMAT) <- nameVec

  return(covMAT)
}
