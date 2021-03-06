gsci.ratio <- function (est, vcmat, Num.Contrast, Den.Contrast, degfree = NULL, 
          conf.level = 0.95, alternative = "two.sided", adjusted = TRUE) 
{
  alternative <- match.arg(alternative, choices=c("two.sided", "less", "greater"))
  vcmat <- as.matrix(vcmat)
  est <- matrix(est, ncol=1)
  
  if(length(est) != ncol(Num.Contrast) | length(est) != ncol(Den.Contrast) |  length(est) != ncol(vcmat)) {stop("The length of est and the column number of \n\tvcmat, Num.Contrast, and Den.Contrast\n\thave to be the same!")}
  if(!is.matrix(Den.Contrast)){Den.Contrast <- matrix(Den.Contrast, nrow=1); warning("Den.Contrast was coerced to a matrix with 1 row.")}
  if(!is.matrix(Num.Contrast)){Den.Contrast <- matrix(Num.Contrast, nrow=1); warning("Num.Contrast was coerced to a matrix with 1 row.")}
  if(nrow(Den.Contrast) != nrow(Num.Contrast)) {stop("Num.Contrast and Den.Contrast need the same row number!")}

  n.comp <- nrow(Num.Contrast)
  gammaC.vec <- (Num.Contrast %*% est)/(Den.Contrast %*% est)

   
  if(adjusted & n.comp >= 2){ADJ <- TRUE}else{ADJ <- FALSE}
  
  if(ADJ){
  CorrMat.plug <- matrix(rep(NA, n.comp * n.comp), nrow = n.comp)
  for (i in 1:n.comp) {
    for (j in 1:n.comp) {
      gdni <- (gammaC.vec[i] * Den.Contrast[i,] - Num.Contrast[i,])
      gdnj <- (gammaC.vec[j] * Den.Contrast[j,] - Num.Contrast[j,])
      CorrMat.plug[i, j] <- (gdni %*% vcmat %*% gdnj)/(sqrt(gdni %*% vcmat %*% gdni)*sqrt(gdnj %*% vcmat %*% gdnj)) 
    }
  }}

  Quad.root <- function(Aj, Bj, Cj, alternative) {
   
    Discrimi <- Bj^2 - 4 * Aj * Cj
    
    switch(alternative,
           "two.sided"={
     if ((Aj > 0) & (Discrimi >= 0)) {
      lower <- (-Bj - sqrt(Discrimi))/(2 * Aj)
      upper <- (-Bj + sqrt(Discrimi))/(2 * Aj)
      Limit.s <- c(lower, upper)
    }
    else{ Limit.s <- c(NA, NA)}},
    
    "less"={
      if ((Aj > 0) & (Discrimi >= 0)) {
        upper <- (-Bj + sqrt(Discrimi))/(2 * Aj)
        Limit.s <- c(upper)
      }
      else{ Limit.s <- c(NA)}},
    "greater"={
      if ((Aj > 0) & (Discrimi >= 0)) {
        lower <- (-Bj - sqrt(Discrimi))/(2 * Aj)
        Limit.s <- c(lower)
      }
      else{ Limit.s <- c(NA)}})
    
    return(Limit.s)
  }
  
  if (alternative == "two.sided") {
    side <- 2
    if (ADJ) {
      if (is.null(degfree)) {
        Cplug <- qmvnorm(conf.level, interval = c(0, 10), corr = CorrMat.plug, mean = rep(0, n.comp), 
                         tail = "both.tails")$quantile
      }
      else {
        Cplug <- qmvt(conf.level, interval = c(0, 10), 
                      df = as.integer(degfree), corr = CorrMat.plug, 
                      delta = rep(0, n.comp), tail = "both.tails")$quantile
      }
    }
    else {
      if (is.null(degfree)) {
        Cplug <- qnorm(1 - ((1 - conf.level)/2))
      }
      else {
        Cplug <- qt(1 - ((1 - conf.level)/2), df = degfree)
      }
    }
  }
  if ((alternative == "less") | (alternative == "greater")) {
    side <- 1

    if (ADJ) {
      if (is.null(degfree)) {
        Cplug <- qmvnorm(conf.level, interval = c(0, 10), corr = CorrMat.plug, mean = rep(0, n.comp), 
                         tail = "lower.tail")$quantile
      }
      else {
        Cplug <- qmvt(conf.level, interval = c(0, 10), 
                      df = as.integer(degfree), corr = CorrMat.plug, 
                      delta = rep(0, n.comp), tail = "lower.tail")$quantile
      }
    }
    else {
      if (is.null(degfree)) {
        Cplug <- qnorm(conf.level)
      }
      else {
        Cplug <- qt(conf.level, df = degfree)
      }
    }
  }
  quant <- Cplug
  PlugCL <- matrix(rep(NA, side * n.comp), nrow = n.comp)
  for (j in 1:n.comp) {
    AjPlug <- (Den.Contrast[j, ] %*% est)^2 - (Cplug^2) *  Den.Contrast[j, ] %*% vcmat %*% Den.Contrast[j, ]
    BjPlug <- -2 * ((Num.Contrast[j, ] %*% est) * (Den.Contrast[j, ] %*% est) - (Cplug^2) * Num.Contrast[j, ] %*% vcmat %*%  Den.Contrast[j, ])
    CjPlug <- (Num.Contrast[j, ] %*% est)^2 - (Cplug^2) *  Num.Contrast[j, ] %*% vcmat %*% Num.Contrast[j, ]
    PlugCL[j, ] <- Quad.root(AjPlug[1, 1], BjPlug[1, 1], CjPlug[1, 1], alternative=alternative)
  }
  sci.table <- data.frame(PlugCL)
  SCIs <- cbind(gammaC.vec, PlugCL)
  if (alternative == "two.sided") 
    colnames(SCIs) <- c("estimate", "lower", "upper")
  if (alternative == "less") 
    colnames(SCIs) <- c("estimate", "upper")
  if (alternative == "greater") 
    colnames(SCIs) <- c("estimate", "lower")
  CI <- matrix(SCIs[, -1], nrow = nrow(Num.Contrast))
  colnames(CI) <- colnames(SCIs)[-1]
  estimate <- cbind(estimate = SCIs[, 1])
  if (all(!is.null(rownames(Num.Contrast)))) {
    rownames(CI) <- rownames(Num.Contrast)
    rownames(estimate) <- rownames(Num.Contrast)
  }
  
  if(ADJ){ METH <- paste("Simultaneous ", signif(conf.level * 100, 4), "% confidence intervals", sep = "")} else {METH <- paste("Unadjusted ", signif(conf.level * 100, 4), "% confidence intervals",  sep = "")}
  
  out <- list()
  out$estimate <- estimate
  if(ADJ){out$CorrMat.est <- CorrMat.plug}else{out$CorrMat.est <- NULL}
  out$Num.Contrast <- Num.Contrast
  out$Den.Contrast <- Den.Contrast
  out$conf.int <- CI
  out$NSD <- any(is.na(SCIs))
  out$alternative <- alternative
  out$conf.level <- conf.level
  out$compnames <- rownames(SCIs)
  out$methodname <- METH
  out$method <- if(ADJ){"Plug"}else{"Unadj"}
  out$type <- "User defined"
  out$df <- degfree
  out$quantile <- quant
  class(out) <- c("sci.ratio", "gsci.ratio")
  return(out)
}
