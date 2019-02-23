###########################
#Lambda Estimator for lme4#
#-------------------------#
# Produced by Ian Douglas #
#-------------------------#
###########################
lambda <- function(lmerMod) {
  if (as.character(class(lmerMod)[1]) != "lmerMod") {
    stop("first argument must be a lmerMod object or formula")
  } else {
    if (as.character(lmerMod@call[4]) == TRUE) {
      sigma <- as.numeric(unclass(lmerMod@devcomp)$cmp["sigmaREML"])
    } else {
      sigma <- as.numeric(unclass(lmerMod@devcomp)$cmp["sigmaML"])
    }
    Tau.squared <- (unclass(summary(lmerMod)$varcor)$id)[1,1]
    nj <- c(rep(0, times = lmerMod@Gp[2]))
    for (i in 1:lmerMod@Gp[2]) {
      nj[i] <- sum(lmerMod@frame[,2] == (unique(lmerMod@frame[,2]))[i])
    }
    lambda.j <- (Tau.squared)/(Tau.squared + ((sigma^2)/nj))
    lambda <- mean(lambda.j)
    out <- c(Tau.squared, sigma^2, lambda)
    names(out) <- c("Tau^2", "sigma^2", "lambda")
    out
  }
}