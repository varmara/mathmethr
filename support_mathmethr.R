# supporting functions for mathmethr course

dt_limit <- function(x, alph = 0.05, df = 18, sides = 2, ncp = 0, what = "alpha") {
  #' Function to generate data for plotting with ggplot
  #' (non)central t distribution
  #' with shaded areas for alpha, beta and power
  #' Authors: Marina Varfolomeeva, Vadim Khaitov
  #' Usage inside stat_function:
  #' stat_function(fun = dt_limit,
  #'               args = list(alph = alpha, df = df, sides = sides),
  #'               geom = "area", fill = "red", alpha = 0.7)
  if(sides == 1) alph <- alph
  if(sides == 2) alph <- alph/2
  t_cr <- abs(qt(p = alph, df = df))

  if(what == "alpha"){
    y <- dt(x, df, ncp = ncp)
    y[!(x < -t_cr | x > t_cr)] <- NA
  }
  if(what == "beta"){
    y <- dt(x, df, ncp = ncp)
    y[!(x >= -t_cr & x <= t_cr)] <- NA
  }
  if(what == "power"){
    y <- dt(x, df, ncp = ncp)
    y[!(x < -t_cr | x > t_cr)] <- NA
  }
  return(y)
}

fix_aov_smr <- function(aov_obj, lang = "en", rown = NULL, coln = NULL){
  #' Function to assist with printing aov summary object with xtable
  #' Fixes p-values, allows to set rownames and colnames
  #' Author: Marina Varfolomeeva
  pvals <- aov_obj[[1]][, ncol(aov_obj[[1]])]
  pvals <- format.pval(pvals, eps = 0.01)
  pvals[pvals == "NA"] <- NA
  aov_obj[[1]][, ncol(aov_obj[[1]])] <- pvals
  aov_obj[[1]][, 1] <- format(aov_obj[[1]][, 1], digits = 0)
  if(!is.null(rown)){
    rownames(aov_obj[[1]]) <- rown
  }
  if(!is.null(coln)){
    colnames(aov_obj[[1]]) <- coln
  }
  return(aov_obj)
}

lm_equation <- function(fit, strict = TRUE, digits = 2, trim = TRUE){
  #' Function to produce equation of linear model
  #' given the model object. Intended for use with lm
  #' Author: Marina Varfolomeeva
  #' Arguments:
  #' fit - lm model object
  #' strict - shoud the variable names be replaced with Xn
  #'
  #   extracting call formula
  frml <- as.character(fit$call)[2]
  #   extract signs
  sign <- ifelse(grepl("-", coef(fit)[-1]), " - ", " + ")
  # extract coefficients
  coeffs <- format(abs(coef(fit)), digits = digits, trim = trim)
  if(strict == TRUE){
    i <- 1:(length(coeffs) - 1)
    vars <- c("Y", paste0(" X", i))

  } else {
    # extract vector of variable names
    vars <- unlist(strsplit(frml, "[~+]"))
    # combine everything
  }
  start <- ifelse(coef(fit)[1] > 0, paste(vars[1], coeffs[1], sep = " = "), paste(vars[1], coeffs[1], sep = " = - "))
  end <- paste(sign, coeffs[-1], vars[-1], sep = "", collapse = "")
  return(cat(start, end, sep = ""))
}
