#' ICC (ICC1) calculation using 1-way Random model 
#'
#' Calculate Intra-class correlation (ICC) and variation using Linear Mixed Model (LMM) 
#' with the Restricted Maximum Likelihood (ReML) estimation.  
#' 
#' Model: 1-way random model using lme4 package. Base model: y ~ 1 + (1|subID)
#' 
#' Output: ICC agreement, single and multiple raters.
#'
#' @param data [n, p]: a data matrix for n observations and p variables.
#' @param subID [n]: a vector containing the subject IDs for each subject.
#' @param session [n]: a vector containing the session (run, subsets, site, etc.)
#' @param cov [n, m]: a covariance matrix for n observations and m variables (default: NULL). 
#' @param shiny_progress_bar [1] a Boolean value - show the Shiny progress bar if this function is called in shiny app (library(shinybusy) is required)
#' @return ICC: output vector contains the following elements:
#' 
#' ICC_a: 1-way random model, Agreement, single raters, defined as ICC(1,1)
#' 
#' ICCk_a: 1-way random model, Agreement, multiple raters, defined as ICC(1,k)
#' 
#' sigma2_b: sigma^2(between-sub), estimated between-subject variation
#' 
#' sigma2_r: sigma^2(residual), estimated within-subject variation
#' 
#' var(data): variation of the dependent variable(s)
#' @import lme4
#' @importFrom stats var as.formula 
#' @importFrom shinybusy show_modal_progress_line update_modal_progress remove_modal_progress
#' @references A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
#' @author Ting Xu
#' @export
lme_ICC_1wayR <- function(data, subID, session, cov=NULL, shiny_progress_bar=FALSE) {
  
  n <- dim(data)[1]
  p <- dim(data)[2]
  
  if (is.null((session)) || is.null((subID)) || n!=length(subID) || n!=length(session)) {
    stop('Invalid Input')
  }
  if (is.null(cov)){cov <- data.frame(matrix("", nrow=n, ncol=0))}
  
  smodel <- "y ~ 1 + (1|subID)"
  for (a in colnames(cov)) smodel <- sprintf("%s + %s", smodel, a)

  ICC <- array(0, dim=c(p,6))
  colnames(ICC) <- c("ICC", "ICCk", 
                     "sigma2_b", "sigma2_w", "var.data",
                     "error.message")
  # show shiny progress bar if this function is called in shiny app
  if (shiny_progress_bar){ 
    show_modal_progress_line()
    if (p<100) nprogress <- max(1,floor(p/10)) else nprogress <- 100
  }
  # loop each variable 
  for (i in 1:p){
    # progress
    if (i%%200==1) print(sprintf("Running LMM for %4.2f present ...", n/p*100))
    # show shiny progress bar
    if (shiny_progress_bar && i%%nprogress==1){update_modal_progress(i/p)}
    
    # run ICC model for the i-th variable
    df <- cbind(data.frame(y=data[,i], subID = as.factor(subID), session = as.factor(session)), cov)
    tryCatch({
      fm_1wayR <- lme4::lmer(as.formula(smodel), data=df, REML=TRUE) 
      k <- length(unique(df$session))
      output <- summary(fm_1wayR)
      sigma2_r = as.numeric(output$sigma^2)
      sigma2_b = as.numeric(output$varcor$subID)
      icc1R = sigma2_b/(sigma2_b + sigma2_r)
      icc1R_avg = sigma2_b/(sigma2_b + sigma2_r/(k))
      var_data = var(df$y)
      ICC[i,1:5] <- c(icc1R, icc1R_avg, sigma2_b, sigma2_r, var_data)
      ICC[i,6] <- 0
    }, 
    warning = function(w){print(sprintf("LMM warning (column=%d): %s",i,w)); ICC[i,"error.message"]<<-1},
    error = function(e){print(sprintf("LMM Error (column=%d): %s",i,e)); ICC[i,"error.message"]<<-2}
    )
  }
  
  # shiny progress
  if (shiny_progress_bar){remove_modal_progress()}
  
  return(ICC)
}

#' ICC (ICC2) calculation using 2-way Random model 
#'
#' Calculate Intra-class correlation (ICC) and variation using Linear Mixed Model (LMM) 
#' with the Restricted Maximum Likelihood (ReML) estimation.  
#' 
#' Model: 2-way random model using lme4 package. Base model: y ~ 1 + (1|session) + (1|subID)
#' 
#' Output: ICC agreement or consistency, single and multiple raters.
#'
#' @param data [n, p]: a data matrix for n observations and p variables.
#' @param subID [n]: a vector containing the subject IDs for each subject.
#' @param session [n]: a vector containing the session (run, subsets, site, etc.)
#' @param cov [n, m]: a covariance matrix for n observations and m variables
#' @param shiny_progress_bar [1] a Boolean value - show the Shiny progress bar if this function is called in shiny app (library(shinybusy) is required)
#' 
#' @return ICC: output vector contains the following elements:
#' 
#' ICC_a: 2-way random model, Agreement, single raters, defined ad ICC(2,1)
#' 
#' ICC_c: 2-way random model, Consistency, single raters
#' 
#' ICCk_a: 2-way random model, Agreement, multiple raters, defined ad ICC(2,k)
#' 
#' ICCk_c:  2-way random model, Consistency, multiple raters
#' 
#' sigma2_b: sigma^2(between-sub), estimated between-subject variation
#' 
#' sigma2_s: sigma^2(session),  estimated between-session variation
#' 
#' sigma2_r: sigma^2(residual),  estimated within-subject variation
#' 
#' var(data) - variation of the dependent variable(s)
#' @import lme4
#' @importFrom stats var as.formula
#' @importFrom shinybusy show_modal_progress_line update_modal_progress remove_modal_progress
#' @references A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
#' @author Ting Xu
#' @export
#' 
lme_ICC_2wayR <- function(data, subID, session, cov=NULL, shiny_progress_bar=FALSE) {
  
  n <- dim(data)[1]
  p <- dim(data)[2]
  
  if (is.null((session)) || is.null((subID)) || n!=length(subID) || n!=length(session)) {
    stop('Invalid Input')
  }
  if (is.null(cov)){cov <- data.frame(matrix("", nrow=n, ncol=0))}
  
  ICC <- array(0, dim=c(p,9))
  colnames(ICC) <- c("ICC.a", "ICC.c", "ICCk.a", "ICCk.c", 
                     "sigma2_b", "sigma2_w", "sigma2_rep", "var.data",
                     "error.message")
  # model
  smodel <- "y ~ 1 + (1|session) + (1|subID)"
  for (a in colnames(cov)) smodel <- sprintf("%s + %s", smodel, a)
  
  # show shiny progress bar if this function is called in shiny app
  if (shiny_progress_bar){ 
    show_modal_progress_line()
    if (p<100) nprogress <- max(1,floor(p/10)) else nprogress <- 100
  }
  for (i in 1:p){
    # progress
    if (i%%200==1) print(sprintf("Running LMM for %4.2f present ...", n/p*100))
    # show shiny progress bar
    if (shiny_progress_bar && i%%nprogress==1){update_modal_progress(i/p)}
    
    df <- cbind(data.frame(y=data[,i], subID = subID, session = session), cov)
    tryCatch({
      fm_2wayR <- lme4::lmer(as.formula(smodel), data=df, REML=TRUE) 
      k <- length(unique(df$session))
      output <- summary(fm_2wayR)
      sigma2_r = as.numeric(output$sigma^2)
      sigma2_b = as.numeric(output$varcor$subID)
      sigma2_session = as.numeric(output$varcor$session)
      icc2R_c = sigma2_b/(sigma2_b + sigma2_r)
      icc2R_a = sigma2_b/(sigma2_b + sigma2_r + sigma2_session)
      icc2R_c_avg = sigma2_b/(sigma2_b + sigma2_r/k)
      icc2R_a_avg = sigma2_b/(sigma2_b + (sigma2_session+sigma2_r)/k )
      var_data = var(df$y)
      ICC[i,1:8] <- c(icc2R_a, icc2R_c, icc2R_a_avg, icc2R_c_avg, sigma2_b, sigma2_r, sigma2_session, var_data)
      ICC[i,9] <- 0
    }, 
    warning = function(w){print(sprintf("LMM warning (column=%d): %s",i,w)); ICC[i,"error.message"]<<-1},
    error = function(e){print(sprintf("LMM Error (column=%d): %s",i,e)); ICC[i,"error.message"]<<-2}
    )
  }
  
  # shiny progress
  if (shiny_progress_bar){remove_modal_progress()}
  
  return(ICC)
}


#' ICC (ICC3) calculation using 2-way Mixed model 
#'
#' Calculate Intra-class correlation (ICC) and variation using Linear Mixed Model (LMM) 
#' with the Restricted Maximum Likelihood (ReML) estimation.  
#' 
#' Model: 2-way mixed model using lme4 package. Base model: y ~ 1 + session + (1|subID)
#' 
#' Output: ICC consistency, single and multiple raters.
#'
#' @param data [n, p]: a data matrix for n observations and p variables.
#' @param subID [n]: a vector containing the subject IDs for each subject.
#' @param session [n]: a vector containing the session (run, subsets, site, etc.)
#' @param cov [n, m]: a covariance matrix for n observations and m variables
#' @param shiny_progress_bar [1] a Boolean value - show the Shiny progress bar if this function is called in shiny app (library(shinybusy) is required)
#' 
#' @return ICC: output vector contains the following elements:
#' 
#' ICC_c:     2-way mixed model, Consistency, single raters, defined as ICC(3,1).
#' 
#' ICCk_c:    2-way mixed model, Consistency, multiple raters, defined as ICC(3,k).
#' 
#' sigma2_b:  sigma^2(between-sub), estimated between-subject variation.
#' 
#' sigma2_r:  sigma^2(residual),  estimated within-subject variation (variation of residual).
#' 
#' var(data): variation of the dependent variable(s)
#' @import lme4
#' @importFrom stats var as.formula
#' @importFrom shinybusy show_modal_progress_line update_modal_progress remove_modal_progress
#' @references A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
#' @author Ting Xu
#' @export
#' 
lme_ICC_2wayM <- function(data, subID, session, cov=NULL, shiny_progress_bar=FALSE) {
  
  n <- dim(data)[1]
  p <- dim(data)[2]
  
  if (is.null((session)) || is.null((subID)) || n!=length(subID) || n!=length(session)) {
    stop('Invalid Input')
  }
  if (is.null(cov)){cov <- data.frame(matrix("", nrow=n, ncol=0))}
  
  ICC <- array(0, dim=c(p,6))
  colnames(ICC) <- c("ICC.c", "ICCk.c", 
                     "sigma2_b", "sigma2_w", "var.data",
                     "error.message") 
  
  # model
  smodel <- "y ~ 1 + session + (1|subID)"
  for (a in colnames(cov)) smodel <- sprintf("%s + %s", smodel, a)
  
  # show shiny progress bar if this function is called in shiny app
  if (shiny_progress_bar){ 
    show_modal_progress_line()
    if (p<100) nprogress <- max(1,floor(p/10)) else nprogress <- 100
  }
  # loop each variable
  for (i in 1:p){
    # progress
    if (i%%200==1) print(sprintf("Running LMM for %4.2f present ...", n/p*100))
    # show shiny progress bar
    if (shiny_progress_bar && i%%nprogress==1){update_modal_progress(i/p)}
    
    df <- cbind(data.frame(y=data[,i], subID = subID, session = session), cov)
    tryCatch({
      fm_2wayM <- lme4::lmer(as.formula(smodel), data=df, REML=TRUE) 
      k <- length(unique(df$session))
      output <- summary(fm_2wayM)
      sigma2_r = as.numeric(output$sigma^2)
      sigma2_b = as.numeric(output$varcor$subID)
      icc2M_c = sigma2_b/(sigma2_b + sigma2_r)
      icc2M_c_avg = sigma2_b/(sigma2_b + sigma2_r/k)
      var_data = var(df$y)
      ICC[i,1:5] <- c(icc2M_c,icc2M_c_avg, sigma2_b, sigma2_r,  var_data)
      ICC[i,6] <- 0
    }, 
    warning = function(w){print(sprintf("LMM warning (column=%d): %s",i,w)); ICC[i,"error.message"]<<-1},
    error = function(e){print(sprintf("LMM Error (column=%d): %s",i,e)); ICC[i,"error.message"]<<-2}
    )
  }
  
  # shiny progress
  if (shiny_progress_bar){remove_modal_progress()}
  
  return(ICC)
}

