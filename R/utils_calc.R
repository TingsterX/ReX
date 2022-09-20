#' calculate the ICC gradient flow (change and normalized change of ICC)
#' 
#' call function \code{gradient_flow} to calculate the ICC gradient flow
#' 
#' @param df the target dataframe contains within-individual variation named as sigma2_w and between-individual variation named as sigma2_b
#' @param df.ref the reference dataframe 
#' @param name assign a name of the target icc dataset (default: NULL)
#' @param name.ref assign a name of the reference icc dataset (default NULL)
#' @param return.input if return the input datasets (target is named as .1 the reference is named as .2)
#' 
#' @return df.gradient contains normalized sigma2_w, normalized sigma2_b, delta.icc, normalized theta [0, 2pi), contrast name,
#' @references A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
#' @author Ting XU
#' @export
calc_icc_gradient_flow <- function(df, df.ref, name=NULL, name.ref=NULL, return.input=FALSE){
  # df.ref: Input dataframe: contains sigma2_w, sigma2_b
  # df: Input dataframe: contains sigma2_w, sigma2_b
  if (!"sigma2_w" %in% colnames(df.ref) || !"sigma2_b" %in% colnames(df.ref) ||
      !"sigma2_w" %in% colnames(df) || !"sigma2_b" %in% colnames(df)){
    stop("Input dataframes require variables: sigma2_w, sigma2_b")
  }
  if (dim(df)[1] != dim(df.ref)[1]){
    stop("Please make sure that the rows of input dataframes are matched")
  }
  if (is.null(name)){name <- 'Target'}
  if (is.null(name.ref)){name.ref <- 'Reference'}
  
  df.gradient <- icc_gradient_flow(df$sigma2_w, df$sigma2_b, df.ref$sigma2_w, df.ref$sigma2_b)
  df.gradient$delta.icc <- df$icc - df.ref$icc
  df.gradient$contrast <- sprintf('%s-%s', name, name.ref)
  if (return.input){
    df.gradient$icc.1 <- df.ref$icc
    df.gradient$sigma2_w.1 <- df.ref$sigma2_w
    df.gradient$sigma2_b.1 <- df.ref$sigma2_b
    df.gradient$icc.2 <- df$icc
    df.gradient$sigma2_w.2 <- df$sigma2_w
    df.gradient$sigma2_b.2 <- df$sigma2_b
  }
  return(df.gradient)
}

#' gradient flow function - calculate the normalized change of ICC (sigma2 and theta)
#' 
#' calculate the normalied sigma2_w, normalized sigma2_b and normalized theta in [0, 2pi)
#' 
#' @param x1 within-individual variation of the target (end point)
#' @param y1 between-individual variation of the target (end point)
#' @param x0 within-individual variation of the reference (start point)
#' @param y0 between-individual variation of the reference (start point)
#' 
#' @return df: a dataframe contains normalized sigma2_w, normalized sigma2_b, normalized theta in  [0, 2pi)
#' @references A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
#' @author Ting XU
#' @export
icc_gradient_flow <- function(x1, y1, x0, y0){
  # calculate the normalized icc gradient flow
  # start point coordinate: x0=sigma2_w0, y0=sigma2_b0
  # end point coordinate:   x1=sigma2_w1, y1=sigma2_b1
  # Return: variation change (raw): var.delta.x, var.delta.y, var.theta
  #         variation change (normalize to reference line x=y): var.delta.x_norm, var.delta.y_norm, var.theta_norm
  # normalize the variance change vector reference line: x=y
  # rotation matrix [cos(theta), -sin(theta); sin(theta) cos(theta)]
  # x' = x*cos(theta) - y*sin(theta) 
  # y' = x*sin(theta) - y*cos(theta) 
  # Author: Ting Xu
  df <- data.frame(delta.sigma2_w = x1-x0, delta.sigma2_b = y1-y0)
  df$delta.theta <- atan2_2pi(y1-y0, x1-x0)
  
  theta0 <- atan2(y0,x0)
  rot <- 45*pi/180 - theta0
  
  df$delta.sigma2_w_norm <- cos(rot)*(x1-x0) - sin(rot)*(y1-y0)
  df$delta.sigma2_b_norm <- sin(rot)*(x1-x0) + cos(rot)*(y1-y0)
  df$delta.theta_norm    <- atan2_2pi(df$delta.sigma2_b_norm, df$delta.sigma2_w_norm) 
  
  return(df)
}

atan2_2pi <- function(y, x){
  # calculate the theta defined by the coordinate (x, y)
  # atan2 to calcualte theta (in range (-pi, pi]), and convert theta : [0,2*pi)
  # Ting Xu
  theta <- atan2(y, x)
  theta <- theta - 2*pi*apply(cbind(sign(theta), rep(0, length(theta ))), 1, min) 
  return(theta) 
}

#' Reliability Explorer wrap-up function - discriminability
#'
#' A function for computing the discriminability for each row variable in data (show progress bar in shiny)
#' 
#' 
#' @param data [n x p] data matrix: n observations x p variables
#' @param sID [n] a vector containing subject ID
#' @param visit [n] a vector containing the repetition identify (e.g. time1, time2)
#' @param method.dist [1] a character indicating which method is used to calcualte the distance (Default: euclidean). Check \code{dist}
#' @param all_discr.return [1] a boolean value - if return the discriminability for each observation (Default: TRUE)
#' @param shiny_progress_bar [1] a boolean value - show the Shiny progress bar if this function is called in shiny app (shinybusy library is required)
#' 
#' @return out_list: a list containing discriminability (Discr) and/or discriminability for each repetition/observation (DiscrSub) for each variable
#' @importFrom shinybusy show_modal_progress_line update_modal_progress remove_modal_progress
#' @importFrom stats dist
#' @references A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
#' @author Ting XU
#' @export
#' 
discriminability_wraper <- function(data, sID, visit, method.dist = "euclidean", all_discr.return=TRUE, shiny_progress_bar=FALSE) {
  # data: dataframe or matrix, sID: dataframe, visit: dataframe 
  #
  n_complete <- dim(data)[1]
  # deal with missing data (NA) in data (remove NA)
  idx_complete <- complete.cases
  if (all(idx_complete)){ 
    print("Warning: missing data (NA) exist and is removed in the Discriminability calculation")
    if (all_discr.return){
      df.discr_sub_comp <- data.frame(array(NA, dim=c(n_complete,p)))
      colnames(df.discr_sub_comp) <- colnames(data)
      df.rep_comp <- cbind(sID, visit)
    }
    # remove the NA data - listwise
    data <- data[complete.cases(data),]
  }
  
  #
  n <- dim(data)[1]
  p <- dim(data)[2]
  
  if ( is.null((sID[,1])) || n!=length(sID[,1]) ) {
    stop('Invalid Input')
  }
  
  df.discr <- data.frame(array(0, dim=c(p,1)))
  colnames(df.discr) <- "discriminability"
  rownames(df.discr) <- colnames(data)
  
  if (all_discr.return){
    df.discr_sub <- data.frame(array(0, dim=c(n,p)))
    colnames(df.discr_sub) <- colnames(data)
    df.rep <- cbind(sID, visit)
  }
  
  # shiny progress 
  if (shiny_progress_bar){ 
  show_modal_progress_line()
  if (p<100) nprogress <- max(1,floor(p/10)) else nprogress <- 100
  }
  for (i in 1:p){
    # shiny progress update
    if (shiny_progress_bar && i%%nprogress==1) update_modal_progress(i/p)
    
    D <- dist(data[,i], method = method.dist)
    d <- discr.discr(discr.rdf(as.matrix(D), sID[,1]), all_discr.return=all_discr.return)
    df.discr[i,1] <- d[[1]]
    if (all_discr.return) {
      df.discr_sub[,i] <- d[[2]]
      }
  }
  
  # shiny progress
  if (shiny_progress_bar){remove_modal_progress()}
  
  if (all_discr.return){
    if (all(idx_complete)){ 
      df.discr_sub_comp[idx_complete,] <- df.discr_sub
      df.discr_sub_comp <- cbind(df.rep_comp, df.discr_sub_comp)
    } 
    else {
      df.discr_sub_comp <- df.discr_sub
      df.discr_sub_comp <- cbind(df.rep, df.discr_sub_comp)
    }
    out_list <- list(df.discr, df.discr_sub)
    names(out_list) <- c("Discr", "DiscrSub")
  }
  else{
    out_list <- list(df.discr)
    names(out_list) <- c("Discr")
  }
  return(out_list)
}

#' Reliability Explorer wrap-up function - fingerprinting
#'
#' A function for computing the fingerprinting for each row variable in data (show progress bar in shiny)
#' 
#' 
#' @param data [n x p] data matrix: n observations x p variables
#' @param sID [n] a vector containing subject ID
#' @param visit [n] a vector containing the repetition identify (e.g. time1, time2)
#' @param method.dist [1] a character value indicating the method to calculate the distance matrix (default: "euclidean"), check \code{dist}
#' @param method.FP [1] a numeric value indicating which method used to count the identification rate (default: 1). Check \code{calc_fingerprinting}
#' @param shiny_progress_bar [1] a Boolean value - show the Shiny progress bar if this function is called in shiny app (shinybusy library is required)
#' 
#' @return out_list: a list containing identification rate (fingerprinting) for each variable
#' @importFrom shinybusy show_modal_progress_line update_modal_progress remove_modal_progress
#' @references A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
#' @author Ting XU
#' @export
#' 
fingerprinting_wraper <- function(data, sID, visit, method.dist = "euclidean", method.FP=1, shiny_progress_bar=FALSE) {
  # data: dataframe or matrix, sID: dataframe, visit: dataframe 
  
  n <- dim(data)[1]
  p <- dim(data)[2]
  
  if ( is.null((sID[,1])) || n!=length(sID[,1]) ) {
    stop('Invalid Input')
  }
  
  df.FP <- data.frame(array(0, dim=c(p,1)))
  colnames(df.FP) <- "fingerprinting"
  rownames(df.FP) <- colnames(data)

  # shiny progress 
  if (shiny_progress_bar){ 
  show_modal_progress_line()
  if (p<100) nprogress <- max(1,floor(p/10)) else nprogress <- 100
  }
  
  for (i in 1:p){
    # shiny progress update
    if (shiny_progress_bar && i%%nprogress==1){update_modal_progress(i/p)}
    
    D <- dist(data[,i], method = method.dist)
    # Fingerprinting
    df.FP[i,1]  <- calc_fingerprinting(D, sID[,1], method=method.FP)
    
  }
  # shiny progress
  if (shiny_progress_bar){remove_modal_progress()}
  
  out_list <- list(df.FP)
  names(out_list) <- c("FP")
  return(out_list)
}


#' Reliability Explorer wrap-up function - discriminability & fingerprinting 
#'
#' A function for computing the fingerprinting for each row variable in data (show progress bar in shiny)
#' 
#' 
#' @param data [n x p] data matrix: n observations x p variables
#' @param sID [n] a vector containing subject ID
#' @param visit [n] a vector containing the repetition identify (e.g. time1, time2)
#' @param method.dist method to calculate the distance matrix (default: "euclidean")
#' @param all_discr.return return the discriminability for each observation/repetition 
#' @param method.FP a numeric value indicating which method used to count the identification rate (default: 1). Check \code{calc_fingerprinting}
#' @param shiny_progress_bar  a Boolean value - show the Shiny progress bar if this function is called in shiny app (shinybusy library is required)
#' 
#' @return a list containing identification rate (fingerprinting) for each variable
#' @importFrom shinybusy show_modal_progress_line update_modal_progress remove_modal_progress
#' @references A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
#' @author Ting XU
#' @export
#' 
Discr_and_FP_wraper <- function(data, sID, visit, method.dist = "euclidean", all_discr.return=TRUE, method.FP=1, shiny_progress_bar=FALSE) {
  # data: dataframe or matrix, sID: dataframe, visit: dataframe 
  
  n <- dim(data)[1]
  p <- dim(data)[2]
  
  if ( is.null((sID[,1])) || n!=length(sID[,1]) ) {
    stop('Invalid Input')
  }
  df.discr <- data.frame(array(0, dim=c(p,1)))
  colnames(df.discr) <- "discriminability"
  rownames(df.discr) <- colnames(data)
  df.FP <- data.frame(array(0, dim=c(p,1)))
  colnames(df.FP) <- "fingerprinting"
  rownames(df.FP) <- colnames(data)
  
  if (all_discr.return){
    df.discr_sub <- data.frame(array(0, dim=c(n,p)))
    colnames(df.discr_sub) <- colnames(data)
    df.rep <- cbind(sID, visit)
  }
  
  # shiny progress 
  if (shiny_progress_bar){ 
  show_modal_progress_line()
  if (p<100) nprogress <- max(1,floor(p/10)) else nprogress <- 100
  }
  # 
  for (i in 1:p){
    # shiny progress update
    if (shiny_progress_bar && i%%nprogress==1){update_modal_progress(i/p)}
    
    D <- dist(data[,i], method = method.dist)
    # Discriminability
    d <- discr.discr(discr.rdf(as.matrix(D), sID[,1]), all_discr.return=all_discr.return)
    df.discr[i,1] <- d[[1]]
    if (all_discr.return) df.discr_sub[,i] <- d[[2]]
    # Fingerprinting
    df.FP[i,1]  <- calc_fingerprinting(D, sID[,1], method=method.FP)
    
  }
  
  # shiny progress
  if (shiny_progress_bar){remove_modal_progress()}
  
  if (all_discr.return){
    df.discr_sub <- cbind(df.rep, df.discr_sub)
    out_list <- list(df.discr, df.discr_sub, df.FP)
    names(out_list) <- c("Discr", "DiscrSub", "FP")
  }
  else{
    out_list <- list(df.discr, df.FP)
    names(out_list) <- c("Discr", "FP")
  }
  return(out_list)
}