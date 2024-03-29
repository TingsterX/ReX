#' @title Distance-based Intraclass Correlation (dbICC) 
#' 
#' @description A function for computing the dbICC - multivariate reliability
#' 
#' @param dmax [n, n]: a distance matrix for n observations 
#' @param subID [n]: a vector containing the subject IDs
#' @param sortdata : a Boolean value - if sort the input data based on subID
#' @param return_var : a Boolean value - if return the within and between individual variance (default=TRUE)
#' 
#' @return distance-based intraclass correlation (dbICC)
#' @examples 
#' data <- demo_brain_trt[,5:10]
#' subID <- as.matrix(demo_brain_trt$subID)
#' calc_dbICC(dist(data), subID)
#' 
#' @note subID should match with the data (row)
#' @references Meng Xu et al., 2021. Generalized reliability based on distances. Biometrics.https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7984087
#' 
#' @author Ting Xu modified from https://github.com/wtagr/dbicc
#' @export
calc_dbICC <- function(dmax, subID, sortdata=TRUE, return_var=TRUE) {
  # calculate the dbICC
  # convert the long format distance to matrix
  dmax <- as.matrix(dmax^2)
  # check the dimension
  if (length(subID) != dim(dmax)[1]){
    stop('check data: the number of subID and the dimension of distance matrix is not matched')
  }
  # default distance matrix should be sorted by subID
  if (sortdata) {
    a <- sort(subID, index.return=TRUE)
    subID <- subID[a$ix]
    dmax <- dmax[a$ix, a$ix]
  } 
  # do the job
  n <- length(subID)
  wmask <- matrix(0,n,n)
  ii <- 1
  for (i in 2:n){
    if (subID[i] == subID[i-1]){ii <- ii+1}
    else{
      a <- matrix(0,ii,ii); a[upper.tri(matrix(1,ii,ii))] <- 1
      wmask[(i-ii):(i-1), (i-ii):(i-1)] <- a
      ii <- 1
    }
  }
  bmask <- upper.tri(matrix(1,n,n)) - wmask
  wmask[wmask == 0] <- NA
  bmask[bmask == 0] <- NA
  
  dbicc <- 1 - mean(dmax * wmask, na.rm = TRUE) / mean(dmax * bmask, na.rm = TRUE)
  if (return_var){
    var_w <- 0.5*mean(dmax * wmask, na.rm = TRUE)
    var_b <- 0.5*mean(dmax * bmask, na.rm = TRUE) - var_w
    out <- list(dbicc, var_w, var_b)
    names(out) <- c("dbicc", "var_w", "var_b")
    return(out) 
  }
  else {
    return(dbicc)
  }
}