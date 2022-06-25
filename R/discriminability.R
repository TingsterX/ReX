#' calculate the discriminability 
#' 
#' call discriminability function to calculate the discriminability \code{discr.discr}
#' 
#' @param dist [n x n]: distance matrix. 
#' @param sID [n]: a subject ID vector
#' @param all_discr.return [1]: if return all discriminaiblity for each within-individual repetition 
#' 
#' @return d.all: discriminability (D) for the entire dataset
#' If all_discr.return = TRUE, a list is returned, containing D for the entire dataset and D for each repetition 
#' 
#' @export
calc_discriminability <- function(dist, sID, all_discr.return=FALSE){
  d.all <- discr.discr(discr.rdf(as.matrix(dist), sID), all_discr.return=all_discr.return)
  return(d.all)
}

#' Discriinability Density Function
#'
#' A function for computing the reliability density function of a dataset.
#'
#' @param dist [n, n]: a distance matrix for n subjects.
#' @param ids [n]: a vector containing the subject ids for each subject.
#' @return rdf [n]: the reliability per subject.
#' @author Shangsi Wang, Eric Bridgeford and Gregory Kiar
#' @export
discr.rdf <- function(dist, ids) {
  N <- dim(dist)[1]
  if (is.null((N))) {
    stop('Invalid datatype for N')
  }
  
  uniqids <- unique(as.character(ids))
  countvec <- vector(mode="numeric",length=length(uniqids))
  
  for (i in 1:length(uniqids)) {
    countvec[i] <- sum(uniqids[i] == ids) # total number of scans for the particular id
  }
  
  scans <- max(countvec) # assume that we will worst case have the most
  rdf <- array(NaN, N*(scans-1)) # initialize empty ra
  
  count <- 1
  for (i in 1:N) {
    ind <- which(ids[i] == ids) # all the indices that are the same subject, but different scan
    for (j in ind) {
      if (!isTRUE(all.equal(j, i))) { # if j != i, then we want j to have a close distance to i, and estimate where it ranks
        di <- dist[i,] # get the entire ra for the particular scan
        di[ind] <- Inf # don't want to consider the particular scan itself
        d <- dist[i,j] # the distance between the particular scan of a subject and another scan of the subject
        rdf[count] <- 1 - (sum(di[!is.nan(di)] < d) + 0.5*sum(di[!is.nan(di)] == d)) / (N-length(ind)) # 1 for less than, .5 if equal, then average
        count <-  count + 1
      }
    }
  }
  return(rdf[1:count-1]) # return only the occupied portion
}

#' Discriminability
#'
#' A function for computing the discriminability. 
#'
#' @param rdf [n]: the reliability density function. call \code{discr.rdf}
#' @param remove_outliers a boolean indicating whether to ignore subjects with rdf below a certain cutoff (Default: TRUE)
#' @param thresh [1]: a numberic value - the threshold, ignore subjects < thred (Default: 0)
#' @param output a boolean value indicating whether to ignore output (Default: FALSE)
#' @param all_discr.return a boolean value indicating if return the discriminability for each observation (Default: FALSE)
#' @return discr [1]: the discriminability statistic.
#' @author Original: Eric Bridgeford and Gregory Kiar; Modified: Ting Xu, return all discriminaiblity for each within-individual repetition 
#' @export
discr.discr <- function(rdf, remove_outliers=TRUE, thresh=0, output=FALSE, all_discr.return=FALSE) {
  if (remove_outliers) {
    discr <- mean(rdf[which(rdf[!is.nan(rdf)] > thresh)]) # mean of the rdf
    ol <- length(which(rdf<thresh))
    if (output) {
      print(paste('Graphs with reliability <',thresh,'(outliers):', ol))
    }
  } else {
    ol <- 0
    discr <- mean(rdf[!is.nan(rdf)])
  }
  nopair <- length(rdf[is.nan(rdf)])
  if (output) {
    print(paste('Graphs with unique ids:',nopair))
    print(paste('Graphs available for reliability analysis:', length(rdf)-ol-nopair))
    print(paste('discr:', discr))
  }
  if (all_discr.return){
    return(list(discr, rdf))
  }else{
  return(discr)
  }
}