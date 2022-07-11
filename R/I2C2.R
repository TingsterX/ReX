#' calculate the I2C2 
#' 
#' call I2C2 function \code{I2C2::I2C2} from https://rdrr.io/github/neuroconductor/I2C2
#' 
#' @param data [n x n]: distance matrix. 
#' @param sID [n]: a subject ID vector
#' @param session [n]: a vector indicate session(repetition)  
#' 
#' @return I2C2
#' 
#' @export
calc_i2c2 <- function(data, sID, session){
  # install I2C2 package (https://rdrr.io/github/neuroconductor/I2C2)
  # if (!require(remotes)) install.packages('remotes')
  # remotes::install_github("neuroconductor/I2C2")
  x <- I2C2::I2C2(as.matrix(data, nrow=length(sID)), sID, as.factor(session))
  return(x$lambda)
}
