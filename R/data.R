#' a test-retest demo data 
#'
#' a dataframe containing a test-retest demo data of brain connectivity (seed in PCC area) 
#' diamonds.
#'
#' @format A data frame with 62 observations and 364 variables:
#' \describe{
#'   \item{subID}{subuject ID}
#'   \item{visit}{The scan time}
#'   \item{age}{The age is perturbated from the real data in HCP (Human Connectome Project)}
#'   \item{gender}{M: male, F: female}
#'   \item{ROI.*}{Region of Interest, 360 ROIs in total}
#'   ...
#' }
#' @source \url{https://www.humanconnectome.org/}
"brain_trt"