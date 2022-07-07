#' a test-retest demo data 
#'
#' a dataframe containing a test-retest demo data of brain connectivity (seed is the posterial cingular cortex) 
#'
#' @format A data frame with 62 observations and 364 variables:
#' @source \url{https://www.humanconnectome.org/}
"demo_brain_trt"

#' ICC and variation output demo using lme_ICC_1wayR 
#'
#' a dataframe containing ICC, intra- and inter-individual variation of
#' brain connectivities between the posterial cingular cortex and all 360 brain regions
#' based on test-retest fMRI data that preprocessed without global signal regression (GSR)
#' 
#' sigma2_b is the inter-individual variation
#' sigma2_w is the intra-individual variation
#'
#' @format A data frame with 360 observations and 7 variables:
#' @source \url{https://www.humanconnectome.org/}
"demo_icc_noGSR"

#' ICC and variation output demo using lme_ICC_1wayR 
#'
#' a dataframe containing ICC, intra- and inter-individual variation of
#' brain connectivities between the posterial cingular cortex and all 360 brain regions
#' based on test-retest fMRI data that preprocessed with global signal regression (GSR)
#' 
#' sigma2_b is the inter-individual variation
#' sigma2_w is the intra-individual variation
#'
#' @format A data frame with 360 observations and 7 variables:
#' @source \url{https://www.humanconnectome.org/}
"demo_icc_GSR"

#' the change of the variation calculate using icc_gradient_flow
#'
#' a dataframe containing the raw and normalized change of the intra- and inter-individual variation (sigma2_w, sigma2_b)
#' of fMRI-based connectivities with GSR compared to those without GSR 
#' See data demo_icc_GSR and demo_icc_noGSR
#' 
#' delta.sigma2_b is the change of inter-individual variation
#' delta.sigma2_w is the change of intra-individual variation
#' delta.sigma2_b_norm is the relative (normalized) change of inter-individual variation
#' delta.sigma2_w_norm is the relative (normalized)change of intra-individual variation
#'
#' @format A data frame with 360 observations and 8 variables:
#' @source \url{https://www.humanconnectome.org/}
"demo_delta_var"