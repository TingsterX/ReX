# ----------------------------------------------------------------------------------
# function for visualization the distance field
# ----------------------------------------------------------------------------------

#' plot distance matrix 
#'
#' plot distance matrix - ggplot based
#' 
#' @param Dmax Distance matrix
#' @param gtitle plot title (default: NULL)
#' @param color palette name from ColorBrewer, or a color array, e.g. c("blue", "red"). By default, color=c("white", "red)
#'              Check out the what color palette available
#'              \code{library(ColorBrewer); display.brewer.all()}
#' @param show.diag If true, assign 0 to diagonal matrix
#' 
#' @return ggplot object of the distance matrix 
#' @import ggplot2
#' @importFrom reshape2 melt
#' @references A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
#' @author Ting XU
#' @export
rex_plot.distance <- function(Dmax, gtitle='', color=c("white", "red"), show.diag=TRUE){
  Dmax <- as.matrix(Dmax)
  if (length(color)==1 && is.element(color, rownames(brewer.pal.info))) {
    cmap <- color.brewer.n(brewer.pal.info[color,]$maxcolors, color)
  }else{
    cmap <- color
  }
  
  if (show.diag) xmin <- 0
  else xmin <- min(Dmax[Dmax>0])
  xmax <- max(Dmax)
  data <- reshape2::melt(as.matrix(Dmax), c("x", "y"), value.name = "distance")
  pdist <- ggplot(data=data, aes(x=.data$y, y=.data$x, fill=.data$distance)) + 
    geom_tile(color="grey") +
    scale_fill_gradientn(colours=cmap, name="distance", limits=c(xmin, xmax)) +
    xlab("") + ylab("") + coord_fixed(ratio=1) +
    ggtitle(gtitle) + 
    scale_y_reverse(expand = c(0,0)) + scale_x_discrete(expand=c(0,0)) +
    #scale_y_discrete(limits=rev, expand = c(0,0)) + scale_x_discrete(expand=c(0,0)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
          axis.ticks.y = element_blank(), axis.text.y = element_blank())
  return(pdist)
} # ----------------------------------------------------------------------------------

#' plot distance matrix, ordered by subID and visit
#'
#' plot distance matrix, ordered by subID and visit
#' 
#' @param Dmax [n x n] a matrix, Distance matrix 
#' @param gtitle plot title (default: NULL)
#' @param subID [n] a vector, subject ID 
#' @param visit [n] a vector, visit (e.g. time1, time2)
#' @note subID and visit should match with the row/column of the Dmax. The color of the plot is white to red
#' 
#' @return pdist: ggplot object of the distance matrix 
#' @import ggplot2
#' @importFrom reshape2 melt
#' @references A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
#' @author Ting XU
#' @export
rex_plot.sub_distance <- function(Dmax, subID, visit, gtitle='') {
  # plot distance matrix (between subjects and repetitions)
  # subID, visit: char vector
  # Input: Dmax (distance matrix)
  # Output matrix will be ordered
  xmin <- min(Dmax[Dmax>0])
  xmax <- max(Dmax)
  data <- reshape2::melt(as.matrix(Dmax), c("x", "y"), value.name = "distance")
  data$x <- paste0(subID,".",visit)
  pdist <- ggplot(data = data, aes(x=.data$y, y=.data$x, fill=.data$distance)) + 
    geom_tile(color="grey") +
    scale_fill_gradientn(colours=c("white", "red"), name="distance", limits=c(xmin, xmax)) +
    xlab("") + ylab("") + coord_fixed(ratio=1) +
    ggtitle(gtitle) + 
    scale_y_discrete(limits=rev, expand = c(0,0)) + scale_x_discrete(expand=c(0,0)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
          axis.ticks.y = element_blank(), axis.text.y = element_text(size=6, angle = 0, hjust = 1)
    )
  return(pdist)
} # ----------------------------------------------------------------------------------

#' plot distance field map
#'
#' plot distance field map (x-axis: within-individual distance, y-axis: between-distance distance)
#' 
#' @param dFM.df a datafrmae containing variables: distance, wD, bD
#' @param ptype plot options: 'Discr', 'FP', 'FP2', 'Discr+FP', 'Discr+FP2', 'All
#' @param point.size dot size in the plot (default = 2)
#' 
#' @return pdist: ggplot object of the distance matrix 
#' @import ggplot2
#' @importFrom reshape2 melt
#' @references A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
#' @author Ting XU
#' @export
rex_plot.distance_field_map <- function(dFM.df, ptype, point.size=2){
  # plot the distance field map
  # Input: distance_field_map.df(Dmax, subID)
  # x-axis: within-individual distance
  # y-axis: between-individual distance
  dFM.df$distance <- NULL
  dFM.df$distance[dFM.df$wD < dFM.df$bD] <- 'Discriminable (x<y)'
  dFM.df$distance[dFM.df$wD > dFM.df$bD] <- 'Indiscriminable (x>y)'
  
  dFMmin <- min(min(dFM.df$wD), min(dFM.df$bD))
  dFMmax <- max(max(dFM.df$wD), max(dFM.df$bD))
  dFMmargin <- (dFMmax - dFMmin)/20
  dFMmin <- max(dFMmin-dFMmargin, 0)
  dFMmax <- dFMmax+dFMmargin
  
  df_line <- data.frame(x=c(dFMmin, dFMmax), y=c(dFMmin, dFMmax))
  df_poly_above <- df_line
  df_poly_above[nrow(df_line)+1,] <- c(dFMmax, dFMmax)
  df_poly_above[nrow(df_line)+2,] <- c(dFMmin, dFMmax)
  
  if (ptype == 'Discr'){
    p <- ggplot(dFM.df,aes(.data$wD, .data$bD)) + 
      geom_point(aes(colour=.data$distance, fill = .data$distance), shape=21, size = point.size) + 
      scale_color_manual(breaks = c("Discriminable (x<y)", "Indiscriminable (x>y)"), values = c('orangered2', 'grey10')) + 
      scale_fill_manual(breaks = c("Discriminable (x<y)", "Indiscriminable (x>y)"), values= c('#ff9987', 'grey80')) + 
      geom_abline(slope=1, intercept=0, color='forestgreen') +
      #annotate(geom="text", x=dFMmax*0.98, y=dFMmax*0.98, label='x=y', color="gray30", angle='45', size=5) +
      scale_x_continuous(limits=c(dFMmin, dFMmax), expand = c(0, 0)) + 
      scale_y_continuous(limits=c(dFMmin, dFMmax), expand = c(0, 0)) +
      xlab('Within-individual distance (x)') + ylab('Observed Between-individual distance (y)') +
      labs(color = "Discriminability") + guides(fill="none") +
      coord_equal() + theme(aspect.ratio=1) +
      theme_bw() 
  }
  else if (ptype == 'FP'){
    p <- ggplot(dFM.df,aes(.data$wD, .data$bD)) + 
      geom_point(aes(colour=.data$FPsub, fill = .data$FPsub), shape=21, size = point.size) + 
      scale_color_manual(breaks = c("Identified", "Not identified"), values = c('orangered2', 'grey10')) + 
      scale_fill_manual(breaks = c("Identified", "Not identified"), values= c('#ff9987', 'grey80')) + 
      geom_abline(slope=1, intercept=0, color='forestgreen') +
      #annotate(geom="text", x=dFMmax*0.98, y=dFMmax*0.98, label='x=y', color="gray30", angle='45', size=5) +
      scale_x_continuous(limits=c(dFMmin, dFMmax), expand = c(0, 0)) + 
      scale_y_continuous(limits=c(dFMmin, dFMmax), expand = c(0, 0)) +
      xlab('Within-individual distance (x)') + ylab('Observed Between-individual distance (y)') +
      labs(color = "Fingerprinting") + guides(fill="none") +
      coord_equal() + theme(aspect.ratio=1) +
      theme_bw() 
  }
  else if (ptype == 'FP2'){
    p <- ggplot(dFM.df,aes(.data$wD, .data$bD)) + 
      geom_point(aes(colour=.data$FPwithin, fill = .data$FPwithin), shape=21, size = point.size) + 
      scale_color_manual(breaks = c("Identified", "Not identified"), values = c('orangered2', 'grey10')) + 
      scale_fill_manual(breaks = c("Identified", "Not identified"), values= c('#ff9987', 'grey80')) + 
      geom_abline(slope=1, intercept=0, color='forestgreen') +
      #annotate(geom="text", x=dFMmax*0.98, y=dFMmax*0.98, label='x=y', color="gray30", angle='45', size=5) +
      scale_x_continuous(limits=c(dFMmin, dFMmax), expand = c(0, 0)) + 
      scale_y_continuous(limits=c(dFMmin, dFMmax), expand = c(0, 0)) +
      xlab('Within-individual distance (x)') + ylab('Observed Between-individual distance (y)') +
      labs(color = "Fingerprinting (repetition)") + guides(fill="none") +
      coord_equal() + theme(aspect.ratio=1) +
      theme_bw() 
  }
  else if (ptype == 'Discr+FP'){
    p <- ggplot(dFM.df,aes(.data$wD, .data$bD)) + 
      geom_point(aes(colour=.data$FPsub, fill = .data$FPsub), shape=21, size = point.size) + 
      geom_polygon(data = df_poly_above, aes('x','y'), fill = "red", alpha = .2) +
      scale_color_manual(breaks = c("Identified", "Not identified"), values = c('orangered2', 'grey10')) + 
      scale_fill_manual(breaks = c("Identified", "Not identified"), values= c('#ff9987', 'grey80')) + 
      geom_abline(slope=1, intercept=0, color='forestgreen') +
      #annotate(geom="text", x=dFMmax*0.98, y=dFMmax*0.98, label='x=y', color="gray30", angle='45', size=5) +
      scale_x_continuous(limits=c(dFMmin, dFMmax), expand = c(0, 0)) + 
      scale_y_continuous(limits=c(dFMmin, dFMmax), expand = c(0, 0)) +
      xlab('Within-individual distance (x)') + ylab('Observed Between-individual distance (y)') +
      guides(color="none", fill="none") + 
      coord_equal() + theme(aspect.ratio=1) +
      theme_bw() 
  }
  else if (ptype == 'Discr+FP2'){
    p <- ggplot(dFM.df,aes(.data$wD, .data$bD)) + 
      geom_point(aes(colour=.data$FPwithin, fill = .data$FPwithin), shape=21, size = point.size) + 
      geom_polygon(data = df_poly_above, aes(.data$x,.data$y), fill = "red", alpha = .2) +
      scale_color_manual(breaks = c("Identified", "Not identified"), values = c('orangered2', 'grey10')) + 
      scale_fill_manual(breaks = c("Identified", "Not identified"), values= c('#ff9987', 'grey80')) + 
      geom_abline(slope=1, intercept=0, color='forestgreen') +
      #annotate(geom="text", x=dFMmax*0.98, y=dFMmax*0.98, label='x=y', color="gray30", angle='45', size=5) +
      scale_x_continuous(limits=c(dFMmin, dFMmax), expand = c(0, 0)) + 
      scale_y_continuous(limits=c(dFMmin, dFMmax), expand = c(0, 0)) +
      xlab('Within-individual distance (x)') + ylab('Observed Between-individual distance') +
      guides(color="none", fill="none") + 
      coord_equal() + theme(aspect.ratio=1) +
      theme_bw() 
  }
  else if (ptype == 'All'){
    p <- ggplot(dFM.df,aes(.data$wD, .data$bD)) + 
      geom_point(aes(colour=.data$FPsub, fill = .data$FPwithin), shape=21, size = point.size) + 
      geom_polygon(data = df_poly_above, aes(.data$x, .data$y), fill = "red", alpha = .2) +
      scale_color_manual(breaks = c("Identified", "Not identified"), values = c('orangered2', 'grey10')) + 
      scale_fill_manual(breaks = c("Identified", "Not identified"), values= c('#ff9987', 'grey80')) + 
      geom_abline(slope=1, intercept=0, color='forestgreen') +
      #annotate(geom="text", x=dFMmax*0.98, y=dFMmax*0.98, label='x=y', color="gray30", angle='45', size=5) +
      scale_x_continuous(limits=c(dFMmin, dFMmax), expand = c(0, 0)) + 
      scale_y_continuous(limits=c(dFMmin, dFMmax), expand = c(0, 0)) +
      xlab('Within-individual distance (x)') + ylab('Observed Between-individual distance (y)') +
      guides(color="none", fill="none") + 
      #labs(color = "Fingerprinting (sub)", fill="Fingerprinting (repetition)") +
      coord_equal() + theme(aspect.ratio=1) +
      theme_bw() 
  }
  else{
    cat("Specify the plot type: ptype=\"Discr\",\"FP\", \"FP2\", \"Discr+FP\", \"Discr+FP2\", \"All\" \n")
    cat("\"Discr\": Discriminability \n")
    cat("\"FP\": Fingerprinting, count by subjects: \n")
    cat("         for each sub_i, count as \"identified\" only when all the within-sub_i distance < all between-sub_i ditance \n")
    cat("\"FP2\": Fingerprinting, count by subjects_x_repetitions: \n")
    cat("         for each sub_i(p,q), between p-th and q-th repetitions, count as \"identified\" if d(sub_i(p), sub-i(q)) < all ( d(sub_i(p), sub_j_*), d(sub_i(q),sub_j_*), i~=j ) \n")
    cat("\"Discr+FP\": Show Discriminability (shaded) and Fingerprinting1 (red dots) in the same plot  \n")
    cat("\"Discr+FP2\": Show Discriminability (shaded) and Fingerprinting2 (red dots) in the same plot  \n")
    cat("\"All\": Show Discriminability (shaded), Fingerprinting1 (red dots with red borders), and Fingerprinting2 (red dots with black borders) in the same plot \n")
  }
  
  return(p)
} # ----------------------------------------------------------------------------------

#' convert distance matrix to a dataframe 
#'
#' convert distance matrix to a dataframe for \code{plot.distance_field_map}, which contains variables: distance, wD, bD
#' 
#' @param Dmax [n x n] distance matrix. Check \code{dist}
#' @param subID [n] a vector containing subject ID
#' 
#' @return df: a dataframe that can be used in \code{plot.distance_field_map}, which contains variables: distance, wD, bD
#' @references A Guide for Quantifying and Optimizing Measurement Reliability for the Study of Individual Differences. doi: https://doi.org/10.1101/2022.01.27.478100
#' @author Ting XU
#' @export
distance_field_map.df <- function(Dmax, subID){
  # write the distance matrix to a dataframe
  # label which within-sub d is from the same subj
  Dmax <- as.matrix(Dmax)
  N <- dim(Dmax)[1]
  if (is.null((subID)) || N!=length(subID) ) {
    stop('Invalid Input')
  }
  subID <- as.character(subID)
  uniqids <- unique(subID)
  nsub <- length(uniqids)
  
  countvec <- vector(mode="numeric",length=nsub)
  for (s in 1:nsub) {
    countvec[s] <- sum(uniqids[s] == subID) # total number of scans for the particular id
  }
  nrow <- 0
  for (s in 1:nsub) {
    nrow <- nrow + sum(countvec[-s])*(countvec[s]*countvec[s]-countvec[s])
  }
  
  wD <- array(NaN, nrow)
  bD <- array(NaN, nrow)
  sub <- as.character(array(NaN, nrow))
  FPwithin <- array("Not identified", nrow)
  
  count <- 1
  for (s in 1:nsub) {
    ind <- which(subID == uniqids[s]) # indices that are the same subject
    for (i in ind) {
      di <- Dmax[i,] # get the i-th row
      nb <- length(di) - length(ind)
      for (j in ind){
        if (i!=j){
          wD[count:(count+nb-1)] <- rep(Dmax[i,j], nb)
          bD[count:(count+nb-1)] <- di[-ind]
          sub[count:(count+nb-1)] <- uniqids[s]
          
          # count identification (FPwithin)
          dj <- Dmax[j,] # get the i-th row
          db <- c(di[-ind], dj[-ind])
          if (Dmax[i,j] < min(db)){
            FPwithin[count:(count+nb-1)] <- "Identified"
          }
          
          count <- count + nb
        }
      }
    }
  }
  
  df <- data.frame(wD=wD, bD=bD, subID=sub, FPwithin=FPwithin, FPsub="Not identified")
  for (s in 1:nsub) {
    ind <- which(df$subID == uniqids[s]) # indices that are the same subject
    if(max(df$wD[ind]) < min(df$bD[ind])){df$FPsub[ind]="Identified"}
  }
  
  return(df)
} # ----------------------------------------------------------------------------------

