
#' colormap of the icc gradient flow
#' colormap of the icc gradient flow (circular color map)
#' @return [360 x 3] a RGB color map
#' @export
colormap.gradient.flow <- function(){
  # colormap for gradient flow
  # raw colormap is saved in data_raw/colormap/colormap_reliability_gradient_flow.csv
  # > df.tmp <- read.csv('data_raw/colormap/colormap_reliability_gradient_flow.csv')
  # > cmap.gradient.flow <- 255*as.matrix(df.tmp)
  return(cmap.gradient.flow)
}

#' RGB to Hex conversion
#' RGB to Hex conversion
#' @param cmap.rgb [n x 3] a RGB color matrix
#' @return a Hex color vector 
#' @importFrom grDevices rgb
#' @export
rgb2hex <- function(cmap.rgb){
  # convert RGB colormap (nx3 matrix) to a HEX color vector
  # Input: cmap.rgb [n x 3] RGB matrix
  # Output HEX color vector
  # author Ting Xu
  cmap.hex <- character(dim(cmap.rgb)[1])
  for (i in 1:dim(cmap.rgb)[1]){
    cmap.hex[i] <- rgb(cmap.rgb[i,1], cmap.rgb[i,2], cmap.rgb[i,3], maxColorValue = 255)
  }
  return(cmap.hex)
}

#' Hex to RGB conversion
#' Hex to RGB conversion
#' @param cmap.hex a Hex color vector
#' @return [n x 3] a RGB color matrix
#' @importFrom grDevices col2rgb
#' @export
hex2rgb <- function(cmap.hex){
  # convert a HEX color vector to a RGB colormap (nx3 matrix) 
  # Input cmap.hex a color vector 
  # Output [n x 3] RGB matrix
  # author Ting Xu
  cmap.rgb <- matrix(0, nrow=length(cmap.hex), ncol=3)
  for (i in 1:length(cmap.hex)){cmap.rgb[i,]<-as.vector(col2rgb(cmap.hex[i]))/255}
  return(cmap.rgb)
}

#' color.brewer.n
#' select a subset color from RColorBrewer palettes
#' @param n.color a integer - the number of subset color
#' @param pla.name a character - a color palette name (default: 'Set2') from RColorBrewer palettes Check \code{RColorBrewer::display.brewer.all()}
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @return a Hex color vector
color.brewer.n <- function(n.color=NULL, pla.name="Set2"){
  # select a subset color from RColorBrewer palettes
  # Input n.color number of subset color
  # Input pal.name color palette name from RColorBrewer palettes
  # Output colormap list
  # author Ting Xu
  # note check all palettes: RColorBrewer::display.brewer.all()
  if (n.color<=brewer.pal.info[pla.name,]$maxcolors){
    cmap <- RColorBrewer::brewer.pal(brewer.pal.info[pla.name,]$maxcolors, pla.name)
    cmap <- cmap[1:n.color]
  }
  else{
    stop("# of colors of palette selected < number of colors required")
  }
  return(cmap)
}
