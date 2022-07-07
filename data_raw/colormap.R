cmap_icc_list <- list(c(0, '#7b1c43'),  c(0.2, '#43435f'), c(0.3, "#5e5f91"), c(0.4, "#9493c8"), 
                      c(0.5, "#64bc46"), c(0.6, "#54b24c"), c(0.7, "#f6eb2b"), c(0.8, "#f5a829"), 
                      c(0.9, "#f07e27"), c(1, "#ec3625"), c(1, "#ec3625"))

# colormap for gradient flow
df.tmp <- read.csv('data_raw/colormap/colormap_reliability_gradient_flow.csv')
cmap.gradient.flow <- 255*as.matrix(df.tmp)
