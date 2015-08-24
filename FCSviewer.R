library(flowCore)
library(Rtsne)
library(metricsgraphics)
library(dplyr)
library(ggplot2)

#test variables
#default.fcs <- read.FCS("FCSfile/Bendall_et_al_Science_2011_Marrow_1_SurfacePanel_Live_CD44pos_Singlets.fcs")
#fcs.matrix <- exprs(default.fcs)
#fcs.df <- as.data.frame.matrix(fcs.matrix)
#smoothScatterPlot(fcs.df, "Rh(102.905)-Dual", "Dy(163.929)-Dual")
#dotPlot(fcs.df, "Rh(102.905)-Dual", "Dy(163.929)-Dual")
#X.channel <- "Rh(102.905)-Dual"
#Y.channel <- "Dy(163.929)-Dual"

#produce an heat map
smoothScatterPlot <- function(fcs.df, X.channel, Y.channel){
  #for debbuging 
  print(X.channel)
  print(Y.channel)
  
  var <- c(X.channel, Y.channel)

  #data frame manipulation
  df <-   fcs.df %>%
          sample_n(5e4) %>%
          select(one_of(var)) %>%
          mutate_each(funs(asinh))

  #prepare the df for ploting
  x <- df[[X.channel]]
  y <- df[[Y.channel]]
  
  #data frame for ploting, adding a collor pallette
  df.plot <- data.frame( x, y, 
                    d = densCols(x, y, 
                    colramp = colorRampPalette(c("blue", "orange", "red"),
                    space = "Lab")))
  print(tbl_df(df.plot))
  
  p <- ggplot(df.plot) +
  geom_point(aes(x, y, col = d), size = 1) +
  scale_color_identity() +
  theme_bw()
  
  print(p)
}

#produce dot plot for bins
dotPlot <- function(fcs.df, X.channel, Y.channel){
  #for debbuging 
  print(X.channel)
  print(Y.channel)
  
  var <- c(X.channel, Y.channel)
  
  #data frame manipulation
  df <-   fcs.df %>%
    sample_n(5e4) %>%
    select(one_of(var)) %>%
    mutate_each(funs(asinh))
  
  #prepare the df for ploting
  x <- df[[X.channel]]
  y <- df[[Y.channel]]
  
  df.plot <- data.frame(x,y)
  
  print(tbl_df(df.plot))
  
  p <- ggplot(df.plot, aes(x, y))+
  geom_jitter(position = position_jitter(width=.1)) +
  theme_bw()
  print(p)
}
