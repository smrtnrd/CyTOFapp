# Dependent libs
library(metricsgraphics)
library(dplyr)
library(DT)
library(ggplot2)
library(flowCore)
library(tidyr)
library(Rtsne)



# This function takes a number and returns a compressed string (e.g. 1624 => 1.6K or 2K, depending on round.by)
compress <- function(x, round.by = 2) {
  # by StackOverflow user 'BondedDust' : http://stackoverflow.com/a/28160474
  div <- findInterval(as.numeric(gsub("\\,", "", x)), c(1, 1e3, 1e6, 1e9, 1e12) )
  paste(round( as.numeric(gsub("\\,","",x))/10^(3*(div-1)), round.by), c("","K","M","B","T")[div], sep = "" )
}


# Use ggplot2 to create dot plot
smoothScatterPlot <- function(fcs.df, X_channel, Y_channel){

  XY_channels <- c(X_channel, Y_channel)

  # data frame manipulation
  df <-   fcs.df %>%
          sample_n(5e4) %>% # sampling the data
          select(one_of(XY_channels)) %>% # select columns
          mutate_each(funs(asinh)) # transform each column function

  # data frame for ploting, adding a collor pallette

  x <- df[[X_channel]]
  y <- df[[Y_channel]]

  df.plot <- data.frame( x, y,
                    d = densCols(x, y,
                    colramp = colorRampPalette(c("blue", "orange", "red"),
                    space = "Lab")))

  print(tbl_df(df.plot))

  # create the plot
  heat_map_plot <- ggplot(df.plot) +
  geom_point(aes(x, y, col = d), size = 1) +
  labs(x = X_channel, y = Y_channel ) +
  scale_color_identity() +
  theme_bw()

  print(heat_map_plot)
}

# Use ggplot2 to create dot plot
dotPlot <- function(fcs.df, X_channel, Y_channel){
  #for debbuging
  print(X_channel)
  print(Y_channel)

  var <- c(X_channel, Y_channel)

  #data frame manipulation
  df <-   fcs.df %>%
    sample_n(5e4) %>%
    select(one_of(var)) %>%
    mutate_each(funs(asinh))

  #prepare the df for ploting
  x <- df[[X_channel]]
  y <- df[[Y_channel]]

  df.plot <- data.frame(x,y)

  print(tbl_df(df.plot))

  p <- ggplot(df.plot, aes(x, y))+
  geom_jitter(position = position_jitter(width=.1)) +
  labs(x = X_channel, y = Y_channel ) +
  theme_bw()
  print(p)
}

