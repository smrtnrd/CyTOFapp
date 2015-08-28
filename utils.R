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

# TODO: create a function transform_data

# Use ggplot2 to create dot plot
smoothScatterPlot <- function(fcs.df, X_channel, Y_channel, updateProgress = NULL ){

  Sys.sleep(0.25)

  # If we were passed a progress update function, call it
  if (is.function(updateProgress)) {
    text <- paste0( "10%")
    updateProgress(detail = text)
  }

  if (is.function(updateProgress)) {
        text <- paste0("20%")
        updateProgress(detail = text)
  }

  Sys.sleep(0.25)
  # data frame for ploting, adding a collor pallette

  x <- fcs.df[[X_channel]]
  y <- fcs.df[[Y_channel]]

  if (is.function(updateProgress)) {
    text <- paste0("30%")
    updateProgress(detail = text)
  }

  Sys.sleep(0.25)

  df.plot <- data.frame( x, y,
                    d = densCols(x, y,
                    colramp = colorRampPalette(c("blue", "orange", "red"),
                    space = "Lab")))


                    if (is.function(updateProgress)) {
                      text <- paste0("50%")
                      updateProgress(detail = text)
                    }
  Sys.sleep(0.25)
  # create the plot
  heat_map_plot <- ggplot(df.plot) +
  geom_point(aes(x, y, col = d), size = 1) +
  labs(x = X_channel, y = Y_channel ) +
  scale_color_identity() +
  theme_bw()

  if (is.function(updateProgress)) {
    text <- paste0("70%")
    updateProgress(detail = text)
  }

  Sys.sleep(0.25)
  if (is.function(updateProgress)) {
    text <- paste0( " Heat Map Created")
    updateProgress(detail = text)
  }

  return(heat_map_plot)

}

# Use ggplot2 to create dot plot
dotPlot <- function(fcs.df, X_channel, Y_channel, updateProgress = NULL){
  #prepare the df for ploting
  x <- fcs.df[[X_channel]]
  y <- fcs.df[[Y_channel]]

  df.plot <- data.frame(x,y)

  Sys.sleep(0.25)
  if (is.function(updateProgress)) {
    text <- paste0( "50%")
    updateProgress(detail = text)
  }

  p <- ggplot(df.plot, aes(x, y))+
  geom_jitter(position = position_jitter(width=.1)) +
  labs(x = X_channel, y = Y_channel ) +
  theme_bw()
  Sys.sleep(0.25)
  if (is.function(updateProgress)) {
    text <- paste0( "80%")
    updateProgress(detail = text)
  }



  Sys.sleep(0.25)
  if (is.function(updateProgress)) {
    text <- paste0( "Dot pot Created")
    updateProgress(detail = text)
  }
  return(p)
}
