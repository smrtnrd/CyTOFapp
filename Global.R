list.of.packages <-
  c("ggplot2", "Rcpp", "metricsgraphics","RColorBrewer", "dplyr", "flowCore", "tidyr",
    "Rtsne","shiny","shinyFiles", "Cairo")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages))
  install.packages(new.packages)

max_length <- 5

library(metricsgraphics)
library(RColorBrewer)
library(dplyr)
library(DT)
library(ggplot2)
library(flowCore)
library(tidyr)
library(Rtsne)
library(shiny)
library(shinyFiles)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(shinydashboard)
library(shinyBS)