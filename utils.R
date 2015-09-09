list.of.packages <-
  c(
    "ggplot2", "Rcpp", "metricsgraphics","RColorBrewer", "dplyr", "flowCore", "tidyr",
    "Rtsne","shiny","shinyFiles", "Cairo"
  )
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages))
  install.packages(new.packages)


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

outputDir <- "data" # directory with .fcs files
user_dataset_names <- list() # store filenames for the raws

#return filenames in
#' Title
#'
#' @return
#' @export
#'
#' @examples
load_files_from_dir <- function() {
  filenames <-
    list.files(outputDir, pattern = "*.fcs$") # list of filenames
  user_dataset_names <- c(user_dataset_names, filenames)
  return(user_dataset_names)
}

removeFiles <- function(filename) {
  unlink(filename, recursive = F, force = F)
  #file.remove(filename)
}


#' Title
#'
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
getFlowFrame <- function(file) {
  # check that the user selected an input
  if (is.null(file)) {
    return()
  }else{
    default.fcs <-
      read.FCS(paste(outputDir, "/", input$select_files, sep = ""))
    fcs.matrix <- exprs(default.fcs)
    fcs.df <- as.data.frame.matrix(fcs.matrix)
    information <- dim(default.fcs)
    
    return(v$df)
  }
}

