library(shiny)
library(shinydashboard)
#TODO(Buildman): installer flowCore dans R-Studio
#library(flowCore)

##
# Local file system
#
##

# get current options

setCurrentOption <- function(caption){
  if(caption == "Heat Map"){
    return("Heat Map")
  }else{
    return("Dot Plot")
  }

}

outputDir <- "responses"

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName),
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}
