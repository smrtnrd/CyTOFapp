library(shiny)
library(shinydashboard)
#TODO(Buildman): installer flowCore dans R-Studio
#library(flowCore)

##
# Local file system
#
##

# get current options

#Global variable
outputDir <- "FCSfile"


setCurrentOption <- function(caption){
  if(caption == "Heat Map"){
    return("Heat Map")
  }else{
    return("Dot Plot")
  }
}

#####
#
# Manipulate files
#
####

#add file to the Dataset
saveDataTemp <- reactive({
    #TODO(not urgent): add a message to user that he needs add a file
    if(is.null(input$FCSfile)){return(data.frame())}
    FCS.file <- input$FCSfile
    write.FCS (
      x = FCS.file,
      file = file.path(outputDir, FCS.file$name)
      )
  })

filenames <- function(){
    list.files(outputDir, pattern="\\.fcs$")
}


Dataset <- reactive({
  # Read all files into a list
  files <- list.files(outputDir, full.names = TRUE)
  dataset <- lapply(files, read.FCS, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  # data <- do.call(rbind, data)
  return(dataset)
})

getFCS <- function(file){
    fcs.file <- read.FCS(file)
    fcs.matrix <- exprs(fcs.file)
    return(fcs.matrix)
}
