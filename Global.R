# load packages
library("shiny"); packageVersion("shiny")
library("shinyFiles"); packageVersion("shinyFiles")
library("shinydashboard"); packageVersion("shinydashboard")
library("shinyBS"); packageVersion("shinyBS")
library("shinythemes"); packageVersion("shinythemes")
library("flowCore"); packageVersion("flowCore")
library("ggplot2"); packageVersion("ggplot2")
library("data.table"); packageVersion("data.table")
library("gridExtra"); packageVersion("gridExtra")
library("metricsgraphics"); packageVersion("metricsgraphics")
library("RColorBrewer");packageVersion("RColorBrewer")
library("dplyr");packageVersion("dplyr")
library("tidyr");packageVersion("tidyr")
library("Rtsne");packageVersion("Rtsne")
library("Cairo"); packageVersion("Cairo")  # For nicer ggplot2 output when deployed on Linux
# Default options for app startup
#TODO: source("core/default-parameters.R", local = TRUE)

# For pasting times into things
simpletime = function(){gsub("\\D", "_", Sys.time())}

# funciton for moving files 
move_file <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir))
    dir.create(todir, recursive = TRUE)
  file.copy(from = from,  to = to)
}
