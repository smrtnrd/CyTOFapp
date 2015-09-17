################################################################################
# Options, default settings, and load packages
################################################################################
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100*1024^2)
# Set Shiny Reaction Log to TRUE
options(shiny.reactlog=TRUE)
# Default ggplot2 theme (Only relevant if panel-specific theme missing or NULL)
theme_set(theme_bw())
# Run the auto-installer/updater code:
source("install.R", local = TRUE)

################################################################################
# Begin Shiny Server definition.
################################################################################
# First store the inventory of objects (for provenance record)
shinyFlowCoreServerObjectsList = ls()

remove_files <- function(filename) {
  unlink(filename, recursive = F, force = F)
}

shinyServer(function(input, output, session) {
  
  # Header 
  source("header/header-server-dashboard.R", local = TRUE)
  
  ########################################
  # Reactive UI Definition of Variables
  ########################################
  
  # store information about the user
  userData <- reactiveValues(
    information = NULL,
    list_fdirectory = NULL
  )
  
  
  # directory with .fcs files
  fcs_user_dir <- "data" 
  
  fileNames <-  reactive({
      fileNames <-list.files(outputDir, pattern = "*.fcs$") # list of filenames
      return(fileNames)
  })

  
  
  
  volumes <- getVolumes() #c('R Installation'=R.home())
  
  v <- reactiveValues(
    df = NULL,
    X = NULL,
    Y = NULL,
    sample = NULL,
    information = NULL,
    selected_input_file = NULL,
    input_file = FALSE,
    x.channel = NULL,
    y.channel = NULL
  )
  
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
  
  
  # normalisation module
  source("tabItem/tab-server-normalisation.R", local = T)
  # visualisation module
  source("tabItem/tab-server-visualisation.R", local = T)
})
