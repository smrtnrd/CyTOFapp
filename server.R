################################################################################
# Options, default settings, and load packages
################################################################################
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100*1024^2)
# Set Shiny Reaction Log to TRUE
#options(shiny.reactlog=TRUE)
# Default ggplot2 theme (Only relevant if panel-specific theme missing or NULL)
theme_set(theme_bw())
# Run the auto-installer/updater code:
source("install.R", local = TRUE)

# directory with .fcs files
fcs_user_dir <- "data" 

################################################################################
# Begin Shiny Server definition.
################################################################################
# First store the inventory of objects (for provenance record)
shinyFlowCoreServerObjectsList = ls()

shinyServer(function(input, output, session) {
  # Data Panel
  
  # Header 
  source("header/header-server-dashboard.R", local = TRUE)
  
  #handling data
  fileNames <-  reactive({
    fileNames <-list.files(outputDir, pattern = "*.fcs$") # list of filenames
    return(fileNames)
  })
  
  remove_files <- function(filename) {
    unlink(filename, recursive = F, force = F)
  }
  
  ########################################
  # Reactive UI Definition of Variables
  ########################################
  
  # store information about the user
  userData <- reactiveValues(
    information = NULL,
    list_fdirectory = NULL,
    selected_file = NULL
  )
  
  resetUserData <- reactive({
    userData$information = NULL
    userData$list_fdirectory = NULL
    userData$selected_file = NULL
  })
  
  resetSelectInput <- reactive({
    updateSelectInput(session,"uselected_file",choices = fileNames() , selected = list())
    updateSelectInput(session, "nbeads_channel", choices = list() )
    updateSelectInput(session, "ndna_channel", choices = list() )
  })
  
  
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

  ########################################
  # Reactive UI Definition of Variables
  ########################################
  
  # Define data-reactive variable lists
  normData <- reactiveValues(
    df = NULL, # data frame
    beads = NULL, #list of beads
    dna = NULL, # dna channel
    information = NULL # information
  ) 
  
  resetNormdata <- reactive({
    normData$beads = NULL #list of beads
    normData$dna = NULL # dna channel
    normData$df = NULL # dna channel
  })
  
  dataNplots <- reactiveValues(
    max_plots = 5
  )
  
  #modal option pannel
  ################################################################################
  # Select Input File
  # Define  options modal dor normalisation module  
  ################################################################################
  

  
  # upload file in norm mudule
  observeEvent(input$upload_nfile,{
    # file upload 
    listFiles <- input$upload_nfile
    #move (copy) the files in ther "server" dircetory
    lapply(listFiles, function(x) {
      file.copy(
        from = paste0(listFiles$datapath) , to = paste0(getwd(),"/",fcs_user_dir,"/",listFiles$name, sep = "")
      )
    })
    
    isolate({
      #get all the files in the local directoris
      userData$list_fdirectory <- fileNames()
      
      # reactive ui update the values according to user input
      resetSelectInput()
      
      # reset reactivevalues
      resetData()
    })
  })
  
  ################################################################################
  # Input Select Files 
  # Update selectInput channels according the user selected file
  ################################################################################
  
  observe ({
    if (!is.null(input$uselected_file) || !is.null(userData$selected_file)) {
      cat(paste("userData$selected_file : ",input$uselected_file, "\n"))
      #create the fcs file
      default.fcs <- read.FCS(paste(outputDir, "/", input$uselected_file, sep = ""))
      fcs.matrix <- exprs(default.fcs)
      fcs.df <- as.data.frame.matrix(fcs.matrix)
      normData$information <- dim(default.fcs)
      
      
      # update channels
      isolate({
        #check that user selected beads or the selected file that the user selection
        if(is.null(normData$beads) || userData$selected_file != input$uselected_file ){
          normData$beads <- colnames(fcs.df)
          updateSelectInput(session, "nbeads_channel", choices = normData$beads, selected = list())
        }
        if(is.null(normData$dna) || userData$selected_file != input$uselected_file ){
          normData$dna <- colnames(fcs.df)
          updateSelectInput(session, "ndna_channel", choices = normData$dna, selected = list())
        }
      })
      
      dataNplots$max_plots <- length(normData$beads)
      userData$selected_file <- input$uselected_files # ! create the files 
      normData$df <- fcs.df
    }else{
      cat(paste("userData$selected_file : ","input$uselected_file", "\n"))
      toggleModal(session, "norm_options", toggle = "open")
    }
  })
  
  
  ################################################################################
  # Remove button
  # Update selectInput channels according the user selected file
  ################################################################################
  observe(
    if (input$abutton_remove_file > 0) {
      isolate({
        filename <- paste0(outputDir,"/",input$uselected_files, sep = "")
        remove_files(filename)
        resetData()
        resetSelectInput()
      })
    })
  
  
  ################################################################################
  # Remove button
  # Update selectInput channels according the user selected file
  ################################################################################
  observe({
    if(input$abutton_run_norm > 0){
      isolate({
        toggleModal(session, "norm_options", toggle = "toggle") 
      })
    }
  })
  
  output$plots <- renderUI({
    if(!is.null(input$nbeads_channel)){
      dataNplots$max_plots = length(input$nbeads_channel)
    }
    
    nplots <- lapply(1:dataNplots$max_plots, function(i){
      plotname <- paste("nplot", i, sep="")
      plotOutput(plotname, height = 280, width = 250) 
    })
    
    do.call(tagList,nplots)
  })
  
  observe({
    #dynamic UI create the plots for the normalisation
    if(input$abutton_run_norm > 0){
      for (i in 1:dataNplots$max_plots){
        local({
          output[[paste0('nplot', i)]] <- renderPlot({
            isolate({
              y.channel <- normData$dna
              x.channel <- normData$beads[[i]]
              df.XY <- select(normData$df, one_of(x.channel, y.channel)) # select column
              
              sample.n <- 50000
              if (as.numeric(userData$information[[1]]) < sample.n) {
                sample.n <- round(0.8 * userData$information[1], 0)
              }
              
              df.sample <- df.XY[sample(nrow(df.XY), 10000),]
              x <- asinh(df.sample[[x.channel]]/ 5 )
              y <- asinh(df.sample[[y.channel]]/ 5 )
              
              df.plot <- data.frame(x, y,
                                    d = densCols(x, y,
                                                 colramp =  colorRampPalette(rev(
                                                   rainbow(10, end = 4 / 6)
                                                 ))))
              ggplot(df.plot, aes(x, y, col = d)) +
                geom_jitter(position = position_jitter(width = .8), alpha = 0.3) +
                labs(x = x.channel, y = y.channel) +
                scale_color_identity() +
                theme_bw() +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y)
            })
          })
        })
      }
    }
    
    # lapply(1:5, function(i){
    #  outputOptions(output, paste0("nplot",i, sep=""), suspendWhenHidden=FALSE)
    # })
    
  })
  ########################################
  # Info
  ########################################
  
  output$nplots <- renderText({
    paste0(length(input$nbeads_channel))
  })
  
  output$nbeads <- renderText({
    paste0(input$nbeads_channel)
  })
  
  
  

  
  # visualisation module
  #source("tabItem/tab-server-visualisation.R", local = T)
})
