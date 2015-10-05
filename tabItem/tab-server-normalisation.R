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

dataNplots <- reactiveValues(
  max_plots = 5
)

#modal option pannel
################################################################################
# Select Input File
# Define  options modal dor normalisation module  
################################################################################

resetData <- reactive({
  normData$information = NULL
  userData$list_fdirectory = NULL
  userData$selected_file = NULL
  normData$beads = NULL #list of beads
  normData$dna = NULL # dna channel
  normData$df = NULL # dna channel
})

resetSelectInput <- reactive({
  updateSelectInput(session,"uselected_file",choices = fileNames() , selected = list())
  updateSelectInput(session, "nbeads_channel", choices = list() )
  updateSelectInput(session, "ndna_channel", choices = list() )
})


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

  
