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
