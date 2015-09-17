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

# action button ==============================================================
observeEvent(input$inputFiles,{
  # name size type datapath ===================
  listFiles <- input$inputFiles
  lapply(listFiles, function(x) {
    file.copy(
      from = paste0(listFiles$datapath) , to = paste0(getwd(),"/",fcs_user_dir,"/",listFiles$name, sep = "")
    )
  })
  isolate({
    dataset <- load_files_from_dir()
    updateSelectInput(session,"select_files",choices = dataset, selected = list())
    updateSelectInput(session, "Xchannel", choices = list() )
    updateSelectInput(session, "Ychannel", choices = list() )
    norm$user.files <- NULL
    norm$bead.channel <- NULL #list of beads
    v$list_name <- NULL # dna channel
    v$df <- NULL
    v$selected_input_file <- NULL
  })
})



observe({
  if (!is.null(input$select_files) || !is.null(v$selected_input_file )) {
    default.fcs <- read.FCS(paste(fcs_user_dir, "/", input$select_files, sep = ""))
    fcs.matrix <- exprs(default.fcs)
    fcs.df <- as.data.frame.matrix(fcs.matrix)
    v$information <- dim(default.fcs)
    isolate({
      if(is.null(norm$bead.channel) || v$selected_input_file != input$select_files ){
        v$list_name <- colnames(fcs.df)
        updateSelectInput(session, "Xchannel", choices = v$list_name , selected = list())
        updateSelectInput(session, "Ychannel", choices = v$list_name , selected = list())
      }
    })
    v$df <- fcs.df
    v$selected_input_file <- input$select_files
  }
  
})



observe({
  if(input$remove_file > 0){
    isolate({
      filename <- paste0(fcs_user_dir,"/",input$select_files, sep = "")
      removeFiles(filename)
      updateSelectInput(session, "Xchannel", choices = list())
      updateSelectInput(session, "Ychannel", choices = list())
      updateSelectInput(session, "select_files", choices = load_files_from_dir(), selected = list())
      v$df <- NULL
      v$X <- NULL
      v$Y <- NULL
      v$sample <- NULL
      v$information <- NULL
      v$selected_input_file <- NULL
    })
  }
  
})






doPlots <- eventReactive(input$apply_modif,{
  sample.n <- as.numeric(input$sample_data)
  if (as.numeric(v$information[[1]]) < sample.n) {
    sample.n <- round(0.8 * v$information[1], 0)
  }
  
  # set the variable
  v$sample <- v$df[sample(nrow(v$df), sample.n),]
  v$X <- input$Xchannel
  v$Y <- input$Ychannel
  
  
  df.X <- select(v$sample, one_of(v$X))
  df.Y <- select(v$sample, one_of(v$Y))
  df.XY <- select(v$sample, one_of(v$X, v$Y)) # select column
  
  #transform options ==========================================================
  
  v$sample <- df.XY
})

output$plot1 <- renderPlot({
  doPlots()
  
  # setting variable
  if (input$X_transf == "asinh") {
    x <- asinh(v$sample[[v$X]] / 5)
  }else{
    x <- v$sample[[v$X]]
  }
  if (input$Y_transf == "asinh") {
    y <- asinh(v$sample[[v$Y]] / 5)
  }else{
    y <- v$sample[[v$Y]]
  }
  
  X_channel <- v$X
  cat(paste("y", unlist((head(
    y
  ))),"\n"))
  Y_channel <- v$Y
  
  df.plot <- data.frame(x, y,
                        d = densCols(x, y,
                                     colramp =  colorRampPalette(rev(
                                       rainbow(10, end = 4 / 6)
                                     ))))
  # plot setting options =======================================================
  if (input$plot_Setting == "HeatMap") {
    ggplot(df.plot, aes(x, y, col = d)) +
      #geom_point(alpha = 0.3) +
      geom_jitter(position = position_jitter(width = .8), alpha = 0.3) +
      labs(x = X_channel, y = Y_channel) +
      scale_color_identity() +
      theme_bw() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  }else{
    ggplot(df.plot, aes(x, y)) +
      geom_jitter(position = position_jitter(width = 2), alpha = 0.3) +
      labs(x = X_channel, y = Y_channel) +
      theme_bw() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  }
})

#output information
output$info_data_xy <- renderText({
  xy_range_str <- function(e) {
    if (is.null(e))
      return("Select variable on the plot\n")
    paste0(
      "X min= ", round(e$xmin, 1), " X max= ", round(e$xmax, 1),
      " Y min= ", round(e$ymin, 1), " Y max= ", round(e$ymax, 1)
    )
  }
  paste0("Gate selected : ", xy_range_str(input$plot1_brush), "\n")
})

output$info_data_plot <- renderText({
  if (is.null(input$select_files))
    return()
  paste0("Dataset :", v$information[[1]], " cells and ",v$information[[2]]," observables \n")
})


output$filepaths <- renderText({
  if (v$input_file) {
    files <- input$files
    if (length(files) > 1) {
      paste0("all files were added to the dataset \n")
    }else{
      paste0(files$name, "was added to the dataset \n")
    }
  } else{
    paste0("File selected :", input$select_files, "\n")
  }
  
})