#Version 0.2.0
source("assets/carouselPanel.R")
source("assets/viewer-module.R")
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100 * 1024 ^ 2)



outputDir <- "data" # directory with .fcs files
user_dataset_names <- list() # store filenames for the raws



my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir))
    dir.create(todir, recursive = TRUE)
  file.(from = from,  to = to)
}



shinyServer(function(input, output, session) {
  # notification ==================================================================================================
  notifs.msg <- data.frame(matrix(vector(), 0, 2, dimnames = list(c(), c("text", "status"))), stringsAsFactors =  F)
  tasks.msg <- data.frame(matrix(vector(), 0, 3, dimnames = list(c(), c("text","value", "color"))), stringsAsFactors = F)
  notifs.msg[nrow(notifs.msg) + 1,] <- list("thi is a test", "info")
  tasks.msg[nrow(tasks.msg) + 1,] <- list("Creating Flowset",50, "blue")
  
  checkData <- reactiveTimer(intervalMs = 1000, session)
  
  addNotif <- function(text,status, notifs.msg) {
    l <- nrow(notifs.msg) + 1
    if (is.null(notifs.msg)) {
      notifs.msg[l,] <- list("info","info")
    }else{
      notifs.msg[l,] <- list(text,status)
    }
  }
  
  addTask <- function(text, value, color, tasks.msg) {
    if (is.null(tasks.msg)) {
      tasks.msg[1,] <- list("tst",80 ,"Red")
    }else{
      l <- nrow(tasks.msg) + 1
      tasks.msg[l,] <- list(text,value,color)
      cat(paste(tasks.msg))
    }
  }
  
  
  updateTask <- function(id, value, text = NULL, tasks.msg) {
    if (is.null(tasks.msg)) {
      tasks.msg$value[id] <- value
      if (!is.null(text)) {
        tasks.msg$text[id] <- text
        cat(paste(tasks.msg$text[id],"\n\n\n\n\n\n\n\n"))
      }
    }
  }
  
  # reactive values ============================================================
  ranges <- reactiveValues(x = NULL, y = NULL) # zoomable plot
  volumes <- getVolumes() #c('R Installation'=R.home())
  v <- reactiveValues(
    df = NULL,
    X = NULL,
    Y = NULL,
    sample = NULL,
    information = NULL,
    selected_input_file = NULL,
    input_file = FALSE,
    messNotif = NULL,
    index = NULL,
    nrow.notif = NULL,
    nrow.task = NULL,
    tasks = "",
    notifs = "",
    sX = NULL,
    xY = NULL
  )
  
  
  # notification ===============================================================
  output$notifications <- renderMenu({
    #checkData()
    #DropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(
      type = "notifications", .list = doNotifs(), icon = icon("fa-question")
    )
    
    
  })
  
  
  doNotifs <- reactive({
    if (nrow(notifs.msg) > 0) {
      # Code to generate each of the messageItems here, in a list. This assumes
      # that messageNotif is a data frame with 3 columns, 'text', 'icon' and 'status'.
      notifs <- apply(notifs.msg, 1, function(row) {
        notificationItem(text = row["text"],  status = row["status"])
      })
    }
  })
  
  output$tasks <- renderMenu({
    # checkData()
    # This is equivalent to calling:
    # DropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "tasks",  badgeStatus = "success", .list = doTasks())
    
  })
  
  doTasks <- reactive({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageNotif is a data frame with 3 columns, 'text', 'icon' and 'status'.
    if (!is.null(v$nrow.task)) {
      tasks <- apply(tasks.msg, 1, function(row) {
        taskItem(text = row["text"], value = as.numeric(row["value"]),  color = row["color"])
      })
    }
  })
  
  
  # action button ==============================================================
  
  shinyFileChoose(
    input,
    'files',
    roots = volumes,
    session = session,

        restrictions = system.file(package = 'base')
  )
  
  # create a dataset from the input_file
  Dataset <- reactive({
    # no file input
    if (is.null(input$files)) {
      return(load_files_from_dir())
    }
    # name size type datapath ===================
    listFiles <- parseFilePaths(volumes, input$files)
    lapply(listFiles, function(x) {
      file.rename(
        from = paste0(listFiles$datapath) , to = paste0(getwd(),"/",outputDir,"/",listFiles$name, sep =
                                                          "")
      )
    })
    v$input_file <- TRUE
    dataset <- load_files_from_dir()
    return(dataset)
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
  
  observe({
    if (!is.null(input$select_files)) {
      isolate({
        v$selected_input_file <- input$select_files
        default.fcs <-
          read.FCS(paste(outputDir, "/", v$selected_input_file, sep = ""))
        fcs.matrix <- exprs(default.fcs)
        fcs.df <- as.data.frame.matrix(fcs.matrix)
        v$information <- dim(default.fcs)
        v$list_name <- colnames(fcs.df)
        v$df <- fcs.df
        updateSelectInput(
          session, "Xchannel", choices = v$list_name , selected = input$Xchannel
        )
        updateSelectInput(
          session, "Ychannel", choices = v$list_name , selected = input$Ychannel
        )
      })
      
    }
    
  
  })
  
  
  
  observeEvent(input$remove_one_file,{
    isolate({
      filename <- paste0(outputDir,"/",input$userDataset, sep = "")
      removeFiles(filename)
      updateSelectInput(session, "Xchannel", choices = list() , selected = list())
      updateSelectInput(session, "Ychannel", choices = list(), selected = list())
      updateSelectInput(session, "select_files", choices = load_files_from_dir(), selected = list())
      updateSelectInput(session, "userDataset", choices = load_files_from_dir(), selected = list())
    })
  })
  
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
  
  v1 <- reactiveValues(text = NULL, value = NULL, color = NULL)
  
  doPlots <- eventReactive(input$apply_modif,{
    # sampling data
    v1$text <- "Analyzing Data"
    v1$value <- 10
    v1$color <- "blue"
    
    addTask(v1$text, v1$value, v1$color, tasks.msg)
    v$nrow.task <- nrow(tasks.msg)
    cat("\n plotttting \n")
    Sys.sleep(0.01)
    
    sample.n <- as.numeric(input$sample_data)
    if (as.numeric(v$information[[1]]) < sample.n) {
      sample.n <- round(0.8 * v$information[1], 0)
    }
    
    updateTask(
      id = v$index, value = 20,text = paste("Analizing Data : Sampling"), tasks.msg
    )
    Sys.sleep(0.01)
    
    # set the variable
    v$sample <- v$df[sample(nrow(v$df), sample.n),]
    v$X <- input$Xchannel
    v$Y <- input$Ychannel
    
    updateTask(
      id = v$index, value = 30,text = paste("Analizing Data : Channel Transormation"), tasks.msg
    )
    Sys.sleep(0.01)
    
    df.X <- select(v$sample, one_of(v$X))
    df.Y <- select(v$sample, one_of(v$Y))
    df.XY <- select(v$sample, one_of(v$X, v$Y)) # select column
    
    #transform options ==========================================================
    
    v$sample <- df.XY
  })
  
  output$plot1 <- renderPlot({
    doPlots()
    updateTask(
      id = v$index, value = 50,text = paste("Creating Plot : Setting variable"), tasks.msg
    )
    Sys.sleep(0.01)
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
      updateTask(
        id = v$index, value = 100,text = paste("Creating Plot : Heat Map"),tasks.msg
      )
      addNotif("Heat Map created", status = "success",notifs.msg)
      v$nrow.notif < nrow(notifs.msg)
      Sys.sleep(0.01)
      
      
      
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
  
  
  
  ##########==================================
  max_plots <- 3
  norm <- reactiveValues(
    df = NULL,
    bead.channel = NULL, #list of beads
    dna.channel = NULL, # dna channel
    user.files = NULL,
    max_plots = NULL,
    information = NULL
  ) #list of file
  
  isolate({
    shinyFileChoose(input, 'normfiles', 
                  roots=c(root='.', volume = getVolumes() ), session=session, 
                  restrictions=system.file(package='base'))
  })
  
  observeEvent(input$normFiles,{
    # name size type datapath ===================
    listFiles <- input$normFiles
    lapply(listFiles, function(x) {
      file.copy(
        from = paste0(listFiles$datapath) , to = paste0(getwd(),"/",outputDir,"/",listFiles$name, sep =
                                                          "")
      )
    })
    isolate({
      dataset <- load_files_from_dir()
      updateSelectInput(session,"userFiles",choices = dataset, selected = list())
      updateSelectInput(session, "inDna", choices = list() )
      updateSelectInput(session, "inBeads", choices = list() )
      norm$user.files <- NULL
      norm$bead.channel <- NULL #list of beads
      norm$dna.channel <- NULL # dna channel
      norm$df <- NULL
      norm$max_plots <- NULL
      norm$information <- NULL
    })
  })
  
  #modal panel interaction setting parameters for normalisation
  observe ({
    if (!is.null(input$userFiles) || !is.null(norm$user.files)) {
      #create the fcs file
      default.fcs <- read.FCS(paste(outputDir, "/", input$userFiles, sep = ""))
      fcs.matrix <- exprs(default.fcs)
      fcs.df <- as.data.frame.matrix(fcs.matrix)
      norm$information <- dim(default.fcs)
      cat(paste0("fcs.df : ", head(fcs.df), "\n"))
       
      isolate({
        if(is.null(norm$bead.channel) || norm$user.files != input$userFiles ){
          norm$bead.channel <- colnames(fcs.df)
          updateSelectInput(session, "inBeads", choices = norm$bead.channel, selected = list())
        }
        if(is.null(norm$dna.channel) || norm$user.files != input$userFiles){
          norm$dna.channel <- colnames(fcs.df)
          updateSelectInput(session, "inDna", choices = norm$dna.channel, selected = list())
        }
      })
      
      max_length <- length(input$inBeads)
      norm$user.files <- input$userFiles # ! create the files 
      norm$df <- fcs.df
    }else{
        toggleModal(session, "modalOptions", toggle = "open")
    }
  })
  
  observe(
    if (input$removeFile > 0){
      isolate({
        filename <- paste0(outputDir,"/",input$userFiles, sep = "")
        removeFiles(filename)
        updateSelectInput(session,"userFiles",choices = load_files_from_dir(), selected = list())
        updateSelectInput(session, "inDna", choices = list() )
        updateSelectInput(session, "inBeads", choices = list() )
        norm$user.files <- NULL
        norm$bead.channel <- NULL #list of beads
        norm$dna.channel <- NULL # dna channel
        norm$df <- NULL
        norm$max_plots <- NULL
        norm$information <- NULL
      })
    }
  )
  
  observe({
    if(input$apply_norm > 0){
      isolate({
        toggleModal(session, "modalOptions", toggle = "toggle") 
      })
      
    }
  })
  
 #dynamic UI create the plots for the normalisation
lapply(1:max_length, function(i){
    
    output[[paste0('nplot', i)]] <- renderPlot({
      
      if(input$apply_norm>0){
        isolate({
          y.channel <- input$inDna
          x.channel <- input$inBeads[[i]]
          df.XY <- select(norm$df, one_of(x.channel, y.channel)) # select column
          
          sample.n <- 50000
          if (as.numeric(norm$information[[1]]) < sample.n) {
            sample.n <- round(0.8 * norm$information[1], 0)
          }
          
          df.sample <- df.XY[sample(nrow(df.XY), 10000),]
          x <- asinh(df.sample[[x.channel]]/ 5 )
          y <- asinh(df.sample[[y.channel]]/ 5 )
          
          cat(paste("head(y) ", head(y) ,"\n"))
          
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
      }
      
      
    })
    
  })

  
  
  outputOptions(output, "nplot1", suspendWhenHidden=FALSE)
})
