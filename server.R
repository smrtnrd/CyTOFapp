#Version 0.2.0

source("utils.R")
source("assets/carouselPanel.R")
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.



options(shiny.maxRequestSize = 100*1024^2)
id <- 1 #index for plot
outputDir <- "FCSfile" # directory with .fcs files
user_dataset_names <- list() # store names for the raws
#filename = c( plotID =NULL, df = NULL, X_channel = NULL, Y_channel = NULL)
user_session <- list()
#save file in  to FCS directory
add_file_to_dir <- function(file){
  file.copy(from = file, to = outputDir)
}

#return filenames in
load_files_from_dir <- function(){
  filenames <- list.files(outputDir, pattern="\\.fcs$") # list of filenames
  return(filenames)
}

#create a dataset that contain all the data of a session
save_dataset <- function(filename){
    if(filename %in% user_dataset_names) return()
    # increment id
    id <- id + 1

    user_dataset_names <- c(user_datasete_names, filename)

    #read FCS file
    fcs.flowFrame <- read.FCS(fileName, dir = outputDir)

    #setting the variables
    sd.filename <- fileName
    sd.df <- as.data.frame.matrix(exprs(fcs.flowFrame))
    sd.colnames <- colnames(sd.df)
    sd.data <- c(paste("plot", id, sep="" ),
                 sd.df ,
                 sd.colnames,
                 sd.colnames)

    #append the current data to the session
    user_session <- c(user_session, sd.filename = c( plotID = paste("plot", id, sep = ""),
                                                     df = sd.df,
                                                     X_channel = sd.colnames,
                                                     Y_channel = sd.colnames))
    }


shinyServer(function(input, output, session) {
  #####---------

  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)


  # FCS viewer Module
  # reactive values to plot function
  v <- reactiveValues(df = NULL,
                      X = NULL,
                      Y = NULL,
                      information = NULL,
                      plot = NULL, # action button to plot
                      selected_input_file = NULL )


  # give us the object flowFrame
getFlowFrame <- reactive({
  # check that the user selected an input
  if(is.null(v$selected_input_file)){
      return()
    } else {
      cat("getFlowFrame if \n")
      cat(paste(outputDir, "/", input$select_files))
      default.fcs <- read.FCS(paste(outputDir, "/", input$select_files, sep = ""))
      cat("getFlowFrame fcs file \n")

      fcs.matrix <- exprs(default.fcs)
      cat("getFlowFrame X \n")
      fcs.df <- as.data.frame.matrix(fcs.matrix)
      v$information <- dim(default.fcs)
      v$df <- fcs.df
      return(fcs.df)
    }
})



#UI select dataset
  output$selectFiles <- renderUI({
    selectInput("select_files",
    "Select the dataset that you want to work with",
    choices = load_files_from_dir(), #output file from de directory
    size = 10,
    width = "100%",
    selectize = FALSE,
    selected = load_files_from_dir()[1]
    )
  })

  #my idea here is to create an input channel
  observeEvent(input$select_files, {
     v$selected_input_file <- input$select_files
     #save the data if it doesn't exist in the data frame
    })

#Select Channel
  output$selectX <- renderUI({
    #check if the user has selected a dataset
    selectInput("Xchannel",
    "Select your X channel",
    size = 4,
    width = "100%",
    selectize = FALSE,
    # I want to link an index to
    # TODO: get the colnames from the current plot
    choices = colnames(getFlowFrame())
    )
  })



  output$transformX <- renderUI({
     selectInput("X_transf",
                 "select a parameter",
                  choices = c("asinh", "no transformation"),
                  width = "100%",
                  selected = "no transformation" )
    })

 output$transformY <- renderUI({
   selectInput("Y_transf",
               "select a function",
                choices = c("asinh", "no transformation"),
                width ="100%",
                selected = "no transformation" )
   })


  output$selectY <- renderUI({
    selectInput("Ychannel",
    "Select your Y channel",
    size = 4,
    width = "100%",
    selectize = FALSE,
    choices = as.character(colnames(getFlowFrame()))
    )
  })


  observeEvent(input$FCSfile, {
       #check that the uploaded file equals to FCS
       if(is.null(input$FCSfile)) return()
       inFile <- input$FCSfile

       #save the data if it doesn't exist in the data frame
       #if((inFile %in% load_files_from_dir()) return()
       add_file_to_dir(inFile$datapath)
       cat("File is added to the directory \n")
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

doPlots <- eventReactive(input$apply_modif,{
    sample_n <- 5e4
    if (as.numeric(v$information[[1]]) < 5e4){
      sample_n <- round(0.8 * v$information[1], 1)
    }
 cat(paste("cells :", v$information[[1]], " sample ", sample_n ))
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Create Plot", value = 0)

    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
        df <- sample_n(v$df, sample_n)
        X = input$Xchannel
        Y = input$Ychannel

  #transform the data
  df.XY <- select(df, one_of(X, Y)) # select column

 isolate({
   if (input$X_transf == "asinh" ){
     transform(df.XY, X = asinh(df.XY[X])) # transform each column function
     cat(paste("\n asinh transformation for the variable " , colnames(df.XY), "\n" ))
     }

   if (input$Y_transf == "asinh" ){
     transform(df.XY, Y = asinh(df.XY[Y])) # transform each column function
     cat(paste("asinh transformation for the variable " , colnames(df.XY), "\n" ))
     }
   })



  # Create a closure to update progress.
  # Each time this is called:
  # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
  #   distance. If non-NULL, it will set the progress to that value.
  # - It also accepts optional detail text.
  updateProgress <- function(value = NULL, detail = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
      }

    if(input$plot_Setting == "HeatMap" ){
            v$plot <- smoothScatterPlot(df.XY,X,Y, updateProgress)
        } else {
            v$plot <- dotPlot(df.XY, X, Y, updateProgress)
        }
      return(v$plot)
  })

output$plot1 <- renderPlot({
   # Re-run when button is clicked
      doPlots() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)

    })
  #output information
  output$info_data_xy <- renderText({
    xy_range_str <- function(e) {
      if(is.null(e)) return("Select variable on the plot\n")
      paste0("X min= ", round(e$xmin, 1), " X max= ", round(e$xmax, 1),
             " Y min= ", round(e$ymin, 1), " Y max= ", round(e$ymax, 1))
    }
    paste0("Gate selected : ", xy_range_str(input$plot1_brush), "\n" )

    })
    #Options
  output$info_data_plot <- renderText({

    if(is.null(input$select_files)) return()
      paste0("Dataset :", v$information[[1]], " cells and ",v$information[[2]]," observables \n")

    })
  })
