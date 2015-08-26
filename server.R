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
  # FCS viewer Module
  # reactive values to plot function
  v <- reactiveValues(df = NULL,
                      X = NULL,
                      Y = NULL,
                      create_plot = FALSE, # action button to plot
                      rb = "HeatMap",
                      selected_input_file = NULL )

  # reactive data.frame will contain files
  # this object is going to be manipulate trough
  # all the Module

  user_data <- list(plotID = "",
                    X_channel = "",
                    Y_channel = "",
                    df = NULL)

  # give us the object flowFrame
  getFlowFrame <- reactive({
    #cat(paste( "getFlowFrame ", input$select_files, " is selected \n"))
    #check that the user selected an input
    if(is.null(input$select_files)) return()
    v$selected_input_file <- input$select_files
    #cat(paste( " if getFlowFrame ", input$select_files, " is selected \n"))
    #check that the filename exist in the session
    if((v$selected_input_file %in% user_dataset_names)) return()
    filename <- v$selected_input_file
    cat(paste("getFlowFrame ",filename, " is bizare \n"))
    fcs.flowFrame <- user_session[[filename]][paste("df")]
    cat(paste("getFlowFrame ",colnames(fcs.flowFrame), " is bizare \n"))
    return(fcs.flowFrame)
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
     cat(paste( "observeEvent ", input$select_files, " is selected \n"))
     cat(paste( "user_session ", is.null(input$select_files), " is selected \n"))
     if(!is.null(input$select_files)) return()
     v$elected_input_file <- input$select_files
     cat(paste(v$selected_input_file, " is bizare \n"))
     #save the data if it doesn't exist in the data frame
     if((v$selected_input_file %in% names(user_session))) return()
     save_dataset(v$selected_input_file)
    })

#Select Channel
  output$selectX <- renderUI({
    cat("output X \n")
    #check if the user has selected a dataset
    if(is.null(input$select_files)) return()
    #if(!(v$selected_input_file %in% names(user_session))) return()
    cat("output X if selected \n")
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



  #my idea here is to create an input channel
  observeEvent(input$Xchannel, {
     if(is.null(input$Xchannel)) return()
     v$X <- input$Xchannel
     cat("X is selected \n")
    })

  output$transformX <- renderUI({
     selectInput("X_transf",
                 "select a parameter",
                  choices = c("asinh", "no transformation"),
                  width = "100%")
    })

 output$transformY <- renderUI({
   selectInput("Y_transf",
               "select a function",
                choices = c("asinh", "no transformation"),
                width ="100%")
   })


  output$selectY <- renderUI({
    if(is.null(input$select_files)) return()
    #if(!(v$selected_input_file %in% names(user_session))) return()
    selectInput("Ychannel",
    "Select your Y channel",
    size = 4,
    width = "100%",
    selectize = FALSE,
    choices = as.character(colnames(getFlowFrame()))
    )
  })

  observeEvent(input$Ychannel, {
     if(is.null(input$Ychannel)) return()
      v$Y <- input$Ychannel
      cat("Y is selected \n")
    })




    # action button for creating the plot
    observeEvent(input$apply_modif, {
     v$create_plot <- input$apply_modif
     cat("create a PLOT \n")
    })


    observeEvent(input$radio.plot, {
       if(v$rb == as.character(input$radio.plot) ) return()
       v$rb <- as.character(input$radio.plot)
       cat("Plot button was selected \n")
       cat(paste("Plot button : ", v$rb, " was selected"))
       })

  observeEvent(input$reset, {
         v$df <- NULL
         v$X <- NULL
         v$Y <- NULL
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


output$plot1 <- renderPlot({

    if(v$create_plot == FALSE) return()
        df = user_session[v$selected_input_file]
        X = v$X
        Y = v$Y
    if ( v$rb == "HeatMap"){
          smoothScatterPlot(df, X, Y)
          cat("print heat MAp \n")
    }else {
          dotPlot(df, X, Y)
          cat("print dot Plot \n")
    }
})






  #Options
  output$value <- renderText({
    input$radio.plot
    })
  })
