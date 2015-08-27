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
   cat(paste("getFlowFrame ", , " is bizare \n"))

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


  # give us the object flowFrame
getFlowFrame <- reactive({
  # check that the user selected an input
  if(is.null(input$select_files)){
      return()
    } else {
      cat("getFlowFrame if \n")
      cat(paste(outputDir, "/", input$select_files))
      default.fcs <- read.FCS(paste(outputDir, "/", input$select_files, sep = ""))
      cat("getFlowFrame fcs file \n")

      fcs.matrix <- exprs(default.fcs)
      cat("getFlowFrame X \n")
      fcs.df <- as.data.frame.matrix(fcs.matrix)
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
        df = v$df
        X = v$X
        Y = v$Y
    if ( v$rb == "HeatMap"){
          smoothScatterPlot(df, X, Y)
          cat("print heat MAp \n")
    } else {
          dotPlot(df, X, Y)
          cat("print dot Plot \n")
    }
})

  #Options
  output$value <- renderText({
    input$radio.plot
    })
  })
