library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)

library(flowCore)

source("helpers.R", chdir = TRUE)
source("FCSviewer.R", chdir = TRUE)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100*1024^2)
outputDir <- "FCSfile"
filenames <- list.files(outputDir, pattern="\\.fcs$")

shinyServer(function(input, output, session) {

  #add file to the Dataset
  saveDataTemp <- reactive({
      #TODO(not urgent): add a message to user that he needs add a file
      if(is.null(input$FCSfile)){return(data.frame())}
      FCS.file <- input$FCSfile
      write.FCS (
        x = FCS.file,
        file = file.path(outputDir, FCS.file$name)
        )
    })

  #menu on the sidebar
  output$menuitem <- renderMenu({
    menuItem("FCS viewer", icon = icon("calendar"))
  })
  #Notification to user
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("line-chart"),
      color = "purple", fill = TRUE
    )
    })

  output$progressBox2 <- renderInfoBox({
    infoBox(
      "FCS files", paste0(25 + input$count), icon = icon("files-o"),
      color = "yellow", fill = TRUE
    )
    })




  fcs.matrix <- reactive({
    inFile <- input$FCSfile
    cat("matrix if statement \n")
    if(is.null(input$FCSfile))return()
      cat("matrix\n")
      fcs.flowFrame <- read.FCS(inFile$datapath)
      #return a matrix
      return(fcs.flowFrame)
  })
  cat("error\n")




  output$selectX <- renderUI({
    selectInput("Xchannel",
    "Select your X channel",
    choices = colnames(fcs.matrix())
    )
  })

  output$selectY <- renderUI({
    selectInput("Ychannel",
    "Select your Y channel",
    choices = as.character(colnames(fcs.matrix()))
    )
  })

  #fcs.channelsTable <- reactive({
  #  fcs.matrix() %>%
  ##  })

  #output$channelTable <- DT::renderDataTable(
    #colnames(),
  #  server = FALSE,
  #  selection = 'single'
  #)
  cat("error 02 \n")

#PLOT OUTPUT
 v <- reactiveValues(df = NULL, X = NULL, Y = NULL, doPlot = FALSE, rb = "HeatMap")



    # action button
   observeEvent(input$plot, {
     v$doPlot <- input$plot

     cat("create a PLOT \n")
     })
     observeEvent(input$radio.plot, {
       if(v$rb == as.character(input$radio.plot) ) return()
       v$rb <- as.character(input$radio.plot)
       cat("Plot button was selected \n")
       cat(v$rb)
       cat("Plot button was selected \n")
       })
     observeEvent(input$reset, {
         v$df <- NULL
         v$X <- NULL
         v$Y <- NULL
      })

   observeEvent(input$Xchannel, {
      if(is.null(input$Xchannel)) return()
      v$X <- input$Xchannel
      cat("X is selected \n")
     })
   observeEvent(input$Ychannel, {
      if(is.null(input$Ychannel)) return()
       v$Y <- input$Ychannel
       cat("Y is selected \n")
     })
     observeEvent(input$FCSfile,  {
       inFile <- input$FCSfile
       fcs.flowFrame <- read.FCS(inFile$datapath)
       v$df <-   as.data.frame.matrix(exprs(fcs.flowFrame))
       cat("input File is selected \n")
     })

    getX <- reactive({
      X <- as.character(input$Xchannel)
      })
      getY <- reactive({
        Y <- as.character(input$Ychannel)
        })

   output$plot1 <- renderPlot({
    if(v$doPlot == FALSE) return()
    df = v$df
    X = getX()
    Y = getY()

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
