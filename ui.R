## ui.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(tidyr)
library(flowCore)

source("helpers.R", chdir = TRUE)
source("FCSviewer.R", chdir = TRUE)


outputDir <- "FCSfile"
filenames <- list.files(outputDir, pattern="\\.fcs$")

shinyUI(fluidPage(

  dashboardPage(
    #TODO(buildman): create reactive component to give feedback to the user
    #Header section
    dashboardHeader(title = "CyTOF Analysiser v.0",
    dropdownMenu(type = "notifications",
        notificationItem(
          text = "5 new users today",
          icon("users")
        ),
        notificationItem(
          text = "12 items delivered",
          icon("truck"),
          status = "success"
        ),
        notificationItem(
          text = "Server load at 86%",
          icon = icon("exclamation-triangle"),
          status = "warning"
        )
      )
    ),
    #Sidebar section
    dashboardSidebar(
        menuItemOutput("menuitem")
      ),

    #Body section
    dashboardBody(
      #TODO(Buildman): notification for running process
      # infoBox section
      fluidRow(
        infoBoxOutput("progressBox"),
        infoBoxOutput("progressBox2")
        ),
      fluidRow(
        column(
          width = 8,

           tabBox(
             title = tagList(shiny::icon("line-chart"), "FCS viewer Plot"),
             id = "tabsetPlot1", height = "500px", width = NULL,
             tabPanel("Plot1",
             textOutput("value"),
             plotOutput("plot1")

             ),
             tabPanel("Plot2", "Tab content 2")

             )
          ),
        column(
          width =4,
          box(
            title = "Options", height = "350px", width = NULL,
            fileInput('FCSfile', 'upload your fcs files', multiple = TRUE),
            uiOutput("selectX"),
            uiOutput("selectY"),
            actionButton("plot", "Plot!"),
            actionButton("reset", "Clear")
            )
          ),
          column(
            width =4,
            box(
              height = "200px", width = NULL,
              radioButtons("radio.plot", label = h5("Radio buttons"),
              choices = list("HeatMap", "Dotplot"),
              selected = "HeatMap", inline = TRUE),
              radioButtons("radio.ashin", label = h5("Asin Transf"),
              choices = list("yes", "no"), selected = "no", inline = TRUE),
              selectInput(
                inputId = "dataset",
                label = "Choose the dataset",
                choices = filenames
                )

              )
            )
          )
      )
    )
  )
)
