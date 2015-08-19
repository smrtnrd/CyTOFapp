## ui.R ##
library(shiny)
library(shinydashboard)

source("helpers.R", chdir = TRUE)

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
        infoBoxOutput("progressBox2"),
        conditionalPanel(
          condition = "input.radio.ashin == 'yes'",
          absolutePanel(
            bottom = 20, right = 20, width = 300,
            draggable = TRUE
            ),
            fluidRow(
              column(2,selectInput('x.value', 'X values',
                                   state.name,
                                   multiple=TRUE,
                                   selectize=TRUE)
              ),
              column(2,
                     selectInput('y.value', 'Y values',
                                 state.name,
                                 multiple=TRUE,
                                 selectize=TRUE)
                )
              ),
            style = "opacity: 0.92"
          )

        ),
      fluidRow(
        column(
          width = 8,

           tabBox(
             title = tagList(shiny::icon("line-chart"), "FCS viewer Plot"),
             id = "tabsetPlot1", height = "500px", width = NULL,
             tabPanel("Plot1",
              textOutput("value"),
              dataTableOutput('ex1')

             ),
             tabPanel("Plot2", "Tab content 2")

             )
          ),
        column(
          width =4,
          box(
            title = "Options", height = "250px", width = NULL,
            fileInput('FCSfile', 'upload your fcs files', multiple = TRUE)
            )
          ),
          column(
            width =4,
            box(
              height = "200px", width = NULL,
              radioButtons("radio.plot", label = h5("Radio buttons"),
              choices = list( heatmap = "Heat Map", dotplot = "Dot plot"),
              selected = "heatmap", inline = TRUE),
              radioButtons("radio.ashin", label = h5("Asin Transf"),
              choices = list("yes", "no"), selected = "no", inline = TRUE)
              )
            )
          )
      )
    )
  )
)
