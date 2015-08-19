## ui.R ##
library(shiny)
library(shinydashboard)

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
             title = tagList(shiny::icon("line-chart"),  if (textOutput("value") == "1") "Heat Mape" else " Dot Plot"),
             id = "tabsetPlot1", height = "500px", width = NULL,
             tabPanel("Plot1", "First tab content"),
             tabPanel("Plot2", "Tab content 2")
             )
          ),
        column(
          width =4,
          box(
            title = "Options", height = "250px", width = NULL,
            fileInput('FCSfile', 'upload your fcs files', multiple = TRUE),
            radioButtons("radio", label = h3("Radio buttons"),
            choices = list("Heat Map" = 1, "Dot plot" = 2), selected = 1)
            )
          )
        )
      )
    )
  )
)
