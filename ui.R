## ui.R ##
library(shiny)
library(shinydashboard)

# Header elements for the visualization
header <- dashboardHeader(title = "CyTOF Analysiser v.0",
    dropdownMenu(type = "notifications",
                notificationItem(text = "5 new users today",
                                 icon("users")),
                notificationItem(text = "12 items delivered",
                                 icon("truck"),
                                 status = "success"),
                notificationItem(text = "Server load at 86%",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"))
)

# Sidebar elements for the search visualizations
sidebar <- dashboardSidebar(
  #TODO(buildman): create reactive component to give feedback to the user
  #Header section
  sidebarMenu(
    menuItem("FCS viewer", tabName = "FCS_viewer", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
            badgeLabel = "new", badgeColor = "green")
  )#/sidebarMenu
)#/dashboardSidebar

#Body elements for the search visualizations.
body <- dashboardBody(
  #TODO(Buildman): notification for running process
  # infoBox section
  tabItems(
    tabItem(tabName = "FCS_viewer" ,
            fluidRow(valueBoxOutput("FCS_viewer_box_1", width = 4),
                     valueBoxOutput("FCS_viewer_box_2", width = 4),
                     valueBoxOutput("FCS_viewer_box_3", width = 4)),
            fluidRow(column(width = 8,
                     tabBox(title = tagList(shiny::icon("line-chart"), "FCS viewer Plot"),
                     id = "tabsetPlot1",
                     width = NULL,
                     #uiOutput("plottabs")
                     tabPanel("File name",
                              plotOutput("plot1"),
                              hr(),
                              hr(),
                              fluidRow(
                                column( width = 6,
                                        uiOutput("selectX"),
                                        #h4("transformation X-channel"),
                                        uiOutput("transformX")),
                                column( width = 6,
                                        uiOutput("selectY"),
                                        #h4("transformation Y-channel"),
                                        uiOutput("transformY"))
                                )),
                     tabPanel("Summary",
                              "Parameters",
                              textOutput("value"),
                              uiOutput("plottabs"))
                            )),
                     column(width = 4,
                            box(title = "File/Sample name", width = NULL,
                            fileInput('FCSfile',
                                      'add fcs files',
                                       multiple = TRUE),
                            uiOutput("selectFiles"),
                            hr(),
                            actionButton("apply_modif", "Apply Modification")),
                      column(width =4,
                             box( width = NULL,
                             radioButtons("Plot Setting",
                                          label = h5("Radio buttons"),
                                          choices = list("HeatMap", "Dotplot"),
                                          selected = "HeatMap",
                                          inline = TRUE)))
                             )
        )
      )
    )
  )


  dashboardPage(header = header, sidebar = sidebar, body = body, skin = "black")
