## ui.R #
library(shiny)
library(shinyFiles)
library(shinydashboard)
library(metricsgraphics)

source("assets/viewer-module.R")
source("assets/norm-module.R")
source("utils.R")

# Simple header -----------------------------------------------------------
dm <- dropdownMenuOutput("messages")
mm <- dropdownMenuOutput("notifications")
tm <- dropdownMenuOutput("tasks")

# Header elements for the visualization
header <- dashboardHeader(title = "CyTOF Analysiser v.0", dm, mm, tm)
# Sidebar elements for the search visualizations
sidebar <- dashboardSidebar(
  #TODO(buildman): create reactive component to give feedback to the user
  #Header section
  sidebarMenu(
    menuItem("Normalisation", tabName = "normFCS",  icon = icon("th") ,
             badgeLabel = "new", badgeColor = "green"),
    menuItem("FCS viewer", tabName = "FCS_viewer", icon = icon("dashboard"))
    
  )#/sidebarMenu
)#/dashboardSidebar
#Body elements for the search visualizations.
body <- dashboardBody(
  tabItems(normFCS,
           fcsViewer)
  
  )
  #TODO(Buildman): notification for running process
  # infoBox section

dashboardPage(header = header, sidebar = sidebar, body = body, skin = "black")
