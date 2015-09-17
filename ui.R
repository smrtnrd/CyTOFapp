# Define generic graphic-type select UI.


# ui for selecting the channels
uichannel <- function(inputId, label, s = 10, ... ){
  selectInput(inputId, label,
              choices = list(),
              width = "100%",
              size = s, ... )
}


# ui for selecting user files (fcs files)
uifiles <- function(inputId, ... ){
  selectInput(inputId, label = "Open file(s)", 
              choices = list.files("data", pattern = "*.fcs$"),
              selected = list(),  ...
              )
}

#ui plot for creating plot function
uiplots <- function(i){
  id = paste0("nplot",i, sep="")
  plotOutput(outputId =  id, 
             dblclick = paste0("dbclik", i), 
             brush = brushOpts( id = paste0("plot_brush",i), 
                                resetOnNew = TRUE )
             )
}

################################################################################
#source header
source("header/header-ui-dashboard.R", local = TRUE)
#source body
source("tabItem/tab-ui-normalisation.R", local = TRUE)
source("tabItem/tab-ui-visualisation.R", local = TRUE)
################################################################################


################################################################################
# Define  sidebar  
################################################################################

norm_menu <- menuItem(
  "Normalisation", 
  tabName = "norm_tab", 
  icon = icon("th") ,
  badgeLabel = "new", 
  badgeColor = "green"
)

fcs_menu <- menuItem(
  "FCS viewer", 
  tabName = "fcs_tab", 
  icon = icon("dashboard")
)

menu <- sidebarMenu(
  norm_menu,
  fcs_menu
)

sidebar <- dashboardSidebar(
  sidebarMenu(menu)#/sidebarMenu
)#/dashboardSidebar


################################################################################
# Define the body
################################################################################

# define corresponding tabItem to menu
tab_items <-  tabItems(
  norm_tab,
  fcs_tab
)
body <- dashboardBody(tab_items)
################################################################################
# Define the full user-interface
################################################################################
dashboardPage(
  header = header, 
  sidebar = sidebar, 
  body = body, 
  skin = "black")
