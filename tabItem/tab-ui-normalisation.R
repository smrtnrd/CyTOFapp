# plotting section of the main frame

################################################################################
source("tabItem/tab-norm/modal-ui-norm-options.R", local = TRUE)
source("assets/carouselPanel.R", local = TRUE)
################################################################################

################################################################################
# plots for nplots_panel
#
# http://stackoverflow.com/questions/10801750/whats-the-difference-between-lapply-and-do-call-in-r
################################################################################

# 5 it's arbitrary for the moment 
nplots <- lapply(1:5, uiplots)
# add CarouselPanel
nplots_carousel <- carouselPanel(do.call(tagList,nplots))
# add ConditionalPanel
nplots_panel <- conditionalPanel(condition = "output.nplot1", nplots_carousel)


################################################################################
# Define  norm_panel  
# This is the dynamic UI for the plots
################################################################################

# ui for showing options for norm_panel
tab_options_nAbutton <- actionButton("abutton_show_opt", "Options", width = "100%", inline = TRUE)

# save gate buttons
save_nAbutton <- actionButton("abutton_save_norm", "Save", width = "100%", inline = T)

# ui pannel for buttons
tab_pannel_nAbuttons <-  fluidRow(
  column(width = 8,
         p("Chose the parameter for your filter")
  ),
  column(width = 2,
         tab_options_nAbutton
  ),
  column(width = 2,
         save_nAbutton
  )
)

norm_panel <- column(
  width = 12,
  tabBox(title = tagList(shiny::icon("line-chart"), "Normalisation"),
         id = "norm_tabSet",
         width = NULL,
         tabPanel("Normalisation",
                  nplots_panel,
                  hr(),
                  tab_pannel_nAbuttons
                  )
         )
  )

norm_panel <- fluidRow(norm_panel)
################################################################################
norm_tab <- tabItem( tabName = "norm_tab", fluidPage( norm_panel, norm_bsModal))
