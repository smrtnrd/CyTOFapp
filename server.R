library(shiny)
library(shinydashboard)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100*1024^2)

shinyServer(function(input, output, session) {

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

  # display 10 rows initially
  output$ex1 <- renderDataTable(
    if (is.null(input$FCSfile)) return(NULL) else input$FCSfile,
    options = list(pageLength = 5))

  #Options
  output$value <- renderText({input$radio.plot})
  })
