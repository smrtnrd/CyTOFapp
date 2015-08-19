library(shiny)
library(shinydashboard)

shinyServer(function(input, output) {
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

  #Options
  output$value <- renderText({ input$radio })


  })
