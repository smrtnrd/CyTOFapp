# plotting section of the main frame


panelPlot.norm <- column(width = 12,
                    tabBox(title = tagList(shiny::icon("line-chart"), "Normalisation"),
                           id = "tabsetNorm",
                           width = NULL,
                           #uiOutput("plottabs")
                           tabPanel("Normalisation",
                                    #plotOutput("normPlots",
                                        #       dblclick = "plot_dblclick",
                                       #        brush = brushOpts(
                                      #           id = "plot_brush",
                                     #            resetOnNew = TRUE )#,
                                    #),
                                    #infoSection,
                                    uiOutput("normPlots"),
                                    fluidRow(
                                      column(width = 8,
                                             p("Chose the parameter for your fileter")
                                             ),
                                      column(width = 4,
                                             actionButton("tabEdit", "Options", width = "50%", inline = TRUE),
                                             p(" "),
                                             actionButton("apply_modif", "Normalize", width = "50%", inline = T)
                                            ))
                                    )
                           ))


normFCS <- tabItem(tabName = "normFCS" , modalEditFiles ,
                     fluidPage(
                       fluidRow(panelPlot.norm)))