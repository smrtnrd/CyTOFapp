# plotting section of the main frame
source("assets/carouselPanel.R")





select.beads <-  selectInput("inBeads",
                             "Beads Channels",
                             width = "100%",
                             size = 10,
                             selectize = F,
                             multiple = T,
                             choices = list())
select.dna <- selectInput("inDna",
                          "DNA Channels",
                          width = "100%",
                          size = 10,
                          selectize = F,
                          choices = list())

modalOptions <- bsModal("modalOptions", "Please set the parameter for normalisation", "tab_options", size = "large",
                          
                        fluidRow(column ( width = 4,
                                          
                                            selectInput("userFiles",
                                                        "File/Sample",
                                                        width = "100%",
                                                        size = 10,
                                                        selectize = F,
                                                        choices = load_files_from_dir(),
                                                        selected = list()
                                                        
                                                        ),
                                          fileInput('normFiles', "Add a file", multiple = TRUE, width = "100%")
          
                                            #shinyFilesButton('normfiles', 'Chose files for Norm', 'Please select a file', FALSE)
                                            #includeHTML("assets/button.html")
                                            ),
                                   column ( width = 4,
                                           select.beads,
                                           actionButton("removeFile", "remove", width = "100%")),
                                   column(width = 4,
                                          select.dna,
                                          actionButton("apply_norm", "Create Plots", width = "100%", inline = T)
                                          )))


panelPlot.norm <- column(width = 12,
                    tabBox(title = tagList(shiny::icon("line-chart"), "Normalisation"),
                           id = "tabsetNorm",
                           width = NULL,
                           #uiOutput("plottabs")
                           tabPanel("Normalisation",
                                    # This is the dynamic UI for the plots
                                    conditionalPanel( condition = "output.nplot1",
                                    carouselPanel(
                                      plotOutput("nplot1", dblclick = "dblclick1", 
                                                 brush = brushOpts( id = "plot_brush1", resetOnNew = TRUE )),
                                      plotOutput("nplot2", dblclick = "dblclick1", 
                                                 brush = brushOpts( id = "plot_brush1", resetOnNew = TRUE )),
                                      plotOutput("nplot3", dblclick = "dblclick1", 
                                                 brush = brushOpts( id = "plot_brush1", resetOnNew = TRUE )),
                                      plotOutput("nplot4", dblclick = "dblclick1", 
                                                 brush = brushOpts( id = "plot_brush1", resetOnNew = TRUE )),
                                      plotOutput("nplot5", dblclick = "dblclick1", 
                                                 brush = brushOpts( id = "plot_brush1", resetOnNew = TRUE ))
                                    )),
                                    hr(),
                                    fluidRow(
                                      column(width = 8,
                                             p("Chose the parameter for your filter")
                                             ),
                                      column(width = 2,
                                             actionButton("tab_options", "Options", width = "100%", inline = TRUE)
                                            ),
                                      column(width = 2,
                                             actionButton("save_norm", "Save", width = "100%", inline = T)
                                      ))
                                    )
                           ))


normFCS <- tabItem(tabName = "normFCS" ,
                     fluidPage(
                       fluidRow(panelPlot.norm),
                       modalOptions))