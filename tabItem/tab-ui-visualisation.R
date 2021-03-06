#edit file seciton =================================================================
modalEditFiles <- bsModal("modalEditFiles", "Files Setting", "tabEdit", size = "large",
        fluidRow(column ( width = 9,
                          selectInput("userDataset",
                                      "Datasest",
                                      width = "100%",
                                      size = 10,
                                      selectize = F,
                                      choices = list.files("data", pattern = "*.fcs$"))),
                 column ( width = 3, 
                          hr(),
                          actionButton("remove_one_file", "Remove", width = "100%"))))
          
                 
                 
infoDataPlot <- textOutput("info_data_plot")
infoDataXY <- textOutput("info_data_xy")

# give info about the data
infoSection <- fluidRow(
  column ( width = 12,
           infoDataPlot,
           infoDataXY))

# manipulate parameters 

channelOpt <- bsCollapsePanel("Select Channels", 
                              column( width = 6,
                                      selectInput("Xchannel",
                                                  "Select your X channel",
                                                  width = "100%",
                                                  choices = list())),
                              column(width = 6,
                                     selectInput("Ychannel",
                                                 "Select your Y channel",
                                                 width = "100%",
                                                 choices = list())), style ="info")

channelTransf <- bsCollapsePanel("Channels transformation", 
                                 column( width = 6,
                                         selectInput("X_transf",
                                                     "transform X channel",
                                                     choices = c("asinh", "no transformation"),
                                                     width = "100%",
                                                     selected = "no transformation" )),
                                 column( width = 6,
                                         selectInput("Y_transf",
                                                     "transform Y channel",
                                                     choices = c("asinh", "no transformation"),
                                                     width ="100%",
                                                     selected = "no transformation" )), style = "info")
samplingOpt <- bsCollapsePanel("Sampling Options",
                               fluidRow(selectInput("sample_data",
                                                    "sample data :",
                                                    choices = c(1000,5000,10000,50000), #output file from de directory
                                                    width = "100%",
                                                    selected = 50000)), style = "info")

paramertersOptions <- fluidRow(
  column(width = 12,
         bsCollapse(id = "optionParam", open = "Select Channels", channelOpt, channelTransf, samplingOpt) )
  
)

# plotting section of the main frame
panelPlot <- column(width = 8,
                    tabBox(title = tagList(shiny::icon("line-chart"), "FCS viewer Plot"),
                           id = "tabsetPlot1",
                           width = NULL,
                           #uiOutput("plottabs")
                           tabPanel("File name",
                                    plotOutput("plot1",
                                               dblclick = "plot1_dblclick",
                                               brush = brushOpts(
                                                 id = "plot1_brush",
                                                 resetOnNew = TRUE )
                                    ),
                                    infoSection,
                                    p("Double-click on the plot to zoom"),
                                    hr(), paramertersOptions
                                    )
                    ))

sidebarOptions <- column(width = 4,
                         box(title = "File/Sample name", width = NULL, #file/sample name
                             hr(),
                             fileInput('inputFiles', "Add a file", multiple = TRUE, width = "100%"),
                             selectInput("select_files",
                                         h4("Working Files"),
                                         choices = list.files("data", pattern = "*.fcs$"), #output file from de directory
                                         size = 10,
                                         width = "100%",
                                         selectize = FALSE,
                                         selected = list()
                             ),
                             actionButton("remove_file", "Remove", width = "100%"),
                             hr(),
                             actionButton("apply_modif", "Update", width = "100%")),
                         hr(),
                         box( title = "Plot settings", width = NULL,
                              radioButtons("plot_Setting",
                                           label = h5("Radio buttons"),
                                           choices = list("HeatMap", "Dotplot"),
                                           selected = "HeatMap",
                                           inline = TRUE)))

fcs_tab <- tabItem(tabName = "fcs_tab",
                     fluidPage(
                       fluidRow(panelPlot,
                                sidebarOptions )))
