library(data.table)
user_session <- data.table(plotID = character(),
                           df = data.frame(()),
                           X_channel = list(),
                           Y_channel = list(),
                           stringsAsFactors = FALSE)



#test variables
default.fcs <- read.FCS( "Bendall_et_al_Science_2011_Marrow_1_SurfacePanel_Live_CD44pos_Singlets.fcs", dir = "/FCSfile")
fcs.matrix <- exprs(default.fcs)
fcs.df <- as.data.frame.matrix(fcs.matrix)
#smoothScatterPlot(fcs.df, "Rh(102.905)-Dual", "Dy(163.929)-Dual")
#dotPlot(fcs.df, "Rh(102.905)-Dual", "Dy(163.929)-Dual")
#X.channel <- "Rh(102.905)-Dual"
#Y.channel <- "Dy(163.929)-Dual"

#produce an heat map




carouselPanel(auto.advance=F,
plotOutput("distPlot1"),
plotOutput("distPlot2"))





plotOutput("tsnePlot",
                    dblclick = "plot1_dblclick",
                    brush = brushOpts(
                      id = "plot_brush",
                      resetOnNew = TRUE
                      )
