########################################
# Reactive UI Definition of Variables
########################################

# Define data-reactive variable lists
normData <- reactiveValues(
  df = NULL, # data frame
  beads = NULL, #list of beads
  dna = NULL, # dna channel
  information = NULL # information
) 


dataNplots <- reactiveValues(
  max_plots = 5
)


#modal option pannel
source("tabItem/tab-norm/modal-server-norm-options.R", local = TRUE)

observe({
#dynamic UI create the plots for the normalisation
lapply(1:dataNplots$max_plots, function(i){
    
    output[[paste0('nplot', i)]] <- renderPlot({
      
      if(input$abutton_run_norm > 0){
        isolate({
          y.channel <- normData$dna
          x.channel <- normData$beads[[i]]
          df.XY <- select(normData$df, one_of(x.channel, y.channel)) # select column
          
          sample.n <- 50000
          if (as.numeric(userData$information[[1]]) < sample.n) {
            sample.n <- round(0.8 * userData$information[1], 0)
          }
          
          df.sample <- df.XY[sample(nrow(df.XY), 10000),]
          x <- asinh(df.sample[[x.channel]]/ 5 )
          y <- asinh(df.sample[[y.channel]]/ 5 )
          
          df.plot <- data.frame(x, y,
                                d = densCols(x, y,
                                             colramp =  colorRampPalette(rev(
                                               rainbow(10, end = 4 / 6)
                                             ))))
          ggplot(df.plot, aes(x, y, col = d)) +
            geom_jitter(position = position_jitter(width = .8), alpha = 0.3) +
            labs(x = x.channel, y = y.channel) +
            scale_color_identity() +
            theme_bw() +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y)
        })
      }
    })
  })
})
########################################
# Info
########################################

  output$nplots <- renderText({
    paste0(length(input$nbeads_channel))
  })
  
  output$nbeads <- renderText({
    paste0(input$nbeads_channel)
  })
lapply(1:5, function(i){
  outputOptions(output, paste0("nplot",i, sep=""), suspendWhenHidden=FALSE)
})
  
