lapply(multiplots, function(i) {
  output[[paste0('beadplot', colnames(muliplot))]] <- renderPlot({
    ggplot(df.plot, aes(x, y)) +
      geom_jitter(position = position_jitter(width = 2), alpha = 0.3) +
      labs(x = colnames(df.plot)[1], y = colnames(df.plot)[2]) +
      theme_bw() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  })
})