observe({
  a <- which(blkname==input$e0)
  start <- input$dateRange[1]
  end <- input$dateRange[2]
  CI <- input$CI
  inc <- input$increment
  dmStdrd <- input$dmStdrd
  output$Name3 <- renderText(paste0("Cumulative % of ",input$e0," block"))
  output$Head3 <- renderText(paste0("Cumulative % of ", input$e0))
  pts <<- paddtrees[which(paddtrees$Date>start&paddtrees$Date<end),]
  pts$DryMatter <- pts$DryMatter+(pts$Amend*inc)
  pts$DryMatter <- round(pts$DryMatter, digits=2)
  grap <- pts[which(pts$Date>start&pts$Date<end),]
  grap <- grap[which(grap$paddock==a),]
  occur <- length(unique(grap$Amend))
  dmScat <- data.frame(sort(grap$DryMatter))
  colnames(dmScat) <- c("DM")
  numb <- nrow(dmScat)
  dmScat$nValue <- c(1:numb)
  dmScat$percent <- (dmScat$nValue/numb)*100
  cumGAvg <- round(mean(dmScat$DM), digits = 1)
  
  
  cumGraph <- ggplot(dmScat, aes(x = dmScat$DM, y = percent)) +
    geom_point(color = "orange", size = 5, shape = 1) +
    labs(x = "DM", y = "Cumulative %")+
    theme(panel.background = element_rect(fill = "black"))
  
  if (occur == 0){output$cumlG <- renderPlot({
    plot(8,15,col="white",xlab= "DM", ylab = "Cumulative %")
    text(8,15,"Warning no DM data available")
  })
  output$cumlGSamp <- renderValueBox({
    valueBox(
      "Warning", "no data",
      icon = icon("remove", lib = "glyphicon"),
      color = "red"
    )
  })
  output$cumlGAvg <- renderValueBox({
    valueBox(
      "No data!",
      icon = icon("remove", lib = "glyphicon"),
      color = "red"
    )
  })
  
  }else{output$cumlG <- renderPlot({
    cumGraph
  })
  output$cumlGSamp <- renderValueBox({
    valueBox(
      "There are ", numb, " fruit sampled.",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  output$cumlGAvg <- renderValueBox({
    valueBox(
      CumGAvg, " was the average DM%",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  }
})