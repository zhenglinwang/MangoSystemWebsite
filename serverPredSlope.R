observe({
  a <- which(blkname==input$e0)
  start <- input$dateRange[1]
  end <- input$dateRange[2]
  CI <- input$CI
  dmStdrd <- input$dmStdrd
  inc <- input$increment
  pts <<- paddtrees[which(paddtrees$Date>start&paddtrees$Date<end),]
  output$Name2 <- renderText(paste0("DM daily increase ",input$e0," block"))
  output$Head2 <- renderText(paste0("Daily Increased DM from average of block ", input$e0))
  grap <- pts[which(pts$Date>start&pts$Date<end),]
  grap <- grap[which(grap$paddock==a),]
  occur <- length(unique(grap$Amend))
  
  if (occur > 2) {splitDate <- split(grap, list(grap$Amend))
  regPlots <- as.numeric(vapply(splitDate, function(x) mean(x$DryMatter, na.rm=TRUE), 0))
  datePlots <- unique(grap$Amend)
  regress <- lm(regPlots~datePlots)
  slope <-as.numeric(regress$coefficients[2])
  interPlot <-as.numeric(regress$coefficients[1])
  grap$ciAmend <- (dmStdrd-grap$DryMatter)/slope
  }else{
    splitDate <- split(filly, list(filly$Amend))
    regPlots <- as.numeric(vapply(splitDate, function(x) mean(x$DryMatter, na.rm=TRUE), 0))
    datePlots <- unique(filly$Amend)
    regress <- lm(regPlots~datePlots)
    slope <- inc
    grap$ciAmend <- (dmStdrd-grap$DryMatter)/slope
  }
  
  recentDM <- grap[which(grap$Amend==min(grap$Amend)),]
  recentDM<- recentDM[with(recentDM, order(recentDM$DryMatter)), ]
  
  recent <- recentDM[recentDM$DryMatter < quantile(recentDM$DryMatter, (CI*0.01)), ]
  recentDM$ciAmend <- recentDM$ciAmend - recentDM$Amend
  highAmend <- round(max(recentDM$ciAmend))
  
  pickDateProj<- Sys.Date()+highAmend}
  
  if(occur==0){output$padIncrease <- renderPlot({
    plot(8,15,col="white",xlab= "Days", ylab = "DM")
    text(8,15,"Warning no DM data available")
  })
  output$dailyInc <- renderValueBox({
    valueBox(
      "Warning", "no data",
      icon = icon("remove", lib = "glyphicon"),
      color = "red"
    )
  })
  output$projectedPickDate <- renderValueBox({
    valueBox(
      "No data!",
      icon = icon("remove", lib = "glyphicon"),
      color = "red"
    )
  })
 } else {
    if (occur == 1){output$padIncrease <- renderplot({
      plot(8,15,col="white",xlab= "Days", ylab = "DM")
      text(8,15,"Warning only single date DM data available")
    })
    
    output$dailyInc <- renderValueBox({
      valueBox(
        "Warning", "single date data",
        "daily increase defaulted to ", inc,
        color = "yellow"
      )
    })
    output$projectedPickDate <- renderValueBox({
      valueBox(
        "Projected pick date based on default increase is: ",
        pickDateProj,
        icon = icon("screenshot", lib = "glyphicon"),
        color = "yellow"
      )
    })
    }else {output$padIncrease <- renderPlot({
      plot(datePlots, regPlots, xlab = "Days", ylab = "Average DM%", col="red")
      abline(lm(regPlots ~ datePlots))
    })
    output$dailyInc <- renderValueBox({
      valueBox(
        "Average daily increase was: ", slope,
        color = "green"
      )
    })
    output$projectedPickDate <- renderValueBox({
      valueBox(
        "Projected pick date based on daily increase is: ",
        pickDateProj,
        icon = icon("screenshot", lib = "glyphicon"),
        color = "green"
      )
    })
    }
  })