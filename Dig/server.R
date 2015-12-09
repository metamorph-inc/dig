library(shiny)
#options(shiny.trace=TRUE)

raw <- read.csv("../data.csv")
# do something to pre-process the data

rawAbsMin = apply(raw, 2, min, na.rm=TRUE)
rawAbsMax = apply(raw, 2, max, na.rm=TRUE)

shinyServer(function(input, output, clientData, session) {
  # icount = 1
  # selPlotx  <-1
  # selPloty <- 2
  # numPlots <- 2
  
  varNames = ls(raw,sort=FALSE)
  varClass = sapply(raw,class)
  
  filterData <- reactive({
    print("In filterData()")
    data <- raw
    for(column in 1:length(varNames)) {
      inpName=paste("inp",toString(column),sep="")
      nname = varNames[column]
      rng = input[[inpName]]
      if(varClass[column]=="numeric") {
        data <- data[data[nname] >= rng[1],]
        data <- data[data[nname] <= rng[2],]
      } else {
        if (varClass[column]=="factor") {
          data <- data[data[[nname]] %in% rng]
        }
      }
      #cat("-----------", inpname, nname, rng, length(bd[nname]), sep = '\n')
    }
    print("Data Filtered")
    data
  })
  
  colorData <- reactive({
    print("In colorData()")
    data <- filterData()
    print(paste("Coloring Data:", input$colVar, input$colSlider[1], input$colSlider[2]))
    data$color <- character(nrow(data))
    data$color <- "yellow"
    data$color[data[paste(input$colVar)] < input$colSlider[1]] <- "green"
    data$color[data[paste(input$colVar)] > input$colSlider[2]] <- "red"
    print("Data Colored")
    data
  })
  
  # Show the pairs plots
  output$pairsPlot <- renderPlot({
    
    validate(
      need(length(input$display) >= 2, "Please select two or more display variables.")
    )
    
    print("Getting Variable List.")
    idx = 0
    for(choice in 1:length(input$display)) {
      mm <- match(input$display[choice],varNames)
      if(mm > 0) { idx <- c(idx,length(varNames)- mm + 1 ) }
    }
    print(idx)
    
    data <- colorData()
    print("Rendering Plot.")
    pairs(data[idx],lower.panel = panel.smooth,upper.panel=NULL, col=data$color)
    print("Plot Rendered.")
  })
  
  output$stats <- renderText({
    infoTable()
  })
  
  infoTable <- eventReactive(input$updateStats, {
    colorCounts = table(colorData()$color)
    paste0("Total Points: ", nrow(raw),
           "\nCurrent Points: ", nrow(filterData()),
           "\nGreen Points: ", try(colorCounts[["green"]]),
           "\nYellow Points: ", try(colorCounts[["yellow"]]),
           "\nRed Points: ", try(colorCounts[["red"]])
    )
  })
  
  
  
  output$singlePlot <- renderPlot({
    
    idx = 0
    mm <- match(input$xInput,varNames)
    if(mm > 0) { idx <- c(idx,length(varNames)- mm + 1 ) }
    mm <- match(input$yInput,varNames)
    if(mm > 0) { idx <- c(idx,length(varNames)- mm + 1 ) }
    
    print(idx)

    data <- filterData()
    if(length(data[idx]) > 0) {
      plot(data[idx])
    }
  })
  
  # output$selected_rows <- renderPrint({
  #     ptidx <<- input$plot_click$x 
  #     ptidy <<- input$plot_click$y
  #     divScat <<- length(scatIdx)
  #     xidx <- as.integer(1+(divScat-1)*ptidx)
  #     yidx <- as.integer(1+(divScat-1)*ptidy)
  #     selPlotx <<- as.integer(scatIdx[xidx+1])
  #     selPloty <<- as.integer(scatIdx[yidx+1])
  #     # input$plot_click$x
  #     cat("Clicked x=",ptidx , "y=",ptidy,"numplots",numPlots," Selected: ", selPlotx,selPloty,"divscat",divScat,"scatIdx",scatIdx ) 
  #     #cat(scatIdx)
  #     icount <<- icount + 1 + input$plot_click$x
  #  })
  
  output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    t(nearPoints(bd, input$plot_click, xvar = input$xInput, yvar = input$yInput))
    # nearPoints() also works with hover and dblclick events
  })
 
  output$exportData <- downloadHandler(
    filename = function() { paste('data-', Sys.Date(), '.csv', sep='') },
    content = function(file) { write.csv(filterData(), file) }
  )
  
  output$exportPlot <- downloadHandler(
    filename = function() { paste('plot-', Sys.Date(), '.pdf', sep='') },
    content = function(file) {
      pdf(paste('plot-', Sys.Date(), '.pdf', sep=''))
      
      idx = 0
      
      print("Getting Variable List.")
      for(choice in 1:length(input$display)) {
        mm <- match(input$display[choice],varNames)
        if(mm > 0) { idx <- c(idx,length(varNames)- mm + 1 ) }
      }
      print(idx)
      
      print("Trimming Data.")
      bd <- raw
      for(column in 1:length(varNames)) {
        inpname=paste("inp",toString(column),sep="")
        nname = varNames[column]
        rng = input[[inpname]]
        bd <- bd[bd[nname] >= rng[1],]
        bd <- bd[bd[nname] <= rng[2],]
        
        bdmin <- apply(bd,2,min)
        bdmax <- apply(bd,2,max)
        #cat("-----------", inpname, nname, rng, length(bd[nname]), sep = '\n')
      }
      
      if(length(bd[idx]) > 0) {
        print(paste("Coloring Data:", input$colVar, input$colSlider[1], input$colSlider[2]))
        bd$colors <- character(nrow(bd))
        bd$colors <- "yellow"
        bd$colors[bd[paste(input$colVar)] < input$colSlider[1]] <- "green"
        bd$colors[bd[paste(input$colVar)] > input$colSlider[2]] <- "red"
        print("Rendering Plot.")
        pairs(bd[idx],lower.panel = panel.smooth,upper.panel=NULL, col=bd$colors)
        print("Plot Rendered.")
      }
      
      dev.off()
      file.copy(paste('plot-', Sys.Date(), '.pdf', sep=''), file)
    }
  )

  observe({
    print("Observing.")
    updateSliderInput(session,
                      "colSlider",
                      step = signif((max(colorData()[paste(input$colVar)], na.rm=TRUE)-min(colorData()[paste(input$colVar)], na.rm=TRUE))*0.01, digits = 2),
                      min = signif(unname(rawAbsMin[paste(input$colVar)])*0.95, digits = 2),
                      max = signif(unname(rawAbsMax[paste(input$colVar)])*1.05, digits = 2),
                      value = c(min(colorData()[paste(input$colVar)], na.rm=TRUE)+0.33*(max(colorData()[paste(input$colVar)], na.rm=TRUE)-min(colorData()[paste(input$colVar)], na.rm=TRUE)), min(colorData()[paste(input$colVar)], na.rm=TRUE)+0.66*(max(colorData()[paste(input$colVar)], na.rm=TRUE)-min(colorData()[paste(input$colVar)], na.rm=TRUE)))
    
    # updateSliderInput(session, "inp1", value=c(input$plot_brush$xmin, input$plot_brush$xmax))
    # updateSliderInput(session, inputID = paste0("inp", "1"),
    #                   min = input$plot_brush$xmin,
    #                   max = input$plot_brush$xmax,
    #                   value = c(input$plot_brush$xmin, input$plot_brush$xmax),
    #                   step = ((input$plot_brush$xmax - input$plot_brush$xmax)*0.1))
    # updateSliderInput(session,
    #                   inputID = paste0("inp", match(input$xInput, varNames)),
    #                   min = input$plot_brush$xmin,
    #                   max = input$plot_brush$xmax,
    #                   value = c(input$plot_brush$xmin, input$plot_brush$xmax),
    #                   step = ((input$plot_brush$xmax - input$plot_brush$xmax)*0.1))
    # updateSliderInput(session,
    #                   inputID = paste0("inp", match(input$yInput, varNames)),
    #                   min = input$plot_brush$ymin,
    #                   max = input$plot_brush$ymax,
    #                   value = c(input$plot_brush$ymin, input$plot_brush$ymax),
    #                   step = ((input$plot_brush$ymax - input$plot_brush$ymax)*0.1))
    )
  })
})  


# output$pairsPlot <- renderPlot({
#   
#   idx = 0
#   
#   if (length(input$display) < 2) {
#     # Too few inputs, print message asking for more.
#     print("Too few inputs.")
#     
#   } else {
#     
#     print("Getting Variable List.")
#     for(choice in 1:length(input$display)) {
#       mm <- match(input$display[choice],varNames)
#       if(mm > 0) { idx <- c(idx,length(varNames)- mm + 1 ) }
#     }
#     print(idx)
#     
#     print("Trimming Data.")
#     bd <<- bladedat
#     for(column in 1:length(varNames)) {
#       inpname=paste("inp",toString(column),sep="")
#       nname = varNames[column]
#       rng = input[[inpname]]
#       bd <<- bd[bd[nname] >= rng[1],]
#       bd <<- bd[bd[nname] <= rng[2],]
#       
#       bdmin <<- apply(bd,2,min)
#       bdmax <<- apply(bd,2,max)
#       #cat("-----------", inpname, nname, rng, length(bd[nname]), sep = '\n')
#     }
#     
#     if(length(bd[idx]) > 0) {
#       print(paste("Coloring Data:", input$colVar, input$colSlider[1], input$colSlider[2]))
#       bd$colors <<- character(nrow(bd))
#       bd$colors <<- "yellow"
#       bd$colors[bd[paste(input$colVar)] < input$colSlider[1]] <<- "green"
#       bd$colors[bd[paste(input$colVar)] > input$colSlider[2]] <<- "red"
#       print("Rendering Plot.")
#       pairs(bd[idx],lower.panel = panel.smooth,upper.panel=NULL, col=bd$colors)
#       print("Plot Rendered.")
#     }
#   }
#   
# })
