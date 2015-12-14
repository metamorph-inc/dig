library(shiny)
#options(shiny.trace=TRUE)
#options(shiny.fullstacktrace = TRUE)
#options(error = function() traceback(2))
#options(shiny.error = function() traceback(2))


shinyServer(function(input, output, clientData, session) {
  # icount = 1
  # selPlotx  <-1
  # selPloty <- 2
  # numPlots <- 2
  raw <- c()
  query <- parseQueryString(isolate(session$clientData$url_search))

  # output$debug <- renderText({
  #   paste(names(query), query, sep = "=", collapse=", ")
  # })

  if (!is.null(query[['csvfilename']])) {
    # raw.csv(paste0(dirname(sys.frame(1)$ofile), "/../webserver/public/csvs/", query[['csvfilename']]), fill=T)
    # raw = read.csv(paste0(dirname("/csvs/", query[['csvfilename']]), fill=T)
    raw = read.csv(paste("/media/sf_kevin/Downloads/", query[['csvfilename']], sep=''), fill=T)
  }
  else
  {
    # raw = read.csv("../data.csv", fill=T)
    raw = read.csv("../../results/mergedPET.csv", fill=T)
  }
  
  # do something to pre-process the data

  rawAbsMin = apply(raw, 2, min, na.rm=TRUE)
  rawAbsMax = apply(raw, 2, max, na.rm=TRUE)
  
  varNames = ls(raw,sort=FALSE)
  varClass = rev(sapply(raw,class))
  
  varRange <- varNames[((rawAbsMax-rawAbsMin)!= "0")]
  print(paste("varRange", varRange))
  
  print("Updating Panel Selections...")
  updateSelectInput(session, "colVar", choices = varRange, selected = varRange[c(1)])
  updateSelectInput(session, "display", choices = varNames, selected = varNames[c(1,2)])
  updateSelectInput(session, "xInput", choices = varNames, selected = varNames[c(1)])
  updateSelectInput(session, "yInput", choices = varNames, selected = varNames[c(2)])
  
  # Sliders ---------------------------------------------------------------------------------------------------
  output$sliders <- renderUI({
    fluidRow(
      lapply(1:length(varNames), function(i) {
        print(paste(i, varNames[i], varClass[i]))
        column(2,
          if(varClass[i] == "numeric") {
            max <- as.numeric(unname(rawAbsMax[varNames[i]]))
            min <- as.numeric(unname(rawAbsMin[varNames[i]]))
            step <- (max-min)*0.01
            if (min == max) {max <- max + 1}
            sliderInput(paste0('inp', i),
                        varNames[i],
                        step = signif(step, digits = 2),
                        min = signif(min-step*10, digits = 2),
                        max = signif(max+step*10, digits = 2),
                        value = c(signif(min-step*10, digits = 2),signif(max+step*10, digits = 2)))
          } else {
            if (varClass[i] == "factor") {
              selectInput(paste0('inp', i),
                          varNames[i],
                          multiple = TRUE,
                          selectize = FALSE,
                          choices = names(table(raw[varNames[i]])))
            } else {
              if (varClass[i] == "integer") {
                max <- as.integer(unname(rawAbsMax[varNames[i]]))
                min <- as.integer(unname(rawAbsMin[varNames[i]]))
                if (min == max) {max <- max + 1}
                sliderInput(paste0('inp', i),
                            varNames[i],
                            min = min,
                            max = max,
                            value = c(min, max))
              }
            }
          }
        )
      })
    )
  })

  # Data functions ----------------------------------------------------------------------
  filterData <- reactive({
    print("In filterData()")
    data <- raw
    for(column in 1:length(varNames)) {
      inpName=paste("inp",toString(column),sep="")
      nname = varNames[column]
      print(paste("Checking", nname))
      rng = input[[inpName]]
      if(varClass[column]=="numeric" | varClass[column]=="integer") {
        print(paste("rng", rng[1], rng[2]))
        data <- data[data[nname] >= rng[1],]
        data <- data[data[nname] <= rng[2],]
      } else {
        if (varClass[column]=="factor") {
          print(paste("rng", rng))
          data <- data[data[[nname]] %in% rng]
        }
      }
      # cat("-----------", inpName, nname, rng, length(data[nname]), sep = '\n')
    }
    print("Data Filtered")
    data
  })

  colorData <- reactive({
    print("In colorData()")
    data <- filterData()
    print(paste("Coloring Data:", input$colVar, input$colSlider[1], input$colSlider[2]))
    data$color <- character(nrow(data))
    data$color <- "black"
    if (input$color == TRUE) {
      if(varClass[[input$colVar]] == "factor") {
        # Coloring of factors is currently unsupported!
        # data$color[data[paste(input$colVar)] %in% input$colSelect]
      } else {
        if (input$radio == "max") {
          data$color[data[paste(input$colVar)] < input$colSlider[1]] <- "red"
          data$color[(data[paste(input$colVar)] >= input$colSlider[1]) & (data[paste(input$colVar)] <= input$colSlider[2])] <- "yellow"
          data$color[data[paste(input$colVar)] > input$colSlider[2]] <- "green"
        } else {
          data$color[data[paste(input$colVar)] < input$colSlider[1]] <- "green"
          data$color[(data[paste(input$colVar)] >= input$colSlider[1]) & (data[paste(input$colVar)] <= input$colSlider[2])] <- "yellow"
          data$color[data[paste(input$colVar)] > input$colSlider[2]] <- "red"
        }
      }
    }
    print("Data Colored")
    data
  })

  # Pairs Tab --------------------------------------------------------------------------------------
  output$stats <- renderText({
    infoTable()
  })

  infoTable <- eventReactive(input$updateStats, {
    tb <- table(factor(colorData()$color, c("green", "yellow", "red", "black")))
    if (input$color) {
      paste0("Total Points: ", nrow(raw),
             "\nCurrent Points: ", nrow(filterData()),
             "\nVisible Points: ", sum(tb[["green"]], tb[["yellow"]], tb[["red"]]),
             "\nGreen Points: ", tb[["green"]],
             "\nYellow Points: ", tb[["yellow"]],
             "\nRed Points: ", tb[["red"]]
      )
    } else {
      paste0("Total Points: ", nrow(raw),
             "\nCurrent Points: ", nrow(filterData()),
             "\nVisible Points: ", tb[["black"]]
      )
    }
  })

  output$pairsPlot <- renderPlot({
    if (input$autoRender == TRUE) {
      vars <- varsList()
    } else {
      vars <- varsListSlow()
    }
    validate(need(length(vars)>=2, "Please select two or more display variables."))
    
    print("Rendering Plot.")
    pairs(colorData()[vars],lower.panel = panel.smooth,upper.panel=NULL, col=colorData()$color)
    print("Plot Rendered.")
  })
  
  varsList <- reactive({
    print("Getting Variable List.")
    idx = 0
    for(choice in 1:length(input$display)) {
      mm <- match(input$display[choice],varNames)
      if(mm > 0) { idx <- c(idx,length(varNames)- mm + 1 ) }
    }
    print(idx)
    idx
  })
  
  varsListSlow <- eventReactive(input$renderPlot, {
    print(paste("input$renderPlot:", input$renderPlot))
    print("Getting Variable List.")
    idx = 0
    for(choice in 1:length(input$display)) {
      mm <- match(input$display[choice],varNames)
      if(mm > 0) { idx <- c(idx,length(varNames)- mm + 1 ) }
    }
    print(idx)
    idx
  })
  
  # Single Plot Tab ----------------------------------------------------------------------------------

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

  output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    t(nearPoints(filterData(), input$plot_click, xvar = input$xInput, yvar = input$yInput, maxpoints = 8))
  })

  output$exportData <- downloadHandler(
    filename = function() { paste('data-', Sys.Date(), '.csv', sep='') },
    content = function(file) { write.csv(filterData(), file) }
  )
  
  output$exportPlot <- downloadHandler(
    filename = function() { paste('plot-', Sys.Date(), '.pdf', sep='') },
    content = function(file) {
      pdf(paste('plot-', Sys.Date(), '.pdf', sep=''), width = 10, height = 10)
      pairs(colorData()[varsList()],lower.panel = panel.smooth,upper.panel=NULL, col=colorData()$color)
      dev.off()
      file.copy(paste('plot-', Sys.Date(), '.pdf', sep=''), file)
    }
  )
  
  # Data Table Tab --------------------------------------------------------------------------------
  output$table <- renderDataTable({
    input$updateDataTable
    data <- isolate(colorData())
  })
  
  # UI Adjustments --------------------------------------------------------------------------------
  updateColorSlider <- function(x) {
    data <- isolate(colorData())

    min <- min(data[[paste(input$colVar)]], na.rm=TRUE)
    max <- max(data[[paste(input$colVar)]], na.rm=TRUE)
    absMin <- unname(rawAbsMin[paste(input$colVar)])
    absMax <- unname(rawAbsMax[paste(input$colVar)])
    absStep <- (max-min)*0.01
    print(paste(absMin, absMax, min, max))
    
    if(varClass[[input$colVar]] == "numeric") {
      if (absMax == absMin) {absMax <- (absMax + 1)}
      updateSliderInput(session,
                        "colSlider",
                        step = signif((max-min)*0.01, digits = 2),
                        min = signif(absMin-absStep*10, digits = 2),
                        max = signif(absMax+absStep*10, digits = 2),
                        value = c(min+0.33*(max-min), min+0.66*(max-min))
      )
    }
    if(varClass[[input$colVar]] == "integer") {
      if (absMax == absMin) {absMax <- (absMax + 1)}
      updateSliderInput(session,
                        "colSlider",
                        min = absMin,
                        max = absMax,
                        value = c(min,max)
      )
    }
    if(varClass[[input$colVar]] == "factor") {
      updateSelectInput(session,
                        "colSlider",
                        choices = names(table(raw[varNames[i]])),
                        selected = )
    }
  }

  updateXSlider <- function(x) {
    updateSliderInput(session,
                      inputID = paste0("inp", match(input$xInput, varNames)),
                      min = input$plot_brush$xmin,
                      max = input$plot_brush$xmax,
                      value = c(input$plot_brush$xmin, input$plot_brush$xmax),
                      step = ((input$plot_brush$xmax - input$plot_brush$xmax)*0.1)
    )
  }

  updateYSlider <- function(x) {
    updateSliderInput(session,
                      inputID = paste0("inp", match(input$yInput, varNames)),
                      min = input$plot_brush$ymin,
                      max = input$plot_brush$ymax,
                      value = c(input$plot_brush$ymin, input$plot_brush$ymax),
                      step = ((input$plot_brush$ymax - input$plot_brush$ymax)*0.1)
    )
  }
    

  observe({
    print("Observing.")
    if (!(as.character(input[["colVar"]]) == "") && !is.null(isolate(colorData()))) {
      updateColorSlider()
    }
  })
})
