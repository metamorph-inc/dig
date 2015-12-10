library(shiny)
options(shiny.trace=TRUE)
#options(shiny.fullstacktrace = TRUE)
#options(error = function() traceback(2))
#options(shiny.error = function() traceback(2))


shinyServer(function(input, output, clientData, session) {
  # icount = 1
  # selPlotx  <-1
  # selPloty <- 2
  # numPlots <- 2
  bladedat <- c()
  query <- parseQueryString(isolate(session$clientData$url_search))

  # output$debug <- renderText({
  #   paste(names(query), query, sep = "=", collapse=", ")
  # })

    if (!is.null(query[['csvfilename']])) {
        # bladedat = read.csv(paste0(dirname(sys.frame(1)$ofile), "/../webserver/public/csvs/", query[['csvfilename']]))
        # bladedat = read.csv(paste0(dirname("/csvs/", query[['csvfilename']]))
        bladedat = read.csv(paste("/media/sf_kevin/Downloads/", query[['csvfilename']], sep=''))
    }
    else
    {
        bladedat = read.csv("../data.csv")
    }
  # do something to pre-process the data

  bdAbsMin = apply(bladedat, 2, min, na.rm=TRUE)
  bdAbsMax = apply(bladedat, 2, max, na.rm=TRUE)

  bdNames = ls(bladedat,sort=FALSE)
  bdmin = apply(bladedat, 2, min)
  bdmax = apply(bladedat, 2, max)
  bd = NULL

  updateSelectInput(session, "colVar", choices = bdNames)
  updateSelectInput(session, "display", choices = bdNames, selected = bdNames[c(1,2)])
  updateSelectInput(session, "xInput", choices = bdNames, selected = bdNames[c(1)])
  updateSelectInput(session, "yInput", choices = bdNames, selected = bdNames[c(2)])


  filteredData <- reactive({
    data <- correctedData()
    for(column in 1:length(bdNames)) {
      inpname=paste("inp",toString(column),sep="")
      nname = bdNames[column]
      rng = input[[inpname]]
      bd <- bd[bd[nname] >= rng[1],]
      bd <- bd[bd[nname] <= rng[2],]
      #cat("-----------", inpname, nname, rng, length(bd[nname]), sep = '\n')
    }
  })

  colorData <- reactive({
    data <- filteredData()
    print(paste("Coloring Data:", input$colVar, input$colSlider[1], input$colSlider[2]))
    data$color <- character(nrow(data))
    data$color <- "yellow"
    data$color[colorData[paste(input$colVar)] < input$colSlider[1]] <- "green"
    data$color[colorData[paste(input$colVar)] > input$colSlider[2]] <- "red"
    data
  })


  output$sliders <- renderUI({
  fluidRow(
    lapply(1:length(bdNames), function(i) {
      column(2,
        sliderInput(paste0('inp', i), bdNames[i],
          step = signif((unname(bdmax[bdNames[i]])-unname(bdmin[bdNames[i]]))*0.01, digits = 2),
          min = signif(unname(bdmin[bdNames[i]])*0.95, digits = 2),
          max = signif(unname(bdmax[bdNames[i]])*1.05, digits = 2),
          value = c(signif(unname(bdmin[bdNames[i]])*0.95, digits = 2),signif(unname(bdmax[bdNames[i]])*1.05, digits = 2))
        )
      )
    })
  )
  })


  output$stats <- renderText({
    infoTable()
  })

  infoTable <- eventReactive(input$updateStats, {
    colorCounts = table(bd$colors)
    paste0("Total Points: ", nrow(bladedat),
           "\nCurrent Points: ", nrow(bd),
           "\nGreen Points: ", try(colorCounts[["green"]]),
           "\nYellow Points: ", try(colorCounts[["yellow"]]),
           "\nRed Points: ", try(colorCounts[["red"]])
    )
  })

  # Show the values using an HTML table
  output$pairsPlot <- renderPlot({

    idx = 0

    if (length(input$display) < 2) {
      # Too few inputs, print message asking for more.
      print("Too few inputs.")

    } else {

      print("Getting Variable List.")
      for(choice in 1:length(input$display)) {
        mm <- match(input$display[choice],bdNames)
        if(mm > 0) { idx <- c(idx,length(bdNames)- mm + 1 ) }
      }
      print(idx)

      print("Trimming Data.")
      bd <<- bladedat
      for(column in 1:length(bdNames)) {
        inpname=paste("inp",toString(column),sep="")
        nname = bdNames[column]
        rng = input[[inpname]]
        bd <<- bd[bd[nname] >= rng[1],]
        bd <<- bd[bd[nname] <= rng[2],]

        bdmin <<- apply(bd,2,min)
        bdmax <<- apply(bd,2,max)
        #cat("-----------", inpname, nname, rng, length(bd[nname]), sep = '\n')
      }

      if(length(bd[idx]) > 0 && !is.null(input$colVar)) {
        print(paste("Coloring Data:", input$colVar, input$colSlider[1], input$colSlider[2]))
        bd$colors <<- character(nrow(bd))
        bd$colors <<- "yellow"
        bd$colors[bd[paste(input$colVar)] < input$colSlider[1]] <<- "green"
        bd$colors[bd[paste(input$colVar)] > input$colSlider[2]] <<- "red"
        print("Rendering Plot.")
        pairs(bd[idx],lower.panel = panel.smooth,upper.panel=NULL, col=bd$colors)
        print("Plot Rendered.")

        updateColorSlider();
      }
    }

  })

  output$singlePlot <- renderPlot({

    idx = 0
    mm <- match(input$xInput,bdNames)
    if(mm > 0) { idx <- c(idx,length(bdNames)- mm + 1 ) }
    mm <- match(input$yInput,bdNames)
    if(mm > 0) { idx <- c(idx,length(bdNames)- mm + 1 ) }

    print(idx)

    bd <- bladedat
    for(column in 1:length(bdNames)) {
      inpname=paste("inp",toString(column),sep="")
      nname = bdNames[column]
      rng = input[[inpname]]
      bd <- bd[bd[nname] >= rng[1],]
      bd <- bd[bd[nname] <= rng[2],]
      #cat("-----------", inpname, nname, rng, length(bd[nname]), sep = '\n')
    }
    if(length(bd[idx]) > 0) {
      plot(bd[idx])
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
    content = function(file) { write.csv(bd, file) }
  )

  output$exportPlot <- downloadHandler(
    filename = function() { paste('plot-', Sys.Date(), '.pdf', sep='') },
    content = function(file) {
      pdf(paste('plot-', Sys.Date(), '.pdf', sep=''))

      idx = 0

      print("Getting Variable List.")
      for(choice in 1:length(input$display)) {
        mm <- match(input$display[choice],bdNames)
        if(mm > 0) { idx <- c(idx,length(bdNames)- mm + 1 ) }
      }
      print(idx)

      print("Trimming Data.")
      bd <<- bladedat
      for(column in 1:length(bdNames)) {
        inpname=paste("inp",toString(column),sep="")
        nname = bdNames[column]
        rng = input[[inpname]]
        bd <<- bd[bd[nname] >= rng[1],]
        bd <<- bd[bd[nname] <= rng[2],]

        bdmin <<- apply(bd,2,min)
        bdmax <<- apply(bd,2,max)
        #cat("-----------", inpname, nname, rng, length(bd[nname]), sep = '\n')
      }

    if(length(bd[idx]) > 0 && !is.null(input$colVar)) {
        print(paste("Coloring Data:", input$colVar, input$colSlider[1], input$colSlider[2]))
        bd$colors <<- character(nrow(bd))
        bd$colors <<- "yellow"
        bd$colors[bd[paste(input$colVar)] < input$colSlider[1]] <<- "green"
        bd$colors[bd[paste(input$colVar)] > input$colSlider[2]] <<- "red"
        print("Rendering Plot.")
        pairs(bd[idx],lower.panel = panel.smooth,upper.panel=NULL, col=bd$colors)
        print("Plot Rendered.")
      }

      dev.off()
      file.copy(paste('plot-', Sys.Date(), '.pdf', sep=''), file)
    }
  )

  updateColorSlider <- function(x) {
  updateSliderInput(session,
                    "colSlider",
                    step = signif((max(bd[paste(input$colVar)], na.rm=TRUE)-min(bd[paste(input$colVar)], na.rm=TRUE))*0.01, digits = 2),
                    min = signif(unname(bdAbsMin[paste(input$colVar)])*0.95, digits = 2),
                    max = signif(unname(bdAbsMax[paste(input$colVar)])*1.05, digits = 2),
                    value = c(min(bd[paste(input$colVar)], na.rm=TRUE)+0.33*(max(bd[paste(input$colVar)], na.rm=TRUE)-min(bd[paste(input$colVar)], na.rm=TRUE)), min(bd[paste(input$colVar)], na.rm=TRUE)+0.66*(max(bd[paste(input$colVar)], na.rm=TRUE)-min(bd[paste(input$colVar)], na.rm=TRUE)))
                    )

                        # updateSliderInput(session, "inp1", value=c(input$plot_brush$xmin, input$plot_brush$xmax))
                        # updateSliderInput(session, inputID = paste0("inp", "1"),
                        #                   min = input$plot_brush$xmin,
                        #                   max = input$plot_brush$xmax,
                        #                   value = c(input$plot_brush$xmin, input$plot_brush$xmax),
                        #                   step = ((input$plot_brush$xmax - input$plot_brush$xmax)*0.1))
                        # updateSliderInput(session,
                        #                   inputID = paste0("inp", match(input$xInput, bdNames)),
                        #                   min = input$plot_brush$xmin,
                        #                   max = input$plot_brush$xmax,
                        #                   value = c(input$plot_brush$xmin, input$plot_brush$xmax),
                        #                   step = ((input$plot_brush$xmax - input$plot_brush$xmax)*0.1))
                        # updateSliderInput(session,
                        #                   inputID = paste0("inp", match(input$yInput, bdNames)),
                        #                   min = input$plot_brush$ymin,
                        #                   max = input$plot_brush$ymax,
                        #                   value = c(input$plot_brush$ymin, input$plot_brush$ymax),
                        #                   step = ((input$plot_brush$ymax - input$plot_brush$ymax)*0.1))

  }
  observe({
    print("Observing.")
    if (!is.null(input$colVar) && !is.null(bd)) {
        updateColorSlider()
    }
  })
})
