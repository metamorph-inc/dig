library(shiny)
library(GGally)
#options(shiny.trace=TRUE)
#options(shiny.fullstacktrace = TRUE)
#options(error = function() traceback(2))
#options(shiny.error = function() traceback(2))

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
         "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

#Use project folder for saved presets
outputDir <- "."
#Items to be saved for user presets

#Function for saving user presets
saveData <- function(data) {
  #data <- t(data)
  # Create unique filename
  fileName <- sprintf("%s_%s.csv", 
                      as.integer(Sys.time()), 
                      digest::digest(data))
  # Write file to local system
  write.table(
    x = data,
    file = file.path(outputDir, fileName),
    row.names = FALSE, 
    col.names = FALSE,
    sep = ", ",
    quote = TRUE
  )
}

shinyServer(function(input, output, clientData, session) {
  # Get Data -----------------------------------------------------------------
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


  #Variable for loading in user preset
  # filedata <- reactive({
  #   infile <- input$presetfile
  #   if(is.null(infile)) {
  #     return(NULL)
  #   }
  #   read.csv(infile$datapath, header = TRUE, stringsAsFactors = FALSE)
  # })
  
  filedata <- eventReactive(
    input$importSession, {
      file <- file.choose()
      read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  })

  #Changes values based on user uploaded csv file
  observe({
    
    if(!is.null(filedata())){
      #updateSelectInput(session, "display", selected = varRange[c(1,2)])

      for(i in 1:length(colnames(filedata()))){
        
        current <- colnames(filedata())[i]
        column <- as.numeric(gsub("[^0-9]", "", current)) #Extract number
        
        if(!is.null(filedata()[current]) & !is.na(column)){
          if (varClass[column] == "factor" & length(names(table(raw[varNames[column]]))) > 1) {
            parsedValue <- as.list(strsplit(toString(filedata()[current]), ", ")[[1]])
            trimmedValue <- gsub("^\\s+|\\s+$", "", parsedValue)
            updateSelectInput(
              session,
              current,
              selected = trimmedValue
            )
          }
          else {
            if(varClass[column] == "numeric") {
              updateSliderInput(
                session,
                current,
                value = as.numeric(unlist(strsplit(toString(filedata()[current]), ", ")))
              )
            }
            if (varClass[column] == "integer") {
              updateSliderInput(
                session,
                current,
                value = as.numeric(unlist(strsplit(toString(filedata()[current]), ", ")))
              )
            }
          }
        }
        else {
          if(current == 'colSlider'){
            print("Updated colslider from csv")
            updateSliderInput(
              session,
              current,
              value = as.numeric(unlist(strsplit(toString(filedata()[current]), ", ")))
            )
          }
          else {
            if (current == 'autoRender' | current == 'removeMissing'){
              trimmedValue <- gsub("^\\s+|\\s+$", "", filedata()[current])
              updateCheckboxInput(
                session,
                current,
                value = as.logical(trimmedValue)
              )
            }
            else {
              parsedValue <- as.list(strsplit(toString(filedata()[current]), ", ")[[1]])
              trimmedValue <- gsub("^\\s+|\\s+$", "", parsedValue)
              updateSelectInput(
                session,
                current,
                selected = trimmedValue
              )
            }
          }
        }
      }
    }
  })
  
  #Save all fields to csv
  formData <- reactive({
    presets <- c()
    # Saving Filter Data
    for(column in 1:length(varNames)) {
      newitem <- paste0('inp', column)
      
      if (varClass[column] == "factor" & length(names(table(raw[varNames[column]]))) > 1) {
        newpreset <- c(newitem, toString(input[[newitem]]))
        presets <- cbind(presets, newpreset)
      }
      
      if (varClass[column] == "numeric") {
        max <- as.numeric(unname(rawAbsMax[varNames[column]]))
        min <- as.numeric(unname(rawAbsMin[varNames[column]]))
        diff <- (max-min)
        # print(paste(column, "min", min, "max", max, "diff", diff))
        if (diff != 0) {
          newpreset <- c(newitem, toString(input[[newitem]]))  
          presets <- cbind(presets, newpreset)
        }
      } 
      
      if (varClass[column] == "integer") {
        max <- as.integer(unname(rawAbsMax[varNames[column]]))
        min <- as.integer(unname(rawAbsMin[varNames[column]]))
        if (min != max) {
          newpreset <- c(newitem, toString(input[[newitem]]))  
          presets <- cbind(presets, newpreset)
        }
      }
    }
    
    # Saving additional plot options
    display <- c('display', toString(input$display))
    color <- c('colType', input$colType)
    colVarNum <- c('colVarNum', input$colVarNum)
    colVarFac <- c('colVarFactor', input$colVarFactor)
    varMinMax <- c('radio', input$radio)
    colSlider <- c('colSlider', toString(input$colSlider))
    xInput <- c('xInput', input$xInput)
    yInput <- c('yInput', input$yInput)
    removeMissing <- c('removeMissing', input$removeMissing)
    autoRender <- c('autoRender', input$autoRender)
    pointStyle <- c('pointStyle', input$pointStyle)
    pointSize <- c('pointSize', input$pointSize)
    
    presets <- cbind(presets, display, color, colVarNum, colVarFac, varMinMax, 
                     colSlider, xInput, yInput, removeMissing, autoRender,
                     pointStyle, pointSize)
    presets
    
  })
  
  #Call when user saves data
  output$exportSession <- downloadHandler(
    filename = function() { 
      if (input$sessionName == ""){
        paste0('session_', Sys.Date(), '.csv')
      }
      else {
        paste0(input$sessionName, '.csv')
      }
    },
    content = function(file) { 
      write.table(
        x = formData(),
        file,
        row.names = FALSE, 
        col.names = FALSE,
        sep = ", ",
        quote = TRUE
      )
    }
  )
  
  observeEvent(
    input$exportSession, {
    print("clicked button dawg")}
  )

  # Pre-processing -----------------------------------------------------------
  # print(str(raw))
  
  print("Starting Preprocessing of the Data -----------------------------------------")
  
  varNames = names(raw)
  varClass = sapply(raw,class)
  print(paste("varClass:"))
  print(paste(varClass))
  
  varFac <- varNames[varClass == "factor"]
  print(paste("varFac:"))
  print(paste(varFac))
  
  varNum <- varNames[varClass != "factor"]
  print(paste("varNum:"))
  print(paste(varNum))
  
  rawAbsMin = apply(raw[varNum], 2, min, na.rm=TRUE)
  rawAbsMax = apply(raw[varNum], 2, max, na.rm=TRUE)
  print(paste(rawAbsMax))
  print(paste(rawAbsMin))

  varRangeNum <- varNum[(rawAbsMin != rawAbsMax) & (rawAbsMin != Inf)]
  print(paste("varRangeNum:"))
  print(paste(varRangeNum))
  
  varRangeFac <- varFac[apply(raw[varFac], 2, function(x) (length(names(table(x))) > 1))]
  print(paste("varRangeFac:"))
  print(paste(varRangeFac))
  
  varRange <- c(varRangeFac, varRangeNum)
  print(paste("varRange:"))
  print(paste(varRange))
  
  print("Updating Panel Selections...")
  updateSelectInput(session, "colVarNum", choices = varRangeNum, selected = varRangeNum[c(1)])
  updateSelectInput(session, "display", choices = varRange, selected = varRange[c(1,2)])
  updateSelectInput(session, "xInput", choices = varRange, selected = varRange[c(1)])
  updateSelectInput(session, "yInput", choices = varRange, selected = varRange[c(2)])
  
  resetPlotOptions <- observeEvent(input$resetOptions, {
    print("In resetPlotOptions()")
    updateSelectInput(session, "display", selected = varRange[c(1,2)])
    updateCheckboxInput(session, "color", value = FALSE)
    # updateSelectInput(session, "colType", selected = "Max/Min")
  })
  
  # resetViewerOptions <- observeEvent(input$resetSettings, {
  #   print("In resetViewerSettings()")
  #   updateCheckboxInput(session, "autoRender", value = TRUE)
  #   updateRadioButtons(session, "pointStyle", choices = c("Normal" = 1,"Filled" = 19), selected = "Normal")
  #   updateRadioButtons(session, "pointSize", choices = c("Small" = 1,"Medium" = 1.5,"Large" = 2), selected = "Small")
  # })
  
  print(paste("Finished Preprocessing the Data ----------------------------------------------------"))
  
  
  #Initializing -------------------------------------------------------
  updateSelectInput(
    session,
    "colVarFactor",
    choices = varFac
  )

  # Sliders ------------------------------------------------------------------
  output$enums <- renderUI({
    fluidRow(
      lapply(1:length(varNames), function(column) {
        if (varClass[column] == "factor" & length(names(table(raw[varNames[column]]))) > 1) {
          column(2,
                 selectInput(paste0('inp', column),
                             varNames[column],
                             multiple = TRUE,
                             selectize = FALSE,
                             choices = names(table(raw[varNames[column]])),
                             selected = names(table(raw[varNames[column]])))
          )
        }
      })
    )
  })
  
  output$sliders <- renderUI({
    fluidRow(
      lapply(1:length(varNames), function(column) {
        if(varClass[column] == "numeric") {
          max <- as.numeric(unname(rawAbsMax[varNames[column]]))
          min <- as.numeric(unname(rawAbsMin[varNames[column]]))
          diff <- (max-min)
          # print(paste(column, "min", min, "max", max, "diff", diff))
          if (diff != 0) {
            #print(paste(column, varNames[column], as.numeric(unname(rawAbsMax[varNames[column]]))))
            step <- max(diff*0.01, abs(min)*0.001, abs(max)*0.001)
            # cat("step", diff*0.01, abs(min)*0.001, abs(max)*0.001, "\n", sep = " ")
            column(2,
              sliderInput(paste0('inp', column),
                          varNames[column],
                          step = signif(step, digits = 4),
                          min = signif(min-step*10, digits = 4),
                          max = signif(max+step*10, digits = 4),
                          value = c(signif(min-step*10, digits = 4), signif(max+step*10, digits = 4)))
            )
          }
        } else {
          if (varClass[column] == "integer") {
            max <- as.integer(unname(rawAbsMax[varNames[column]]))
            min <- as.integer(unname(rawAbsMin[varNames[column]]))
            if (min != max) {
              column(2, 
                     sliderInput(paste0('inp', column),
                            varNames[column],
                            min = min,
                            max = max,
                            value = c(min, max))
              )
            }
          }
        }
      })
    )
  })
  
  output$constants <- renderUI({
    fluidRow(
      lapply(1:length(varNames), function(column) {
        # print(paste(column, varNames[column], varClass[column]))
        if(varClass[column] == "numeric") {
          max <- as.numeric(unname(rawAbsMax[varNames[column]]))
          min <- as.numeric(unname(rawAbsMin[varNames[column]]))
          diff <- (max-min)
          if (diff == 0) {column(2, p(strong(paste0(varNames[column],":")), min))}
        } else {
          if (varClass[column] == "integer") {
            max <- as.integer(unname(rawAbsMax[varNames[column]]))
            min <- as.integer(unname(rawAbsMin[varNames[column]]))
            if (min == max) {column(2, p(strong(paste0(varNames[column],":")), min))}
          } else {
            if (varClass[column] == "factor" & length(names(table(raw[varNames[column]]))) == 1) {
              column(2, p(strong(paste0(varNames[column],":")), names(table(raw[varNames[column]]))))
            }
          }
        }
      })
    )
  })
  
  resetDefaultSliders <- observeEvent(input$resetSliders, {
    print("In resetDefaultSliders()")
    for(column in 1:length(varNames)) {
      if(varClass[column] == "numeric") {
        max <- as.numeric(unname(rawAbsMax[varNames[column]]))
        min <- as.numeric(unname(rawAbsMin[varNames[column]]))
        diff <- (max-min)
        if (diff != 0) {
          step <- max(diff*0.01, abs(min)*0.001, abs(max)*0.001)
          updateSliderInput(session, paste0('inp', column), value = c(signif(min-step*10, digits = 4), signif(max+step*10, digits = 4)))
        }
      } else {
        if(varClass[column] == "integer") {
          max <- as.integer(unname(rawAbsMax[varNames[column]]))
          min <- as.integer(unname(rawAbsMin[varNames[column]]))
          if(min != max) {
            updateSliderInput(session, paste0('inp', column), value = c(min, max))
          }
        } else {
          if(varClass[column] == "factor") {
            updateSelectInput(session, paste0('inp', column), selected = names(table(raw[varNames[column]])))
          }
        }
      }
    }
  })
  
  # Data functions -----------------------------------------------------------
  filterData <- reactive({
    print("In filterData()")
    data <- raw
    # print(paste("Length of VarNames:", length(varNames)))
    for(column in 1:length(varNames)) {
      inpName=paste("inp",toString(column),sep="")
      nname = varNames[column]
      rng = input[[inpName]]
      # print(paste("column: ", column, "Checking", nname, "rng", rng[1], "(", rawAbsMin[column], ",", rawAbsMax[column], ")", rng[2]))
      if(length(rng) != 0) {
        if((varClass[column]=="numeric" | varClass[column]=="integer")) {
          print(paste("Filtering", nname, "between", rng[1], "and", rng[2]))
          above <- (data[[nname]] >= rng[1])
          below <- (data[[nname]] <= rng[2])
          inRange <- above & below
        } else {
          if (varClass[column]=="factor") {
            print(paste(varNames[column],class(rng)))
            # print(paste(rng))
            inRange <- (data[[nname]] %in% rng)
          }
        }
        if(input$removeMissing == FALSE) {inRange <- inRange | is.na(data[[nname]])}
        data <- subset(data, inRange)
      }
      
      # cat("-----------", inpName, nname, rng, length(data[nname]), sep = '\n')
    }
    print("Data Filtered")
    data
  })

  colorData <- reactive({
    print("In colorData()")
    slider <- input$colSlider
    data <- filterData()
    #validate(need(nrow(data) > 0, "No data points fit the current filtering scheme"))
    data$color <- character(nrow(data))
    data$color <- "black"
     if (input$colType == "Max/Min") {
      name <- isolate(input$colVarNum)
      bottom <- slider[1]
      top <- slider[2]
      print(paste("Coloring Data:", name, bottom, top))
      data$color[(data[[name]] >= bottom) & (data[[name]] <= top)] <- "yellow"
      if (input$radio == "max") {
        data$color[data[[name]] < bottom] <- "red"
        data$color[data[[name]] > top] <- "green"
      } else {
        data$color[data[[name]] < bottom] <- "green"
        data$color[data[[name]] > top] <- "red"
      }
     } 
     else {
       if (input$colType == "Discrete") {
         varList = names(table(raw[input$colVarFactor]))
         for(i in 1:length(varList)){
           data$color[(data[[input$colVarFactor]] == varList[i])] <- palette()[i]
         }
       }
     }
    print("Data Colored")
    data
  })

  output$colorLegend <- renderUI({
    listSize <- length(names(table(raw[input$colVarFactor])))
    rawLabel <- ""
    for(i in 1:listSize){
      rawLabel <- HTML(paste(rawLabel, "<font color=", palette()[i], "<b>", "&#9632", " ",
                         names(table(raw[input$colVarFactor]))[i], '<br/>'))
    }
    rawLabel
  })

  # Pairs Tab ----------------------------------------------------------------
  
  pairs_data <- reactive({
    if (input$autoRender == TRUE) {
      data <- colorData()
    } else {
      data <- slowData()
    }
    
    data
  })
  
  pairs_vars <- reactive({
    if (input$autoRender == TRUE) {
      vars <- varsList()
    } else {
      vars <- slowVarsList()
    }
    
    vars
  })
  
  output$pairsPlot <- renderPlot({
    
    output$displayVars <- renderText("")
    output$filterVars <- renderText("")
    
    if (length(input$display) >= 2 & nrow(filterData()) > 0) {
      print("Rendering Plot.")
      # if(input$colType == 'Discrete') {
      #   print("Printing 'Discrete' plot.")
      #   pairs(data[vars],lower.panel = panel.smooth,upper.panel=NULL, col=data$color, pch = as.numeric(input$pointStyle))
      #   legend('topright',legend=levels(colorData()[[paste(varFactor[1])]]),pch=1,title=paste(varFactor[1]))
      # } else {
        # print(as.numeric(input$pointStyle))
          pairs(pairs_data()[pairs_vars()],
               lower.panel = panel.smooth,
               upper.panel=NULL, 
               col = pairs_data()$color,
               pch = as.numeric(input$pointStyle), 
               cex = as.numeric(input$pointSize))
      # }
      print("Plot Rendered.")
    }
    else {
      if (nrow(filterData()) == 0) {
        output$filterVars <- 
          renderText(
            "No data points fit the current filtering scheme")
      }
      if (length(input$display) < 2) {
        output$displayVars <- 
          renderText(
            "Please select two or more display variables.")
      }
    }
  })
  
  output$pairsDisplay <- renderUI({
    plotOutput("pairsPlot", click = "pairs_click", brush = "pairs_brush", height=700)
  })
  
  output$filterError <- renderUI({
    h4(textOutput("filterVars"), align = "center")
  })
  
  output$displayError <- renderUI({
    h4(textOutput("displayVars"), align = "center")
  })
  
  # output$pairs_info <- renderPrint({
  #   t(brushedPoints(pairs_data(), input$pairs_brush,
  #                   xvar = input$display[1],
  #                   yvar = input$display[2]))
  # })
  
  
  varsList <- reactive({
    print("Getting Variable List.")
    idx = 0
    for(choice in 1:length(input$display)) {
      mm <- match(input$display[choice],varNames)
      if(!is.null(mm) & mm > 0) { idx <- c(idx,mm) }
    }
    print(idx)
    idx
  })
  
  slowVarsList <- eventReactive(input$renderPlot, {
    print(paste("input$renderPlot:", input$renderPlot))
    print("Getting Variable List.")
    idx = 0
    for(choice in 1:length(input$display)) {
      mm <- match(input$display[choice],varNames)
      if(!is.null(mm) & mm > 0) { idx <- c(idx,mm) }
    }
    print(idx)
    idx
  })
  
  slowData <- eventReactive(input$renderPlot, {
    colorData()
  })
  
  output$stats <- renderText({
    infoTable()
  })

  infoTable <- eventReactive(input$updateStats, {
    tb <- table(factor(colorData()$color, c("green", "yellow", "red", "black")))
    if (input$colType == 'Max/Min') {
      paste0("Total Points: ", nrow(raw),
             "\nCurrent Points: ", nrow(filterData()),
             "\nVisible Points: ", sum(tb[["green"]], tb[["yellow"]], tb[["red"]], tb[["black"]]),
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
  
  # Single Plot Tab ----------------------------------------------------------

  output$singlePlot <- renderPlot({
    data <- filterData()
    plot(data[[paste(input$xInput)]], data[[paste(input$yInput)]], xlab = paste(input$xInput), ylab = paste(input$yInput), pch = as.numeric(input$pointStyle))
  })

  output$info <- renderPrint({
    t(nearPoints(filterData(), 
                 input$plot_click, 
                 xvar = input$xInput, 
                 yvar = input$yInput, 
                 maxpoints = 8))
  })

  # Data Table Tab --------------------------------------------------------------------------------
  output$table <- renderDataTable({
    input$updateDataTable
    data <- isolate(filterData())
  })
  
  # UI Adjustments -----------------------------------------------------------
  updateColorSlider <- function() {
    data <- isolate(filterData())
    min <- min(data[[paste(input$colVarNum)]], na.rm=TRUE)
    max <- max(data[[paste(input$colVarNum)]], na.rm=TRUE)
    print(paste("colSlider:", isolate(input$colSlider[1]), isolate(input$colSlider[2])))
    print(paste("In updateColorSlider(). colVarNum:", input$colVarNum, min, max))
    thirtythree <- quantile(data[[paste(input$colVarNum)]], 0.33, na.rm=TRUE)
    sixtysix <- quantile(data[[paste(input$colVarNum)]], 0.66, na.rm=TRUE)
    
    absMin <- as.numeric(unname(rawAbsMin[paste(input$colVarNum)]))
    absMax <- as.numeric(unname(rawAbsMax[paste(input$colVarNum)]))
    absStep <- max((max-min)*0.01, abs(min)*0.001, abs(max)*0.001)
    # print(paste("class(max)", class(max), "class(min)", class(min)))
    # print(paste("class(absMax)", class(absMax), "class(absMin)", class(absMin), "class(absStep)", class(absStep)))
    # print(paste(absMin, min, max, absMax))
    if(varClass[[input$colVarNum]] == "numeric") {
      # print("In updated slider: numeric")
      # if (absMax == absMin) {absMax <- (absMax + 1)}
      if(is.null(filedata())){
        updateSliderInput(session,
                          "colSlider",
                          step = signif(absStep, digits = 4),
                          min = signif(absMin-absStep*10, digits = 4),
                          max = signif(absMax+absStep*10, digits = 4),
                          value = c(unname(thirtythree), unname(sixtysix))
        )
      }
      else {
        print("Updated colslider from csv")
        sliderValue <- as.numeric(unlist(strsplit(toString(filedata()['colSlider']), ", ")))
        if (input$colVarNum != gsub("^\\s+|\\s+$", "",  filedata()$colVarNum)){
          sliderValue <- c(unname(thirtythree), unname(sixtysix))
        }
        updateSliderInput(
          session,
          "colSlider",
          step = signif(absStep, digits = 4),
          min = signif(absMin-absStep*10, digits = 4),
          max = signif(absMax+absStep*10, digits = 4),
          value = sliderValue
        )
      }
    }
    if(varClass[[input$colVarNum]] == "integer") {
      # print("In updated slider: integer")
      # if (absMax == absMin) {absMax <- (absMax + 1)}
      updateSliderInput(session,
                        "colSlider",
                        min = absMin,
                        max = absMax,
                        value = c(floor(thirtythree), ceiling(sixtysix))
      )
    }
  }
  
  updateColorFSlider <- function() {
    data <- isolate(filterData())
    min <- min(data[[paste(input$colVarFac)]], na.rm=TRUE)
    max <- max(data[[paste(input$colVarFac)]], na.rm=TRUE)
    print(paste("colFSlider:", isolate(input$colFSlider[1]), isolate(input$colFSlider[2])))
    print(paste("In updateColorSlider(). colVarFac:", input$colVarFac, min, max))
    thirtythree <- quantile(data[[paste(input$colVarFac)]], 0.33, na.rm=TRUE)
    sixtysix <- quantile(data[[paste(input$colVarFac)]], 0.66, na.rm=TRUE)
  }

  updateXSlider <- observeEvent(input$updateX, {
    updateSlider(input$xInput, input$plot_brush$xmin, input$plot_brush$xmax)
  })
  
  updateYSlider <- observeEvent(input$updateY, {
    updateSlider(input$yInput, input$plot_brush$ymin, input$plot_brush$ymax)
  })
  
  updateBothSlider <- observeEvent(input$updateBoth, {
    updateSlider(input$xInput, input$plot_brush$xmin, input$plot_brush$xmax)
    updateSlider(input$yInput, input$plot_brush$ymin, input$plot_brush$ymax)
  })
  
  updateSlider <- function(varName, min, max) {
    if(!is.null(min) & !is.null(max)) {
      if(varName %in% varRangeNum) {
        print(paste0("Updating ", varName, " Slider: ", min, " to ", max))
        updateSliderInput(session,
                          paste0("inp", match(varName, varNames)),
                          value = c(min, max))
      } else {
        if(varClass[[varName]] == "factor") {
          selectedFactors <- ceiling(min):floor(max)
          names <- levels(filterData()[[varName]])[selectedFactors]
          print(paste0("Updating ", varName, ": ", names))
          updateSelectInput(session, paste0('inp', match(varName, varNames)), selected = names)
        } else {print(paste0("Error: can't update slider for '", varName, "'."))}
      }
    } else {print("Error: no selection available for update.")}
  }

  constrainXVariable <- eventReactive (input$updateX, {
    print("In constrainXVariable()")
    #print(paste("new upper value", input$xInput, "=", input$plot_brush$xmax))
    #updateSliderInput()
  })
  
  observe({
    print("Observing.")
    if (!is.null(isolate(colorData())) & !(as.character(input[["colVarNum"]]) == "")) {
      updateColorSlider()
    }
    # else {
    #   if (!is.null(isolate(colorData())) & !(as.character(input[["colVarFac"]]) == "")) {
    #     updateColorFSlider()
    #   }
    # }
  })
})
