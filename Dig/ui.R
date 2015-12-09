library(shiny)


raw <- read.csv("../data.csv",fill=T)
raw[is.na(raw)] <- 0

varNames = ls(raw,sort=FALSE)
varClass = sapply(raw,class)

rawMin = apply(raw,2,min)
rawMax = apply(raw,2,max)

print(varNames)

# Define UI for BladeMDA application
shinyUI(fluidPage(

  #  Application title
  titlePanel("BladeMDA Design Space Browser"),
  tabsetPanel(
    tabPanel("Pairs Plot",
      fluidRow(
        column(3,
          br(),
          wellPanel(
            selectInput("display",
                        "Display:",
                        varNames,
                        multiple = TRUE,
                        selected = varNames[c(1,2)]),
            checkboxInput("autoRender", "Automatically Rerender", value = FALSE),
            conditionalPanel(
              condition = "input.autoRender == false",
              actionButton("renderPlot", "Render Plot"),
              br()
            ),
            checkboxInput("color", "Color Data", value = TRUE),
            conditionalPanel(
              condition = "input.color == true",
              selectInput("colVar", "Colored Variable:", varNames, selected = varNames[c(1)]),
              radioButtons("radio", NULL, c("Maximize" = "max", "Minimize" = "min")),
              sliderInput("colSlider", NULL, min=0, max=1, value=c(0.3,0.7), step=0.1)
            ),
            p(strong("Info:")),
            actionButton("updateStats", "Update"),
            br(),
            verbatimTextOutput("stats"),
            p(strong("Currently Filtered Data:")),
            downloadButton('exportData', 'Dataset'),
            paste("          "),
            downloadButton('exportPlot', 'Plot')
          )
        ),
        column(9,
          plotOutput("pairsPlot", height=700)
            # tabPanel("Table", tableOutput("table"))
        )
      )
    ),
    tabPanel("SinglePlot",
      fluidRow(
        column(3,
          br(),
          wellPanel(
            p(strong("Plot:")),
            selectInput("xInput", "X-axis", varNames, selected = varNames[c(1)]),
            selectInput("yInput", "Y-Axis", varNames, selected = varNames[c(2)]),
            br(),
            p(strong("Selection:")),
            actionButton("updateX", "Constrain X"),
            actionButton("updateY", "Constrain Y"),
            actionButton("updateBoth", "Constrain Both")
            # p(strong("Info:")),
            # actionButton("updateStatsSingle", "Update"),
            # br(),
            # verbatimTextOutput("statsSingle")
          )
        ),
        column(9,
          plotOutput("singlePlot", click = "plot_click", brush = "plot_brush", height=700)
        ),
        column(12,
          # tableOutput('table')
          verbatimTextOutput("info")
        )
      )
    )
  ),
  fluidRow(
    lapply(1:length(varNames), function(i) {
      column(2,
        if(varClass[i] == "numeric" | varClass[i] == "integer") {
          sliderInput(paste0('inp', i),
                      varNames[i],
                      step = signif((unname(rawMax[varNames[i]])-unname(rawMin[varNames[i]]))*0.01, digits = 2),
                      min = signif(unname(rawMin[varNames[i]])*0.95, digits = 2),
                      max = signif(unname(rawMax[varNames[i]])*1.05, digits = 2),
                      value = c(signif(unname(rawMin[varNames[i]])*0.95, digits = 2),signif(unname(rawMax[varNames[i]])*1.05, digits = 2)))
        } else {
          if (varClass[i] == "factor") {
            selectInput(paste0('inp', i),
                        varNames[i],
                        multiple = TRUE,
                        selectize = FALSE,
                        choices = names(table(raw[varNames[i]])))
          }
        }
      )
    })
  )
)
)
