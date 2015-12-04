library(shiny)


bd <- read.csv("../data.csv",fill=T)
bd[is.na(bd)] <- 0
bdNames = ls(bd,sort=FALSE)
bdmin = apply(bd,2,min)
bdmax = apply(bd,2,max)

print(bdNames)

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
                        bdNames,
                        multiple = TRUE,
                        selected = bdNames[c(1,2)]),
            selectInput("colVar", "Colored Variable:", bdNames, selected = bdNames[c(1)]),
            sliderInput("colSlider", NULL, min=0, max=1, value=c(0.3,0.7), step=0.1),
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
            selectInput("xInput", "X-axis", bdNames, selected = bdNames[c(1)]),
            selectInput("yInput", "Y-Axis", bdNames, selected = bdNames[c(2)])
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
)
)
