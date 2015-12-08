library(shiny)

# Define UI for BladeMDA application
shinyUI(fluidPage(

  #  Application title
  titlePanel("Title"),
  tabsetPanel(
    tabPanel("Pairs Plot",
      fluidRow(
        column(3,
          br(),
          wellPanel(
            selectInput("display",
                        "Display:",
                        c(),
                        multiple = TRUE),
            selectInput("colVar", "Colored Variable:", c()),
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
            selectInput("xInput", "X-axis", c()),
            selectInput("yInput", "Y-Axis", c())
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
  uiOutput("sliders")
)
)
