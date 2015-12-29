library(shiny)

# Define UI for PET Design Space Browser application
shinyUI(fluidPage(

  #  Application title
  titlePanel("PET Design Space Browser"),
  #verbatimTextOutput("debug"),
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
            checkboxInput("autoRender", "Automatically Rerender", value = TRUE),
            conditionalPanel(
              condition = "input.autoRender == false",
              actionButton("renderPlot", "Render Plot"),
              br()
            ), hr(),
            checkboxInput("color", "Color Data", value = FALSE),
            conditionalPanel(
              condition = "input.color == true",
              selectInput("colType", "Type:", choices = c("Max/Min", "Discrete"), selected = "Max/Min"),
              conditionalPanel(
                condition = "input.colType == 'Max/Min'",
                selectInput("colVarNum", "Colored Variable:", c()),
                radioButtons("radio", NULL, c("Maximize" = "max", "Minimize" = "min")),
                sliderInput("colSlider", NULL, min=0, max=1, value=c(0.3,0.7), step=0.1)
              )
              # conditionalPanel(
              #   condition = "input.colType == 'Discrete'",
              #   selectInput("colVarFactor")
              # )
            ), hr(),
            p(strong("Info:")), #br(),
            verbatimTextOutput("stats"),
            actionButton("updateStats", "Update"), br(), br(), hr(),
            p(strong("Currently Filtered Data:")),
            downloadButton('exportData', 'Dataset'),
            paste("          "),
            downloadButton('exportPlot', 'Plot'), hr(),
            actionButton("resetOptions", "Reset to Default Options")
          )
        ),
        column(9,
          plotOutput("pairsPlot", height=700)
        )
      )
    ),
    tabPanel("Single Plot",
      fluidRow(
        column(3,
          br(),
          wellPanel(
            selectInput("xInput", "X-axis", c()),
            selectInput("yInput", "Y-Axis", c()),
            br(),
            p(strong("Adjust Sliders to Selection:")),
            actionButton("updateX", "X"),
            actionButton("updateY", "Y"),
            actionButton("updateBoth", "Both")
          )
        ),
        column(9,
          plotOutput("singlePlot", click = "plot_click", brush = "plot_brush", height=700)
        ),
        column(12,
          verbatimTextOutput("info")
        )
      )
    ),
    tabPanel("Data Table",
      wellPanel(
        fluidRow(
          br(), actionButton("updateDataTable", "Update Data Table"), br(), br()
        ),
        fluidRow(
          dataTableOutput(outputId="table")
        )
      )
    )
  ),
  h3("Filter Data:"),
  actionButton("resetSliders", "Reset All Sliders"), br(), br(),
  uiOutput("enums"),
  uiOutput("sliders"),
  h3("Constants:"),
  uiOutput("constants")
  
  
)
)
