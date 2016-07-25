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
            h4("Plot Options"),
            selectInput("display",
                        "Display Variables:",
                        c(),
                        multiple = TRUE),
            conditionalPanel(
              condition = "input.autoRender == false",
              actionButton("renderPlot", "Render Plot"),
              br()
            ), hr(),
              h4("Data Coloring"),
              selectInput("colType", "Type:", choices = c("None", "Max/Min", "Discrete", "Highlighted"), selected = "None"),
              conditionalPanel(
                condition = "input.colType == 'Max/Min'",
                selectInput("colVarNum", "Colored Variable:", c()),
                radioButtons("radio", NULL, c("Maximize" = "max", "Minimize" = "min"), selected = "max"),
                sliderInput("colSlider", NULL, min=0, max=1, value=c(0.3,0.7), step=0.1)
              ),
              conditionalPanel(
                condition = "input.colType == 'Discrete'",
                selectInput("colVarFactor", "Colored Variable:", c()),
                htmlOutput("colorLegend")
                # conditionalPanel(
                #   condition = "input.colVarFactor == 'designVariable.ResinName' 
                #              | input.colVarFactor == 'designVariable.IEC_WindClass' 
                #              | input.colVarFactor == 'designVariable.FabricName'",
                #   radioButtons("radioF", NULL, c("Maximize" = "max", "Minimize" = "min"), selected = "max"), br(),
                #   sliderInput("colFSlider", label = "", min = 0, max = 1, value = c(0,1), step = 1)
                # )
              )
              
            ,  hr(),
            h4("Info"), #br(),
            verbatimTextOutput("stats"),
            actionButton("updateStats", "Update"), br(), hr(),
            h4("Download"),
            downloadButton('exportData', 'Dataset'),
            paste("          "),
            downloadButton('exportPlot', 'Plot'), hr(),
            actionButton("resetOptions", "Reset to Default Options")
          )
        ),
        column(9,
            uiOutput("displayError"),   
            uiOutput("filterError"),
            uiOutput("pairsDisplay")
          
          #h4(textOutput("filterVars"), align = "center")
        )
        # column(6,
        #   verbatimTextOutput("pairs_info")
        # )
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
            p(strong("Adjust Sliders to Selection")),
            actionButton("updateX", "X"),
            actionButton("updateY", "Y"),
            actionButton("updateBoth", "Both"),
            br(), br(),
            #p(strong("Highlight Selection")),
            bootstrapPage(
              actionButton("selectedData", "Highlight Selection", class = "btn btn-primary")
            )
          )
        ),
        column(9,
          plotOutput("singlePlot", click = "plot_click", brush = "plot_brush", height=700)
        )
        # column(12,
        #   verbatimTextOutput("info")
        # )
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
    ),
    tabPanel("Options",
      fluidRow(
        column(6,
          br(),
          wellPanel(
            h4("Data Processing Options"),
            checkboxInput("removeMissing", "Remove Incomplete Rows", value = TRUE),
            fluidRow(
              column(4, checkboxInput("removeOutliers", "Remove Outliers", value = FALSE)),
              column(8, conditionalPanel("input.removeOutliers == '1'",
                               selectInput("numDevs", HTML("Within _ &sigma;'s of data"), choices = seq(5), selected = 2)
              ))
            ),
            hr(),
            
            h4("Render Options"),
            checkboxInput("autoRender", "Automatically Rerender", value = TRUE),
            br(),
            strong("Data Point Style"),
            fluidRow(
              column(4, radioButtons("pointStyle", NULL, c("Normal" = 1,"Filled" = 19))),
              column(8, radioButtons("pointSize", NULL, c("Small" = 1, "Medium" = 1.5, "Large" = 2)))
            ),
            hr(),
            
            h4("Session Options"),
            strong("Save Session"),
            textInput("sessionName", NULL, placeholder = "Enter a filename..."),
            downloadButton("exportSession", "Download"),
            br(), br(),
            strong("Load Session"), br(),
            actionButton('importSession', 'Choose File'),
            br(), br(), br(),
            hr(),
            
            h4("About"),
            p(strong("Version:"), "v1.2.2"),
            p(strong("Date:"), "7/25/2016"),
            p(strong("Developer:"), "Metamorph Software"),
            p(strong("Support:"), "wknight@metamorphsoftware.com")
          )
        ),
      column(9))
    )
  ),
  h3("Filter Data:"),
  actionButton("resetSliders", "Reset Sliders"), br(), br(),
  uiOutput("enums"),
  uiOutput("sliders"),
  h3("Constants:"),
  uiOutput("constants")
  
  
  
)
)
