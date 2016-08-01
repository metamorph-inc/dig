library(shiny)
require(shinyjs)

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
            conditionalPanel(condition = "input.autoInfo == false",
                            actionButton("updateStats", "Update"),
                            br()),  hr(),
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
              actionButton("highlightData", "Highlight Selection", class = "btn btn-primary")
            )
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
          br(), 
          conditionalPanel(condition = "input.autoData == false", 
                           actionButton("updateDataTable", "Update Data Table")
                           ), br(), br()
        ),
        fluidRow(
          dataTableOutput(outputId="table")
        )
      )
    ),
    tabPanel("Ranges",
     wellPanel(
        fluidRow(
          br(), 
          column(6, conditionalPanel(condition = "input.autoRange == false",
                           actionButton("updateRanges", "Update Ranges")), br(),
          downloadButton('exportRanges', 'Download Ranges'), br(), br())
        ),
        fluidRow(
          column(12,
                 verbatimTextOutput("ranges")
          )
        )
      )
    ),
    tabPanel("Options",
      fluidRow(
        column(6,
          br(),
          wellPanel(
            h4("Data Processing Options"),
            tags$div(title = "Remove data points with incomplete rows.",
              checkboxInput("removeMissing", "Remove Incomplete Rows", value = TRUE)),
            tags$div(title = "Remove data points outside of standard deviation.",
              checkboxInput("removeOutliers", "Remove Outliers", value = FALSE)),
            conditionalPanel("input.removeOutliers == '1'",
                             sliderInput("numDevs", HTML("&sigma;:"), min = 1, max = 11, step = 0.1, value = 2)
            ),
            hr(),
            
            h4("Plot Options"),
            tags$div(title = "Pairs Plot immediately reflects data filtering.",
              checkboxInput("autoRender", "Automatically Rerender Plot", value = TRUE)),
            strong("Data Point Style"),
            fluidRow(
              column(4, radioButtons("pointStyle", NULL, c("Normal" = 1,"Filled" = 19))),
              column(8, radioButtons("pointSize", NULL, c("Small" = 1, "Medium" = 1.5, "Large" = 2)))
            ),
            hr(),
            
            h4("Automatic Refresh"),
            tags$div(title = "Info pane on Pairs tab immediately reflects data fitlering.",
              checkboxInput("autoInfo", "Info Pane", value = TRUE)),
            tags$div(title = "Data tab immediately reflects data fitlering.",
              checkboxInput("autoData", "Data Table Tab", value = TRUE)),
            tags$div(title = "Ranges tab immediately reflects data filtering.",
              checkboxInput("autoRange", "Ranges Tab", value = TRUE)),
            hr(),

            h4("Color Options"),
            #h5("Min/Max", align = "center"),
            fluidRow(
              column(4, tags$div(title = "Color of default data point.",
                          colourInput("normColor", "Normal", "black")))
            ),
            fluidRow(
              column(4, tags$div(title = "Color of data below Max/Min range.",
                          colourInput("minColor", "Min", "#F1C40F"))),
              column(4, tags$div(title = "Color of data within Max/Min range.",
                          colourInput("midColor", "In Between", "#2ECC71"))),
              column(4, tags$div(title = "Color of data above Max/Min range.",
                          colourInput("maxColor", "Max", "#E74C3C")))
            ),
            #h5("Highlighted", align = "center"),
            fluidRow(
              column(4, tags$div(title = "Color of data selected by brush on Single Plot.",
                          colourInput("highlightColor", "Highlighted", "#377EB8")))
            ), hr(), br(), br(),


            tags$div(title = "A session preserves all of the user-selected options, plotting, and filtering.",
                      h4("Session Options")),
            strong("Save Session"),
            textInput("sessionName", NULL, placeholder = "Enter a filename..."),
            downloadButton("exportSession", "Download"),
            br(), br(),
            strong("Load Session"), br(),
            actionButton('importSession', 'Choose File'),
            hr(),
            
            h4("About"),
            p(strong("Version:"), "v1.2.4"),
            p(strong("Date:"), "7/27/2016"),
            p(strong("Developer:"), "Metamorph Software"),
            p(strong("Support:"), "tthomas@metamorphsoftware.com")
          )
        ),
      column(9))
    ),
  id = "inTabset"),
  h3("Filter Data:"),
  actionButton("resetSliders", "Reset Sliders"), br(), br(),
  uiOutput("enums"),
  uiOutput("sliders"),
  h3("Constants:"),
  uiOutput("constants")
  
  
  
)
)
