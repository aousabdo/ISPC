library(shiny)

footer <- source("../footer.R")$value
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel(    HTML(
    '<div id="stats_header">
    	Interactive Statistical-Power Calculator
    <a href="http://www.statstudio.net/free-tools/" target="_blank">
    <img id="statstudio_logo" align="right" alt="StatStudio Logo" src="./StatStudio_Logo.png", height = 72, width = 72>
    </a>
    </div>'
  ),
  "Interactive Statistical-Power Calculator"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    wellPanel(   
      HTML('<p align=\"justify\">Interactive calculator to illustrate the power of a statistical 
             hypothesis test for a two-sided symmetrical t-test. This page shows how 
             statistical power is related to the p-value and the significance level</p>')),
    wellPanel(   
      sliderInput("alpha", 
                  "alpha:", 
                  min = 0.01,
                  max = 0.2, 
                  value = 0.1, 
                  step = 0.001)),
    wellPanel(
      helpText("Null Distribution:"),
      sliderInput("m1", 
                  HTML('Mean value for null distribution, &mu;1'),
                  min = 0.0,
                  max = 2, 
                  value = 0., 
                  step = 0.1),
      br(),
      sliderInput("sd1", 
                  HTML('Standard deviation for null distribution, &sigma;1:'), 
                  min = 0.5,
                  max = 2, 
                  value = 1., 
                  step = 0.1)),
    wellPanel(
      helpText("Sample Distribution:"),
      sliderInput("m2", 
                  HTML('Mean value for sample distribution, &mu;2'), 
                  min = 2.1,
                  max = 5, 
                  value = 3., 
                  step = 0.1),
      br(),
      sliderInput("sd2", 
                  HTML('Standard deviation for sample distribution, &sigma;2:'), 
                  min = 0.5,
                  max = 2, 
                  value = 1., 
                  step = 0.1)),
    wellPanel(
      checkboxInput(inputId = "show_arrows",
                    label = strong("Show Labels"),
                    value = TRUE)
    ),
    wellPanel(
      downloadButton("savePlot", "Download PDF Graphic"),
      br(),
      br(),
      downloadButton("savePNGPlot", "Download PNG Graphic")),
    footer()
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    HTML('<div class=\"span8\">
                                   <div id=\"SPICPlot\" class=\"shiny-plot-output\" style=\"position:fixed ; width: 60% ; height: 60%\">
                                                </div>                            
                    </div>')
#     plotOutput("SPICPlot", height = "550px")
  )
))