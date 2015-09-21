
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Study design for time to event outcomes in the presence of error-prone diagnostic tests or self-reports"),
  br(),
  br(),

  # Sidebar with a slider input for number of bins
  sidebarLayout(

      sidebarPanel(
      
        selectInput("select", "Choose a study you want to conduct",
                    choices = list("Sample Size Estimation" = 1,
                      "Comparison of power versus total sample size (N)" = 2)),
        
        conditionalPanel(
          condition = "input.select == 1",
          textInput("HR", "Hazard ratio:",
                    value = 1.25),
          sliderInput("sen", "Sensitivity:",
                      min = 0, max = 1, value = 0.55),
          sliderInput("spe", "Specificity:",
                      min = 0, max = 1, value = 0.99),
          sliderInput("power", "Power:",
                      min = 0, max = 1, value = 0.9)
          ),
        
        conditionalPanel(
          condition = "input.select == 2",
          sliderInput("sen2", "Sensitivity:",
                      min = 0, max = 1, value = 0.55),
          sliderInput("spe2", "Specificity:",
                      min = 0, max = 1, value = 0.99),
          sliderInput("tim", "Times:",
                      min = 1, max = 20, value = c(1,8)),
          sliderInput("neven", "Noevent:",
                      min = 0, max = 1, value = 0.9)
        )
      ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 br(),
                 conditionalPanel(
                   condition = "input.select == 1",
                   textOutput("values")
                 ),
                 conditionalPanel(
                   condition = "input.select == 2",
                   plotOutput("disPlot")
                 )),
        tabPanel("Tutorial")
      )
    )
  )
))
