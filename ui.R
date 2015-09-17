
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Sample Size Estimation"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("HR",
                  "Hazard ratio:",
                  value = 1.25),
      sliderInput("sen", "Sensitivity:",
                  min = 0, max = 1, value = 0.55),
      sliderInput("spe", "Specificity:",
                  min = 0, max = 1, value = 0.99),
      sliderInput("power", "Power:",
                  min = 0, max = 1, value = 0.9),

      sliderInput("tim", "Times:",
                  min = 1, max = 20, value = c(1,8)),
      sliderInput("neven", "Noevent:",
                  min = 0, max = 1, value = 0.9)
      ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("values"),
      plotOutput("disPlot")
    )
  )
))
