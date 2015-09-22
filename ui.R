
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
                                   "Figure 1. Comparison of power versus total sample size (N) for different values of the (sensitivity, specificity) of the diagnostic test" = 2,
                                   "Figure 2. Effects of hazard ratio, event rate, and number of tests on sample size for different (sensitivity, specificity)" = 3,
                                   "Figure 3. Relative efficiency of test schedule 1 and schedule 2" = 4)),
        
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
                      min = 1, max = 20, 
                      step = 1, value = c(1,8)),
          sliderInput("neven", "Noevent:",
                      min = 0, max = 1, value = 0.9)
        ),
        
        conditionalPanel(
          condition = "input.select == 3",
          radioButtons("effect", "Effect of",
                       choices = list("Hazard Ratio" = 1,
                                      "Cumulative Incidence" = 2,
                                      "Number of Tests" = 3)),
          conditionalPanel(
            condition = "input.effect == 1",
            sliderInput("HRange", "Hazard Ratio Range:",
                        min = 1, max = 3, step = 0.05, 
                        value = c(1.5, 2.5))
            ),
          conditionalPanel(
            condition = "input.effect == 2",
            sliderInput("nevent", "Noevent:",
                        min = 0, max = 1, step = 0.05, 
                        value = c(0.2, 0.9))
          ),
          conditionalPanel(
            condition = "input.effect == 3",
            sliderInput("ntest", "Notest:",
                        min = 2, max = 20, step = 2, 
                        value = c(2, 10))
          ),
          
          sliderInput("sen3", "Sensitivity:",
                      min = 0, max = 1, value = 0.55),
          sliderInput("spe3", "Specificity:",
                      min = 0, max = 1, value = 0.99)
        ),
        
        conditionalPanel(
          condition = "input.select == 4",
          helpText("* Schedules: a list of test time indices for each schedule."),
          helpText("* Ratios: ratio of subjects assigned to each schedule."),
          br(),
          radioButtons("effect4", "Effect of",
                       choices = list("Hazard Ratio" = 1,
                                      "Cumulative Incidence" = 2,
                                      "Number of Tests" = 3)),
          conditionalPanel(
            condition = "input.effect4 == 1",
            sliderInput("HRange", "Hazard Ratio Range:",
                        min = 1, max = 3, step = 0.05, 
                        value = c(1.5, 2.5))
          ),
          conditionalPanel(
            condition = "input.effect4 == 2",
            sliderInput("nevent", "Noevent:",
                        min = 0, max = 1, step = 0.05, 
                        value = c(0.2, 0.9))
          ),
          conditionalPanel(
            condition = "input.effect4 == 3",
            sliderInput("ntest4", "Notest:",
                        min = 2, max = 20, step = 2, 
                        value = c(4, 16))
          ),
          
          sliderInput("sen4", "Sensitivity:",
                      min = 0, max = 1, value = 0.55),
          sliderInput("spe4", "Specificity:",
                      min = 0, max = 1, value = 0.99)
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
                   plotOutput("disPlot1")
                 ),
                 
                 conditionalPanel(
                   condition = "input.select == 3",
                   
                   conditionalPanel(
                     condition = "input.effect == 1",
                     plotOutput("disPlot2_1")
                     ),
                   conditionalPanel(
                     condition = "input.effect == 2",
                     plotOutput("disPlot2_2")
                     ),
                   conditionalPanel(
                     condition = "input.effect == 3",
                     plotOutput("disPlot2_3")
                   )
                   ),
                 
                 conditionalPanel(
                   condition = "input.select == 4",
                   
                   conditionalPanel(
                     condition = "input.effect4 == 1",
                     plotOutput("disPlot3_1")
                   ),
                   conditionalPanel(
                     condition = "input.effect4 == 2",
                     plotOutput("disPlot3_2")
                   ),
                   conditionalPanel(
                     condition = "input.effect4 == 3",
                     plotOutput("disPlot3_3")
                   )
                 )
                 ),
        tabPanel("Tutorial")
      )
      )
    )
))
