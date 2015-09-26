
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(navbarPage(" ",
                   tabPanel("Study Design",
                            titlePanel("Study design for time to event outcomes in the presence of error-prone diagnostic tests or self-reports"),
                            br(),
                            br(),
                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                radioButtons("select", h4("Choose a Calculation"),
                                             choices = list("Sample Size" = 1,
                                                            "Power" = 2)),
                                br(),
                                br(),
                                
                                conditionalPanel(
                                  condition = "input.select == 2",
                                  numericInput("n", "Sample Size:",
                                               min = 1, value = 10000)),
                                conditionalPanel(
                                  condition = "input.select == 1",
                                  numericInput("pw", "Power:",
                                               min = 0, max = 1, 
                                               step = 0.0001, value = 0.9)),
                                
                                numericInput("HR", "Hazard ratio:",
                                             step = 0.01, value = 1.25),
                                sliderInput("sen", "Sensitivity:",
                                            min = 0, max = 1, 
                                            step = 0.01, value = 0.55),
                                sliderInput("spe", "Specificity:",
                                            min = 0, max = 1, 
                                            step = 0.01, value = 0.99),
                                textInput("surv", "Survival:", 
                                          value = "0.998, 0.978, 0.898, 0.798"),
                                sliderInput("rho", "Rho:",
                                            min = 0, max = 1, 
                                            step = 0.1, value = 0.5),
                                numericInput("pmis", "Pmiss:",
                                             min = 0, max = 1, 
                                             step = 0.01, value = 0),
                                radioButtons("dsn", "Design:", inline = T,
                                             choices = list("MCAR", "NTFP"),
                                             selected = "MCAR"),
                                numericInput("negp", "Negpred:",
                                             min = 0, max = 1, 
                                             step = 0.01, value = 1),
                                br(),
                                br(),
                                actionButton("submt", "Submit"),
                                br(),
                                helpText('Click the "Submit" to display your result.')
                              ),
                              mainPanel(
                                conditionalPanel(condition = "input.select == 1",
                                                 textOutput("ssize")),
                                conditionalPanel(condition = "input.select == 2",
                                                 textOutput("power"))
                              )
                            )
                   ),
                   tabPanel("Data Analysis",
                            sidebarLayout(
                              sidebarPanel(
                                
                                #Insert File
                                fileInput('file1', 'Choose CSV File',
                                          accept=c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv'))
                              ),
                              mainPanel(
                                tableOutput('contents')
                              )
                            )
                   ),
                   tabPanel("Background")
))