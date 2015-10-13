
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyBS)

shinyUI(navbarPage(" ",
                   tabPanel("Study Design",
                            tags$head(
                              tags$style(HTML("
                              @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
                              h1 {
                              font-size: 28px;
                              font-weight: 500;
                              line-height: 1.5;
                              }

                              "))
                            ),
                            headerPanel("Study design for time to event outcomes in the presence of error-prone diagnostic tests or self-reports"),
                            br(),
                            br(),
                            # Some custom CSS
                            tags$head(
                              tags$style(HTML("
                                              /* Smaller font for preformatted text */
                                              pre, table.table {
                                              font-size: smaller;
                                              }
                                              
                                              body {
                                              min-height: 2000px;
                                              }
                                              
                                              .option-group {
                                              border: 1px solid #ccc;
                                              border-radius: 6px;
                                              padding: 0px 5px;
                                              margin: 5px -10px;
                                              background-color: #f5f5f5;
                                              }
                                              
                                              .option-header {
                                              font-weight: bold;
                                              color: #808080;
                                              text-transform: uppercase;
                                              margin-bottom: 5px;
                                              }
                                              "))
                                              ),

                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                div(class = "option-group",
                                    div(class = "option-header", "Choose a Calculation"),
                                    radioButtons("select", " ",
                                                 choices = list("Sample Size" = 1,
                                                                "Power" = 2)),
                                    bsPopover("select", 
                                              "The <b>power</b> (or sensitivity) is the probability of accepting the alternative hypothesis when it is true.",
                                              "i.e. You choose to calculate Sample Size, then you need to set a value to Power in the following, and vice versa.",
                                              "right", options = list(container = "body"))
                                    ),
                                br(),
                                div(class = "option-group",
                                    div(class = "option-header", "Values of Argument"),
                                    br(),
                                    conditionalPanel(
                                      condition = "input.select == 2",
                                      textInput("n", "Sample Size:",
                                                value = "100, 1000, 10000"),
                                      bsPopover("n", "A vector of sample sizes to calculate corresponding powers.",
                                                "Type in any postive integers, i.e. 25673.",
                                                "right", options = list(container = "body"))),
                                    conditionalPanel(
                                      condition = "input.select == 1",
                                      numericInput("pw", "Power:",
                                                   min = 0, max = 1, 
                                                   step = 0.0001, value = 0.9),
                                      bsPopover("pw", "A vector of powers to calculate corresponding sample sizes. Higher values indicate a greater likelihood of detecting an effect.",
                                                "Type in any postive values between 0 and 1, i.e. 0.985.",
                                                "right", options = list(container = "body"))),
                                    
                                    textInput("HR", "Hazard ratio:",
                                              value = "1.25, 10, 125"),
                                    bsPopover("HR", "<b>Hazard ratio</b> (HR) is the ratio of the hazard rates corresponding to the conditions described by two levels of an explanatory variable.",
                                              "Type in any non-zero positive values, i.e. 10.",
                                              "right", options = list(container = "body")),
                                    sliderInput("sen", "Sensitivity:",
                                                min = 0, max = 1, 
                                                step = 0.01, value = 0.55),
                                    bsPopover("sen", "Sensitivity (also called the true positive rate) measures the proportion of positives that are correctly identified.",
                                              "It ranges from 0 to 1. Move the slider to set a value.",
                                              "right", options = list(container = "body")),
                                    sliderInput("spe", "Specificity:",
                                                min = 0, max = 1, 
                                                step = 0.01, value = 0.99),
                                    bsPopover("spe", "Specificity (also called the true negative rate) measures the proportion of negatives that are correctly identified.",
                                              "It ranges from 0 to 1. Move the slider to set a value.",
                                              "right", options = list(container = "body")),
                                    selectInput("surv_tt", "Choose covariates:",
                                                choices = list("Type in Survivals" = 1,
                                                               "Type in Cumulative Incidence and Test Times" = 2)),
                                    conditionalPanel(
                                      condition = "input.surv_tt == 1",
                                      textInput("surv", "Survivals:", 
                                                value = "0.998, 0.978, 0.898, 0.798"),
                                      bsPopover("surv", "A vector of survival function at each test time for baseline(reference) group. Its length determines the number of tests.",
                                                "Type in a decreasing sequence of values between 0 and 1, with a common between each two, i.e. 0.9998, 0.976, 0.903, 0.899, 0.877.",
                                                "right", options = list(container = "body"))
                                      ),
                                    conditionalPanel(
                                      condition = "input.surv_tt == 2",
                                      helpText("We assume survival function follows Exponential Distribution."),
                                      sliderInput("ci", "Cumulative Incidence", 
                                                  min = 0, max = 1, 
                                                  step = 0.01, value = 0.1),
                                      bsPopover("ci", "Probability that a particular event, such as occurrence of a particular disease, has occurred before a given time.",
                                                "It ranges from 0 to 1. Move the slider to set a value.",
                                                "right", options = list(container = "body")),
                                      textInput("ttime", "Test Times:", 
                                                value = "1, 3, 4, 7, 11, 16, 22"),
                                      bsPopover("ttime", "variable in data for test time. Assume all test times are non-negative.",
                                                "Type in a sequence of values, with a common between each two, i.e. 1, 3, 4, 7, 11, 16, 22.",
                                                "right", options = list(container = "body"))
                                    ),
                                    
                                    sliderInput("rho", "Rho:",
                                                min = 0, max = 1, 
                                                step = 0.1, value = 0.5),
                                    bsPopover("rho", "Proportion of subjects in baseline(reference) group.",
                                              "It ranges from 0 to 1. Move the slider to set a value.", 
                                              "right", options = list(container = "body")),
                                    sliderInput("pmis", "Pmiss:",
                                                 min = 0, max = 1, 
                                                 step = 0.01, value = 0),
                                    bsPopover("pmis", "A value or a vector (must have same length as survivals) of the probabilities of each test being randomly missing at each test time. If pmiss is a single value, then each test is assumed to have an identical probability of missingness.",
                                              "It ranges from 0 to 1. Move the slider to set a value.",
                                              "right", options = list(container = "body")),
                                    radioButtons("dsn", "Design:", inline = T,
                                                 choices = list("MCAR", "NTFP"),
                                                 selected = "MCAR"),
                                    bsPopover("dsn", "The <b>MCAR</b> setting assumes that each test is subject to a constant, independent probability of missingness. <p>The <b>NTFP</b> mechanism includes two types of missingness - (1) incorporates a constant, independent, probability of missing for each test prior to the first positive test result; and (2) all test results after first positive are missing.</p>",
                                              "Chose one of missing mechanisms you want to study.", 
                                              "right", options = list(container = "body")),
                                    numericInput("negp", "Negpred:",
                                                 min = 0, max = 1, 
                                                 step = 0.01, value = 1),
                                    bsPopover("negp", "Baseline negative predictive value, i.e. the probability of being truely disease free for those who were tested (reported) as disease free at baseline. If baseline screening test is perfect, then negpred = 1.",
                                              "It ranges from 0 to 1. Please type in a value.",
                                              "right", options = list(container = "body"))
                                    ),
                                br(),
                                actionButton("submt", "Analysis"),
                                helpText('Click the "Analysis" to display your result.'),
                                br(),
                                br(),
                                bootstrapPage(
                                  div(style="display:inline-block", textInput("names", "Dataset Name:", value = "results")),
                                  div(style="display:inline-block", downloadButton('downloadData','Download'))
                                  ),
                                helpText("If you want to download the results into a csv. file, please type in your file name, and Click 'Download' button.")
                              ),
                              mainPanel(
                                h4("Your Input is:"),
                                conditionalPanel(condition = "input.select == 1",
                                                 tableOutput("matrix1")),
                                conditionalPanel(condition = "input.select == 2",
                                                 tableOutput("matrix2")),
                                tags$style(HTML("#matrix1 table{ 
                                  margin: auto;
                                   }")),
                                tags$style(HTML("#matrix2 table{ 
                                  margin: auto;
                                   }")),
                                hr(),
                                h4("Your Result is:"),
                                conditionalPanel(condition = "input.select == 1",
                                                 tableOutput("ssize")),
                                conditionalPanel(condition = "input.select == 2",
                                                 tableOutput("power")),
                                tags$style(HTML("#ssize table{ 
                                  margin: auto;
                                   }")),
                                tags$style(HTML("#power table{ 
                                  margin: auto;
                                   }"))
                                )
                              )
                            ),
                   tabPanel("Data Analysis",
                            
                            tags$head(
                              tags$style(HTML("
                                              /* Smaller font for preformatted text */
                                              pre, table.table {
                                              font-size: smaller;
                                              }
                                              
                                              body {
                                              min-height: 2000px;
                                              }
                                              
                                              .option-group2 {
                                              border: 1px solid #ccc;
                                              border-radius: 6px;
                                              padding: 0px 5px;
                                              margin: 5px -10px;
                                              background-color: #f5f5f5;
                                              }
                                              
                                              .option-header {
                                              font-weight: bold;
                                              color: #808080;
                                              text-transform: uppercase;
                                              margin-bottom: 5px;
                                              }
                                              "))
                                              ),
                            
                            sidebarLayout(
                              sidebarPanel(
                                
                                div(class = "option-group2",
                                    div(class = "option-header", "Data Input"),
                                #Insert File
                                helpText("Please chose a .csv file"),
                                fileInput('file1', '',
                                          accept=c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv')),
                                textInput("sep", "Separator Character:", value = ","),
                                br(),
                                actionButton("upload", "Upload"),
                                br(),
                                helpText('Click the "Upload" to display your subtable.'),
                                bsPopover("sep", title = "", content = "If no separator character, please leave it blank",
                                          placement = "right", options = list(container = "body"))),
                                br(),
                                
                                div(class = "option-group2",
                                    div(class = "option-header", "Predictors Input"),
                                    br(),
                                conditionalPanel("output.data",
                                                 uiOutput("Id"),
                                                 bsPopover("Id", title = "", content = "Variable in data for subject id.",
                                                           placement = "right", options = list(container = "body")),
                                                 uiOutput("Tt"),
                                                 bsPopover("Tt", title = "", content = "Variable in data for test time. Assume all test times are non-negative. testtime = 0 refers to baseline visit (only used/needed if the model is time varying covarites).",
                                                           placement = "right", options = list(container = "body")),
                                                 uiOutput("Res"),
                                                 bsPopover("Res", title = "", content = "Variable in data for test result.",
                                                           placement = "right", options = list(container = "body")),
                                                 uiOutput("Cov"),
                                                 bsPopover("Cov", title = "Variable in data that is possibly predictive of the outcome.", 
                                                           content = "If no covariates, please leave it blank.", placement = "right", options = list(container = "body")),
                                                 sliderInput("sen", "Sensitivity",
                                                             min = 0, max = 1, 
                                                             step = 0.01, value = 0.7),
                                                 sliderInput("spe", "Specificity",
                                                             min = 0, max = 1, 
                                                             step = 0.01, value = 0.98),
                                                 br(),
                                                 #HTML("This button will open Results using <code>updateCollapse</code>."),
                                                 actionButton("submt2", "Submit"),
                                                 br(),
                                                 helpText('Click the "Submit" to display your result.')
                                                 ))
                              ),
                              mainPanel(
                                bsCollapse(id = "Panels",
                                           bsCollapsePanel("Review your dataset", 
                                                           tableOutput('data'),
                                                           tags$style(HTML("#data table{ 
                                                                            margin: auto;
                                                                           }"))),
                                           bsCollapsePanel("Results",
                                                           textOutput("loglik"),
                                                           br(),
                                                           uiOutput("coefui"),
                                                           br(),
                                                           tableOutput("surv"),
                                                           br(),
                                                           uiOutput('betaui'),
                                                           br(),
                                                           textOutput("n")
                                                           ))
                              )
                            )
                   ),
                   tabPanel("Background",
                            mainPanel(
                              h4("Abstract"),
                              br(),
                              "The onset of several silent, chronic diseases such as diabetes can be de- tected only through diagnostic tests. Due to cost considerations, self-reported outcomes are routinely collected in lieu of expensive diagnostic tests in large- scale prospective investigations such as the Women’s Health Initiative. How- ever, self-reported outcomes are subject to imperfect sensitivity and speci- ficity. Using a semiparametric likelihood-based approach, we present time to event models to estimate the association of one or more covariates with a error-prone, self-reported outcome. We present simulation studies to assess the effect of error in self-reported outcomes with regard to bias in the estima- tion of the regression parameter of interest. We apply the proposed methods to prospective data from 152,830 women enrolled in the Women’s Health Initiative to evaluate the effect of statin use with the risk of incident diabetes mellitus among postmenopausal women. The current analysis is based on follow-up through 2010, with a median duration of follow-up of 12.1 years. The methods proposed in this paper are readily implemented using our freely available R software package icensmis, which is available at the Comprehensive R Archive Network (CRAN) website.",
                              br(),
                              br(),
                              h4("Links"),
                              br(),
                              p("[1]", a("SEMIPARAMETRIC TIME TO EVENT MODELS IN THE PRESENCE OF ERROR-PRONE, SELF-REPORTED OUTCOMES—WITH APPLICATION TO THE WOMEN’S HEALTH INITIATIVE1",     
                                href="http://arxiv.org/pdf/1509.04080.pdf")),
                              br(),
                              br(),
                              h4("Tutorials")
                              
                            ))
))