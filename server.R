
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(icensmis)

shinyServer(function(input, output, session) {
  
  #Insert File
  output$contents <- renderTable(input$submt, {
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })

    ## Calculate Sample Size
    output$ssize <- renderText({
      if (input$submt == 0)
        return()
      
      ssize <- isolate(icpower(HR = input$HR,
                               sensitivity = input$sen,
                               specificity = input$spe,
                               survivals = as.numeric(strsplit(input$surv, split = ",")[[1]]),
                               rho = input$rho,
                               pmiss = input$pmis,
                               design = input$dsn,
                               negpred = input$negp,
                               power = input$pw))
      result_ss <- ssize$result[[1]]
      
      paste("Your sample size is:", as.character(result_ss), ".")
      
    })
    
    # Calculate Power
    output$power <- renderText({
      if (input$submt == 0)
        return()
      
      spower <- isolate(icpower(HR = input$HR,
                                sensitivity = input$sen,
                                specificity = input$spe,
                                survivals = as.numeric(strsplit(input$surv, split = ",")[[1]]),
                                rho = input$rho,
                                pmiss = input$pmis,
                                design = input$dsn,
                                negpred = input$negp, 
                                N = input$n))
      result_p <- spower$result[[4]]
      
      paste("Your power is:", as.character(round(result_p,2)), ".")
      
    })
    
  }) 
