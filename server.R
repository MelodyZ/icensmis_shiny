
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyBS)
library(icensmis)

shinyServer(function(input, output, session) {
  
  #Insert File
  output$contents <- renderTable( {
    
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
      result_ss_1 <- ssize$result[[2]]
      result_ss_2 <- ssize$result[[3]]
      
      paste("The total Sample Size is:", as.character(result_ss), ".", 
            "There are ", as.character(result_ss_1), "subjects in GROUP 1, ",
            "and", as.character(result_ss_2) ,"subjects in GROUP 2.")
      
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
      
      paste("Power:", as.character(round(result_p,2)), ".")
      
    })
    
    output$matrix1 <- renderTable({
      if (input$submt == 0)
        return()
      
      df <- isolate(as.data.frame(matrix(list(input$pw, input$HR, input$sen, input$spe, input$surv, 
                    input$rho, input$pmis, input$dsn, input$negp),
                    nrow = 9)))
      rownames(df) <- c('Power', 'Hazard Ratio', 'Sensitivity', 'Specivility',
                       "Survivals", "Rho", "Pmiss", "Design", "Negpred")
      colnames(df) <- "Values"
      df
    }, align = "ll")
    
    output$matrix2 <- renderTable({
      if (input$submt == 0)
        return()
      
      df <- isolate(as.data.frame(matrix(list(input$n, input$HR, input$sen, input$spe, input$surv, 
                                              input$rho, input$pmis, input$dsn, input$negp),
                                         nrow = 9)))
      rownames(df) <- c('Sample Size', 'Hazard Ratio', 'Sensitivity', 'Specivility',
                        "Survivals", "Rho", "Pmiss", "Design", "Negpred")
      colnames(df) <- "Values"
      df
    }, align = "ll")
  }) 
