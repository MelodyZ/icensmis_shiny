
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyBS)
library(icensmis)

shinyServer(function(input, output) {

#------------------------------------------------------------------------------------------------
    # Calculate Sample Size
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
    
    #---------------------------------------------------------------------------------------------------
    
    #Insert File
    data <- reactive({
      file <- input$file1
      if(is.null(file)){return()}
      read.csv(file = file$datapath, header = T, sep = input$sep)
    })
    
    datavar <- reactive({
      #if(is.null(data())){return()}
      Names <- names(data())
      return(Names)
    })
    
    output$data <- renderTable({
      if(is.null(data())){return()}
      head(data())
    })
    
    outputOptions(output, 'data', suspendWhenHidden=FALSE)
    
    output$Id <- renderUI({
      #df <- data()
      #vars <- datavar()
      if(is.null(data())){return()}
      selectInput("id", "Subject ID", datavar())
    })
    
    output$Tt <- renderUI({
      df <- data()
      vars <- datavar()
      vars2 <- subset(vars, vars != input$id)
      #vars <- names(df[, -which(names(df) == input$id)])
      if(is.null(df)){return()}
      selectInput("tt", "Test Time", vars2)
    })
    
    output$Res <- renderUI({
      df <- data()
      vars <- datavar()
      vars3 <- subset(vars, vars != input$id & vars != input$tt)
      #vars <- names(df[, -which(names(df) == input$id)])
      if(is.null(df)){return()}
      selectInput("res", "Result", vars3)
    })
    
    output$Cov <- renderUI({
      df <- data()
      vars <- datavar()
      vars4 <- subset(vars, vars != input$id & vars != input$tt & vars != input$res)
      if(is.null(df)){return()}
      selectInput("cov", "Covariate", vars4, selected = NULL, multiple = T)
    })
    
    ## Results
    covInput <- reactive({
      df <- data()
      if (is.null(input$cov)){return(NULL)}
      as.formula(paste0("~", paste0(input$cov, collapse = " + ")))
    })
    
    output$loglik <- renderText({
      df <- data()
      #cov <- as.name(paste(input$cov, collapse = " + "))
      fit1 <- icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity = input$spe,
                    formula = covInput()
                    )
      res1 <- round(fit1$loglik, 4)
      paste("Your Loglikelihood is:", res1)
    })
    
    output$coef <- renderTable({
      df <- data()
      fit2 <- icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity= input$spe,
                    formula = covInput())
      res2 <- fit2$coefficient
      res2 <- as.matrix(res2)
      res2
      
    }, caption = "Coefficient Table:",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    output$coefui <- renderUI({
      df <- data()
      fit2 <- icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity= input$spe,
                    formula = covInput())
      res2 <- fit2$coefficient
      if (is.na(res2)){return("There are no coefficient for your dataset.")}
      tableOutput("coef")
    })
    
    output$surv <- renderTable({
      df <- data()
      fit3 <- icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity= input$spe,
                    formula = covInput())
      res3 <- fit3$survival
      res3 <- as.matrix(res3)
      res3
    }, caption = "Survival Table:",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    output$beta <- renderTable({
      df <- data()
      fit4 <- icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity= input$spe,
                    formula = covInput())
      res4 <- as.matrix(fit4$beta.cov)
      res4 <- format(res4, nsmall = 4)}, caption = "Beta.cov Table:",
      caption.placement = getOption("xtable.caption.placement", "top"), 
      caption.width = getOption("xtable.caption.width", NULL))
    
    output$betaui <- renderUI({
      df <- data()
      fit4 <- icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity= input$spe,
                    formula = covInput())
      res4 <- fit4$coefficient
      if (is.na(res4)){return("There are no beta.cov for your dataset.")}
      tableOutput("beta")
    })

    
    output$n <- renderText({
      df <- data()
      fit5 <- icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity= input$spe,
                    formula = covInput())
      res5 <- fit5$nsub
      paste("Your Sample Size is: ", res5)
    })
    
  }) 

