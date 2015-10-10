
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyBS)
library(icensmis)

shinyServer(function(input, output, session) {

#------------------------------------------------------------------------------------------------
  # choose survivals or testtimes
  survInput <- reactive({
    if (input$surv_tt == 1){
      ### chosse survivals
      sort(as.numeric(strsplit(input$surv, split = ",")[[1]]), decreasing = T)
    } else{
      ### choose testtimes
      ttime <- sort(as.numeric(strsplit(input$ttime, split = ",")[[1]]))
      exp(log(0.9) * ttime/max(ttime))
    }
  })
   
   # Calculate Sample Size
    output$ssize <- renderTable({
      if (input$submt == 0){
        return()}else {
          m1 <- function(y){
            ssize <- isolate(icpower(HR = y,
                             sensitivity = input$sen,
                             specificity = input$spe,
                             survivals = survInput(),
                             rho = input$rho,
                             pmiss = input$pmis,
                             design = input$dsn,
                             negpred = input$negp,
                             power = input$pw))
            result_ss <- ssize$result[[1]]
            return(result_ss)
          } 
          
          hr <- isolate(sort(as.numeric(strsplit(input$HR, split = ",")[[1]])))
          result_n <- sapply(hr, m1)
          result_ss_1 <- round(input$rho * result_n)
          result_ss_2 <- result_n - result_ss_1
          
          table_n <- isolate(cbind(hr, result_n, result_ss_1, result_ss_2))
          colnames(table_n) <- c("HR", "N", "N1", "N2")
          rownames(table_n) <- 1:length(hr)
          table_n
        }
        
      #result_ss <- ssize$result[[1]]
      #result_ss_1 <- ssize$result[[2]]
      #result_ss_2 <- ssize$result[[3]]
      
      #paste0("The total Sample Size is:", as.character(result_n), ".", 
      #      "There are ", as.character(result_ss_1), "subjects in GROUP 1, ",
       #     "and", as.character(result_ss_2) ,"subjects in GROUP 2.")
      
    }, align = "lllll")
    
    # Calculate Power
    output$power <- renderTable({
      if (input$submt == 0){
        return()}else {
      m2 <- function(x, y){

            spower <- isolate(icpower(HR = y,
                              sensitivity = input$sen,
                              specificity = input$spe,
                              survivals = survInput(),
                              rho = input$rho,
                              pmiss = input$pmis,
                              design = input$dsn,
                              negpred = input$negp,
                              N = x))
            result_p <- spower$result[[4]]
            return(result_p)
          }
        
        ss <- isolate(sort(as.numeric(strsplit(input$n, split = ",")[[1]])))
        hr <- isolate(sort(as.numeric(strsplit(input$HR, split = ",")[[1]])))
        ss_hr <- expand.grid(ss, hr)
        result_pw <- mapply(x = ss_hr[,1], y = ss_hr[,2], m2)
        #result_p <- m2(ss_hr[,1], ss_hr[,2])
        table_p <- cbind(ss_hr, result_pw)
        colnames(table_p) <- c("N", "HR", "Power")
        rownames(table_p) <- 1:nrow(ss_hr)
        table_p
        }
        
    }, align = "llll")
    
    output$matrix1 <- renderTable({
      if (input$submt == 0){
        return()
      }else {
        ## Choose survivals or ttimes
        if (input$surv_tt == 1){
          df <- isolate(as.data.frame(matrix(list(input$pw, input$HR, input$sen, input$spe, input$surv, 
                                                  input$rho, input$pmis, input$dsn, input$negp),
                                             nrow = 9)))
          rownames(df) <- c('Power', 'Hazard Ratio', 'Sensitivity', 'Specificity',
                            'Survivals', "Rho", "Pmiss", "Design", "Negpred")
          colnames(df) <- "Values"
          df}else {
            df <- isolate(as.data.frame(matrix(list(input$pw, input$HR, input$sen, input$spe, input$ttime, 
                                                    input$rho, input$pmis, input$dsn, input$negp),
                                               nrow = 9)))
            rownames(df) <- c('Power', 'Hazard Ratio', 'Sensitivity', 'Specificity',
                              'Testtimes', "Rho", "Pmiss", "Design", "Negpred")
            colnames(df) <- "Values"
            df
          }
        }
    }, align = "ll")
    
    output$matrix2 <- renderTable({
      if (input$submt == 0){
        return()
      }else {
          ### chosse survivals or ttimes
        if (input$surv_tt == 1){
          df <- isolate(as.data.frame(matrix(list(input$n, input$HR, input$sen, input$spe, input$surv, 
                                                  input$rho, input$pmis, input$dsn, input$negp),
                                             nrow = 9)))
          rownames(df) <- c('Sample Size', 'Hazard Ratio', 'Sensitivity', 'Specificity',
                            'Survivals', "Rho", "Pmiss", "Design", "Negpred")
          colnames(df) <- "Values"
          df}else {
            df <- isolate(as.data.frame(matrix(list(input$n, input$HR, input$sen, input$spe, input$ttime, 
                                                    input$rho, input$pmis, input$dsn, input$negp),
                                               nrow = 9)))
            rownames(df) <- c('Sample Size', 'Hazard Ratio', 'Sensitivity', 'Specificity',
                              'Testtimes', "Rho", "Pmiss", "Design", "Negpred")
            colnames(df) <- "Values"
            df
          }      
          }
    }, align = "ll")
    
####################################################################
    
    # Calculate Sample Size
    down_matrix1 <- reactive({
      if (input$submt == 0){
        return()
      }else {
        ## Choose survivals or ttimes
        hr <- isolate(sort(as.numeric(strsplit(input$HR, split = ",")[[1]])))

        if (input$surv_tt == 1){
          surv <- isolate(sort(as.numeric(strsplit(input$surv, split = ",")[[1]]), decreasing = T))
          #hr <- c(1.25, 10, 25, 40.5, 55)
          #surv <- c(0.99, 0.9, 0.88, 0.76, 0.75)
          df <- isolate(as.data.frame(t(sapply(hr, function(x) {c(input$pw, x, input$sen, input$spe, surv, 
                                                  input$rho, input$pmis, input$dsn, input$negp)}))))
          #df <- as.data.frame(t(sapply(hr, function(x) {c(0.9, x, 0.99, 0.55, surv, 0.5, 0, "M", 1)})))
          colnames(df) <- c('Power', 'Hazard Ratio', 'Sensitivity', 'Specificity',
                            sapply(order(surv, decreasing = T), function(x){paste0('Survival ', x)}), 
                            "Rho", "Pmiss", "Design", "Negpred")
          rownames(df) <- 1:length(hr)
          #df
          }else {
            ttime <- isolate(sort(as.numeric(strsplit(input$ttime, split = ",")[[1]])))
            surv <- exp(log(0.9) * ttime/max(ttime))
            #hr <- c(1.25, 10, 25, 40.5, 55)
            #surv <- c(0.99, 0.9, 0.88, 0.76, 0.75)
            df <- isolate(as.data.frame(t(sapply(hr, function(x) {c(input$pw, x, input$sen, input$spe, ttime, 
                                                                    input$rho, input$pmis, input$dsn, input$negp)}))))
            #df <- as.data.frame(t(sapply(hr, function(x) {c(0.9, x, 0.99, 0.55, surv, 0.5, 0, "M", 1)})))
            colnames(df) <- c('Power', 'Hazard Ratio', 'Sensitivity', 'Specificity',
                              sapply(order(ttime), function(x){paste0('Testtime ', x)}), 
                              "Rho", "Pmiss", "Design", "Negpred")
            rownames(df) <- 1:length(hr)
            #df
          }
            ## Calculation
            m1 <- function(y){
              ssize <- isolate(icpower(HR = y,
                                       sensitivity = input$sen,
                                       specificity = input$spe,
                                       survivals = survInput(),
                                       rho = input$rho,
                                       pmiss = input$pmis,
                                       design = input$dsn,
                                       negpred = input$negp,
                                       power = input$pw))
              result_ss <- ssize$result[[1]]
              return(result_ss)
            } 
            
            #hr <- isolate(as.numeric(strsplit(input$HR, split = ",")[[1]]))
            result_n <- sapply(hr, m1)
            result_ss_1 <- round(input$rho * result_n)
            result_ss_2 <- result_n - result_ss_1
            
            table_n <- isolate(cbind(result_n, result_ss_1, result_ss_2))
            colnames(table_n) <- c("N", "N1", "N2")
            rownames(table_n) <- 1:length(hr)
            cbind(df, table_n)
      }
    })
 
    #==================================   
    # Calculate Power
    down_matrix2 <- reactive({
      if (input$submt == 0){
        return()
      }else {
        ss <- isolate(sort(as.numeric(strsplit(input$n, split = ",")[[1]])))
        hr <- isolate(sort(as.numeric(strsplit(input$HR, split = ",")[[1]])))
        #ss <- c(100, 200, 300, 500)
        #hr <- c(1.25, 5.5, 10, 20)
        ss_hr <- expand.grid(ss, hr)
        
        ## Choose survivals or ttimes
        if (input$surv_tt == 1){
          surv <- isolate(sort(as.numeric(strsplit(input$surv, split = ",")[[1]]), decreasing = T))
          #surv <- c(0.99, 0.9, 0.88, 0.76, 0.75)
          
          df <- isolate(as.data.frame(t(mapply(x = ss_hr[,1], y = ss_hr[,2], function(x, y) {c( x, y, input$sen, input$spe, surv, 
                                                                  input$rho, input$pmis, input$dsn, input$negp)}))))
          #df <- as.data.frame(t(mapply(x = ss_hr[,1], y = ss_hr[,2], function(x, y) {c(x, y, 0.99, 0.55, surv, 0.5, 0, "M", 1)})))
          colnames(df) <- c('Sample Size', 'Hazard Ratio', 'Sensitivity', 'Specificity',
                            sapply(order(surv, decreasing = T), function(x){paste0('Survival ', x)}), 
                            "Rho", "Pmiss", "Design", "Negpred")
          rownames(df) <- 1:nrow(ss_hr)
          #df
          }else {
            ttime <- isolate(sort(as.numeric(strsplit(input$ttime, split = ",")[[1]])))
            surv <- exp(log(0.9) * ttime/max(ttime))
            
            df <- isolate(as.data.frame(t(mapply(x = ss_hr[,1], y = ss_hr[,2], function(x, y) {c( x, y, input$sen, input$spe, ttime, 
                                                                                                  input$rho, input$pmis, input$dsn, input$negp)}))))
            #df <- as.data.frame(t(mapply(x = ss_hr[,1], y = ss_hr[,2], function(x, y) {c(x, y, 0.99, 0.55, surv, 0.5, 0, "M", 1)})))
            colnames(df) <- c('Sample Size', 'Hazard Ratio', 'Sensitivity', 'Specificity',
                              sapply(order(ttime), function(x){paste0('Testtime ', x)}), 
                              "Rho", "Pmiss", "Design", "Negpred")
            rownames(df) <- 1:nrow(ss_hr)
            #df
          }
        m2 <- function(x, y){
          
          spower <- isolate(icpower(HR = y,
                                    sensitivity = input$sen,
                                    specificity = input$spe,
                                    survivals = survInput(),
                                    rho = input$rho,
                                    pmiss = input$pmis,
                                    design = input$dsn,
                                    negpred = input$negp,
                                    N = x))
          result_p <- spower$result[[4]]
          return(result_p)
        }
        
        result_pw <- mapply(x = ss_hr[,1], y = ss_hr[,2], m2)
        #result_p <- m2(ss_hr[,1], ss_hr[,2])
        table_p <- cbind(df, result_pw)
        colnames(table_p) <- c(names(df), "Power")
        table_p
      }
    })
      
    
    # Download data
    
    output$downloadData <- downloadHandler(
      filename = function() {paste(input$names," ", Sys.time(), '.csv', sep='')},
      content = function(file) {
        if (input$select == 1){
          write.csv(down_matrix1(), file)
        }else {
          write.csv(down_matrix2(), file)
          }
      }
    )
####################################################################    
    #---------------------------------------------------------------------------------------------------
    
    #Insert File
    data <- reactive({
      file <- input$file1
      if (input$upload == 0)
        return()
      if(is.null(file))
        {return()}
      isolate(read.csv(file = file$datapath, header = T, sep = input$sep))
    })
    
    datavar <- reactive({
      #if(is.null(data())){return()}
      Names <- names(data())
      return(Names)
    })
    
    ### Open Panel 1
    observeEvent(input$upload, ({
      updateCollapse(session, "Panels", open = "Review your dataset", close = "Results")
    }))
    
    observeEvent(input$file1, ({
      updateCollapse(session, "Panels", close = c("Results", "Review your dataset"))
    }))
    
    output$data <- renderTable({
      #if (input$upload == 0)
      #  return()
      if(is.null(data())){return()}
      #isolate(
        head(data())
      #  )
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
    
    #observeEvent(input$upload, ({
     # updateSelectInput(session, "id")
     # updateSelectInput(session, "tt")
     # updateSelectInput(session, "res")
     # updateSelectInput(session, "cov")
    #}))
    
    ## Results
    
    ### Open Panel 2
    observeEvent(input$submt2, ({
      updateCollapse(session, "Panels", open = "Results")
    }))
    
    observeEvent(input$file1, ({
      updateCollapse(session, "Panels")
    }))
    
    covInput <- reactive({
      df <- data()
      if (is.null(input$cov)){return(NULL)}
      as.formula(paste0("~", paste0(input$cov, collapse = " + ")))
    })
    
    output$loglik <- renderText({
      if (input$submt2 == 0)
        return()
      df <- data()
      #cov <- as.name(paste(input$cov, collapse = " + "))
      fit1 <- isolate(icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity = input$spe,
                    formula = covInput()
                    ))
      res1 <- round(fit1$loglik, 4)
      paste("Your Loglikelihood is:", res1)
    })
    
    output$coef <- renderTable({
      if (input$submt2 == 0)
        return()
      df <- data()
      fit2 <- isolate(icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity= input$spe,
                    formula = covInput()))
      res2 <- fit2$coefficient
      res2 <- as.matrix(res2)
      res2
      
    }, caption = "Coefficient Table:",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    output$coefui <- renderUI({
      if (input$submt2 == 0)
        return()
      df <- data()
      fit2 <- isolate(icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity= input$spe,
                    formula = covInput()))
      res2 <- fit2$coefficient
      if (is.na(res2)){return("There are no coefficient for your dataset.")}
      tableOutput("coef")
    })
    
    output$surv <- renderTable({
      if (input$submt2 == 0)
        return()
      df <- data()
      fit3 <- isolate(icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity= input$spe,
                    formula = covInput()))
      res3 <- fit3$survival
      res3 <- as.matrix(res3)
      res3
    }, caption = "Survival Table:",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    output$beta <- renderTable({
      if (input$submt2 == 0)
        return()
      df <- data()
      fit4 <- isolate(icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity= input$spe,
                    formula = covInput()))
      res4 <- as.matrix(fit4$beta.cov)
      res4 <- format(res4, nsmall = 4)}, caption = "Beta.cov Table:",
      caption.placement = getOption("xtable.caption.placement", "top"), 
      caption.width = getOption("xtable.caption.width", NULL))
    
    output$betaui <- renderUI({
      if (input$submt2 == 0)
        return()
      df <- data()
      fit4 <- isolate(icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity= input$spe,
                    formula = covInput()))
      res4 <- fit4$coefficient
      if (is.na(res4)){return("There are no beta.cov for your dataset.")}
      tableOutput("beta")
    })

    
    output$n <- renderText({
      if (input$submt2 == 0)
        return()
      df <- data()
      fit5 <- isolate(icmis(subject = df[,input$id],
                    testtime = df[,input$tt],
                    data = data(),
                    result = df[,input$res],
                    sensitivity = input$sen,
                    specificity= input$spe,
                    formula = covInput()))
      res5 <- fit5$nsub
      paste("Your Sample Size is: ", res5)
    })
    
  }) 


