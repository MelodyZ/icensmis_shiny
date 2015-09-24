
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(icensmis)

shinyServer(function(input, output) {
  
  #Insert File
  output$contents <- renderTable({
    
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
  
  # Sample Size
  output$values <- renderText({
    
    # Calculate Power and Sample Size
    surv <- exp(log(0.9) * (1:8)/8)
    ssize <- icpower(HR = as.numeric(input$HR), 
                     sensitivity = input$sen, 
                     specificity = input$spe, 
                     survivals = surv, 
                     power = input$power)
    result_ss <- ssize$result[[1]]
    
    paste("Your sample size is:", as.character(result_ss), ".")

  })
  
  #Figure 1
  Nlist <- seq(100, 15000, by = 100)
  
  output$disPlot1 <- renderPlot({
    par(mfrow = c(2,2))
    times <- seq(input$tim[1], input$tim[2], by = 1)
    lambda.base <- -log(input$neven)/max(times)
    survivals <- 1 - pexp(times, lambda.base)
    plot(0, 0, type = "n", xlab = "N", ylab = "Power", xlim = c(0, max(Nlist)), 
         cex.lab = 1.4, cex.main = 1.5, ylim = c(0, 1))
    
    pow <- icpower(HR = 1.25, 
                   sensitivity = input$sen2, 
                   specificity = input$spe2, 
                   survivals, 
                   Nlist)$result
    lines(pow$N, pow$power, lwd = 2)
    legend("topleft", cex = 1.25, legend = paste("(", input$sen2, ", ", input$spe2, ")"), lwd = 1.5)
    
    ## 3.5 Effect of using an imperfect diagnostic procedure at study entry
      negpred <- seq(1, 0.9, -0.01)
      survivals <- exp(log(0.9) * (1:8)/8)
      ssize <- sapply(negpred, function(x) icpower(2, input$sen2, input$spe2, survivals, power = 0.9, negpred = x)$result$N)
      mard <- par("mar") ## margin size
      mard[3] <- mard[3]/2
      par(mar = mard)
      plot(1 - negpred, ssize, type = "o", xlab = expression(1 - eta), ylab = "Sample Size", lwd = 2)
    
  })
  
  #Figure 2
  testtimes <- c(2, 4, 6, 8)
   ## Sample size as function of hazard ratio
  output$disPlot2_1 <- renderPlot({
    noevent <- 0.9
    survivals <- exp(log(noevent) * testtimes/max(testtimes))
    HR <- seq(input$HRange[1], input$HRange[2], by = 0.05)
    ssize <- sapply(HR, function(x) icpower(x, input$sen3, input$spe3, survivals, power = 0.9)$result$N)
    plot(HR, ssize, type = "n", xlim = c(1,3), ylim = c(0, 12000), xlab = "HR", ylab = "Sample Size", main = "(a)")
    lines(HR, ssize, lwd = 2)
    legend("topright", cex = 1.25, legend = paste("(", input$sen3, ", ", input$spe3, ")"), lwd = 1.5)
  })
   ## Sample size as function of cumulative incidence
  output$disPlot2_2 <- renderPlot({
    HR <- 1.25
    testtimes <- c(2, 4, 6, 8)
    noevent <- seq(input$nevent[1], input$nevent[2], by = 0.05)
    ssize <- sapply(noevent, function(x) icpower(HR, input$sen3, input$spe3, exp(log(x) * testtimes/max(testtimes)), power = 0.9)$result$N)
    plot(1 - noevent, ssize, type = "n", xlab = expression(paste("Cumulative incidence (", 1 - S[J + 1], ")")), ylab = "Sample size", main = "(b)", ylim = c(0, 14000), cex.lab = 1.25)
    lines(1 - noevent, ssize, lwd = 2)
    legend("topright", legend = paste("(", input$sen3, ", ", input$spe3, ")"), lwd = 2)
  })
   ## Sample size as function of number of tests
  output$disPlot2_3 <- renderPlot({
    noevent <- 0.9
    HR <- 1.25
    ntest <- c(input$ntest[1]:input$ntest[2])
    survivals <- exp(log(noevent) * testtimes/max(testtimes))
    ssize <- sapply(ntest, function(x) {
      testtimes <- 1:x
      survivals <- exp(log(noevent) * testtimes/max(testtimes))
      icpower(HR, input$sen3, input$spe3, survivals, power = 0.9)$result$N
    })
    plot(ntest, ssize, type = "n", ylim = c(8000, 20000), xlab = "Number of tests", ylab = "Sample size", main = "(c)", cex.lab = 1.25)
    lines(ntest, ssize, lwd = 2)
    legend("topright", legend = paste("(", input$sen3, ", ", input$spe3, ")"), lwd = 2)
  })
  
  # 3.3 Relative efficiency of test schedule 1 and schedule 2
  icpower.schedule <- function(schedules, ratios, HR, sensitivity, specificity, survivals, alpha = 0.05, power = 0.9) {
    ntest <- length(survivals)
    If <- matrix(0, nrow = ntest + 1, ncol = ntest + 1)
    for (i in seq_along(schedules)) {
      id <- sort(schedules[[i]])
      mid <- c(1, id + 1)
      pow <- icpower(HR = HR, sensitivity = sensitivity, specificity = specificity,
                     survivals = survivals[id], power = 0.9)
      If[mid, mid] <- If[mid, mid] + ratios[i] * (pow$I1 + pow$I2)/2
    }
    inv.If <- solve(If)
    beta.var <- inv.If[1]
    N <- ceiling((qnorm(1 - alpha/2) + qnorm(power))^2 * beta.var/log(HR)^2) 
    N
  }
   ## Relative sample size as function of hazard ratio
  output$disPlot3_1 <- renderPlot({
  noevent <- 0.9
  testtimes <- 1:8
  survivals <- exp(log(noevent) * testtimes/max(testtimes)) 
  HR <- seq(input$HRange[1], input$HRange[2], by = 0.05)
  ssize.HR <- function(HR, sensitivity, specificity) {
    schedules <- list(c(1, 3, 5, 7), c(2, 4, 6, 8))
    ratios <- c(0.5, 0.5)
    ssize1 <- icpower.schedule(schedules, ratios, HR, sensitivity, specificity, survivals)
    ssize2 <- icpower(HR, sensitivity, specificity, survivals[schedules[[2]]], power = 0.9)$result$N 
    ssize1/ssize2
  }
  ratio <- sapply(HR, function(x) ssize.HR(x, input$sen4, input$spe4))
  plot(HR, ratio, type = "n", ylim = c(1.06, 1.20), xlab = "HR", ylab = "Sample size ratio", main = "(a)") 
  lines(HR, ratio, lwd = 2)
  legend("topright", legend = paste("(", input$sen4, ", ", input$spe4, ")"), lwd = 2)
  })
   ## Relative sample size as function of cumulative incidence
  output$disPlot3_2 <- renderPlot({
    HR <- 1.25
    testtimes <- 1:8
    noevent <- seq(input$nevent[1], input$nevent[2], by = 0.05)
    ssize.cumi <- function(noevent, sensitivity, specificity) { 
      survivals <- exp(log(noevent) * testtimes/max(testtimes)) 
      schedules <- list(c(1, 3, 5, 7), c(2, 4, 6, 8))
      ratios <- c(0.5, 0.5)
      ssize1 <- icpower.schedule(schedules, ratios, HR, sensitivity, specificity, survivals)
      ssize2 <- icpower(HR, sensitivity, specificity, survivals[schedules[[2]]], power = 0.9)$result$N 
      ssize1/ssize2
    }
    ratio <- sapply(noevent, function(x) ssize.cumi(x, input$sen4, input$spe4))
    plot(1 - noevent, ratio, type = "n", xlab = expression(paste("Cumulative incidence (", 1 - S[J + 1], ")")), ylab = "Sample size ratio", main = "(b)", cex.lab = 1.25, ylim = c(1, 1.33))
    lines(1 - noevent, ratio, lwd = 2)
    legend("topright", legend = paste("(", input$sen4, ", ", input$spe4, ")"), lwd = 2)
  })
   ## Relative sample size as function of number of tests
  output$disPlot3_3 <- renderPlot({
    HR <- 1.25
    noevent <- 0.9
    ntest <- seq(input$ntest4[1], input$ntest4[2], by = 2)
    ssize.ntest <- function(ntest, sensitivity, specificity) { 
      testtimes <- 1:ntest
      survivals <- exp(log(noevent) * testtimes/max(testtimes)) 
      schedules <- list(seq(1, ntest, by = 2), seq(2, ntest, by = 2))
      ratios <- c(0.5, 0.5)
      ssize1 <- icpower.schedule(schedules, ratios, HR, sensitivity, specificity, survivals)
      ssize2 <- icpower(HR, sensitivity, specificity, survivals[schedules[[2]]], power = 0.9)$result$N
      ssize1/ssize2
    }
    ratio <- sapply(ntest, function(x) ssize.ntest(x, input$sen4, input$spe4))
    plot(ntest, ratio, type = "n", ylim = c(1, 1.3), xlab = "Number of test points", ylab = "Sample size ratio", main = "(c)")
    lines(ntest, ratio, lwd = 2)
    legend("topright", legend = paste("(", input$sen4, ", ", input$spe4, ")"), lwd = 2)
  })
  
  # 3.4 Relative efficiency of NTFP when compared to the Design 1
  output$disPlot4 <- renderPlot({
    HR <- 1.25
    tests <- 1:8
    specificity <- seq(0.7, 1, by = 0.01)
    plot(1, 1, type = "n", log = "y", xlab = "Specificity", ylab = "Sample size ratio", ylim = c(1, 55), xlim = c(0.7, 1), cex.lab = 1.2)
    noevent <- input$cuminc
    survivals <- exp(log(noevent)/max(tests) * tests) 
    MCAR <- sapply(specificity, function(x) icpower(HR, input$sen5, x, survivals, power = 0.9, design = "MCAR")$result$N)
    NTFP <- sapply(specificity, function(x) icpower(HR, input$sen5, x, survivals, power = 0.9, design = "NTFP")$result$N)
    ratio <- NTFP/MCAR
    lines(specificity, ratio, lwd = 2)
    legtex <- paste("S[J + 1] =", input$cuminc, ", Sensitivity=", input$sen5)
    legend("topright", legend = legtex, lwd = 2)
  })
  
  # Total cost as function of the number of tests
  output$disPlot5 <- renderPlot({
    HR <- 1.25
    noevent <- 0.9
    ntest <- seq(input$ntest6[1], input$ntest6[2], by = 1)
    ## Function to calculate sample size for number of tests 
    sample.size <- function(num.test) {
      testtimes <- 1:num.test/num.test * 8
      survivals <- exp(log(noevent) * testtimes/max(testtimes))
      ssize <- icpower(HR, 0.55, 0.99, survivals, power = 0.9)$result$N
    }
    ssize <- sapply(ntest, sample.size)
    C0 <- 1
    C <- input$cost
    cost <- C0 * ssize + C * ssize * ntest
    cost1 <- C0 * ssize + 0.1 * ssize * input$ntest6
    cost2 <- C0 * ssize + 1 * ssize * input$ntest6
    par(mfrow = c(1, 2))
    plot(ntest, cost, ylim = range(c(cost1, cost2)), type = "o", pch = 1, xlab = "Number of tests", ylab = "Cost", cex.lab = 1.25, main = "(a)")
    legend("topleft", legend = paste("(", input$sen6, ", ", input$spe6, ")"), lwd = 2)
    plot(ntest, cost, type = "o", pch = 2, xlab = "Number of tests", ylab = "Cost", cex.lab = 1.25, main = "(b)")
    
  })
 
})
