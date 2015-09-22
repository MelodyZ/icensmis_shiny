
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(icensmis)

shinyServer(function(input, output) {
  
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
    
    lambda.base <- -log(input$neven)/max(input$tim)
    survivals <- 1 - pexp(input$tim, lambda.base)
    plot(0, 0, type = "n", xlab = "N", ylab = "Power", xlim = c(0, max(Nlist)), 
         cex.lab = 1.4, cex.main = 1.5, ylim = c(0, 1))
    
    pow <- icpower(HR = 1.25, 
                   sensitivity = input$sen2, 
                   specificity = input$spe2, 
                   survivals, 
                   Nlist)$result
    lines(pow$N, pow$power, lwd = 2)
    legend("topleft", cex = 1.25, legend = paste("(", input$sen2, ", ", input$spe2, ")"), lwd = 1.5)
  })
  
  #Figure 2
  testtimes <- c(2, 4, 6, 8)
   ## Sample size as function of hazard ratio
  output$disPlot2_1 <- renderPlot({
    noevent <- 0.9
    survivals <- exp(log(noevent) * testtimes/max(testtimes))
    HR <- seq(input$HRange[1], input$HRange[2], by = 0.05)
    ssize <- sapply(HR, function(x) icpower(x, input$sen3, input$spe3, survivals, power = 0.9)$result$N)
    plot(HR, ssize, type = "n", xlab = "HR", ylab = "Sample Size", main = "(a)")
    lines(HR, ssize, lwd = 2)
    legend("topright", cex = 1.25, legend = paste("(", input$sen3, ", ", input$spe3, ")"), lwd = 1.5)
  })
   ## Sample size as function of cumulative incidence
  output$disPlot2_2 <- renderPlot({
    HR <- 1.25
    noevent <- seq(input$nevent[1], input$nevent[2], by = 0.05)
    ssize <- sapply(noevent, function(x) icpower(HR, input$sen3, input$spe3, exp(log(x) * testtimes/max(testtimes)), power = 0.9)$result$N)
    plot(1 - noevent, ssize, type = "n", xlab = expression(paste("Cumulative incidence (", 1 - S[J + 1], ")")), ylab = "Sample size", main = "(b)", ylim = c(0, 14000), cex.lab = 1.25)
    lines(1 - noevent, ssize)
    legend("topright", legend = paste("(", input$sen3, ", ", input$spe3, ")"), lwd = 1.5)
  })
 
})
