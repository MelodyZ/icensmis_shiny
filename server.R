
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(icensmis)

shinyServer(function(input, output) {
  
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
  
  Nlist <- seq(100, 15000, by = 100)
  
  output$disPlot <- renderPlot({
    
    lambda.base <- -log(input$neven)/max(input$tim)
    survivals <- 1 - pexp(input$tim, lambda.base)
    plot(0, 0, type = "n", xlab = "N", ylab = "Power", xlim = c(0, max(Nlist)), 
         cex.lab = 1.4, cex.main = 1.5, ylim = c(0, 1))
    
    pow <- icpower(HR = 1.25, 
                   sensitivity = input$sen, 
                   specificity = input$spe, 
                   survivals, 
                   Nlist)$result
    lines(pow$N, pow$power, lwd = 2)
    legend("topleft", cex = 1.25, legend = paste("(", input$sen, ", ", input$spe, ")"), lwd = 1.5)
  })
 
})
