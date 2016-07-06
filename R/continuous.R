#' @title Beta distribution gadget
#' @description This gadget lets you choose the parameters of a Beta
#'   distribution easily. It supports two parameterization options: the classic
#'   one with shape parameters and a more intuitive one using the expectation
#'   and precision as proposed by Ferrari and Cribari-Neto (2004).
#' @return A numeric vector containing the chosen shape parameter values.
#' @import shiny
#' @import miniUI
#' @references Ferrari, S., & Cribari-Neto, F. (2004). Beta regression for
#'   modelling rates and proportions. Journal of Applied Statistics.
#' @export
beta_addin <- function() {
  
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("you must have RStudio v0.99.878 or newer to use this gadget",
         call. = FALSE)
  }
  
  ui <- miniPage(
    gadgetTitleBar("Beta Distribution"),
    miniContentPanel(
      fillPage(
        fillCol(fillRow(radioButtons("params", "Parameterization",
                                     choices = c("Classic", "Intuitive"),
                                     inline = TRUE),
                        height = "50px"),
                fillRow(uiOutput("slider_1"), uiOutput("slider_2"), height = "100px"),
                plotOutput("distribution"),
                flex = c(1, 2, 8)))
    )
  )
  
  server <- function(input, output, session) {
    output$slider_1 <- renderUI({
      if (input$params == "Classic") {
        sliderInput('a', 'α', 0.1, 100, step = 0.01, value = 1.5,
                    width = '90%', animate = TRUE)
      } else {
        sliderInput('expectation', 'Expected Response', 0, 1, step = 0.01, value = 0.5,
                    width = '90%', animate = TRUE)
      }
    })
    
    output$slider_2 <- renderUI({
      if (input$params == "Classic") {
        sliderInput('b', 'β', 0.1, 100, step = 0.01, value = 1.5,
                    width = '90%', animate = TRUE)
      } else {
        sliderInput('precision', 'Precision', 0.1, 100, step = 0.1, value = 3,
                    width = '90%', animate = TRUE)
      }
    })
    
    output$distribution <- renderPlot({
      if (input$params == "Classic") {
        a <- input$a
        b <- input$b
      } else {
        a <- input$expectation * input$precision
        b <- (1 - input$expectation) * input$precision
      }
      curve(dbeta(x, shape1 = a, shape2 = b), from = 0, to = 1, n = 201,
            ylab = expression(f(x ~ "|" ~ alpha, beta)), lwd = 2,
            main = sprintf("Beta(%0.2f,%0.2f)", a, b))
    })
    observeEvent(input$done, {
      if (input$params == "Classic") {
        output <- c("shape1" = input$a, "shape2" = input$b)
      } else {
        output <- c("shape1" = input$expectation * input$precision,
                    "shape2" = (1 - input$expectation) * input$precision)
      }
      stopApp(output)
    })
  }
  
  viewer <- paneViewer("maximize")
  runGadget(ui, server, viewer = viewer)
  
}
