#' @title Univariate discrete distribution gadget
#' @description This gadget lets you choose the parameters of a univariate,
#'   discrete distribution easily.
#' @return A named vector containing the chosen parameter value(s).
#' @import shiny
#' @import miniUI
#' @export
univariate_discrete_addin <- function() {
  
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("you must have RStudio v0.99.878 or newer to use this gadget", call. = FALSE)
  }
  
  ui <- miniPage(
    gadgetTitleBar("Univariate Discrete Distribution"),
    miniContentPanel(
      shinyjs::useShinyjs(),
      fillRow(
        selectInput(
          "distribution", "Distribution",
          c("Binomial", "Poisson"),
          selected = "Binomial", multiple = FALSE
        ),
        height = "100px"
      ),
      uiOutput("parameters"),
      plotOutput("distribution")
    ),
    theme = shinythemes::shinytheme("flatly")
  )
  
  server <- function(input, output, session) {
    output$parameters <- renderUI({
      if (input$distribution == "Binomial") {
        fillRow(
          sliderInput('n', 'n: number of independent experiments', 1, 100, step = 1, value = 10, width = '90%'),
          sliderInput('p', 'p: probability of success', 0, 1, step = 0.001, value = 0.5, width = '90%'),
          height = "100px"
        )
      } else if (input$distribution == "Poisson") {
        fillRow(
          sliderInput('lambda', HTML("&lambda;: expected number of occurrences"), 0.01, 10, step = 0.01, value = 1, width = '90%'),
          height = "100px"
        )
      }
    })
    
    output$distribution <- renderPlot({
      if (input$distribution == "Binomial") {
        n <- input$n; p <- input$p
        req(n, p)
        x <- 0:n
        y <- dbinom(x, n, p)
        title <- sprintf("Number of successes ~ Binomial(%0.2f,%0.2f)", n, p)
        y_lab <- expression(f(x ~ "|" ~ n, p))
      } else if (input$distribution == "Poisson") {
        lambda <- input$lambda
        req(lambda)
        x <- 0:10
        y <- dpois(x, lambda)
        title <- sprintf("Number of occurrences ~ Poisson(%0.2f)", lambda)
        y_lab <- expression(f(x ~ "|" ~ lambda))
      }
      plot(x, y, type = "h", lwd = 2, xaxt = "n", xlab = expression(x), ylab = y_lab, main = title)
      points(x, y, pch = 16)
      axis(1, x)
    })
    observeEvent(input$done, {
      if (input$distribution == "Binomial") {
        output <- c("n" = input$n, "p" = input$p)
      } else if (input$distribution == "Poisson") {
        output <- c("lambda" = input$lambda)
      }
      stopApp(output)
    })
  }
  
  viewer <- paneViewer("maximize")
  runGadget(ui, server, viewer = viewer)
  
}
