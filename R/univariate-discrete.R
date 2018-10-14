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
          c("Binomial", "Hypergeometric", "Poisson"),
          multiple = FALSE
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
      } else if (input$distribution == "Hypergeometric") {
        tagList(
          HTML(paste("<span class=\"help-block\">The binomial uses draws <em>with</em> replacement,",
                     "while hypergeometric distribution uses draws <em>without</em> replacement.</span>")),
          fillRow(
            sliderInput('N', 'N: population size', 0, 200, step = 1, value = 10, width = '90%'),
            sliderInput('K', 'K: instances with feature', 0, 10, step = 1, value = 0, width = '90%'),
            sliderInput('n', 'n: draws without replacement', 0, 10, step = 1, value = 0, width = '90%'),
            height = "100px"
          ),
          helpText("The maximum values of K and n depend on N, so adjust that first.")
        )
      } else if (input$distribution == "Poisson") {
        fillRow(
          sliderInput('lambda', HTML("&lambda;: expected number of occurrences"), 0.01, 10, step = 0.01, value = 1, width = '90%'),
          height = "100px"
        )
      }
    })
    
    observe({
      if (input$distribution == "Hypergeometric") {
        updateSliderInput(session, "K", max = input$N, value = 0)
        updateSliderInput(session, "n", max = input$N, value = 0)
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
      } else if (input$distribution == "Hypergeometric") {
        req(input$N, input$K, input$n)
        N <- input$N # total number of balls (black and white) in urn
        K <- input$K # number of white balls in urn
        n <- input$n # number of draws
        req(K <= N, n <= N)
        x <- 0:min(n, K)
        y <- dhyper(x, m = K, n = N - K, k = n)
        title <- sprintf("Number of successes ~ Hypergeometric(%i, %i, %i)", N, K, n)
        y_lab <- expression(f(x ~ "|" ~ N, K, n))
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
      } else if (input$distribution == "Hypergeometric") {
        output <- list(
          "Wikipedia parameterization" = c("N" = input$N, "K" = input$p, "n" = input$n),
          "R parameterization" = c("m" = input$K, "n" = input$N - input$K, "k" = input$n)
        )
      } else if (input$distribution == "Poisson") {
        output <- c("lambda" = input$lambda)
      }
      stopApp(output)
    })
  }
  
  viewer <- paneViewer("maximize")
  runGadget(ui, server, viewer = viewer)
  
}
