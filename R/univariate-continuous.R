#' @title Univariate continuous distribution gadget
#' @description This gadget lets you choose the parameters of a univariate,
#'   continuous distribution easily. For certain distributions, it supports
#'   multiple parameterization options. For example, the Beta distribution
#'   has two parameterizations: the classic one with shape parameters and a
#'   more intuitive one using the expectation and precision as proposed by
#'   Ferrari and Cribari-Neto (2004).
#' @return A named vector containing the chosen parameter value(s).
#' @import shiny
#' @import miniUI
#' @references Ferrari, S., & Cribari-Neto, F. (2004). Beta regression for
#'   modelling rates and proportions. Journal of Applied Statistics.
#' @export
univariate_continuous_addin <- function() {
  
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("you must have RStudio v0.99.878 or newer to use this gadget", call. = FALSE)
  }
  
  ui <- miniPage(
    gadgetTitleBar("Univariate Continuous Distribution"),
    miniContentPanel(
      shinyjs::useShinyjs(),
      fillRow(
        selectInput(
          "distribution", "Distribution",
          c("Normal", "Beta", "Inverse-gamma", "Chi-squared"),
          selected = "Beta", multiple = FALSE
        ),
        radioButtons("parameterization", "Parameterization",
                     choices = c("Classic", "Intuitive"),
                     inline = TRUE),
        height = "100px"
      ),
      uiOutput("parameters"),
      plotOutput("distribution")
    ),
    theme = shinythemes::shinytheme("flatly")
  )
  
  server <- function(input, output, session) {
    output$parameters <- renderUI({
      if (input$distribution == "Normal") {
        shinyjs::disable("parameterization")
        fillRow(
          sliderInput('mean', HTML("&mu;"), -10, 10, step = 0.1, value = 0, width = '90%'),
          sliderInput('sd', HTML("&sigma;"), 0.1, 100, step = 0.1, value = 1, width = '90%'),
          height = "100px"
        )
      } else if (input$distribution == "Beta") {
        shinyjs::enable("parameterization")
        if (input$parameterization == "Classic") {
          fillRow(
            sliderInput('a', HTML("&alpha;"), 0.1, 100, step = 0.01, value = 1.5, width = '90%'),
            sliderInput('b', HTML("&beta;"), 0.1, 100, step = 0.01, value = 1.5, width = '90%'),
            height = "100px"
          )
        } else {
          fillRow(
            sliderInput('expectation', 'Expected Response', 0, 1, step = 0.01, value = 0.5, width = '90%'),
            sliderInput('precision', 'Precision', 0.1, 100, step = 0.1, value = 3, width = '90%'),
            height = "100px"
          )
        }
      } else if (input$distribution == "Inverse-gamma") {
        shinyjs::disable("parameterization")
        fillRow(
          sliderInput('a', HTML("&alpha;: shape"), 0.001, 10, step = 0.001, value = 0.01, width = '90%'),
          sliderInput('b', HTML("&beta;: scale"), 0.001, 10, step = 0.001, value = 0.01, width = '90%'),
          height = "100px"
        )
      } else if (input$distribution == "Chi-squared") {
        shinyjs::disable("parameterization")
        fillRow(
          sliderInput('df', 'k: degrees of freedom', 0.001, 10, step = 0.001, value = 0.01, width = '90%'),
          sliderInput('ncp', HTML("&lambda;: non-centrality parameter"), 0.001, 10, step = 0.001, value = 0.01, width = '90%'),
          height = "100px"
        )
      }
    })
    
    output$distribution <- renderPlot({
      if (input$distribution == "Normal") {
        mu <- input$mean; sigma <- input$sd
        curve(dnorm(x, mu, sigma),
              from = -20, to = 20, n = 201,
              ylab = expression(f(x ~ "|" ~ mu, sigma)), lwd = 2,
              main = sprintf("x ~ Normal(%0.2f,%0.2f)", mu, sigma))
      } else if (input$distribution == "Beta") {
        if (input$parameterization == "Classic") {
          a <- input$a; b <- input$b
        } else {
          a <- input$expectation * input$precision
          b <- (1 - input$expectation) * input$precision
        }
        curve(dbeta(x, shape1 = a, shape2 = b),
              from = 0, to = 1, n = 201,
              ylab = expression(f(x ~ "|" ~ alpha, beta)), lwd = 2,
              main = sprintf("x ~ Beta(%0.2f,%0.2f)", a, b))
      } else if (input$distribution == "Inverse-gamma") {
        a <- input$a; b <- input$b
        curve(extraDistr::dinvgamma(x, alpha = a, beta = b),
              from = 0, to = 10, n = 201,
              ylab = expression(f(x ~ "|" ~ alpha, beta)), lwd = 2,
              main = sprintf("x ~ Inv-Gamma(%0.2f,%0.2f)", a, b))
      } else if (input$distribution == "Chi-squared") {
        df <- input$df; ncp <- input$ncp
        curve(dchisq(x, df, ncp),
              from = 0, to = 50, n = 201,
              ylab = expression(f(x ~ "|" ~ k, lambda)), lwd = 2,
              main = substitute(x ~ "~" ~ chi^{2} ~ group("(",list(df, ncp),")"), list(df = df, ncp = ncp)))
        abline(v = ncp, lty = "dashed")
        legend("topright", lty = "dashed", legend = expression(lambda), bty = "n")
      }
    })
    observeEvent(input$done, {
      if (input$distribution == "Normal") {
        output <- c("mean" = input$mean, "std.dev" = input$sd)
      } else if (input$distribution == "Beta") {
        if (input$parameterization == "Classic") {
          output <- c("shape1" = input$a, "shape2" = input$b)
        } else {
          output <- c("shape1" = input$expectation * input$precision,
                      "shape2" = (1 - input$expectation) * input$precision)
        }
      } else if (input$distribution == "Inverse-gamma") {
        output <- c("alpha" = input$a, "beta" = input$b)
      } else if (input$distribution == "Chi-squared") {
        output <- c("df" = input$df, "ncp" = input$ncp)
      }
      stopApp(output)
    })
  }
  
  viewer <- paneViewer("maximize")
  runGadget(ui, server, viewer = viewer)
  
}
