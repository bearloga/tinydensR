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
          c(
            "Beta",
            "Cauchy",
            "Chi-squared",
            "Exponential",
            "Gamma",
            "Inverse-gamma",
            "Log-Normal",
            "Normal",
            "Student-t",
            "Weibull (incl. exponentiated)"
          ),
          multiple = FALSE
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
    observe({
      shinyjs::toggleState("parameterization", condition = input$distribution %in% c("Beta", "Gamma"))
    })
    output$parameters <- renderUI({
      if (input$distribution == "Beta") {
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
      } else if (input$distribution == "Cauchy") {
        fillRow(
          sliderInput('location', HTML("x<sub>0</sub>: location"), -10, 10, step = 1, value = 0, width = '90%'),
          sliderInput('scale', HTML("&gamma;: scale"), 0.001, 10, step = 0.001, value = 5, width = '90%'),
          height = "100px"
        )
      } else if (input$distribution == "Chi-squared") {
        fillRow(
          sliderInput('df', 'k: degrees of freedom', 0.001, 10, step = 0.001, value = 0.01, width = '90%'),
          sliderInput('ncp', HTML("&lambda;: non-centrality parameter"), 0.001, 10, step = 0.001, value = 0.01, width = '90%'),
          height = "100px"
        )
      } else if (input$distribution == "Exponential") {
        fillRow(
          sliderInput('rate', HTML("&lambda;: rate"), 0.001, 2, step = 0.001, value = 0.01, width = '90%'),
          height = "100px"
        )
      } else if (input$distribution == "Gamma") {
        if (input$parameterization == "Classic") {
          fillRow(
            sliderInput('shape', 'k: shape', 0.01, 10, step = 0.01, value = 2, width = '90%'),
            sliderInput('scale', HTML("&theta;: scale"), 0.1, 2, step = 0.1, value = 1.5, width = '90%'),
            height = "100px"
          )
        } else {
          fillRow(
            sliderInput('shape', HTML("&alpha;: shape"), 0.01, 10, step = 0.01, value = 2, width = '90%'),
            sliderInput('rate', HTML("&beta;: rate"), 0.5, 10, step = 0.1, value = 0.667, width = '90%'),
            height = "100px"
          )
        }
      } else if (input$distribution == "Inverse-gamma") {
        fillRow(
          sliderInput('a', HTML("&alpha;: shape"), 0.001, 10, step = 0.001, value = 0.01, width = '90%'),
          sliderInput('b', HTML("&beta;: scale"), 0.001, 10, step = 0.001, value = 0.01, width = '90%'),
          height = "100px"
        )
      } else if (input$distribution == "Log-Normal") {
        fillRow(
          sliderInput('mean', HTML("&mu;"), -2, 10, step = 0.1, value = 0, width = '90%'),
          sliderInput('sd', HTML("&sigma;"), 0.1, 4, step = 0.1, value = 1, width = '90%'),
          height = "100px"
        )
      } else if (input$distribution == "Normal") {
        fillRow(
          sliderInput('mean', HTML("&mu;"), -10, 10, step = 0.1, value = 0, width = '90%'),
          sliderInput('sd', HTML("&sigma;"), 0.1, 100, step = 0.1, value = 1, width = '90%'),
          height = "100px"
        )
      } else if (input$distribution == "Student-t") {
        fillRow(
          sliderInput('df', HTML("&nu;: degrees of freedom"), 0.001, 10, step = 0.001, value = 0.5, width = '90%'),
          height = "100px"
        )
      } else if (input$distribution == "Weibull (incl. exponentiated)") {
        tagList(
          fillRow(
            sliderInput('shape1', 'k: first shape parameter', 0.5, 10, step = 0.25, value = 1.5, width = '90%'),
            sliderInput('shape2', HTML("&alpha;: second shape parameter"), 0.5, 10, step = 0.25, value = 1, width = '90%'),
            sliderInput('scale', HTML("&lambda;: scale parameter"), 0.5, 10, step = 0.25, value = 1, width = '90%'),
            height = "100px"
          ),
          HTML("Setting &alpha; = 1 yields the regular, non-exponentiated Weibull distribution")
        )
      }
    })
    
    output$distribution <- renderPlot({
      if (input$distribution == "Beta") {
        if (input$parameterization == "Classic") {
          a <- input$a; b <- input$b
        } else {
          a <- input$expectation * input$precision
          b <- (1 - input$expectation) * input$precision
        }
        req(a, b)
        curve(dbeta(x, shape1 = a, shape2 = b),
              from = 0, to = 1, n = 201,
              ylab = expression(f(x ~ "|" ~ alpha, beta)), lwd = 2,
              main = sprintf("x ~ Beta(%0.2f,%0.2f)", a, b))
      } else if (input$distribution == "Cauchy") {
        location <- input$location; scale <- input$scale
        req(location, scale)
        curve(dcauchy(x, location = location, scale = scale),
              from = -20, to = 20, n = 401,
              ylab = expression(f(x ~ "|" ~ x[0], gamma)), lwd = 2,
              main = sprintf("x ~ Cauchy(%0.2f,%0.2f)", location, scale))
      } else if (input$distribution == "Chi-squared") {
        df <- input$df; ncp <- input$ncp
        req(df, ncp)
        curve(dchisq(x, df, ncp),
              from = 0, to = 50, n = 201,
              ylab = expression(f(x ~ "|" ~ k, lambda)), lwd = 2,
              main = substitute(x ~ "~" ~ chi^{2} ~ group("(",list(df, ncp),")"), list(df = df, ncp = ncp)))
        abline(v = ncp, lty = "dashed")
        legend("topright", lty = "dashed", legend = expression(lambda), bty = "n")
      } else if (input$distribution == "Exponential") {
        rate <- input$rate
        req(rate)
        curve(dexp(x, rate = rate),
              from = 0, to = 20, n = 401,
              ylab = expression(f(x ~ "|" ~ lambda)), lwd = 2,
              main = sprintf("x ~ Exp(%0.2f)", rate))
      } else if (input$distribution == "Gamma") {
        shape <- input$shape
        if (input$parameterization == "Classic") {
          scale <- input$scale
        } else {
          scale <- 1 / input$rate
        }
        req(shape, scale)
        curve(dgamma(x, shape = shape, scale = scale),
              from = 0, to = 20, n = 201,
              ylab = expression(f(x ~ "|" ~ alpha, beta)), lwd = 2,
              main = sprintf("x ~ Gamma(%0.2f,%0.2f)", shape, 1 / scale))
      } else if (input$distribution == "Inverse-gamma") {
        a <- input$a; b <- input$b
        req(a, b)
        curve(extraDistr::dinvgamma(x, alpha = a, beta = b),
              from = 0, to = 10, n = 201,
              ylab = expression(f(x ~ "|" ~ alpha, beta)), lwd = 2,
              main = sprintf("x ~ Inv-Gamma(%0.2f,%0.2f)", a, b))
      } else if (input$distribution == "Log-Normal") {
        mulog <- input$mean; sigmalog <- input$sd
        req(mulog, sigmalog)
        curve(dlnorm(x, meanlog = mulog, sdlog = sigmalog),
              from = 0, to = 10, n = 201,
              ylab = expression(f(x ~ "|" ~ mu, sigma)), lwd = 2,
              main = sprintf("x ~ Log-Normal(%0.2f,%0.2f)", mulog, sigmalog))
      } else if (input$distribution == "Normal") {
        mu <- input$mean; sigma <- input$sd
        req(mu, sigma)
        curve(dnorm(x, mu, sigma),
              from = -20, to = 20, n = 201,
              ylab = expression(f(x ~ "|" ~ mu, sigma)), lwd = 2,
              main = sprintf("x ~ Normal(%0.2f,%0.2f)", mu, sigma))
      } else if (input$distribution == "Student-t") {
        df <- input$df
        req(df)
        curve(dt(x, df),
              from = -20, to = 20, n = 201,
              ylab = expression(f(x ~ "|" ~ nu)), lwd = 2,
              main = sprintf("x ~ t(%0.3f)", df))
      } else if (input$distribution == "Weibull (incl. exponentiated)") {
        dweibull <- function(x, k, alpha, lambda) {
          return(alpha * (k / lambda) * (x / lambda)^(k - 1) * (1 - exp(-(x / lambda)^k))^(alpha - 1) * exp(-(x / lambda)^k))
        }
        shape1 <- input$shape1; shape2 <- input$shape2; scale <- input$scale
        req(shape1, shape2, scale)
        plot_title <- ifelse(shape2 == 1,
                             sprintf("x ~ Weibull(%0.2f,%0.2f)", shape1, scale),
                             sprintf("x ~ ExpWeibull(%0.2f,%0.2f,%0.2f)", shape1, shape2, scale))
        curve(dweibull(x, k = shape1, alpha = shape2, lambda = scale),
              from = 1e-6, to = 10, n = 201,
              ylab = expression(f(x ~ "|" ~ k, alpha, lambda)), lwd = 2,
              main = plot_title)
      }
    })
    observeEvent(input$done, {
      if (input$distribution == "Beta") {
        if (input$parameterization == "Classic") {
          output <- c("shape1" = input$a, "shape2" = input$b)
        } else {
          output <- c("shape1" = input$expectation * input$precision,
                      "shape2" = (1 - input$expectation) * input$precision)
        }
      } else if (input$distribution == "Cauchy") {
        output <- c("location" = input$location, "scale" = input$scale)
      } else if (input$distribution == "Chi-squared") {
        output <- c("df" = input$df, "ncp" = input$ncp)
      } else if (input$distribution == "Exponential") {
        output <- c("rate" = input$rate)
      } else if (input$distribution == "Gamma") {
        if (input$parameterization == "Classic") {
          output <- c("shape" = input$shape, "scale" = input$scale)
        } else {
          output <- c("shape" = input$shape, "rate" = input$rate)
        }
      } else if (input$distribution == "Inverse-gamma") {
        output <- c("alpha" = input$a, "beta" = input$b)
      } else if (input$distribution == "Log-Normal") {
        output <- c("meanlog" = input$mean, "sdlog" = input$sd)
      } else if (input$distribution == "Normal") {
        output <- c("mean" = input$mean, "std.dev" = input$sd)
      } else if (input$distribution == "Student-t") {
        output <- c("df" = input$df)
      } else if (input$distribution == "Weibull (incl. exponentiated)") {
        output <- c("k (shape 1)" = input$shape1,
                    "alpha (shape 2)" = input$shape2,
                    "lambda (scale)" = input$scale)
      }
      stopApp(output)
    })
  }
  
  viewer <- paneViewer("maximize")
  runGadget(ui, server, viewer = viewer)
  
}
