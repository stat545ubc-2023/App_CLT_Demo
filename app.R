#' @title App to Demo Central Limit Theorem
#' @author Thu Nguyen
#' @description
#' This app lets the users to experience Central Limit Theorem.
#' The users can choose from a few starting distribution and a sample size.
#' The app will then generate an appropriate sample based on users' choices.
#' The app provides a few plots.
#' 1st plot shows values from a single sample.
#' 2nd plot shows the sample means, and illustrates the CLT.
#' 3rd plot shows other sample statistics for comparison.
#' 4th tab shows the data and allows for downloading the data.
#' 

library(shiny)
# 1) Required libraries -------------------------------------------------------
library(ggplot2)
library(reshape2)
library(dplyr)
library(DT)

# 2) Required functions -------------------------------------------------------
distn.list <- c('Normal', 'Exponential', 'Poisson', 'Geometric', 'Binomial')

# fn to generate random samples, based on chosen distribution and sample size
fn.generate.sample <- function(distn, n, B = 1000) {
  if (distn == 'Normal') {
    dat <- matrix(rnorm(n*B), ncol = n)
  } else if (distn == 'Exponential') {
    dat <- matrix(rexp(n*B, rate = 1), ncol = n)
  } else if (distn == 'Poisson') {
    dat <- matrix(rpois(n*B, lambda = 1), ncol = n)
  } else if (distn == 'Geometric') {
    dat <- matrix(rgeom(n*B, prob = .5), ncol = n)
  } else if (distn == 'Binomial') {
    dat <- matrix(rbinom(n*B, n, prob = .5), ncol = n)
  } else {
    stop('Distribution not found.')
  }
  return(dat)
}

# fn to compute a few sample statistics
fn.compute.sample.statistics <- function(dat) {
  dat.summary <- data.frame(
    sample = 1:nrow(dat),
    mean = apply(dat, 1, mean),
    min = apply(dat, 1, min),
    median = apply(dat, 1, median),
    max = apply(dat, 1, max)
  )
  return(dat.summary)
}



# 3) Shiny App ----------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel('Central Limit Theorem Demos'),
    h4('Recently learnt of Central Limit Theorem and want to see it in action?'),
    h4('You can choose the starting distribution and the sample size. The app will then generate the appropriate samples, calculate the sample means, and plot them.'),
    h4('Besides sample means, other sample statistics such as min, median, and max are also computed and plotted for comparison.'),
    p('Options for the starting distribution include normal N(0,1), exponential Exp(1), Poisson Pois(1), Geometric Geom(.5), and Binomial Bino(.5).'),
    p('How to use this demo:'),
    p('Step 1: Choose the starting distribution.'),
    p('Step 2: Choose the sample size n.'),
    p('1,000 samples each of size n will be generated from the chosen distribution.'),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput('distribution', 'Select distribution:',
                      choices = distn.list, selected = 'Normal'),
          sliderInput('sample.size', 'Select sample size, n,:',
                        min = 1, max = 500, value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = 'tabs',
                      tabPanel('Draw from 1st sample',
                               plotOutput('sample.draw')),
                      tabPanel('Sample means',
                               plotOutput('sample.means')),
                      tabPanel('Other sample statistics',
                               plotOutput('other.sample.statistics')),
                      tabPanel('Sample summary',
                               dataTableOutput('sample.summary.table'),
                               downloadButton('download.samples', 'Download Samples'),
                               downloadButton('download.sample.statistics', 'Download Sample Statistics'))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Generate Data -----------------------------------------
  
  # Generate samples as chosen by user
  dat <- reactive({
    fn.generate.sample(input$distribution, input$sample.size)
  })
  
  # Compute sample statistics
  dat.summary <- reactive({
    fn.compute.sample.statistics(dat())
  })
  
  # Plots & Tables for App --------------------------------
  
  # Plot of 1 sample
  output$sample.draw <- renderPlot({
    data.frame(s1 = dat()[1,]) %>% 
      ggplot(aes(x = s1)) +
      geom_histogram(aes(y=..density..), color = NA, fill = 'gray') +
      labs(x = '', y = 'Density', title = 'Histogram and density curve of sample means') +
      theme_minimal()
  })
  
  # Plot of sample means
  output$sample.means <- renderPlot({
    dat.summary() %>% 
      ggplot(aes(x = mean)) +
      geom_histogram(aes(y=..density..), color = NA, fill = 'lightblue', alpha = 0.5) +
      geom_density(lwd = 1, color = '#336699') +
      geom_vline(aes(xintercept=mean(mean)), color= 'lightblue', linetype = 'dashed', size=1) +
      labs(x = '', y = 'Density', title = 'Histogram and density curve of sample means') +
      theme_minimal()
  })
  
  # Plot of multiple sample statistics
  output$other.sample.statistics <- renderPlot({
    dat.summary() %>% 
      melt(id.vars = 'sample') %>% 
      ggplot(aes(x = value, color = variable)) +
      geom_density(lwd = 1) +
      labs(x = '', y = 'Density', title = 'Density curves of a few sample statistics') +
      scale_color_discrete(name = 'Sample Statistics') +
      theme_minimal()
  })
  
  # Interactive table of sample statistics
  output$sample.summary.table <- renderDataTable({
    datatable(dat.summary(), class = 'cell-border stripe', rownames = FALSE) %>%
      formatRound(c(2:5), 3)
  })
  
  
  # Download Data -----------------------------------------
  
  # Download Samples
  output$download.samples <- downloadHandler(
    filename = function() {
      paste0(input$distribution, '_Samples.csv') 
    },
    content = function(file) {
      write.csv(dat(), file)
    }
  )
  
  # Download Sample Statistics
  output$download.sample.statistics <- downloadHandler(
    filename = function() {
      paste0(input$distribution, '_Sample_Statistics.csv') 
    },
    content = function(file) {
      write.csv(dat.summary(), file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
