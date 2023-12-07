#' @title App to Demo Central Limit Theorem
#' @author Thu Nguyen
#' 
#' @description
#' This app lets the users experience the Central Limit Theorem.
#' The users can choose from a few starting distributions and a sample size.
#' The app will then generate an appropriate sample based on users' choices.
#' The app provides a few plots.
#' 1st plot shows values from a single sample.
#' 2nd plot shows the sample means, and illustrates the CLT.
#' 3rd plot shows other sample statistics for comparison.
#' 4th tab shows the data and allows for downloading the data.
#' 5th tab displays a video explaining the CLT.
#' 6th tab provides a custom example of the CLT.
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

# fn to print output of CLT example
fn.print.CLT.eg <- function(mean, sd, n) {
  paste0(
    'By the Central Limit Theorem, the sample mean approximately follows a normal distribution with the mean of ', 
    mean, 
    ' and the standard deviation of ', 
    sd, '/sqrt(', n, ')', 
    ' = ', round(sd/sqrt(n), 3)
  )
}


# 3) Shiny App ----------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel('Central Limit Theorem Demos'),
  tags$hr(),
  h4('Recently learnt of the Central Limit Theorem and want to see it in action?'),
  tags$hr(),
  tags$div(
    tags$p('
           You can choose the starting distribution and the sample size. 
           The app will then generate the appropriate samples, calculate the sample means, and plot them.
           Besides sample means, other sample statistics such as min, median, and max are also computed and plotted for comparison.
           ')
  ),
  tags$p('Options for the starting distribution include Normal, N(0,1), Exponential, Exp(1), Poisson, Poisson(1), Geometric, Geom(.5), and Binomial, Bino(.5).'),
  tags$hr(),
  # 
  p('How to use this demo:'),
  tags$ol(
    tags$li('Choose the starting distribution'), 
    tags$li('Choose the sample size n')
  ),
  p('1,000 samples each of size n will be generated from the chosen distribution.'),
  tags$hr(),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('distribution', 'Select distribution:',
                  choices = distn.list, selected = 'Normal'),
      sliderInput('sample.size', 'Select sample size, n,:',
                  min = 1, max = 500, value = 30),
      #Wait until action button is clicked
      actionButton(
        inputId = "btn_Update",
        label = "Update")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = 'tabs',
                  tabPanel('Draws from 1st sample',
                           plotOutput('sample.draw')),
                  tabPanel('Sample means',
                           plotOutput('sample.means')),
                  tabPanel('Other sample statistics',
                           plotOutput('other.sample.statistics')),
                  tabPanel('Sample summary',
                           dataTableOutput('sample.summary.table'),
                           downloadButton('download.samples', 'Download Samples'),
                           downloadButton('download.sample.statistics', 'Download Sample Statistics')),
                  tabPanel('Learn about the CLT',
                           tags$div(
                            tags$p('
                             Never heard of the Central Limit Theorem before? 
                             Or want to refresh your memory of the Central Limit Theorem?
                             This 3Blue1Brown explains the Central Limit Theorem and its intuition.
                             '),
                            tags$hr(),
                            tags$iframe(width = '100%', height = '400', frameborder = '0', src='https://www.youtube.com/embed/zeJD6dqJ5lo?si=f8KVAuSvWOXgXdQ_', allowfullscreen = TRUE))
                  ),
                  tabPanel('Apply the CLT',
                           h4('Want to see how the CLT is applied?'),
                           p('Think of a distribution, any distribution. Without giving me the distribution, what are the mean and standard deviation of your distribution?'),
                           sliderInput('eg.Mean', 'Distribution mean:',
                                       min = 1, max = 500, value = 50),
                           sliderInput('eg.SD', 'Distribution standard deviation:',
                                       min = 1, max = 500, value = 50),
                           p('Great! Now imagine drawing a sample of size n:'),
                           sliderInput('eg.Sample.Size', 'Sample Size:',
                                       min = 1, max = 500, value = 50),
                           p('Without knowing your distribution, the Central Limit Theorem can still model the sample mean. Click the Run button below to find out.'),
                           actionButton(
                             inputId = "btn_eg.Update",
                             label = "Run CLT"),
                           tags$hr(),
                           p(textOutput('eg.Output', container = span))
                  )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Generate Data, only on Update Button Click ------------
  dat <- eventReactive(input$btn_Update, {
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
  
  # Apply CLT: Update Text --------------------------------
  eg.CLT <- eventReactive(input$btn_eg.Update, {
    fn.print.CLT.eg(input$eg.Mean, input$eg.SD, input$eg.Sample.Size)
  })
  
  output$eg.Output <- renderText({
    eg.CLT()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)