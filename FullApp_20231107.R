library(shiny)
library(ggplot2)
library(dplyr)
library(mvtnorm)
library(MASS)
library(shinythemes)
library(shinyjs)

setwd("~/Dropbox/SKLab/Rshiny_JDFE")

CSS <- "
p {
  line-height: 1.6; 
  font-family: Helvetica;
  margin: 0;
  font-size: 14px;
}

.FullSchematic {
  float: right;
  width: 1000px;
  shape-outside: url(https://www.r-project.org/logo/Rlogo.svg);
  shape-margin: 20px;
  margin-right: 10px;
  margin-left: 20px;
  margin-bottom: 20px;
}
"

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs 
  tags$head( # set up use of the declared CSS options above , allows wrapping text around image
    tags$style(HTML(CSS))
  ),
  
  # Nice top label
  tags$div(class = "jumbotron text-center", style = "margin-bottom:0px;margin-top:0px",
           tags$h2(class = 'jumbotron-heading', stye = 'margin-bottom:0px;margin-top:0px', 'Exploring the pleiotropic outcomes of evolution'),
           p('Using the Joint Distribution of Fitnes Effects (JDFE)!')
  ),
  
  # introductory preamble 
  h1('Introduction'), # head size 1
# img(src = "JDFE_1.png", width = "15%", height = 270),#image needs to be in folder called www in app directory 
 column(
   width = 12,
   
    img(
      class = 'FullSchematic',
      src = "FullSchematic.png"
    ), # to add figure text wraps around
   p('Hello and welcome to my app all about better understanding the major evolutionary biology concept of', strong('pleiotropy'),
 '. We can think about pleiotropy like this:  as a population of microbes evolves to improve fitness in its "Home" environment, it must be \
  simultaneously gaining, losing, or not changing its fitness in other "Non-home" environments. ',
 'In a recent paper (', a('https://elifesciences.org/articles/73250'),'), I developed a method to predict what the pleiotropic trajectory \
 should look like given what we call a JDFE (or, Joint Distribution of Fitness Effects). The JDFE maps how mutations of any given fitness \
 effect in the "Home" environment will tend to impact fitness in the other "Non-home" environment. The mutations which are most important \
 on the JDFE are those that are beneficial in the home environment (e.g., Home fitness > 0). These are the mutations in quadrants 1 and 4 of the \
 2D plot. Mutations in quadrant 1 are those that promote "pleiotropic fitness gains" because they are beneficial in both the home and non-home.\
 Mutations in quadrant 4, on the other hand, are those that promote "pleiotropic fitness losses" because they are beneficial in the home but are\
 deleterious in the non-home. The shape of the JDFE determines both speed and direction of the "non-home" (pleiotropic) fitness trajectory  ')
   
 ),
#img(src = "FullSchematic.png", width = "50%", height = 400),#image needs to be in folder called www in app directory 
#h1('Simulating Evolution'),
#actionButton('toggleButton_PlaySimEvo', 'Play'),
#plotOutput('simEvoPlot'),
# br(),
 br(),
 'You try! Below, try toggling the slider bars to change what the \
 JDFE looks like. Consider the correlation of the JDFE as well as the relative densities in quadrants 1 and 4 correspond \
 to the fitness trajectory they create',
  br(),
  br(),
  
 h1('Application'),
  # Input fields - put in side bar
  sidebarLayout(
    sidebarPanel(
      sliderInput("uHome", label = "Mean - Home Fitness Effect", min = -1.25, max = 1.25, value = 0.05),
      sliderInput("uNonHome", label = "Mean -Non-Home Fitness Effect", min = -1.25, max = 1.25, value = -0.05),
      sliderInput("sdHome", label = "Stdev - Home Fitness Effect", min = 0.001, max = 1, value = 0.5),
      sliderInput("sdNonHome", label = "Stdev -Non-Home Fitness Effect", min = 0.001, max = 1, value = 0.5),
      sliderInput("corrJDFE", label = "JDFE correlation", min = -0.99, max = 0.99, value = 0.63),
      # Button to toggle visibility of sliders
      actionButton("toggleButton", "Plot Area Parameters"),
      
      # Sliders initially hidden, will be shown when the button is clicked
      div(id = "sliders", style = "display: none;",
          sliderInput("xlim", label = "JDFE X-axis Limits", min = -5, max = 5, value = c(-1,1)),
          sliderInput("ylim", label = "JDFE Y-axis Limits", min = -5, max = 5, value = c(-1,1)),
          sliderInput("xlimFitTraj", label = "Fit Traj X-axis Limits", min = 0, max = 10000, value = c(0,500)),
          sliderInput("ylimFitTraj", label = "Fit Traj Y-axis Limits", min = -1000, max = 1000, value = c(-400,400))
      ),
      width = 3
    ),
    mainPanel(
      # Output: Plot or other output components
      fluidRow(
        
        column(5,  plotOutput("JDFEplot")), # First plot will be displayed in the left column
        column(6, plotOutput("FitTraj")),
        height = '10%' # Second plot will be displayed in the right column
      )
    )
  )
)

server <- function(input, output, session) {
  # Show/hide sliders based on button click
  observeEvent(input$toggleButton, {
    toggle(id = "sliders")  # Toggle the visibility of sliders
  })
  
  # Get all reactives for JDFE params
  uHome <- reactive({
    input$uHome
  })
  
  sdHome <- reactive({
    input$sdHome
  })
  
  
  uNonHome <- reactive({
    input$uNonHome
  })
  
  sdNonHome <- reactive({
    input$sdNonHome
  })
  
  corrJDFE <- reactive({
    input$corrJDFE
  })
  
  xlimits <- reactive({
    input$xlim
  }) 
  
  ylimits <- reactive({
    input$ylim
  }) 
  
  xlimits_FitTraj <- reactive({
    input$xlimFitTraj
  }) 
  
  ylimits_FitTraj <- reactive({
    input$ylimFitTraj
  }) 
  
  
  
  
  # define the sim Evo plot 
  output$simEvoPlot <- renderPlot({ 
    
   df_fake = data.frame(x = c(1,2,3,4,5), y = c(9,9,9,9,9), color = c('red', 'red', 'red', 'blue', 'red'))
    plot =  ggplot(df_fake, aes(x, y, color = color)) +
      geom_point()+
      xlab('Individual')+
      ylab('Generation')+
      theme_classic()+
      theme(axis.text = element_text(color = 'black', size = 11),
            axis.title = element_text(color = 'black', size = 12),
            plot.title = element_text(size = 20, color = 'black') )
    
    
    
    plot
    
    
  }, res = 96, height = 600)
  
  # define the JDFE plot
  output$JDFEplot <- renderPlot({ 
    
    covar = corrJDFE()*sdHome()*sdNonHome()
    varHome = sdHome() * sdHome()
    varNonHome = sdNonHome() * sdNonHome()
    m <- c(uHome(), uNonHome())
    sigma <- matrix(c(varHome,covar,covar,varNonHome), nrow=2)
    
    data.grid <- expand.grid(s.1 = seq(xlimits()[1], xlimits()[2], length.out=200), s.2 = seq(ylimits()[1], ylimits()[2], length.out=200))
    q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = m, sigma = sigma))
    
    plot =  ggplot(q.samp, aes(x=s.1, y=s.2, z=prob)) +
      geom_contour(bins = 3)+
      geom_hline(yintercept = 0, color = 'grey')+
      geom_vline(xintercept = 0, color = 'grey')+
      xlim(xlimits())+
      ylim(ylimits())+
      xlab('Home Fitness Effect')+
      ylab('Non-Home Fitness Effect')+
      ggtitle("JDFE")+
      theme_classic()+
      theme(axis.text = element_text(color = 'black', size = 11),
            axis.title = element_text(color = 'black', size = 12),
            plot.title = element_text(size = 20, color = 'black') )
    
    
    
    plot
    
    
  }, res = 96, height = 600)
  
  
  
  output$FitTraj <- renderPlot({ 
    
    covar = corrJDFE()*sdHome()*sdNonHome()
    varHome = sdHome() * sdHome()
    varNonHome = sdNonHome() * sdNonHome()
    m <- c(uHome(), uNonHome())
    sigma <- matrix(c(varHome,covar,covar,varNonHome), nrow=2)
    
    
    UbAdjust = 1 - pnorm(0, mean = uHome(), sd = sdHome())
    JDFE = data.frame(mvrnorm(n = 100000, mu = m, Sigma = sigma))
    names(JDFE) = c('s.x', 's.y')
    homeSvals = JDFE$s.x *(JDFE$s.x>=0)
    r1 = 2*mean((homeSvals)^2)/UbAdjust 
    r2 = 2*mean((homeSvals)*(JDFE$s.y))/UbAdjust 
    
    xVals = 1:500
    dfout_1 = data.frame(x = xVals, y = xVals*r1, type = 'Home')
    dfout_2 = data.frame(x = xVals, y = xVals*r2, type = 'Non-Home')
    dfout = rbind(dfout_1,dfout_2)
    
    result = 'Pleiotropic Neutrality'
    colortext = 'darkgrey'
    if(r2>0.007)
    {
      result = 'Pleiotropic Fitness Gain'
      colortext =  'palegreen4'
    }else if(r2 < -0.007)
    {
      result = 'Pleiotropic Fitness Loss'
      colortext = 'darkred'
    }
    
    
    plot2 =  ggplot(dfout) +
      scale_color_manual(values = c('purple', 'orange'))+
      geom_hline(yintercept = 0, color = 'grey', linewidth = 1)+
      geom_line(aes(x, y, color = type), linewidth = 2)+
      xlim(xlimits_FitTraj())+
      ylim(ylimits_FitTraj())+
      xlab('Generations')+
      ylab('Fitness')+
      ggtitle("Fitness Trajectory", subtitle = result)+
      theme_classic()+
      theme(legend.position = 'right')+
      theme(axis.text = element_text(color = 'black', size = 11),
            axis.title = element_text(color = 'black', size = 12), 
            plot.title = element_text(size = 20, color = 'black') ,
            plot.subtitle = element_text(size = 12, color = colortext) )
    
    
    
    
    plot2
    
    
  }, res = 96, height = 600)
  
}

shinyApp(ui, server)
