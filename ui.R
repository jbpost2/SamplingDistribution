###########################################################################
##R Shiny App to visualize sampling distributions
##Justin Post - Fall 2015
###########################################################################

#Load package
library(shiny)


# Define UI for application that draws the prior and posterior distributions given a value of y
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sampling Distribution App"),
  
  # Sidebar with a slider input for the number of successes
  fluidRow(
    column(3,br(),
      selectizeInput("dist",label=h3("Distribution"),selected="Uniform",choices=sort(c("Uniform","Normal","Gamma","Exponential","Chi-Square","Beta","Binomial","Geometric","Poisson","Cauchy"))),
      numericInput("param1","Lower Limit",value=0,step=1),
      conditionalPanel(condition= "(input.dist=='Uniform')||(input.dist=='Normal')||(input.dist=='Gamma')||(input.dist=='Beta')||(input.dist=='Cauchy')",
      numericInput("param2","Upper Limit",value=1,step=1)),
      br(),
      selectizeInput("stat",label=h3("Statistic"),selected="Sample Mean",choices=c("Sample Mean","Standardized Sample Mean","Sample Variance","Sample Standard Deviation","Sample Median","Sample Max","Sample Min")),
      sliderInput("N", label=h4("Number of Data Sets:"), 
                  min = 1, max = 10000, value = 1, step = 1,
                  animate=list(TRUE, interval=350,loop=TRUE)),br(),
      numericInput("n",
                   label=h4("Sample Size (2 if blank)"),value=10,min=2,step=1),
      numericInput("rows",label=h4("Number of Rows of Data to Display"),value=5,step=1,min=1,max=25)
    ),

    #Show a plot of the parent population  
    column(9,
           fluidRow(
                column(5,
                     plotOutput("parentPlot")
                       ),
                # Show a plot of the sampling distribution
                column(7,
                    plotOutput("samplePlot")
                )
           ),br(),
            fluidRow(
                column(7,
                    plotOutput("statPlot")
                ),
                column(2,h5("Summary Stats for the Sampling Dist"),
                       tableOutput("sampStatTable")
                ),
                column(2,h5("Summary Stats for the Sample"),
                       tableOutput("statTable")
                ),
                column(1,h5("Data for sample"),
                       tableOutput("dataTable")   
                )
            )
    )
)))

