###########################################################################
##R Shiny App to visualize sampling distributions
##Justin Post - Fall 2015 (updated 2017)
###########################################################################

library(shiny)
library(shinydashboard)

# Define UI for application that displays an about page and the app itself

dashboardPage(skin="red",
            
  #add title
  dashboardHeader(title="Sampling Distribution App",titleWidth=750),
              
  #define sidebar items
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("archive")),
    menuItem("Application", tabName = "app", icon = icon("laptop"))
  )),
              
  #define the body of the app
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "about",
        fluidRow(
          #add in latex functionality if needed
          withMathJax(),
                
          #two columns for each of the two items
          column(6,
            #Description of App
            h1("What does this app do?"),
            #box to contain description
            box(background="red",width=12,
              h4("This application visualizes the sampling distribution of different statistics when sampling from different populations."),
              h4("Statistics are random variables that have a distribution - called a sampling distribution.  These distributions are of the utmost importance as they allow us to find probabilities about the statistic.  The sampling distribution depends on the population from which it is sampled as well as the statistic you are investigating."),
              h4("To understand the behavior of statistics, this application generates many random samples of a chosen size, calculates the value of the statistic, and plots them on a histogram.  These data sets can be stepped through and the histogram serves as an estimate of the sampling distribution."),
              h4("This application allows the user to choose between many 'parent populations.'  These are the Binomial, Beta, Cauchy, Chi-Square, Exponential, Gamma, Geometric, Normal, Poisson, and Uniform distributions."),
              h4("The user can also choose from many different statistics.  These are the sample mean, standardized sample mean (subtracting the true mean and dividing by the true standard deviation), sample variance, sample standard deviation, sample median, sample max, and sample min.")
            )
          ),
                
          column(6,
            #How to use the app
            h1("How to use the app?"),
            #box to contain description
            box(background="red",width=12,
              h4("The controls for the app are located to the left and the visualization and information are available on the right."),
              h4("In the distribution box on the top left, the user can select the parent population from which the sample will be taken.  The parameters of the distribution can be changed from there as well.  This distribution is displayed in the top left graph."),
              h4("The statistic to calculate as well as the sample size can be changed from the menus on the middle left.  The histogram on the bottom left is the visualization of the sampling distribution of this statistic."),
              h4("In the bottom left, the user can control how many calculated values of the statistic the sampling distribution histogram shows.  By clicking the play button, you can visualize the creation of the sampling distribution.  By double clicking on the play button, you can increase by one data set at a time."),
              h4("The information on the bottom right of the graph displays the summary information for the sampling distribution, the particular sample, and the values of that sample."),
              h4("Once the process is understood, minimizing the top two histograms can improve the layout of the application.")
            )
          )
        )
      ),
      
      #actual app layout      
      tabItem(tabName = "app",
        fluidRow(
          column(2,
            box(width=12,background="red",
              selectizeInput("dist",label=h3("Distribution"),selected="Uniform",choices=sort(c("Uniform","Normal","Gamma","Exponential","Chi-Square","Beta","Binomial","Geometric","Poisson","Cauchy"))),
              numericInput("param1","Lower Limit",value=0,step=1),
              conditionalPanel(condition= "(input.dist=='Uniform')||(input.dist=='Normal')||(input.dist=='Gamma')||(input.dist=='Beta')||(input.dist=='Cauchy')",numericInput("param2","Upper Limit",value=1,step=1))
            ),
            box(width=12,background="red",
              selectizeInput("stat",label=h3("Statistic"),selected="Sample Mean",choices=c("Sample Mean","Standardized Sample Mean","Sample Variance","Sample Standard Deviation","Sample Median","Sample Max","Sample Min")),
              numericInput("n",label=h4("Sample Size (2 if blank)"),value=10,min=2,step=1)
            ),
            sliderInput("N", label=h4("Number of Data Sets:"),min = 1, max = 10000, value = 1, step = 1,animate=list(TRUE, interval=350,loop=TRUE)),
            numericInput("rows",label=h4("Number of Rows of Data to Display"),value=5,step=1,min=1,max=25)
          ),

          column(10,
            fluidRow(
              box(width=6,title="Parent Population Distr.",collapsible = TRUE,
                #Show a plot of the parent population  
                plotOutput("parentPlot")
              ),
              box(width=6,title="Histogram of Sample",collapsible = TRUE,
              # Show a plot of the sampling distribution
                plotOutput("samplePlot")
              )
            ),
            br(),
            fluidRow(
              box(width=6,title="Histogram of Sampling Distr.",collapsible=TRUE,
                plotOutput("statPlot")
              ),
              box(width=2,title="Summary of Sampling Distr.",collapsible=TRUE,
                background = "red",
                tableOutput("sampStatTable")
              ),
              box(width=2,title="Summary of Sample",collapsible=TRUE,
                background = "red",
                tableOutput("statTable")
              ),
              box(width=2,title="Data for sample",collapsible=TRUE,
                collapsed=TRUE,background = "red",
                tableOutput("dataTable")   
              )
            )
          )
        )
      )
    )
  )
)

