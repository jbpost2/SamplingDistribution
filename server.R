library(shiny)

#number of data sets to create
NumData<-10000

# Define server logic required to draw the plots
shinyServer(function(input, output,session) {
  
  #generate data
  simData<-reactive({
    #get reactive things and inputs
    input$dist
    input$n
    input$param1
    input$param2
        
    #sample size
    n<-input$n
    #Num samples
    dist<-input$dist
        
    if (is.na(n)){n<-2}
    
    #get samples from appropriate distribution 
    if (dist=="Uniform"){
      min<-input$param1
      max<-input$param2
      samples<-matrix(runif(n*NumData,min=min,max=max),nrow=NumData,ncol=n)
    } else if (dist=="Normal") {
      mean<-input$param1
      sd<-input$param2
      samples<-matrix(rnorm(n*NumData,mean=mean,sd=sd),nrow=NumData,ncol=n)
    } else if (dist=="Gamma") {
      alpha<-input$param1
      lambda<-input$param2
      samples<-matrix(rgamma(n*NumData,shape=alpha,rate=lambda),nrow=NumData,ncol=n)
    } else if (dist=="Exponential") {
      lambda<-input$param1
      samples<-matrix(rexp(n*NumData,rate=lambda),nrow=NumData,ncol=n)
    } else if (dist=="Chi-Square") {
      df<-input$param1
      samples<-matrix(rchisq(n*NumData,df=df),nrow=NumData,ncol=n)
    } else if (dist=="Beta") {
      alpha<-input$param1
      beta<-input$param2
      samples<-matrix(rbeta(n*NumData,shape1=alpha,shape2=beta),nrow=NumData,ncol=n)
    } else if (dist=="Binomial") {
      prob<-input$param1
      samples<-matrix(rbinom(n*NumData,size=1,prob=prob),nrow=NumData,ncol=n)
    } else if (dist=="Geometric") {
      prob<-input$param1
      samples<-matrix(rgeom(n*NumData,prob=prob),nrow=NumData,ncol=n)
    } else if (dist=="Poisson") {
      lambda<-input$param1
      samples<-matrix(rpois(n*NumData,lambda=lambda),nrow=NumData,ncol=n)
    } else if (dist=="Cauchy") {
      location<-input$param1
      scale<-input$param2
      samples<-matrix(rcauchy(n*NumData,location=location,scale=scale),nrow=NumData,ncol=n)
    }
    
    #return data
    samples
  })

 
  #update parameters in ui  
  observe({
    #get input
    dist <- input$dist
        
    #update the values of the parameters in the ui based on the distribution
    if (dist=="Uniform"){
      updateNumericInput(session, "param1", label="Lower Limit",value = 0,step=1)
      updateNumericInput(session, "param2", label="Upper Limit",value = 1,step=1)
    } else if (dist=="Normal") {
      updateNumericInput(session, "param1", label="Mean",value = 0,step=1)
      updateNumericInput(session, "param2", label="Standard Deviation",value = 1,min=0,step=1)
    } else if (dist=="Gamma") {
      updateNumericInput(session, "param1", label="Alpha (Shape)",value = 1,min=0,step=1)
      updateNumericInput(session, "param2", label="Lambda (Rate)",value = 1,min=0,step=1)
    } else if (dist=="Exponential") {
      updateNumericInput(session, "param1", label="Lambda (Rate)",value = 1,min=0,step=1)
      updateNumericInput(session, "param2", label="Not Applicable",value = 0,min=0,max=0)
    } else if (dist=="Chi-Square") {
      updateNumericInput(session, "param1", label="DF",value = 1,min=0,step=1)
      updateNumericInput(session, "param2", label="Not Applicable",value = 0,min=0,max=0)
    } else if (dist=="Beta") {
      updateNumericInput(session, "param1", label="alpha",value = 1,min=0,step=1)
      updateNumericInput(session, "param2", label="beta",value = 1,min=0,step=1)
    } else if (dist=="Binomial") {
      updateNumericInput(session, "param1", label="Probability of Success",value = 0.5,min=0,max=1,step=0.05)
      updateNumericInput(session, "param2", label="Not Applicable",value = 0,min=0,max=0)
    } else if (dist=="Geometric") {
      updateNumericInput(session, "param1", label="Probability of Success",value = 0.5,min=0,max=1,step=0.05)
      updateNumericInput(session, "param2", label="Not Applicable",value = 0,min=0,max=0)
    } else if (dist=="Poisson") {
      updateNumericInput(session, "param1", label="Lambda (Mean)",value = 1,min=0,step=1)
      updateNumericInput(session, "param2", label="Not Applicable",value = 0,min=0,max=0)
    } else if (dist=="Cauchy") {
      updateNumericInput(session, "param1", label="Location",value = 0,step=1)
      updateNumericInput(session, "param2", label="Scale",value = 1,min=0,step=1)
    }  

  })

  
  #create parent population plot
  output$parentPlot<-renderPlot({
    #input distribution
    dist<-input$dist
    #sample size
    n<-input$n
    
    #choose appropriate distribution  
    if (dist=="Uniform"){
      #parameters
      min<-input$param1
      max<-input$param2
      #Plotting sequence
      x <- seq(from=min,to=max,length=2)
      #draw the parent distribution plot
      plot(x=x,y=dunif(x=x,min=min,max=max),main=paste(dist," Density"),
           xlab="y", ylab="f(y)",type="l")
    } else if (dist=="Normal") {
      #parameters
      mean<-input$param1
      sd<-input$param2
      #Plotting sequence
      x <- seq(from=mean-4*sd,to=mean+4*sd,length=1000)
      #draw the parent distribution plot
      plot(x=x,y=dnorm(x=x,mean=mean,sd=sd),main=paste(dist," Density"),
           xlab="y", ylab="f(y)",type="l")
    } else if (dist=="Gamma") {
      #parameters
      alpha<-input$param1
      lambda<-input$param2
      #Plotting sequence
      x <- seq(from=0,to=alpha/lambda+4*sqrt(alpha/lambda^2),length=1000)
      #draw the parent distribution plot
      plot(x=x,y=dgamma(x=x,shape=alpha,rate=lambda),main=paste(dist," Density"), xlab="y", ylab="f(y)",type="l")
    } else if (dist=="Exponential") {
      #parameters
      lambda<-input$param1
      #Plotting sequence
      x <- seq(from=0,to=5/lambda,length=1000)
      #draw the parent distribution plot
      plot(x=x,y=dexp(x=x,rate=lambda),main=paste(dist," Density"),xlab="y", ylab="f(y)",type="l")
    } else if (dist=="Chi-Square") {
      #parameters
      df<-input$param1
      #Plotting sequence
      x <- seq(from=0,to=df+4*sqrt(2*df),length=1000)
      #draw the parent distribution plot
      plot(x=x,y=dchisq(x=x,df=df),main=paste(dist," Density"), xlab="y", ylab="f(y)",type="l")
    } else if (dist=="Beta") {
      #Plotting sequence
      x <- seq(from=0,to=1,length=1000)
      #parameters
      alpha<-input$param1
      beta<-input$param2
      #draw the parent distribution plot
      plot(x=x,y=dbeta(x=x,shape1=alpha,shape2=beta),main=paste(dist," Density"), xlab="y", ylab="f(y)",type="l")
    } else if (dist=="Binomial") {
      #Plotting sequence
      x <- seq(from=0,to=n,by=1)
      #parameters
      prob<-input$param1
      #draw the parent distribution plot
      plot(x=x,y=dbinom(x=x,size=n,prob=prob),main=paste(dist," Density"), xlab="y", ylab="f(y)",type="h")
    } else if (dist=="Geometric") {
      #parameters
      prob<-input$param1
      #Plotting sequence
      x<-seq(from=0,to=1/prob+4*sqrt((1-prob)/prob^2)+2,by=1)
      #draw the parent distribution plot
      plot(x=x,y=dgeom(x=x,prob=prob),main=paste(dist," Density"),xlab="y", ylab="f(y)",type="h")
    } else if (dist=="Poisson") {
      #parameters
      lambda<-input$param1
      #Plotting sequence
      x <- seq(from=0,to=lambda+4*sqrt(lambda),by=1)
      #draw the parent distribution plot
      plot(x=x,y=dpois(x=x,lambda=lambda),main=paste(dist," Density"), xlab="y", ylab="f(y)",type="h")
    } else if (dist=="Cauchy") {
      #Plotting sequence
      #parameters
      location<-input$param1
      scale<-input$param2
      x <- seq(from=location-6*scale,to=location+6*scale,length=1000)
      #draw the parent distribution plot
      plot(x=x,y=dcauchy(x=x,location=location,scale=scale),main=paste(dist," Density"), xlab="y", ylab="f(y)",type="l")
    }
    
  })
  
  
  #get plot of sample data for current data set
  output$samplePlot<-renderPlot({
    #input data set number
    N<-input$N
    #sample size
    n<-input$n
    #Get data for data set N
    samples<-simData()
    samples<-samples[N,]

    #Get statistic being used
    statistic<-input$stat

    #create histogram and save it, need to find where stat lies so we can color it
    temp<-hist(samples,main=paste("Histgram of Sample ",N,"'s data"),xlab="Data Values, y",prob=TRUE)    

    #calculate value of statistic
    if ((statistic=="Sample Mean")||(statistic=="Standardized Sample Mean")){
      statval<-mean(samples)
    } else if (statistic=="Sample Variance"){
      statval<-var(samples)
    } else if (statistic=="Sample Standard Deviation"){
      statval<-sd(samples)
    } else if (statistic=="Sample Median"){
      statval<-median(samples)
    } else if (statistic=="Sample Max"){
      statval<-max(samples)
    } else if (statistic=="Sample Min"){
      statval<-min(samples)
    }
        
    #get the break from the histogram where the stat occurs
    index<-which((temp$breaks[1:(length(temp$breaks)-1)]<statval)*(statval<temp$breaks[2:length(temp$breaks)])==1)
    #create the color vector to color the histogram
    clr<-rep("white",length(temp$breaks))
    if ((statistic=="Sample Mean")||(statistic=="Standardized Sample Mean")||(statistic=="Sample Median")||(statistic=="Sample Max")||(statistic=="Sample Min")){
      clr[index]<-"blue"
    }
      #create histogram
      hist(samples,main=paste("Histgram of Sample ",N,"'s data"),xlab="Data Values, y",prob=TRUE,col=clr)    
      
  })

    
  #get histogram of statistic based on data sets so far
  output$statPlot <- renderPlot({
    #sample size
    n<-input$n
    #Num samples
    N<-input$N
    #distribution
    dist<-input$dist
    #stat
    statistic<-input$stat
    
    #figure out what mean and standard deviation of parent are for z-score
    if (dist=="Uniform"){
      min<-input$param1
      max<-input$param2
      meanz<-(min+max)/2
      varz<-(max-min)^2/12
    } else if (dist=="Normal") {
      mean<-input$param1
      sd<-input$param2
      meanz<-mean
      varz<-sd^2
    } else if (dist=="Gamma") {
      alpha<-input$param1
      lambda<-input$param2
      meanz<-alpha/lambda
      varz<-alpha/lambda^2
    } else if (dist=="Exponential") {
      lambda<-input$param1
      meanz<-1/lambda
      varz<-1/lambda^2
    } else if (dist=="Chi-Square") {
      df<-input$param1
      meanz<-df
      varz<-2*df
    } else if (dist=="Beta") {
      alpha<-input$param1
      beta<-input$param2
      meanz<-alpha/(alpha+beta)
      varz<-(alpha*beta)/((alpha+beta)^2*(alpha+beta+1))
    } else if (dist=="Binomial") {
      prob<-input$param1
      meanz<-n*prob
      varz<-n*prob*(1-prob)
    } else if (dist=="Geometric") {
      prob<-input$param1
      meanz<-1/prob
      varz<-(1-prob)/prob^2
    } else if (dist=="Poisson") {
      lambda<-input$param1
      meanz<-varz<-lambda
    } else if (dist=="Cauchy") {
      location<-input$param1
      scale<-input$param2
      meanz<-location
      varz<-scale
    }
    
    #get values of the stat for each sample
    samples<-simData()
    if(N==1){
      if (statistic=="Sample Mean"){
        statvals<-mean(samples[1,])
      } else if (statistic=="Standardized Sample Mean"){
        statvals<-(mean(samples[1,])-meanz)/sqrt(varz/n)
      } else if (statistic=="Sample Variance"){
        statvals<-var(samples[1,])
      } else if (statistic=="Sample Standard Deviation"){
        statvals<-sd(samples[1,])
      } else if (statistic=="Sample Median"){
        statvals<-median(samples[1,])
      } else if (statistic=="Sample Max"){
        statvals<-max(samples[1,])
      } else if (statistic=="Sample Min"){
        statvals<-min(samples[1,])
      }
    } else if (N>1){
      if (statistic=="Sample Mean"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=mean)
      } else if (statistic=="Standardized Sample Mean"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=function(x,meanz,varz){(mean(x)-meanz)/sqrt(varz/n)}, meanz,varz)
      } else if (statistic=="Sample Variance"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=var)
      } else if (statistic=="Sample Standard Deviation"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=sd)
      } else if (statistic=="Sample Median"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=median)
      } else if (statistic=="Sample Max"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=max)
      } else if (statistic=="Sample Min"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=min)
      }
    }

    #get the break from the histogram where the stat for data set N occurs
    temp<-hist(statvals,main=paste("Sampling Distribution of the",statistic),prob=TRUE,xlab="Observed Values") 
    index<-which((temp$breaks[1:(length(temp$breaks)-1)]<statvals[N])*(statvals[N]<temp$breaks[2:length(temp$breaks)])==1)
    #create the color vector to color the histogram
    clr<-rep("white",length(temp$breaks))
    clr[index]<-"blue"
    
    # draw the sampling distribution
    if ((dist=="Cauchy")&&(N>1)&&(N<2000)){
      hist(statvals,main=paste("Sampling Distribution of the",statistic),prob=TRUE,xlab="Observed Values",breaks=100,col=clr) 
    } else {
      hist(statvals,main=paste("Sampling Distribution of the",statistic),prob=TRUE,xlab="Observed Values",col=clr) 
    }

  })

  
  #get table of data
  output$dataTable<-renderTable({
    #input # of rows to display
    rows<-input$rows
    #sample size
    n<-input$n
    rows<-min(n,rows)
    #Number of data set
    N<-input$N
    #Get data for data set N
    samples<-simData()
    datadisplay<-data.frame(Obs=1:rows,DataValue=sprintf("%.4f",samples[N,1:rows]))
    datadisplay
  },include.colnames=FALSE)

  
  #get summary stats for sample
  output$statTable<-renderTable({
    #Number of data set
    N<-input$N
    #Get data for data set N
    samples<-simData()
    samples<-samples[N,]
    
    #find stats
    statdisplay<-data.frame(Stats=c("Sample Mean","Sample Var","Sample SD","Sample Min","Sample Median","Sample Max"), Value=c(sprintf("%.4f",mean(samples)),sprintf("%.4f",var(samples)),sprintf("%.4f",sd(samples)), sprintf("%.4f",min(samples)),sprintf("%.4f",median(samples)),sprintf("%.4f",max(samples))))

    statdisplay
  },include.colnames=FALSE)
    
  
  #get statistics about the sampling distribution  
  output$sampStatTable<-renderTable({
    #sample size
    n<-input$n
    #Num samples
    N<-input$N
    #distribution
    dist<-input$dist
    #stat
    statistic<-input$stat
    
    #figure out what mean and standard deviation of parent are for z-score
    if (dist=="Uniform"){
      min<-input$param1
      max<-input$param2
      meanz<-(min+max)/2
      varz<-(max-min)^2/12
    } else if (dist=="Normal") {
      mean<-input$param1
      sd<-input$param2
      meanz<-mean
      varz<-sd^2
    } else if (dist=="Gamma") {
      alpha<-input$param1
      lambda<-input$param2
      meanz<-alpha/lambda
      varz<-alpha/lambda^2
    } else if (dist=="Exponential") {
      lambda<-input$param1
      meanz<-1/lambda
      varz<-1/lambda^2
    } else if (dist=="Chi-Square") {
      df<-input$param1
      meanz<-df
      varz<-2*df
    } else if (dist=="Beta") {
      alpha<-input$param1
      beta<-input$param2
      meanz<-alpha/(alpha+beta)
      varz<-(alpha*beta)/((alpha+beta)^2*(alpha+beta+1))
    } else if (dist=="Binomial") {
      prob<-input$param1
      meanz<-n*prob
      varz<-n*prob*(1-prob)
    } else if (dist=="Geometric") {
      prob<-input$param1
      meanz<-1/prob
      varz<-(1-prob)/prob^2
    } else if (dist=="Poisson") {
      lambda<-input$param1
      meanz<-varz<-lambda
    } else if (dist=="Cauchy") {
      location<-input$param1
      scale<-input$param2
      meanz<-location
      varz<-scale
    }
      
    #get values of the stat for each sample
    samples<-simData()
    if(N==1){
      if (statistic=="Sample Mean"){
        statvals<-mean(samples[1,])
      }else if (statistic=="Standardized Sample Mean"){
        statvals<-(mean(samples[1,])-meanz)/sqrt(varz)
      } else if (statistic=="Sample Variance"){
        statvals<-var(samples[1,])
      } else if (statistic=="Sample Standard Deviation"){
        statvals<-sd(samples[1,])
      } else if (statistic=="Sample Median"){
        statvals<-median(samples[1,])
      } else if (statistic=="Sample Max"){
        statvals<-max(samples[1,])
      } else if (statistic=="Sample Min"){
        statvals<-min(samples[1,])
      }
    } else if (N>1){
      if (statistic=="Sample Mean"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=mean)
      } else if (statistic=="Standardized Sample Mean"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=function(x,meanz,varz){(mean(x)-meanz)/sqrt(varz/n)}, meanz,varz)
      } else if (statistic=="Sample Variance"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=var)
      } else if (statistic=="Sample Standard Deviation"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=sd)
      } else if (statistic=="Sample Median"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=median)
      } else if (statistic=="Sample Max"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=max)
      } else if (statistic=="Sample Min"){
        statvals<-apply(X=samples[1:N,],MARGIN=1,FUN=min)
      }
    }
    sampstatdisplay<-data.frame(Stats=c("Sample Mean","Sample Var","Sample SD","Sample Min","Sample Median","Sample Max"),Value=c(sprintf("%.4f",mean(statvals)),sprintf("%.4f",var(statvals)),sprintf("%.4f",sd(statvals)),sprintf("%.4f",min(statvals)),sprintf("%.4f",median(statvals)),sprintf("%.4f",max(statvals))))
    
    sampstatdisplay
  },include.colnames=FALSE)
    
})    
