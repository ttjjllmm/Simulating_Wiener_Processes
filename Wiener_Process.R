# Project exercises in Quantitative Finance course
# Simulating paths of a Wiener Process with an own function in R

# WP is short for Wiener Process
WP<-function(TT,dt,RR) # TT= end value, dt= step sizes, RR= replicates
{
  tt<-seq(from=0,to=TT,by=dt) #Time sequence
  result<-NULL 
  for(rr in 1:RR) # Simulating a path for each replicate
  {
    zz<-cumsum(c(0,sqrt(dt)*rnorm(n=length(tt)-1))) # The Wiener process function
    temp<-data.frame(time=tt,value=zz,rep=paste(rr)) 
    result<-rbind(result,temp) 
  } 
  return(result)
}

# Plotting
output <- WP(TT=1.5,dt=0.001,RR=100) # output is a dataframe with 150100 rows and 3 columns
gg<-ggplot(data=output,aes(x=time,y=value,col=rep))+geom_line()
gg<-gg+theme(legend.position='none')
print(gg)
