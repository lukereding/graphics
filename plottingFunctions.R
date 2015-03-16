cat("Plotting functions:\nstrip -- like a box and whisker plot, but show the data \nbar -- barplot, still needs some work\nscatter -- scatterplot\nmod -- modified box plot\nhistogram -- \n\nStats functions:\nmonte_unpaired -- monte carlo permutation test, two unpaired samples\nmonte_paired -- monte carlo permutation test, two paired samples\n")
library(magrittr)
# some attractive colors:
ruby <-rgb(202/255,53/255,7/255,1)
slate <- rgb(137/255,157/255,164/255,1)
mint <- rgb(73/255,191/255,150/255,1)

# check out the beeswarm package as an alternative to strip plots

#data should be a list of vectors you want to plot

# examples
# strip(list(rnorm(50,5),rnorm(20,10)),points=21)
# strip(list(rpois(50,5),rpois(20,3),rnorm(100,7)),type="ci",lab=c("a","b","c"),xlab="group",ylab="response")
# lots of groups:
#strip(list(rpois(50,5),rpois(20,3),rnorm(100,7),rnorm(45,2),rnorm(90,9)),type="ci",lab=c("a","b","c","d","e"),xlab="group",ylab="response")

strip<-function(data,lab=rep(c(),length(data)),type="se",jitter=T,points=16,xlab="",ymin="determine",ymax="determine",point_size=1.5,use_black=F,...){
  par(bty="l",lwd=1.7)
  
  # error checking:
  # if there are NAs in your dataset, throw an error
  if(length(Filter(is.na,data))>0){
    stop("there are NAs in your list")
  }
  
  number_groups<-length(data)
  factor_names<-lab
  
  # colors:
  if(use_black==F){
    cols<-c("#3EB489","#F21A00","#5B1A18","#E1AF00","#446455")
    cols_points<-c("#3EB48970","#F21A0070","#5B1A1870","#E1AF0070","#44645570")
  }
  else{
    cols<-rep("black",6)
    cols_points<-rep("#00000050",6)
  }
  
  if(ymin=="determine"&ymax=="determine"){
    maximum_value = -1000
    minimum_value = 1000
    for(i in 1:length(data)){
      if(max(data[[i]]>maximum_value)){
        maximum_value <- max(data[[i]])
      }
      if(min(data[[i]]<minimum_value)){
        minimum_value <- min(data[[i]])
      }
    }
  }
  
  else{
    maximum_value=ymax
    minimum_value=ymin
  }
  
  
  plot(c(0,1),c(minimum_value,maximum_value),type="n",xaxt="n",xlab=xlab,...)
  
  if(number_groups == 1){
    x_values = 0.5
    offset= 0.15
  }
  else{
    x_values<-seq(0.2,0.8,length.out=number_groups)
    # offset is the distance between the data points and their means
    offset <- 0.15 / number_groups
  }
  
  # use lapply to extract the means
  means<-lapply(data,mean)
  
  
  if(type=="se"){
    std_dev<-lapply(data,sd)
    for(i in 1:length(data)){
      std_error <- std_dev[[i]] / sqrt(length(data[[i]]))
      arrows(x_values[i]+offset,means[[i]]-std_error,x_values[i]+offset,means[[i]]+std_error,angle=90,code=3,length=0.07)
    }
  }
  
  if(type=="sd"){
    std_dev<-lapply(data,sd)
    for(i in 1:length(data)){
      arrows(x_values[i]+offset,means[[i]]-std_dev[[i]],x_values[i]+offset,means[[i]]+std_dev[[i]],angle=90,code=3,length=0.07)
    }
  }
  
  if(type=="ci"){
    std_dev<-lapply(data,sd)
    for(i in 1:length(data)){
      std_error <- std_dev[[i]] / sqrt(length(data[[i]]))
      arrows(x_values[i]+offset,means[[i]]-std_error*1.96,x_values[i]+offset,means[[i]]+std_error*1.96,angle=90,code=3,length=0.07)
    }
  }
  
  # plot the means
  for(i in 1:length(data)){
    points(x=x_values[i]+offset,y=means[[i]],cex=point_size*1.3,pch=16,col=cols[i])
  }
  
  
  axis(side=1,at=x_values,labels=lab)
  
  
  if(jitter==F){
    # now to draw the points
    for(i in 1:number_groups){
      points(x=rep(x_values[i],length(data[[i]])),y=data[[i]],pch=points,col=cols_points[i])
    }
  }
  
  else if(jitter==T){
    for(i in 1:number_groups){
      points(x=rep(x_values[i],length(data[[i]]))+rnorm(length(data[[i]]),0,0.02/(number_groups/2)),y=data[[i]],pch=points,col=cols_points[i],cex=point_size)
    }
  }
  par(bty="o",lwd=1)
}


#########################################################


# strip2 works fine, but it only accepts dataframes
# to increase flexibility, I've introduced strip(), which takes a list of vectors you want to plot
# it's a lot more flexible
strip2<-function(response,group,data=dat,lab=levels(group),type="se",jitter=T,points=16,...){
  par(bty="l",lwd=1.7)
  if(is.factor(group)!=TRUE){
    warning("'group' is not a factor: converting now")
    group<-factor(group)
  }
  
  number_groups<-nlevels(group)
  factor_names<-levels(group)
  boxplot_table<-boxplot(response~group,plot=F)
  
  # colors:
  cols<-c("#3B9AB2","#F21A00","#5B1A18","#E1AF00","#446455")
  cols_points<-c("#3B9AB270","#F21A0070","#5B1A1870","#E1AF0070","#44645570")
  
  plot(c(0,1),c(min(response),max(response)),type="n",xaxt="n",...)
  
  if(number_groups == 1){
    x_values = 0.5
    offset= 0.15
  }
  else{
    x_values<-seq(0.2,0.8,length.out=number_groups)
    # offset is the distance between the data points and their means
    offset <- 0.15 / number_groups
  }
  
  
  
  # use tapply to extract the means
  means<-tapply(response,group,mean)
  
  
  if(type=="se"){
    # now for +- one s.e.
    std_dev<-tapply(response,group,sd)
    std_error <- std_dev / sqrt(boxplot_table$n)
    # draw standard errors:
    for(i in 1:number_groups){
      arrows(x_values[i]+offset,means[i]-std_error[i],x_values[i]+offset,means[i]+std_error[i],angle=90,code=3,length=0.07)
    }
  }
  
  if(type=="sd"){
    # now for +- one s.e.
    std_dev<-tapply(response,group,sd)
    # draw standard deviations:
    for(i in 1:number_groups){
      arrows(x_values[i]+offset,means[i]-std_dev[i],x_values[i]+offset,means[i]+std_dev[i],angle=90,code=3,length=0.07)
    }
  }
  
  if(type=="ci"){
    std_dev<-tapply(response,group,sd)
    std_error <- std_dev / sqrt(boxplot_table$n)
    for(i in 1:number_groups){
      arrows(x_values[i]+offset,means[i]-(1.96*std_error[i]),x_values[i]+offset,means[i]+(1.96*std_error[i]),angle=90,code=3,length=0.07)
    }
  }
  
  if(type=="cred"){
    cred<-tapply(response,group,credible_intervals)
    print("error bars are credible intervals determined by 100,000 permutations")
    for(i in 1:number_groups){
      arrows(x_values[i]+offset,cred[[i]][[1]],x_values[i]+offset,cred[[i]][[2]],angle=90,code=3,length=0.07)
    }
  }
  
  # plot the means
  points(x=x_values+offset, y=means,cex=1.5,pch=16,col=cols)
  
  
  axis(side=1,at=x_values,labels=lab)
  
  
  if(jitter==F){
    # now to draw the points
    for(i in 1:number_groups){
      points(x=rep(x_values[i],boxplot_table$n[i]),y=response[group==factor_names[i]],pch=points,col=cols_points[i])
    }
  }
  
  else if(jitter==T){
    for(i in 1:number_groups){
      points(x=rep(x_values[i],boxplot_table$n[i])+rnorm(boxplot_table$n[i],0,0.02/(number_groups/2)),y=response[group==factor_names[i]],pch=points,col=cols_points[i])
    }
  }
  par(bty="o",lwd=1)
}


# for making credible intervals
# used in strip() above
credible_intervals<-function(x,n=100000){
  means<-vector(length=n)
  for(i in 1:n){
    means[i]<-mean(sample(x,size=length(x),replace=T))
  }
  #print("first result is 5th percentile, second is 95th")
  results<-quantile(means,c(0.05,0.95))
  print(results)
  #hist(means)
  return(results)
}

##############################################

# barplot

# sim is a list of vectors you want to plot
# lab is the labels for under each bar

# examples of usage:
# simplest:
# bar(list(rnorm(20,5),rnorm(15,5)),lab=c("A","B"),xlab="group",ylab="response",jitter=F)
# with conf. intervals:
# bar(list(rnorm(20,5),rnorm(15,5)),CI=T,lab=c("A","B"),xlab="group",ylab="response",jitter=F)
# nicer:
# bar(list(rnorm(20,5),rnorm(15,5)),CI=T,lab=c("A","B"),xlab="group",ylab="response")
# with more groups
# bar(list(rnorm(20,5),rnorm(15,5),rnorm(200,50),rnorm(50,1),rnorm(34,19)),CI=T)
bar<-function(sim,lab=rep(c(),length(sim)),CI=F,SE=F,bar_color="grey80",jitter=T,point_col="#00000080",...){
  par(lwd = 1,family = 'Helvetica')
  
  if(is.list(sim)==F){
    stop("the first argument needs to be a list of the vectors you want to plot")
  }
  
  means<-vector(length=length(sim))
  for(i in 1:length(sim)){
    means[i]<-mean(sim[[i]])
  }
  
  max_value = 0
  for(i in 1:length(sim)){
    if(max(sim[[i]]) >= max_value){
      max_value <- max(sim[[i]]) 
    }
  }
  
  min_value = 0
  for(i in 1:length(sim)){
    if(min(sim[[i]]) <= min_value){
      min_value <- min(sim[[i]]) 
    }
  }
  
  (p<-barplot(means,bty="l",space=0.4,ylim=c(floor(min_value),ceiling(max_value)+(0.1*max_value)),axes=F,col=bar_color,border=NA,...))
  axis(1,at=p,lwd=2,cex=1.5,labels=lab,tick=F)
  axis(2,cex=1.5,lwd=2)
  
  if(SE==T & CI ==T){
    stop("can't plot both a conf. interval and a standard error. choose only one.")
  }
  
  # for 95% CIs:
  # why doesn't this work?
  if(CI==T){
    for(i in 1:length(sim)){
      # find the stardard error
      se<-sd(sim[[i]])/sqrt(length(sim[[i]]))
      arrows(x0=p[i],y0=mean(sim[[i]])-(se*1.96),x1=p[i],y1=mean(sim[[i]])+(se*1.96),col="grey50",angle=90,code=3,lwd=1.5,length=.1)
      #lines(rep(p[i],2),y=c(mean(sim[[i]]),mean(sim[[i]])-(se*1.96)),lwd=5,col="white")
      #lines(rep(p[i],2),y=c(mean(sim[[i]]),mean(sim[[i]])+(se*1.96)),lwd=5)
    }
  }
  
  if(SE==T){
    for(i in 1:length(sim)){
      # find the stardard error
      se<-sd(sim[[i]])/sqrt(length(sim[[i]]))
      arrows(x0=p[i],y0=mean(sim[[i]])-se,x1=p[i],y1=mean(sim[[i]])+se,col="grey50",angle=90,code=3,lwd=1.5,length=.1)
      #lines(rep(p[i],2),y=c(mean(sim[[i]]),mean(sim[[i]])-se),lwd=5,col="white")
      #lines(rep(p[i],2),y=c(mean(sim[[i]]),mean(sim[[i]])+se),lwd=5)
    }
  }
  
  if(jitter==T){
    x_vals = p + 0.2
    for(i in 1:length(sim)){
      points(x=rep(x_vals[i],length(sim[[i]]))+rnorm(length(sim[[i]]),0,0.02),y=sim[[i]],pch=16,col=point_col)
    }
  }
  return(p)
}

########################################################
#scatter


# helper function that will draw line of best fit, but not extend beyond the reaches of the data
line<-function(x,y,color="black",...){
  slope <- summary(lm(y~x))$coefficients[2,1]
  intercept <- summary(lm(y~x))$coefficients[1,1]
  lines(x=c(min(x),max(x)),y=c(transform(min(x),m=slope,b=intercept),transform(max(x),m=slope,b=intercept)),col=color,...)
}

# returns y given x along a line
transform <- function(x,m,b){
  return((m*x)+b)
}


scatterOld<-function(x,y,xlab="",ylab="",line=T,sig=T,color="black",line_col="red",confidenceInterval=T,...){
  par(lwd=2,cex=1,bg="white")
  
  # error checking
  if(length(x)!=length(y)){
    stop("x and y lengths differ")
  }
  
  # do the actual plotting
  plot(x,y,xlab=xlab,ylab=ylab,pch=16,bty="n",col=color,...)
  
  p<-summary(lm(y~x))$coefficients[2,4]
  c<-summary(lm(y~x))$coefficients[2,1]
  
  # draw the line of best fit
  if(line==T){
    if(p<=0.05){
      line(x,y,lwd=2,color=line_col)
    }
    else{
      line(x,y,lwd=2,lty=2,color=line_col)
    }
  }
  
  # add statistical significance to the plot
  if(sig==T){
    legend(x="bottomright",bty="n",legend=paste("p = ",round(p,3)))
  }
  
  # adding confidence intervals around the regression line
  if(confidenceInterval==T){
    model<-lm(y~x)
    xVals<-seq(min(x),max(x),.1)
    conf<-predict(model,data.frame(x=xVals),interval="confidence",level=0.95)
    lines(xVals,conf[,2],col="#00000050",lty=2)
    lines(xVals,conf[,3],col="#00000050",lty=2)
  }
  
  # reset the par values
  par(lwd=1,cex=1,bg="white")
}





scatter<-function(x,y,xlab="",ylab="",line=T,sig=T,color="black",line_col="red",confidenceInterval=T,plottingCharacter=16,...){
  par(lwd=1,cex=1,bg="white")
  
  # error checking
  if(length(x)!=length(y)){
    stop("x and y lengths differ")
  }
  
  # do the actual plotting
  plot(x,y,xlab=xlab,ylab=ylab,pch=plottingCharacter,bty="o",col=color,...)
  
  p<-summary(lm(y~x))$coefficients[2,4]
  c<-summary(lm(y~x))$coefficients[2,1]
  
  # draw the line of best fit
  if(line==T){
    if(p<=0.05){
      line(x,y,lwd=2,color=line_col)
    }
    else{
      line(x,y,lwd=2,lty=2,color=line_col)
    }
  }
  
  # add statistical significance to the plot
  if(sig==T){
    legend(x="bottomright",bty="n",legend=paste("p = ",round(p,3)))
  }
  
  # adding confidence intervals around the regression line
  if(confidenceInterval==T){
    model<-lm(y~x)
    xVals<-seq(min(x),max(x),.1)
    conf<-predict(model,data.frame(x=xVals),interval="confidence",level=0.95)
    lines(xVals,conf[,2],col="#00000050",lty=2,lwd=2)
    lines(xVals,conf[,3],col="#00000050",lty=2,lwd=2)
  }
  
  # reset the par values
  par(lwd=1,cex=1,bg="white")
}





## modified boxplot
## example:
### x<-data.frame(c(rnorm(20,5),rnorm(30,10,1.5),rnorm(25,12,1.6),rnorm(20,20,3)),c(rep("A",20),rep("B",30),rep("C",25),rep("D",20)))
### mod(x[,1],x[,2])
mod<-function(response,group,lab=levels(group),...){
  
  # make sure group is a factor; if not, convert it to one
  if(is.factor(group)!=TRUE){
    warning("'group' is not a factor: converting now")
    group<-factor(group)
  }
  
  number_groups<-nlevels(group)
  
  # get statistics you need to plot
  boxplot_table<-boxplot(response~group,plot=F)
  
  # create the plot
  plot(c(0,1),c(min(response),max(response)),type="n",xaxt="n",...)
  x_values<-seq(0.7,0.3,length.out=number_groups)
  
  # plot the median
  points(x=x_values, y=boxplot_table$stats[3,1:number_groups],cex=1.1,pch=16,col="red")
  
  # creare x axis
  axis(side=1,at=x_values,labels=lab)
  
  # draw lines representing 1 and fourth quartiles
  for(i in 1:number_groups){
    lines(x=rep(x_values[i],2),y=c(boxplot_table$stats[1,i],boxplot_table$stats[2,i]),lwd=2)
    lines(x=rep(x_values[i],2),y=c(boxplot_table$stats[4,i],boxplot_table$stats[5,i]),lwd=2)
  }
}

# histogram
histogram=function(x,color="grey50",bor="grey50",...){
  hist(x,col=color,border=bor,lwd=2,cex.lab=1.2,...)
}


### some non-parametric stats functions
#############################################
### monte carlo, two-samples, unpaired #####
###########################################
# the idea here is to randomize associations of the data with the group
# then calculate the difference in the group means under the null hypothesis
# then compare it to the observed difference in means

## should do some more error checking to make sure this works, but looks good, 14 may 2014


monte_unpaired<-function(group_a,group_b,n=9999,null=0,table=TRUE){
  values<-vector(length=n)
  crit = mean(group_a)-mean(group_b)
  diff<-abs(crit-null)
  
  # make a data frame
  dat<-data.frame(c(group_a,group_b),c(rep("group_a",length(group_a)),rep("group_b",length(group_b))))
  names(dat)<-c("values","group")
  
  for(i in 1:n){
    dat$group<-sample(dat$group,replace=F)
    x<-tapply(dat$value,dat$group,mean)
    values[i]<-x[[1]]-x[[2]]
  }
  
  lower_bound <- quantile(values,0.025)[[1]]
  upper_bound <- quantile(values,0.975)[[1]]
  
  p <- ((length(values[values<=null-diff])+length(values[values>=null+diff]))+1)/(n+1)
  # why add the +1's? see Ruxton and Neuha¨user, methods in ecology and evolution, 2013, doi:10.1111/2041-210X.12102
  
  # plot
  par(lwd = 3,family = 'Helvetica',cex.lab=1.3,cex.lab=1.3)
  (plot<-hist(values,cex.lab=1.3,xlab="Simulated Differences",main="Monte Carlo Simulation",col="#99999940",breaks=20))
  text(mean(values),max(plot$counts),paste("observed = ",round(crit,3)),pos=4)
  segments(crit,0,crit,n,lty=2)
  segments(lower_bound,0,lower_bound,n,lty=3,lwd=2)
  segments(upper_bound,0,upper_bound,n,lty=3,lwd=2)
  
  if(table==TRUE){
    g<-c(round(lower_bound,2),round(upper_bound,2),round(p,3))
    h<-c("lower_bound","upper_bound","p-value")
    result<-data.frame(h,g)
    colnames(result)<-c(" "," ")
    print(paste("based on ",n+1," iterations"))
    # I construct a confidence interval for the p-value based on the same paper above:
    # Ruxton and Neuha¨user, methods in ecology and evolution, 2013, doi:10.1111/2041-210X.12102
    ci <- 1.96*sqrt((p*(1-p))/(n+1))
    print(paste("lower confidence level for p-value:",round(p-ci,3),". Upper confidence interval: ",round(p+ci,3)))
    print(result)
    #return(values)
  }
  else{
    print(paste("based on",n,"iterations"))
    return(p) 
  }
}


#######################################################
# monte carlo method for a two-sample paired t-test ##
#####################################################
# example of use:
#monte_paired(df,n=9999)

monte_paired<-function(data,n=9999,table=TRUE,diff_col=3,one_sided=F){
  x<-vector(length=n)
  if(!is.data.frame(data)){
    stop("please enter a data frame as the first argument")
  }
  
  if(one_sided==T){
    crit <- (mean(data[,diff_col]))
  }
  else{
    crit <- abs(mean(data[,diff_col]))
  }
  
  
  for(i in 1:n){
    data1<-data.frame(t(apply(df[,1:2],1,sample)))
    data1$diff<-data1[,1]-data1[,2]
    if(one_sided==T){
      x[i]<-(mean(data1$diff))
    }
    else{
      x[i]<-abs(mean(data1$diff))
    }
  }
  
  
  extreme <- length(x[x>=crit])+1
  p <- extreme/(n+1)
  # why add the +1's? see Ruxton and Neuha¨user, methods in ecology and evolution, 2013, doi:10.1111/2041-210X.12102
  par(lwd = 3,family = 'Helvetica',cex.lab=1.3,cex.lab=1.3)
  
  (plot<-hist(x,cex.lab=1.3,xlab="Simulated Differences",main="MC Simulation",col="#99999940"))
  text(mean(x),max(plot$counts),paste("observed = ",round(crit,1)),pos=4)
  segments(crit,0,crit,n,lty=2)
  
  if(table==TRUE){
    g<-c(crit,round(extreme,0),p)
    h<-c("observed diff","num_extreme","p-value")
    result<-data.frame(h,g)
    colnames(result)<-c(" "," ")
    print(result)
    #return(x)
  }
  else{
    return(x) 
  }
}




