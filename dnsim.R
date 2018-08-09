dnsim <- function(y, x, nsim=1000,showall= FALSE) 
  { ### Common part 
  nn <- length(y) 
  lm<-lm(y~x) 
  xd1<-qnorm((((nn+2):(2*nn+1))-0.375)/(2*nn+1.25)) 
  if(showall==TRUE){
    ## par(mfrow=c(1,3)) 
    ########### Internally Standardized residual 
    rs1<-sort(abs(stanres(lm))) 
    lc1<-rnorm(nn,mean=0,sd=1) 
    lm1<-lm(lc1~x) 
    rssim<-sort(abs(stanres(lm1))) 
    for(j in 2:nsim){ 
      lc1<-rnorm(nn,mean=0,sd=1) 
      lm1<-lm(lc1~x) 
      rssim<-c(rssim,sort(abs(stanres(lm1)))) 
      } 
    rssim<-matrix(rssim,ncol=nn,byrow=T)
    rs2<-rep(0,nn) 
    rs3<-rep(0,nn) 
    for(i in 1:nn){
      t1<-sort(rssim[,i])
      rs2[i]<-t1[0.05*nsim]
      rs3[i]<-t1[0.95*nsim] } 
    ymin<-min(c(rs1,rs2,rs3)) 
    ymax<-max(c(rs1,rs2,rs3)) 
    plot(xd1,rs1,xlab='Expected Value',ylab='Observed Value',
    main="Internally Standardized Residual",ylim=c(ymin,ymax),pch='o') 
    points(xd1,rs2,pch='-') 
    points(xd1,rs3,pch='-')
    
}

## par(mfrow=c(1,2))

########### Externally Studentized residual 
  rs1<-sort(abs(studres(lm))) 
  lc1<-rnorm(nn,mean=0,sd=1) 
  lm1<-lm(lc1~x) 
  rssim<-sort(abs(studres(lm1))) 
  for(j in 2:nsim){
    lc1<-rnorm(nn,mean=0,sd=1)
    lm1<-lm(lc1~x)
    rssim<-c(rssim,sort(abs(studres(lm1)))) } 
  rssim<-matrix(rssim,ncol=nn,byrow=T) 
  rs2<-rep(0,nn) 
  rs3<-rep(0,nn) 
  for(i in 1:nn){
  t1<-sort(rssim[,i])
  rs2[i]<-t1[0.05*nsim]
  rs3[i]<-t1[0.95*nsim] } 
  ymin<-min(c(rs1,rs2,rs3)) 
  ymax<-max(c(rs1,rs2,rs3)) 
  plot(xd1,rs1,xlab='Expected Value',ylab='Observed Value',main="Externally Studentized Residual",
       ylim=c(ymin,ymax),pch='o') 
  points(xd1,rs2,pch='-') 
  points(xd1,rs3,pch='-')
                                                                                   
  ########### DFFITS
  
  rs1<-sort(abs(dffits(lm))) 
  lc1<-rnorm(nn,mean=0,sd=1) 
  lm1<-lm(lc1~x) 
  rssim<-sort(abs(dffits(lm1))) 
  for(j in 2:nsim){
    lc1<-rnorm(nn,mean=0,sd=1)
    lm1<-lm(lc1~x)
    rssim<-c(rssim,sort(abs(dffits(lm1)))) 
    } 
    rssim<-matrix(rssim,ncol=nn,byrow=T) 
    rs2<-rep(0,nn) 
    rs3<-rep(0,nn) 
    for(i in 1:nn){
      t1<-sort(rssim[,i])
      rs2[i]<-t1[0.05*nsim]
      rs3[i]<-t1[0.95*nsim] 
      } 
    ymin<-min(c(rs1,rs2,rs3)) 
    ymax<-max(c(rs1,rs2,rs3)) 
    plot(xd1,rs1,xlab='Expected Value',ylab='Observed Value',
      main="DFFITS",ylim=c(ymin,ymax),pch='o') 
    points(xd1,rs2,pch='-') 
    points(xd1,rs3,pch='-') 
    return()
  
  }