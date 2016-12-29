
ChanceOfTerminal<- function(c,n1,n2,px,py,type){  
  

RejectH0<- function(c,n1,n2,px,py,type){
  
  x<- rbinom(n1*n2,1,px)
  y<- rbinom(n1*n2,1,py)
  r<- matrix(0,nrow=n2,ncol=1)
  
  for (i in 1:n2) {
    
    n<- n1*i
    
    MeanX<- mean(x[1:n])
    MeanY<- mean(y[1:n])
    Theta<- abs(MeanX-MeanY)
    V<- ((MeanX*(1-MeanX))+MeanY*(1-MeanY))/n #前i天的theta和variance
    
    if (type==1) {
      k<- 1.96*sqrt(V)
      r[i,1] <- ifelse(Theta > k, 1, 0) #判断前i天是否有拒绝零假设的情况出现
      
    } else if (type==2) { 
      k<- 1.64- (abs(Theta)/sqrt(V) )
      pk<- pnorm(k)
      r[i,1]<- ifelse(1-pk>=0.8,1,0)  #判断前i天是否有足够的power下结论
      
    } else if (type==3) {
      a<- 0.05
      t<- 0.01
      k<- sqrt( (2*log(1/a)-log(V/(V+t)))*( V*(V+t)/t ))
      r[i,1] <- ifelse(Theta > k, 1, 0) #判断前i天是否检测到效果，可以停止实验
      
    } else (k<- 4)
    
  }
  if(sum(r[,1])>0&&k<=3) {
    return (1)}  else if(sum(r[,1])<=0&&k<=3) {
      return (0)
    } else {
      print("Invalid Type Parameter!!!")
    }
  
  #只要前i天出现了一次拒绝零假设的情况，记为1
  
}

#Chance of reject H0
  r<- sapply(1:c, FUN=RejectH0,n1,n2,px,py,type)
  sum(r)/c
}

