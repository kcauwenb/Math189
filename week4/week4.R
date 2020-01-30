#function that will return the 5 things she asked for in a list, given a p
#usage example: we want p_hat for p=0.1
  #p0.1<-binom500(0.1)
  #p0.1$p_hat

binom500<-function(p){
  #simulate 500 bin(100,p)
  sims<-rbinom (500, 100, p)
  
  #avg. of estimated p’s
  p_hat<-mean(sims/100)
  
  #empirical variance of the p_hat’s
  #np(1-p)
  emp_var<-var(sims)
  
  #avg. estimated variances of the p_hat’s (TODO)
  avg_var<-0

  #proportion of 95% CI’s that contain true value of p (TODO)
  true_CI_prop<-0
  
  #avg. length of the 95% CI’s (TODO)
  avg_CI_length<-0
  
  return(list(p_hat=p_hat,emp_var=emp_var,
              avg_var=avg_var,true_CI_prop=true_CI_prop,
              avg_CI_length=avg_CI_length))
}

p0.1<-binom500(0.1)
p0.2<-binom500(0.2)
p0.3<-binom500(0.3)
p0.4<-binom500(0.4)
p0.5<-binom500(0.5)

