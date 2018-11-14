library("quantmod") 
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

#Get returns
aapl=getSymbols("AAPL",from="2012-01-01",auto.assign=FALSE) 
jnj=getSymbols("JNJ",from="2012-01-01",auto.assign=FALSE) 
prpfx=getSymbols("PRPFX",from="2012-01-01",auto.assign=FALSE) 
aapl.ret=monthlyReturn(Ad(aapl))
jnj.ret=monthlyReturn(Ad(jnj)) 
prpfx.ret=monthlyReturn(Ad(prpfx))


#Some Initilization
nsims=10000 
sr.boot.aapl=sr.boot.jnj=sr.boot.prpfx=rep(NA,nsims) 

#and the resampling done 10000 times 
for(i in 1:nsims){ 
  aapl.sample=sample(aapl.ret,length(aapl.ret)) 
  jnj.sample=sample(jnj.ret,length(jnj.ret)) 
  prpfx.sample=sample(prpfx.ret,length(prpfx.ret)) 
  sr.boot.aapl[i]=Omega(aapl.sample)
  sr.boot.jnj[i]=Omega(jnj.sample)
  sr.boot.prpfx[i]=Omega(prpfx.sample) 
}

#and check out the results

sr.boot.aapl.o=sr.boot.aapl[order(sr.boot.aapl)]
aapl.ci=quantile(sr.boot.aapl,probs=c(.025,.975)
                 
sr.boot.jnj.o=sr.boot.jnj[order(sr.boot.jnj)] 
jnj.ci=quantile(sr.boot.jnj,probs=c(.025,.975)
                                 
sr.boot.prpfx.o=sr.boot.prpfx[order(sr.boot.prpfx)] 
prpfx.ci=quantile(sr.boot.prpfx,probs=c(.025,.975)
                                                   
aapl.ci
jnj.ci
prpfx.ci
                                                   