#sem-lab

print(sqrt(99))

data=rnorm(100)
boxplot(data,main="BOXPLOT")

print(log10(10))

print(abs(-15))

marks<-c(1,2,3)
s=c(11,22,33)
pie(marks,labels=s)

print(ceiling(7.5))
print(tanh(3))

x=runif(50,1,10)
yx=runif(50,1,10)
plot(x,yx,pch=20)

f<-function(x) x^2
print(integrate(f,0,5))

x<-seq(-5,5,by=0.1)
y=x^2
plot(x,y,type='l')

n<-19 
p<-0.1235 
x<-0:n 
pdf<-dbinom(x,size=n,prob=p) 
barplot(pdf,names.arg=x,) 
cdf<-pbinom(x,size=n,prob=p) 
plot(x,cdf,type="s")

#---------------

a<-c(1,2,3)
b<-c(2,3,4)
f<-var.test(a,b)
print(f)
if(f$p.value<0.05){
  cat("reject h0")
} else{
  cat("accept")
}

X<-matrix(c(10,20,30,40),byrow=TRUE,nrow=2)
c<-chisq.test(X)
print(c)
print(f)
if(c$p.value<0.05){
  cat("reject h0")
} else{
  cat("accept")
}


o<-c(1,2,3,4,5)
e<-c(2,1,3,4,5)
ch<-chisq.test(o,p=e/sum(e))
print(ch)
if(ch$p.value<0.05){
  cat("reject h0")
} else{
  cat("accept")
}

#------------------------

library(markovchain)
t<-matrix(c(0.1,0.2,0.7,
            0.4,0.5,0.1,
            0.7,0.1,0.2),
          nrow=3,
          byrow=TRUE,
          dimnames = list(c("G","D","R"),
                          c("G","D","R"))
)
chain<-new("markovchain",states=c("G","D","R"),transitionMatrix=t)
print(chain)
ss<-steadyStates(chain)
print(ss)
set.seed(123)
sim<-rmarkovchain(n=20,object=chain,t0="D")
print(sim)


#----------------------

library(queueing)

mm1_in<-NewInput.MM1(lambda=4,mu=6)
mm1_model<-QueueingModel(mm1_in)
summary(mm1_model)

mmc_in=NewInput.MMC(lambda=10,mu=5,c=3)
mmc_model<-QueueingModel(mmc_in)
summary(mmc_model)

mm1k_in<-NewInput.MM1K(lambda = 8,mu=12,k=5)
mm1k_model<-QueueingModel(mm1k_in)
summary(mm1k_model)

mmck_in<-NewInput.MMCK(lambda=4,mu=5,c=2,k=4)
mmck_model<-QueueingModel(mmck_in)
summary(mmck_model)


#-------------------------

s0<-100
mu<-0.001
sigma<-0.02
T<-252
n_sims=1000
simulate_stock<-function(s0,mu,sigma,T){
  prices<-numeric(T)
  prices<-(s0)
  for(t in 2:T){
    prices[t]<-prices[t-1]*exp(rnorm(1,mean=mu,sd=sigma))
  }
  return(prices)
}
prices<-simulate_stock(s0,mu,sigma,T)
plot(prices,type='l')
simulate_multiple<-function(s0,mu,sigma,T,n_paths){
  paths<-matrix(NA,nrow=T,ncol=n_paths)
  for(i in 1:n_paths){
    paths[,i]<-simulate_stock(s0,mu,sigma,T)
  }
  return(paths)
}
n_paths<-5
paths<-simulate_multiple(s0,mu,sigma,T,n_paths)
matplot(paths,type="l",col=1:n_paths,lty=1)
legend("topright",legend = paste("Path",1:n_paths),col=1:n_paths,lty=1)
