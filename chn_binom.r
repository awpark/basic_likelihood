
pvec<-NULL

for (rep in 1:1000){
# chain binomial model for outbreak
tmp.s<-99
tmp.i<-1
tmp.r<-0
results<-as.data.frame(matrix(c(0,tmp.s,tmp.i,tmp.r),nrow=1))
names(results)<-c("t","s","i","r")
p=0.3

for (j in 1:9){ # 10 timesteps total (IC+9)
new.i<-rbinom(1,tmp.s,(1-(1-p)^tmp.i))
tmp.r<-tmp.r+tmp.i
tmp.s<-tmp.s-new.i
tmp.i<-new.i
results<-rbind(results,c(j,tmp.s,tmp.i,tmp.r))  
}

lvec<-NULL
for (pp in seq(0,1,0.01)){
my.l<-1
for (j in 1:9){
p<-1-(1-pp)^results$i[j]
my.l<-my.l*choose(results$s[j],results$i[j+1])*p^(results$i[j+1])*(1-p)^(results$s[j+1])
}
lvec<-c(lvec,my.l)
}
#plot(seq(0,1,0.01),lvec,type="l")

pvec<-c(pvec,((which(lvec==max(lvec)))-1)/100)
}
hist(pvec,breaks=50)