virtu=10
fifo=rep(0,200)
rnd=rep(0,200)
greed=rep(0,200)
greedb=rep(0,200)
ours=rep(0,200)
greedmin=rep(0,200)
greedminb=rep(0,200)
oursmin=rep(0,200)
fifofun=function(n)
{
  fiflist=rep(0,virtu)
  tmp=1
  for(i in 1:length(n)){
    fiflist[tmp]=fiflist[tmp]+n[i]
    tmp=tmp+1;
    if(tmp>virtu)
      tmp=1
  }
  return(max(fiflist))
}
greedfun=function(n)
{
  greedlist=rep(0,virtu)
  tmp=1
  for(i in 1:length(n)){
    greedlist[tmp]=greedlist[tmp]+n[i]
    tmp=which.min(greedlist)
  }
  return(c(max(greedlist),min(greedlist)))
}
greedfunb=function(n)
{
  greedlist=rep(0,virtu)
  blba=rep(0,virtu)
  tmp=1
  blba[tmp]=blba[tmp]+1
  for(i in 1:length(n)){
    blba[tmp]=blba[tmp]+1
    greedlist[tmp]=greedlist[tmp]+n[i]
    tmp=which(blba==min(blba))
    if(length(tmp)!=1){
      tmptmp=which.min(greedlist[tmp])
      tmp=tmp[tmptmp]
    }
  }
  return(c(max(greedlist),min(greedlist)))
}
rndfun=function(n)
{
  rndlist=rep(0,virtu)
  tmp=sample(1:virtu,1)
  for(i in 1:length(n)){
    rndlist[tmp]=rndlist[tmp]+n[i]
    tmp=sample(1:virtu,1)
  }
  return(max(rndlist))
}
oursfun=function(n)
{
  ourslist=rep(0,virtu)
  m=sort(n,decreasing = TRUE)
  tmp=1
  for(i in 1:length(n)){
    ourslist[tmp]=ourslist[tmp]+m[i]
    tmp=which.min(ourslist)
  }
  return(c(max(ourslist),min(ourslist)))
}
for(i in 1:200)
{
  tmpfifo=rep(0,50)
  tmprnd=rep(0,50)
  tmpgreed=rep(0,50)
  tmpgreedb=rep(0,50)
  tmpours=rep(0,50)
  tmpoursmin=rep(0,50)
  tmpgreedmin=rep(0,50)
  tmpgreedminb=rep(0,50)
  for(j in 1:50){
    n=sample(1:50,i,replace = TRUE)
    tmpfifo=fifofun(n)
    tmprnd=rndfun(n)
    tmp=greedfun(n)
    tmpgreed=tmp[1]
    tmpgreedmin=tmp[2]
    tmp=greedfunb(n)
    tmpgreedb=tmp[1]
    tmpgreedminb=tmp[2]
    tmp=oursfun(n)
    tmpours=tmp[1]
    tmpoursmin=tmp[2]
  }
  fifo[i]=mean(tmpfifo)
  greed[i]=mean(tmpgreed)
  greedb[i]=mean(tmpgreedb)
  rnd[i]=mean(tmprnd)
  ours[i]=mean(tmpours)
  greedmin[i]=mean(tmpgreedmin)
  greedminb[i]=mean(tmpgreedminb)
  oursmin[i]=mean(tmpoursmin)
}
windows()
plot(fifo,type='l',ylim=c(0,max(rnd)),col='green',xlab="Number of tasks",ylab="Av. makespan")
legend("topleft", c("RANDOM","FIFO","ILBA","BLBA","OURS"),col=c("brown", "green","blue","red","black"), lty=1,)
lines(rnd,col='brown')
lines(greed,col='blue')
lines(greedb,col='red')
lines(ours,col='black')
windows()
plot(greedb-ours,type='l',xlab="Number of tasks",ylab="Av. makespan Diff.",col="red")
legend("topleft", c("ILBA-OURS","BLBA-OURS"),col=c("blue","red"), lty=1)
lines(greed-ours,col='blue')
windows()
plot(greedb-greedminb,type='l',ylim=c(min(ours-oursmin),10+max(greedb-greedminb)),xlab="Number of tasks",ylab="Av. TCD",col="red")
legend("topleft", c("BLBA","ILBA","OURS"),col=c("red","blue","black"), lty=1,)
lines(greed-greedmin,col="blue")
lines(ours-oursmin,col="black")

for(i in 1:200)
{
  tmpfifo=rep(0,50)
  tmprnd=rep(0,50)
  tmpgreed=rep(0,50)
  tmpgreedb=rep(0,50)
  tmpours=rep(0,50)
  tmpoursmin=rep(0,50)
  tmpgreedmin=rep(0,50)
  tmpgreedminb=rep(0,50)
  m=rpois(50,7)+1
  for(j in 1:50){
    n=sample(1:50,m[j],replace = TRUE)
    tmpfifo[j]=fifofun(n)
    tmprnd[j]=rndfun(n)
    tmp=greedfun(n)
    tmpgreed[j]=tmp[1]
    tmpgreedmin[j]=tmp[2]
    tmp=greedfunb(n)
    tmpgreedb[j]=tmp[1]
    tmpgreedminb[j]=tmp[2]
    tmp=oursfun(n)
    tmpours[j]=tmp[1]
    tmpoursmin[j]=tmp[2]
  }
  fifo[i]=mean(tmpfifo)
  greed[i]=mean(tmpgreed)
  greedb[i]=mean(tmpgreedb)
  rnd[i]=mean(tmprnd)
  ours[i]=mean(tmpours)
  greedmin[i]=mean(tmpgreedmin)
  greedminb[i]=mean(tmpgreedminb)
  oursmin[i]=mean(tmpoursmin)
}

windows()
plot(fifo,type='p',pch=15,ylim=c(40,max(rnd)+20),col='green',xlab="Time steps",ylab="Av. makespan")
legend("topleft", c("RANDOM","FIFO","ILBA","BLBA","OURS"),col=c("brown", "green","blue","red","black"), lty=1,)
lines(rnd,col='brown',type='p',pch=15)
lines(greed,col='blue',type='p',pch=15)
lines(greedb,col='red',type='p',pch=15)
lines(ours,col='black',type='p',pch=15)
abline(h=mean(ours),col='black')
abline(h=mean(fifo),col='green')
abline(h=mean(rnd),col='brown')
abline(h=mean(greed),col='blue')
abline(h=mean(greedb),col='red')

rnd1=rep(0,50)
greed1=rep(0,50)
greed1b=rep(0,50)
ours1=rep(0,50)
fifo1=rep(0,50)
oursmin1=rep(0,50)
greedmin1=rep(0,50)
greedmin1b=rep(0,50)
tfif1=rep(0,50)
tgrd1=rep(0,50)
trnd1=rep(0,50)
tour1=rep(0,50)
tgrdb1=rep(0,50)
options(digits=22)
for(k in 1:50)
{
  tfif2=rep(0,200)
  tgrd2=rep(0,200)
  trnd2=rep(0,200)
  tour2=rep(0,200)
  tgrdb2=rep(0,200)
  for(i in 1:200)
  {
    tfif3=rep(0,50)
    tgrd3=rep(0,50)
    trnd3=rep(0,50)
    tour3=rep(0,50)
    tgrdb3=rep(0,50)
    tmpfifo=rep(0,50)
    tmprnd=rep(0,50)
    tmpgreed=rep(0,50)
    tmpgreedb=rep(0,50)
    tmpours=rep(0,50)
    tmpoursmin=rep(0,50)
    tmpgreedmin=rep(0,50)
    tmpgreedminb=rep(0,50)
    m=rpois(50,7)+1
    for(j in 1:50){
      n=sample(1:50,m[j],replace = TRUE)
      start_time <- Sys.time()
      tmpfifo[j]=fifofun(n)
      end_time <- Sys.time()
      tfif3[j]=end_time - start_time
      start_time <- Sys.time()
      tmprnd[j]=rndfun(n)
      end_time <- Sys.time()
      trnd3[j]=end_time - start_time
      start_time <- Sys.time()
      tmp=greedfun(n)
      tmpgreed[j]=tmp[1]
      tmpgreedmin[j]=tmp[2]
      end_time <- Sys.time()
      tgrd3[j]=end_time - start_time
      start_time <- Sys.time()
      tmp=greedfunb(n)
      tmpgreedb[j]=tmp[1]
      tmpgreedminb[j]=tmp[2]
      end_time <- Sys.time()
      tgrdb3[j]=end_time - start_time
      start_time <- Sys.time()
      tmp=oursfun(n)
      tmpours[j]=tmp[1]
      tmpoursmin[j]=tmp[2]
      end_time <- Sys.time()
      tour3[j]=end_time - start_time
    }
    tfif2[i]=mean(tfif3)
    tour2[i]=mean(tour3)
    tgrd2[i]=mean(tgrd3)
    tgrdb2[i]=mean(tgrdb3)
    trnd2[i]=mean(trnd3)
    fifo[i]=mean(tmpfifo)
    greed[i]=mean(tmpgreed)
    greedb[i]=mean(tmpgreedb)
    rnd[i]=mean(tmprnd)
    ours[i]=mean(tmpours)
    greedmin[i]=mean(tmpgreedmin)
    greedminb[i]=mean(tmpgreedminb)
    oursmin[i]=mean(tmpoursmin)
  }
  tfif1[k]=sum(tfif2)
  tour1[k]=sum(tour2)
  tgrd1[k]=sum(tgrd2)
  tgrdb1[k]=sum(tgrdb2)
  trnd1[k]=sum(trnd2)
  fifo1[k]=mean(fifo)
  greed1[k]=mean(greed)
  greed1b[k]=mean(greedb)
  rnd1[k]=mean(rnd)
  ours1[k]=mean(ours)
  oursmin1[k]=mean(oursmin)
  greedmin1[k]=mean(greedmin)
  greedmin1b[k]=mean(greedminb)
}

mean(tour1)
mean(tfif1)
mean(trnd1)
mean(tgrdb1)
mean(tgrd1)


windows()
plot(fifo1,type='l',ylim=c(min(ours1),max(rnd1)+20),col='green',xlab="Time steps",ylab="Av. makespan")
legend("topleft", c("RANDOM","FIFO","ILBA","BLBA","OURS"),col=c("brown", "green","blue","red","black"), lty=1,)
lines(rnd1,col='brown',type='l')
lines(greed1,col='blue',type='l')
lines(greed1b,col='red',type='l')
lines(ours1,col='black',type='l')
windows()
plot(greed1b-ours1,ylim=c(min(greed1-ours1),max(greed1b-ours1)+0.05),type='l',xlab="Time steps",ylab="Av. makespan Diff.",col="red")
legend("topleft", c("ILBA-OURS","BLBA-OURS"),col=c("blue","red"), lty=1)
lines(greed1-ours1,col='blue')

windows()
plot(greed1-greedmin1,type='l',ylim=c(min(ours1-oursmin1),max(greed1b-greedmin1b)+0.3),xlab="Time steps",ylab="Av. TCD",col="blue")
legend("topleft", c("BLBA","ILBA","OURS"),col=c("red","blue","black"), lty=1,)
lines(ours1-oursmin1,col="black")
lines(greed1b-greedmin1b,col="red")



##############################200 timestep continuous
fiflist=rep(0,virtu)
greedlist=rep(0,virtu)
greedlistb=rep(0,virtu)
blba=rep(0,virtu)
rndlist=rep(0,virtu)
ourslist=rep(0,virtu)

fifofun1=function(n,fiflist,tmp)
{
  if(tmp>virtu)
    tmp=1
  for(i in 1:length(n)){
    fiflist[tmp]=fiflist[tmp]+n[i]
    tmp=tmp+1;
    if(tmp>virtu)
      tmp=1
  }
  return(c(max(fiflist),tmp,fiflist))
}
greedfun1=function(n,greedlist,tmp1)
{
  tmp=tmp1
  for(i in 1:length(n)){
    greedlist[tmp]=greedlist[tmp]+n[i]
    tmp=which.min(greedlist)
  }
  return(c(max(greedlist),min(greedlist),tmp,greedlist))
}
greedfun1b=function(n,greedlist,tmp1,blba)
{
  tmp=tmp1
  for(i in 1:length(n)){
    blba[tmp]=blba[tmp]+1
    greedlist[tmp]=greedlist[tmp]+n[i]
    tmp=which(blba==min(blba))
    if(length(tmp)!=1){
      tmptmp=which.min(greedlist[tmp])
      tmp=tmp[tmptmp]
    }
  }
  return(c(max(greedlist),min(greedlist),tmp,greedlist,blba))
}
rndfun1=function(n,rndlist)
{
  tmp=sample(1:virtu,1)
  for(i in 1:length(n)){
    rndlist[tmp]=rndlist[tmp]+n[i]
    tmp=sample(1:virtu,1)
  }
  return(c(max(rndlist),rndlist))
}
oursfun1=function(n,ourslist)
{
  m=sort(n,decreasing = TRUE)
  for(i in 1:length(n)){
    tmp=which.min(ourslist)
    ourslist[tmp]=ourslist[tmp]+m[i]
  }
  return(c(max(ourslist),min(ourslist),ourslist))
}

tmp=1
tmp1=1
tmp2=1
tfif1=rep(0,200)
tgrd1=rep(0,200)
trnd1=rep(0,200)
tour1=rep(0,200)
tgrdb1=rep(0,200)
for(i in 1:200)
{
  tfif=rep(0,50)
  tgrd=rep(0,50)
  trnd=rep(0,50)
  tour=rep(0,50)
  tgrdb=rep(0,50)
  tmpfifo=rep(0,50)
  tmprnd=rep(0,50)
  tmpgreed=rep(0,50)
  tmpgreedb=rep(0,50)
  tmpours=rep(0,50)
  tmpoursmin=rep(0,50)
  tmpgreedmin=rep(0,50)
  tmpgreedminb=rep(0,50)
  m=rpois(50,7)+1
  for(j in 1:50){
    n=sample(1:50,m[j],replace = TRUE)
    start_time <- Sys.time()
    temp=fifofun1(n,fiflist,tmp)
    tmpfifo[j]=temp[1]
    tmp=temp[2]
    fiflist=temp[3:length(temp)]
    end_time <- Sys.time()
    tfif[j]=end_time - start_time
    start_time <- Sys.time()
    temp=rndfun1(n,rndlist)
    tmprnd[j]=temp[1]
    rndlist=temp[2:length(temp)]
    end_time <- Sys.time()
    trnd[j]=end_time - start_time
    start_time <- Sys.time()
    temp=greedfun1(n,greedlist,tmp1)
    tmpgreed[j]=temp[1]
    tmpgreedmin[j]=temp[2]
    tmp1=temp[3]
    greedlist=temp[4:length(temp)]
    end_time <- Sys.time()
    tgrd[j]=end_time - start_time
    start_time <- Sys.time()
    temp=greedfun1b(n,greedlistb,tmp2,blba)
    tmpgreedb[j]=temp[1]
    tmpgreedminb[j]=temp[2]
    tmp2=temp[3]
    greedlistb=temp[4:13]
    blba=temp[14:length(temp)]
    end_time <- Sys.time()
    tgrdb[j]=end_time - start_time
    start_time <- Sys.time()
    temp=oursfun1(n,ourslist)
    tmpours[j]=temp[1]
    tmpoursmin[j]=temp[2]
    ourslist=temp[3:length(temp)]
    end_time <- Sys.time()
    tour[j]=end_time - start_time
  }
  fifo[i]=mean(tmpfifo)
  greed[i]=mean(tmpgreed)
  greedb[i]=mean(tmpgreedb)
  rnd[i]=mean(tmprnd)
  ours[i]=mean(tmpours)
  greedmin[i]=mean(tmpgreedmin)
  greedminb[i]=mean(tmpgreedminb)
  oursmin[i]=mean(tmpoursmin)
  tfif1[i]=mean(tfif)
  tgrd1[i]=mean(tgrd)
  trnd1[i]=mean(trnd)
  tour1[i]=mean(tour)
  tgrdb1[i]=mean(tgrdb)
}
sum(tfif1)
sum(tgrd1)
sum(tgrdb1)
sum(trnd1)
sum(tour1)

windows()
plot(1:200,fifo,type='l',ylim=c(500,(max(rnd)+1000)),xlab="Time steps",ylab="Av. makespan",col="green")
legend("topleft", c("RANDOM","FIFO","ILBA","BLBA","OURS"),col=c("brown", "green","blue","red","black"), lty=1,)
lines(1:200,rnd,col='brown',type='l')
lines(1:200,greed,col='blue',type='l')
lines(1:200,greedb,col='red',type='l')
lines(1:200,ours,col='black',type='l')
windows()
plot(greed-ours,type='l',xlab="Time steps",ylab="Av. makespan Diff.")
legend("topleft", c("ILBA-Ours","BLBA-Ours"),col=c("blue","red"), lty=1,)
lines(greed-ours,col='red',type='l')

windows()
plot(greed-greedmin,type='l',ylim=c(min(ours-oursmin),max(greed-greedmin)+2),xlab="Time steps",ylab="Av. TCD",col="red")
legend("topleft", c("ILBA","OURS"),col=c("red","black"), lty=1)
lines(ours-oursmin,col="black")
lines(greed-greedmin,col="blue")


fiflist=rep(0,virtu)
greedlist=rep(0,virtu)
rndlist=rep(0,virtu)
ourslist=rep(0,virtu)

rnd1=rep(0,50)
greed1=rep(0,50)
ours1=rep(0,50)
fifo1=rep(0,50)
oursmin1=rep(0,50)
greedmin1=rep(0,50)
tmp=1
tmp1=1
for(k in 1:50)
{
  for(i in 1:200)
  {
    tmpfifo=rep(0,50)
    tmprnd=rep(0,50)
    tmpgreed=rep(0,50)
    tmpours=rep(0,50)
    tmpoursmin=rep(0,50)
    tmpgreedmin=rep(0,50)
    m=rpois(50,7)+1
    for(j in 1:50){
      n=sample(1:50,m[j],replace = TRUE)
      temp=fifofun1(n,fiflist,tmp)
      tmpfifo[j]=temp[1]
      tmp=temp[2]
      fiflist=temp[3:length(temp)]
      temp=rndfun1(n,rndlist)
      tmprnd[j]=temp[1]
      rndlist=temp[2:length(temp)]
      temp=greedfun1(n,greedlist,tmp1)
      tmpgreed[j]=temp[1]
      tmpgreedmin[j]=temp[2]
      tmp1=temp[3]
      greedlist=temp[4:length(temp)]
      temp=oursfun1(n,ourslist)
      tmpours[j]=temp[1]
      tmpoursmin[j]=temp[2]
      ourslist=temp[3:length(temp)]
    }
    fifo[i]=mean(tmpfifo)
    greed[i]=mean(tmpgreed)
    rnd[i]=mean(tmprnd)
    ours[i]=mean(tmpours)
    greedmin[i]=mean(tmpgreedmin)
    oursmin[i]=mean(tmpoursmin)
  }
  fifo1[k]=mean(fifo)
  greed1[k]=mean(greed)
  rnd1[k]=mean(rnd)
  ours1[k]=mean(ours)
  oursmin1[k]=mean(oursmin)
  greedmin1[k]=mean(greedmin)
}

windows()
plot(fifo1,type='l',ylim=c(min(ours1),max(rnd1)+20),col='red',xlab="Index",ylab="Av. makespan")
legend("topleft", c("RANDOM","FIFO","GREEDY","OURS"),col=c("blue", "red","green","black"), lty=1,)
lines(rnd1,col='blue',type='l')
lines(greed1,col='green',type='l')
lines(ours1,col='black',type='l')
windows()
plot(greed1-ours1,type='l',xlab="Index",ylab="Av. makespan Diff.")
windows()
plot(greed1-greedmin1,type='l',ylim=c(min(ours1-oursmin1),max(greed1-greedmin1)),xlab="Index",ylab="Av. TCD",col="red")
legend("topleft", c("GREEDY","OURS"),col=c("red","black"), lty=1,)
lines(ours1-oursmin1,col="black")
