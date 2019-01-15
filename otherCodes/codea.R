AAA = read.csv("AAA.csv")
CPC = read.csv("CPC.csv")
CPP = read.csv("CPP.csv")
MDN = read.csv("MDN.csv")
IT = read.csv("IT.csv")
CLPS = read.csv("CLPS_RD.csv")

summary(MDN)
dim(MDN)
names(MDN)

table(MDN$Company, MDN$Department)
length(table(MDN$Supplier.No))
length(table(MDN$Linkage.Id))
table(table(MDN$Linkage.Id))
#table(MDN$Supplier.No,MDN$Linkage.Id)
linka = sort(unique(MDN$Linkage.Id))
linkano= list()
for(i in linka) linkano[[i]]= sort(unique(
    MDN$Supplier.No[MDN$Linkage.Id==i]))

length(linkano)
length(unlist(linkano))
length(table(MDN$Supplier.No))


#table(MDN$Supplier.No,MDN$Linkage.Id)
table(MDN$Supplier.Country.Code, MDN$Linkage.Id)
table(MDN$Order.Type.Code)
table(MDN$Department)
table(MDN$Linkage.Id)
table(MDN$Linkage.Id,MDN$Department) -> tt

c1 = cor(tt)
quantile(c1, c(0:100)/100)
x1 = array(rnorm(110000),dim=c(10000,11)) 
quantile(cor(x1), c(0:100)/100)
c1[c1 < 0.013 ] = 0
dim(c1)
c2 = c1
dimnames(c2) = NULL
pairs(c2,pch=".",col=2)
c2
round(c2,2)
c3 = sqrt(1 -c2^2)
round(c3,2)
hc = hclust(as.dist(c3), method="ward")
library(glasso)
glasso(s=solve(c2[c(7,9,10),c(7,9,10)],sym=T), 0.01)
c2[c(7, 9, 10), c(7, 9, 10)]
glasso(s=c2[c(7,9,10),c(7,9,10)], 0.01)
glasso(s=c2[c(7,9,10),c(7,9,10)], 0.05)
glasso(s=c2[c(2,3,4,6,8),c(2:4,6,8)], 0.05)
glasso(s=c2[c(2,3,4,6,8),c(2:4,6,8)], 0.02)

===========================================================================
  ff = function(x,rango= c(2:12)) {
    library(cluster)
    
    sl = rango
    i0 = rango[1]-1
    for(i in rango) 
      sl[i-i0]=mean(silhouette(pam(x, k=i))[,3])
    names(sl) = rango
    uu = rango[which.max(sl)]
    attr(uu,"a")=sl
    uu
   }
    
    
    library(MASS)
    library(mvtnorm)
    ro= (0:9)/10
    qq= c()
    n = nrow(x)
    i=5  
    z=rmvnorm(n,sigma= matrix(c(1,ro[i]*sqrt(2),ro[i]*sqrt(2),1),2))
    chisq.test(table(z[,1]<qq[1],z[,2] < qq[1]))$stat
    
    
    tcor = function(x1,x2) {
      tt = table(x1,x2)
      n = length(x1)
      qnorm((sum(tt[,2])+.333)/(n+0.666))->q2
      qnorm((sum(tt[2,])+.333)/(n+0.666))->q1
      b= NULL  
      a = NULL
      for( k in 1:length(ro)) {
        for(i in 1:100) { 
          z=rmvnorm(n,sigma= matrix(c(1,ro[k],ro[k],1),2))
          a[i] = sum( z[,1]< q1 & z[,2]< q2) }
        b[k] = median(a)
      }
      predict(smooth.spline(b,ro),x=tt[2,2])
    }
    
    
    
    for(j in 1:88) 
      for(l in (j+1):89) 
        cc1[j,l] = tcor(x[,j],x[,l])$y
    



# ===========================
y = AAA[,19] # "Credo.Spend.Ind" or women minority owned 
yy = (AAA[,18] =="S")*1 # "Business.Size.Code"  Small or  nor small
table(y,yy)/length(yy)
tim =   floor((dat - min(dat))/30)
categ = AAA[,12]
typ= AAA[,22]
tot = AAA[,14]
site= AAA[,3]
names(AAA)[22]
tt = table(y,yy,tim)
for(i in 1:37) tt[,,i] = tt[,,i]/sum(tt[,,i]) #  Over time
table(typ,site)

datt = data.frame(y,yy,categ,typ,tot,site)
par(mfrow=c(1,1))
plot(tt[2,1,]+tt[2,2,],ylim=c(0,.7))
plot(tt[2,2,]+tt[1,2,],ylim=c(0,.4))
plot(tt[2,2,],ylim=c(0,.2))
plot(tt[2,1,])
par(mfrow=c(2,3))
plot(tt[2,1,]+tt[2,2,])
plot(tt[1,1,])
plot(tt[2,1,])
plot(tt[2,2,]+tt[1,2,])
plot(tt[1,2,])
plot(tt[2,2,])

tt = table(y,typ,tim)
for(i in 1:37) tt[,,i] = tt[,,i]/sum(tt[,,i]) #  Over time
par(mfrow=c(2,2))
plot(tt[2,4,])

## Adding date
dd=unlist(strsplit(as.character(MDN$PO.Create)," "))
dd = dd[2*(1:(length(dd)/2))-1]
dd = as.Date(dd,"%m/%d/%Y")

d0 =as.numeric(strsplit(as.character(min(dd)),"-")[[1]][1:2])
d1 = matrix(as.numeric(unlist(strsplit(as.character(dd),"-"))),ncol=3,byrow=T)
month = d1[,2] # month of the year
dmonth = (d1[,1]-d0[1])*12+d1[,2]-d0[2]+1 # Months since the begining of the data

table(table(MDN$Requester.WWID))
table(table(MDN$Preparer.WWID))
table(tt <-table(MDN$Requester.Supervisor.WWID))
ttn = names(tt) [tt >20]
ttni = match (MDN$Requester.Supervisor.WWID,ttn)
i = MDN$Requester.Supervisor.WWID == ttn[1]
plot(dd[i],pch=".")  
nm = names(MDN)
#######################
y = MDN[,21] # "Credo.Spend.Ind" or women minority owned 
yy = (MDN[,20] =="S")*1 # "Business.Size.Code"  Small or  nor small
table(y,yy)/length(yy)
categ = MDN[,14]
typ= MDN[,24]
tot = MDN[,16]
site= MDN[,3]
tt = table(y,yy,dmonth)
for(i in 1:36) tt[,,i] = tt[,,i]/sum(tt[,,i]) #  Over time
plot(tt[1,1,],type="l",ylim=range(tt))
lines(tt[1,2,],col=2); lines(tt[2,1,],col=3); lines(tt[2,2,],col=4)
table(typ,site)
par(mfrow=c(1,1))
plot(tt[2,1,]+tt[2,2,],ylim=c(0,.7))
plot(tt[2,2,]+tt[1,2,],ylim=c(0,.4))
plot(tt[2,2,],ylim=c(0,.2))
plot(tt[2,1,])
par(mfrow=c(2,3))
plot(tt[2,1,]+tt[2,2,])
plot(tt[1,1,])
plot(tt[2,1,])
plot(tt[2,2,]+tt[1,2,])
plot(tt[1,2,])
plot(tt[2,2,])

tt = table(y,typ,dmonth)
for(i in 1:36) tt[,,i] = tt[,,i]/sum(tt[,,i]) #  Over time
par(mfrow=c(2,5),mar=c(1,1,3,3))
for(i in 1:2) for(j in 1:5) plot(tt[i,j,])
plot(tt[1,4,])


datt = data.frame(y,yy,categ,typ,tot,site)


################ BICLUSTERS ############
library(biclust)

tt =as.matrix(table(site,categ))
tt1 = array(unlist(sqrt(tt)),dim=dim(tt))
tt2 = array(unlist(tt),dim=dim(tt))
?biclust
biclust(tt1, method=BCCC(), delta=1.5,  alpha=1, number=10)-> bb
tt[bb@RowxNumber[,1], bb@NumberxCol[1,]]
bb


################ TREES ############

categ1 = substring( as.character(categ),1,5) 
datt = data.frame(y,yy,categ=categ1,typ,tot,site)

library(rpart)
for(j in 1:36) print(rpart( y~categ+typ+tot+ site, data=datt[tim==(j-1),]))
for(j in 1:36) print(rpart( yy~categ+typ+tot+ site, data=datt[tim==(j-1),]))

library(ARF)
aa = arf(datt[tim==0,3:6],y[tim==0],pred=F)


###################  BAYESIAN NETWORK  ######## 
## Another example

library(bnlearn)
alpha = mmpc(Y, alpha=0.01) #Max-Min Parents and Children Method
plot(alpha, main="")
title(main="Bayesian Netword/mmpc method", cex.main=1.2)
beta = gs(Y) #Grow-Shrink Markov Blanket Method
plot(beta, main="")
title(main="Bayesian Netword/gs method", cex.main=1.2)
gamma = empty.graph(names(Y))
modelstring(gamma) =
  paste("[chemical]","[consult][package][capital|consult][market|consult][rd|consult:package:market]",
        "[it|market]","[manufacture|rd][exclusion|rd]", "[exclusion|manufacture]", sep = "" )
graphviz.plot(gamma) **END

#  +++++++++++++++++++++++++++++++++++++
# 

table(categ)

c1 = as.character(datt$categ)
c2 = as.numeric(factor(datt$categ))
table(c2)[10:20]
c1[c2 <=3] = "Other"
c1[c2==6 | c2 ==5] = "Other"
c1[c2>=9 & c2 <=11] = "Other"
c1[c2>=15 & c2 <=20] = "Other"
table(c2)
for( i in 1:21) if (table(c2)[i]<500) c1[c2==i]="Other"
dattt = datt
names(dattt)
hist(log(1+datt$tot),200)
dattt$tot = cut(log(1+datt$tot),c(-Inf,4,7,9,14,Inf))
dattt$categ= factor(c1)
table(dattt$categ, dattt$tot)

summary( glm(y~yy+categ*typ+categ*site + tot,data=dattt[dmonth==26,],family=binomial))
summary( glm(y~yy+categ*typ+categ*site + tot,data=dattt,family=binomial))
summary( glm(y~.,data=dattt,family=binomial))


#  +++++++++++++++++++++++++++++++++++++

pdf("bayesn2.pdf",width=9,height=9)
par(mfrow=c(6,6),mar=c(0,0,0,0),cex=0.5)
for(i in 1:36)
  plot(mmpc(dattt[dmonth==i,]))

dev.off()
#


