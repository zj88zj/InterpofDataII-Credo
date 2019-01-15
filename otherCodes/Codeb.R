load("MDNdata.R")
#######################
y = as.numeric(MDN[,21])-1 # "Credo.Spend.Ind" or women minority owned 
yy = (MDN[,20] =="S")*1 # "Business.Size.Code"  Small or  nor small
#Define some possible base units 
#table(y,yy)/length(yy)
categ = MDN[,14]
subcateg = MDN[,15]
typ= MDN[,24]  # order type
tot = MDN[,16]
site= MDN[,3]

#table length(table(MDN$Requester.WWID))
#table(table(MDN$Preparer.WWID))
#table(tt <-table(MDN$Requester.Supervisor.WWID))
#length(tt[tt>200])
#
## Adding date  MDN 
dd=unlist(strsplit(as.character(MDN$PO.Create)," "))
dd = dd[2*(1:(length(dd)/2))-1]
dd = as.Date(dd,"%m/%d/%Y")
d0 =as.numeric(strsplit(as.character(min(dd)),"-")[[1]][1:2])
d1 = matrix(as.numeric(unlist(strsplit(as.character(dd),"-"))),ncol=3,byrow=T)
month = d1[,2] # month of the year
dmonth = (d1[,1]-d0[1])*12+d1[,2]-d0[2]+1 
# Months since the begining of the data
#table( length(table(MDN$Requester.WWID)))
#table( length(table(MDN$Preparer.WWID)))
(tt <- table(MDN$Requester.Supervisor.WWID))
#length(tt)
#sum(tt)
#sum(tt[tt>200])
ttn = names(tt) [tt >200]
ttni = match (MDN$Requester.Supervisor.WWID,ttn)
# Define time periods
mon = (d1[,1]-2010)*12 + d1[,2]
mon2 = trunc((mon-1)/2)+1
mon3 = trunc((mon-1)/3)+1
mon6 = trunc((mon-1)/6)+1
# lets focus on the first subject data
i = MDN$Requester.Supervisor.WWID == ttn[1]
j = i; j[is.na(i)]=F
plot(dd[j],y[j],pch=20,cex=1,col=3)  
lines(smooth.spline(as.numeric(dd[j]),y[j],df=5),col=2)
#table(categ[j])
#plot(dd[j],y[j],pch=20,cex=1,col=3)  
#plot(mon2[j],y[j],pch=20,cex=1,col=3)  
#lines(smooth.spline(as.numeric(mon[j]),y[j],df=5),col=2)
#plot(tapply(as.numeric(y[j])-1,mon6[j],mean,na.rm=T))
#u <-tapply(y[j],mon[j],length)
#plot(names(u),u)
#nm = names(MDN)
#table(as.character(subcateg[j]), as.character(categ[j]))
#table(as.character(subcateg[j])) ;  as.character(categ[j]))
#table(typ[j],mon6[j])
#table(subcateg[j])
categ = as.character(categ)
subcateg = as.character(subcateg)
dataj=model.matrix(~.,data.frame(categ=categ[j],subcateg=subcateg[j]))[,-1]
datajj=apply(dataj,2,function(x,u) tapply(x,u,sum),u=mon2[j])
apply(datajj!=0,2,sum) > 2 -> jjj
datajj = datajj[,jjj]
ncol(datajj)
# yj = tapply(y[j],mon2[j],function(x) c(sum(x),length(x)))

yj = matrix(unlist(tapply(y[j],mon2[j],function(x) c(sum(x),length(x)))),ncol=2,byrow=T)

summary(glm(yj~sqrt(datajj),family="binomial"))
summary(glm(yj[-1,]~sqrt(datajj)[-17,]+yj[-17,],family="binomial"))

m = ncol(datajj)
for(u in 1:(m-1)) for(v in (u+1):m) {
  uv = 1*(datajj[,u]*datajj[,v] !=0)
  if(sum(uv)>2) datajj = cbind(datajj,uv)
}   

### Now we fit a model per individual
library(glmnet)
cv0 =cv.glmnet(sqrt(datajj),yj,family="binomial",alpha=0.8)
bb=glmnet(sqrt(datajj),yj,family="binomial",alpha=0.8,lambda = cv0$lambda.1se)$beta
dataj1=sqrt(datajj)[,bb[,1]!=0]
summary(glm(yj~dataj1,family="binomial"))


### Other Variables
deptcode = MDN$Dept.Code
categ1 = substring( as.character(categ),1,5) 
datt = data.frame(y,yy,categ=categ1,typ,tot,site)


################ BICLUSTERS ############
library(biclust)

tt =as.matrix(table(site,categ))
tt1 = array(unlist(sqrt(tt)),dim=dim(tt))
tt2 = array(unlist(tt),dim=dim(tt))
?biclust
biclust(tt1, method=BCCC(), delta=1.5,  alpha=1, number=10)-> bb
tt1[bb@RowxNumber[,1], bb@NumberxCol[1,]]
bb

m15 = mm2[,1:15]
u1 = apply(m15,1,paste,sep="",collapse="")
w = table(u1)
u1[1:10]

biclust(m15, method=BCCC(), delta=.1,  alpha=1, number=10)-> bb
m15[bb@RowxNumber[,1], bb@NumberxCol[1,]]

################ TREES ############


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

table(datt$categ)
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


