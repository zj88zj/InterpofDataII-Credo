##### 1. Exploratory Data Analysis #####
summary(CLPS) 
dim(CLPS) 
names(CLPS) 

table(table(CLPS$Company)) # frequency of company
table(CLPS$Order.Type.Code) # frequency of order type
table(CLPS$Department) # frequency of department 
table(CLPS$Linkage.Id) # frequency of linkage 
table(CLPS$Linkage.Id,CLPS$Department) # frequency of linkage by department

##### Defining New Variables #####
attach(CLPS)
categ <- as.character(Category.Name)
subcateg <- as.character(Subcategory.Name)
j = CLPS$Requester.Supervisor.WWID 
catj <- as.character(categ[j])
subcatj <- as.character(subcateg[j])
credo <- as.numeric(CLPS[,21])-1
size <- (CLPS[,20] == 'S')*1
typ <- CLPS[,25]
tot <- CLPS[,16]
site <- CLPS[,26]
WWID <- CLPS$Requester.Supervisor.WWID

#converts times from string to date
date <- unlist(strsplit(as.character(CLPS$PO.Create.Date)," "))
date <- date[2*(1:(length(date)/2))-1]
date <- as.Date(date,"%m/%d/%Y")

#Redefines dates in terms of months since time 0
basedate <- as.numeric(strsplit(as.character(min(date)),"-")[[1]][1:2])
datematrix <- matrix(as.numeric(unlist(strsplit(as.character(date),"-"))),ncol=3,byrow=T)
month <- datematrix[,2] # month of the year
totalmonths <- (datematrix[,1]-basedate[1])*12+datematrix[,2]-basedate[2]+1 # Months since the begining of the data

#creates table that counts each type of cat and sub every 2 months
monthnum = (datematrix[,1]-2010)*12 + datematrix[,2]
month2 = trunc((monthnum-1)/2)+1

#select columns that have more than 2 non zero entries
nonzero = apply(data2month!=0,2,sum) > 2
data2month = data2month[,nonzero]
#How many are left?
ncol(data2month)
#Creates ordered index of supervisors (purchases>500)
suptable <- table(CLPS$Requester.Supervisor.WWID)
supID <- names(suptable) [suptable >200]
length(supID) #there are 417 supervisors with >500 purchases

##### Bayesian Network #####
CLPSsubset <- data.frame(credo, size, categ, subcateg, typ, tot, site)
library(bnlearn)
alpha = mmpc(CLPSsubset, alpha=0.01) #Max-Min Parents and Children Method
plot(alpha, main="Max-Min Parent-Child Method") 
title(main="Bayesian Netword/mmpc method", cex.main=1.2)
beta = gs(CLPSsubset) #Grow-Shrink Markov Blanket Method
plot(beta, main="Grow-Shrink Markov Blanket Method")
fittedbn <- bn.fit(alpha, data = CLPSsubset)

directedBN = hc(CLPSsubset) # Creates directed BN 
plot(directedBN, main="Directed Bayesian Network") 

# Probabilities of Bayesian Network
fittedbn <- bn.fit(directedBN, data = CLPSsubset) 
print(fittedbn$yy)
print(fittedbn$y)
print(fittedbn$subcateg)
print(fittedbn$categ)
print(fittedbn$tot)
print(fittedbn$typ)

pdf("bayesn2.pdf",width=9,height=9)
par(mfrow=c(6,6),mar=c(0,0,0,0),cex=0.5)
for(i in 1:36)
  plot(mmpc(CLPSsubset[totalmonths==i,]))

dev.off()

# Supervisor Number 1
i = CLPS$Requester.Supervisor.WWID == supID[4]
j = i; j[is.na(i)]=F
table(categ[j])
plot(date[j],credo[j],pch=20,cex=1,col=3)  
plot(month[j],credo[j],pch=20,cex=1,col=3)  
lines(smooth.spline(as.numeric(month[j]),credo[j],df=5),col=2)
plot(tapply(as.numeric(y[j])-1,month[j],mean,na.rm=T))
credpermonth <-tapply(credo[j],month[j],length) 
plot(names(credpermonth),credpermonth) #plot of credo purchases per month

modelm = model.matrix(~.,data.frame(categ=categ[j],subcateg=subcateg[j], typ=typ[j]))[,-1]
data2month=apply(modelm,2,function(x,u) tapply(x,u,sum),u=monthnum[j])
atleast6 <- apply(data2month!=0,2,sum) > 6 # restricts observation to at least 6 non-ref-level covariates
data2month = data2month[,atleast6]
ncol(data2month)

yj = tapply(credo[j],monthnum[j],function(x) c(sum(x),length(x)))
yj = matrix(unlist(tapply(credo[j],monthnum[j],function(x) c(sum(x),length(x)))),ncol=2,byrow=T)
summary(glm(yj~sqrt(data2month),family="binomial"))
summary(glm(yj[-1,]~sqrt(data2month)[-17,]+yj[-17,],family="binomial"))



for( l in 1:length(supID)) {
  i = CLPS$Requester.Supervisor.WWID == supID[l]
  j = i
  j[is.na(i)]=F
  if (length(table(yy))>1 & min(table(yy))>5) {
    if (length(table(subcatj))>1 & min(table(subcatj))>3) {
      if (length(table(catj))>1 & min(table(catj))>3) {
        modelm[j]=model.matrix(~.,data.frame(categ=catj,subcateg=subcatj))[,-1]}}}
   else{
        modelm[j]=model.matrix(~.,data.frame(subcateg=subcatj))[,-1]
       }
}
data2month=apply(modelm,2,function(x,u) tapply(x,u,sum),u=month2[j])
yj = matrix(unlist(tapply(credo[j],month2[j],function(x) c(sum(x),length(x)))),ncol=2,byrow=T)
summary(glm(yj~sqrt(data2month),family="binomial"))
summary(glm(yj[-1,]~sqrt(data2month)[-17,]+yj[-17,],family="binomial"))



#yj = tapply(credo[j],month2[j],function(x) c(sum(x),length(x)))
## create response


summary(glm(yj~sqrt(data2month),family="binomial"))
summary(glm(yj[-1,]~sqrt(data2month)[-17,]+yj[-17,],family="binomial"))


#generate the network variables
# Could be replaced by byessian network or correlation network 
m = ncol(data2month)
for(u in 1:(m-1)) for(v in (u+1):m) {
  uv = 1*(data2month[,u]*data2month[,v] !=0)
  if(sum(uv)>2) data2month = cbind(data2month,uv)
}   
##check the data
## Make sure all the variables contain some info
dim(data2month)
summary(data2month)

library(glmnet)
xx<-cbind(data2month[-17,],yj[-17,1]/yj[-17,2])
yy<- yj[-1,1]/yj[-1,2]
qqq=cv.glmnet(xx, yy,alpha=1)

bb= glmnet(xx,yy,lambda=c(exp(c(-4.2,-5.8))),alpha=1)$beta
xf=xx[,bb[,1]!=0]
bblm = lm(yy~xf)
step(bblm)
summary(step(bblm))
