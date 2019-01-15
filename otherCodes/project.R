######## reads the file "CLPS_RD.csv" #########
CLPS = read.csv("CLPS_RD.csv")

######## Exploration of the CLPS dataset ############
summary(CLPS) # summarizes the contents of CLPS
dim(CLPS) #checks dimension row by column (observations by variables)
names(CLPS) # variables names (column names)

table(table(CLPS$Company)) # frequency of company type
length(table(CLPS$Supplier.No)) # frequency of supplier type

table(CLPS$Order.Type.Code) # frequency of order type
table(CLPS$Department) # frequency of department type
table(CLPS$Linkage.Id) # frequency of linkage type
table(CLPS$Linkage.Id,CLPS$Department) -> tt # frequency of linkage by department
############# end of exploration ##############


#### assigning columns of CLPS to variables ##########
y = as.numeric(CLPS[,21])-1 # "Credo.Spend.Ind" or women minority owned 
yy = (CLPS[,20] =="S")*1 # "Business.Size.Code"  Small or  nor small
table(y)
table(y,yy)/length(yy)
categ = CLPS[,14]
subcateg = CLPS[,15]
typ= CLPS[,25]
tot = CLPS[,16]
site= CLPS[,26]
names(CLPS)[15]

names(CLPS)
table(typ,site)
typ.site = data.frame(table(typ,site))
datt = data.frame(y,yy,categ,subcateg,typ,tot,site)

table(tt <-table(CLPS$Requester.Supervisor.WWID))
datatt= data.frame(tt)
length(tt[tt>200])

######### creating subset of the dataset CLPS. #########

datt = data.frame(y,yy,categ,subcateg,typ,tot,site) 



###################  BAYESIAN NETWORK  ######## 

library(bnlearn) #loading the package bnlearn
alpha = mmpc(datt, alpha=0.01) #Max-Min Parents and Children Method
plot(alpha, main="") #plotting the graph mmpc
title(main="Bayesian Netword/mmpc method", cex.main=1.2)
beta = gs(datt) #Grow-Shrink Markov Blanket Method
plot(beta, main="")
title(main="Bayesian Netword/gs method", cex.main=1.2)

charlie = hc(datt) # Bayesian network using a hill-climbing (HC) or a Tabu search (TABU) greedy search method
plot(charlie, main="") #plotting hill climbing
title(main="Bayesian network/hc method", cex.main=1.2)

### removing some of the arcs to create better relationships and subset.

charlie$arcs <- charlie$arcs[-which((charlie$arcs[,'from'] == "categ" & charlie$arcs[,'to'] == "subcateg")),]

### we check the conditional probabilities of each factor to determine the
## appropriate variables in the model.
fittedbn <- bn.fit(charlie, data = datt) # fitting the hill climbing 

###### checking probabilities of Bayesian Network ######
print(fittedbn$yy)
print(fittedbn$y)
print(fittedbn$subcateg)
print(fittedbn$categ)
print(fittedbn$tot)
print(fittedbn$typ)

############


############# Adding date CLPS ###########
dd=unlist(strsplit(as.character(CLPS$PO.Create)," "))
dd = dd[2*(1:(length(dd)/2))-1]
dd = as.Date(dd,"%m/%d/%Y")

d0 =as.numeric(strsplit(as.character(min(dd)),"-")[[1]][1:2])
d1 = matrix(as.numeric(unlist(strsplit(as.character(dd),"-"))),ncol=3,byrow=T)
month = d1[,2] # month of the year
dmonth = (d1[,1]-d0[1])*12+d1[,2]-d0[2]+1 # Months since the begining of the data

############# creating table for Requester.Supervisor.WWID(unit) ##############
tt <- table(CLPS$Requester.Supervisor.WWID)

length(tt[tt>250]) # lists the supervisors with greater than a frequency of 200. 

ttn = names(tt) [tt >200] # assigning
ttni = match(CLPS$Requester.Supervisor.WWID,ttn) # assigning

mon = (d1[,1]-2010)*12 + d1[,2] # monthly


# lets focus on the fourth subject

i = CLPS$Requester.Supervisor.WWID == ttn[4]
j = i; j[is.na(i)]=F


table(categ[j])
plot(dd[j],y[j],pch=20,cex=1,col=3)  

plot(mon[j],y[j],pch=20,cex=1,col=3)  
lines(smooth.spline(as.numeric(mon[j]),y[j],df=5),col=2)

plot(tapply(as.numeric(y[j])-1,mon[j],mean,na.rm=T))

u <-tapply(y[j],mon[j],length)

plot(names(u),u)

nm = names(CLPS)
table(as.character(subcateg[j]),as.character(categ[j]))
table(as.character(subcateg[j])) ;as.character(categ[j])
table(typ[j],mon[j])
table(typ[j])
table(mon[j])
table(subcateg[j])
categ = as.character(categ)
subcateg = as.character(subcateg)
group = data.frame(CLPS[,1])

dataj=model.matrix(~.,data.frame(categ=categ[j],subcateg=subcateg[j], typ=typ[j]))[,-1]
datajj=apply(dataj,2,function(x,u) tapply(x,u,sum),u=mon[j])
apply(datajj!=0,2,sum) > 6 -> jjj

datajj = datajj[,jjj]

ncol(datajj)

# "Creating Credo.Spend.Ind" or women minority owned time variable. 
yj = tapply(y[j],mon[j],function(x) c(sum(x),length(x)))

yj = matrix(unlist(tapply(y[j],mon[j],function(x) c(sum(x),length(x)))),ncol=2,byrow=T)
####

#### creating generalized linear model based on the bayesian network variables.
summary(glm(yj~sqrt(datajj),family="binomial"))


################### improving the model ############

categ=categ[j]
subcateg = subcateg[j]
typ = typ[j]
categ <- categ[!categ %in% "Research & Development (Products & Packaging)"]
subcateg <- subcateg[!subcateg %in% "Training and Development"]
typ <- typ[!typ %in% "99"]

dataj=model.matrix(~.,data.frame(subcateg=subcateg[j]))[,-1]
datajj=apply(dataj,2,function(x,u) tapply(x,u,sum),u=mon[j])
apply(datajj!=0,2,sum) > 6 -> jjj

datajj = datajj[,jjj]

ncol(datajj)

# "Creating Credo.Spend.Ind" or women minority owned time variable. 
yj = tapply(y[j],mon[j],function(x) c(sum(x),length(x)))

yj = matrix(unlist(tapply(y[j],mon[j],function(x) c(sum(x),length(x)))),ncol=2,byrow=T)
####

#### creating generalized linear model based on the bayesian network variables.
summary(glm(yj~sqrt(datajj),family="binomial"))






# lets focus on the eighth subject

i = CLPS$Requester.Supervisor.WWID == ttn[8]
j = i; j[is.na(i)]=F

plot(dd[j],y[j],pch=20,cex=1,col=3)  
lines(smooth.spline(as.numeric(dd[j],y[j],df=5)))

table(categ[j])
plot(dd[j],y[j],pch=20,cex=1,col=3)  

plot(mon[j],y[j],pch=20,cex=1,col=3)  
lines(smooth.spline(as.numeric(mon[j]),y[j],df=5),col=2)

plot(tapply(as.numeric(y[j])-1,mon[j],mean,na.rm=T))

u <-tapply(y[j],mon[j],length)

plot(names(u),u)

nm = names(CLPS)
table(as.character(subcateg[j]),as.character(categ[j]))
table(as.character(subcateg[j])) ;as.character(categ[j])
table(typ[j],mon[j])
table(typ[j])
table(mon[j])
table(subcateg[j])
categ = as.character(categ)
subcateg = as.character(subcateg)

dataj=model.matrix(~.,data.frame(categ=categ[j],subcateg=subcateg[j]))[,-1]
datajj=apply(dataj,2,function(x,u) tapply(x,u,sum),u=mon[j])
apply(datajj!=0,2,sum) > 6 -> jjj

datajj = datajj[,jjj]

ncol(datajj)

# "Creating Credo.Spend.Ind" or women minority owned time variable. 
yj = tapply(y[j],mon[j],function(x) c(sum(x),length(x)))

yj = matrix(unlist(tapply(y[j],mon[j],function(x) c(sum(x),length(x)))),ncol=2,byrow=T)
####

#### creating generalized linear model based on the bayesian network variables.
summary(glm(yj~sqrt(datajj),family="binomial"))




# lets focus on the 16th subject

i = CLPS$Requester.Supervisor.WWID == ttn[16]
j = i; j[is.na(i)]=F

plot(dd[j],y[j],pch=20,cex=1,col=3)  
lines(smooth.spline(as.numeric(dd[j],y[j],df=5)))

table(categ[j])
plot(dd[j],y[j],pch=20,cex=1,col=3)  

plot(mon[j],y[j],pch=20,cex=1,col=3)  
lines(smooth.spline(as.numeric(mon[j]),y[j],df=5),col=2)

plot(tapply(as.numeric(y[j])-1,mon[j],mean,na.rm=T))

u <-tapply(y[j],mon[j],length)

plot(names(u),u)

nm = names(CLPS)
table(as.character(subcateg[j]),as.character(categ[j]))
table(as.character(subcateg[j])) ;as.character(categ[j])
table(typ[j],mon[j])
table(typ[j])
table(mon[j])
table(subcateg[j])
table(categ[j])
categ = as.character(categ)
subcateg = as.character(subcateg)

dataj=model.matrix(~.,data.frame(categ=categ[j],subcateg=subcateg[j]))[,-1]
datajj=apply(dataj,2,function(x,u) tapply(x,u,sum),u=mon[j])
apply(datajj!=0,2,sum) > 6 -> jjj

datajj = datajj[,jjj]

ncol(datajj)

# "Creating Credo.Spend.Ind" or women minority owned time variable. 
yj = tapply(y[j],mon[j],function(x) c(sum(x),length(x)))

yj = matrix(unlist(tapply(y[j],mon[j],function(x) c(sum(x),length(x)))),ncol=2,byrow=T)
####

#### creating generalized linear model based on the bayesian network variables.
summary(glm(yj~sqrt(datajj),family="binomial"))
summary(glm(yj[-1,]~sqrt(datajj)[-17,]+yj[-17,],family="binomial"))


# lets focus on the 17th subject

i = CLPS$Requester.Supervisor.WWID == ttn[17]
j = i; j[is.na(i)]=F

plot(dd[j],y[j],pch=20,cex=1,col=3)  
lines(smooth.spline(as.numeric(dd[j],y[j],df=5)))

table(categ[j])
plot(dd[j],y[j],pch=20,cex=1,col=3)  

plot(mon[j],y[j],pch=20,cex=1,col=3)  
lines(smooth.spline(as.numeric(mon[j]),y[j],df=5),col=2)

plot(tapply(as.numeric(y[j])-1,mon[j],mean,na.rm=T))

u <-tapply(y[j],mon[j],length)

plot(names(u),u)

nm = names(CLPS)
table(as.character(subcateg[j]),as.character(categ[j]))
table(as.character(subcateg[j])) ;as.character(categ[j])
table(typ[j],mon[j])
table(typ[j])
table(mon[j])
table(subcateg[j])
table(categ[j])
categ = as.character(categ)
subcateg = as.character(subcateg)

dataj=model.matrix(~.,data.frame(categ=categ[j],subcateg=subcateg[j]))[,-1]
datajj=apply(dataj,2,function(x,u) tapply(x,u,sum),u=mon[j])
apply(datajj!=0,2,sum) > 6 -> jjj

datajj = datajj[,jjj]

ncol(datajj)

# "Creating Credo.Spend.Ind" or women minority owned time variable. 
yj = tapply(y[j],mon[j],function(x) c(sum(x),length(x)))

yj = matrix(unlist(tapply(y[j],mon[j],function(x) c(sum(x),length(x)))),ncol=2,byrow=T)
####

#### creating generalized linear model based on the bayesian network variables.
summary(glm(yj~sqrt(datajj),family="binomial"))



