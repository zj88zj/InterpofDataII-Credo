CLPS$time<-monthnum

length(unique(Category.Name))
four<-filter(CLPS, CLPS$Requester.Supervisor.WWID==4093)
i = CLPS$Requester.Supervisor.WWID == supID[4]
j = i
j[is.na(i)]=F
modelm = model.matrix(~.,data.frame(four$Category.Name,four$Subcategory.Name, four$PO.Total.Amount))[,-1, drop = F]
dataJJ=apply(modelm,2,function(x,u) tapply(x,u,sum),u=four$time)
jjj<-apply(dataJJ!=0,2,sum) > 3
dataJJmore = dataJJ[,jjj]
yj = tapply(as.numeric(four$Credo.Spend.Ind),four$time,function(x) c(sum(x),length(x)))
summary(glm(yj~sqrt(dataJJmore),family="binomial"))
yj = matrix(unlist(tapply(as.numeric(four$Credo.Spend.Ind)-1,four$time,function(x) c(sum(x),length(x)))),ncol=2,byrow=T)
summary(glm(yj~sqrt(dataJJmore)+yj,family="binomial"))
predict(bb,matrix(c(data2month[18,],yj[18,1]/yj[18,2])), nrow=1)
datee <- unlist(strsplit(as.character(subset$PO.Create.Date)," "))
datee <- datee[2*(1:(length(date)/2))-1]
datee <- as.Date(date,"%m/%d/%Y")

#Redefines dates in terms of months since time 0
basedatee <- as.numeric(strsplit(as.character(min(datee)),"-")[[1]][1:2])
datematrixx <- matrix(as.numeric(unlist(strsplit(as.character(datee),"-"))),ncol=3,byrow=T)
monthh <- datematrixx[,2] # month of the year
totalmonthss <- (datematrixx[,1]-basedatee[1])*12+datematrixx[,2]-basedatee[2]+1 # Months since the begining of the data

#creates table that counts each type of cat and sub every 2 months
monthnumm = (datematrixx[,1]-2010)*12 + datematrixx[,2]

xx<-cbind(unique(sort(four$time, decreasing=F)),yj[,1]/yj[,2])
xx<-cbind(dataJJmore)
yy<- yj[,1]/yj[,2]
qqq=cv.glmnet(xx, yy,alpha=1)
plot(qqq)
bb= glmnet(xx,yy,lambda=c(exp(c(-4,-3))),alpha=1)
xf=xx[,bb[,1]!=0]
bblm = lm(yy~xx)
step(bblm)
summary(step(bblm))
predict(bb,matrix(c(time[17,],yj[17,1]/yj[17,2])), nrow=1)

plot(date[j],size[j],pch=20,cex=1,col=3, main="Size by Date")  
