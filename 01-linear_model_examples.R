# simple linear model
data=read.table(file="http://eric.univ-lyon2.fr/~jjacques/Download/DataSet/serie1.txt")

plot(data$V1,type='l',xlim=c(1,120),ylim=c(1,80),xlab='time',ylab='')

t=1:100;x=data$V1
model=lm(x~t)
newt=data.frame(t=101:120)
p=predict(model,newt)
plot(t,x,type='l',xlim=c(1,120),ylim=c(1,80),xlab='time',
     ylab='')
lines(newt$t,p,col=2)

# change of trend (linear model not adapted because same weight for all datas)
data=read.table(file="http://eric.univ-lyon2.fr/~jjacques/Download/DataSet/serie2.txt")
plot(data$V1,type='l',xlim=c(1,120),ylim=c(1,80),xlab='time',ylab='')

t=1:100;x=data$V1
model=lm(x~t)
newt=data.frame(t=101:120)
p=predict(model,newt)
plot(t,x,type='l',xlim=c(1,120),ylim=c(1,80),xlab='time',
     ylab='')
lines(newt$t,p,col=2)
