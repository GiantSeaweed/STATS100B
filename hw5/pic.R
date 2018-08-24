library(ggplot2)
#分位数图，画f分布密度带p值,F(0.05;40,12)=0.5
n1=40;
n2=12;
x=seq(0,10,length=1000);
y=df(x,n1,n2)
r1=0;
r2=0.5;
x2=c(r1,r1,x[x<r2&x>r1],r2,r2)
y2=c(0,df(c(r1,x[x<r2&x>r1],r2),n1,n2),0)
plot(x,y,type="l",
     ylab=paste("Density of F(",as.character(n1),",",as.character(n2),")"),
     xlim=c(0,10))
abline(h=0);
polygon(x2,y2,col="red")

title(paste("Density of F(",as.character(n1),",",as.character(n2), ")") );
text(5,0.5,c("F(0.05;40,12=0.5)"))

#F(0.95;12,40)=2
n1=12;
n2=40;
x=seq(0,10,length=1000);
y=df(x,n1,n2)
r1=0;
r2=2;
x2=c(r1,r1,x[x<r2&x>r1],r2,r2)
y2=c(0,df(c(r1,x[x<r2&x>r1],r2),n1,n2),0)
plot(x,y,type="l",
     ylab=paste("Density of F(",as.character(n1),",",as.character(n2),")"),
     xlim=c(0,10))
abline(h=0);
polygon(x2,y2,col="red")

title(paste("Density of F(",as.character(n1),",",as.character(n2), ")") );
text(5,0.5,c("F(0.95;12,40=2)"))


#F(0.95;1,60)=4
n1=1;
n2=60;
x=seq(0,4,length=1000);
y=df(x,n1,n2)
r1=0.00001;
r2=4;
x2=c(r1,r1,x[x<r2&x>r1],r2,r2)
y2=c(0,df(c(r1,x[x<r2&x>r1],r2),n1,n2),0)
plot(x,y,type="l",
     ylab=paste("Density of F(",as.character(n1),",",as.character(n2),")"),
     xlim=c(0,6))
abline(h=0);
polygon(x2,y2,col="red")

title(paste("Density of F(",as.character(n1),",",as.character(n2), ")") );
text(2.5,3.5,c("F(0.95;1,60=4)"))

#分位数图，画t分布密度,t(0.975;60)=2
n=60
x=seq(-6,6,length=1000);
y=dt(x,60)
r1=-2;
r2=2;
x2=c(r1,r1,x[x<r2&x>r1],r2,r2)
y2=c(0,dt(c(r1,x[x<r2&x>r1],r2),19),0)
plot(x,y,type="l",ylab="Density of t(60)",xlim=c(-5,5))
abline(h=0);polygon(x2,y2,col="red")

title("Density of t(60)")
text(2.8,0.3,c("t(0.975;60)=2"))

