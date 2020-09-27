
#traffic fatality data;
traffic<-read.csv("Fatalities.csv", header = T)

plot(traffic$unemp, traffic$income, col="blue",ylab= "Income",xlab="Unemployment rate")


#drug data;
marijuana<-c(22,17,40,5,37,19,23,6,7,53)
otherdrug<-c(4,3,21,1,16,8,14,3,3,31)

plot(marijuana, otherdrug, col="blue",ylab= "Other Drugs (%)",xlab="Marijuana %")
abline(a=-3.019,b=0.586)

mean(marijuana)
sd(marijuana)
mean(otherdrug)
sd(otherdrug)

cor(marijuana,otherdrug)
