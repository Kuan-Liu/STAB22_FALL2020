#First exercise, 
#use comparing histogram and box-plot at identifying distribution shape;
par(mfrow = c(1, 2))

#1. uniformly distributed 
dat_uni<-runif(500, min = 0.5, max=0.5)
hist(dat_uni, breaks = 20)
boxplot(dat_uni)

#2. symmetric normal distribution 
dat_norm<-rnorm(500, mean = 0.5, sd = 0.1)
hist(dat_norm, breaks = 20)
boxplot(dat_norm)

#3.right skewed;
dat_right <- rbeta(500,2,6)
hist(dat_right, breaks = 20)
boxplot(dat_right)

#4.left skewed;
dat_left <- rbeta(500,6,2)
hist(dat_left, breaks = 20)
boxplot(dat_left)

#5. bimodal
dat_norm1<-rnorm(250, mean = 0.1, sd = 0.1)
dat_norm2<-dat_norm[1:250]
dat_bimodal<-c(dat_norm1,dat_norm2)
hist(dat_bimodal)
boxplot(dat_bimodal) #you can't tell if this is bimodal from box-plot!

#Second exercise, 
#explain why we wish to standardize
set.seed(123)
x<-rnorm(n=100, mean=65, sd=12) 
y<-rnorm(n=100, mean=60, sd=9)

hist(x, breaks = 10, xlim = c(0,100), labels = T, right = F, xlab="", main = "Histogram of grades from 100 students taking class A")
abline(v=85, col="red", lty=2)
hist(y,xlim = c(0,100), labels = T, right = F, xlab="", main ="Histogram of grades from 100 students taking class B")
abline(v=75, col="red", lty=2)

#Third exercise, Histogram on cereal calories with density curve
cereal<-read.csv("cereal_nutrition.csv", header = T)
par(mfrow = c(1, 1))
h<-hist(cereal$calories, col="darkgrey", breaks = 10,
    xlim = c(40,160),
     xlab="Calories per serving", 
     main = "Histogram of Cereal Calories per Serving", 
     right = F)

xfit<-seq(min(cereal$calories),max(cereal$calories),length=40)
yfit<-dnorm(xfit,mean=mean(cereal$calories),sd=sd(cereal$calories))
yfit <- yfit*diff(h$mids[1:2])*length(cereal$calories)
lines(xfit, yfit, col="red", lwd=2)


#Fourth exercise, plotting different normal distributions
x <- seq(0, 20, length=1000)
y <- dnorm(x, mean=10, sd=2)
plot(x, y, type="l", lwd=1, main = "Normal Dist (mean = 10, sd = 2)", 
     xlab="",ylab ="Density" )

#same sd but different mean
y2 <- dnorm(x, mean=8, sd=2)
plot(x, y, type="l", lwd=1, main = "Normal Dist 1 (mean = 10, sd = 2) \n Normal Dist 2 (mean = 8, sd = 2)", 
     xlab="",ylab ="Density" )
lines(x, y2, type="l", lwd=1, col="red")

#same mean but different sd
y3 <- dnorm(x, mean=10, sd=4)
plot(x, y, type="l", lwd=1, main = "Normal Dist 1 (mean = 10, sd = 2) \n Normal Dist 2 (mean = 10, sd = 4)", 
     xlab="",ylab ="Density" )
lines(x, y3, type="l", lwd=1, col="red")

#different mean and sd
y4 <- dnorm(x, mean=8, sd=4)
plot(x, y, type="l", lwd=1, main = "Normal Dist 1 (mean = 10, sd = 2) \n Normal Dist 2 (mean = 8, sd = 4)", 
     xlab="",ylab ="Density" )
lines(x, y4, type="l", lwd=1, col="red")

#Fifth exercise, getting proportion giving a nomral dist or a standard normal dist
pnorm(70, mean = 65, sd = 12,lower.tail = TRUE)
pnorm(0.42, mean = 0, sd = 1,lower.tail = TRUE) #rounding issue;
pnorm(0.417, mean = 0, sd = 1,lower.tail = TRUE) #rounding issue;

pnorm(62, mean = 60, sd = 9,lower.tail = TRUE)
pnorm(0.22, mean = 0, sd = 1,lower.tail = TRUE) #rounding issue;
pnorm(0.222, mean = 0, sd = 1,lower.tail = TRUE) #rounding issue;

pnorm(-0.42, mean = 0, sd = 1,lower.tail = TRUE) #P(z<-0.42)
pnorm(-0.42, mean = 0, sd = 1,lower.tail = FALSE) #P(z>-0.42)

pnorm(0.42, mean = 0, sd = 1,lower.tail = TRUE) #P(z<0.42)
pnorm(0.42, mean = 0, sd = 1,lower.tail = FALSE) #P(z>0.42)
pnorm(0.42, mean = 0, sd = 1,lower.tail = TRUE)+pnorm(0.42, mean = 0, sd = 1,lower.tail = FALSE) #P(z<0.42)+P(z>0.42)=1

#reverse table;
qnorm(0.9, mean = 0, sd = 1,lower.tail = TRUE) #find z such that P(<z)=0.9

#normal probability plot
par(mfrow = c(1, 2))
hist(dat_norm, breaks = 20)
qqnorm(dat_norm, pch = 1, frame = FALSE)
qqline(dat_norm, col = "steelblue", lwd = 2)

hist(dat_right, breaks = 20)
qqnorm(dat_right, pch = 1, frame = FALSE)
qqline(dat_right, col = "steelblue", lwd = 2)

hist(dat_left, breaks = 20)
qqnorm(dat_left, pch = 1, frame = FALSE)
qqline(dat_left, col = "steelblue", lwd = 2)




