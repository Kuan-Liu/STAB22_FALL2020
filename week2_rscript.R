#First exercise;
#Using Cereal data;
#summarizing quantitative variable;
#Step 1. Reading dataset;
cereal<-read.csv("cereal_nutrition.csv", header = T)

#Checking min and max of the calories per serving;
max(cereal$calories)
min(cereal$calories)
summary(cereal)

#plotting histogram with the number of bins set at 20;
library(lattice)

hist(cereal$calories, col="darkgrey", breaks = c(40,60,80,100,120,140,160),xlab="Calories per serving", main = "Histogram of Cereal Calories per Serving", labels = T, right = F)

h <- hist(cereal$calories, breaks = c(40,60,80,100,120,140,160), plot=FALSE,right = F)
h$counts=round((h$counts/sum(h$counts))*100,2)
plot(h,col="darkgrey", xlab="Calories per serving", ylab="Relative Frequency",main = "Histogram of Cereal Calories per Serving", labels = T)

#stem and leaf;
stem(cereal$calories,scale = 2) 

#dotplots;
stripchart(cereal$calories, method = "stack", offset = .5, at =0, pch = 19, vertical=T,
           main = "Dotplot of Calories per serving", xlab = "Calories per serving")

#mean, median for calories per serving;
summary(cereal$calories)

#histogram adding mean and median
hist(cereal$calories, col="darkgrey", breaks = c(40,60,80,100,120,140,160),xlab="Calories per serving", main = "Histogram of Cereal Calories per Serving", right = F)
abline(v = mean(cereal$calories), col="red", lwd=3, lty=2)
abline(v = median(cereal$calories), col="blue", lwd=3, lty=2)


#Boxplot;
IQR <-quantile(cereal$calories, probs =0.75) - quantile(cereal$calories, probs =0.25)
upper_fence <- quantile(cereal$calories, probs =0.75) + 1.5*IQR
lower_fence <- quantile(cereal$calories, probs =0.25) - 1.5*IQR

boxplot(cereal$calories,
        main = "Boxplot of Cereal Calories per Serving",
        ylab = "Calories per serving",
        col = "grey")
abline(h = upper_fence, col="red", lwd=2, lty=2)
abline(h = lower_fence, col="blue", lwd=2, lty=2)


#Second exercise;
#Using Titanic data;
#Comparing quantitative data bv groups;
#Titanic revist, age by survival status;
Titanic<- read.csv("Titanic.csv")
#keep only observations with complete data;
Titanic_c<-Titanic[complete.cases(Titanic),]

hist(Titanic_c$Age[Titanic_c$Survived==1], col="darkgrey", xlab="Age", 
     main = "Histogram of age among survivors", right = F, ylim=c(0,100),xlim = c(0,80))
hist(Titanic_c$Age[Titanic_c$Survived==0], col="darkgrey", xlab="Age", 
     main = "Histogram of age among non-survivors",
     breaks = 16, right = F, ylim=c(0,100),xlim = c(0,80))


tapply(Titanic_c$Age, Titanic_c$Survived, summary)

boxplot(Age ~ Survived, data = Titanic_c,
        main = "Boxplot of age by survival status",
        ylab = "Age",
        col = "grey")


boxplot(Age ~ PClass, data = Titanic_c,
        main = "Boxplot of age by passenger class",
        ylab = "Age",
        col = "grey")

#now let's try in Rcmdr, number of breaks=6;
# library(Rcmdr)
