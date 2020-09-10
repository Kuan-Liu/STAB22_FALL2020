# R code for week 1;

#First exercise
#1. What year of study are you in now?
# First year students, 184 respondents
# Second year student, 83 respondents
# Third year student, 36 respondents
# Fourth year and plus student,14 respondents
# In total 317 students have completed this question on our pre-course survey;

#Step 1. Creating frequency data;
counts<-c(184, 83, 36, 14) #create a vector storing all frequency counts;
year<-c("First","Second","Third","Fourth plus") #create a string vector to store the categories;

mydata<-data.frame(year, counts) #creating data with two vectors as columns;
# View(mydata) #view your data;

total_count <- sum(counts) #calculate the sum of all counts;

#calculate relative frequency in percentage and round the decimals;
mydata$rel_freq<- round(mydata$counts/total_count*100,2) 

print(mydata)


#Step 2. generating bar plot
library(ggplot2)

#this is a step added to order the categories by how they appear in data;
mydata$year <- factor(mydata$year, levels=mydata$year)

# Bar representing counts;
ggplot(data=mydata, aes(x=year, y=counts)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=counts),vjust=-1) +
  theme_minimal()+labs(x ="Year of Study", y = "Frequency counts")

# Bar representing relative frequency;
ggplot(data=mydata, aes(x=year, y=rel_freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=rel_freq),vjust=-1) +
  theme_minimal()+labs(x ="Year of Study", y = "Relative Frequency")

#Step 3. generating pie chart
ggplot(data=mydata, aes(x=0, y=counts, fill=year))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)+
  geom_text(aes(label=counts), hjust=1.5)+
  scale_fill_brewer(palette="Greens")+
  theme(axis.text=element_blank(), axis.ticks = element_blank(), panel.background = element_blank())+
  labs(x ="Year of Study", y = "Frequency counts")


#Second exercise
# Let's look at the Titanic passenger survival data;
# https://vincentarelbundock.github.io/Rdatasets/doc/Stat2Data/Titanic.html

Titanic<- read.csv("Titanic.csv")
summary(Titanic) #we have some missing data;

#keep only observations with complete data;
Titanic_c<-Titanic[complete.cases(Titanic),]
# dim(Titanic_c)
# [1] 756   7 #756 observations with no missing data;

#First contingency table let's look at survival by sex;
table(Titanic_c$Sex, Titanic_c$Survived)

#Second table, let's look at survival by passenger class;
table(Titanic_c$PClass, Titanic_c$Survived)

#Getting relative frequencies;
library(dplyr) #we this package to do data manipulation;

table1<-table(Titanic_c$Sex, Titanic_c$Survived)

#First calculating proportions by row sum
table1_rowp<- prop.table(table1, margin=1) %>% as.data.frame.matrix()
print.data.frame(table1_rowp)

#Second calculating proportions by col sum
table1_colp<- prop.table(table1, margin=2) %>% as.data.frame.matrix()
print.data.frame(table1_colp)

#last calculating proportions by total
table1_totp<- prop.table(table1) %>% as.data.frame.matrix()
print.data.frame(table1_totp)

#added bonus, getting row sums and column sums;
addmargins(table1)

#Try replicating prop by row, column, and total for table(Titanic_c$PClass, Titanic_c$Survived);
#sample code to get you started;
table2<-table(Titanic_c$PClass, Titanic_c$Survived)
table2_rowp<- prop.table(table2, margin=1) %>% as.data.frame.matrix()
table2_colp<- prop.table(table2, margin=2) %>% as.data.frame.matrix()
table2_totp<- prop.table(table2) %>% as.data.frame.matrix()
addmargins(table2)

#Pie chart and bar plots;
#To use Rcmdr, we need to make sure the variables are in the correct coded to it's attributes;
# str(Titanic_c)
Titanic_c$Survived<-as.factor(Titanic_c$Survived)

library(Rcmdr)
#First Pie chart of Passenger class by survival status;
with(Titanic_c[Titanic$Survived==1,], piechart(PClass, xlab="", ylab="", 
        main="PClass among survivor", col=rainbow_hcl(3), scale="frequency"))
with(Titanic_c[Titanic$Survived==0,], piechart(PClass, xlab="", ylab="", 
 main="PClass among non-survivor", col=rainbow_hcl(3), scale="frequency"))

#Second Pie chart of Sex by survival status;
with(Titanic_c[Titanic$Survived==1,], piechart(Sex, xlab="", ylab="", 
        main="Sex by survivor", col=rainbow_hcl(2), scale="frequency"))
with(Titanic_c[Titanic$Survived==0,], piechart(Sex, xlab="", ylab="", 
        main="Sex by non-survivor", col=rainbow_hcl(2), scale="frequency"))

#now switch frequency to percent, only doing this for Pclass by survival,
#try replicating Sex by survival yourself;
with(Titanic_c[Titanic$Survived==1,], piechart(PClass, xlab="", ylab="", 
        main="PClass among survivor", col=rainbow_hcl(3), scale="percent"))
with(Titanic_c[Titanic$Survived==0,], piechart(PClass, xlab="", ylab="", 
        main="PClass among non-survivor", col=rainbow_hcl(3), scale="percent"))


#bar plot;
with(Titanic_c, Barplot(PClass, by=Survived, style="parallel", legend.pos="above", xlab="Passenger Class", ylab="Frequency", 
                        label.bars=TRUE))
with(Titanic_c, Barplot(Sex, by=Survived, style="parallel", legend.pos="above", xlab="Sex", ylab="Frequency", 
                        label.bars=TRUE))

with(Titanic_c, Barplot(PClass, by=Survived, style="parallel", legend.pos="above", xlab="Passenger Class", ylab="Percent", 
                        label.bars=TRUE, scale="percent"))
with(Titanic_c, Barplot(Sex, by=Survived, style="parallel", legend.pos="above", xlab="Sex", ylab="Percent", 
                        label.bars=TRUE, scale="percent"))




