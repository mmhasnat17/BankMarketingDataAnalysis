#Mohammed Hasnat
#CS544 Final Project
#Analyis of Bank Marketing Data Set | Deposit Analysis


#Intialize Data set from Kaggle
#Main Source: https://www.kaggle.com/janiobachmann/bank-marketing-dataset
library(readr)
bankdataset <- read_csv("/Volumes/Academics/BostonUniversity/Fall2019/CS544FoundAnalyticswithR/CS544_FinalProject_Hasnat/bank.csv")
head(bankdataset,n = 3)
is.null(bankdataset)
str(bankdataset)

#Do the analysis as in Module 3 for at least one categorical variable and at least one numerical variable. Show appropriate plots for your data.

library(UsingR)
#categorical for deposit
barplot(table(bankdataset$deposit), col="cyan", ylim=c(0,5000),
         xlab="Deposit",ylab="Frequency")

#numerical for age
fivenum(bankdataset$age)

boxplot(bankdataset$age, col=hcl(0),xaxt="n",
        xlab="Age Distribution", horizontal=TRUE)
axis(side=1, at=fivenum(bankdataset$age), labels=TRUE,las=2)
text(fivenum(bankdataset$age),rep(1.2,5),srt=90,adj=0,
     labels=c("Min","Lower","Median","Upper","max"))

ggplot(bankdataset, aes(x = bankdataset$age)) +
  geom_density(fill = "deepskyblue", 
               bw = 1) + 
  labs(title = "Participants by age",
       subtitle = "bandwidth = 1")



#Martial Staus and Deposit
unique(bankdataset$marital)
martialstatus_deposit<-table(bankdataset$deposit,bankdataset$marital)
mosaicplot(martialstatus_deposit, color = c("red","blue","green"), xlab="Deposit", ylab="Martial Status")

ggplot(bankdataset, 
       aes(x = bankdataset$deposit, 
           fill = bankdataset$marital)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")

#Education and Deposit

ggplot(bankdataset, 
       aes(x = bankdataset$deposit, 
           fill = bankdataset$education)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")


#Do the analysis as in Module 3 for at least one set of two or more variables. Show appropriate plots for your data. 

library(tidyverse)
filtered_data_deposit <- filter(bankdataset, bankdataset$deposit == "yes")
filtered_data_deposit

#Age and Balance
# simple scatterplot

length(filtered_data_deposit$age)

fivenum(filtered_data_deposit$age)

ggplot(filtered_data_deposit, 
       aes(x = filtered_data_deposit$age, 
           y = filtered_data_deposit$balance)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")

#Pick one variable with numerical data and examine the distribution of the data. 

 
library(plyr)
y <- count(filtered_data_deposit$age)
df <- as.data.frame(y)
df1 <- df[order(df$freq),]
df1

ggplot(df1, 
       aes(x = df1$x, 
           y = df1$freq)) +
  geom_bar(stat = "identity")



# Probability distribution of Random Variable
f<-df1$freq/(sum(df1$freq))
chance<-f*100
chance

#Draw various random samples of the data and show the applicability of the Central Limit Theorem for this variable


filtered_data_deposit_under60<- filter(filtered_data_deposit, filtered_data_deposit$age < 60)

mean<-mean(filtered_data_deposit_under60$age)
sd<-sd(filtered_data_deposit_under60$age)
hist(filtered_data_deposit_under60$age, prob=TRUE, col="blue")

length <- length(filtered_data_deposit_under60$age)

sample.size <- 500
xbar1 <- numeric(length)
for(i in 1:length){
  xbar1[i] <- mean(rnorm(sample.size,mean=mean,sd=sd))
}
hist(xbar1, prob=TRUE, xlab = "X",col="red",breaks=3)


sample.size <- 1000
xbar2 <- numeric(length)
for(i in 1:length){
  xbar2[i]<-mean(rnorm(sample.size,mean=mean,sd=sd))
}
hist(xbar2, prob=TRUE,xlab = "X", col="red",breaks=3)


#Mean and SD of original data
mean(filtered_data_deposit_under60$age)
sd(filtered_data_deposit_under60$age)

#Mean and SD of sample size 500
mean(xbar1)
sd(xbar1)

#Mean and SD of sample size 1000
mean(xbar2)
sd(xbar2)

#Show how various sampling methods can be used on your data. What are your conclusions if these samples are used instead of the whole dataset. 


#Simple Random Sampling
library(sampling)
head(filtered_data_deposit_under60,n=2)
sample.size <- 1500
data <- filtered_data_deposit_under60

s <- srswor(sample.size, nrow(data))
res <- data[s!=0,]
res

#balance
a = mean(res$balance)
a

reg = as.data.frame(table(res$age))
colnames(reg) = c("Age","Frequency")
print("Frequency of each age")
reg 

library(dplyr)

# plot reg
ggplot(res, 
       aes(x = res$age,
           y = res$balance)) +
  geom_violin() +
  labs(title = "Balance by age")




percent <- as.data.frame(prop.table(table(res$age)))
percent$Percentage <- percent$Freq*100
print("Percent of each region")
percent



#Systematic Sampling

N <- nrow(filtered_data_deposit_under60)
n <- sample.size
k <- ceiling(N/n)
r <- sample(k,1)
s <- seq(r,by=k,length=n)

res <- data[s!=0,]
#res <- data[s,]
res

#balance
b = mean(res$balance)
b
reg <- as.data.frame(table(res$age))
colnames(reg) <- c("Age","Frequency")
print("Frequency per region")
reg 

# plot reg
ggplot(res, 
       aes(x = res$age,
           y = res$balance)) +
  geom_violin() +
  labs(title = "Balance by age")

percent <- as.data.frame(prop.table(table(res$age)))
percent$Percentage <- percent$Freq*100
print("Percent per region")
percent

#Systematic with Inclusion probability
pik <- inclusionprobabilities(filtered_data_deposit_under60$duration,sample.size)
s <- UPsystematic(pik)
res <- data[s!=0,]
res

#balance
c <- mean(res$balance)
c
reg <- as.data.frame(table(res$age))
colnames(reg) <- c("Age","Frequency")
print("Frequency of each region")
reg 

# plot reg
ggplot(res, 
       aes(x = res$age,
           y = res$balance)) +
  geom_violin() +
  labs(title = "Balance by age")


percent <- as.data.frame(prop.table(table(res$age)))
percent$Percentage <- percent$Freq*100
print("Percent of each region")
percent

#Compare
print("Mean for a,b,c")
compare_mean <- c(a,b,c)
as.data.frame(compare_mean)


#Implementation of what not mentioned

fivenum(filtered_data_deposit_under60$balance)
filtered_data_deposit_under60_under5000 <- filter(filtered_data_deposit_under60, filtered_data_deposit_under60$balance < 5000)

ggplot(filtered_data_deposit_under60_under5000, aes(x = filtered_data_deposit_under60_under5000$balance)) +
  geom_histogram(color = "white",
                 fill = "cornflowerblue") +
  facet_grid(filtered_data_deposit_under60_under5000$marital ~ filtered_data_deposit_under60_under5000$education) +
  labs(title = "Balance histograms by education and martial status having a deposit",
       x = "Balance")


