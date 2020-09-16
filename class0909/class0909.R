temperature <- c(36.2, 38.1, 36.7)
subject_name <- c("Kim", "Steve", "Jane")
flu_status <- c(FALSE, TRUE, FALSE)

temperature[2]
temperature[2:3]
temperature[1:3]
temperature[c(1,3)]
temperature[flu_status]

print(c(1,3))

#Nominal Value Vector
gender <- factor(c("male", "female", "male"))
blood <- factor(c("O", "AB", "A"), levels = c("A", "B", "AB", "O"))

#Ordinal Value Vector
symptons <- factor(c("mild", "severe", "moderate"), levels = c("mild", "moderate", "severe"), ordered = TRUE)
symptons
symptons > "moderate"
subject_name[symptons > "moderate"]

pt_data <- data.frame(subject_name, temperature, flu_status, gender, blood, symptons, stringsAsFactors = FALSE)
pt_data
pt_data$subject_name
pt_data[c("temperature", "flu_status")]
pt_data[1,2]
pt_data[c(1,3), c(2,4)]
pt_data[,1]
pt_data[1,]
pt_data[,]
pt_data[c(1,3), c("temperature", "gender")]

aggregate(pt_data$temperature, by=list(pt_data$flu_status), mean)

Tdata <- read.csv("class0909/HH_trips_data.csv")
typeof(Tdata)
str(Tdata)
summary(Tdata)

Myvars <- c("HHsize", "HHincome", "NofTrips", "Auto_share")
summary(Tdata[Myvars])

Tdata$Region
str(Tdata$Region)
summary(Tdata$Region)
table(Tdata$Region)
table(Tdata$Region, Tdata$Car_own)

Mytable <- table(Tdata$Region,Tdata$Car_own)
addmargins(Mytable) #Show "Sum" Information

addmargins(prop.table(Mytable))
prop.table(Mytable, 1)
prop.table(Mytable, 2)

aggregate(Tdata[Myvars], by=list(Tdata$Region), mean)
aggregate(Tdata[Myvars], by=list(Tdata$Region), sd)

install.packages("vcd")
library("vcd")

counts <- table(Tdata$Region)
barplot(counts, main="Simple Bar Plot", xlab ="Region", ylab ="Frequency")

hist(Tdata$NofTrips)        

d <- density(Tdata$NofTrips)
plot(d)

install.packages("sm")
library(sm)

attach(Tdata)
Car_own.f <- factor(Car_own, levels=c(1,2), labels=c("Own","No car"))
sm.density.compare(NofTrips, Car_own, xlab="Number of trips per houshold")
title(main="Number of trip distribution by car ownership")
colfill <-c(2:(1+length(levels(Car_own.f))))
legend(40,0.1,levels(Car_own.f), fill=colfill)
detach(Tdata)

boxplot(Tdata$NofTrips, data=Tdata, main="Number of trips")
boxplot(Tdata$NofTrips ~ Tdata$Region, main="Number of trips by region", xlab="Region", ylab="Number of trips")

plot(Tdata$Auto_share, Tdata$NofTrips)

attach(Tdata)
plot(Auto_share,Transit_share)
abline(lm(Transit_share~Auto_share))
pairs(~Auto_share+Transit_share+NofTrips)
detach(Tdata)
