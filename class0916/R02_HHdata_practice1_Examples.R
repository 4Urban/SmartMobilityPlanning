# EX2
HHdata <- read.csv("C:/Users/user/Documents/R_data/Travel_diary_data.csv")

# EX3  
summary(HHdata)

# EX4
HHdata$Age <- 2016-HHdata$Byear

# EX5
HHdata$AgeG <- NA
HHdata <- within(HHdata, {
  AgeG[Age<20] <- 1
  AgeG[Age>=20 & Age<30] <- 2
  AgeG[Age>=30 & Age<40] <- 3
  AgeG[Age>=40 & Age<50] <- 4
  AgeG[Age>=50 & Age<60] <- 5
  AgeG[Age>=60] <- 6
})


# EX6
HHdata$Trip_purpG <- NA
HHdata <- within(HHdata, {
  Trip_purpG[Trip_purp==4] <- 1 #work
  Trip_purpG[Trip_purp==5|Trip_purp==6] <- 2 #education
  Trip_purpG[Trip_purp==2|Trip_purp==7] <- 3 #business
  Trip_purpG[Trip_purp==8|Trip_purp==9|Trip_purp==10|Trip_purp==11] <- 4 #shopping/social/leisure
  Trip_purpG[Trip_purp==3] <- 5 #back home
  Trip_purpG[Trip_purp==1|Trip_purp==12] <- 6 #other
})


# EX7
HHdata$TripL_modeG <- NA
HHdata <- within(HHdata, {
  TripL_modeG[TripL_mode==2|TripL_mode==3] <- 1 #car
  TripL_modeG[TripL_mode>=4 & TripL_mode<=9] <- 2 #bus
  TripL_modeG[TripL_mode>=10 & TripL_mode<=13] <- 3 #rail
  TripL_modeG[TripL_mode==14] <- 4 #taxi
  TripL_modeG[TripL_mode==17|TripL_mode==18] <- 5 #bike
  TripL_modeG[TripL_mode==1] <- 6 #walk
  TripL_modeG[TripL_mode==15|TripL_mode==16|TripL_mode==19|TripL_mode==20|TripL_mode==21] <- 7 #other
})


# EX8
HHdata$TripL_time <- (((HHdata$TripL_a_P-1)*12+HHdata$TripL_a_hh)*60+HHdata$TripL_a_mm) - (((HHdata$TripL_d_P-1)*12+HHdata$TripL_d_hh)*60+HHdata$TripL_d_mm)

# EX9
HHdata$Trip_time <- (((HHdata$Trip_a_P-1)*12+HHdata$Trip_a_hh)*60+HHdata$Trip_a_mm) - (((HHdata$Trip_d_P-1)*12+HHdata$Trip_d_hh)*60+HHdata$Trip_d_mm)


# EX12
Mode_share_ageG <-table(HHdata$TripL_modeG,HHdata$AgeG)
Mode_shareP_ageG=prop.table(Mode_share_ageG,2)*100


# EX13
barplot(Mode_shareP_ageG,main="Travel mode share", 
        xlab="Age group", ylab="Percentage", 
        names.arg=c("under 20","20-29","30-39","40-49","50-59","60 or older"),
        legend=c("Car","Bus","Rail","Taxi","Bike","Walk","Others"), beside=TRUE)

# EX14
TripL_time_by_mode=aggregate(HHdata$TripL_time, by=list(Mode=HHdata$TripL_modeG),mean)
barplot(TripL_time_by_mode[,2],main="Average travel time", 
        xlab="Travel mode", ylab="Minutes", 
        names.arg=c("Car","Bus","Rail","Taxi","Bike","Walk","Others"))

# EX15
hist(HHdata$TripL_time,breaks=100)
hist(HHdata$TripL_time,breaks=100,xlim= c(0,100))

TripL_time_den <- density(na.omit(HHdata$TripL_time))
plot(TripL_time_den,xlim = c(0,100))


# EX16
install.packages("sm")
library(sm)

sm.density.compare(HHdata$TripL_time, HHdata$TripL_modeG, xlab="Travel time",xlim=c(-10,100))

title(main="Travel time distribution by travel mode")

TripL_modeG.f <- factor(HHdata$TripL_modeG, levels=c(1,2,3,4,5,6,7), labels=c("Car","Bus","Rail","Taxi","Bike","Walk","Other"))
colfill <-c(2:(1+length(levels(TripL_modeG.f))))
legend(80,0.077, levels(TripL_modeG.f), fill=colfill)


