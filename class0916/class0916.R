getwd()
HHdata <- read.csv("class0916/Travel_diary_data.csv")
summary(HHdata)
str(HHdata$HH_ID)

#create 'Age' column with calculated data
HHdata$Age <- 2016-HHdata$Byear

#create 'Age Group' column with blank value
HHdata$AgeG <- NA

#Age Group: divided by 10 years
HHdata <- within(HHdata, {
  AgeG[Age<20] <- 1
  AgeG[Age>=20 & Age<30] <- 2
  AgeG[Age>=30 & Age<40] <- 3
  AgeG[Age>=40 & Age<50] <- 4
  AgeG[Age>=50 & Age<60] <- 5
  AgeG[Age>=60] <- 6
})

#Trip Purpose Group
HHdata$Trip_purpG <- NA
HHdata <- within(HHdata, {
  Trip_purpG[Trip_purp==4] <- 1 #work
  Trip_purpG[Trip_purp==5|Trip_purp==6] <- 2 #education
  Trip_purpG[Trip_purp==2|Trip_purp==7] <- 3 #business
  Trip_purpG[Trip_purp==8|Trip_purp==9|Trip_purp==10|Trip_purp==11] <- 4 #shopping/social/leisure
  Trip_purpG[Trip_purp==3] <- 5 #back home
  Trip_purpG[Trip_purp==1|Trip_purp==12] <- 6 #other
})

#attach() only applies to list, data.frame, and environment

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

HHdata$TripL_time <- (((HHdata$TripL_a_P-1)*12+HHdata$TripL_a_hh)*60+HHdata$TripL_a_mm) - (((HHdata$TripL_d_P-1)*12+HHdata$TripL_d_hh)*60+HHdata$TripL_d_mm)
HHdata$Trip_time <- (((HHdata$Trip_a_P-1)*12+HHdata$Trip_a_hh)*60+HHdata$Trip_a_mm) - (((HHdata$Trip_d_P-1)*12+HHdata$Trip_d_hh)*60+HHdata$Trip_d_mm)

Mode_share <- table(HHdata$TripL_modeG)
prop.table(Mode_share)*100
#share of bus = 15.1875661%
#share of walk = 59.8291155%
#So calculate mode share excluding trips by walk!

Mode_share_nowalk <- table(HHdata$TripL_modeG[HHdata$TripL_modeG!=6])
prop.table(Mode_share_nowalk)*100
#share of bus = 37.8073978%

Mode_share_ageG <-table(HHdata$TripL_modeG,HHdata$AgeG)
Mode_shareP_ageG=prop.table(Mode_share_ageG,2)*100

barplot(Mode_shareP_ageG,main="Travel mode share", 
        xlab="Age group", ylab="Percentage", 
        names.arg=c("under 20","20-29","30-39","40-49","50-59","60 or older"),
        legend=c("Car","Bus","Rail","Taxi","Bike","Walk","Others"), beside=TRUE)

TripL_time_by_mode=aggregate(HHdata$TripL_time, by=list(Mode=HHdata$TripL_modeG),mean)
barplot(TripL_time_by_mode[,2],main="Average travel time", 
        xlab="Travel mode", ylab="Minutes", 
        names.arg=c("Car","Bus","Rail","Taxi","Bike","Walk","Others"))

hist(HHdata$TripL_time,breaks=100)
hist(HHdata$TripL_time,breaks=100,xlim= c(0,100)) #only show below 100 := zoom in!

TripL_time_den <- density(na.omit(HHdata$TripL_time))
plot(TripL_time_den,xlim = c(0,100))

library(sm)

sm.density.compare(HHdata$TripL_time, HHdata$TripL_modeG, xlab="Travel time",xlim=c(-10,100))

title(main="Travel time distribution by travel mode")

TripL_modeG.f <- factor(HHdata$TripL_modeG, levels=c(1,2,3,4,5,6,7), labels=c("Car","Bus","Rail","Taxi","Bike","Walk","Other"))
colfill <-c(2:(1+length(levels(TripL_modeG.f))))
legend(80,0.077, levels(TripL_modeG.f), fill=colfill)
