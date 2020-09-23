################ You need to run the following scripts before starting EX1 ################
HHdata <- read.csv("class0916/Travel_diary_data.csv")
HHdata$Age <- 2016-HHdata$Byear

HHdata$AgeG <- NA
HHdata <- within(HHdata, {
  AgeG[Age<20] <- 1
  AgeG[Age>=20 & Age<30] <- 2
  AgeG[Age>=30 & Age<40] <- 3
  AgeG[Age>=40 & Age<50] <- 4
  AgeG[Age>=50 & Age<60] <- 5
  AgeG[Age>=60] <- 6
})


HHdata$Trip_purpG <- NA
HHdata <- within(HHdata, {
  Trip_purpG[Trip_purp==4] <- 1 #work
  Trip_purpG[Trip_purp==5|Trip_purp==6] <- 2 #education
  Trip_purpG[Trip_purp==2|Trip_purp==7] <- 3 #business
  Trip_purpG[Trip_purp==8|Trip_purp==9|Trip_purp==10|Trip_purp==11] <- 4 #shopping/social/leisure
  Trip_purpG[Trip_purp==3] <- 5 #back home
  Trip_purpG[Trip_purp==1|Trip_purp==12] <- 6 #other
})


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

###########################################################################




#filter only purpose trip
Tripdata <- unique(HHdata[,c(1:32,46:48,51)])

#EX2
Purpose_share <-table(Tripdata$Trip_purpG)
prop.table(Purpose_share)*100

#EX3
Purpose_share_nobackH <- table(Tripdata$Trip_purpG[Tripdata$Trip_purpG!=5])
prop.table(Purpose_share_nobackH)*100

#EX4
Tirp_time_by_purp=aggregate(Tripdata$Trip_time, by=list(Purpose=Tripdata$Trip_purpG),mean)
barplot(Tirp_time_by_purp[,2],main="Average travel time", 
        xlab="Trip purpose", ylab="Minutes", 
        names.arg=c("Work","Education","Business","Shopping/Social/Leisure","Back home","Other"))


#EX5
Trip_time_den <- density(na.omit(Tripdata$Trip_time))
plot(Trip_time_den,xlim = c(0,100))


#the number of people (incl. person who didn't make any trips)
Pdata <- unique(Tripdata[,c(1:20,33:34)])

#EX7
nrow(Tripdata[Tripdata$Trip_made==1,])/nrow(Pdata)

#EX8
Pdata$NofTrips <- 0
Pdata$TripTime <- 0
k <- 1
for (n in 1:nrow(Tripdata)) {
  if((Pdata$HH_ID[k]==Tripdata$HH_ID[n])&(Pdata$P_ID[k]==Tripdata$P_ID[n])) {
    Pdata$NofTrips[k] <- Pdata$NofTrips[k]+1
    Pdata$TripTime[k] <- Pdata$TripTime[k]+Tripdata$Trip_time[n]
  } else {
    k <- k+1
    Pdata$NofTrips[k] <- Pdata$NofTrips[k]+1
    Pdata$TripTime[k] <- Pdata$TripTime[k]+Tripdata$Trip_time[n]
  }
}
Pdata <- within(Pdata, {
  NofTrips[Trip_made==1] <- NofTrips
  NofTrips[Trip_made==2] <- 0
})


#EX9
Pdata_temp <- na.omit(Pdata[,c("TripTime","AgeG")])
Trip_time_by_age=aggregate(Pdata_temp$TripTime, by=list(Mode=Pdata_temp$AgeG),mean)
barplot(Trip_time_by_age[,2],main="Travel time per day by age group", 
        xlab="Age group", ylab="Minutes", 
        names.arg=c("under 20","20-29","30-39","40-49","50-59","60 or older"))
