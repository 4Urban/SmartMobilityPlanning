#read csv file
HHdata <- read.csv("class0916/Travel_diary_data.csv")

#get Age from Birth Year
HHdata$Age <- 2016-HHdata$Byear

#group by Age
HHdata$AgeG <- NA
HHdata <- within(HHdata, {
  AgeG[Age < 20] <- 1
  AgeG[Age >= 20 & Age < 30] <- 2
  AgeG[Age >= 30 & Age < 40] <- 3
  AgeG[Age >= 40 & Age < 50] <- 4
  AgeG[Age >= 50 & Age < 60] <- 5
  AgeG[Age >= 60] <- 6
})

AgeG_label <- c("under 20", "20-29", "30-39", "40-49", "50-59", "60 or older")

#group by Purpose
HHdata$Trip_purpG <- NA
HHdata <- within(HHdata, {
  Trip_purpG[Trip_purp == 4] <- 1 #work
  Trip_purpG[Trip_purp %in% c(5:6)] <- 2 #education
  Trip_purpG[Trip_purp %in% c(2, 7)] <- 3 #business
  Trip_purpG[Trip_purp %in% c(8:11)] <- 4 #shopping/social/leisure
  Trip_purpG[Trip_purp == 3] <- 5 #back home
  Trip_purpG[Trip_purp %in% c(1, 12)] <- 6 #other
})

Trip_purpG_label <- c("work", "education", "business", "shopping/social/leisure", "back home", "other")

#linked trip data
Tripdata <- unique(HHdata[,c(1:32,46:48)])
table(Tripdata$AgeG)
table(Tripdata$Trip_purpG)
str(na.omit(Tripdata$AgeG))
str(na.omit(Tripdata$Trip_purpG)) #NA exists in Trip_purpG

Trip_purp_Age <- table(Tripdata$Trip_purpG, Tripdata$AgeG)
addmargins(Trip_purp_Age)
Trip_purp_Age_P <- prop.table(Trip_purp_Age, margin = 2)*100
addmargins(Trip_purp_Age_P)

locator()

barplot(Trip_purp_Age_P, beside = TRUE, 
        main = "Distribution of Trip purpose by Age Group",
        names.arg = AgeG_label, legend.text = Trip_purpG_label, args.legend = list(x = 45.6, y = 69.5),
        ylim = c(0, 59),
        xlab = "Age group", ylab = "Percentage")

#person data
Tripdata$Trip_time <- (((Tripdata$Trip_a_P-1)*12+Tripdata$Trip_a_hh)*60+Tripdata$Trip_a_mm) - (((Tripdata$Trip_d_P-1)*12+Tripdata$Trip_d_hh)*60+Tripdata$Trip_d_mm)
Pdata <- unique(Tripdata[,c(1:20, 33:34)])

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

NofTrips_Age <- aggregate(Pdata$NofTrips, by = list(AgeG=Pdata$AgeG), sum)
barplot(NofTrips_Age[,2],
        main = "The number of Trips per day by Age Group",
        names.arg = AgeG_label,
        xlab = "Age group", ylab = "Number of Trips")

Pdata_temp <- na.omit(Pdata[,c("NofTrips","AgeG")])
AvgNofTrips_Age <- aggregate(Pdata_temp$NofTrips, by = list(AgeG=Pdata_temp$AgeG), mean)
barplot(AvgNofTrips_Age[,2],
        main = "The Average number of Trips per day by Age Group",
        names.arg = AgeG_label,
        xlab = "Age group", ylab = "Number of Trips")

#Kernel density distributions
library(sm)
sm.density.compare(Tripdata$Trip_time, Tripdata$Trip_purpG, xlab = "Travel time", xlim = c(-10, 100), lwd=2)

title(main = "Travel time distribution by Trip purpose")
Trip_purpG.f <- factor(Tripdata$Trip_purpG, levels=c(1:6),
                       labels=Trip_purpG_label)
colfill <- c(2:(1+length(levels(Trip_purpG.f))))
legend(65, 0.045, levels(Trip_purpG.f), fill = colfill)
