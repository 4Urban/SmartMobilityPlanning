#read data csv file
getwd()
HHdata <- read.csv("class0916/Travel_diary_data.csv")
str(HHdata)


#create new column "TripL_modeG" that Groups by Mode
HHdata$TripL_modeG <- NA
str(HHdata$TripL_modeG)

#group by Mode
HHdata <- within(HHdata, {
  TripL_modeG[TripL_mode == 1] <- 6 #walk
  TripL_modeG[TripL_mode >= 2 & TripL_mode <= 3] <- 1 #car
  TripL_modeG[TripL_mode >= 4 & TripL_mode <= 9] <- 2 #bus
  TripL_modeG[TripL_mode >= 10 & TripL_mode <= 13] <- 3 #rail
  TripL_modeG[TripL_mode == 14] <- 4 #taxi
  TripL_modeG[TripL_mode >= 17 & TripL_mode <= 18] <- 5 #bike
  TripL_modeG[TripL_mode %in% c(15:16, 19:21)] <- 7 #others
})

#Gender => 1: male; 2: female
Table_TripL_modeG_Gender <- table(HHdata$TripL_modeG, HHdata$Gender)
Table_TripL_modeG_Gender_P <- prop.table(Table_TripL_modeG_Gender, margin = 2) * 100
addmargins(Table_TripL_modeG_Gender_P)

barplot(Table_TripL_modeG_Gender_P, main = "Travel mode share by Gender", xlab = "Gender", ylab = "Mode Share", names.arg = c("Male", "Female"), legend = c("Car", "Bus", "Rail", "Taxi", "Bike", "Walk", "Others"), args.legend = list(x=3, y=64), beside = TRUE)



#create new column "Trip_time"
HHdata$Trip_time <- (((HHdata$Trip_a_P-1)*12+HHdata$Trip_a_hh)*60+HHdata$Trip_a_mm)-
  (((HHdata$Trip_d_P-1)*12+HHdata$Trip_d_hh)*60+HHdata$Trip_d_mm)

Table_Trip_time_Gender <- table(HHdata$Trip_time, HHdata$Gender)
addmargins(Table_Trip_time_Gender)

#get density grouped by Gender
# Trip_time_Male_den <- density(na.omit(HHdata$Trip_time[HHdata$Gender == 1]))
# Trip_time_Female_den <- density(na.omit(HHdata$Trip_time[HHdata$Gender == 2]))
# plot(Trip_time_Male_den)
# plot(Trip_time_Female_den)

#compare density by Gender
library(sm)
sm.density.compare(HHdata$Trip_time, HHdata$Gender, xlab="Travel time", xlim=c(-10, 150), lwd = 2)

summary(HHdata$Trip_time[HHdata$Gender == 1])
summary(HHdata$Trip_time[HHdata$Gender == 2])

title(main="Travel time distribution by Gender")
Gender.f <- factor(HHdata$Gender, levels=c(1:2), labels=c("Male", "Female"))
colfill <- c(2:(2+length(levels(Gender.f))))
legend(125, 0.02, levels(Gender.f), fill = colfill)
