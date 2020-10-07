Cdata <- read.csv("class1007/Transit_card.csv")

table(Cdata$User)

#The number of Trips by User and Mode
NoTrips_User_Mode = table(Cdata$User, Cdata$Mode)
addmargins(NoTrips_User_Mode)

#Percentage of Trips by User and Mode
NoTrips_User_Mode.p = prop.table(NoTrips_User_Mode, 1) * 100
addmargins(NoTrips_User_Mode.p)


# #calculate IVT(: In-Vehicle Travel time)
# Cdata$IVT <- (Cdata$Off_hour*60 + Cdata$Off_min + Cdata$Off_sec/60) - (Cdata$On_hour*60 + Cdata$On_min + Cdata$On_sec/60)
# hist(Cdata$IVT, breaks=100)
# 
# #find error
# Error <- Cdata[Cdata$IVT<0, ] #Check Column index(-> blank)
# summary(Error)
# table(Error$Off_hour)
# 
# Cdata$IVT[Cdata$IVT<0] <- Cdata$IVT + 24*60

#compare hourly distributions among User
Time_dist_User = table(Cdata$On_hour,Cdata$User)
Time_dist_User.p = prop.table(Time_dist_User, 2) * 100
addmargins(Time_dist_User.p)

Timeofday = c(0:23)
plot(Timeofday, Time_dist_User.p[, 1], type="o", lwd=2, xlab="Time of day", ylab="%", ylim=c(0, max(Time_dist_User.p)))
for(i in c(2:ncol(Time_dist_User))){
  lines(Timeofday, Time_dist_User.p[, i], type="b", lwd=2, col=i)
}
legend('topright', colnames(Time_dist_User), lwd=2, col=c(1:ncol(Time_dist_User)))
