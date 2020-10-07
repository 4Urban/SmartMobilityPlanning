# EX2
Cdata <- read.csv("class1007/Transit_card.csv")

# EX3
summary(Cdata)

# EX4
table(Cdata$User)
table(Cdata$Mode)

# EX5
Cdata$IVT <- ((Cdata$Off_hour)*60+(Cdata$Off_min)+(Cdata$Off_sec)/60)-((Cdata$On_hour)*60+(Cdata$On_min)+(Cdata$On_sec)/60)

# EX6
hist(Cdata$IVT,breaks=100)

# EX7
Time_error=Cdata[Cdata$IVT<0,]

summary(Time_error)

table(Time_error$Off_hour)

Time_error2=Time_error[Time_error$Off_hour==1,]
summary



# EX8
attach(Cdata)
Cdata$IVT[IVT<0] <- 
  ((24+Off_hour[IVT<0])*60+(Off_min[IVT<0])+(Off_sec[IVT<0])/60)-
  ((On_hour[IVT<0])*60+(On_min[IVT<0])+(On_sec[IVT<0])/60)
detach(Cdata)

hist(Cdata$IVT,breaks=100)

summary(Cdata)

# EX9
aggregate(Cdata$IVT, by=list(Mode=Cdata$Mode),mean)


# EX10
plot(density(Cdata$IVT[Cdata$Mode=="bus"]),xlim = c(0,100))
lines(density(Cdata$IVT[Cdata$Mode=="subway"]),col=2)

# EX11
Time_dist_mode=table(Cdata$On_hour,Cdata$Mode)
Time_dist_mode.p=prop.table(Time_dist_mode,2)*100

Timeofday=c(0:23)
plot(Timeofday,Time_dist_mode.p[,1],type="o",xlab="Time of day",ylab="%",ylim = c(0,max(Time_dist_mode.p)))
lines(Timeofday,Time_dist_mode.p[,2],type="b",col=2)
legend('topright',c("bus","subway"),lwd=c(1,1),col=c("black","red"))


# EX12
Station <- read.csv("class1007/station_code_data.csv")

# EX13
Station.on <- data.frame(table(Cdata$On_station))
Station.off <- data.frame(table(Cdata$Off_station))

names(Station.on) <- c("station_code","On_freq")
names(Station.off) <- c("station_code","Off_freq")

Station <- merge(x=Station,y=Station.on,by='station_code',all.x=TRUE)
Station <- merge(x=Station,y=Station.off,by='station_code',all.x=TRUE)

Station <- na.omit(Station)


# EX14
Station_order.on <- Station[order(-Station$On_freq),]

Station_order.on[1:10,]
