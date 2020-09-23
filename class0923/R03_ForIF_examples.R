# Cumulative sum from 1 to 10

y<-0
for(n in 1:10) {
  y <- y+n
}

# Print repeatedly

x<-1:5
for(n in x){
  print(n)
}



# If

z<-5
if(z>3){
  print("z is greater than 3")
}else{
  print("z is less than 3")
}


# for & if 
z<-c(2,5,4,6,7,1)
for(n in 1:6){
  if(z[n]>3){
    print("z is greater than 3")
  }else{
    print("z is less than 3")
  }
}


# count if 
Data<-c("car","bus","car","bus","bus","car","car","bus","car","car")
Mode<-c("car","bus")
Count<-data.frame(Mode)

Count$Freq<-0
for(n in 1:length(Data)){
  if(Data[n]=="car"){
    Count$Freq[1] <- Count$Freq[1]+1
  }else{
    Count$Freq[2] <- Count$Freq[2]+1
  }
}
