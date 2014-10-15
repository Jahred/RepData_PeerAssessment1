library(reshape2)

require(ggplot2)
require(plyr)


reprodata <- read.csv("activity.csv",header = TRUE)
totalStepPerDay <- sapply(split(reprodata,reprodata$date),function(x) sum(x$steps,na.rm=FALSE))


medianStepPerDay <- median(totalStepPerDay,na.rm=TRUE)
meanStepPerDay <- mean(totalStepPerDay,na.rm=TRUE)

vmedian_text=paste("median: ",medianStepPerDay)
vmean_text=paste("mean: ",meanStepPerDay)


hist(totalStepPerDay,col="light green",main="Histogram Total number of steps taken per day", xlab="steps")
abline(v=meanStepPerDay,col="red")
abline(v=medianStepPerDay,col="blue")
legend("topright", cex=0.4,col=c("red","blue"), lty=c(1,1), legend=c(vmedian_text,vmean_text))

dev.copy(png,file="histogram.png",width=480,height=480)
dev.off()

######### 2.Q
 #What is the average daily activity pattern?
#reprodata <- read.csv("activity.csv",header = TRUE)

reprodataReshaped=melt(reprodata[,c("interval","steps")],id="interval", measure.vars=c("steps"),variable.name="steps", na.rm=TRUE)
reprodataMean= dcast(reprodataReshaped, interval~ steps,mean)
reprodataSum = dcast(reprodataReshaped, interval~ steps,sum)

require(ggplot2)
require(plyr)
ggplot(reprodataMean, aes(interval,steps)) + geom_line(color="blue")+ labs(title="steps average daily activity")+ labs(x="5-minute interval")+ labs(y="averaged across all days")  

dev.copy(png, file = "ASS_1_2.png",width=500 ,height=500)
dev.off()
max <- reprodataSum[reprodataSum$steps ==max(reprodataSum$steps),]
maxmean <-reprodataMean[reprodataMean$steps ==max(reprodataMean$steps),]

## Inputing missing values

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
#require(zoo)
 

  






weekDays<-c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag")
weekEnd <-c("Samstag","Sonntag")

reprodataFilled <- reprodata
numRow <- nrow(reprodataFilled)
j =1
numberOfReplacedNa=0
for(i in 1:numRow){
  
  if(is.na(reprodataFilled$steps[i])){
    
    reprodataFilled$steps[i] = reprodataMean$steps[j]
    numberOfReplacedNa=numberOfReplacedNa+1
    
  }
  j=j+1
  
  if(j==289){
    j=1
  }
  
  
}
head(reprodataFilled,n=10)
 

#### second way ##############
copyFilled <- reprodataFilled
copymelt <-  melt(copyFilled ,id=c("interval","date"), measure.vars=c("steps"),variable.name="steps", na.rm=TRUE)

copymeltMutate<-mutate(copymelt,daytype = factor( x=ifelse(weekdays(as.Date(copymelt$date)) %in% weekDays,"weekday","weekend"),levels = c("weekday","weekend") ))

copymeltMean= dcast(copymeltMutate, interval+daytype ~ steps,mean)







#############################





reprodataFilled <-mutate(reprodataFilled,daytype = factor( x=ifelse(weekdays(as.Date(reprodataFilled$date)) %in% weekDays,"weekday","weekend"),levels = c("weekday","weekend") ))
# 
# reprodataFilled$daytype <-  factor( x=ifelse(weekdays(as.Date(reprodataFilled$date)) %in% weekDays,"weekday","weekend"),levels = c("weekday","weekend") )
str(reprodataFilled)
head(reprodataFilled)

reprodataFilledReshaped=melt(reprodataFilled ,id=c("interval","daytype"), measure.vars=c("steps"),variable.name="steps", na.rm=TRUE)
reprodataFilledReshapedMean= dcast(reprodataFilledReshaped, interval~ steps,mean)
str(reprodataFilledReshaped)
head(reprodataFilledReshaped)

# z <- zoo(c(2, NA, 1, 4, 5, 2), c(1, 3, 4, 6, 7, 8))
# na.approx(z)
# na.approx(z, na.rm = FALSE)
# d0 <- as.Date("2000-01-01")
# z <- zoo(c(11, NA, 13, NA, 15, NA), d0 + 1:6)
# 
# na.approx(z, xout = d0 + 7)
# na.approx(z, xout = d0 + 7, rule = 2)
# na.approx(reprodata)
# rep <-read.zoo(reprodata,FUN = identity)
# d <- data.frame(x= 1:10,y=(1:10)^2)
# da <- approx(d$x,d$y)
# 
#  
# 
# 


# m#What is the average daily activity pattern?
# 
# file<- read.csv("activity.csv",header = TRUE)
# file_3=melt(file[,c("interval","steps")],id="interval", measure.vars=c("steps"),variable.name="steps", na.rm=TRUE)
# file_3= dcast(file_3, interval~ steps,mean)
# 
# require(ggplot2)
# ggplot(file_3, aes(steps, interval)) + geom_line(color="blue")+ labs(title="steps average daily activity")+ labs(x="5-minute interval")+ labs(y="averaged across all days")
# 
# dev.copy(png, file = "ASS_1_2.png",width=500 ,height=500)
# dev.off()
