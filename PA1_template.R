#Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
  
adata=read.csv(unzip("./activity.zip"),header=TRUE, sep=",")
adata$date=as.Date(adata$date, "%Y-%m-%d")


## What is mean total number of steps taken per day?


library(dplyr)
day_group<-group_by(adata, date)
step_totals<-summarize(day_group, total=sum(steps))
step_totals


library(ggplot2)
ggplot(step_totals, aes(x=total)) + geom_histogram(binwidth = 3000)


mean_totalsteps<-mean(step_totals$total,na.rm=TRUE)
mean_totalsteps
median_totalsteps<-median(step_totals$total,na.rm=TRUE)
median_totalsteps

mean_steps<-tapply(adata$steps, adata$interval, mean, na.rm = TRUE)
plot(rownames(mean_steps), mean_steps, type="l", xlab="5-min. intervals")

max_steps<-names(which(mean_steps==max(mean_steps)))
plot(rownames(mean_steps), mean_steps, type="l", xlab="5-min. intervals")
abline(v=as.numeric(max_steps<-names(which(mean_steps==max(mean_steps)))), col="red")

rows_na<-sum(is.na(adata$steps))

newdata<-adata
newdata$steps[is.na(newdata$steps)]<-mean_steps[as.character(newdata$interval)]       

new_day_group<-group_by(newdata, date)
new_step_totals<-summarize(new_day_group, total=sum(steps))
new_step_totals

ggplot(new_step_totals, aes(x=total)) + geom_histogram(binwidth = 3000)

new_mean_totalsteps<-mean(new_step_totals$total,na.rm=TRUE)
new_mean_totalsteps
new_median_totalsteps<-median(new_step_totals$total,na.rm=TRUE)
new_median_totalsteps

summary(median_totalsteps)
summary(new_median_totalsteps)

summary(mean_totalsteps)
summary(new_mean_totalsteps)

library(lubridate)
newdata$day<-as.factor(ifelse(match(wday(newdata$date),c(1,7), nomatch=0), "weekend", "weekday"))

library(lattice)
ndata.weekday<-subset(newdata, day=="weekday")
ndata.weekend<-subset(newdata, day=="weekend")
wkday_mean_steps<-tapply(ndata.weekday$steps, ndata.weekday$interval, mean, na.rm = TRUE)
wkend_mean_steps<-tapply(ndata.weekend$steps, ndata.weekend$interval, mean, na.rm = TRUE)
df1 <- data.frame(matrix(unlist(wkend_mean_steps)),interval=names(wkend_mean_steps),day="weekend")
names(df1)[1]<-"numberOfsteps"
df2 <- data.frame(matrix(unlist(wkday_mean_steps)),interval=names(wkday_mean_steps),day="weekday")
names(df2)[1]<-"numberOfsteps"
df <- rbind(df1,df2) #combine new data frames
df <- df[with(df, order(interval)), ] #order data frames
rownames(df) <- NULL #reindex data frames
xyplot(numberOfsteps ~ interval | day, data=df, type="l", layout= c(1,2))
dev.off()