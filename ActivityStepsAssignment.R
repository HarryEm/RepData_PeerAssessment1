data1 <- read.csv("activity.csv")
data1[is.na(data1)] <- 0
totalsteps <- tapply(data1$steps,data1$date,FUN=sum)
totalsteps <- totalsteps[grepl("",totalsteps)]

hist(totalsteps)

mean(totalsteps)
median(totalsteps)

intervalstepsave <- tapply(data1$steps,data1$interval,FUN=mean)
plot(intervalstepsave,type = "l")

which.max(intervalstepsave)

data2 <- read.csv("activity.csv")
length(which(is.na(data2)))

data2 <- cbind(data2,intervalstepsave[match(data2$interval,names(intervalstepsave))])
data2[is.na(data2),1] <- data2[is.na(data2),4]
data2 <- data2[1:3]

totalsteps2 <- tapply(data2$steps,data2$date,FUN=sum)
totalsteps2 <- totalsteps2[grepl("",totalsteps2)]

hist(totalsteps2)

mean(totalsteps2)
median(totalsteps2)

f <- weekdays(as.Date(data2$date)) == "Saturday" | weekdays(as.Date(data2$date)) == "Sunday"

f <- replace(f, f=="FALSE", "Weekday")
f <- replace(f, f=="TRUE", "Weekend")

data2 <- cbind(data2, as.factor(f))

xyplot(steps~interval | as.factor(f),data2,type = "a",layout = c(1,2))

