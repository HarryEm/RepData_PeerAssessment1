# PA_template

Begin by reading in the dataset assuming its in the working directory, summing it by day and ignoring days with no observations


```r
data1 <- read.csv("activity.csv")
data1 <- data1[grepl("",data1$steps),]
totalsteps <- tapply(data1$steps,data1$date,FUN=sum)
totalsteps <- totalsteps[grepl("",totalsteps)]
```

Here are the required outputs of the histogram, mean and median steps taken


```r
hist(totalsteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(totalsteps)
```

```
## [1] 10766.19
```

```r
median(totalsteps)
```

```
## [1] 10765
```

Next we split by interval and output the interval with the maximum average number of steps, interval 835


```r
intervalstepsave <- tapply(data1$steps,data1$interval,FUN=mean)
plot(intervalstepsave,type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
which.max(intervalstepsave)
```

```
## 835 
## 104
```

Next we read in the orignial dataset and see how many missing values there are, after which point we clean the data by replacing na values for each interval with the interval average.


```r
data2 <- read.csv("activity.csv")
length(which(is.na(data2)))
```

```
## [1] 2304
```

```r
data2 <- cbind(data2,intervalstepsave[match(data2$interval,names(intervalstepsave))])
data2[is.na(data2),1] <- data2[is.na(data2),4]
data2 <- data2[1:3]
head(data2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

We then sum the clean dataset and see that while the median is unchanged, the mean is slightly higher with the clean data. 


```r
totalsteps2 <- tapply(data2$steps,data2$date,FUN=sum)
totalsteps2 <- totalsteps2[grepl("",totalsteps2)]

hist(totalsteps2)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
mean(totalsteps2)
```

```
## [1] 10766.19
```

```r
median(totalsteps2)
```

```
## [1] 10766.19
```

Finally we add a weekday factor and see if this affects the data, there appears to be more activity earlier and a spike around intervals 700-900 on a weekday.


```r
require(lattice)
```

```
## Loading required package: lattice
```

```r
f <- weekdays(as.Date(data2$date)) == "Saturday" | weekdays(as.Date(data2$date)) == "Sunday"

f <- replace(f, f=="FALSE", "Weekday")
f <- replace(f, f=="TRUE", "Weekend")

data2 <- cbind(data2, as.factor(f))

xyplot(steps~interval | as.factor(f),data2,type = "a",layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
