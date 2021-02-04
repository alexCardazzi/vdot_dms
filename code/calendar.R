library("lubridate")
library("scales")
OLDMAR <- c(5.1, 4.1, 4.1, 2.1)
k <- read.csv("../data/safety slogan schedules/schedule.csv", stringsAsFactors = FALSE)

k$begin1 <- mdy(k$begin1)
k$end1 <- mdy(k$end1)
k$begin <- NULL; k$end <- NULL
k$begin_day <- weekdays(k$begin1)
k$end_day <- weekdays(k$end1)

the <- c()
holiday <- c()
popculture <- c()
for(i in c(1:nrow(k))){
  
  the <- c(the, k$begin1[i]:k$end1[i])
  
  if(k$holiday[i] == 1){
    
    holiday <- c(holiday, k$begin1[i]:k$end1[i])
  }
  
  if(k$popculture[i] == 1) {
    
    popculture <- c(popculture, k$begin1[i]:k$end1[i])
  }
}
the <- as_date(the)

all <- data.frame(day = as_date(ymd("2017-06-01"):ymd("2020-02-29")))
dayz <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
weekz <- unique(floor_date(all$day, "week"))

all$message <- ifelse(all$day %in% the, 1, 0)
all$holiday <- ifelse(all$day %in% holiday, 1, 0)
all$popculture <- ifelse(all$day %in% popculture, 1, 0)

par(mar = c(4.1, 4.1, 1.1, 1.1))

plot(table(k$end1 - k$begin1 + 1), xlab = "Length of Campaign (in Days)", ylab = "Frequency")
dist <- all$day[all$message == 1][(2:length(all$day[all$message == 1]))] - all$day[all$message == 1][(2:length(all$day[all$message == 1])) - 1]
dist <- dist[dist > 1]
plot(table(dist), xlim = c(0, max(dist)), ylim = c(0, 4),
     xlab = "Days between Campaigns", ylab = "Frequency")

dayz <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
plot(table(weekdays(all$day[all$message == 1])), xlab = "Weekday", ylab = "Frequency") #figure out how to get a not stupid order
plot(table(month(all$day[all$message == 1])), xlab = "Month", ylab = "Frequency")
plot(as.numeric(table(day(all$day[all$message == 1]))) / as.numeric(table(day(all$day))), ylim = c(0, .6),
     type = "h", xlab = "Day of Month", ylab = "Relative Frequency")

table(floor_date(all$day, "month"), day(all$day))
plot(day(all$day), floor_date(all$day, "month"),
     pch = ifelse(weekdays(all$day) %in% c("Friday", "Saturday", "Sunday"), 15, 20),
     col = alpha(ifelse(all$message == 1, "orangered", "dark green"), ifelse(all$message == 1, .67, .33)),
     xlab = "Day of Month (Fri - Sun as Squares, Else as Circles)", ylab = "Month of Year")

plot(day(all$day), floor_date(all$day, "month"),
     pch = ifelse(weekdays(all$day) %in% c("Friday", "Saturday", "Sunday"), 15, 20),
     col = alpha("black", ifelse(all$message == 1, .75, .25)),
     xlab = "Day of Month (Fri - Sun as Squares, Else as Circles)", ylab = "Month of Year")

plot(day(all$day), floor_date(all$day, "month"),
     pch = ifelse(weekdays(all$day) %in% c("Friday", "Saturday", "Sunday"), 15, 20),
     col = alpha(ifelse(all$holiday == 1, "red", ifelse(all$popculture == 1, "blue", "black")), ifelse(all$message == 1, .75, .25)),
     xlab = "Day of Month (Fri - Sun as Squares, Else as Circles)", ylab = "Month of Year")

table(weekdays(all$day), all$message)