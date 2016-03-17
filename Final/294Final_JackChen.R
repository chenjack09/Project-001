# Econ 294 Final Exam 
# Jack Chen   
# SID: 1446433


### R packages needed for the final 

install.packages("knitr")
install.packages("RSQLite")

library(knitr)
library("dplyr")  
library("RSQLite") 
library("ggplot2")
library ("nycflights13")


### Load dataset

my_db <- src_sqlite("my_db.sqlite3", create = T)

flights_sqlite <- copy_to(
  my_db, flights, temporary = FALSE
)

flights<-collect(flights_sqlite)

################################ Facet: Weather ###############################

weather_sqlite <- copy_to(
  my_db, weather, temporary = FALSE
)

weather<-collect(weather_sqlite)

weather_db<-inner_join(weather,flights, c("month","day","hour"))


weather_db<-summarize(
  
  group_by(weather_db,temp),
  
  number_delay = sum(dep_delay>0),
  
  number_cancelled = sum(is.na(arr_delay))
)

View(weather_db)

plot(weather_db$temp, weather_db$number_delay, main="Flight Delay and Cancellation on temperature", xlab="Temperature", ylab="Number of deplays/cancellation",ylim=c(0,2500),col=4)
par(new = TRUE)
plot(weather_db$temp, weather_db$number_cancelled, main="Flight Delay and Cancellation on temperature", xlab="Temperature", ylab="Number of deplays/cancellation",ylim=c(0,2500),col=2)

# red=cancellation; blue=delay.
# For most points in temperature, number of flights delayed far excceed the number of flights cancelled.

# run a regression of delay against cancellation, temperature wise

reg1<-lm(number_delay~number_cancelled,weather_db)
summary(reg)

# The linear OLS regression shows the relationship between number of delay and number of cancellation is statistically significant.
# the coef. on number of cancellation is 74.4. This means the model predicts that, on average, an additional flight cancelled is associated with roughly 74 flights being delayed.

# we can visually verify the relation between these two variables through a scatter plot

plot(weather_db$number_cancelled, weather_db$number_delay, main="Flight Facet: Weather", xlab="Number of cancellations", ylab="Number of delays")

# With the facet of temperature, the scatter plot clearly demonstrates a positive correlation between delay and cancellation of flights.



############################### Facet: Time ##################################


TIME<-summarize(
  
    group_by(flights, month),
    
    number_of_delay = sum(dep_delay>0),
    
    number_of_cancelled = sum(is.na(arr_delay))
)
  
db<-collect(TIME)
View(db)

# plot number of delays as a function of month

plot(db$month, db$number_of_delay, type='l', main="Flight Delay and Cancellation Overtime", ylim=c(0,14000), xlab="Month", ylab="Number of deplays/cancellation",col=4)

par(new = TRUE)

plot(db$month, db$number_of_cancelled, type='l', ylim=c(0,14000), xlab="Month", ylab="Number of deplays/cancelled", col=2)

# Note: red curve indicates number of cancellation; blue curve indicates number of delay.
# suggest positive correlation btw cancellation and delay. 
# For each month in 2013, the number of flights cancelled is much much smaller compared to the number of flights delayed.
# cancellation remains relatively constant, whereas flight delay fluctuates more drastically from month to month. 
# the largest monthly difference between the number of cancellation and the number of delay occurs in July. This difference is 

# run a regression of number of deplay against number of cancellation (month-wise)

reg2<-lm(number_of_delay ~ number_of_cancelled,db)
summary(reg)

# coef. is 3.530, which means with respect to time facet, on average each flight cancellation is associated with around three and a half flight delays.


# visual verification: confirm the relation found with a scatter plot of the two variables 

plot(db$number_of_cancelled,db$number_of_delay, main="Flight Facet: Time", xlab="Number of cancellations", ylab="Number of delays")

# as one can see, the plot does demonstrate that flight cancellation and delay are correlated positively, with the facet of time (month of the year in this case). 




####################### Facet: airport destination ##########################


air_dest<-summarize(
  
          group_by(flght, dest),
          
          number_delay = sum(dep_delay > 0, rm.na=T),
          
          number_cancelled = sum(is.na(arr_delay)),
          
          count = n()
)

### View(air_dest) -- Note the overwhelming number of NA's in the delay column. 

#Filter the dataset to keep observation (i.e. destination airport) with no "NA".

airdest<-filter(air_dest,!is.na(number_delay))

View(airdest)

# ggplot

ggplot(airdest,
       aes(x=dest, y=number_delay)) +
  geom_boxplot() +
  geom_point(aes(color = number_cancelled))


# run a regression 

reg3<-lm(number_delay~number_cancelled,airdest)

summary(reg3)

# The OLS linear model predicts the relationship btw number of delay and cancellation with respect to airports destination is as follows:
# 1. on average, zero cancellation of flight is associated with roughly 28 flight delays.
# 2. on average, an additional flight cancellation is associated with roughly 55 additional flight delays.


######################### Facet: Plane Characteristics  ##########################

planes_sqlite <- copy_to(
  my_db, planes, temporary = FALSE, 
  indexes = list("tailnum")
)

planes<-collect(planes_sqlite)

plane_char<-inner_join(planes,flights, "tailnum")


plane_char_final<-summarize(
  
  group_by(plane_char, tailnum),
  
  number_delay = sum(dep_delay > 0, rm.na=T),
  
  number_cancelled = sum(is.na(arr_delay)),
  
  count = n()
)

# View(plane_char_final) - again many NA's

#filter/remove rows with NA's

plane_char_final <- na.omit(plane_char_final)

View(plane_char_final)

# create a ggplot 

ggplot(plane_char_final,
       aes(x=number_cancelled, y=number_delay)) +
  geom_point()

# run a regression to estimate relation btw cancellation and delay
   
reg4<-lm(number_delay~number_cancelled,plane_char_final)

summary(reg4)

# The OLS linear model predicts the relationship btw number of delay and cancellation with respect to plane characteristics (tailnum) is as follows:
# 1. on average, zero cancellation of flight is associated with roughly 21 flight delays.
# 2. on average, an additional flight cancellation is associated with roughly 21 additional flight delays.
