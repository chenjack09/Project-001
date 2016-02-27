# Assignment 4 R script
# ECON 294
# Jack Chen

library(foreign)
library(dplyr)
library(tidyr)

#----------------------------------- Question 0 ------------------------------------#

NAME<-"Jack Chen"
SID<-1446433
print(NAME); print(SID)

#----------------------------------- Question 1 ------------------------------------#

flights<-read.csv(
  "https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/flights.csv", 
  stringsAsFactors = F)

flights<-tbl_df(flights)


planes<-read.csv(
  "https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/planes.csv",
  stringsAsFactors = F)

planes<-tbl_df(planes)


airport<-read.csv(
  "https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/airports.csv",
  stringsAsFactors = F)

airport<-tbl_df(airport)


weather<-read.csv(
  "https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/weather.csv",
  stringsAsFactors = F)

weather<-tbl_df(weather)

#----------------------------------- Question 2 ------------------------------------#


# only "flights" and "weather" have date column

flights$date<-as.Date(flights$date)

weather$date<-as.Date(weather$date)


#----------------------------------- Question 3 ------------------------------------#


flights.2a<-filter(flights,dest=="OAK"|dest=="SFO")

print(nrow(flights.2a)) # 3508 obs

flights.2b<-filter(flights,arr_delay>=60)
print(nrow(flights.2b)) #10584 obs

flights.2c<-filter(flights,flights$arr_delay>2*flights$dep_delay)
print(nrow(flights.2c)) #70772 obs


#----------------------------------- Question 4 ------------------------------------#

method1 <- select(flights, dep_delay, arr_delay)
method2 <- select(flights, dep_delay : arr_delay)
method3 <- select(flights, contains("_delay"))

#----------------------------------- Question 5 ------------------------------------#

# -------- 5(a) --------- #

arranged_flights <- arrange(flights, desc(dep_delay))

print(arranged_flights$dep_delay[1:5])  
# the top 5 delayed times are: 981 970 931 869 814

print(arranged_flights$plane[1:5])
# the 5 most delayed flights are: "N69063" "N473AA" "N502MQ" "N670UA" "N6EAMQ"
  

# -------- 5(b) --------- #

flights$net_delay <- flights$arr_delay - flights$dep_delay

arranged_flights2 <- arrange(flights, net_delay)

print(arranged_flights2$net_delay[1:5])

# This gives the 5 smallest net_delay time: -69 -57 -54 -53 -53

print(arranged_flights2$plane[1:5])

# The corresponding 5 flights (caught up the most) is as follows: 
# "N12157" "N73406" "N37437" "N74856" "N814SK"



#----------------------------------- Question 6 ------------------------------------#

#convert flight mins to hours

flights$flight_hrs <- (flights$time) / 60

# add speed column

flights <- mutate(flights, speed = dist / flight_hrs)

#create delta = arr. delay - dep. delay

flights <- flights %>% mutate(delta = arr_delay - dep_delay)

View(flights)

# top 5 flights by speeds

flights <- flights %>% arrange(desc(speed))

print(head(flights,5))

# top 5 flights made up the most time (i.e. with the five smallest delta's)

flights <- flights %>% arrange(delta)

print(head(flights,5))

# top 5 flights lost the most time (i.e. with the five biggest delta's)

flights <- flights %>% arrange(desc(delta))

print(head(flights,5))



#----------------------------------- Question 7 ------------------------------------#


### Group by "carrier"


flights.7a<-flights %>%
  
  group_by(carrier) %>%               
  
    summarize(
    
    total = n(),
    
    num.cancelled = sum(cancelled==1),
 
    percent.cancelled = num.cancelled / total,
   
    min (delta, na.rm=T),
    
    delta_1stQ = quantile(delta,0.25,na.rm=T),
    
    mean(delta, na.rm=T),
    
    median(delta, na.rm=T),
    
    delta_3rdQ = quantile(delta,0.75,na.rm=T),
    
    delta_90th = quantile(delta,0.9,na.rm=T),
    
    max (delta, na.rm=T)
    
  ) %>%
  
  arrange(desc(percent.cancelled))
    
print(flights.7a)  


# The following R code is from the assignment question.

#######################################################################

day_delay <- dplyr::filter( summarize(
  group_by( dplyr::filter(
    flights,
    !is.na(dep_delay) ),
    date ),
  delay = mean(dep_delay), n = n()
),
n > 10 )
 
#######################################################################


# Explain what the code above does


cat( "The code does the followings: 

    1) Cleanses the the dataset 'flights': drop rows with NA's in the variable 'dep_delay';
    
    2) Groups the dataset by date;
    
    3) For each group (i.e. date), creates two columns ('delay' and 'n'). One gives the 
    average departure delay of flights on that particular date. Another gives the total 
    number of flights of the same date.
    
    4) Finally, filter the new dataset to keep only rows with n>10. In other words, only 
    keep the date observations during which more than 10 flights had occurred. " )



# Rewrite the code above (from assignment question) using the %>% operator:


day_delay <- flights  %>%
  
                 filter( !is.na(dep_delay) )  %>%
  
                     group_by(date)  %>%
  
                           summarize( 
                           
                                delay = mean(dep_delay),
                            
                                n = n() 
                                
                                ) %>%
   
                                    filter(n > 10)



#----------------------------------- Question 8 ------------------------------------#

# create a new column with difference in average dep_delay on two consecutive days

day_delay$change_mean_dep_delay <- NA  #first set this new column to contain NA's only

for (i in 2:nrow(day_delay)) {

   day_delay$change_mean_dep_delay [i] = day_delay$delay[i] - day_delay$delay[i-1]
}


# now sort this dataset by change in mean dep_delay from highest to lowest

NEW_day_delay <- arrange(day_delay, desc(change_mean_dep_delay))

print(NEW_day_delay$date[1:5])
  
# Results show the top five dates are: "2011-10-09"; "2011-06-22"; "2011-12-31";
# "2011-05-12"; and "2011-03-03".


#----------------------------------- Question 9 ------------------------------------#
  

#create the 'dest_delay' table

dest_delay <- summarize(
    
  group_by( dplyr::filter(
    
    flights,
    
    !is.na(arr_delay) ),
    
    dest ),
  
  delay = mean(arr_delay), 
  
  n = n() )


# now organize the dataset 'airport' as directed before merge 

airports <- airport[-5]  # drop the variable "country" as required

airports <- rename(airports, dest = iata)
airports <- rename(airports, name = airport)


#left_jion

df.9a <- left_join(dest_delay, airports, by = "dest")  

#inner_jion

df.9b <- inner_join(dest_delay, airports, by = "dest")


nrow(df.9b); nrow(df.9a) # 114 != 116, so their numbers of obs don't match. 

#right_jion

df.9c <- right_join(dest_delay, airports)

summary(df.9c$delay)  # 3262 NA's          

# There are NA's in avg delay since many 'dest' values in 'airports' are absent 
# in the dataset 'dest_delay'.


#full_jion

df.9d <- full_join(dest_delay, airports)

summary(df.9d$delay)   # again there are 3262 NA's for reasons stated above



#----------------------------------- Question 10 ------------------------------------#

#create 'hourly_delay' dataframe

hourly_delay <- summarize(
  
  group_by( dplyr::filter(
    
    flights,
    
    !is.na(dep_delay) ),
    
    date, hour ),
  
  delay = mean(dep_delay), 
  
  n = n() )


# merge with 'weather' dataset using inner_join by date and hour

df.10 <- inner_join(hourly_delay, weather, by= c("date","hour"))


df.10 <- arrange(df.10, desc(delay))


print(df.10$delay[1])       # the biggest delay is 24.4 mins

print(df.10$conditions[1])  # The associated condition is "Partly Cloudy" 


#----------------------------------- Question 11 ------------------------------------#

# part (a)

df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))

df


df.11a <- df %>% gather(subject, value, -treatment) %>%
  
                mutate(subject = extract_numeric(subject)) %>%
  
                     select(subject, treatment, value) %>%
  
                         arrange(subject, treatment)


View(df.11a)


# part (b)

df <- data.frame(
  
  subject = c(1,1,2,2),
  
  treatment = c("a","b","a","b"),
  
  value = c(3,4,5,6) )

df


df.11b <- spread(df,subject,value)

names(df.11b) <- c("treatment", "subject1", "subject2")

View(df.11b)


# part (c)

df <- data.frame(
  
  subject = c(1,2,3,4),
  
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  
  value = c(3,4,5,6))

df

df.11c <- data.frame(
  
  subject = c(1,2,3,4),
  
  sex = c("f","f","m","m"),
  
  age = c(15,50,45,18),
  
  state = c("CA","NY","HI","DC"),
  
  value = c(3,4,5,6) )

df.11c


# part (d)

df <- data.frame(
  
  subject = c(1,2,3,4),
  
  sex = c("f","f","m",NA),
  
  age = c(11,55,65,NA),
  
  city = c("DC","NY","WA",NA),
  
  value = c(3,4,5,6)
)

df

df.11d <- data.frame(
             
  subject = c(1,2,3,4),
  
  demo = c("f.11.DC", "f.55.NY", "m.65.WA", NA),
  
  value = c(3,4,5,6) )

df.11d