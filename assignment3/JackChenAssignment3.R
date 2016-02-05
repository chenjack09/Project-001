# Assignment 3 R Script
# Jack Chen

# Question 0  

name<-"Jack Chen"
SID<-"1446433"
email<-"jchen164@ucsc.edu"
print(name)
print(SID)
print(email)

# Question 1
# Through out the homework 'df.ex.' is referred to 'org'

library(foreign)
org<-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
summary(org)
View(org)

# Question 2  

install.packages("dplyr")
library(dplyr)
org_filter<-dplyr::filter(org, year==2013, month==12)
obs<-nrow(org_filter) 
print(obs)
org_filter2<-dplyr::filter(org, year==2013, month==7 | month==8 | month==9)
obs2<-nrow(org_filter2) 
print(obs2)

# Question 3

df.ex.3a<-arrange(org, order(year),order(month))

# Question 4

df.ex.4a<-select(org,year:age)
df.ex.4b<-select(org,year,month,starts_with("i"))
distinct(select(org, state))

# Question 5

stndz <- function(x){
  (x - mean(x, na.rm = T))  /  sd(x, na.rm = T)
}

nrmlz <- function(x){
  (x - min(x,na.rm=T) ) / ( max(x,na.rm=T) - min(x,na.rm=T) )
}

df.ex.5a<-transmute(org,
    rw.stndz = stndz(rw),
    rw.nrmlz = nrmlz(rw)      
)
View(df.ex.5a)

group_data<-group_by(org, year, month)
df.ex.5b<-transmute(group_data,
  rw.stndz = stndz(rw),
  rw.nrmlz = nrmlz(rw),
  count = n()
)
View(df.ex.5b) #verified to be consistent with hint~~! 

# Question 6

Group_data<-group_by(org,year,month,state)
df.ex.6<-summarise(
  Group_data,
  rw_min = min(rw, na.rm=T),
  rw_1stQ = quantile(rw,0.25,na.rm=T),
  rw_mean = mean(rw, na.rm=T),
  rw_med = median(rw,na.rm=T),
  rw_3rdQ = quantile(rw,0.75,na.rm=T),
  rw_max = max(rw, na.rm=T),
  count = n()
)
View(df.ex.6)

max<-max(df.ex.6$rw_mean,na.rm=T)

max_rw_combination<-dplyr::filter(df.ex.6, rw_mean==max)  

print(max_rw_combination[1:3])

# End