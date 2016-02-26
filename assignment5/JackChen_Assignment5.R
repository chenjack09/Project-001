# Econ 294 - Assignment 5
# Jack Chen


library(foreign)
library(dplyr)
library(ggplot2)


########################## Question 1(a)  ############################## 


# generate the new variable volume 'xyz' (which equals x*y*z)

diamonds <- mutate(diamonds,xyz = x*y*z)

# generate the ggplot with 'log10(xyz)' on the x-axis and 'log10(price)' on the y-axis

ggplot(diamonds, aes(log10(xyz), log10(price)))  +
  
  geom_point( aes(colour=clarity, size=carat) ) 
  
   
########################## Question 1(b)  ##############################              


ggplot(diamonds, aes(x=carat)) +
  
  geom_histogram(aes(fill = clarity, y = ..density..), bins = 20) +
  
  facet_grid(cut ~ .)

  
########################## Question 1(c)  ##############################


ggplot(diamonds, aes(cut, price)) +
  
  geom_violin() +
  
  geom_jitter(alpha=0.03)


########################## Question 3(a)  ##############################


# load data

org<-read.dta("/Users/dmedlen/Downloads/org_example.dta")


# use summarize + group_() to reshape the dataset as needed

org <- mutate(
  summarize(
  group_by(org, year, month),
  med_rw = median(rw, na.rm=T),
  rw.25 = quantile(rw,0.25, na.rm=T),
  rw.75 = quantile(rw,0.75, na.rm=T),
  rw.10 = quantile(rw,0.1, na.rm=T),
  rw.90 = quantile(rw,0.9, na.rm=T)),
  date = paste(year, month, "01", sep = "-"),
  date = as.Date(date, format = "%Y-%m-%d")
) 



ggplot(
  data = org,
  aes(
    x = date, 
    y = med_rw
  ) ) +

geom_line()  +

geom_ribbon(aes(ymin=rw.10, ymax=rw.90),alpha=0.2) +

geom_ribbon(aes(ymin=rw.25, ymax=rw.75),alpha=0.5) 
  


########################### Question 3b ###########################


org<-read.dta("/Users/dmedlen/Downloads/org_example.dta")


org <- mutate(
          summarize(
             group_by(org, year, month, educ),
             med_rw = median(rw, na.rm=T)),
    
             date = paste(year, month, "01", sep = "-"),
             date = as.Date(date, format = "%Y-%m-%d")
) 


ggplot(
  data = org,
  aes(
    x = date, 
    y = med_rw
  ) ) +
  
  geom_line(aes(colour = educ)) 

### End of assignment ###