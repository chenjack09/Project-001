# Econometrics Lab - Assignment 1 #
print("Jack Chen")
print("SID:1446433")

#Q1

df.dta<-read.dta("/Users/dmedlen/Downloads/NHIS_2007_dta.dta")
summary(df.dta)

df.csv<-read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/NHIS_2007_CSV.csv")
summary(df.csv)

df.td<-read.table("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/NHIS_2007_TSV.txt")
summary(df.td) 

load("/Users/dmedlen/Downloads/NHIS_2007_RData.RData")

print("The name is NHIS_2007_RData.") 

#Q2. 

print("NHIS_2007_dta: 188KB; NHIS_2007_CSV: 139KB; 
      NHIS_2007_TSV: 139 KB; NHIS_2007_RData: 45.3KB")   
print("'NHIS_2007_RData' is smallest")
print("The variability is due to the compactness of file. With a common dataset, 
      R stores it with different storage compacities depending on the original source.
      R stores its own dataset the most efficiently.")

#Q3 

typeof(NHIS_2007_RData)
print("list")
class(NHIS_2007_RData)
print("data.frame") 

#Q4

df<-read.dta("/Users/dmedlen/Downloads/org_example.dta")
summary(df)
str(df)

print("number of obs: 1119754")
print("number of variables: 30")

summary(df$rw)

print("Min. 1st Qu.  Median    Mean   3rd Qu.    Max.    NA's 
      1.8    10.7    15.9     19.8    24.4     354.8   521279 ")

#Q5

v<-c(1,2,3,4,5,6,7,4,NULL,NA)
length(v)

print("The reported length is 9")
print("The length does not match the number of values in the vector (namely,10) 
      because 'NULL' is not counted as an element.")

mean(v, na.rm=TRUE)

pinrt("The mean is 4.")

#Q6

X=matrix(data=c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3, ncol=3,byrow = TRUE)  
print(X)

t(X)

eigen(X)

print("The eigen values are: 1.611684e+01 -1.116844e+00 -1.303678e-15.")

print("The eigen vectors are:   
      [1]        [2]        [3]
      [1] -0.2319707 -0.78583024  0.4082483
      [2] -0.5253221 -0.08675134 -0.8164966
      [3] -0.8186735  0.61232756  0.4082483")

Y=matrix(data=c(1, 2, 3, 3, 2, 1, 2, 3, 0), nrow=3, ncol=3,byrow = TRUE)  

print(Y)

invY=matrix.inverse(Y)
print(Y)

Y%*%invY    # This gives the identity matrix

#Q7

diamonds <- data.frame(
  carat = c(5,2,0.5,1.5,5,NA,3),
  cut = c("fair", "good", "very good", "good", "fair", "Ideal", "fair"),
  clarity = c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", NA),
  price = c(850, 450, 450, NA , 750, 980, 420)
)

print(mean(diamonds$price, na.rm = T))
# The mean price is 650.
print(mean(diamonds$price[diamonds$cut == "fair"], na.rm = T))
# The mean price of cut "fair" is 673.3333.
print(mean(diamonds$price[diamonds$cut != "fair"], na.rm = T))
# The mean price of cut "good", "very good", and "Ideal" is 626.6667.
print(median(diamonds$price[diamonds$carat > 2 & (diamonds$cut == "Ideal" | diamonds$cut == "very good" )], na.rm = T))

# End of Assignment1 #
