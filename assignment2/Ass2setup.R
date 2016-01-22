# Assignment 2 R Script
# Jack Chen

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 0  

JackChenAssignment2 <- list(
  
  firstName = "Jack",
  lastName  = "Chen",
  email = "jchen164@ucsc.edu",
  studentID = 1446433
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 1
  
load(file=url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData"))

summary(diamonds)
View(diamonds)

JackChenAssignment2$s1a <- nrow(diamonds)

JackChenAssignment2$s1b <- ncol(diamonds)

JackChenAssignment2$s1c <- names(diamonds)

JackChenAssignment2$s1d <- summary(diamonds$price)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 2

NHIS_2007_TSV <- read.table(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",
  sep = "\t",
  header = T
)

summary(NHIS_2007_TSV)
View(NHIS_2007_TSV)

JackChenAssignment2$s2a <- nrow(NHIS_2007_TSV)

JackChenAssignment2$s2b <- ncol(NHIS_2007_TSV)

JackChenAssignment2$s2c <- names(NHIS_2007_TSV)

JackChenAssignment2$s2d <- mean(NHIS_2007_TSV$weight)

JackChenAssignment2$s2e <- median(NHIS_2007_TSV$weight)
  

hist(NHIS_2007_TSV$weight)
table(NHIS_2007_TSV$weight)

new_weight<-ifelse(test = NHIS_2007_TSV$weight>=996 & NHIS_2007_TSV$weight<=999,
       yes  = NA, 
       no   = NHIS_2007_TSV$weight)


JackChenAssignment2$s2f <- summary(new_weight)[4]
  
JackChenAssignment2$s2g <- summary(new_weight)[3]


subd<-subset(NHIS_2007_TSV,SEX==1)

male<-ifelse(test = subd$weight>=996 & subd$weight<=999,
                   yes  = NA, 
                   no   = subd$weight)


JackChenAssignment2$s2h <- summary(male)


subd2<-subset(NHIS_2007_TSV,SEX==2)

female<-ifelse(test = subd2$weight>=996 & subd2$weight<=999,
             yes  = NA, 
             no   = subd2$weight)


JackChenAssignment2$s2i <- summary(female)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 3

vec <- c(letters,LETTERS)
vec <- as.factor(vec)
unclass(vec) 
levels(vec)
vec<-as.numeric(vec)

evenvec<-vec[vec%%2==0]

JackChenAssignment2$s3a <- evenvec


vec <- c(letters,LETTERS)


JackChenAssignment2$s3b <- paste(vec[c(36,1,3)],collapse = "")


arr <- array( c(letters,LETTERS), dim = c(3,3,3) )

arr_matrix2 <- arr[,,2]              

                 
JackChenAssignment2$s3c <- arr_matrix2[,1]

JackChenAssignment2$s3d <- arr[c(5,14,23)]

JackChenAssignment2$s3e <- paste(arr[1,1,2],arr[1,1,1],arr[3,1,1], sep="")


# # # Present all solutions stored in the list

JackChenAssignment2

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# save(JackChenAssignment2, file = "/Users/dmedlen/Desktop/LAB294/Project-001/assignment2/JackChenAssignment#2.RData")
