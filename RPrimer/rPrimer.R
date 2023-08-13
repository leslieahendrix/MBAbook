####### Appendix - R Primer #######

### Arithmetic Operators
2*3 #multiplication
9/3 #division
27/9 #27/9
27/(3*3) #equivalent way to compute 27/9
27/(3^2) #another way to compute 27/9

### Creating Objects
2*3  #ask R to calculate 2*3 and print the answer
A <- 2*4  #calculate 2*4 and store the answer as 'A'
A  #show what A is
(B <- 12)  #define B to be 12 and print it, too
ls()  #show all stored objects
objects()  #same as ls()

### Data Types, Classes and Structures
typeof(2)
typeof(2L) #L makes it an integer
typeof("a") #since it's in quotes, it will be a character
typeof(TRUE) #a logical
is.double(2)  #can check if it's the type you think it is
is.integer(2L)
is.character("a")
is.logical(TRUE)

class(2)
class(2L)
class("a")
class(TRUE)

C <- "c"
cFactor <- as.factor(C)
levels(cFactor)

vec1 <- c(2,4,6,8,10)
vec1
is.double(vec1) #is it type 'double'?
is.numeric(vec1) #is it class 'numeric'?
is.atomic(vec1) #is it homogeneous?
is.vector(vec1) #should be a vector - is it?

vec2 <- seq(2,10,2) #seq(from,to,by)
vec2
vec3 <- c(1:10) #a:b indexes 'from a to b'
vec3
is.integer(vec3)
is.character(c("dog","cat","fish"))
is.vector(c("dog","cat","fish"))
vec4<-c(10,20,5,17)
vec4==10  #check each element to see if it is 10
vec4>15|vec4==10 
vec5<-c(5,10,17,20,30,40,5,17)
vec4%in%vec5 #is each element of vec4 in vec5
vec5%in%vec4 #is each element of vec5 in vec4

A <- matrix(c(1,2,3,4,5,6),nrow=2,byrow=TRUE) #create a matrix
A
B <- matrix(c(1,6,3,5,7,2),nrow=3)
B

is.atomic(A) #is it filled with same data type?
is.matrix(A) #should be a matrix - is it?
is.atomic(B) #same checks for B
is.matrix(B) 

# Calculations on Numeric Matrices and Vectors
vec3 #remind me what vec3 is
vec3+5 #add 5 to every entry of vec3
vec3*2 #multiply every entry of vec3 by 2

vec1 #what was vec1 again?
vec2 #what was vec2 again?
vec1*vec2 
vec1%*%vec2
2*2 + 4*4 + 6*6 + 8*8 + 10*10  #calculation done in vec1%*%vec2

# Built-in Functions
(1+2+3+4+5+6+7+8+9+10)/10 #can calculate the mean in R
mean(vec3) #calculate the mean #easier and quicker 
sd(vec3) #calculate the sample standard deviation
sum(vec3) #sum (add) the numbers
summary(vec3) #five no. summary plus mean for numeric data
round(log(vec3),2) 
round(exp(vec3),1)
vec4 #what was vec4 again?
ifelseExamp <- ifelse(vec4==10,10,0) 
ifelseExamp
which(vec4==10) 
paste("I","am","learning","a","lot")
c("I","am","learning","a","lot")
length(paste("I","am","learning","a","lot"))
length(c("I","am","learning","a","lot"))
names<-paste("samp",1:4,sep="")
names
namesWithSp<-paste("samp",1:4,sep=" ")
namesWithSp
namesByMach<-paste("samp",1:4,sep="Mach")
namesByMach

### Reading in Data
getwd()  #shows you working directory
setwd("yourPathGoesHere")  #put the path you want to point R to

aloha<-read.csv("HiListings.csv", strings=T)  #call in data from working directory
aloha<-read.csv("C:/Users/first.last/Desktop/HiListings.csv",strings=T) #point to the file 
aloha<-read.csv(file.choose(),strings=T) #pop-up window allows you to double-click file

head(aloha) #first 6 rows (not recommended for wide datsets)
str(aloha) #structure of data
summary(aloha) #gives 5 no. summary plus mean for numerical and count of each category for factor
mean(aloha$accommodates) #average number rental accomodates
mean(aloha$bedrooms) #returns NA since there are NAs in that column
mean(aloha$bedrooms,na.rm=TRUE) #tell mean() to remove NAs and then calculate
mean(aloha$bedrooms,na.rm=T) #same as above
mean(aloha$bedrooms,na=T) #same
mean(aloha$bedrooms,n=T) #same again

aloha$superhost[2] #2nd entry of the superhost column
aloha[2,11] #2nd row of the 11th column
aloha[['superhost']][2] #2nd element of list called superhost
aloha[[11]][2] #2nd element of 11th list in aloha

superhost[2] #won't find
attach(aloha)
superhost[2] #after attaching, can refer directly to column 

### Cleaning the Data
aloha$host.response.rate <-
   gsub("[%N/A]","",aloha$host.response.rate) #removes all instances of % and N/A
str(aloha$host.response.rate) #it's a character

aloha$host.response.rate <- 
   as.numeric(aloha$host.response.rate) #make it numeric
str(aloha$host.response.rate) #there we go

aloha$host.acceptance.rate <- 
   as.numeric(gsub("[%N/A]","",aloha$host.acceptance.rate)) #clean and make numeric 
str(aloha$host.acceptance.rate)

# cleaning up $ takes different strategy since it is at beginning of entry
aloha$price<-as.numeric(gsub("[\\$,]","",aloha$price)) #need \\ to remove $ on front of entry
summary(aloha$price)

# formatting dates
aloha$first.review<-as.Date(aloha$first.review,"%m/%d/%Y") #dates in R
aloha$last.review<-as.Date(aloha$last.review,"%m/%d/%Y")
aloha$last.review[1]-aloha$first.review[1] 

# missing data
sum(is.na(aloha)) #total number of NAs in aloha
colSums(is.na(aloha)) #how many NAs in each column
rows<-which(is.na(aloha$reviews.per.month)) #row nums for NAs
table((aloha$last.review-aloha$first.review)[rows])
summary(aloha$number.of.reviews[rows])
aloha$numRevsImputed<-aloha$reviews.per.month
aloha$numRevsImputed[is.na(aloha$reviews.per.month)]<-0
summary(aloha$reviews.per.month) #original column
summary(aloha$numRevsImputed) #new column with 0's to replace NAs


### The Basics of Subsetting
mean(aloha$review.scores.value[aloha$superhost=="t"],na.rm=T)
mean(aloha$review.scores.value[aloha$host.total.listings.count>9],na.rm=T)
mean(aloha$review.scores.value[aloha$superhost=="t" & aloha$host.total.listings.count>9],na.rm=T)

alohaSuperhosts<-subset(aloha,aloha$superhost=="t")
mean(alohaSuperhosts$review.scores.value,na.rm=T)

### Graphical Displays
hist(aloha$reviews.per.month,col="blue",
   xlab="Average Number of Reviews per Month",
   main="",cex.lab=1.25,cex.main=2)

boxplot(aloha$price[aloha$price<600] ~ 
   aloha$neighborhood.group[aloha$price<600],
	col="#00c3e3", #HI official water color according to Google
	xlab="Neighborhood Group",
	ylab="Price ($)",
	sub="HI airbnb listings less than $600 per night",
	cex.lab=1.25
	)
	
plot(aloha$longitude,aloha$latitude,
   col=aloha$neighborhood.group,
	xlab="Longitude",
	ylab="Latitude",
	cex.lab=1.25		
)
legend("topright",fill=1:4,
   legend=levels(aloha$neighborhood.group))
   
##### Advanced Topics for Functions
### User Defined Functions (UDF)

# List the arguments in regular parenthesis and what the function does with the arguments inside curly brackets {}
subtract<-function(x,y){
x-y
}

# Use the function by its name with the specified values for the arguments.
# Either explicitly define the arguments or just put the values in the order they go in.
subtract(x=2,y=3) #explicitly define x and y 
subtract(2,3) #or let R assume the first thing is x and the second thing is y
subtract(y=2,x=3) #if you explicitly define arguments, you can go out of order
subtract(3,2) #it assumes x=3 and y=2 since you didn't explicitly state

# For loops
mat1<-matrix(c(1,2,3,4,5,6,7,8,9),byrow=F,ncol=3) #create a matrix
avgs<-c() #create empty vector to fill
 
# This loop starts with i=1 and keeps going until i=number of columns of mat1.
for(i in 1:ncol(mat1)){
	avgs<-c(avgs,mean(mat1[,i])) #calculates the average for all 3 columns 
	}

mat1 #take a look
avgs
 
# We'll define a function named avgCols() that calculates the averages of the columns of a matrix.
avgCols<-function(mat){
	avgs<-c() #create empty vector to fill
	for(i in 1:ncol(mat)){
	avgs<-c(avgs,mean(mat[,i]))
	}
   print(avgs)
}

mat2<-matrix(c(2,3,4,5,6,7,8,9,10),ncol=3) #matrix to use in function
mat2
avgCols(mat2) #Ah yes, the averages of the three columns are 3, 6, and 9

avgs  #What's happening? that's our old "avgs" object (not the one inside avgCols)


### Avoid Loops: The apply() Functions
# For loops can get the job done but are not fast or efficient. 
# The apply() family of functions can save a lot of computing time.
# The apply() function is the basic one.
apply(mat2,2,mean) #apply the mean to the columns of mat (1=rows, 2=columns)
# The lapply() function takes a list and returns a list of the same length.
lapply(mat2,mean) #mat has 9 elements, so lapply() returns a list of 9 elements
# Another example with lapply()
a<-c(1,2,3)
b<-c(4,5,6)
c<-c(7,8,9)
ourList<-list(a,c,b) #our list has 3 elements
lapply(ourList,mean) #returns 3 elements

# sapply() does the same thing as lapply() but tries to simplify the output 
# and returns a vector or matrix, if possible
sapply(ourList,mean)

# vapply() does the same thing as sapply() but allows the user to specify the type of output.
vapply(ourList,mean,numeric(1))

# We can calculate the average review score by superhost status 
# using separate subsets(inefficient).
mean(aloha$review.scores.value[aloha$superhost=="t"],na.rm=T)
mean(aloha$review.scores.value[aloha$superhost=="f"],na.rm=T)

# Or have tapply() do both at once.
tapply(aloha$review.scores.value,aloha$superhost,mean,na.rm=T)


### Packages
# A package is a set of functions or datasets (or both) in R.
# There are many packages that come with your R intallation.
# To use a function or dataset from a package, though, you need to load it into your workspace first.
library(rmarkdown) #you will likely get a warning this dataset isn't found
install.packages("rmarkdown") #install package, choose CRAN mirror if prompted
library(rmarkdown) #now you should be able to load it into your work session

# render script file that has markdown language in it
render("SampMark.R") #this line requires that you have already run library(rmarkdown)

rmarkdown::render("SampMark.R") #loads rmarkdown and renders
# check your working directory for sampMark.html 
# double click sampMark.html to see your rendered document















