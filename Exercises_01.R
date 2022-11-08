#First Problem
   #1st Subproblem
data<-runif(20) #Here I'm generating the sample
   #2nd Subproblem
sigma<-mean(data) #Sigma is the mean of the sample
newdata<-((data-sigma)^2) #We are going to need newdata for computing the variance of data
variance<-mean(newdata) #In fact the mean of newdata is the variance of data
  #3rd Subproblem
newdata<-data[data>0.5] #Here we are filtering the elements of data greater than 0.5
sigma<-mean(newdata) 
newdata<-((newdata-sigma)^2) #In this line we are reusing newdata to have an array for computing the variance of newdata
variance<-mean(newdata) #This is the variance of the elements of data greater than 0.5
 #4th Subproblem
sumsq<-sum((data)^2)
 #5th Subproblem
sumsq<-sum((data[-1]-data[-20])^2)
#2nd Problem

points<-list() #'points' will be the list of vectors in the rectangle [-1,1] x [-1,1]

x<-runif(1000,-1,1)#Generating random coordinates
y<-runif(1000,-1,1)
dpoints<-c() #Vector of distances from the origin
for(i in 1:1000){
  points<-c(points, list(c(x[i],y[i]))) #Appending the vectors to 'points'
  dpoints<-c(dpoints, sqrt(((x[i])^2)+((y[i])^2)))
  }
percentage=((length(dpoints[dpoints<0.6]))/10)#Final result

#3rd Problem
  #1st Subproblem
datos<-nycflights13::flights
datos<-filter(datos, year==2013, month==9, day==7)
datos %>% filter(year == 2013, month==9, day==7) %>% summarise(meandel = mean(dep_delay))
  #2nd Subproblem
dh<-filter(datos, carrrier=='UA')
#We have 118 flights of UA, so 12* are the 10% (approximately) of flights with the largest mean.
d<- dh %>% arrange(dep_delay) %>% slice(1:12)
#d contains the 10% of the flights with largest mean of dep_delay of UA flights.