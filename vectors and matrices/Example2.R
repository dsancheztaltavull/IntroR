#read the help of the functions vector and c

?c
?vector

#Combine 18, 26, 29, 65

a <- c(18, 26, 29, 65)

a

#Each position

a[1]
a[2]
a[3]
a[4]

#Positions 2 and 3

a[c(2,3)]

#Operations

a[2]-a[4]

#How many are above 28?
a > 28

#Create vector of characters

cvec <- c("Menorca", "Barcelona", "Ottawa", "Bern")

#Positions
cvec[1]
cvec[2]
cvec[3]
cvec[4]


#Add names to the vector

names(a)
names(a) <- cvec
names(a)

a


a["Menorca"]


a["Bern"]-a["Ottawa"]
#Did you see something strange?

max(a)

which(a==29)

which(a==max(a))

#Sum (1,2,3) + (4,5,6)

bdayW <- c(17, 6, 1984) 

date <- c("Day", "Month", "Year")

names(bdayW) <- date

bdayW["Month"]


bdayS <- c(30,5,2006)
names(bdayS) <- date

#To check who is older we can compare the year of their birthdays

bdayW["Year"] < bdayS["Year"]


#Be careful! This is not equivalent to this

bdayW["Year"] > bdayS["Year"]

#An example to illustrate this

bdayA <- c(1,4,1984)
names(bdayA) <- date

#check
bdayA["Year"] < bdayW["Year"]
bdayA["Year"] > bdayW["Year"]

#both are false

bdayA["Year"] == bdayW["Year"]



#Alternative
bdayA <- c(1,"April",1984)
bdayW <- c(17,"June",1984)

bdayA
bdayW

?as.Date

#Operations with vectors

c(1,2,3)+c(3,4,5)

#Be careful with vectors of different length. Sum (1,2,3) + (4,5,6,7,8)

c(1,2,3)+c(4,5,6,7,8)





#More operations with vectors
#Assign a vector to a

a <- c(1,2,4,8)

#Multiply a vector by a number

2*a

#Multiply two vectors

b <- c(0,-1,4,6)

a*b

#Sign b
sign(b)

#Check position 3 of the vector
a[3]



#check which positions of a vector are bigger than 5

a > 5

#write the elements that are bigger than 5

a[a>5]


#Ask which positions of a vector satisfy a condition
which(a>5)


#Other ways of creating vectors

a <- 1:10
a
#Sequence

a <- seq(2,100,2)



#Go back to lecture 1

x <- c(1,2,3,4,5,6,7,8,9,10)
y <- c(20, 18, 15, 10, 4, 4, 10, 15, 18, 20)

#Plot x and y

plot(x,y)

#plot x and y with lines and color red
plot(x,y, "l", col="red")

#make the points and lines in blue
plot(x,y)
lines(x,y, col="blue")

#With lines we can add another trajectory to the plot
y2 <- c(15, 18, 16, 11, 5, 12, 15, 15, 10, 10)
lines(x,y2, col="green")



#Mean of a vector
a <- c(1,3,8)
mean(a)

#sum of a
sum(a)

#standard deviation

sd(a)

b <- c(2,4,2)

t.test(a,b)


#Another example (because no one likes p-values > 0.05)

a <- c(1,2,3)


b <- c(100,101, 100)

t.test(a,b)

class(t.test(a,b))

t <- t.test(a,b)
class(t)


#concatenate the two vectors
c <- c(a,b)
c


#More ways of creating a vector

rep("gold", 7)

c(rep("gold",5), rep(c("green", "blue"),5))


?vector


#I promise this will be useful in the future:

#create a numeric vector of length 10
vector(mode="numeric", length=10)

#create a character vector of length 10
vector(mode="character", length=6)

#create a logical vector of length 10
vector(mode="logical", length=12)

