#Other distributions

x <- rnorm(10000, mean=10, sd=3)
hist(x, breaks=50)

#Find in google other distributions and plot them in R



#Compute the mean of the rows

mymatrix <- matrix( rnorm(100000, mean=10, sd=3), ncol=5)

n <- nrow(mymatrix)
mymean <- rep(0, n)
for(i in 1:n){
  mymean[i] <- mean(mymatrix[i,])
}
head(mymean)

#Alternative using apply
mymean <- apply(mymatrix, 1, mean)
head(mymean)


#by columns
m <- ncol(mymatrix)
mymean <- rep(0, m)
for(i in 1:m){
  mymean[i] <- mean(mymatrix[,m])
}
mymean

mymean <- apply(mymatrix, 2, mean)
mymean

#Read more
?apply
?lapply



#linear regression

head(iris)


linearMod <- lm(Petal.Width ~ Petal.Length, data=iris)  # build linear regression model on full data
print(linearMod)



plot(iris$Petal.Length, iris$Petal.Width)
abline(linearMod)

#


plot(iris$Petal.Length, iris$Petal.Width)

levels(iris$Species)

setosa <- subset(iris, Species=="setosa")

linearModS <- lm(Petal.Width ~ Petal.Length, data=setosa)  
print(linearModS)

points(setosa$Petal.Length, setosa$Petal.Width, col="blue")
abline(linearModS, col="blue")



versicolor <- subset(iris, Species=="versicolor")

linearModS <- lm(Petal.Width ~ Petal.Length, data=versicolor)  
points(versicolor$Petal.Length, versicolor$Petal.Width, col="red")
abline(linearModS, col="red")



virginica <- subset(iris, Species=="virginica")

linearModS <- lm(Petal.Width ~ Petal.Length, data=virginica)  
points(virginica$Petal.Length, virginica$Petal.Width, col="green")
abline(linearModS, col="green")


#Repeat it with a loop.




#More Montecarlo problems?

#I've read of this method to always win at the roulette. 
#It consists on betting 1$ to the red.
#If it hits red, you bet again 1$ to the red.
#If it hits black, you bet 2$ to the red.
#Then, if it hits red, you go back to bet 1$ to red.
#If it hits black, you bet 4$ to the red, and so on...

#Compute the probability of doubling my money, without running out of money, so I can't bet again.
#Assume I start with 100$


#Step 1 set variables
mymoney <- 
bet <- 

#Write the conditions
while(mymoney >= ???? & mymoney< ????){
  #Adjust variables
  mymoney <- 
  #Generate the necessary random number
  myrandom <- 
  #if you win
  if(myrandom???){
    mymoney <- 
    bet <- 
  }else{
    bet <- 
  }
  

}

