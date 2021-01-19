mysum <- function(a, b){
  a+b
}

mysum(a=9,b=3)
mysum(9,3)

mydif <- function(a, b){
  a-b
}
mydif(a=9,b=3)
mydif(9,3)
mydif(a=3, b=9)
mydif(b=3, a=9)


#Sum the first two and substract the third
myop <- function(a,b,c){
  a+b-c
}
myop(10,2,5)


#function sum
a <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49)
sum(a)

mysum <- function(vec){
  n <- length(vec)
  total <- 0
    for(i in 1:n){
      total <- total+vec[i]
    }
  total
}
mysum(a)

####

mm <- matrix(1:36, ncol=3)
mm
head(mm)

myhead <- function(matrix){
        matrix[1:6,]  
}
myhead(mm)  
  

#Try myhead with the following matrix
mm <- matrix(1:12, ncol=3)
mm
head(mm)
myhead(mm)


myhead <- function(matrix){
  n <- min(nrow(matrix), 6)
  matrix[1:n,]  
}
myhead(mm)  



#Conditionals and functions

iseven <- function(a){
  if(a%%2==0){ return("yes")}
  if(a%%2==1){ return("no")}
}

iseven(4)
iseven(7)
iseven(3.5)

#alternative

iseven <- function(a){
  if(a%%2==0){ return("yes")}
  "no"
}

iseven(4)
iseven(7)
iseven(3.5)







#Make your own function to multiply two numbers, that is, 
#given the input a and b, your function should return a*b.

myprod <- function(????){
  ????
}

  
#Make a function to compute the mean of a vector



#Make a function to decide if the product of two numbers is even.

f3 <- function(a,b){

}
f3(4,7) #The output should be "yes"
f3(3,1) #The output should be "no"



#Some functions we have seen

#c
c(1,2)

#Mean
mean(1:5)

#Standard deviation
sd(1:5)

#t.test
t.test(1:3, 6:8)

#pearson coefficient
cor(c(1,2,3,4,5), c(4,2,6,1,5))

#plot 
plot(1:10, c(1,3,5,1,5,7,1,2,5,6))

#and many more like cbind, rbind, nrow, ncol, setwd, getwd, etc


#Exercise: Make your own version of the functions: tail(), colSums(), and rowSums()
#For colSums and rowSums you can use the function sum() if needed.