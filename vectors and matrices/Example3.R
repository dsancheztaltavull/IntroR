#Matrices

?matrix

#Create a matrix

mymatrix <- matrix(c(1,2,3,4,5,6,7,8,9), ncol=3, nrow=3)
mymatrix

#Create a matrix filling the positions by rows

mymatrix2 <- matrix(1:9, ncol=3, nrow=3, byrow=T)
mymatrix2

#access first row, first column

mymatrix[1,1]

#access first row
mymatrix[1,]

#access first column
mymatrix[,1]


#subset a smaller matrix 
mymatrix

smallmatrix <- mymatrix[c(1,2), c(2,3)]
smallmatrix




#Sum
mymatrix+mymatrix2

#Product matrices will multiply each element by each element.

mymatrix*mymatrix2

#Product matrices (Mathematical definition)

mymatrix %*% mymatrix2

#Let's check it
sum(mymatrix[1,]*mymatrix2[,1])
sum(mymatrix[1,]*mymatrix2[,2])
sum(mymatrix[1,]*mymatrix2[,3])

sum(mymatrix[2,]*mymatrix2[,1])
sum(mymatrix[2,]*mymatrix2[,2])
sum(mymatrix[2,]*mymatrix2[,3])

sum(mymatrix[3,]*mymatrix2[,1])
sum(mymatrix[3,]*mymatrix2[,2])
sum(mymatrix[3,]*mymatrix2[,3])


#Sum the elements in each column
colSums(mymatrix)

#Sum the elements in each row
rowSums(mymatrix)

#Compute the determinant of a matrix
det(mymatrix)

#transpose
t(mymatrix)

#cbind, rbind

year1 <- c(10, 5, 27, 4)
year2 <- c(12, 6, 33, 8)
year3 <- c(16, 1, 8, 16)


cbind(year1, year2, year3)
rbind(year1, year2, year3)

food <- rbind(year1, year2, year3)


colnames(food)
row.names(food)

colnames(food) <- c("Pizza", "Schnitzel", "Salad", "Sushi")
food

row.names(food) <- c(2018, 2019, 2020)

food


#How many restaurants were visited each year?

rowSums(food)

#How many pizzas, schnitzel, salads and japanese food were eaten?
colSums(food)



#Total cost
price <- c(25, 28, 14, 45)

food*price
t(food)*price
#Which one makes sense?


money <- t(food)*price



colSums(money)
rowSums(money)



#Let's add some data

year1 <- c(41, 0, 12, 3)
year2 <- c(33, 1, 7, 7)
year3 <- c(35, 0, 8, 7)


food2 <- rbind(year1, year2, year3)

food2

row.names(food2) <- c(2015, 2016, 2017)
colnames(food2) <- c("Pizza", "Schnitzel", "Salad", "Sushi")
food2


rbind(food2)



totalfood <- rbind(food,food2)

totalfood

#We can make it look better

totalfood <- totalfood[c(4,5,6,1,2,3),]

totalfood


#Restaurants visited every year:

rowSums(totalfood)


#Solve linear system of equations

#3x=21
#x+2y=17
#2y+z=18


a <- matrix(c(3,0,0,1,2,0,0,2,1), ncol=3, byrow = T)
a

vec <- c(21,17,18)
vec

solve(a, vec)


