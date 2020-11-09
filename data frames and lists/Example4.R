#Data.frames

#Create a matrix

x <- matrix(c(1:56), ncol=2)
head(x)

#Let's add a column to indicate their gender

x <- cbind(x, rep(c("Man","Male","Woman", "Female"), 7))
head(x)

class(x[1,1])

#Now everything are characters. 
#That's because a matrix can not deal with multiple object types at one


?data.frame



df <- data.frame(x=1:28, y=29:56, rep(c("Man","Male", "Woman", "Female"), 7))

head(df)

class(df[1,1]) #What is an integer?


class(df[1,3]) #What is a factor?
?factor

#We can add row and col names like in matrices

colnames(df) <- c("ID", "Age", "Sex")

#Most of functions from matrices still work with data frames
dim(df)


colSums(df[,1:2])

#but be careful, the third column would give an error
colSums(df)



#we can access a column with $

df$Sex

df$ID


#Let's fix the dataset
levels(df$Sex)

levels(df$Sex) <- c("Woman", "Man", "Man", "Woman")
df
df$Sex

female <- subset(df, Sex=="Woman")

female


#We can create the data.frame with characters instead of factors
df2 <- data.frame(x=1:28, y=29:56, 
                 rep(c("Man","Male", "Woman", "Female"), 7),
                 stringsAsFactors = FALSE)
head(df2)
class(df2[1,3])

levels(df2$rep.c..Man....Male....Woman....Female....7.) #You can rename this column.

#We will see more of data frames in a moment, but before that let's talk about lists

?list


vec <- 1:3

mylist <- list(df, vec)

mylist

mylist[[1]]

mylist[[2]]




#datasets from R
?data()
?attach
library(datasets)
?datasets


library(help = "datasets")


#We can start with the most used dataset ever
data(mtcars)
mtcars

head(mtcars)

#Another dataset which is very popular
data(iris)
iris

head(iris)
tail(iris)

#Small parentesis: head is doing this
iris[1:6,]
#and tail
iris[(nrow(iris)-5):nrow(iris),]


#We can plot the distribution of the length and widths

hist(iris$Sepal.Length, breaks=30)
hist(iris$Sepal.Width, breaks=30)
hist(iris$Petal.Length, breaks=30)
hist(iris$Petal.Width, breaks=30)

#Do the distributions look normal?

#A mathematician once said: "if you don't understand a problem, draw it..."
plot(iris$Petal.Length, iris$Petal.Width)


#"...and if it's still confusing, add some colors"
plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)


#In the future we will make beautiful plots
library(ggplot2)
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, col=Species))+
  geom_point()+scale_color_manual(values=c("red", "green", "blue"))+
  xlab("Petal Length")+ylab("Petal Width")+theme_bw()


#I hope everyone watched the movie
data("Titanic")
Titanic
Titanic <- as.data.frame(Titanic)

Titanic

#Let's study male survival based on class
#Step 1: select the males
Male <- subset(Titanic, Sex=="Male")
Male

#Step 2: select the adults 
AdultMale <- subset(Male, Age=="Adult")
AdultMale

#We could do the analysis already, but let's reformat it

#Separate them according to survival
SAdultMale <- subset(AdultMale, Survived=="Yes")
NSAdultMale <- subset(AdultMale, Survived=="No")
SAdultMale
NSAdultMale

#Merge them
MAdults <- merge(SAdultMale, NSAdultMale, by="Class")
MAdults

#Delete useless columns
MAdults <- MAdults[,c(1,5,9)]
MAdults

colnames(MAdults) <- c("Class", "Alive", "Dead")

#Add a column with the survival rate

MAdultsExtended <- cbind(MAdults, MAdults$Alive/(MAdults$Alive+MAdults$Dead))
colnames(MAdultsExtended)[4] <- "Survival"
MAdultsExtended


#Play a bit with the Titanic dataset



#Likely you will want to read your own data. Open the ExampleData.csv

df <- read.csv("ExampleData.csv", header=T, sep=",", row.names=1)
head(df)

#Be careful, sometimes datasets are separated by tabs (sep="\t") or by semicolon (sep=";").

df <- read.csv("ExampleData2.csv", header=T, sep=",", row.names=1)
head(df)

#You can open it 
df <- read.csv("ExampleData2.csv", header=F, sep=",")
head(df)

df <- read.csv("ExampleData2.csv", header=T, sep="\t", row.names=1)
head(df)

#The example data is a random dataset generated with the R package randomNames. Explore it.



