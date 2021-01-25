#Useful functions

#grep
genes <- c("HBG1", "GAS2", "CD12", "CD14","CD15",  "MIR575", 
           "ANKRD22","CD19", "CLDN3", "GAS6","CD23", "CD25",
           "GATA1", "GATA2", "GATA3", "GATA6", "ALB", "HBB")

grep("CD", genes)

ids <- grep("CD", genes)
genes[ids]

#order

vector <- sample(1:100, 50)
vector

order(vector)
ids <- order(vector)
vector[ids]

#unique

names <- c("Maria", "Laura", "Olivia", "Emma", "Isabella", "Mia",
           "Evelyn", "Maria", "Emma", "Camila", "Mila", "Chloe")

unique(names)

#duplicated

duplicated(names)
ids <- duplicated(names)

names[ids]


names <- c("Maria", "Laura", "Olivia", "Emma", "Isabella", "Mia")
names2 <- c("George", "Maria", "Alex", "Isabella", "Harry", "Leo")

union(names, names2)
intersect(names, names2)

setdiff(names, names2)
setdiff(names2, names)


#Read excel
#install.packages('readxl')
library(readxl)

Sheet1 <- read_excel("test.xlsx", sheet = 1)

Sheet2 <- read_excel("test.xlsx", sheet = 2)



#qplot

library(ggplot2)

data(iris)
iris

head(iris)
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species))+
  geom_point()+geom_smooth(method="loess")

qplot(Petal.Length, Petal.Width, data = iris, color = Species,
      geom=c("point", "smooth"))


?qplot
# Violin plot
qplot(Species, Petal.Width, data = iris, 
      geom=c("violin"), trim = FALSE)



#Try all the examples of ?qplot
?qplot




