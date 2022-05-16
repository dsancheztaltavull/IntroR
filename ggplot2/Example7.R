list.files()

#Open the RNA-seq data
data <- read.csv("data.txt", sep="\t", header=T, row.names=1)
head(data)
summary(data)
dim(data)
str(data)


#Read per million normalization
rpm <- 1000000*t(t(data)/colSums(data))
colSums(rpm)

#log normalization

rpm <- log(1+rpm)

#Principal Component Analysis
pc <- prcomp(t(rpm))



#Inspect it
summary(pc)
names(pc)


#We can look at how much variance we explain with each component 
percent_var <- pc$sdev^2/sum(pc$sdev^2)
percent_var <- percent_var*100  #From 0 to 100
barplot(percent_var, xlab="PCs", ylab="variance (%)")
#We observe that the first 2 principal components explain a high percentage of the variance

#Alternatively, we can check it
cum_var <- cumsum(pc$sdev^2/sum(pc$sdev^2))
cum_var <- cum_var*100 #From 0 to 100
barplot(cum_var, xlab="Principle component", ylab="Cumulative % of variance" )

#We can add an horizonal with the threshold on how much variance do we need
abline(h=80)
#For 80% we need 3 PCs
abline(h=60)
#For 60% we need 2 PCs


#the coordinates of the PCA are stored in a variable names x of the pc object
pc$x
head(pc$x)

library(ggplot2)

df <- data.frame(pc$x)

ggplot(aes(x=PC1, y=PC2), data=df)+
  geom_point(size=2, alpha=0.8)+
  theme_bw()

#We can plot other PCs, for example 1 vs 3

ggplot(aes(x=PC1, y=PC3), data=df)+
  geom_point(size=2, alpha=0.8)+
  theme_bw()




colnames(data)
#Add some info about the samples, in this case the day of the experiment
Condition <- c(0,0,2,2,4,4,6,6,7.5,7.5,8,8,8.5,8.5,10,10,10.5,10.5,11,11,11.5,11.5,12,12,14,14,16,16)
#?ggplot

df <- data.frame(pc$x, Group=Condition)

ggplot(data=df, aes(x=PC1, y=PC2, color=Group))+
  geom_point(size=2, alpha=1)+ xlab(paste0("PC1: ",round(percent_var[1],5),"% variance")) +
  ylab(paste0("PC2: ",round(percent_var[1],5),"% variance"))+theme_bw()+
  scale_color_gradient(low="black", high="red")

#Plot a meaningful gene for this dataset
HBB <- rpm["HBB",]

df <-  data.frame(pc$x, GeneExpression=HBB)       

ggplot(data=df, aes(x=PC1, y=PC2, color=Group))+
  geom_point(size=2, alpha=1)+ xlab(paste0("PC1: ",round(percent_var[1],5),"% variance")) +
  ylab(paste0("PC2: ",round(percent_var[1],5),"% variance"))+theme_bw()+
  scale_color_gradient(low="black", high="red")+ggtitle("HBB")

#Another one
HBB <- rpm["SPI1",]

ggplot(data=df, aes(x=PC1, y=PC2, color=Group))+
  geom_point(size=2, alpha=1)+ xlab(paste0("PC1: ",round(percent_var[1],5),"% variance")) +
  ylab(paste0("PC2: ",round(percent_var[1],5),"% variance"))+theme_bw()+
  scale_color_gradient(low="black", high="red")+ggtitle("SPI1")


#Plot the expression of your favorite genes


#Read more about ggplot2
?ggplot


#create a data.frame


time <- 1:24
temperature <- c(10,9,9,8,9,9,9.5,10,9.5,9,12,11.5,12,13,13,13.5,14,14,13,12,11,11,11,10)

df <- data.frame(time, temperature)
head(df)

#ggplot input: data.frame
ggplot(data=df) 

#Inside aesthetics (aes) we include the variables to represent
ggplot(data=df,aes(x=time, y=temperature))

#Next we specify how we plot it
#with points
ggplot(data=df,aes(x=time, y=temperature))+geom_point()

#or with lines
ggplot(data=df,aes(x=time, y=temperature))+geom_line()

#or with bars
ggplot(data=df,aes(x=time, y=temperature))+geom_bar(stat="identity")


#They can be combined

ggplot(data=df,aes(x=time, y=temperature))+geom_bar(stat="identity")+geom_line()

#range

ggplot(data=df,aes(x=time, y=temperature))+geom_bar(stat="identity")+
  geom_line()+xlim(c(6,23))+ylim(0,30)

#Add a title


ggplot(data=df,aes(x=time, y=temperature))+geom_bar(stat="identity")+
  geom_line()+xlim(c(6,23))+ylim(0,30)+ggtitle("18th of July 2021")


#Change the labels

ggplot(data=df,aes(x=time, y=temperature))+geom_bar(stat="identity")+
  geom_line()+xlim(c(6,23))+ylim(0,30)+ggtitle("18th of July 2021")+
  xlab("Time (hours)") + ylab("Temperature (°C)")



#Plot the points and a linear regression between the variables


ggplot(data=df,aes(x=time, y=temperature))+geom_point()+ geom_smooth(method="lm")

#It seems there is no linear relation between time and temperature. What about a non-linear relation?


ggplot(data=df,aes(x=time, y=temperature))+geom_point()+ geom_smooth(method="loess")



#Add Colors

light <- c(rep("night", 7), rep("day", 12),rep("night", 5))

df2 <- data.frame(df, light) 

df2

ggplot(data=df,aes(x=time, y=temperature, col=light))+geom_point()

#Different colors

ggplot(data=df,aes(x=time, y=temperature, col=light))+geom_point()+scale_color_manual(values=c("green", "black"))



#Be careful

ggplot(data=df,aes(x=time, y=temperature, col=light))+geom_line()


#With bars

ggplot(data=df,aes(x=time, y=temperature, col=light))+geom_bar(stat="identity")+
  scale_color_manual(values=c("green", "black"))

#col needs to be replaced with fill

ggplot(data=df,aes(x=time, y=temperature, fill=light))+geom_bar(stat="identity")+
  scale_fill_manual(values=c("green", "black"))



#Size

ggplot(data=df,aes(x=time, y=temperature, col=light, size=light))+geom_point()

ggplot(data=df,aes(x=time, y=temperature, col=light, size=temperature))+geom_point()


#Shape

ggplot(data=df,aes(x=time, y=temperature, col=light, size=light, shape=light))+geom_point()



#Transparency


ggplot(data=df,aes(x=time, y=temperature, col=light, size=light))+geom_point(alpha=0.5)


#Change the theme

ggplot(data=df,aes(x=time, y=temperature, col=light, size=light))+geom_point(alpha=0.5)+theme_bw()


#Save plot

pdf("Myplot.pdf", width=6, height=5)
ggplot(data=df,aes(x=time, y=temperature, col=light, size=light))+geom_point(alpha=0.5)
dev.off()


#Extra: Try to make some meaningful plots with iris or mtcars

data(iris)
iris



data(mtcars)
mtcars




#A reference for PCA, or google Principal Component Analysis R for further reading!

# https://www.youtube.com/watch?v=_UVHneBUBW0
