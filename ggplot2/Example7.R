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


#Inspect the variance explained by each component
percent_var <- pc$sdev^2/sum(pc$sdev^2)
barplot(percent_var, xlab="Principle component", ylab="% of variance")

cum_var <- cumsum(pc$sdev^2/sum(pc$sdev^2))
barplot(cum_var, xlab="Principle component", ylab="Cumulative % of variance" )

#PCA coordinates
pc$x
head(pc$x)

library(ggplot2)

ggplot(aes(x=PC1, y=PC2), data=data.frame(pc$x))+
  geom_point(size=4, alpha=0.5)+
  theme_bw()


colnames(data)
#Add some info about the samples, in this case the day of the experiment
Condition <- c(0,0,2,2,4,4,6,6,7.5,7.5,8,8,8.5,8.5,10,10,10.5,10.5,11,11,11.5,11.5,12,12,14,14,16,16)
#?ggplot
ggplot(aes(x=PC1, y=PC2, color=Group), data=data.frame(pc$x, Group=Condition))+
  geom_point(size=4, alpha=1)+ xlab(paste0("PC1: ",100*percent_var[1],"% variance")) +
  ylab(paste0("PC2: ",100*percent_var[2],"% variance"))+theme_bw()+
  scale_color_gradient(low="black", high="red")

#Plot a meaningful gene for this dataset
HBB <- rpm["HBB",]

ggplot(aes(x=PC1, y=PC2,  color=GeneExpression), data=data.frame(pc$x, GeneExpression=HBB))+
  geom_point(size=4, alpha=1)+ xlab(paste0("PC1: ",100*percent_var[1],"% variance")) +
  ylab(paste0("PC2: ",100*percent_var[2],"% variance"))+theme_bw()+
  scale_color_gradient(low="black", high="red")+ggtitle("HBB")

#Another one
HBB <- rpm["SPI1",]

ggplot(aes(x=PC1, y=PC2,  color=GeneExpression), data=data.frame(pc$x, GeneExpression=HBB))+
  geom_point(size=4, alpha=1)+ xlab(paste0("PC1: ",100*percent_var[1],"% variance")) +
  ylab(paste0("PC2: ",100*percent_var[2],"% variance"))+theme_bw()+
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
  geom_line()+xlim(c(6,23))+ylim(0,30)+ggtitle("13th of October 2020")


#Change the labels

ggplot(data=df,aes(x=time, y=temperature))+geom_bar(stat="identity")+
  geom_line()+xlim(c(6,23))+ylim(0,30)+ggtitle("13th of October 2020")+
  xlab("Time (hours)") + ylab("Temperature (°C)")



#Plot the points and a linear regression between the variables


ggplot(data=df,aes(x=time, y=temperature))+geom_point()+ geom_smooth(method="lm")

#It seems there is no linear relation between time and temperature 



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
