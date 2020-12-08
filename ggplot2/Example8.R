#Heatmap
library(gplots) #install the package if you don't have it!

my_palette <- colorRampPalette(c("blue", "white", "red"))
my_palette <- colorRampPalette(c("black", "red"))

#pdf("Heatmap.pdf", height=9, width=9)

mymatrix <- matrix(runif(60), ncol=6)

heatmap.2(mymatrix,Colv=F,col=my_palette, trace='none')







#Large dataset visualization


list.files()

#Read the process data from GSE134134
data <- read.csv("dataSC.csv", sep=",", header=T, row.names=1)

#Check what's in it
head(data)
summary(data)
str(data)
#Too large


dim(data)
data[1:5,1:5]



#UMI per million normalization
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


#install.packages("Rtsne")
library(Rtsne)

?Rtsne

tsne_out <- Rtsne(t(rpm), theta=0.1, partial_pca = TRUE, verbose = TRUE)

head(tsne_out)

tsnecoords <- tsne_out$Y
head(tsnecoords)
row.names(tsnecoords) <- colnames(rpm)
colnames(tsnecoords) <- c("tSNE1", "tSNE2")
head(tsnecoords)

ggplot(tsnecoords, aes(x=tSNE1, y=tSNE2))+geom_point()
#very common error
class(tsnecoords)

tsnecoords <- as.data.frame(tsnecoords)
ggplot(tsnecoords, aes(x=tSNE1, y=tSNE2))+geom_point()



Gene <- rpm["Krt7",]


ggplot(aes(x=tSNE1, y=tSNE2, color=GeneExpression), data=data.frame(tsnecoords, GeneExpression=Gene))+
  geom_point(size=2, alpha=0.5)+ xlab("tSNE1") +
  ylab("tSNE2")+theme_bw()+
  scale_color_gradient(low="black", high="red")+ggtitle("Krt7")


Gene <- rpm["Adgre1",]


ggplot(aes(x=tSNE1, y=tSNE2, color=GeneExpression), data=data.frame(tsnecoords, GeneExpression=Gene))+
  geom_point(size=2, alpha=0.5)+ xlab("tSNE1") +
  ylab("tSNE2")+theme_bw()+
  scale_color_gradient(low="black", high="red")+ggtitle("Adgre1")


Gene <- rpm["Cd8a",]

ggplot(aes(x=tSNE1, y=tSNE2, color=GeneExpression), data=data.frame(tsnecoords, GeneExpression=Gene))+
  geom_point(size=2, alpha=0.5)+ xlab("tSNE1") +
  ylab("tSNE2")+theme_bw()+
  scale_color_gradient(low="black", high="red")+ggtitle("Gata6")



#Add unsupervised clustering
#In the second part of the R course we will learn some tools to cluster data. 
#For now we will use the classification provided by the publication.

list.files()

df <- read.csv("identity.csv", header=T, sep=",", row.names=1)

head(df)
head(tsnecoords)

df2 <- merge(df, tsnecoords, by="row.names")
head(df2)

row.names(df2) <- df2[,1]
df2 <- df2[,2:ncol(df2)]
head(df2)


ggplot(aes(x=tSNE1, y=tSNE2, color=x), data=df2)+
  geom_point(size=1, alpha=1)+ xlab("tSNE1") +
  ylab("tSNE2")+theme_bw()

ggplot(aes(x=tSNE1, y=tSNE2, color=as.factor(x)), data=df2)+
  geom_point(size=1, alpha=1)+ xlab("tSNE1") +
  ylab("tSNE2")+theme_bw()


ggplot(aes(x=tSNE1, y=tSNE2, color=as.factor(x)), data=df2)+
  geom_point(size=1, alpha=1)+ xlab("tSNE1") +
  ylab("tSNE2")+theme_bw()+facet_wrap(~x)

#Plot the gene expression of Krt7 after splitting the clusters with facet_wrap

ggplot(aes(x=tSNE1, y=tSNE2, color=as.factor(x)), data=df2)+ 
  -----------FILL THIS PART-------------------


#change colors
ggplot(aes(x=tSNE1, y=tSNE2, color=as.factor(x)), data=df2)+
  geom_point(size=1, alpha=1)+ xlab("tSNE1") +
  ylab("tSNE2")+theme_bw()+scale_color_manual(values=c("FILL WITH 10 COLORS"))


#Practice 8.1. 
#Create a matrix: 6 columns, 10 rows. 
#Fill them with the average expression of the following 6 genes in the 10 clusters
#Alb, Adgre1, Marco, Krt7, Gata6, Cd8a

#Prepare dataset
rpm[1:5,1:5]
rpm2 <- t(rpm)

df3 <- merge(rpm2, df2, by="row.names")
row.names(df3) <- df3[,1]
df3 <- df3[,2:ncol(df3)]
df3[1:5,1:5]
df3[1:5, 9860:9864]

subdf <- subset(df3, x==0)

subdf[1:5,9860:9864]

avcluster0 <- colSums(subdf)
veccluster0 <- avcluster0[c("Alb", "Adgre1", "Marco", "Krt7", "Gata6", "Cd8a")]

#Repeat for the other clusters. Do it 9 more times or you can use a loop.
#Create a matrix containing the 10 vectors

markermatrix <- c(veccluster0,...)

#Plot the matrix with heatmap.2



heatmap.2(markermatrix,Colv=F,col=my_palette, trace='none')


heatmap.2(markermatrix,Colv=F,col=my_palette, trace='none', scale="row")


#Assign cell type


#Cluster 0: Endothelial cells
#Cluster 1: Macrophages
#Cluster 2: Stellate cells
#Cluster 3: Cholangiocytes
#Cluster 4: Lymphocytes
#Cluster 5: Endothelial cells
#Cluster 6: Dendritic cells
#Cluster 7: Endothelial cells
#Cluster 8: Hepatocytes
#Cluster 9: Lymphocytes


df2 <- data.frame(df2, factor(df2$x))
head(df2)
colnames(df2)[5] <- "celltype"

levels(df2$celltype) <- c("EC", "Macro", "SC", "Cholan", "Lymph", "EC", "DC", "EC", "Hep", "Lymph")


ggplot(aes(x=tSNE1, y=tSNE2, color=celltype), data=df2)+
  geom_point(size=1, alpha=1)+ xlab(paste0("PC1: ",100*percent_var[1],"% variance")) +
  ylab(paste0("PC2: ",100*percent_var[2],"% variance"))+theme_bw()


# Exercise
# Follow the part1, part2 and part3 of this ggplot2 tutorial
# http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html


