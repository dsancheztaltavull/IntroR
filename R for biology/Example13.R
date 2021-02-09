list.files()

#Open the RNA-seq data from Lecture 7
data <- read.csv("data.txt", sep="\t", header=T, row.names=1)
head(data)
summary(data)
dim(data)
str(data)


#Read per million normalization

colSums(data)


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





#Compare groups 2 and 3
colnames(rpm)
subdata <-rpm[,c(21:28 )]
head(subdata)

colnames(subdata) <- c(rep("Group2", 4), rep("Group3", 4))

#Practice: Make a dotplot of expression of HBB in Group2 and Group3

subdata["HBB",]

df <- data.frame(subdata["HBB",],"Group2", stringsAsFactors = F)
df
df[5:8,2] <- rep("Group3",4)
colnames(df) <- c("Expression", "Group")

df

ggplot(df, aes(x=Group,y=Expression))+geom_point()

#A fast way to plot them all

subrpm <- rpm[,c(21:28)]
head(subrpm)


cord2 = c(1,1,1,1)
cord3 = c(2,2,2,2)



ymax = apply(subrpm, 1, max)
?apply


mygenes <- rownames(subrpm)

head(mygenes)

ngene <- nrow(subrpm)
genesofinterest <- c("HBB", "Alb", "Ajuba", ...)

pdf("AllGenes.pdf", height=9, width=9)
par(mfrow=c(3,3))



for(i in 1:ngene) {
  plot(0,0,  xaxt = "n", type="n", xlim=c(0.5,2.5), ylim=c(0,ymax[i]), main=mygenes[i], xlab="Condition", ylab="Expression")
  axis(1, at=1:2, labels=c("Group2","Group3"))
  points(cord2, as.numeric(subrpm[i,1:4]), type="b", col=1, pch=17)
  points(cord3, as.numeric(subrpm[i,5:8]), type="b", col=2, pch=18)
  if(i %% 1000 == 0) print(i)
}
dev.off()





#Differentially expressed genes

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("DESeq2")

library("DESeq2")

#Exercise: Read the Analyzing RNA-seq data with DESeq2
browseVignettes("DESeq2")


#Compute DE genes

counts <- data[,21:28]
head(counts)
Condition <- c(rep("Group2", 4), rep("Group3", 4))

x<-as.factor(Condition)
x

sampleTable<-data.frame(sampleID=names(counts),condition=x)
sampleTable
dds<-DESeqDataSetFromMatrix(countData=counts,colData=sampleTable,design=~condition)
dds<-DESeq(dds)

dds

# 4. Results list

DE_result<-results(dds,contrast=c("condition","Group3","Group2"))


DE_list<-as.data.frame(DE_result)
head(DE_list)

nrow(DE_list[which(DE_list$padj<0.05), ])
nrow(DE_list[which(DE_list$padj<0.01), ])
nrow(DE_list[which(DE_list$padj<0.01 & abs(DE_list$log2FoldChange)>2),])

# 6. Write resuts

dataout <- DE_list[which(DE_list$padj<0.01),]
head(dataout)

write.csv(dataout,file="results_Group3vsGroup2.csv",row.names=T,col.names=T,quote=F)



#Make a volcano plot with the log2FC and the adjusted pvalue

ggplot(dataout, aes(x=log2FoldChange, y=-log10(pvalue)))+geom_point()


ggplot(DE_list, aes(x=log2FoldChange, y=-log10(pvalue)))+geom_point()

ids <- is.na(DE_list$padj)

DE_list <- DE_list[!ids,]
head(DE_list)


ids <- DE_list$padj<0.05

DE_list$sig <-  DE_list$padj<0.05

head(DE_list)

ggplot(DE_list, aes(x=log2FoldChange, y=-log10(pvalue), col=as.factor(sig)))+geom_point()



#Make a heatmap of the differentially expressed genes with heatmap.2
library(gplots)

head(rpm)

rpm <- rpm[,21:28]

ids <- row.names(rpm) %in% row.names(dataout)

rpmDE <- rpm[ids,]


my_palette <- colorRampPalette(c("blue","black", "red"))

#pdf("Heatmap.pdf", height=9, width=9)

mymatrix <- as.matrix(rpmDE)

heatmap.2(mymatrix,Colv=F,col=my_palette, trace='none', scale="row")

#dev.off()


heatmap.2(mymatrix,Colv=F,col=my_palette,trace='none', labRow=NA, margins = c(10,1),
          hclust=function(x) hclust(x,method="complete"), 
          distfun=function(x) as.dist((1-cor(t(x)))/2),
          scale="row")



#All Data


rpm <- 1000000*t(t(data)/colSums(data))
colSums(rpm)

#log normalization

#rpm <- log(1+rpm) #We skip this step



ids <- row.names(rpm) %in% row.names(dataout)

rpmDE <- rpm[ids,]


my_palette <- colorRampPalette(c("blue","black", "red"))

#pdf("Heatmap.pdf", height=9, width=9)

mymatrix <- as.matrix(rpmDE)

heatmap.2(mymatrix,Colv=F,col=my_palette, trace='none')

#scale rows

heatmap.2(mymatrix,Colv=F,col=my_palette, trace='none', scale="row")


#correlation distance

heatmap.2(mymatrix,Colv=F,col=my_palette,trace='none', labRow=NA, margins = c(10,1),
          hclust=function(x) hclust(x,method="complete"), 
          distfun=function(x) as.dist((1-cor(t(x)))/2),
          scale="row")


#upload the list of DE genes to metascape.org


#Find all upregulated genes in G0. That is, compare G0 with all the other conditions.

#Repeat for G1

#Repeat for G2

#Repeat for G3

#Make an excel file with the 4 lists in 4 different columns 
#and upload it to metascape with the option "Multiple gene list" selected



