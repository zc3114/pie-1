setwd("~/Dropbox/BG")
setwd('C:/Users/zc3114/dropbox/BG')
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)
library(cluster)
library(gplots)
library(gtools)
library(gdata)
library(caTools)
library(bitops)
library(RColorBrewer)

md<-read.csv('md.csv')
quadplot <- function(x){
  onegene<-subset(md, gene==print(x))
  ggplot()+
    geom_point(data=onegene, aes(x=time_min, y=rel_expression), size=1, alpha=1, colour="black")+
    geom_smooth(data=onegene, aes(x=time_min, y=rel_expression), method=lm, formula=y~poly(x,2), size=0.5, alpha=0.5, colour="red", se=FALSE)+
    ylab("relative expression")+
    xlab("time (min)")+
    coord_cartesian(xlim = c(0, 800), ylim = c(0, 5.6))+
    scale_x_continuous(breaks=pretty_breaks(n=10))+
    scale_y_continuous(breaks=pretty_breaks(n=7))+
    ggtitle(print(onegene$gene[1]))+
    theme_classic(base_size = 12, base_family = "")+
    theme(axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
}

interaction <- read.csv('pie-1.csv')
interaction.gene <- unique(c(as.character(interaction$Effector),as.character(interaction$Affected)))

mart<-read.csv('mart_export.txt')

interaction.gene.id <- NA
non.id <- NA
a=b=1
for (i in 1:length(interaction.gene)){
  n <- as.character(mart$Gene.stable.ID[mart$Gene.name == interaction.gene[i]][1])
  if (is.na(n)==T){
    non.id[a]<-interaction.gene[i]
   a=a+1
  }
  else{
    interaction.gene.id[b]<-n
    b=b+1
  }  
}  


#disabled beacause take too long - run only once
##alternatively, load d using d<- read.csv('d.csv')[,2:55]
#mydf<-read.csv('transp.evodevomics.csv')
mydf <-mydf [order(mydf $time_min),]

a=NA
for (i in 1:length(interaction.gene.id)){
  a[i]<- pmatch(interaction.gene.id[i],colnames(mydf))
}

d <- mydf[,c(2,a[!(is.na(a))])]

#w/o WBG id:
#d<- read.csv('d.csv')[,2:55]

dm<-dist(d[,2:dim(d)[2]], method = "euclidean", diag = TRUE, upper = F, p = 1)

heatmap<-heatmap.2(data.matrix(d[,2:dim(d)[2]]), 
                   col=bluered(25), 
                   trace="none",
                   density.info="none", 
                   mar=c(10,10),
                   symkey=TRUE,
                   dendrogram="column",
                   labRow=d$time_min,
                   scale=c("col"),
                   Rowv=FALSE,
                   hclustfun=function(dm) hclust((dm),method="ward.D2")) 
source('cluster.plot.R')
cluster.plot(df=d,cor='pearson',cut.off=0.8,communities = 'neg')#'pos' for communi-
#ties with + interaction,'neg' for - interaction, or F for both(whole network, no communities)
