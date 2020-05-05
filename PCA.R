setwd("~/Dropbox/Amphispiza Channel Islands/Measurement_Data")

library(dplyr) #to install package install.packages("nameof")
install.packages(ggplot2)
library(ggplot2)
library(stats)
library(grid)
library(gridExtra)

sparrowdata<-read.csv("1_SagesparrowdataforR.csv",sep=",",nrows=20000,header=T)
head(sparrowdata)
sparrowdata
sparrow<-data.frame(sparrowdata)
sparrow
head(sparrow)
class(sparrow)

#source:http://www.vince.vu/software/#ggbiplot
#http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
library(devtools)
library(ggbiplot)
head(sparrow)
population<-sparrow[,4]
population
subsp<-sparrow[,4:11]
head(subsp)
num.sp<-subsp[,3:8]
pca5<-prcomp(num.sp, center = TRUE, scale.=TRUE)
pca5
summary(pca5)
p5<-ggbiplot(pca5, obs.scale=1, var.scale=1,
             groups= population, ellipse=TRUE, circle=FALSE)
p5 <- p5 + scale_color_discrete(name = '')
p5 <- p5 + theme(legend.direction = 'horizontal', 
                 legend.position = 'top')
print(p5)
ggsave("PCA5-3.jpeg")
ggsave("PCA5-3.tiff")
#------------------------------------------------
#IGNORE EVERYTHING BELOW
#PCA3, No Population Column, No BillVol Column
SS3<-cbind(sparrow$WingL,
           sparrow$TailL,
           sparrow$BillL,
           sparrow$BillD,
           sparrow$BillW,
           sparrow$TarToe)
SS3
colnames(SS3)<-c("WinLen","TaiLen","BilLen","BilDen","BilWid","Tartoe")
head(SS3)
pca3<-prcomp(SS3,center=TRUE,scale=TRUE)
pca3
summary(pca3)
pca3$sdev
head(pca3$rotation)
head(pca3$x)
class(pca3)
biplot(pca3)
#PCA1 with populations
SS<-cbind(sparrow$Population, #made a matrix i think, all numercal
          sparrow$WingL,
          sparrow$TailL,
          sparrow$BillL,
          sparrow$BillD,
          sparrow$BillW,
          sparrow$TarToe)
SS
head(SS)
colnames(SS)<-c("Population","WinLen","TaiLen","BilLen","BilDen","BilWid","Tartoe","BilVol")
pca1<-prcomp(SS,center=TRUE,scale=TRUE) #it worked, don't know if it's right
pca1
summary(pca1)
pca1$sdev
head(pca1$rotation)
head(pca1$x)
class(pca1)
head(SS)
sparrow.pop<-SS[, 1]
sparrow.pop
biplot(pca1)
p<-ggbiplot(pca1,obs.scale=1,var.scale=1,
            ellipse = TRUE, circle = TRUE)
p
p <- p + scale_color_discrete(name='')
p <- p + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
#PCA2:take out Population column, don't need it
SS2<-cbind(sparrow$WingL,
           sparrow$TailL,
           sparrow$BillL,
           sparrow$BillD,
           sparrow$BillW,
           sparrow$TarToe,
           sparrow$BillVol)
SS2
colnames(SS2)<-c("WinLen","TaiLen","BilLen","BilDen","BilWid","Tartoe","BilVol")
head(SS2)
pca2<-prcomp(SS2,center=TRUE,scale=TRUE)
pca2
summary(pca2)
pca2$sdev
head(pca2$rotation)
head(pca2$x)
#Visualize PCA2:http://planspace.org/2013/02/03/pca-3d-visualization-and-clustering-in-r/
biplot(pca2)
ggsave("pca2.jpeg")
#past attempt 2 that didn't work
dataset = data.frame(species = SS2[,"Species"],
                     pca = pca$x)
pp <- ggplot(dataset) + geom_point(aes(pca.PC1, pca.PC2, colour = species, shape = species), size = 2.5) +
  labs(x = paste("PC1 (", percent(prop.pca[1]), ")", sep=""),
       y = paste("PC2 (", percent(prop.pca[2]), ")", sep=""))
#past attempt 1 that didn't work
class(pca2)
scores<-as.data.frame(pca2$x)
scores
plot(pca2, type="lines")
ggplot(data = scores, aes(x = PC1, y = PC2, 
                          label = rownames(scores))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "tomato", alpha = 0.8, size = 4) +
  ggtitle("PCA plot Sage Sparrow Morphological Traits") #worked but...
#change numbers to dots
#need a circle
#need arrows
#need all 6 traits labelled
#do not need to label populations because not analyzing pops
#code for circle below #doesn't work
circle<-function(center=c(0,0),npoints=100) {
  r=1 tt=seq(0,2*pi, length=npoints)
  xx=center[1]+r*cos(tt)
  yy=center[1]+r*sin(tt)
  return(data.frame(x=xx,y=yy))
}
correlations<-as.data.frame(cor(SS,pca1$x))
correlations