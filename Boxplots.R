setwd("~/Dropbox/Amphispiza Channel Islands/Measurement_Data")

library(ggplot2)
library(dplyr)

#sparrow
sparrowdata<-read.csv("1_SagesparrowdataforR.csv",sep=",",nrows=20000,header=T)
sparrowdata
head(sparrowdata)
sparrow<-data.frame(sparrowdata) #made local data frame of sparrow data
head(sparrow)

data.frame(head(flights)) #flight
data.frame(head(sparrow)) #made normal data frame to see all columns
head(sparrow)

#filter:keep rows matching criteria
filter(flights,Month==1,DayofMonth==1) #flight
filter(sparrow,BillL==6.8) 
filter(sparrow, Population==Coastal) #error coastal not found
#filter function only does numerical, not categorical? 
#Solution:Population=="Coastal"

#select:pick column by name
select(flights, DepTime, ArrTime)
select(sparrow, Population, WingL)

head(sparrow)
#calculate averages of traits for each population using summarise()
sparrow%>%group_by(Population) %>%
  summarise(avg_BillL=mean(BillL,na.rm=TRUE))
sparrow%>%group_by(Population)%>%
  summarise(avg_BillD=mean(BillD,na.rm=TRUE))
sparrow%>%group_by(Population)%>%
  summarise(avg_BillW=mean(BillW,na.rm=TRUE))
sparrow%>%group_by(Population)%>%
  summarise(avg_TailL=mean(TailL,na.rm=TRUE))
sparrow%>%group_by(Population)%>%
  summarise(avg_WingL=mean(WingL,na.rm=TRUE))
sparrow%>%group_by(Population)%>%
  summarise(avg_TarToe=mean(TarToe,na.rm=TRUE))
?summarise
#scatterplot of WingL(y-axis) by Population(x-axis)
ggplot(sparrow, aes(Population, WingL)) + geom_point()

#------BOXPLOTS-----------------------------------------------------
#boxplot of WingL(y-axis) by Population (x-axis)
WingL<-ggplot(sparrow, aes(Population, WingL)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(aes(fill=Population))+
  theme(plot.title = element_text(size=16, face="bold", vjust=1))+
  labs(y="Wing Length (cm)")+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())
WingL
WL<-aov(WingL~Population, data=sparrow) #one-way ANOVA
summary(WL)
TukeyHSD(WL)
ggsave("WingLBoxplotIMG2.jpeg")
ggsave("WingLength2.tiff")
#boxplot of TailL
TailL<-ggplot(sparrow, aes(Population, TailL)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(aes(fill=Population))+
  theme(plot.title = element_text(size=16,face="bold",vjust=1))+
  labs(y="Tail Length (cm)")+
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
TailL
TL<-aov(TailL~Population, data=sparrow) #one-way ANOVA
summary(TL)
TukeyHSD(TL)
ggsave("TailLBoxplotIMG2.jpeg")
ggsave("TailLength2.tiff")
head(sparrow)
#boxplot of BillL
BillL<-ggplot(sparrow, aes(Population, BillL))+
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(aes(fill=Population))+
  theme(plot.title = element_text(size=16,face="bold",vjust=1))+
  labs(y="Bill Length (cm)")+
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
BillL
BL<-aov(BillL~Population, data=sparrow) #one-way ANOVA
summary(BL)
TukeyHSD(BL)
ggsave("BillLBoxplotIMG2.jpeg")
ggsave("BillLength2.tiff")
#boxplot of BillD
BillD<-ggplot(sparrow, aes(Population,BillD))+
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(aes(fill=Population))+
  theme(plot.title = element_text(size=16,face="bold",vjust=1))+
  labs(y="Bill Depth (cm)")+
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
BillD
BD<-aov(BillD~Population, data=sparrow) #one-way ANOVA
summary(BD)
TukeyHSD(BD)
ggsave("BillDBoxplotIMG2.jpeg")
ggsave("BillDepth2.tiff")
head(sparrow)
#boxplot of BillW
BillW<-ggplot(sparrow, aes(Population, BillW))+
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(aes(fill=Population))+
  theme(plot.title = element_text(size=16,face="bold",vjust=1))+
  labs(y="Bill Width (cm)")+
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
BillW
BW<-aov(BillW~Population, data=sparrow) #one-way ANOVA
summary(BW)
TukeyHSD(BW)
ggsave("BillWBoxplotIMG2.jpeg")
ggsave("BillWidth2.tiff")
#boxplot of TarToe
TarToe<-ggplot(sparrow, aes(Population, TarToe))+
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(aes(fill=Population))+
  theme(plot.title = element_text(size=16,face="bold",vjust=1))+
  labs(y="Tarsus and Toe Length (cm)")+
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())+
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom')
TarToe
TT<-aov(TarToe~Population, data=sparrow) #one-way ANOVA
summary(TT)
TukeyHSD(TT)
ggsave("TarToeBoxplotIMG2.jpeg")
ggsave("LEGEND.tiff")
#boxplot of Bill Volume (BillLxBillDxBillW)
BillVol<-ggplot(sparrow, aes(Population,BillVol))+
  stat_boxplot(geom ='errorbar')+ #whisker ends
  geom_boxplot(aes(fill=Population))+
  ggtitle("Bill Volume Variation in \n Subspecies Populations of A.belli")+
  theme(plot.title = element_text(size=16,face="bold",vjust=1))+
  labs(y="Bill Volumte (cm^3)")+
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
BillVol
BV<-aov(BillVol~Population, data=sparrow) #one-way ANOVA
summary(BV)
TukeyHSD(BV)
ggsave("BillVolBoxplotIMG.jpeg")

#------------------------------------------
#practice making boxplot with Iris Tutorial
#http://sharpstatistics.co.uk/r/ggplot2-guide/
data(iris)
ggplot(iris,aes(Sepal.Length,Sepal.Width)) #error no layers in plot
  #because the data frame iris has been specified along with which
  #2 variables to plot the axes, but no info have been given on
  #what graphic component to use. 
ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point()
ggplot() + geom_point(data=iris,aes(Sepal.Length,Sepal.Width))
#the two lines produce the same plot

# %+% operator to update a chart with another data set
setosaData<-subset(iris, Species == "setosa") #dataset1
verData<-subset(iris, Species == "versicolor") #dataset2
p<-ggplot(setosaData,aes(Sepal.Length,Sepal.Width))+geom_point()
p #p is the code for the chart
p %+% verData