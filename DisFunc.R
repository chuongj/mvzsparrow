setwd("~/Dropbox/Amphispiza Channel Islands/Measurement_Data")
install.packages("name")
library(MASS)
library(dplyr)
library(ggplot2)
library(adegenet) #pretty lda/pca circle graph package
library(scales)
library(grid)
library(gridExtra)
library(candisc)

malfem<-read.csv("IslandsparrowdataforR.csv")
head(malfem)
island<-data.frame(malfem)
island
class(island)
head(island)

#filter to see males and females
filter(island,Sex=="M")
filter(island,Sex=="F")
#discriminant function on sexes, M or F
ldamf<-lda(Sex~WinLen+TaiLen+BilLen+
            BilDep+BilWid+TarToe,data=island)
ldamf
#R predicts the sex of the sparrows based on the stats
pred<-predict(ldamf,island)$class
pred
predmf<-predict(ldamf,island)
predmf
#how well did prediction fit-classificaton matrix
table(pred,island[,2])
#leave one out cross-validation, CV=TRUE
lda(Sex~WinLen+TaiLen+BilLen+
             BilDep+BilWid+TarToe,data=island, CV=TRUE)
#Visualize - DO NOT NEED TO VISUALIZE THIS FOR THE PAPER
plot(ldamf) #histogram
plot(ldamf, dimen=1,type="both" ) #adds lines to histogram

prop.ldamf<-ldamf$svd^2/sum(ldamf$svd^2)
datasetmf<-data.frame(species=island[,"Sex"],lda=predmf$x)
p2 <- ggplot(datasetmf)+geom_point(aes(lda.LD1, lda.LD2, colour = species), size = 6) + 
  labs(x = paste("LD1 (", percent(prop.ldamf[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.ldamf[2]), ")", sep="")) +
  theme(legend.title = element_text(colour="black", size=12, face="bold"))+
  scale_color_discrete(name="Sex")+
  ggtitle("Linear Discriminant Analysis \n between Male and Female A.b.clementeae")+
  theme(plot.title = element_text(size=16, face="bold", vjust=1))
p2
#didn't work >>> "lda.LD1 not found"...really weird
#it's ok. DO NOT NEED this male female graph
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DF on All 4 Populations and 6 Traits
sparrowdata<-read.csv("1_SagesparrowdataforR.csv")
sparrow<-data.frame(sparrowdata)
head(sparrow)

#LDA
ldapop<-lda(Population~WingL+TailL+BillL+
              BillD+BillW+TarToe,data=sparrow)
ldapop
#predict
predpop<-predict(ldapop,sparrow)$class #for classification matrix
predpop
predplot<-predict(ldapop,sparrow) #general report, for visualization
predplot
#how well did prediction fit?
head(sparrow)
table(predpop,sparrow[,4])
#take one out cross-validation, add CV=TRUE to lda
lda(Population~WingL+TailL+BillL+
      BillD+BillW+TarToe,data=sparrow, CV=TRUE)
#Visualize
plot(predplot$x)
#use these 2 tutorials:
#https://tgmstat.wordpress.com/2014/01/15/computing-and-visualizing-lda-in-r/
#https://gist.github.com/thigm85/8424654
prop.lda<-ldapop$svd^2/sum(ldapop$svd^2)
prop.lda
dataset<-data.frame(species=sparrow[,"Population"],lda=predplot$x)
p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = species), size = 6) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep="")) +
  theme(legend.title = element_text(colour="black", size=12, face="bold"))+
  scale_color_discrete(name="Population")+
  xlim(c(-4,4))+ylim(c(-4,4))+
  coord_equal()
p1
ggsave("LDA_4pop_6traits.jpeg")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Histogram LDA for Coastal and Island, refer to Cicero, Koo Paper, 2006, Auk
coastal<-filter(sparrow, Population=="Coastal")
coastal
isl<-filter(sparrow,Population=="Island")
isl
class(coastal) #dataframe
class(isl) #dataframe
coaisl<-rbind_list(coastal, isl)
coaisl
lda(Population~WingL+TailL+BillL+
              BillD+BillW+TarToe,data=coaisl)
merge1<-merge(coastal,isl,all.y="Population","Subspecies",
              "WingL","TailL","BillD","BillL","BillW","TarToe",sort=TRUE,
              all.x=TRUE,all.y=TRUE)
merge1
#didntwork, just made a new cvs file
coastisland<-read.csv("coastalislandsubsetforR.csv")
coaisl<-data.frame(coastisland)
class(coaisl)
ldacoaisl<-lda(Population~WingL+TailL+BillL+ #use for visualization
      BillD+BillW+TarToe,data=coaisl)
#R predicts the class, etc of the sparrows based on the stats
prepclass<-predict(ldacoaisl,coaisl)$class #need for classification matrix
prepclass
predci<-predict(ldacoaisl,coaisl) #need as a general report
predci
#how well did prediction fit-classificaton matrix
head(coaisl)
table(prepclass,coaisl[,4])
#take one out cross-validation, add CV=TRUE to lda
lda(Population~WingL+TailL+BillL+
      BillD+BillW+TarToe,data=coaisl, CV=TRUE)
#VISUALIZE Histogram
plot(ldacoaisl,type="h",col="darkgray",lwd=1)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CANONICAL ANALYSIS REDO - 4 populations, 6 traits 

#need canonical discriminant analysis
#need canonical means and canonical roots 
#standardized canonical coefficients 
#try the package candisc

#grass example
head(Grass)
grass.mod <- lm(cbind(N1,N9,N27,N81,N243) ~ Block + Species, data=Grass)
grass.mod
Anova(grass.mod,test="Wilks")
grass.can1 <-candisc(grass.mod, term="Species")
plot(grass.can1, type="n")
heplot(grass.can1, scale=6, fill=TRUE)

#sparrow
head(sparrow)
sparrow
pop.mod<-lm(cbind(WingL,TailL,BillL,BillD,BillW,TarToe) ~ Population, data=sparrow )
pop.mod
Anova(pop.mod,test="Wilks")
pop.can1<-candisc(pop.mod,term="Population")
pop.can1
plot(pop.can1, type = "n")
heplot(pop.can1,scale=6,fill=TRUE)

pop.can2<-candisc(pop.mod,data=sparrow) #unfilled graph
pop.can2
col <- rep(c("red", "black", "blue"), each=50)
pch <- rep(1:3, each=50)
plot(pop.can2,col=col,pch=pch)
heplot(pop.can2)

pop.can3<-candisc(pop.mod, data=sparrow,ndim=1) #dimensions plot
pop.can3
plot(pop.can3)
#I do not know how to get canonical means, canonical roots, coefficients
#I do have canonical r squared 

#--iris data
head(iris)
iris.mod <- lm(cbind(Petal.Length, Sepal.Length, Petal.Width, Sepal.Width) ~ Species, data=iris)
iris.mod
iris.can <- candisc(iris.mod, data=iris)
iris.can
#-- assign colors and symbols corresponding to species
col <- rep(c("red", "black", "blue"), each=50)
pch <- rep(1:3, each=50)
plot(iris.can, col=col, pch=pch)
heplot(iris.can)
# 1-dim plot
iris.can1 <- candisc(iris.mod, data=iris, ndim=1)
plot(iris.can1)
#-------
