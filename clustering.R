library(flexclust)
library(ggplot2)


data<- data.frame(x=c(1,2,1.5,4.5,3.5,4.5,3.5,4,2.8,1,2.5,1.5,2.5,2,1,1.5,1.5,2,4,4.5,4.5,4,4,4,4.5,5,5), y=c(1,1,2,2.5,2.5,2.7,3.5,2.7,1.2,2.5,2.1,2.5,1.6,1.8,1.5,1.5,1.5,1.3,3,3.5,3.5,3.3,4,2.5,3.1,4,2.7), Species=c("A", "B", "C","D","F","G","K","P","M","N","L","K","V","J","H","S","I","R","C","W","Y","Q","O","I","V","Z","X"))
data
data1<- data
data1$Species= NULL

##Step 1

dataCent<- rbind(cent1=data1[1,], cent2= data1[2,])

##Slect centroids, and calculate distance from each point to each centroid


dis1<- round(dist2(data1, dataCent[1,], method = "euclidean", p=2),3)
dis2<- round(dist2(data1, dataCent[2,], method = "euclidean", p=2),3)
dataDist<- data.frame(x=data1$x, y=data1$y, Cent1=c(dis1), Cent2=c(dis2), species=data$Species)
dataDist
for (i in 1:nrow(dataDist)){
  if (dataDist$Cent1[i] < dataDist$Cent2[i])  {dataDist$clust[i]<-1 } else
  { dataDist$clust[i]<-2  }}
g0<- data.frame(x=data1$x, y=data1$y, clust=dataDist$clust,species=data$Species)

## Assing each point to appropriate cluster.

g0

theme_set(theme_gray(base_size = 17))
 ggplot(data1, aes(x,y, color="red"))+ geom_point(size=7, alpha=4/5)+
  geom_point(data=dataCent, aes(x, y, fill="Centroids"), color= "black",size=5, shape=17)+
  guides(colour=FALSE)+
  scale_fill_manual(name="Centroids", values=c(dataCent$x, dataCent$y))+ggtitle("Step 1")

##Step 2

##New Centroids

p1<-which(g0$clust==1)
p2<-which(g0$clust==2)
cent21<- c(x= sum(sapply(data$x[p1], function(x) x/length(p1))), y= sum(sapply(data$y[p1], function(x) x/length(p1))))
cent22<- c(x= sum(sapply(data$x[p2], function(x) x/length(p2))), y= sum(sapply(data$y[p2], function(x) x/length(p2))))

##Distance for new centroids

dataCent2<- data.frame(rbind(cent1= cent21, cent2= cent22))
dis21<- round(dist2(data1, cent21, method = "euclidean", p=2),3)
dis22<- round(dist2(data1, cent22, method = "euclidean", p=2),3)
dataDist<- data.frame(x=data1$x, y=data1$y, Cent21=c(dis21), Cent22=c(dis22), species=data$Species)
dataDist
for (i in 1:nrow(dataDist)){
  if (dataDist$Cent21[i] < dataDist$Cent22[i]) { dataDist$clust[i]<- 1} else
  { dataDist$clust[i]<-2 }
  }
g1<- data.frame(x=data1$x, y=data1$y, clust=dataDist$clust, species=data$Species)

g1

g<- ggplot(g1, aes(x,y, color=factor(g1$clust)))+ geom_point(size=7, alpha=4/5)+
  geom_point(data=dataCent2, aes(x, y, fill="Centroids"), color= "black",size=5, shape=17)+
  scale_fill_manual(name="Centroids", values=c(dataDist$Cent21, dataDist$Cent22))+ ggtitle("Step 2")+
  scale_color_discrete(name="Groups")
g

##Step 3

##New centroids

p21<-which(g1$clust==1)
p22<-which(g1$clust==2)
cent31<- c(x= sum(sapply(data1$x[p21], function(x) x/length(p21))), y= sum(sapply(data1$y[p21], function(x) x/length(p21))))
cent32<- c(x= sum(sapply(data$x[p22], function(x) x/length(p22))), y= sum(sapply(data1$y[p22], function(x) x/length(p22))))
dataCent3<- data.frame(rbind(cent1= cent31, cent2= cent32))

##Distance for new centroids

dis31<- round(dist2(data1, cent31, method = "euclidean", p=2),3)
dis32<- round(dist2(data1, cent32, method = "euclidean", p=2),3)
dataDist2<- data.frame(Cent31=c(dis31), Cent32=c(dis32), species=data$Species)
dataDist2
for (i in 1:nrow(dataDist)){
  if (dataDist2$Cent31[i] < dataDist2$Cent32[i]) { dataDist2$clust[i]<-1} else
  { dataDist2$clust[i]<-2 }
}
g2<- data.frame(x=data1$x, y=data1$y,clust=dataDist2$clust, species=data$Species)

 
## Repeate everything as in previous steps.
g2

g<- ggplot(g2, aes(x,y, color=factor(g2$clust)))+ geom_point(size=7, alpha=4/5)+
  geom_point(data=dataCent3, aes(x, y, fill="Centroids"), color= "black",size=5, shape=17)+
  scale_fill_manual(name="Centroids", values=c(dataDist2$Cent31, dataDist2$Cent32))+ ggtitle("Step 3")+
  scale_color_discrete(name="Groups")
g

##New centroids

p31<-which(g2$clust==1)
p32<-which(g2$clust==2)
cent41<- c(x= sum(sapply(data1$x[p31], function(x) x/length(p31))), y= sum(sapply(data1$y[p31], function(x) x/length(p31))))
cent42<- c(x= sum(sapply(data$x[p32], function(x) x/length(p32))), y= sum(sapply(data1$y[p32], function(x) x/length(p32))))
dataCent4<- data.frame(rbind(cent1= cent41, cent2= cent42))

##Distance for new centroids

dis41<- round(dist2(data1, cent41, method = "euclidean", p=2),3)
dis42<- round(dist2(data1, cent42, method = "euclidean", p=2),3)
dataDist3<- data.frame(Cent41=c(dis41), Cent42=c(dis42), species=data$Species)
dataDist3
for (i in 1:nrow(dataDist)){
  if (dataDist3$Cent41[i] < dataDist3$Cent42[i]) { dataDist3$clust[i]<-1} else
  { dataDist3$clust[i]<-2 }
}
g3<- data.frame(x=data1$x, y=data1$y,clust=dataDist3$clust, species=data$Species)

##Fix points in new clusters.
g3
g<- ggplot(g3, aes(x,y, color=factor(g3$clust)))+ geom_point(size=7, alpha=4/5)+
  geom_point(data=dataCent4, aes(x, y, fill="Centroids"), color= "black",size=5, shape=17)+
  scale_fill_manual(name="Centroids", values=c(dataDist3$Cent41, dataDist3$Cent42))+ ggtitle("Step 4")+
  scale_color_discrete(name="Groups")
g
 
##There is no changes on a plot(centroids remain the same), that's why process stooped.
##Step 4 is a answer.



