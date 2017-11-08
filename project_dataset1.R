#setwd("C:\\Users\\Vagisha Tyagi\\Desktop\\IDM2\\New Folder")
if(!require(rgl)) install.packages("rgl", dependencies = TRUE)
if(!require(cluster)) install.packages("cluster", dependencies = TRUE,  quiet=TRUE)
if(!require(dbscan)) install.packages("dbscan", dependencies = TRUE,  quiet=TRUE)

library(cluster)
library(dbscan)
library(rgl)



myNormalize <- function(x){
  if(typeof(x)=="double"){
    return ((x-min(x))/(max(x)-min(x)))
  }else{
    return(x)
  }
}


## hirearchy
myhier<-function(d1norm){
print("hierarchical clustering: ")
set.seed(12374)
clusters <- hclust(dist(d1norm[, 1:3], method = "euclidean"), method = 'centroid')
clusterCut <- cutree(clusters, 8)
open3d()
(plot3d(d1norm[,1:3], col=clusterCut, main="hierarchy clusters"))
rgl.snapshot("hierarchy.png","png")
t0 = table(clusterCut, d1norm$cluster)
print(table(clusterCut, d1norm$cluster))
bulbu<-(sum(diag(t0))/sum(t0))
print(sprintf("The Hierarchical clustering accuracy: %.3f",bulbu))
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
}


#k means
mykmeans<-function(d1norm){
print("K-means clustering: ")
temp<-c()
set.seed(12374)
for(i in 1:5){
  k <- kmeans(d1norm[,1:3], 8, nstart=25, iter.max = 100,  algorithm="MacQueen")
  open3d()
  (plot3d(d1norm[,1:3], col=k$cluster, main="k-means clusters"))
  rgl.snapshot("kmeans.png","png")
  t1<- table(k$cluster, d1norm$cluster)
  s2<-(sum(diag(t1))/sum(t1))
  #temp <- append(temp, s2)
  temp<- append(s2, temp)
  
}
print(sprintf("The average K means clustering accuracy: %.3f",mean(temp)))
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
}

#density
mydens<-function(d1norm){
print("Density based clustering: ")
set.seed(12374)
#kNNdistplot(d1norm[,1:3], k=8)
#abline(h=0.13, col="red")
db <- dbscan(d1norm[,1:3], 0.13, 20)
jpeg("density.jpg")
(plot(d1norm, col=db$cluster))
dev.off()
#points(d1norm[db$cluster==0,], pch = 3, col = "grey")
print("result table: ")
t1<-(table(db$cluster, d1norm$cluster))
print(t1)
ssup<-(sum(diag(t1))/sum(t1))
print(sprintf("The density based clustering accuracy: %.3f",ssup))
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
}


#graph
mygraph<-function(d1norm){
print("Graph based clustering: ")
set.seed(12374)
cl <- sNNclust(d1norm[,1:3], k = 8, eps = 1, minPts = 8)
jpeg("graph.jpg")
plot(d1norm[,1:3], col = cl$cluster + 1L, cex = .5)
dev.off()
print("result table: ")
tt<-(table(cl$cluster, d1norm$cluster))
print(tt)
acc <- sum(sum(diag(tt))/sum(tt))
print(sprintf("The graph based clustering accuracy: %.3f",acc))
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
}




main<-function(){
  #read in data
  d1 <- read.csv(file="dataset1.csv", header=TRUE, sep=",")
  set.seed(12374)
  d1 <- d1[sample(nrow(d1)),]
  d1norm <- as.data.frame(lapply(d1, myNormalize))
  #print(plot3d(d1norm[,1:3], col=d1$cluster, main="Actual"))
  myhier(d1norm)
  mykmeans(d1norm)
  mydens(d1norm)
  mygraph(d1norm)
  
}
main()


