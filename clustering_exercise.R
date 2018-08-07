install.packages(c("cluster", "rattle.data","NbClust"))
data(wine, package="rattle.data")
head(wine)
#Exercise 1
df <- scale(wine[-1])

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(df)

#Exercise 2 - This is a 3 cluster solution - There's a distinct drop when moving
# 3 clusters, and then the decrease drops off

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)  
fit.km$size
fit.km$centers 
ct.km <- table(wine$Type, fit.km$cluster)
ct.km
library(flexclust)
randIndex(ct.km)
#The randindex value is close to 1, so it indicates a pretty good clustering
??clusplot
clusplot(wine, fit.km$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)