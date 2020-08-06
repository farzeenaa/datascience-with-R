library(readxl)
Airline<-read.csv(file.choose())
View(Airline)
# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(Airline[,2:12]) #excluding the university name columnbefore normalizing
?dist
d <- dist(normalized_data, method = "euclidean") # distance matrix
?hclust
fit <- hclust(d, method="complete")
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)

?cutree
rect.hclust(fit, k=10, border="red")
?rect.hclust
groups <- cutree(fit, k=10) # cut tree into 5 clusters

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(Airline, membership)

View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(Airline[,-1],by=list(final$membership),mean)


# as data set containing large no.of observations we are not able to decide no.of clusters we will go for
#kmeans clustering
# selecting K for kmeans clustering using kselection
install.packages("kselection")
library(kselection)
?kselection

# To implement the parallel processing in getting the optimum number of
# clusters using kselection method 
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=4)
k <- kselection(Airline, parallel = TRUE, k_threshold = 0.9, max_centers=12)
k
#choosing best clusters as 2
k_2<-kmeans(normalized_data,2)
str(k_2)
View(k_2)
library(cluster)
Airline$cluster<-as.matrix(k_2$cluster)
View(Airline)
aggregate(Airline[,-1],by=list(Airline$cluster),mean)

# Creating a empty variable to store total within sum of sqares of clusters
twss <- NULL
for (i in 2:14){
  twss <- c(twss,kmeans(normalized_data,i)$tot.withinss)
}

twss
plot(2:14,twss,type="o",xlab="Number of clusters",ylab="Within groups sum of square",main="Screenplot")

#choosing best clusters as 4
k_4<-kmeans(normalized_data,4)
str(k_4)
View(k_4)
Airline$cluster<-as.matrix(k_4$cluster)
View(Airline)
aggregate(Airline[,-1],by=list(Airline$cluster),mean)

k_5<-kmeans(normalized_data,4)
str(k_5)
View(k_5)
Airline$cluster<-as.matrix(k_5$cluster)
aggregate(Airline[,-1],by=list(Airline$cluster),mean)
