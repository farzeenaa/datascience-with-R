mydata<-read.csv(file.choose())
View(mydata)
## the first column in mydata has wine type
View(mydata[-1]) 
# mydata[-1] -> Considering only numerical values for applying PCA
wine_data <- mydata[,-1]
attach(wine_data)
cor(wine_data)
# cor = TRUE use correlation matrix for getting PCA scores
?princomp
pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)
str(pcaObj)
## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
#pcaObj$loadings
pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data
# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
mydata<-cbind(mydata,pcaObj$scores[,1:3])
View(mydata)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-mydata[,14:16]
# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance
#scree plot for selection of no .of of clusters with suggested pcs
twss<-NULL
for (i in 2:14) {
  twss<-c(twss,type="b",xlab="Number of clusters",ylab="within groups sum of squares",main="screeplot")
  
}
# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1)# Displaying Dendrogram
plot(fit1,hang = -1)
groups<-cutree(fit1,7) # Cutting the dendrogram for 7 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)
final1<-cbind(membership_1,mydata) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,9:11)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the wine data on membership_1
library(kselection)
#kmeans clustering with pc
k_7<-kmeans(norm_clus,7)
str(k_7)
View(k_7)
mydata$cluster<-as.matrix(k_7$cluster)
View(mydata)
aggregate(mydata,by=list(mydata$cluster),mean)
wine_norm<-scale(wine_data)
dist2<-dist(wine_norm,method = "euclidean")
twss_2<-NULL
for (i in 2:14) {
  twss_2<-c(twss_2,kmeans(wine_norm,i)$tot.withinss)
  
}
plot(2:14,twss_2,type = "b",xlab="Number of clusters",ylab = "within group of squares",main = "screeplot")
fit2<-hclust(dist2,method = "complete")
plot(fit2)
plot(fit2,hang = -1)
rect.hclust(fit2,k=5,border="red")
groups2<-cutree(fit2,5)
membership_2<-as.matrix(groups2)
View(membership_2)
final2<-cbind(membership_2,wine_data)
View(final2)
View(aggregate(final2[,-c(2,9:11)],by=list(membership_2),FUN = mean))
#kmeans clustering 
k_5<-kmeans(wine_norm,5)
str(k_5)
View(k_5)
wine_data$cluster<-as.matrix(k_5$cluster)
View(wine_data)
aggregate(wine_data,by=list(wine_data$cluster),mean)
