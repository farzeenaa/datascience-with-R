crime_data <- read.csv(file.choose())

# Normalizing the data 
normalized_crime_data<-scale(crime_data[,2:5])
?dist
d <- dist(crime_data, method = "euclidean") # distance matrix
?hclust
fit <- hclust(d, method="complete")
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)

?cutree
rect.hclust(fit,plot(fit,hang = -1),k=4, border="red")
?rect.hclust
groups <- cutree(fit, k=4) # cut tree into 5 clusters

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime_data, membership)

View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(crime_data[,2:5],by=list(final$membership),mean)

