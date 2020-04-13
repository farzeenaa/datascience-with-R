e_quakes<-datasets::quakes
head(quakes)
tail(quakes)
quakes[,c(1,2)]
quakes$dep
summary(quakes$mag)
summary(quakes)
plot(quakes$mag)
plot(quakes$depth,quakes$stations)
plot(quakes)
#points and lines
plot(quakes$depth,type = "l")#p:points,l:lines,b:both
plot(quakes$depth,xlab = "depth ",
     ylab = "no.of instance",main="depth levels in NY city",
     col="blue")

#horizontal barplot
barplot(quakes$depth,main = "depth of earth",
        xlab="depth levels",col = "blue",horiz = F)
#histogram
hist(quakes$stations)
hist(quakes$stations,main="stations",
     xlab = "stations",col = "blue")

#single boxplot
boxplot(quakes$stations)

#multiple boxplots
boxplot(quakes[,1:4],main="multiple")

#margin of the grid(mar),
#no of rows and columns(mfrow),
#whether a border is to be included(bty)
#and position of the
#labels(las:1 for horizontal, las:0 for vertical)
#bty-box around the plot

par(mfrow=c(3,3),mar=c(2,5,2,1),las=1,bty="o")

plot(quakes$stations)
plot(quakes$depth,quakes$mag)
plot(quakes$depth,type="l")
plot(quakes$depth,type = "l")
plot(quakes$depth,type = "l")
barplot(quakes$depth,main="depth of land",
        xlab="depth level",col="green",horiz=T)
hist(quakes$mag)
boxplot(quakes$stations)
boxplot(quakes[,0:4],main="multiple box plots")
