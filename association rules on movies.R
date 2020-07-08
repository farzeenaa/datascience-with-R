library(arules)
library(arulesViz)
movies<-read.csv(file.choose())
movies1<-read.transactions(file.choose(),format="basket")
inspect(movies1[1:10])
class(movies1)
summary(movies1)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(movies1,topN=20)
movies_rules<-apriori(movies1,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
summary(movies_rules)
inspect(movies_rules[1:5])
windows()
plot(movies_rules,method = "scatterplot")
plot(movies_rules,method = "grouped")
plot(movies_rules,method = "graph")
plot(movies_rules,method = "mosaic")
rules <- sort(movies_rules,by="lift")

inspect(movies_rules[1:4])

movies_rules <- apriori(movies1,parameter = list(support = 0.002,confidence = 0.05,minlen=5))
inspect(movies_rules[1:5])
windows()
plot(movies_rules,method = "scatterplot")
plot(movies_rules,method = "grouped")
plot(movies_rules,method = "graph")
plot(movies_rules,method = "mosaic")

rules <- sort(rules,by="lift")

inspect(rules[1:4])
