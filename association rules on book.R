library(arules)
library(arulesViz)
book_data<-read.csv(file.choose())
book_data1 <- read.transactions(file.choose(),format = "basket")
inspect(book_data1[1:10])
class(book_data1)
summary(book_data1)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(book_data1,topN=20)
book_data_rules<-apriori(book_data1,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
inspect(book_data_rules[1:5])
windows()
plot(book_data_rules,method = "scatterplot")
plot(book_data_rules,method = "grouped")
plot(book_data_rules,method = "graph")
plot(book_data_rules,method = "mosaic")

rules <- sort(book_data_rules,by="lift")
inspect(rules[1:4])

book_rules <- apriori(book_data1,parameter = list(support = 0.002,confidence = 0.05,minlen=4))
summary(book_rules)
inspect(book_rules[1:5])
windows()
plot(book_rules,method = "scatterplot")
plot(book_rules,method = "grouped")
plot(book_rules,method = "graph")
plot(book_rules,method = "mosaic")

rules <- sort(book_rules,by="lift")

inspect(rules[1:4])
