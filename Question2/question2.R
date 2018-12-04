Data <- read.table("data.txt", header=TRUE)
Data

hist(Data$ConversionDeLaRarete, xlab="Niveau de rareté", ylab="Fréquence", main="Rareté des cartes", labels=TRUE)

boxplot(Data$ConversionDeLaRarete)



