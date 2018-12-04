require(xlsx)
data = read.xlsx(file.choose(), sheetName = "Sheet1")
data
#1896985_1899696_1903555.xlsx

midPrice = data$ResaleMid

### Obtention des diagrammes de statistiques descriptives
jpeg("bad-price-box.jpeg", width = 600, height = 350)
boxplot(midPrice,main="Figure 1: Analyse du prix moyen boîte à moustache")
dev.off()

jpeg("bad-price-hist.jpeg",width = 600, height = 350)
hist(midPrice, breaks=30, main="Figure 2: Analyse du prix moyen histogramme",xlab="Prix moyen", ylab="Fréquence")
dev.off()

### Effacement des données abhérentes du vecteur de prix moyen
quart = quantile(midPrice)
quart
IQR = quart[[4]] - quart[[2]]
IQR
IQRmod = IQR  *1.5
IQRmod
upperBoundPrice = quart[[4]] + IQRmod
upperBoundPrice
lowerBoundPrice = quart[[2]] - IQRmod
lowerBoundPrice

dataFilteredByPrice = subset(data, midPrice < upperBoundPrice & midPrice > lowerBoundPrice )


### Nouveaux diagrammes pour la statistiques descrives

jpeg("good-price-box.jpeg",width = 600, height = 350)
boxplot(dataFilteredByPrice$ResaleMid, main="Figure 3: Analyse du prix moyen filtré boîte à moustache")
dev.off()

jpeg("good-price-hist.jpeg",width = 600, height = 350)
hist(dataFilteredByPrice$ResaleMid, breaks=30, main="Figure 4: Analyse du prix moyen filtré histogramme", xlab="Prix moyen filtré", ylab="Fréquence")
dev.off()


### Estimateur pour l'effectif de la population 
averagePrice = mean(dataFilteredByPrice$ResaleMid)

### Intervalle de conficance pour l'effectif de la population
n = length(dataFilteredByPrice$ResaleMid)
s = var(dataFilteredByPrice$ResaleMid)
error = qnorm(0.975)*s/sqrt(n)
error

### Estimateur pour lambda (loi exponentielle)
lambdaEstimator = 1/averagePrice
lambdaEstimator

### Intervalle de confiance pour lambda
lambdaErrorDown = abs(lambdaEstimator - 1/(averagePrice+error))
lambdaErrorDown
lambdaErrorUp = abs(lambdaEstimator - 1/(averagePrice-error))
lambdaErrorUp


### Test khi deux pour une loi exponentielle
exponentialKhiDeux = function(k, lambdaEstimator) {
  ### khi-deux effectifs observées
  observedEffectives = vector()
  maxPrice = 0
  for(price in dataFilteredByPrice$ResaleMid) {
    if(price > maxPrice) {
      maxPrice = price
    }
  }
  minPrice = Inf
  for(price in dataFilteredByPrice$ResaleMid) {
    if(price < minPrice) {
      minPrice = price
    }
  }
  minPrice
  priceInterval = maxPrice-minPrice
  VInterval = priceInterval/k
  
  priceVector <- dataFilteredByPrice$ResaleMid
  
  
  observedEffective = vector()
  for(i in 1:k) {
    observedEffective = 0
    for(price in priceVector) {
      if (price>(i-1)*VInterval+minPrice && price < i*VInterval+minPrice) {
        observedEffective = 1 + observedEffective 
      }
    }
    observedEffectives[i] = observedEffective
  }
  print(observedEffectives)
  
  #Khi deux probabilité théorique pour chacun des intervalles
  pexp(1, rate=lambdaEstimator)
  theoreticalEffectives = vector()
  for(i in 1:(k-1)) {
    theoreticalEffectives[i] = (pexp(VInterval*(i),rate = lambdaEstimator) - pexp(VInterval*(i-1),rate = lambdaEstimator))
  }
  theoreticalEffectives[k] = 1 - pexp(VInterval*(k-1), rate=lambdaEstimator)
  print(theoreticalEffectives)
  
  chisq.test(observedEffectives, p=theoreticalEffectives)
}

lambdaEstimator
lambdaEstimator+lambdaErrorUp
lambdaEstimator-lambdaErrorDown

exponentialKhiDeux(15, lambdaEstimator)
exponentialKhiDeux(15, lambdaEstimator+lambdaErrorUp)
exponentialKhiDeux(15, lambdaEstimator-lambdaErrorDown)
exponentialKhiDeux(25, lambdaEstimator)
exponentialKhiDeux(25, lambdaEstimator+lambdaErrorUp)
exponentialKhiDeux(25, lambdaEstimator-lambdaErrorDown)

linearMod = lm( ResaleMid ~ ConvertedManaCost, data=dataFilteredByPrice)
print(linearMod)
summary(linearMod)$r.squared

jpeg("plot-line.jpeg",width = 600, height = 350)
plot(dataFilteredByPrice$ConvertedManaCost, dataFilteredByPrice$ResaleMid,
     main="Figure 5: Évaluation graphique de la relation entre le prix en jeu et à l'extérieur du jeu",
     xlab = "Prix en jeu", ylab = "Prix à l'extérieur du jeu")
abline(0.254836, -0.001725, col = 'red')
dev.off()

## -------------------------------------
## ------ Analyse de la Rareté ---------
## -------------------------------------

# Histogramme de la fréquence pour la rareté des cartes
hist(data$RarityConversion, xlab="Niveau de rareté", ylab="Fréquence", main="Figure 6: Rareté des cartes", labels=TRUE)

# Différence entre le prix élevé et le prix médian
diff = data$ResaleHigh - data$ResaleMid
diffFiltered = dataFilteredByPrice$ResaleHigh - dataFilteredByPrice$ResaleMid

## Régression linéaire (non filtrée)
rarete.lm = lm(diff~data$RarityConversion, data=data)
summary(rarete.lm)
print(rarete.lm)


## Analyse des résidus (non filtrée)
rarete.res = resid(rarete.lm)
plot(data$RarityConversion, rarete.res, ylab="Résidus", xlab="Rareté", main="Figure 7: Analyse des Résidus")
abline(-3.320 ,3.703)


## Régression linéaire (filtrée)
raretef.lm = lm(diffFiltered~dataFilteredByPrice$RarityConversion, data=dataFilteredByPrice)
summary(raretef.lm)
# print(raretef.lm)

## Analyse des résidus (filtrée)
raretef.res = resid(raretef.lm)
plot(dataFilteredByPrice$RarityConversion, raretef.res, ylab="Résidus", xlab="Rareté", main="Analyse des Résidus")
abline(-1.730,2.652)






