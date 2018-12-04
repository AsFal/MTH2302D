Data <- read.table("data.txt", header=TRUE)
Data

hist(Data$ConversionDeLaRarete, xlab="Niveau de rareté", ylab="Fréquence", main="Rareté des cartes", labels=TRUE)

### Estimateur pour l'effectif de la population 
avgPrice = mean(Data$ConversionDeLaRarete)

### Intervalle de conficance pour l'effectif de la population
n = length(Data$ConversionDeLaRarete)
n
s = var(Data$ConversionDeLaRarete)
s
error = qnorm(0.975)*s/sqrt(n)
error

### Estimateur pour lambda (loi exponentielle)
lambdaEstimator = 1/avgPrice
lambdaEstimator

### Intervalle de confiance pour lambda
lambdaErrorDown = abs(lambdaEstimator - 1/(avgPrice+error))
lambdaErrorDown
lambdaErrorUp = abs(lambdaEstimator - 1/(avgPrice-error))
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
  
  
  #Khi deux probabilité théorique pour chacun des intervalles
  pexp(1, rate=lambdaEstimator)
  theoreticalEffectives = vector()
  for(i in 1:(k-1)) {
    theoreticalEffectives[i] = (pexp(VInterval*(i),rate = lambdaEstimator) - pexp(VInterval*(i-1),rate = lambdaEstimator))
  }
  theoreticalEffectives[k] = 1 - pexp(VInterval*(k-1), rate=lambdaEstimator)
  
  chisq.test(observedEffectives, p=theoreticalEffectives)
}


#Regression lineaire

scatter.smooth(Data$ConversionDeLaRarete, diff)
cor(Data$ConversionDeLaRarete, diff)


