require(xlsx)
data = read.xlsx("1896985_1899696_1903555.xlsx", sheetName = "Sheet1")
data

cmc <- data$ConvertedManaCost
cmc

midPrice = data$ResaleMid
midPrice

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

hist(dataFilteredByPrice$ResaleMid, breaks=50)

averagePrice = mean(dataFilteredByPrice$ResaleMid)
averagePrice
n = length(dataFilteredByPrice$ResaleMid)
s = var(dataFilteredByPrice$ResaleMid)
error = qnorm(0.975)*s/sqrt(n)
error

lambdaEstimator = 1/averagePrice
lambdaEstimator


lambdaErrorDown = abs(lambdaEstimator - 1/(averagePrice+error))
lambdaErrorDown
lamdaErrorUp = abs(lambdaEstimator - 1/(averagePrice-error))
lamdaErrorUp

k = 15
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
priceInterval
VInterval = priceInterval/k
VInterval

priceVector <- dataFilteredByPrice$ResaleMid
priceVector

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
observedEffectives



#On commence a calculer les effectif a partir du prix minimum
lambdaEstimator
lambdaEstimator = 8
pexp(1, rate=lambdaEstimator)
theoreticalEffectives = vector()
for(i in 1:(k-1)) {
  print(i)
  print(pexp(minPrice+VInterval*(i-1),rate = lambdaEstimator))  
  print(pexp(minPrice+VInterval*(i),rate = lambdaEstimator))
  
  theoreticalEffectives[i] = (pexp(VInterval*(i),rate = lambdaEstimator) - pexp(VInterval*(i-1),rate = lambdaEstimator))
  print(theoreticalEffectives[i])
}
theoreticalEffectives[k] = 1 - pexp(VInterval*(k-1), rate=lambdaEstimator)
theoreticalEffectives
print(theoreticalEffectives[1])

sum(theoreticalEffectives)

chisq.test(observedEffectives, p=theoreticalEffectives)



khi_deux = 0.
for(i in 1:k) {
  print("====================================")
  print(i)
  print(khi_deux)
  print(theoreticalEffectives[i])
  print((observedEffectives[i]-theoreticalEffectives[i])**2/theoreticalEffectives[i])
  print("====================================")
  

  khi_deux = khi_deux + (observedEffectives[i]-theoreticalEffectives[i])**2/theoreticalEffectives[i]
}
khi_deux


hist(dataFilteredByPrice$ResaleMid)
plot(dataFilteredByPrice$ConvertedManaCost, dataFilteredByPrice$ResaleMid)
