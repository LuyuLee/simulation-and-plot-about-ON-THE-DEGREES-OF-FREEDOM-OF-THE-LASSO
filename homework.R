library(lars)
data(diabetes)
n = length(diabetes$y)
lm = lm(formula = y~ x2, data = diabetes)
y_ = predict(lm, newdata = diabetes$x2)
beta_ols = lm$coefficients
beta_ols[which(is.na(beta_ols))] = 0
# par(mfrow=c(2,2))
attach(diabetes)
lml1 = cv.lars(x2,y,trace=TRUE,max.steps=80,
               index = seq(from = 0, to = 1, length =100))
index = lml1$index
object <- lars(x, y)
objectp = lars(x2, y, trace = T, max.steps = 80)
plot(object, )
plot(object, xvar = "step", breaks = T)
plot(object, xvar = "arc.length", breaks = T)
plot(object, xvar = "df", breaks = F)
object = lars(x2, y)
plot(object, xvar = "df", plottype = "Cp", breaks = T)
# object2 <- lars(x,y,type="lar")
# plot(object2)
# object3 <- lars(x,y,type="for") # Can use abbreviations
# plot(object3)

### make predictions at the values in x, at each of the
### steps produced in object
fits <- predict.lars(objectp, x2, type="fit", s = 20)
### extract the coefficient vector with L1 norm=4.1
coef4.1 <- coef(object, s=4.1, mode="norm") # or
coef4.1 <- predict(object, s=4.1, type="coef", mode="norm")

summary(object)
detach(diabetes)



###########my simulation###########
# install.package("lars")
library(lars)
x2 = diabetes$x2
X = cbind(rep(1, 442), diabetes$x2)
C = X %*% beta_ols
data(diabetes)
n = length(diabetes$y)
lm = lm(formula = y~ x2, data = diabetes)
y_ = predict(lm, newdata = diabetes$x2)
beta_ols = lm$coefficients
sigma_ols = 53.23 # get from lm stand error
# B = 20000
B = 1000
ans = array()
for (b in 1: B)
{
  y = C + rnorm(442, 0, 1) * sigma_ols
  lml1 = cv.lars(x2,y,trace=TRUE,max.steps=80,
                 index = seq(from = 0, to = 1, length =100), plot.it = F)
  step = which(lml1$cv == min(lml1$cv))
  index = lml1$index[step]
  objectp = lars(x2, y, trace = T, max.steps = 80, intercept = T)
  mu = predict.lars(objectp, x2, type="fit")
  # coef(object) don't include the intercept coef
  # a = x2 %*% t(coef(objectp))
  Cm = matrix(C)
  j = 1
  while(j < length(beta_ols))
  {
    j = j + 1
    Cm = cbind(Cm, y)
  }
  cov = matrix(nrow = 442, ncol = ncol(mu$fit))
  for (i in (1: ncol(mu$fit)))
  {
    cov[1 : 442, i] = (mu$fit[,i] - C) * (y - C)
  }
  # cov = (mu$fit- Cm) * (y - C) 
  # ans matrix 442*81
  df = array()
  for (i in (1 : ncol(cov)))
  {
    df[i] = sum(cov[, i]) / (sigma_ols^2)
  }
  id (b == 1)
  {
    ans = df
    next
  }
  ans = ans + df
}
ans = ans / B


fits = predict.lars(objectp, x2, type="fit", s = 20)
e = diabetes$y - fits$fit
sse = sum(e^2) / n
