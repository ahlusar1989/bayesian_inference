set.seed(1989)

# gamma params
a = 2.0
m = 100
b = 1.0 / 3.0

#draw theta from gamma
theta = rgamma(n= m, shape = a, rate = b)

#histogram
hist(theta, freq = FALSE)

#actual pdf
curve(dgamma(x, shape=a, rate= b),  col="blue", add=TRUE)

#mcmc approximation

mean(theta)
# or use this
sum(theta) / m


#variance and standard error

#actual
a/ b^2

# computed
var(theta)


probability_less = theta < 0.5
mean(probability_less)

pgamma(q = 0.5, shape = a, rate = b)

quantile(theta, probs = 0.9)
qgamma(p = 0.9, shape = a, rate = b)


se = sd(theta) / sqrt(m)
2.0 * se # we are reasonably confident that the Monte Carlo estimate is no more than this far from the truth

ind = theta < 5.0
se = sd(ind) / sqrt(m)
2.0 * se # we are




m = 10e4

y = numeric(m) # create the vectors we will fill in with simulations
phi = numeric(m)

for (i in 1:m) {
  phi[i] = rbeta(n=1, shape1=2.0, shape2=2.0)
  y[i] = rbinom(n=1, size=10, prob=phi[i])
}
# which is equivalent to the following 'vectorized' code
phi = rbeta(n=m, shape1=2.0, shape2=2.0)
y = rbinom(n=m, size=10, prob=phi)

mean(y)


plot(prop.table(table(y)), ylab="P(y)", main="Marginal distribution of y")