library("car");
data("Leinhardt")
?Leinhardt
head(Leinhardt)


plot(infant ~ income, data = Leinhardt)

hist(Leinhardt$infant)
hist(Leinhardt$income)


Leinhardt$log_infant = log(Leinhardt$infant)

Leinhardt$log_income = log(Leinhardt$income)

pairs(Anscombe)  # scatter plots for each pair of variables


plot(Leinhardt$log_infant ~ Leinhardt$log_income, data = Leinhardt)


### Modeling

## baseline model
lmod = lm(Leinhardt$log_infant ~ Leinhardt$log_income, data = Leinhardt)


dat = na.omit(Leinhardt)

summary(lmod)

# first model


library("rjags")

mod1_string = " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec) ## precision of normal
        mu[i] = b[1] + b[2]*log_income[i] 
    }
    ## two coefficients
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(y=dat$log_infant, n=nrow(dat), 
                  log_income=dat$log_income)

params1 = c("b", "sig")

inits1 = function() {
  inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)



update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains


### Diagnostics

plot(mod1_sim)


gelman.diag(mod1_sim)


autocorr.diag(mod1_sim)

effectiveSize(mod1_sim)

summary(mod1_sim)
