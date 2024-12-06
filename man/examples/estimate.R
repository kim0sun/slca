m <- slca(lc[3] ~ y1 + y2 + y3 + y4)
pi <- rep(1 / 3, 3)
rho <- c(.9, .1, .9, .1, .9, .1, .9, .1, # class 1
         .9, .1, .9, .1, .1, .9, .1, .9, # class 2
         .1, .9, .1, .9, .1, .9, .1, .9) # class 3
dt <- simulate(m, parm = c(pi, rho))
estimate(m, dt$response)

# Several estimation methods
estimate(m, dt$response, method = "em",
         control = slcaControl(verbose = TRUE)) # default
estimate(m, dt$response, method = "nlm",
         control = slcaControl(verbose = TRUE))
estimate(m, dt$response, method = "hybrid",
         control = slcaControl(verbose = TRUE))

# Parameter restriction
mf <- estimate(m, dt$response)
param(mf, index = TRUE)
mf0 <- estimate(mf, fix2zero = c(4, 6, 8, 10))
param(mf0)

# Estimation control
estimate(m, dt$response, control = slcaControl(nrep = 10, verbose = TRUE))
estimate(m, dt$response, control = slcaControl(init.param = c(pi, rho)))
