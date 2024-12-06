m1 <- slca(lc1[3] ~ x1 + x2 + x3 + x4 + x5,
              lc2[4] ~ y1 + y2 + y3 + y4 + y5)
sim <- simulate(m1, 1000)
sapply(sim$class, table)

# simulate data with defined number of levels of manifest items
m2 <- slca(lc1[3] ~ x1 + x2 + x3 + x4)
sim <- simulate(m2, nlevel = c(3, 3, 3, 3))
d <- sim$response
sapply(d, table)

sim <- simulate(m2, nlevel = c(x1 = 2, x3 = 3, x4 = 4, x5 = 5))
d <- sim$response
sapply(d, table)

# simulate data with user-defined parameters
pi <- rep(1 / 3, 3)
rho <- c(.9, .1, .9, .1, .9, .1, .9, .1,
         .9, .1, .9, .1, .1, .9, .1, .9,
         .1, .9, .1, .9, .1, .9, .1, .9)
par <- c(pi, rho)
m3 <- slca(lc[3] ~ y1 + y2 + y3 + y4)
sim <- simulate(m3, parm = par)
mf <- estimate(m3, sim$response)
param(mf)
