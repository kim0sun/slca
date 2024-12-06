set.seed(0)
pi <- rep(1/3, 3)
rho <- c(.9, .1, .9, .1, .9, .1, .9, .1, # class 1
         .9, .1, .9, .1, .1, .9, .1, .9, # class 2
         .1, .9, .1, .9, .1, .9, .1, .9) # class 3
par1 <- c(pi, rho)
model1 <- slca(lc[3] ~ x1 + x2 + x3 + x4)
sim1 <- simulate(model1, parm = par1)$response

# Latent transition analysis (LTA)
pi <- rep(1/3, 3)
tau <- c(.9, .1, .5, .5, .1, .9)
rho <- c(.9, .1, .9, .1, .9, .1, .9, .1,
         .9, .1, .9, .1, .1, .9, .1, .9,
         .1, .9, .1, .9, .1, .9, .1, .9, # lx
         .9, .1, .9, .1, .9, .1, .9, .1,
         .1, .9, .1, .9, .1, .9, .1, .9) # ly
par2 <- c(pi, tau, rho)
model2 <- slca(lx[3] ~ x1 + x2 + x3 + x4,
               ly[2] ~ y1 + y2 + y3 + y4,
               lx ~ ly)
sim2 <- simulate(model2, parm = par2)$response

# LTA with measurement invariance
pi <- rep(1/3, 3)
tau <- c(.8, .1, .1, .2, .6, .2, .1, .1, .8)
rho <- c(.9, .1, .9, .1, .9, .1, .9, .1,
         .9, .1, .9, .1, .1, .9, .1, .9,
         .1, .9, .1, .9, .1, .9, .1, .9)
par3 <- c(pi, tau, rho)
model3 <- slca(l1[3] ~ y11 + y21 + y31 + y41,
               l2[3] ~ y12 + y22 + y32 + y42,
               l1 ~ l2, constraints = c("l1", "l2"))
sim3 <- simulate(model3, parm = par3)$response

# Joint latent class analysis
pi <- rep(1/3, 3)
tau <- c(.9, .1, .9, .1, .1, .9, # lx
         .8, .1, .1, .1, .8, .1, .1, .1, .8, # ly
         .4, .4, .1, .1, .1, .4, .4, .1, 0, .1, .1, .8) # lz
rho <- c(.9, .1, .9, .1, .9, .1, .9, .1,
         .1, .9, .1, .9, .1, .9, .1, .9, # lx
         .9, .1, .9, .1, .9, .1, .9, .1,
         .9, .1, .9, .1, .1, .9, .1, .9,
         .1, .9, .1, .9, .1, .9, .1, .9, # ly
         .9, .1, .9, .1, .9, .1, .9, .1,
         .9, .1, .9, .1, .1, .9, .1, .9,
         .1, .9, .1, .9, .9, .1, .9, .1,
         .1, .9, .1, .9, .1, .9, .1, .9) # lz
par4 <- c(pi, tau, rho)
model4 <- slca(lx[2] ~ x1 + x2 + x3 + x4,
               ly[3] ~ y1 + y2 + y3 + y4,
               lz[4] ~ z1 + z2 + z3 + z4,
               jc[3] ~ lx + ly + lz)
sim4 <- simulate(model4, parm = par4)$response

# Latent class profile analysis (with measurement invariance)
pi <- rep(1/4, 4)
tau <- c(.8, .1, .1, .1, .8, .1, .1, .1, .8, .8, .1, .1, # time 1 (l1)
         .8, .1, .1, .1, .8, .1, .1, .1, .8, .1, .8, .1, # time 2 (l2)
         .8, .1, .1, .1, .8, .1, .1, .1, .8, .1, .1, .8) # time 3 (l3)
rho <- c(.9, .1, .9, .1, .9, .1, .9, .1,
         .9, .1, .9, .1, .1, .9, .1, .9,
         .1, .9, .1, .9, .1, .9, .1, .9)
par5 <- c(pi, tau, rho)
model5 <- slca(l1[3] ~ x1 + x2 + x3 + x4,
               l2[3] ~ y1 + y2 + y3 + y4,
               l3[3] ~ z1 + z2 + z3 + z4,
               pf[4] ~ l1 + l2 + l3,
               constraints = c("l1", "l2", "l3"))
sim5 <- simulate(model5, parm = par5)$response
