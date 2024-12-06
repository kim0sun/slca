# Standard LCA
slca(lc[3] ~ y1 + y2 + y3 + y4)
# Latent transition analysis (LTA)
slca(lx[3] ~ x1 + x2 + x3 + x4,
     ly[2] ~ y1 + y2 + y3 + y4,
     lx ~ ly)
# LTA with measurement invariance
slca(l1[3] ~ y11 + y21 + y31 + y41,
     l2[3] ~ y12 + y22 + y32 + y42,
     l1 ~ l2, constraints = c("l1", "l2"))
# Joint latent class analysis
slca(lx[2] ~ x1 + x2 + x3 + x4,
     ly[3] ~ y1 + y2 + y3 + y4,
     lz[2] ~ z1 + z2 + z3 + z4,
     jc[3] ~ lx + ly + lz)
# Latent class profile analysis (with measurement invariance)
slca(l1[3] ~ x1 + x2 + x3 + x4,
     l2[3] ~ y1 + y2 + y3 + y4,
     l3[3] ~ z1 + z2 + z3 + z4,
     pf[4] ~ l1 + l2 + l3,
     constraints = c("l1", "l2", "l3"))
