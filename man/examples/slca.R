# Standard LCA
slca(lc[3] ~ y1 + y2 + y3)
# Latent transition analysis (LTA)
slca(l1[3] ~ y11 + y21 + y31,
     l2[3] ~ y12 + y22 + y32,
     l1 ~ l2)
# LTA with measurement invariance
slca(l1[3] ~ y11 + y21 + y31,
     l2[3] ~ y12 + y22 + y32,
     l1 ~ l2, constraints = c("l1", "l2"))
# Joint latent class analysis
slca(lx[3] ~ x1 + x2 + x3, ly[3] ~ y1 + y2 + y3,
     lz[3] ~ z1 + z2 + z3, jc[3] ~ lx + ly + lz)
# Latent class profile analysis (with measurement invariance)
slca(l1[3] ~ x1 + x2 + x3, l2[3] ~ y1 + y2 + y3,
     l3[3] ~ z1 + z2 + z3, pf[3] ~ lx + ly + lz,
     constraints = c("l1", "l2", "l3"))
