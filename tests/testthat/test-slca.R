# ============================================================
# Shared fixtures (simulated once, reused across test blocks)
# ============================================================

# Simple 3-class LCA with 4 binary indicators
m_lca <- slca(lc[3] ~ y1 + y2 + y3 + y4)

pi_true  <- c(0.4, 0.35, 0.25)
rho_true <- c(
   .9, .1, .9, .1, .9, .1, .9, .1,
   .5, .5, .5, .5, .5, .5, .5, .5,
   .1, .9, .1, .9, .1, .9, .1, .9
)
sim_lca <- simulate(m_lca, nsim = 300, seed = 42, parm = c(pi_true, rho_true))

ctrl_fast <- slcaControl(em.iterlim = 500, nrep = 1)
fit_lca   <- estimate(m_lca, sim_lca$response, control = ctrl_fast)


# ============================================================
# slca() — model construction
# ============================================================

# [] and () notation must produce identical model objects
test_that("parenthesis/bracket identical", {
   expect_equal(slca(l[3] ~ a + b + c), slca(l(3) ~ a + b + c))
})

# Formula without a class count should raise an error
test_that("stop without number works", {
   expect_error(slca(l ~ a + b + c))
})

# A manifest variable with two parents is not allowed
test_that("stop if there are multiple parents", {
   expect_error(slca(l[3] ~ a + b + c, l2[3] ~ a + b + c))
})

# LTA with T time points must produce T latent nodes and (T-1) structural edges
test_that("LTA structure: T time points -> T latent nodes, T-1 edges", {
   # 2-timepoint LTA
   m2 <- slca(l1[3] ~ x1 + x2 + x3,
              l2[3] ~ y1 + y2 + y3,
              l1 ~ l2)
   expect_equal(nrow(m2$model$latent), 2)
   expect_equal(nrow(m2$model$struct), 1)
   expect_true(any(m2$model$latent$root))

   # 3-timepoint LTA
   m3 <- slca(l1[3] ~ x1 + x2 + x3,
              l2[3] ~ y1 + y2 + y3,
              l3[3] ~ z1 + z2 + z3,
              l1 ~ l2, l2 ~ l3)
   expect_equal(nrow(m3$model$latent), 3)
   expect_equal(nrow(m3$model$struct), 2)
   expect_true(any(m3$model$latent$root))
})

# JLCA with K sub-latent variables must produce (K+1) latent nodes and K edges
test_that("JLCA structure: K sub-variables -> K+1 latent nodes, K edges", {
   # K = 2
   m2 <- slca(lx[2] ~ x1 + x2,
              ly[2] ~ y1 + y2,
              jc[3] ~ lx + ly)
   expect_equal(nrow(m2$model$latent), 3)
   expect_equal(nrow(m2$model$struct), 2)

   # K = 3
   m3 <- slca(lx[2] ~ x1 + x2,
              ly[2] ~ y1 + y2,
              lz[2] ~ z1 + z2,
              jc[3] ~ lx + ly + lz)
   expect_equal(nrow(m3$model$latent), 4)
   expect_equal(nrow(m3$model$struct), 3)
})

# Measurement invariance constraint must assign the same constraint label to both variables
test_that("measurement invariance constraint produces equal constraint labels", {
   m <- slca(l1[3] ~ x1 + x2 + x3,
             l2[3] ~ y1 + y2 + y3,
             l1 ~ l2,
             constraints = c("l1", "l2"))
   expect_equal(
      m$model$measure$constraint[1],
      m$model$measure$constraint[2]
   )
})


# ============================================================
# simulate.slca()
# ============================================================

# Simulated data dimensions must match nsim and the model specification
test_that("simulate produces data with the requested number of rows and columns", {
   sim <- simulate(m_lca, nsim = 150, seed = 2)
   expect_equal(nrow(sim$class),    150)
   expect_equal(nrow(sim$response), 150)
   expect_equal(ncol(sim$class),      1)  # one latent variable
   expect_equal(ncol(sim$response),   4)  # four manifest items
})

# The same seed must always produce identical output
test_that("simulate is reproducible with the same seed", {
   sim1 <- simulate(m_lca, nsim = 100, seed = 99)
   sim2 <- simulate(m_lca, nsim = 100, seed = 99)
   expect_equal(sim1$class,    sim2$class)
   expect_equal(sim1$response, sim2$response)
})

# When user-supplied parameters are provided, parm in the return value should exist
test_that("simulate with user parm returns a parameter list", {
   sim <- simulate(m_lca, nsim = 100, seed = 5, parm = c(pi_true, rho_true))
   expect_type(sim$parm, "list")
   expect_named(sim$parm, c("pi", "tau", "rho"))
})


# ============================================================
# estimate.slca()
# ============================================================

# logLik must return a logLik object with a finite value
test_that("logLik returns a finite value", {
   ll <- logLik(fit_lca)
   expect_s3_class(ll, "logLik")
   expect_true(is.finite(as.numeric(ll)))
})

# MLE must achieve a log-likelihood no lower than the true simulation parameters
test_that("MLE log-likelihood is at least as large as the true-parameter log-likelihood", {
   expect_true(as.numeric(logLik(fit_lca)) >= sim_lca$llik)
})

# estimation with method = "nlm" must return a valid slcafit object
test_that("estimate with method = 'nlm' works", {
   fit_nlm <- estimate(m_lca, sim_lca$response,
                       method  = "nlm",
                       control = slcaControl(nlm.iterlim = 200))
   expect_s3_class(fit_nlm, "slcafit")
   expect_true(is.finite(as.numeric(logLik(fit_nlm))))
})

# estimation with method = "hybrid" must return a valid slcafit object
test_that("estimate with method = 'hybrid' works", {
   fit_hyb <- estimate(m_lca, sim_lca$response,
                       method  = "hybrid",
                       control = slcaControl(em.iterlim = 100, nlm.iterlim = 50))
   expect_s3_class(fit_hyb, "slcafit")
   expect_true(is.finite(as.numeric(logLik(fit_hyb))))
})

# Parameters fixed to zero must have exp(par) == 0
test_that("fix2zero sets the specified parameters to zero probability", {
   fit0 <- estimate(fit_lca, fix2zero = c(4, 6))
   expect_equal(exp(fit0$par[c(4, 6)]), c(0, 0))
})

# Fixing parameters to zero must reduce the log-likelihood relative to the unconstrained model
test_that("fix2zero produces a lower log-likelihood than the unconstrained model", {
   fit0 <- estimate(fit_lca, fix2zero = c(4, 6))
   expect_true(as.numeric(logLik(fit0)) < as.numeric(logLik(fit_lca)))
})


# ============================================================
# param.slcafit()
# ============================================================

# PI probabilities across all classes must sum to 1
test_that("param: pi probabilities sum to 1", {
   p <- param(fit_lca)
   for (pi_mat in p$pi) {
      expect_equal(sum(pi_mat), 1, tolerance = 1e-6)
   }
})

# All RHO values must be valid probabilities in [0, 1]
test_that("param: rho values are in [0, 1]", {
   p <- param(fit_lca)
   for (rho_mat in p$rho) {
      expect_true(all(rho_mat >= 0 & rho_mat <= 1))
   }
})

# With se = TRUE, all returned standard errors must be non-negative
test_that("param with se = TRUE returns non-negative standard errors", {
   p_se <- param(fit_lca, se = TRUE)
   vals <- unlist(p_se)
   expect_true(all(vals[!is.nan(vals)] >= 0))
})


# ============================================================
# predict.slcafit()
# ============================================================

# type = "posterior" must return a list of probability matrices that row-sum to 1
test_that("predict with type = 'posterior' returns row-stochastic matrices", {
   post <- predict(fit_lca, type = "posterior")
   expect_type(post, "list")
   for (mat in post) {
      expect_true(all(mat >= 0 & mat <= 1))
      expect_equal(rowSums(mat), rep(1, nrow(mat)), tolerance = 1e-6, ignore_attr = TRUE)
   }
})

# Passing newdata must return a data.frame with the same number of rows (tests levels() fix)
test_that("predict with newdata returns correct dimensions", {
   newdata <- sim_lca$response[1:20, ]
   pred <- predict(fit_lca, newdata = newdata)
   expect_s3_class(pred, "data.frame")
   expect_equal(nrow(pred), 20)
})


# ============================================================
# vcov.slcafit() and confint.slcafit()
# ============================================================

# Lower bounds must not exceed upper bounds
test_that("confint lower bound <= upper bound for all parameters", {
   ci <- confint(fit_lca)
   expect_true(is.matrix(ci))
   expect_equal(ncol(ci), 2)
   expect_true(all(ci[, 1] <= ci[, 2]))
})


# ============================================================
# reorder.slcafit()
# ============================================================

# Reordering must not change the log-likelihood
test_that("reorder preserves log-likelihood", {
   reord <- reorder(fit_lca, lc = c(3, 1, 2))
   expect_equal(
      as.numeric(logLik(reord)),
      as.numeric(logLik(fit_lca)),
      tolerance = 1e-6
   )
})

# Reordering changes the order of pi values
test_that("reorder changes parameter ordering", {
   p_orig  <- as.numeric(param(fit_lca)$pi[[1]])
   reord   <- reorder(fit_lca, lc = c(3, 1, 2))
   p_reord <- as.numeric(param(reord)$pi[[1]])
   expect_equal(p_reord, p_orig[c(3, 1, 2)], tolerance = 1e-8)
})

# Reordering a model with fix2zero must still work correctly (tests arg$fix2zero bug)
test_that("reorder with fix2zero preserves log-likelihood", {
   fit0  <- estimate(fit_lca, fix2zero = c(4, 6))
   reord <- reorder(fit0, lc = c(2, 3, 1))
   expect_s3_class(reord, "slcafit")
   expect_equal(
      as.numeric(logLik(reord)),
      as.numeric(logLik(fit0)),
      tolerance = 1e-6
   )
})


# ============================================================
# gof() and compare()
# ============================================================

# gof with multiple models must have one row per model
test_that("gof with multiple models has one row per model", {
   fit2 <- estimate(slca(lc[2] ~ y1 + y2 + y3 + y4), sim_lca$response,
                    control = slcaControl(em.iterlim = 200))
   g <- gof(fit2, fit_lca)
   expect_equal(nrow(g), 2)
})

# The model with more parameters must have a higher log-likelihood
test_that("compare: model with more parameters has higher log-likelihood", {
   fit2 <- estimate(slca(lc[2] ~ y1 + y2 + y3 + y4), sim_lca$response,
                    control = slcaControl(em.iterlim = 200))
   cmp <- compare(fit2, fit_lca)
   expect_true(cmp$logLik[2] >= cmp$logLik[1])
})


# ============================================================
# LTA with measurement invariance
# ============================================================

# Measurement invariance reduces the number of rho parameter groups
test_that("LTA invariance constraint reduces rho groups and improves parsimony", {
   m_lta_free <- slca(l1[2] ~ x1 + x2 + x3,
                      l2[2] ~ y1 + y2 + y3,
                      l1 ~ l2)
   m_lta_inv  <- slca(l1[2] ~ x1 + x2 + x3,
                      l2[2] ~ y1 + y2 + y3,
                      l1 ~ l2,
                      constraints = c("l1", "l2"))
   sim_lta <- simulate(m_lta_free, nsim = 200, seed = 7)
   fit_free <- estimate(m_lta_free, sim_lta$response,
                        control = slcaControl(em.iterlim = 200))
   fit_inv  <- estimate(m_lta_inv,  sim_lta$response,
                        control = slcaControl(em.iterlim = 200))

   # Invariant model shares one rho block; free model has two
   expect_equal(length(param(fit_inv)$rho),  1)
   expect_equal(length(param(fit_free)$rho), 2)
   # Constrained model must have fewer parameters (lower df usage)
   expect_true(attr(logLik(fit_inv), "df") < attr(logLik(fit_free), "df"))
})


# ============================================================
# regress.slcafit() and confint.reg.slca()
# ============================================================

# regress coefficients matrix must have one row per non-reference class
test_that("regress coefficients matrix has correct dimensions", {
   reg <- regress(nlsy_jlcpa, SMK_98 ~ SEX, nlsy97)
   nclass <- nlevels(factor(predict(nlsy_jlcpa)[[1]]))
   expect_equal(nrow(reg$coefficients), nclass - 1)
})

# confint on reg.slca must return a list of CI matrices (tests ci[, parm] fix)
test_that("confint.reg.slca returns a list of 2-column CI matrices", {
   reg <- regress(nlsy_jlcpa, SMK_98 ~ SEX, nlsy97)
   ci  <- confint(reg)
   expect_type(ci, "list")
   for (mat in ci) {
      expect_equal(ncol(mat), 2)
      expect_true(all(mat[, 1] <= mat[, 2]))
   }
})

# parm subset in confint.reg.slca must restrict to the specified columns
test_that("confint.reg.slca with parm subset restricts to selected predictors", {
   reg <- regress(nlsy_jlcpa, SMK_98 ~ SEX, nlsy97)
   ci  <- confint(reg, parm = 1)
   expect_type(ci, "list")
   for (mat in ci) {
      expect_equal(nrow(mat), 1)
   }
})
