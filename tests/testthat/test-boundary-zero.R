test_that("zero subtree messages agree with exhaustive latent-state enumeration", {
   model <- slca(a[2] ~ x1 + x2, b[2] ~ y1 + y2, c[2] ~ z1 + z2,
                 s[2] ~ a + b, r[2] ~ s + c)
   dat <- data.frame(x1 = c(1, 2, 1, 2, 1, 2, NA, 2),
                     x2 = c(2, 1, NA, 2, 1, 2, 1, NA),
                     y1 = c(1, 2, 2, 1, NA, 1, 2, 1),
                     y2 = c(2, 1, 1, 2, 2, 1, 1, 2),
                     z1 = c(1, 2, 1, 2, 1, NA, 2, 1),
                     z2 = c(2, 1, 2, 1, 2, 1, 1, 2))
   mf <- proc_data(dat, model$model, FALSE)
   a <- arg_mf(model$model, model$arg, mf, NULL)
   skeleton <- get_frame(model$model, a, mf)
   p <- skeleton$par
   p$pi[[1]][] <- c(.6, .4)
   for (j in seq_along(p$tau)) p$tau[[j]][] <- c(.7, .3, .2, .8)
   nodes <- row.names(model$model$latent)
   for (d in seq_len(a$nrl))
      if (nodes[a$ul[d] + 1] %in% c("a", "s"))
         p$tau[[a$eqrl[d] + 1]][] <- diag(2)
   for (j in seq_along(p$rho))
      p$rho[[j]][] <- c(.8, .2, .7, .3, .3, .7, .2, .8)
   leaf <- match(match("a", nodes) - 1, a$lf)
   p$rho[[a$eqlf[leaf] + 1]][1:2, ] <- diag(2)
   states <- as.matrix(expand.grid(rep(list(1:2), a$nlv)))
   root <- a$rt + 1
   a$y <- attr(mf, "y")
   a$nlev <- unlist(a$nlev)
   a$ref <- a$ref - 1

   for (epsilon in c(0, 1e-6)) {
      par <- unlist(p, use.names = FALSE)
      par[par == 0] <- epsilon
      par <- par / ave(par, a$id, FUN = sum)
      a$par <- log(par)
      prob <- utils::relist(par, skeleton$par)
      actual <- do.call(calcModel, a[names(formals(calcModel))])
      weight <- matrix(prob$pi[[1]][states[, root]], nrow(states), nrow(dat))
      for (d in seq_len(a$nrl)) {
         ix <- cbind(states[, a$ul[d] + 1], states[, a$vl[d] + 1])
         weight <- weight * prob$tau[[a$eqrl[d] + 1]][ix]
      }
      for (v in seq_len(a$nlf)) {
         items <- model$model$measure$indicator[[v]]
         rho <- prob$rho[[a$eqlf[v] + 1]]
         for (i in seq_len(nrow(dat))) {
            for (m in seq_along(items)) {
               code <- mf[[items[m]]][i]
               if (!is.na(code))
                  weight[, i] <- weight[, i] *
                     rho[(m - 1) * 2 + code, states[, a$lf[v] + 1]]
            }
         }
      }
      likelihood <- colSums(weight)
      post <- sweep(weight, 2, likelihood, "/")
      reference_post <- lapply(seq_len(a$nlv), function(v)
         rbind(colSums(post[states[, v] == 1, , drop = FALSE]),
               colSums(post[states[, v] == 2, , drop = FALSE])))
      reference_joint <- lapply(seq_len(a$nrl), function(d) {
         out <- array(0, c(2, 2, nrow(dat)))
         for (k in 1:2) for (l in 1:2)
            out[k, l, ] <- colSums(post[states[, a$ul[d] + 1] == k &
               states[, a$vl[d] + 1] == l, , drop = FALSE])
         out
      })
      expect_true(all(is.finite(actual$ll)))
      expect_equal(as.numeric(actual$ll), log(likelihood), tolerance = 1e-12)
      expect_equal(exp(actual$post), unlist(reference_post, use.names = FALSE),
                   tolerance = 1e-12)
      expect_equal(exp(actual$joint), unlist(reference_joint, use.names = FALSE),
                   tolerance = 1e-12)
   }
})

test_that("simultaneous NLSY zero constraints no longer corrupt EM posteriors", {
   original <- nlsy_jlcpa
   fixed <- which(exp(original$par) < 1e-8)
   expect_length(fixed, 41)
   fit <- estimate(original, fix2zero = fixed)
   expect_true(isTRUE(fit$convergence[["EM"]]))
   expect_true(is.finite(as.numeric(logLik(fit))))
   expect_false(anyNA(fit$par))
   expect_true(all(is.finite(unlist(fit$posterior))))
   expect_equal(exp(fit$par[fixed]), rep(0, length(fixed)))
   expect_lt(abs(as.numeric(logLik(fit) - logLik(original))), 1e-6)
   expect_lt(max(abs(exp(fit$par) - exp(original$par))), 1e-4)
   expect_equal(predict(fit), predict(original))
   expect_identical(nlsy_jlcpa, original)
})
