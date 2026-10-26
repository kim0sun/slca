# Run against an installed candidate; no package code or saved fits are changed.
# Arguments: library directory, output directory, optional isolated case.
args <- commandArgs(TRUE)
stopifnot(length(args) >= 2L)
.libPaths(c(args[1], .libPaths()))
library(slca)
dir.create(args[2], recursive = TRUE, showWarnings = FALSE)
set.seed(710)
m <- slca(l[2] ~ y1 + y2 + y3 + y4)
d <- simulate(m, nsim = 300, seed = 710,
   parm = c(.6,.4,.8,.2,.7,.3,.9,.1,.65,.35,.2,.8,.3,.7,.15,.85,.25,.75))$response
f <- estimate(m, d)
if (length(args) == 3L) {
   if (args[3] == "refit-new-level") {
      d$y1 <- factor(rep(1:3, 100))
      continued <- estimate(f, d)
      fresh <- estimate(m, d)
      print(list(continued_logLik = logLik(continued),
                 continued_convergence = continued$convergence,
                 missing_parameters = sum(is.na(continued$par)),
                 fresh_logLik = logLik(fresh),
                 fresh_convergence = fresh$convergence))
   } else stop("Unknown isolated case")
   quit(save = "no")
}
results <- list()
check <- function(name, expr) {
   cat("\nCASE:", name, "\n")
   warnings <- character()
   value <- withCallingHandlers(tryCatch(force(expr), error = function(e)
      list(error = conditionMessage(e))), warning = function(w) {
         warnings <<- c(warnings, conditionMessage(w))
         invokeRestart("muffleWarning")
      })
   results[[name]] <<- list(value = value, warnings = warnings)
   print(value)
   if (length(warnings)) print(warnings)
}
x <- data.frame(x = seq_len(nrow(d))/nrow(d), o = rep(c(-1, 1), 150))
check("offset", {
   set.seed(42); a <- regress(f, l ~ x, data = x)
   set.seed(42); b <- regress(f, l ~ x + offset(o), data = x)
   x$y <- as.integer(predict(f)$l == 1)
   ga <- glm(y ~ x, data = x, family = binomial())
   gb <- glm(y ~ x + offset(o), data = x, family = binomial())
   list(slca_difference = max(abs(a$coefficients - b$coefficients)),
        glm_difference = max(abs(coef(ga) - coef(gb))),
        naive_vs_glm = max(abs(drop(a$coefficients) - coef(ga))))
})
check("unknown-control", {
   z <- estimate(m, d, control = list(typo = 2, em.iterlim = 5))
   c(requested = 5, actual = z$control$em.iterlim)
})
check("scaled-covariate", {
   set.seed(42)
   regress(f, l ~ x, data = data.frame(x = x$x * 1e6))$coefficients
})
check("zero-design-column", {
   set.seed(42)
   z <- regress(f, l ~ x + z, data = data.frame(x = x$x, z = 0))
   list(coefficients = z$coefficients, se = z$std.err)
})
check("unidentified-measurement", {
   set.seed(1)
   dat <- data.frame(y1 = factor(rep(c(1,1,2,2), 25)),
                     y2 = factor(rep(c(1,2,1,2), 25)))
   fit <- estimate(slca(l[2] ~ y1 + y2), dat)
   free <- setdiff(seq_along(fit$par), fit$arg$ref_idx)
   list(npar = fit$arg$df,
        rank = qr(crossprod(fit$score[, free, drop = FALSE]))$rank,
        se = sqrt(diag(vcov(fit))))
})
check("legacy-compare", {
   legacy <- f
   attr(legacy$mf, "y") <- attr(legacy$mf, "y") + 1
   attr(legacy$mf, "yu") <- attr(legacy$mf, "yu") + 1
   compare(legacy, f)
})
check("metadata-compare", {
   legacy <- f
   attr(legacy$mf, "saturated.converged") <- NULL
   attr(legacy$mf, "saturated.iterations") <- NULL
   compare(legacy, f)
})
check("legacy-estimate", {
   legacy <- f
   attr(legacy$mf, "y") <- attr(legacy$mf, "y") + 1
   attr(legacy$mf, "yu") <- attr(legacy$mf, "yu") + 1
   estimate(legacy, control = slcaControl(em.iterlim = 2))$convergence
})
check("constraint-refit", {
   set.seed(5)
   constrained <- estimate(m, d, fix2zero = 3)
   continued <- estimate(constrained)
   list(before = c(df = constrained$arg$df, fixed = constrained$fix2zero,
                   probability = exp(constrained$par[3])),
        after = c(df = continued$arg$df, fixed = continued$fix2zero,
                  probability = exp(continued$par[3])),
        AIC_difference = AIC(continued) - AIC(constrained))
})
check("same-levels-refit", {
   z <- estimate(f, d)
   c(logLik_change = as.numeric(logLik(z) - logLik(f)),
     parameter_change = max(abs(exp(z$par) - exp(f$par))))
})
check("identical-model-bootstrap-plot", {
   pdf(file.path(args[2], "identical-model-bootstrap.pdf"))
   tryCatch(compare(f, f, test = "boot", nboot = 2, plot = TRUE),
            finally = dev.off())
})
check("independent-lca-likelihood-and-score", {
   dm <- d
   dm$y2 <- factor(rep(1:3, 100))
   dm$y1[1:20] <- NA
   dm$y3[15:30] <- NA
   set.seed(41)
   fit <- estimate(m, dm)
   par <- utils::relist(exp(fit$par), fit$skeleton$par)
   rho <- par$rho[[1]]
   nv <- lengths(attr(fit$mf, "levels"))
   start <- c(0, head(cumsum(nv), -1))
   joint <- matrix(rep(as.numeric(par$pi[[1]]), each = nrow(dm)), nrow(dm))
   for (j in seq_along(dm)) {
      code <- as.integer(dm[[j]])
      obs <- !is.na(code)
      joint[obs, ] <- joint[obs, ] * rho[start[j] + code[obs], , drop = FALSE]
   }
   manual_ll <- log(rowSums(joint))
   manual_post <- joint / rowSums(joint)
   calc <- getFromNamespace("calcModel", "slca")
   a <- fit$arg
   a$y <- attr(fit$mf, "y"); a$nlev <- unlist(a$nlev); a$ref <- a$ref - 1
   a$par <- fit$par
   a <- a[names(formals(calc))]
   free <- setdiff(seq_along(fit$par), fit$arg$ref_idx)
   numeric_score <- sapply(free, function(j) {
      plus <- minus <- fit$par
      plus[j] <- plus[j] + 1e-5; minus[j] <- minus[j] - 1e-5
      a$par <- unlist(tapply(plus, fit$arg$id, function(z) z - log(sum(exp(z)))))
      lp <- rowSums(do.call(calc, a)$ll)
      a$par <- unlist(tapply(minus, fit$arg$id, function(z) z - log(sum(exp(z)))))
      lm <- rowSums(do.call(calc, a)$ll)
      (lp - lm)/2e-5
   })
   differences <- c(logLik = max(abs(manual_ll - fit$loglikelihood)),
                    posterior = max(abs(manual_post - fit$posterior$marginal$l)),
                    score = max(abs(numeric_score - fit$score[, free])))
   stopifnot(max(differences) < 1e-7)
   differences
})
check("predict-matrix-column-labels", {
   mat <- as.matrix(d[, 4:1])
   colnames(mat) <- paste0("wrong", 1:4)
   c(changed_classes = sum(predict(f, mat)$l != predict(f, d)$l))
})
check("independent-three-step-objectives", {
   p <- f$posterior$marginal$l
   X <- cbind(1, x$x)
   output <- list()
   for (assignment in c("modal", "prop")) {
      W <- if (assignment == "prop") p else
         diag(ncol(p))[max.col(p, ties.method = "first"), ]
      D <- matrix(0, 2, 2)
      for (i in 1:2) for (j in 1:2)
         D[i, j] <- sum(W[, i] * p[, j])/sum(p[, j])
      BW <- W %*% t(solve(D))
      for (method in c("naive", "BCH", "ML")) {
         set.seed(42)
         reg <- regress(f, l ~ x, data = x, imputation = assignment,
                        method = method)
         objective <- function(beta) {
            q <- plogis(drop(X %*% beta))
            Q <- cbind(q, 1 - q)
            if (method == "ML") -sum(W * log(Q %*% t(D)))
            else -sum((if (method == "BCH") BW else W) * log(Q))
         }
         reference <- optim(c(0, 0), objective, method = "BFGS",
                            control = list(reltol = 1e-12, maxit = 1000))
         output[[paste(assignment, method)]] <- c(
            coefficient_difference = max(abs(reg$coefficients - reference$par)),
            objective_difference = abs(-reg$ll - reference$value),
            reference_convergence = reference$convergence)
      }
   }
   do.call(rbind, output)
})
saveRDS(results, file.path(args[2], "edge-results.rds"))
writeLines(capture.output(sessionInfo()), file.path(args[2], "session-info.txt"))
cat("\nAUDIT COMPLETE\n")
