library(magrittr)
set.seed(20261017)
data <- gss7677[gss7677$COHORT == "YOUNG", ]
stat2 <- slca(status(2) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data, control = list(verbose = FALSE))
stat3 <- slca(status(3) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data, control = list(verbose = FALSE))
stat4 <- slca(status(4) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data, control = list(verbose = FALSE))

gof(stat2, stat3, stat4)
gof(stat2, stat3, stat4, test = "chisq")
\donttest{
# Five replicates illustrate the interface only; use more for inference.
absolute_boot <- gof(stat2, stat3, stat4, test = "boot",
                     nboot = 5, maxiter = 5000, tol = 1e-6)
absolute_boot
attr(absolute_boot, "bootFail")
}

compare(stat3, stat4)
compare(stat3, stat4, test = "chisq")
\donttest{
# Use EM refits here; always inspect convergence failures.
relative_boot <- compare(stat3, stat4, test = "boot", method = "em",
                         nboot = 5, maxiter = 5000, tol = 1e-6)
relative_boot
attr(relative_boot, "bootFail")
}
