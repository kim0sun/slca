rep_row <- function(x, lev) {
   sb <- as.matrix(expand.grid(lev[is.na(x)]))
   m <- t(replicate(nrow(sb), x))
   m[, is.na(x)] <- sb
   m
}

match_row <- function(x, y) {
   nna <- !is.na(x)
   which(!colSums(t(y[,nna, drop = FALSE]) != x[nna]))
}

expand_row <- function(x, y) {
   nna <- !is.na(x)
   nna
}

count_row <- function(mf, nlev, na.rm) {
   y_sort <- mf[do.call(order, mf), ]
   if (!sum(is.na(mf))) na.rm = TRUE
   if (na.rm) {
      uniq <- !duplicated(y_sort)
      y_uniq <- y_sort[uniq,]
      freq <- tabulate(cumsum(uniq))
      theta <- freq / sum(freq)
      loglik <- sum(freq * log(theta))
      row.names(y_uniq) <- NULL
      return(list(y_unique = y_uniq, freq = freq,
                  theta = theta, loglik = loglik,
                  converged = TRUE, iterations = 0L))
   } else {
      miss <- rowSums(is.na(y_sort)) >  0
      yobs <- y_sort[!miss, ]
      uobs <- !duplicated(yobs)
      obs_uniq <- yobs[uobs, ]
      obs_freq <- tabulate(cumsum(uobs))
      ymis <- y_sort[miss, ]
      umis <- !duplicated(ymis)
      mis_uniq <- ymis[umis, ]
      large <- prod(nlev) * length(nlev) >= 1e6
      if (prod(nlev) * length(nlev) < 1e6) {
         fmat <- as.matrix(expand.grid(lapply(nlev, seq_len)))
         obs_uniq <- unique(rbind(obs_uniq, fmat))
      } else {
         # Only cells compatible with an observed pattern can have positive mass.
         sizes <- apply(is.na(mis_uniq), 1, function(x) prod(nlev[x]))
         if ((sum(sizes) + nrow(obs_uniq)) * length(nlev) > 1e7) {
            warning("Saturated response table is too large; absolute fit statistics are unavailable.",
                    call. = FALSE)
            if (!nrow(obs_uniq)) {
               obs_uniq <- mis_uniq[1, , drop = FALSE]
               obs_uniq[is.na(obs_uniq)] <- 1
            }
            return(list(y_unique = obs_uniq,
                        freq = rep(NA_real_, nrow(obs_uniq)),
                        theta = rep(NA_real_, nrow(obs_uniq)), loglik = NA_real_,
                        converged = FALSE, iterations = 0L))
         }
         lev <- lapply(nlev, seq_len)
         cells <- lapply(seq_len(nrow(mis_uniq)), function(i)
            rep_row(unlist(mis_uniq[i, ], use.names = FALSE), lev))
         cells <- do.call(rbind, cells)
         colnames(cells) <- names(obs_uniq)
         obs_uniq <- unique(rbind(obs_uniq, cells))
      }
      obs_freq <- c(obs_freq, rep(0, nrow(obs_uniq) - length(obs_freq)))
      ncell <- length(obs_freq)
      row.names(obs_uniq) <- NULL

      misobs <- lapply(seq_len(nrow(mis_uniq)), function(i)
         match_row(mis_uniq[i, ], obs_uniq))
      rmis <- sapply(misobs, length)
      nmis <- length(misobs)
      imis <- unlist(misobs) - 1
      fmis <- tabulate(cumsum(umis))

      calc_mis <- calcfreq(
         imis, rmis, nmis, fmis, obs_freq, ncell,
         nrow(mf), if (large) 1e-10 else 1e-6, if (large) 20000 else 500
      )
      if (!isTRUE(calc_mis$converged)) {
         warning("Saturated response table did not converge; absolute fit statistics are unavailable.",
                 call. = FALSE)
         calc_mis$freq[] <- NA_real_
         calc_mis$theta[] <- NA_real_
         calc_mis$loglik <- NA_real_
      }
      return(c(list(y_unique = obs_uniq), calc_mis))
   }
}

encode_response <- function(mf, child) {
   mf[] <- lapply(mf, as.numeric)
   mf[is.na(mf)] <- 0
   unlist(lapply(child, function(x) t(mf[x])), use.names = FALSE)
}

proc_data <- function(data, model, na.rm) {
   child <- model$measure$indicator
   f <- paste("~", paste(unlist(child), collapse = "+"))
   if (na.rm)
      mf <- stats::model.frame(stats::formula(f), data)
   else
      mf <- stats::model.frame(stats::formula(f), data, na.action = NULL)

   mf <- mf[rowSums(!is.na(mf)) > 0, , drop = FALSE]
   if (!nrow(mf))
      stop("no observations with observed manifest variables.", call. = FALSE)

   mf[] <- lapply(mf, function(x)
      if (is.factor(x)) x else factor(x))
   lev <- lapply(mf, levels)
   nlev <- sapply(mf, nlevels)
   mf[] <- lapply(mf, as.numeric)
   if (any(nlev < 2))
      stop("wrong indicator variable(s) (< 2 level):\n",
           paste0("`", unlist(child)[nlev < 2], "`", collapse = " "))

   res <- count_row(mf, nlev, na.rm)

   attr(mf, "y") <- encode_response(mf, child)
   attr(mf, "levels") <- lev
   attr(mf, "y_unique") <- res$y_unique
   attr(mf, "yu") <- encode_response(res$y_unique, child)
   attr(mf, "freq") <- res$freq
   attr(mf, "theta") <- res$theta
   attr(mf, "loglik") <- res$loglik
   attr(mf, "saturated.converged") <- res$converged
   attr(mf, "saturated.iterations") <- res$iterations
   attr(mf, "df") <- prod(nlev) - 1

   mf
}


proc_data2 <- function(data, model, na.rm) {
   child <- model$measure$indicator
   f <- paste("~", paste(unlist(child), collapse = "+"))
   if (na.rm)
      mf <- stats::model.frame(stats::formula(f), data)
   else
      mf <- stats::model.frame(stats::formula(f), data, na.action = NULL)

   mf[] <- lapply(mf, function(x)
      if (is.factor(x)) x else factor(x, levels = c()))
   lev <- lapply(mf, levels)
   nlev <- sapply(mf, nlevels)
   mf[] <- lapply(mf, as.numeric)
   attr(mf, "y") <- encode_response(mf, child)
   attr(mf, "levels") <- lev
   mf
}
