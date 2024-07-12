#' Plot \code{slca} Object
#'
#'
#'
#' @exportS3Method base::plot slca
plot.slca <- function(
   x, dir = "TD", font = "Helvetica", ...
) {
   msr <- x$model$measure
   str <- x$model$struct
   pa <- c(as.character(str$parent), row.names(msr))
   ch <- c(as.character(str$child),
           unname(sapply(msr$indicator, paste0, collapse = "', '")))
   pa <- ifelse(
      is.na(msr[pa, "constraint"]),
      pa, paste0(pa, " (", msr[pa, "constraint"], ")")
   )
   ch <- ifelse(
      is.na(msr[ch, "constraint"]),
      ch, paste0(ch, " (", msr[ch, "constraint"], ")")
   )
   lb <- c(str$constraint, rep(NA, nrow(msr)))

   node <- paste0(
      "node [shape = box]\n",
      paste0(paste0("'", setdiff(ch, pa), "'", collapse = ", ")),
      "\n\n node [shape = oval]\n",
      paste0(paste0("'", pa, "'"), collapse = ", ")
   )
   path <- paste0("'", pa, "' -> { '", ch, "' }",
                  ifelse(is.na(lb), "", paste0(" [label = '", lb, " 'fontname = '", font, "']")))

   text <- paste0(
      "digraph { \n  rankdir = '", dir, "';",
      "node[fontname = '", font, "']\n\n",
      node, "\n\n", paste0(path, collapse = "\n"), "\n}"
   )

   # if (inherits(x, "estimated")) {
   #    par <- slca::param(x)
   #    npar <- c(x$arg$npi, x$arg$ntau, x$arg$nrho)
   #    p <- menu(names(par)[npar > 0], title = "Select parameter type")
   #    if (p == 0) invisible()
   #    if (names(par)[npar > 0][p] == "pi") {
   #       v <- menu(names(par$pi), title = "Which latent variable ?")
   #       if (v == 0) invisible()
   #       else sp <- par$pi[[v]][1,]
   #       barplot(sp, col = gray.colors(length(sp)),
   #               main = "Latent class prevalence",
   #               ylab = "Prevalence", xlab = "Class")
   #    } else if (names(par)[npar > 0][p] == "tau") {
   #       vn <- split(apply(x$model$struct[,1:3], 1, paste0, collapse = " "),
   #                   x$model$struct$constraint)
   #       sn <- paste0(names(vn), " (", sapply(vn, paste, collapse = ", "), ")")
   #       v <- menu(sn, title = "Which tau parameter ?")
   #       if (v > 0) {
   #          sp <- par$tau[[v]]
   #          cols <- gray.colors(nrow(sp), start = 0.1, end = 0.9)
   #          barplot(sp[nrow(sp):1,], xlab = "Parent", ylab = "",
   #                  col = cols, main = "Transition probabilities")
   #          legend("topright", fill = rev(cols),
   #                 title = "child", legend = seq_len(nrow(sp)))
   #       }
   #    } else if (names(par)[npar > 0][p] == "rho") {
   #       vn <- split(rownames(x$model$measure), x$model$measure$constraint)
   #       sn <- paste0(names(vn), " (", sapply(vn, paste, collapse = ", "), ")")
   #       v <- menu(sn, title = "Which rho parameter ?")
   #       if (v > 0) {
   #          sp <- par$rho[[v]]
   #          nlev <- x$arg$nlev[[v]]
   #          cols <- unlist(sapply(rev(nlev), gray.colors, start = 0.1, end = 0.9))
   #          barplot(sp[nrow(sp):1,], xlab = "Class", ylab = "", col = cols,
   #                  main = "Item response probabilities", axes = FALSE)
   #          axis(2, at = 1:length(nlev) - 0.5, labels = paste0("V", length(nlev):1),
   #               las = 1, tick = FALSE, hadj = 0)
   #       }
   #    }
   # }
   DiagrammeR::grViz(text, ...)
}


