#' Plot \code{slca} Object
#'
#'
#'
#' @exportS3Method base::plot slca
plot.slca <- function(
   x, abbreviation = FALSE, dir = "TD",
   equal_rank = NULL, font = "Helvetica", ...
) {
   latent <- x$model$latent
   tree <- x$model$tree

   latent$child <- NA
   if (abbreviation)
      latent$children[latent$leaf] <-
         sapply(latent$children[latent$leaf], function(x)
            paste0("'", x[1], " ~ ", x[length(x)], "'"))

   parent <- row.names(latent)
   child <- sapply(latent$children, paste, collapse = ", ")

   node <- paste0(
      "node [shape = box]\n",
      paste(setdiff(unlist(latent$children), parent), collapse = ", "),
      "\n\n node [shape = oval]\n",
      paste(names(child), collapse = ", ")
   )
   path <- paste(paste(names(child), " -> {", child, "}"), collapse = "\n")
   if (!missing(equal_rank)) {
      equal_rank <- equal_rank[equal_rank %in% row.names(latent)]
      ranks <- paste0(
         "{rank = same; '", paste0(equal_rank, collapse = "';'"), "';}"
      )
   }

   text <- paste0(
      "digraph { \n  rankdir = '", dir, "';",
      "node[fontname = '", font, "']\n\n",
      node, "\n\n", path, "\n\n",
      if (!missing(equal_rank)) ranks, "\n}"
   )

   if (inherits(x, "estimated")) {
      par <- slca::param(x)
      npar <- c(x$arg$npi, x$arg$ntau, x$arg$nrho)
      p <- menu(names(par)[npar > 0], title = "Select parameter type")
      if (p == 0) invisible()
      if (names(par)[npar > 0][p] == "pi") {
         v <- menu(names(par$pi), title = "Which latent variable ?")
         if (v == 0) invisible()
         else sp <- par$pi[[v]][1,]
         barplot(sp, col = gray.colors(length(sp)),
                 main = "Latent class prevalence",
                 ylab = "Prevalence", xlab = "Class")
      } else if (names(par)[npar > 0][p] == "tau") {
         vn <- split(apply(x$model$struct[,1:3], 1, paste0, collapse = " "),
                     x$model$struct$constraint)
         sn <- paste0(names(vn), " (", sapply(vn, paste, collapse = ", "), ")")
         v <- menu(sn, title = "Which tau parameter ?")
         if (v > 0) {
            sp <- par$tau[[v]]
            cols <- gray.colors(nrow(sp), start = 0.1, end = 0.9)
            barplot(sp[nrow(sp):1,], xlab = "Parent", ylab = "",
                    col = cols, space = 0,
                    main = "Transition probabilities")
            legend("topright", fill = rev(cols),
                   title = "child", legend = seq_len(nrow(sp)))
         }
      } else if (names(par)[npar > 0][p] == "rho") {
         vn <- split(rownames(x$model$measure), x$model$measure$constraint)
         sn <- paste0(names(vn), " (", sapply(vn, paste, collapse = ", "), ")")
         v <- menu(sn, title = "Which rho parameter ?")
         if (v > 0) {
            sp <- par$rho[[v]]
            nlev <- x$arg$nlev[[v]]
            cols <- unlist(sapply(rev(nlev), gray.colors, start = 0.1, end = 0.9))
            barplot(sp[nrow(sp):1,], xlab = "Class", ylab = "", col = cols,
                    main = "Item response probabilities", axes = FALSE)
            axis(2, at = 1:length(nlev) - 0.5, labels = paste0("V", length(nlev):1),
                 las = 1, tick = FALSE, hadj = 0)
         }
      }
   }
   DiagrammeR::grViz(text, ...)
}


