#' @exportS3Method base::plot slca
plot.slca <- function(x, abbreviation = FALSE, dir = "TD",
                        equal_rank = NULL, font = "Helvetica", ...) {
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

   DiagrammeR::grViz(text, ...)

   if (inherits(x, "estimated")) {
      oldmar <- par()$mar
      on.exit(par(mar = oldmar))

      par <- slca::param(x)
      npar <- c(x$arg$npi, x$arg$ntau, x$arg$nrho)
      p <- menu(names(par)[npar > 0], title = "Select parameter type")
      if (p == 1) {
         v <- menu(names(par$pi), title = "Which latent variable ?")
         if (v == 0) invisible()
         else sp <- par$pi[[v]]
      } else if (p == 2) {
         vn <- split(apply(x$model$struct[,1:3], 1, paste0, collapse = " "),
                     x$model$struct$constraint)
         sn <- paste0(names(vn), " (", sapply(vn, paste, collapse = ", "), ")")
         v <- menu(sn, title = "Which tau parameter ?")
         if (v == 0) invisible()
         else sp <- par$tau[[v]]
         barplot(par$tau[[1]], xlab = "Parent", ylab = "",
                 main = "Transition probabilities", space = 0)
      } else if (p == 3) {
         vn <- split(rownames(x$model$measure), x$model$measure$constraint)
         sn <- paste0(names(vn), " (", sapply(vn, paste, collapse = ", "), ")")
         v <- menu(sn, title = "Which rho parameter ?")
         if (v == 0) invisible()
         else sp <- par$rho[[v]]
      } else invisible()
   }
   invisible()
}


