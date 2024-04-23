#' Construct Structural Latent Class Model
#'
#' This function constructs a structural latent class model (SLCM) for specified latent class variables.
#'
#' @param formula a formula specifying the latent structure. Detailed model specification is provided under 'Details'.
#' @param ... additional formulae for defining the model structure.
#' @param constraints a list of constraints for maintaining measurement invariance. Detailed explanation of applying constraints is available under 'Details'.
#'
#' @details
#' The \strong{`formula`} can be broadly categorized into three main types, each serving a distinct purpose:
#'
#' 1. \strong{Defining Latent Class Variables with Manifest Indicators}: Specify the relationship between a latent class variable and its manifest indicators. For example:
#'
#' 1. \strong{Defining Latent Class Variables with Manifest Indicators}: Specify the relationship between a latent class variable and its manifest indicators. In these formulas, the latent class variable, denoted with square brackets or parentheses indicating the number of classes, is on the left-hand-side (lhs) and its manifest indicators are specified on right-hand-side (rhs). For example,
#' \preformatted{LC1[k] ~ x1 + x2 + x3
#' LC2[k] ~ y1 + y2 + y3
#' LC3(k) ~ z1 + z2 + z3}
#' In these formulas, `k` denotes the number of latent classes for the variable.
#'
#' 2. \strong{Relating Latent Class Variables to Each Other}: Define relationships where one latent class variable is influenced by another. The subsequent example implies that `LC2` is conditionally affected based on `LC1`.
#' \preformatted{LC1 ~ LC2}
#'
#' 3. \strong{Defining higher-level latent class variable}: Identify latent class variables by other latent class variables rather than manifest indicators. Following example suggests that the `P` is measured by `LC1`, `LC2`, and `LC3` -- all of which are latent class variables.
#' \preformatted{P[k] ~ LC1 + LC2 + LC3}
#'
#' In all types of the formula, variables specified on the left-hand side (lhs) influence those on the right-hand side (rhs).
#'
#' The `constraints` parameter allows you to enforce specific conditions on the model to ensure precise inference. For instance, in Longitudinal Latent Class Analysis (LTA), it's imperative that latent class variables across various time-points convey identical meanings. With the `constraints` option, users can uphold measurement invariance in both the measurement and structural components of the model.
#'
#' 1. \strong{Measurement Invariance for Measurement Model}: Ensures that probabilities associated with latent class variables are consistent, thus maintaining semantic meaning across classes.
#' \preformatted{c("LC1", "LC2", "LC3")}
#' This command ensures that variables `LC1`, `LC2`, and `LC3` are semantically consistent.
#'
#' 2. \strong{Measurement invariance for structural model}: Applies constraints within the structural model to ensure consistent interpretations of transition probabilities.
#' \preformatted{c("P ~ LC1", "P -> LC2")}
#' This command implies that the transition probabilities from `P` to `LC1` and from `P` to `LC2` are consistent.
#'
#' @returns
#' An object of class `slca` containing various components of the model:
#' - `tree`: A `data.frame` that details the parent-child relationships among latent class and manifest variables.
#' - `latent`: A `data.frame` listing all latent class variables with details on each.
#' - `measure`: A `data.frame` that describes the measurement model.
#' - `struct`: A `data.frame` that details the structural model.
#'
#' The object prints model description with four part.
#' 1. Latent variables: lists the latent class variables incorporated in the model, along with the number of classes for each variable. The root variable is marked by asterisk (`*`).
#' 2. Measurement model: Shows manifest indicators for each latent class variable and indicates measurement constraints (lowercase letters signify consistency).
#' 3. Structural model: Describes conditional dependencies between latent class variables.
#' 4. Dependency constraints: Outlines the constraints applied to the conditional dependencies between latent class variables. Each column marked with an uppercase alphabet symbolizes a consistent dependency structure.
#'
#'
#' @example man/examples/slca.R
#'
#' @export
slca = function(
   formula = NULL, ..., constraints = NULL
) {
   if (!is.list(formula)) formulae <- list(formula, ...)
   else formulae <- formula
   formulae <- formulae[sapply(formulae, inherits, "formula")]
   model <- proc_formula(formulae, constraints)
   arg <- arguments(model)

   res <- list()
   res$model <- model
   res$arg <- arg

   class(res) <- "slca"
   res
}
