# Print Estimated Parameters of an `slcafit` Object

Prints the estimated parameters of an `slca` model using an `slcafit`
object.

## Usage

``` r
param(object, ...)

# S3 method for class 'slcafit'
param(
   object, type = c("probs", "logit"),
   se = FALSE, index = FALSE, ...
)
```

## Arguments

- object:

  an object of class `slcafit`.

- ...:

  additional arguments passed to other methods.

- type:

  a character string specifying the format in which the estimated
  parameters should be displayed. The options are `"probs"` for
  probability format or `"logit"` for log-odds (logit) format. The
  default setting is `"probs"`.

- se:

  a logical value indicating whether to display standard errors (`TRUE`)
  or parameter estimates (`FALSE`). The default is `FALSE`.

- index:

  a logical value indicating whether to include (`TRUE`) or exclude
  (`FALSE`) the indices of the estimated parameters in the output. The
  default is `FALSE`.

## Value

A `list` containing the requested estimated parameters or their standard
errors (if `se = TRUE`). The components of the list include:

- pi:

  Membership probabilities for the root latent variable.

- tau:

  Conditional probabilities between latent class variables, represented
  with uppercase letters to account for measurement invariance.

- rho:

  Item response probabilities for each measurement model, represented
  with lowercase letters to account for measurement invariance.
