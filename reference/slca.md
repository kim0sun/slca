# Construct Structural Latent Class Model

Constructs a latent structure with multiple latent class variables.

## Usage

``` r
slca(formula = NULL, ..., constraints = NULL)
```

## Arguments

- formula:

  a formula specifying the latent structure. Detailed model
  specifications are provided under 'Details'.

- ...:

  additional formulae for defining the model structure.

- constraints:

  a list of constraints to enforce measurement invariance. Detailed
  explanations of applying constraints are available under 'Details'.

## Value

An object of class `slca` with the following components:

- tree:

  A `data.frame` describing the parent-child relationships among latent
  class and manifest variables.

- latent:

  A `data.frame` listing all latent class variables with details such as
  the number of classes.

- measure:

  A `data.frame` describing the measurement model.

- struct:

  A `data.frame` detailing the structural model.

The printed model description is divided into four parts:

1.  **Latent variables**: Lists the latent class variables and the
    number of classes for each variable. The root variable is marked
    with an asterisk (`*`).

2.  **Measurement model**: Displays manifest indicators for each latent
    class variable and any applied measurement constraints (lowercase
    letters indicate consistency).

3.  **Structural model**: Describes the conditional relationships
    between latent class variables.

4.  **Dependency constraints**: Outlines constraints applied to
    conditional dependencies, where uppercase letters represent
    consistent dependency structures.

## Details

The **`formula`** can be categorized into three types, each serving a
distinct purpose:

1.  **Defining Latent Class Variables with Manifest Indicators**:
    Specify the relationship between a latent class variable and its
    manifest indicators. The latent class variable is on the left-hand
    side (lhs), denoted with square brackets `[]` or parentheses `()` to
    indicate the number of classes, and manifest indicators are listed
    on the right-hand side (rhs). For example:


        LC1[k] ~ x1 + x2 + x3
        LC2[k] ~ y1 + y2 + y3
        LC3(k) ~ z1 + z2 + z3
           

    Here, `k` denotes the number of latent classes for the variable.

2.  **Relating Latent Class Variables to Each Other**: Define
    relationships where one latent class variable influences another.
    For example:


        LC2 ~ LC1
           

    This formula implies that `LC2` is conditionally dependent on `LC1`.

3.  **Defining Higher-Level Latent Class Variables**: Specify
    relationships where a latent class variable is measured by other
    latent class variables instead of manifest indicators. For example:


        P[k] ~ LC1 + LC2 + LC3
           

    This indicates that the latent variable `P` is measured by the
    latent class variables `LC1`, `LC2`, and `LC3`.

In all formulas, variables on the lhs influence those on the rhs.

The `constraints` argument enforces specific conditions to ensure
precise inference, such as measurement invariance. This is particularly
useful for longitudinal analysis (eg. LTA or LCPA), where consistent
meanings of latent classes across time are essential.

1.  **Measurement Invariance for the Measurement Model**: Ensures
    probabilities associated with latent class variables remain
    consistent. For example:


        c("LC1", "LC2", "LC3")
           

    This ensures that `LC1`, `LC2`, and `LC3` have semantically
    consistent measurement probabilities.

' 2. **Measurement Invariance for the Structural Model**: Applies
constraints to ensure consistent interpretations of transition
probabilities between latent class variables. For example:


    c("P ~ LC1", "P -> LC2")
       

This ensures that the transitions from `P` to `LC1` and `P` to `LC2` are
consistent.

## Examples

``` r
# Standard LCA
slca(lc[3] ~ y1 + y2 + y3 + y4)
#> Structural Latent Variable Model
#> 
#> Latent variables (Root*):           
#>  Label: lc*
#> nclass: 3  
#> 
#> Measurement model:                            
#>  lc -> { y1, y2, y3, y4 }  a
# Latent transition analysis (LTA)
slca(lx[3] ~ x1 + x2 + x3 + x4,
     ly[2] ~ y1 + y2 + y3 + y4,
     lx ~ ly)
#> Structural Latent Variable Model
#> 
#> Latent variables (Root*):              
#>  Label: lx* ly
#> nclass: 3   2 
#> 
#> Measurement model:                            
#>  lx -> { x1, x2, x3, x4 }  a
#>  ly -> { y1, y2, y3, y4 }  b
#> 
#> Structural model:             
#>  lx -> { ly }
#> 
#> Dependency constraints:
#>  A       
#>  lx -> ly
# LTA with measurement invariance
slca(l1[3] ~ y11 + y21 + y31 + y41,
     l2[3] ~ y12 + y22 + y32 + y42,
     l1 ~ l2, constraints = c("l1", "l2"))
#> Structural Latent Variable Model
#> 
#> Latent variables (Root*):              
#>  Label: l1* l2
#> nclass: 3   3 
#> 
#> Measurement model:                                
#>  l1 -> { y11, y21, y31, y41 }  a
#>  l2 -> { y12, y22, y32, y42 }  a
#> 
#> Structural model:             
#>  l1 -> { l2 }
#> 
#> Dependency constraints:
#>  A       
#>  l1 -> l2
# Joint latent class analysis
slca(lx[2] ~ x1 + x2 + x3 + x4,
     ly[3] ~ y1 + y2 + y3 + y4,
     lz[2] ~ z1 + z2 + z3 + z4,
     jc[3] ~ lx + ly + lz)
#> Structural Latent Variable Model
#> 
#> Latent variables (Root*):                    
#>  Label: lx ly lz jc*
#> nclass: 2  3  2  3  
#> 
#> Measurement model:                            
#>  lx -> { x1, x2, x3, x4 }  a
#>  ly -> { y1, y2, y3, y4 }  b
#>  lz -> { z1, z2, z3, z4 }  c
#> 
#> Structural model:                     
#>  jc -> { lx, ly, lz }
#> 
#> Dependency constraints:
#>  A        B        C       
#>  jc -> lx jc -> ly jc -> lz
# Latent class profile analysis (with measurement invariance)
slca(l1[3] ~ x1 + x2 + x3 + x4,
     l2[3] ~ y1 + y2 + y3 + y4,
     l3[3] ~ z1 + z2 + z3 + z4,
     pf[4] ~ l1 + l2 + l3,
     constraints = c("l1", "l2", "l3"))
#> Structural Latent Variable Model
#> 
#> Latent variables (Root*):                    
#>  Label: l1 l2 l3 pf*
#> nclass: 3  3  3  4  
#> 
#> Measurement model:                            
#>  l1 -> { x1, x2, x3, x4 }  a
#>  l2 -> { y1, y2, y3, y4 }  a
#>  l3 -> { z1, z2, z3, z4 }  a
#> 
#> Structural model:                     
#>  pf -> { l1, l2, l3 }
#> 
#> Dependency constraints:
#>  A        B        C       
#>  pf -> l1 pf -> l2 pf -> l3
```
