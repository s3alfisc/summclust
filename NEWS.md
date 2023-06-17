# summclust 0.7

* Adds option to sparsify `vcov_CR3J.fixest`. This reduces the computational cost of 
  computing CRV3 covariance matrices when the regression's design matrix 
  gets very sparse. It can't be used within `summclust` because the `beta_jack` do not 
  match. However, it's useful when users just want to access `vcov_CR3J.fixest`.

# summclust 0.6

# summclust 0.7

* fixes a bug that turned NA values in cluster variables not contained in the model call into a distinct cluster. Instead, `summclust` now throws an error.

# summclust 0.6

* support for regression weights / weighted least squares
* prettier errors and warnings via `cli` (0.5.2)

# summclust 0.5

* Initial version released on CRAN. 
