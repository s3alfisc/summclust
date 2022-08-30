# functions taken from 'clubSandwich' package
matrix_split <- function (x, fac, dim) {
  if (is.vector(x)) {
    if (dim != "both")
      stop(paste0("Object must be a matrix in order to subset by ",
                  dim, "."))
    x_list <- split(x, fac)
    lapply(x_list, function(x) diag(x, nrow = length(x)))
  }
  else {
    lapply(levels(fac), sub_f(x, fac, dim))
  }
}

sub_f <- function (x, fac, dim){
  function(f) switch(dim,
                     row = x[fac == f, , drop = FALSE],
                     col = x[, fac == f, drop = FALSE],
                     both = x[fac == f,
                              fac == f, drop = FALSE])
}
