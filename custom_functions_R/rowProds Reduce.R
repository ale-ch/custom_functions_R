rowProds <- function(df, col_names = character()) {
  Reduce(`*`, df[, col_names])
}


