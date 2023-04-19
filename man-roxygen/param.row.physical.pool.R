#' @param row.physical.pool  Should physical pooling be done (default) or should logical pooling be done. For example, if there are 3 rows in 
#'   the data matrix and row.pool.in=c(1,1,3), then in physical pooling, the entries in rows 1 and 2 are physically added together to create
#'   2 rows in the data matrix before fitting. Because the data has changed, you cannot compare physical pooling using AIC. In logical pooling,
#'   the data matrix is unchanged, but now parameters p1=p2 but the movement parameters for the rest of the matrix are not forced equal.
