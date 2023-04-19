#' @param row.pool.in,col.pool.in Vectors (character/numeric) of length s and t respectively. These identify the rows/columns to be pooled before the analysis is done.
#'   The vectors consists of entries where pooling takes place if the entries are the same. For example, if s=4, then 
#'   row.pool.in = c(1,2,3,4) implies no pooling because all entries are distinct; row.pool.in=c("a","a","b","b") implies that the 
#'   first two rows will be pooled and the last two rows will be pooled. It is not necessary that row/columns be continuous to be pooled, but
#'   this is seldom sensible. A careful choice of pooling labels helps to remember what as done, e.g. row.pool.in=c("123","123","123","4") indicates
#'   that the first 3 rows are pooled and the 4th row is not pooled. Character entries ensure that the resulting matrix is sorted properly (e.g. if 
#'   row.pool.in=c(123,123,123,4), then the same pooling is done, but the matrix rows are sorted rather strangely.
