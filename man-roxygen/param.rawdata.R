#' @param rawdata An (s+1) x (t+1) of the raw data BEFORE pooling.
#'   The s x t upper left matrix is the number of animals released in row stratum i and recovered in
#'   column stratum j. Row s+1 contains the total number of UNMARKED animals recovered in column stratum j.
#'   Column t+1 contains the number of animals marked in each row stratum but not recovered in any column stratum.
#'   The rawdata[s+1, t+1] is not used and can be set to 0 or NA.
#'   The sum of the entries in each of the first s rows is then the number of animals marked in each row stratum.
#'   The sum of the entries in each of the first t columns is then the number of animals captured (marked and unmarked) in each column stratum.
#'   The row/column names of the matrix may be set to identify the entries in the output.
