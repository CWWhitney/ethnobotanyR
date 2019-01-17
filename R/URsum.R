#' Sum of all Use Reports (UR) for all species
#'
#' This function allows you to calculate the sum of all ethnobotany use reports (UR) for all species, a common metric for ethnobotany studies.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords ethnobotany, use report, quantitative ethnobotany
#'
#' @importFrom dplyr select
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#'
#' @examples
#' 
#' URsum(ethnobotanydata)
#' 
#' @export URsum
URsum <- function(data) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  URsum <- informant <- sp_name <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  #add error stops with validate_that
  assertthat::validate_that("informant" %in% colnames(data), msg = "A column called \"informant\" is missing from your data.")
  assertthat::validate_that("sp_name" %in% colnames(data), msg = "A column called \"sp_name\" is missing from your data.")
  assertthat::validate_that(all(sum(dplyr::select(data, -informant, -sp_name)>0)) , msg = "Not all uses have values.")
  
  URsum <- sum(dplyr::select(data, -informant, -sp_name))
  
  print("Sum of all Use Reports (UR) for all species in the data set")
  print(URsum)
}
