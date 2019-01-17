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
  
  # Setting 'universal' variables to NULL, appeasing R CMD check
  URsum <- informant <- sp_name <- NULL 
  
  #add error stops with validate_that
  assertthat::validate_that("informant" %in% colnames(data), msg = "The required column called \"informant\" is missing from your data. Add it.")
  assertthat::validate_that("sp_name" %in% colnames(data), msg = "The required column called \"sp_name\" is missing from your data. Add it.")
  
  assertthat::validate_that(is.factor(data$informant), msg = "The \"informant\" is not a factor variable. Transform it.")
  assertthat::validate_that(is.factor(data$sp_name), msg = "The \"sp_name\" is not a factor variable. Transform it.")
  
  assertthat::validate_that(all(sum(dplyr::select(data, -informant, -sp_name)>0)) , msg = "The sum of all UR is not greater than zero. Perhaps not all uses have values or are not numeric.")
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  data_complete<-data[stats::complete.cases(data), ]
  #message about complete cases
  assertthat::see_if(length(data_complete) == length(data), msg = "Some of your observations included \"NA\" and were removed. Consider using \"0\" instead.")
  
  URsum <- sum(dplyr::select(data, -informant, -sp_name))
  
  print("Sum of all Use Reports (UR) for all species in the data set")
  print(URsum)
}
