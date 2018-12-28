#' Sum of all Use Reports (UR) for all species
#'
#' This function allows you to calculate the sum of all ethnobotany use reports (UR) for all species, a common metric for ethnobotany studies.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords ethnobotany, use report, quantitative ethnobotany
#'
#' @examples
#' 
#' URsum(ethnobotanydata)
#' 
#' @export URsum
URsum <- function(data) {
  URsum <- sum(data[, -c(1:2)])
  
  print("Sum of all Use Reports (UR) for all species in the data set")
  print(URsum)
}
