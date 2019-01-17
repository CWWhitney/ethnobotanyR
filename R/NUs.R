#' Number of Uses (NU)
#'
#' This function allows you to calculate the number of uses (NU) per species.
#'
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, number of uses
#'
#' @importFrom stats aggregate
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' 
#' @examples
#' 
#' NUs(ethnobotanydata)
#' 
#'@export NUs
NUs <- function(data) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  NUdataaggr <- NUs <- informant <- sp_name <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
    NUdataaggr <- stats::aggregate(data[, -c(1:2)],
        by = list(sp_name = data$sp_name),
        FUN = sum)
    NUdataaggr[, -1][NUdataaggr[, -1] >
        0] <- 1
    NUdataaggr$NUs <- rowSums(NUdataaggr[,
        -1])
    
    #change sort order
    NUs<-NUdataaggr[, c(1, length(names(NUdataaggr)))]
    NUs <- NUs[order(-NUs$NUs),] 
    
    print("Number of Uses (NU) for each species in the data set")
    print(NUs)
}
