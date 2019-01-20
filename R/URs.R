#' Calculate the use report (UR) per species
#'
#' This function allows you to calculate the use report (UR) per secies, a common metric for ethnobotany studies.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords ethnobotany, cultural value, use report
#'
#' @importFrom magrittr %>%
#' @importFrom plyr ddply 
#' @importFrom plyr summarise 
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' 
#' @examples
#' 
#' URs(ethnobotanydata)
#' 
#' @export URs
URs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
  
  URdata <- URs <- sp_name <- informant <- URps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
   URdata <- data #create subset-able data
   URdata$URps <- dplyr::select(URdata, -informant, -sp_name) %>% rowSums()
    data_URs <- plyr::ddply(URdata, ~sp_name,
                plyr::summarise, URs = sum(URps))
    
    #change sort order
    URs <- data_URs[order(-data_URs$URs),] 
    
    print("Total number of Use Reports (URs) for each species in the data set")
    print(URs)
}
