#' Use Value (UV) index per species
#'
#' This function allows you to calculate the use value (UV) index for each species in the data set (see Albuquerque et al. 2006).
#' @source Albuquerque, U. P., R. F. P. Lucena, J. M. Monteiro, A. T. N. Florentino, and C. F. C. B. R. Almeida. 2006. Evaluating Two Quantitative Ethnobotanical Techniques. Ethnobotany Research and Applications 4:51–60. <http://hdl.handle.net/10125/237>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, cultural importance
#'
#' @importFrom plyr ddply
#' @importFrom plyr summarise
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#'
#' @examples
#'
#' UVs(ethnobotanydata)
#' 
#'@export UVs
UVs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.", 
            call. = FALSE)
    }
  
  UVpsdata <- sp_name <- informant <- UVps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
  #create subsettable data
  UVpsdata <- data
  
  UVpsdata$UVps <- rowSums(dplyr::select(UVpsdata, -informant, -sp_name) > 0)
    UVs <- plyr::ddply(UVpsdata, ~sp_name, plyr::summarise, 
        UVs = sum(UVps)/(length(unique(informant))))
    
    #change sort order
    UVs <- UVs[order(-UVs$UVs),] 
    
    print("Use Value index (UV) for each species in the data set")
    print(UVs)
}
