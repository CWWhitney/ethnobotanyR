#' Cultural Importance index (CI)
#'
#' This function allows you to calculate the Cultural Importance Index (CI) per species.
#' @source Tardío, J., and M. Pardo-de-Santayana, 2008. Cultural Importance Indices: A Comparative Analysis Based on the Useful Wild Plants of Southern Cantabria (Northern Spain) 1. Economic Botany, 62(1), 24-39. <https://doi.org/10.1007/s12231-007-9004-5>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany cultural importance
#'
#' @importFrom magrittr %>%
#' @importFrom plyr ddply 
#' @importFrom plyr summarise
#' @importFrom dplyr select
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' 
#' @examples
#' 
#' CIs(ethnobotanydata)
#'
#'@export CIs
#'
CIs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
  
  CIs <- URdata  <- data_Ci <- data_URs <- URps <- sp_name <- informant <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
  URdata<- data #create subset-able data
  URdata$URps <- dplyr::select(URdata, -informant, -sp_name) %>% rowSums()
    data_URs <- plyr::ddply(URdata, ~sp_name,
                plyr::summarise, URs = sum(URps))
    data_Ci <- data_URs
    data_Ci$Ci <- data_URs$URs/(length(unique(URdata$informant)) *
        ncol(URdata[, -c(1:2)]))
    
    #change sort order
    CIs<-data_Ci[c(1, 3)]
    CIs <- CIs[order(-CIs$Ci),] 
    
    print("Cultural Importance index (CI) for each species in the data set")
    print(CIs)
}
