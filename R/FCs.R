
#' Frequency of citation (FC)
#'
#' This function allows you to calculate the frequency of citation (FC) per species.
#' 
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, number of uses
#' 
#' @importFrom magrittr %>%
#' @importFrom plyr ddply 
#' @importFrom plyr summarise 
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' 
#' @examples
#' 
#' FCs(ethnobotanydata)
#' 
#'@export FCs
FCs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
  
  FCps <- sp_name <- informant <- FCdata <- FCs <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
  FCdata <- data #create subset-able data
  FCdata$FCps <- dplyr::select(FCdata, -informant, -sp_name) %>% rowSums()
  FCdata$FCps[FCdata$FCps > 0] <- 1
    FCs <- plyr::ddply(FCdata, ~sp_name, plyr::summarise,
        FCs = sum(FCps))
    
    #change sort order
    FCs <- FCs[order(-FCs$FCs),] 
    
    print("Frequency of citation (FC) for each species in the data set")
    print(FCs)
}

