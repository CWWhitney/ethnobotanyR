#' Relative Frequency of Citation (RFC)
#'
#' This function allows you to calculate the relative frequency of citation (RFC) per species published by Pardo-de-Santayana (2003).
#' @source Tardio, J., and M. Pardo-de-Santayana, 2008. Cultural Importance Indices: A Comparative Analysis Based on the Useful Wild Plants of Southern Cantabria (Northern Spain) 1. Economic Botany, 62(1), 24-39. <https://doi.org/10.1007/s12231-007-9004-5>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, number of uses
#'
#' @importFrom plyr ddply
#' @importFrom dplyr select
#' 
#' @examples
#' RFCs(ethnobotanydata)
#' 
#' @export RFCs
RFCs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
  
  RFCdata <- informant <- sp_name <- FCps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
  #Create subsettable data
  RFCdata <- data
  
  RFCdata$FCps <- rowSums(dplyr::select(RFCdata, -informant, -sp_name) >
        0)
  RFCdata$FCps[RFCdata$FCps > 0] <- 1
    RFCs<-plyr::ddply(RFCdata, ~sp_name, plyr::summarise,
        RFCs = sum(FCps/(length(unique(informant)))))
    
    #change sort order
    RFCs <- RFCs[order(-RFCs$RFCs),] 
    
    print("Relative Frequency of Citation (RFC) for each species in the data set")
    print(RFCs)
    }

