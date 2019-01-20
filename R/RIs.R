#' #Relative Importance Index (RI)
#'
#' This function allows you to calculate the relative importance index (RI) per species, published by Pardo-de-Santayana (2003).
#' @source Tardio, J., and M. Pardo-de-Santayana, 2008. Cultural Importance Indices: A Comparative Analysis Based on the Useful Wild Plants of Southern Cantabria (Northern Spain) 1. Economic Botany, 62(1), 24-39. <https://doi.org/10.1007/s12231-007-9004-5>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, relative importance
#'
#' @importFrom plyr ddply summarise
#' @importFrom stats aggregate
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' @importFrom dplyr select
#' 
#' @examples
#' 
#' RIs(ethnobotanydata)
#' 
#' @export RIs
RIs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
  
  FCps <- RFCstestdata <- informant <- sp_name <- RFCstestdata2 <- RNUs <- RNUsdataaggr <- RNUstestdata <- RFCs <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
 
    #create subsettable data for RFCs
    RFCstestdata <- data
    
    #calculate RFCs
    RFCstestdata$FCps <- rowSums(dplyr::select(RFCstestdata, -informant, -sp_name) > 0)
    RFCstestdata$FCps[RFCstestdata$FCps >0] <- 1
    RFCstestdata2 <- plyr::ddply(RFCstestdata,
        ~sp_name, plyr::summarise, FCs = sum(FCps))
    RFCstestdata2$RFCs <- RFCstestdata2$FCs/max(RFCstestdata2$FCs)
    RFCs <- RFCstestdata2[, c(1, length(names(RFCstestdata2)))]
    
   
    #create subsettable data for RFCs
    RNUstestdata <- data
    
    #calculate RNUs
    RNUsdataaggr <- stats::aggregate(dplyr::select(RNUstestdata, -informant, -sp_name), 
                                     by = list(sp_name = RNUstestdata$sp_name),
                                     FUN = sum)
    RNUsdataaggr[, -1][RNUsdataaggr[, -1] > 0] <- 1
    RNUsdataaggr$NUs <- rowSums(RNUsdataaggr[,-1])
    RNUsdataaggr[, c(1, length(names(RNUsdataaggr)))]
    RNUsdataaggr$RNUs <- RNUsdataaggr$NUs/max(RNUsdataaggr$NUs)
    RNUs <- RNUsdataaggr[, c(1, length(names(RNUsdataaggr)))]

    #merge RNUs and RFCs    
    RIs <- merge(RNUs, RFCs, by = "sp_name")
    RIs$RIs <- (RIs$RNUs + RIs$RFCs)/2
    
    #change sort order
    RIs <- RIs[order(-RIs$RIs),] 

    print("Relative Importance Index (RI) for each species in the data set")
    print(RIs[, c(1, length(names(RIs)))], digits=3)
}
