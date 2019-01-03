#' #Relative Importance Index (RI)
#'
#' This function allows you to calculate the relative importance index (RI) per species, published by Pardo-de-Santayana (2003).
#' @source Tardío, J., and M. Pardo-de-Santayana, 2008. Cultural Importance Indices: A Comparative Analysis Based on the Useful Wild Plants of Southern Cantabria (Northern Spain) 1. Economic Botany, 62(1), 24-39. <DOI:10.1007/s12231-007-9004-5>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, relative importance
#'
#' @importFrom plyr ddply summarise
#' @importFrom stats aggregate
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
  
  FCps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  

    RFCstestdata <- data
    RFCstestdata$FCps <- rowSums((RFCstestdata[,
        -c(1:2)]) > 0)
    RFCstestdata$FCps[RFCstestdata$FCps >
        0] <- 1
    RFCstestdata2 <- plyr::ddply(RFCstestdata,
        ~sp_name, plyr::summarise, FCs = sum(FCps))
    RFCstestdata2$RFCs <- RFCstestdata2$FCs/max(RFCstestdata2$FCs)
    RFCs <- RFCstestdata2[, c(1, length(names(RFCstestdata2)))]

    RNUstestdata <- data
    RNUsdataaggr <- stats::aggregate(RNUstestdata[,
        -c(1:2)], by = list(sp_name = RNUstestdata$sp_name),
        FUN = sum)
    RNUsdataaggr[, -1][RNUsdataaggr[, -1] >
        0] <- 1
    RNUsdataaggr$NUs <- rowSums(RNUsdataaggr[,
        -1])
    RNUsdataaggr[, c(1, length(names(RNUsdataaggr)))]
    RNUsdataaggr$RNUs <- RNUsdataaggr$NUs/max(RNUsdataaggr$NUs)
    RNUs <- RNUsdataaggr[, c(1, length(names(RNUsdataaggr)))]

    RIs <- merge(RNUs, RFCs, by = "sp_name")
    RIs$RIs <- (RIs$RNUs + RIs$RFCs)/2
    
    #change sort order
    RIs <- RIs[order(-RIs$RIs),] 

    print("Relative Importance Index (RI) for each species in the data set")
    print(RIs[, c(1, length(names(RIs)))])
}
