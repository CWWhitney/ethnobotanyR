#' Number of Uses (NU)
#'
#' This function allows you to calculate the number of uses (NU) per species.
#'
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, number of uses
#'
#' @importFrom stats aggregate
#' 
#' @examples
#' 
#' NUs(ethnobotanydata)
#' 
#'@export NUs
NUs <- function(data) {
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
