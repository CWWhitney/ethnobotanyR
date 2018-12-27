#' #Relative Importance Index (RI)
#'
#' This function allows you to calculate the relative importance index (RI) per species, published by Pardo-de-Santayana (2003).
#' @source Pardo-de-Santayana, M. 2003. Las plantas en la cultura tradicional de la antigua Merindad de Campoo. Ph.D. dissertation, Departamento de Biología, Facul- tad de Ciencias, Universidad Autónoma de Madrid, Spain.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, relative importance
#'
#' @examples
#' RIs(ethnobotanydata)
#' @export RIs
RIs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
    RFCstestdata <- data
    RFCstestdata$FCps <- rowSums((RFCstestdata[,
        -c(1:2)]) > 0)
    RFCstestdata$FCps[RFCstestdata$FCps >
        0] <- 1
    RFCstestdata2 <- plyr::ddply(RFCstestdata,
        ~sp_name, summarise, FCs = sum(FCps))
    RFCstestdata2$RFCs <- RFCstestdata2$FCs/max(RFCstestdata2$FCs)
    RFCs <- RFCstestdata2[, c(1, length(names(RFCstestdata2)))]

    RNUstestdata <- data
    RNUsdataaggr <- aggregate(RNUstestdata[,
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

    print("Relative Importance Index (RI) for each species in the data set")
    print(RIs[, c(1, length(names(RIs)))])
}
