#' A Cultural Value Index (CV) Function
#'
#' This function allows you to calculate the Cultural Value Index (CV) published by Reyes-García et al. (2006).
#' @source Reyes-García, V., T. Huanca, V. Vadez, W. Leonard, and D. Wilkie. 2006. Cultural, Practical, and Economic Value of Wild Plants: A Quantitative Study in the Bolivian Amazon. Economic Botany 60(1):62–74.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords ethnobotany, cultural value
#'
#' @examples
#' CVs(ethnobotanydata)
#'
#' @export CVs
#'
CVs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.", 
            call. = FALSE)
    }
    CVURdata <- data
    CVURdata$URps <- rowSums(CVURdata[, 
        -c(1:2)])
    CVURdata2 <- plyr::ddply(CVURdata, 
        ~sp_name, summarise, URs = sum(URps))
    CVURdata2$URdivN <- CVURdata2$URs/(length(unique(CVURdata$informant)))
    URdivN <- CVURdata2[, c(1, length(names(CVURdata2)))]
    
    CVs1 <- merge(NUdivNC, FCdivN, by = "sp_name")
    CVs2 <- merge(CVs1, URdivN, by = "sp_name")
    CVs2$CVs <- CVs2$NUdivNC * CVs2$FCdivN * 
        CVs2$URdivN
    print(CVs2[, c(1, length(names(CVs2)))])
}
