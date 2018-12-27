#' Use Value (UV) index
#'
#' This function allows you to calculate the use value (UV) index per species (see Albuquerque et al. 2006).
#' @source Albuquerque, U. P., R. F. P. Lucena, J. M. Monteiro, A. T. N. Florentino, and C. F. C. B. R. Almeida. 2006. Evaluating Two Quantitative Ethnobotanical Tech- niques. Ethnobotany Research and Applications 4:51–60.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, cultural importance
#'
#' @examples
#' UVs(ethnobotanydata)
#'@export UVs
UVs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.", 
            call. = FALSE)
    }
    data$UVps <- rowSums((data[, -c(1:2)]) > 
        0)
    plyr::ddply(data, ~sp_name, summarise, 
        UVs = sum(UVps)/(length(unique(informant))))
}
