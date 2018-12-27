#' Calculate the use report (UR) per secies
#'
#' This function allows you to calculate the use report (UR) per secies, a common metric for ethnobotany studies.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords ethnobotany, cultural value, use report
#'
#' @examples
#' URs(ethnobotanydata)
#' @export URs
URs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
    data$URps <- rowSums(data[, -c(1:2)])
    library(plyr)
    data_URs <- plyr::ddply(data, ~sp_name,
        summarise, URs = sum(URps))
    print("Total number of Use Reports (URs) for each species in the data set")
    print(data_URs)
}
