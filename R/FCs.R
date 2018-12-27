
#' Frequency of citation (FC)
#'
#' This function allows you to calculate the frequency of ciation (FC) per species.
#'
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, number of uses
#'
#' @examples
#' FCs(ethnobotanydata)
#'@export FCs
FCs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
    data$FCps <- rowSums((data[, -c(1:2)]) >
        0)
    data$FCps[data$FCps > 0] <- 1
    FCs <- plyr::ddply(data, ~sp_name, summarise,
        FCs = sum(FCps))
    print("Frequency of citation (FC) for each species in the data set")
    print(FCs)
}

