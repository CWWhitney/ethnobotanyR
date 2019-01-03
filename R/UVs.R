#' Use Value (UV) index per species
#'
#' This function allows you to calculate the use value (UV) index for each species in the data set (see Albuquerque et al. 2006).
#' @source Albuquerque, U. P., R. F. P. Lucena, J. M. Monteiro, A. T. N. Florentino, and C. F. C. B. R. Almeida. 2006. Evaluating Two Quantitative Ethnobotanical Techniques. Ethnobotany Research and Applications 4:51–60.<DOI:http://hdl.handle.net/10125/237>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, cultural importance
#'
#' @importFrom plyr ddply
#' @importFrom plyr summarise
#'
#' @examples
#'
#' UVs(ethnobotanydata)
#' 
#'@export UVs
UVs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.", 
            call. = FALSE)
    }
  
  sp_name <- informant <- UVps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
    data$UVps <- rowSums((data[, -c(1:2)]) > 
        0)
    UVs <- plyr::ddply(data, ~sp_name, plyr::summarise, 
        UVs = sum(UVps)/(length(unique(informant))))
    
    #change sort order
    UVs <- UVs[order(-UVs$UVs),] 
    
    print("Use Value index (UV) for each species in the data set")
    print(UVs)
}
