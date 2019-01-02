#' Calculate the use report (UR) per secies
#'
#' This function allows you to calculate the use report (UR) per secies, a common metric for ethnobotany studies.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords ethnobotany, cultural value, use report
#'
#' @importFrom magrittr %>%
#' @importFrom plyr ddply 
#' @importFrom plyr summarise 
#' 
#' @examples
#' 
#' URs(ethnobotanydata)
#' 
#' @export URs
URs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
  
  sp_name <- informant <- URps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
   URdata <- data #create subset-able data
   URdata$URps <- dplyr::select(URdata, -informant, -sp_name) %>% rowSums()
    data_URs <- plyr::ddply(URdata, ~sp_name,
                plyr::summarise, URs = sum(URps))
    
    #change sort order
    URs <- data_URs[order(-data_URs$URs),] 
    
    print("Total number of Use Reports (URs) for each species in the data set")
    print(URs)
}
