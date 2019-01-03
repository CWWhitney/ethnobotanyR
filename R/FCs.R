
#' Frequency of citation (FC)
#'
#' This function allows you to calculate the frequency of ciation (FC) per species.
#' 
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, number of uses
#' 
#' 
#' @importFrom magrittr %>%
#' @importFrom plyr ddply 
#' @importFrom plyr summarise 
#' 
#' @examples
#' 
#' FCs(ethnobotanydata)
#' 
#'@export FCs
FCs <- function(data) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package \"plyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
  
  sp_name <- informant <- FCps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  FCdata <- data #create subset-able data
  FCdata$FCps <- dplyr::select(FCdata, -informant, -sp_name) %>% rowSums()
  FCdata$FCps[FCdata$FCps > 0] <- 1
    FCs <- plyr::ddply(FCdata, ~sp_name, plyr::summarise,
        FCs = sum(FCps))
    
    #change sort order
    FCs <- FCs[order(-FCs$FCs),] 
    
    print("Frequency of citation (FC) for each species in the data set")
    print(FCs)
}

