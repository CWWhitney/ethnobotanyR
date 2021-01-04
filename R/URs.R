#' Use Report (UR)
#'
#' Calculates the use reports (UR) per species, a common metric for ethnobotany studies.
#' 
#' @usage URs(data)
#' 
#' @references  
#' Prance, G. T., W. Balee, B. M. Boom, and R. L. Carneiro. 1987. “Quantitative Ethnobotany and the Case for Conservation in Amazonia.” Conservation Biology 1 (4): 296–310.
#' @references
#' Whitney, Cory W., J. Gebauer, and M. Anderson. “A Survey of Wild Collection and Cultivation of Indigenous Species in Iceland.” Human Ecology 40, no. 5 (2012): 781–87. \doi{10.1007/s10745-012-9517-0}
#' 
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @keywords arith math logic methods misc survey
#'
#' @return Data frame of species and use reports (UR) values.
#' 
#' @section Warning:
#' 
#' Identification for informants and species must be listed by the names 'informant' and 'sp_name' respectively in the data set.
#' The rest of the columns should all represent separate identified ethnobotany use categories. These data should be populated with counts of uses per informant (should be 0 or 1 values).
#' 
#' @importFrom dplyr filter summarize select left_join group_by 
#' @importFrom magrittr %>%
#' 
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' URs(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' URs(eb_data)
#' 
#' @export URs
#' 
URs <- function(data) {
  
  #Add error stops ####
  #Check that packages are loaded
    {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("Package \"dplyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Package \"magrittr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }# end package check
    
    ## Check that use categories are greater than zero
    if (!any(sum(dplyr::select(data, -informant, -sp_name)>0))){
      warning("The sum of all UR is not greater than zero. Perhaps not all uses have values or are not numeric.")
      data<-data[stats::complete.cases(data), ]
    }
    
    ## Use 'complete.cases' from stats to get to the collection of obs without NA
    if (any(is.na(data))) {
      warning("Some of your observations included \"NA\" and were removed. Consider using \"0\" instead.")
      data<-data[stats::complete.cases(data), ]
    }
  } #end error stops
  
  # Set the variables to NULL first, appeasing R CMD check
  URdata <- URs <- sp_name <- informant <- URps <- NULL
  
   URdata <- data #create complete subset-able data
   
   URdata$URps <- dplyr::select(URdata, -informant, -sp_name) %>% rowSums()
    URs <- URdata %>% dplyr::group_by(sp_name) %>%
                dplyr::summarize (URs = sum(URps))%>%
      dplyr::arrange(-URs) 
    
    as.data.frame(URs)
}
