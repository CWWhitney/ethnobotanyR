#' Use Value (UV) index per species
#'
#' Calculates the simplified use value (UV) index for each species in the 
#' data set (see Albuquerque et al. 2006). 
#' This is calculated the same as the \link[ethnobotanyR]{CIs} function.
#' 
#' @usage simple_UVs(data)
#' 
#' @references 
#' Albuquerque, Ulysses P., Reinaldo FP Lucena, Julio M.Monteiro, Alissandra TN Florentino, and Cecilia de Fatima CBR Almeida. 2006. “Evaluating Two Quantitative Ethnobotanical Techniques.” Ethnobotany Research and Applications 4: 51–60.
#' 
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @keywords arith math logic methods misc survey
#'
#' @return Data frame of species and simplified use value (UV) index values.
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
#' simple_UVs(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' simple_UVs(eb_data)
#' 
#'@export simple_UVs
#'
simple_UVs <- function(data) {
  
  simple_UVs <- CI <- NULL # Set variables to NULL first, appeasing R CMD check
  
  simple_UVs <- CIs(data) 
  dplyr::rename(simple_UVs, simple_UV = CI)
    
}
