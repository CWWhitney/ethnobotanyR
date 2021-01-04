#' Use Value (UV) index per species
#'
#' Calculates the use value (UV) index for each species in the data set 
#' (see Tardio and Pardo-de-Santayana 2008). 
#' This is calculated the same as the \link[ethnobotanyR]{CIs} function.
#' 
#' @usage UVs(data)
#' 
#' @references  
#' Tardio, Javier, and Manuel Pardo-de-Santayana. “Cultural Importance Indices: A Comparative Analysis Based on the Useful Wild Plants of Southern Cantabria (Northern Spain)1.” Economic Botany 62, no. 1 (May 2008): 24–39. \doi{10.1007/s12231-007-9004-5}
#' 
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @keywords quantitative ethnobotany cultural importance use value
#'
#' @return Data frame of species and use value (UV) index results.
#' 
#' @section Warning:
#' 
#' Identification for informants and species must be listed by the names 'informant' and 'sp_name' respectively in the data set.
#' The rest of the columns should all represent separate identified ethnobotany use categories. These data should be populated with counts of uses per informant (should be 0 or 1 values).
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter summarize select left_join group_by mutate
#'
#' @examples
#'
#' #Use built-in ethnobotany data example
#' UVs(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant<-sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name<-sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' UVs(eb_data)
#' 
#'@export UVs
#'
UVs <- function(data) {
  
UVs <- CI <- NULL # Set variables to NULL first, appeasing R CMD check
  
  UVs <- CIs(data)
  dplyr::rename(UVs, UV = CI)
  
}
