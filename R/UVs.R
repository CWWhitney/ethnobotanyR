#' Use Value (UV) index per species
#'
#' Calculates the use value (UV) index for each species in the data set (see Tardio and Pardo-de-Santayana 2008).
#' @usage UVs(data)
#' 
#' @references  
#' Tardio, Javier, and Manuel Pardo-de-Santayana. “Cultural Importance Indices: A Comparative Analysis Based on the Useful Wild Plants of Southern Cantabria (Northern Spain)1.” Economic Botany 62, no. 1 (May 2008): 24–39. <https://doi.org/10.1007/s12231-007-9004-5>
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
  
  #Add error stops ####
  {
    #Check that packages are loaded
    {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("Package \"dplyr\" needed for this function to work. Please install it.", 
            call. = FALSE)
    }
    if (!requireNamespace("magrittr", quietly = TRUE)) {
      stop("Package \"magrittr\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
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
  URdata <- sp_name <- informant <- UVps <- UV <- NULL
  
  URdata <- data #create complete subset-able data
  
  #Use-Value (UV) calculated after Tardio and Pardo-de-Santayana (2008)
  
  data_URs <- URs(URdata) #calculate URs()
  
  data_UV <- data_URs #create new subset-able data for UVs
  
  #UV differs from Ci (the CIs function) only in that 
  #it sums UR grouping by informant (the sum of the uses cited by each informant) 
  #then sums all these data 
  #calcualte UV, c.f.. calcualte CI (UR/N)
  data_UV$UV <- data_URs$URs/(length(unique(URdata$informant)))
  
  #change sort order, arrange and round
  UVs <- data_UV %>% dplyr::select(-URs) %>%
    dplyr::arrange(-UV) %>%
    dplyr::mutate(UV = round(UV, 3))
    
    as.data.frame(UVs)
}
