#' Relative Frequency of Citation (RFC)
#'
#' Calculates the relative frequency of citation (RFC) per species published by Pardo-de-Santayana (2003).
#' @source Tardio, Javier, and Manuel Pardo-de-Santayana. 2008. “Cultural Importance Indices: A Comparative Analysis Based on the Useful Wild Plants of Southern Cantabria (Northern Spain) 1.” Economic Botany 62 (1): 24–39.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#'
#' @keywords arith math logic methods misc survey
#'
#' @return Data frame of species and relative frequency of citation (RFC) values.
#'
#'@section Warning:
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
#' RFCs(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' RFCs(eb_data)
#' 
#' @export RFCs
#' 
RFCs <- function(data) {
  
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
    } #end error stops
  
  # Set the variables to NULL first, appeasing R CMD check
  RFCdata <- informant <- sp_name <- FCps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  #Create subsettable data
  RFCdata <- data
  
  #calculate FC per species (FCps)
  RFCdata$FCps <- rowSums(dplyr::select(RFCdata, -informant, -sp_name) > 0)
  
  #all UR greater than zero to count of '1' FC per species 
  RFCdata <- RFCdata %>% dplyr::mutate_if(is.numeric, ~1 * (. != 0))
    
  #calculate and creat data set of RFCs
  RFCs <- RFCdata %>% dplyr::group_by(sp_name) %>%
      dplyr::summarize(RFCs = sum(FCps/(length(unique(informant))))) %>%
      dplyr::arrange(-RFCs) %>%
      dplyr::mutate(RFCs = round(RFCs, 3))
    
    as.data.frame(RFCs)
    }

