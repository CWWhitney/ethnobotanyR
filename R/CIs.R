#' Cultural Importance index (CI)
#'
#' Calculates the Cultural Importance Index (CI) per species.
#' @source Tardio, Javier, and Manuel Pardo-de-Santayana. 2008. “Cultural Importance Indices: A Comparative Analysis Based on the Useful Wild Plants of Southern Cantabria (Northern Spain) 1.” Economic Botany 62 (1): 24–39.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per informant (should be 0 or 1 values).
#' 
#' @keywords arith math logic methods misc survey
#' 
#' @return Data frame of species and Cultural Importance Index (CI) values.
#'
#' @section Warning:
#' 
#' Identification for informants and species must be listed by the names 'informant' and 'sp_name' respectively in the data set.
#' The rest of the columns should all represent separate identified ethnobotany use categories. These data should be populated with counts of uses per informant (should be 0 or 1 values).
#' 
#' @importFrom dplyr filter summarize select left_join group_by arrange mutate
#' @importFrom magrittr %>%
#' 
#' 
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' CIs(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' CIs(eb_data)
#'
#'@export CIs
#'
CIs <- function(data) {
  
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
      } # end package loading check
    
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
  CI <- CIs <- URdata  <- data_Ci <- data_URs <- URps <- sp_name <- informant <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  URdata <- data #create complete subset-able data
  
  #calculate URs
  URdata$URps <- dplyr::select(URdata, -informant, -sp_name) %>% rowSums()
    data_URs <- URdata %>% dplyr::group_by(sp_name) %>%
      dplyr::summarize (URs = sum(URps))
    
    #create new subset-able data
    data_Ci <- data_URs
    
    #calcualte CI (UR/N)
    data_Ci$CI <- data_URs$URs/(length(unique(URdata$informant))) #*
        #ncol(dplyr::select(URdata, -informant, -sp_name, -URps)))
    
    #change sort order, arrange and round
    CIs <- data_Ci %>% dplyr::select(-URs) %>%
      dplyr::arrange(-CI) %>%
      dplyr::mutate(CI = round(CI, 3))
    
    as.data.frame(CIs)
}
