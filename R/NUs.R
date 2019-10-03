#' Number of Uses (NU)
#'
#' Calculates the number of uses (NU) per species.
#' @source Prance, G. T., W. Balee, B. M. Boom, and R. L. Carneiro. 1987. “Quantitative Ethnobotany and the Case for Conservation in Amazonia.” Conservation Biology 1 (4): 296–310.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @keywords arith math logic methods misc survey
#' 
#' @return Data frame of species and number of uses (NU) values.
#' 
#' @section Warning:
#' 
#' Identification for informants and species must be listed by the names 'informant' and 'sp_name' respectively in the data set.
#' The rest of the columns should all represent separate identified ethnobotany use categories. These data should be populated with counts of uses per informant (should be 0 or 1 values).
#' 
#' @importFrom dplyr select
#' @importFrom stats aggregate
#' 
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' 
#' NUs(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant<-sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name<-sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' NUs(eb_data)
#' 
#'@export NUs
NUs <- function(data) {
  
  #Add error stops ####
  #Check that packages are loaded
    {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
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
    }#end error stops
  
  # Set the variables to NULL first, appeasing R CMD check
  NUdata <- NUdataaggr <- NUs <- informant <- sp_name <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  NUdata <- data #create complete subset-able data
  
  #Calculate NUs
    NUdataaggr <- stats::aggregate(dplyr::select(NUdata, -informant, -sp_name),
        by = list(sp_name = data$sp_name),FUN = sum)
    
    NUdataaggr <- NUdataaggr %>% dplyr::mutate_if(is.numeric, ~1 * (. != 0))
    
    NUdataaggr$NUs <- NUdataaggr %>% dplyr::select(-sp_name) %>% rowSums()
    
    #change sort order
    NUs <- dplyr::select(NUdataaggr, sp_name, NUs) %>%
      dplyr::arrange(-NUs) 
    
    as.data.frame(NUs)
}
