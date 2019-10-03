#' Frequency of Citation (FC)
#'
#' Calculates the frequency of citation (FC) per species.
#' @source Prance, G. T., W. Balee, B. M. Boom, and R. L. Carneiro. 1987. “Quantitative Ethnobotany and the Case for Conservation in Amazonia.” Conservation Biology 1 (4): 296–310.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @keywords arith math logic methods misc survey
#' 
#' @return Data frame of species and frequency of citation (FC) values.
#' 
#' @importFrom dplyr filter summarize select left_join group_by 
#' @importFrom magrittr %>%
#' 
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' FCs(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant<-sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name<-sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' FCs(eb_data)
#'
#'@export FCs
#'
FCs <- function(data) {
  
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
  FCps <- sp_name <- informant <- FCdata <- FCs <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  FCdata <- data #create complete subset-able data
  
  FCdata$FCps <- dplyr::select(FCdata, -informant, -sp_name) %>% rowSums()
  FCdata <- FCdata %>% dplyr::mutate_if(is.numeric, ~1 * (. != 0))
    FCs <- FCdata %>% 
      dplyr::group_by(sp_name) %>% 
      dplyr::summarize(FCs = sum(FCps))%>%
      dplyr::arrange(-FCs) 
    
    as.data.frame(FCs)
}

