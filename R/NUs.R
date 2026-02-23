#' Number of Uses (NU)
#'
#' Calculates the number of uses (NU) per species.
#' @usage NUs(data)
#' 
#' @references  
#' Prance, G. T., W. Balee, B. M. Boom, and R. L. Carneiro. 1987. “Quantitative Ethnobotany and the Case for Conservation in Amazonia.” Conservation Biology 1 (4): 296–310.
#' 
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @param calculate_ci Logical. If TRUE, returns 95% confidence intervals for the mean per species.
#' @importFrom stats qt sd
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
#' @importFrom dplyr select mutate_if arrange
#' @importFrom magrittr %>%
#' @importFrom stats aggregate complete.cases
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
NUs <- function(data, calculate_ci = FALSE) {
  
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
  NUdata$NUps <- dplyr::select(NUdata, -informant, -sp_name) %>% rowSums()
  if (!calculate_ci) {
    #Calculate NUs
    NUdataaggr <- stats::aggregate(dplyr::select(NUdata, -informant, -sp_name),
        by = list(sp_name = data$sp_name),FUN = sum)
    NUdataaggr <- NUdataaggr %>% dplyr::mutate_if(is.numeric, ~1 * (. != 0))
    NUdataaggr$NUs <- NUdataaggr %>% dplyr::select(-sp_name) %>% rowSums()
    NUs <- dplyr::select(NUdataaggr, sp_name, NUs) %>%
      dplyr::arrange(-NUs)
    return(as.data.frame(NUs))
  } else {
    mean_NU <- NUdata %>% dplyr::group_by(sp_name) %>%
      dplyr::summarize(
        mean_NU = mean(NUps),
        sd_NU = sd(NUps),
        n = dplyr::n()
      )
    error <- qt(0.975, mean_NU$n - 1) * mean_NU$sd_NU / sqrt(mean_NU$n)
    mean_NU$lower <- mean_NU$mean_NU - error
    mean_NU$upper <- mean_NU$mean_NU + error
    mean_NU <- mean_NU %>% dplyr::arrange(-mean_NU)
    attr(mean_NU, "note") <- "Confidence interval is for the mean number of uses per informant for each species (95% CI, t-distribution)."
    return(as.data.frame(mean_NU))
  }
}
