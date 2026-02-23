#' Frequency of Citation (FC)
#'
#' Calculates the frequency of citation (FC) per species.
#' @usage FCs(data)
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
#' @return Data frame of species and frequency of citation (FC) values.
#' 
#' @importFrom dplyr filter summarize select left_join group_by arrange mutate
#' @importFrom magrittr %>%
#' @importFrom stats complete.cases
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
FCs <- function(data, calculate_ci = FALSE) {
  
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
  if (!calculate_ci) {
    FCs <- FCdata %>% 
      dplyr::group_by(sp_name) %>% 
      dplyr::summarize(FCs = sum(FCps))%>%
      dplyr::arrange(-FCs)
    return(as.data.frame(FCs))
  } else {
    mean_FC <- FCdata %>% dplyr::group_by(sp_name) %>%
      dplyr::summarize(
        mean_FC = mean(FCps),
        sd_FC = sd(FCps),
        n = dplyr::n()
      )
    error <- qt(0.975, mean_FC$n - 1) * mean_FC$sd_FC / sqrt(mean_FC$n)
    mean_FC$lower <- mean_FC$mean_FC - error
    mean_FC$upper <- mean_FC$mean_FC + error
    mean_FC <- mean_FC %>% dplyr::arrange(-mean_FC)
    attr(mean_FC, "note") <- "Confidence interval is for the mean frequency of citation per informant for each species (95% CI, t-distribution)."
    return(as.data.frame(mean_FC))
  }
}

