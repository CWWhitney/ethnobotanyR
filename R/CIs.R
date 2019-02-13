#' Cultural Importance index (CI)
#'
#' This function allows you to calculate the Cultural Importance Index (CI) per species.
#' @source Tardio, J., and M. Pardo-de-Santayana, 2008. Cultural Importance Indices: A Comparative Analysis Based on the Useful Wild Plants of Southern Cantabria (Northern Spain) 1. Economic Botany, 62(1), 24-39. <https://doi.org/10.1007/s12231-007-9004-5>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany cultural importance
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter summarize select left_join group_by arrange
#' @importFrom assertthat validate_that see_if
#' 
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' CIs(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant<-sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name<-sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' CIs(eb_data)
#'
#'@export CIs
#'
CIs <- function(data) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("Package \"dplyr\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Package \"magrittr\" needed for this function to work. Please install it.",
         call. = FALSE)
    }
  
  CI <- CIs <- URdata  <- data_Ci <- data_URs <- URps <- sp_name <- informant <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  #add error stops with validate_that
  assertthat::validate_that("informant" %in% colnames(data), msg = "The required column called \"informant\" is missing from your data. Add it.")
  assertthat::validate_that("sp_name" %in% colnames(data), msg = "The required column called \"sp_name\" is missing from your data. Add it.")
  
  assertthat::validate_that(is.factor(data$informant), msg = "The \"informant\" is not a factor variable. Transform it.")
  assertthat::validate_that(is.factor(data$sp_name), msg = "The \"sp_name\" is not a factor variable. Transform it.")
  
  assertthat::validate_that(all(sum(dplyr::select(data, -informant, -sp_name)>0)) , msg = "The sum of all UR is not greater than zero. Perhaps not all uses have values or are not numeric.")
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  data_complete<-data[stats::complete.cases(data), ]
  #message about complete cases
  assertthat::see_if(length(data_complete) == length(data), msg = "Some of your observations included \"NA\" and were removed. Consider using \"0\" instead.")
  
  URdata<- data #create subset-able data
  
  #calculate URs
  URdata$URps <- dplyr::select(URdata, -informant, -sp_name) %>% rowSums()
    data_URs <- URdata %>% dplyr::group_by(sp_name) %>%
      dplyr::summarize (URs = sum(URps))
    
    #create new subset-able data
    data_Ci <- data_URs
    
    #calcualte CI (UR/N)
    data_Ci$CI <- data_URs$URs/(length(unique(URdata$informant)) *
        ncol(dplyr::select(URdata, -informant, -sp_name)))
    
    #change sort order, arragne and round
    CIs <- data_Ci %>% dplyr::select(-URs) %>%
      dplyr::arrange(-CI) %>%
      dplyr::mutate(CI = round(CI, 4))
    
    print(as.data.frame(CIs))
}
