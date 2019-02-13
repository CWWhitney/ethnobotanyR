#' Use Value (UV) index per species
#'
#' Allows users to calculate the use value (UV) index for each species in the data set (see Albuquerque et al. 2006).
#' @source Albuquerque, U. P., R. F. P. Lucena, J. M. Monteiro, A. T. N. Florentino, and C. F. C. B. R. Almeida. 2006. Evaluating Two Quantitative Ethnobotanical Techniques. Ethnobotany Research and Applications 4:51–60. <http://hdl.handle.net/10125/237>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, cultural importance
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter summarize select left_join group_by 
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#'
#' @examples
#'
#' #Use built-in ethnobotany data example
#' UVs(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant<-sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name<-sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' UVs(eb_data)
#' 
#'@export UVs
UVs <- function(data) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("Package \"dplyr\" needed for this function to work. Please install it.", 
            call. = FALSE)
    }
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Package \"magrittr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  UVpsdata <- sp_name <- informant <- UVps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
  #create subsettable data
  UVpsdata <- data
  
  UVpsdata$UVps <- rowSums(dplyr::select(UVpsdata, -informant, -sp_name) > 0)
    UVs <- UVpsdata %>% dplyr::group_by(sp_name) %>% 
      dplyr::summarize (UVs = sum(UVps)/(length(unique(informant)))) %>%
      dplyr::arrange(-UVs)
    
    print(as.data.frame(UVs))
}
