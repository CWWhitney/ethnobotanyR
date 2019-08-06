#' Number of Uses (NU)
#'
#' Calculates the number of uses (NU) per species.
#' @source Prance, G. T., W. Baleé, B. M. Boom, and R. L. Carneiro. “Quantitative Ethnobotany and the Case for Conservation in Amazonia.” Conservation Biology 1, no. 4 (1987): 296–310.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany number of uses
#'
#' @importFrom stats aggregate
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' @importFrom dplyr select
#' 
#' 
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' NUs(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant<-sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name<-sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' NUs(eb_data)
#' 
#'@export NUs
NUs <- function(data) {
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

  NUdata <- NUdataaggr <- NUs <- informant <- sp_name <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
  NUdata <- data_complete #create complete subset-able data
  
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
