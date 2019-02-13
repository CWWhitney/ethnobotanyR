#' #Relative Importance Index (RI)
#'
#' This function allows you to calculate the relative importance index (RI) per species, published by Pardo-de-Santayana (2003).
#' @source Tardio, J., and M. Pardo-de-Santayana, 2008. Cultural Importance Indices: A Comparative Analysis Based on the Useful Wild Plants of Southern Cantabria (Northern Spain) 1. Economic Botany, 62(1), 24-39. <https://doi.org/10.1007/s12231-007-9004-5>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany, relative importance
#'
#' @importFrom magrittr %>%
#' @importFrom stats aggregate
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' @importFrom dplyr filter summarize select left_join group_by
#' 
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' RIs(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant<-sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name<-sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' RIs(eb_data)
#' 
#' @export RIs
RIs <- function(data) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("Package \"dplyr\" needed for this function to work. Please install it.",
            call. = FALSE)
  }
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Package \"magrittr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  FCps <- RFCstestdata <- informant <- sp_name <- RFCstestdata2 <- RNUs <- RNUsdataaggr <- RNUstestdata <- RFCs <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
 
  #create subsettable data for RFCs
  RFCdata <- data
  
  RFCdata$FCps <- rowSums(dplyr::select(RFCdata, -informant, -sp_name) > 0)
  #all UR greater than zero to count of '1' FC
  RFCdata <- RFCdata %>% dplyr::mutate_if(is.numeric, ~1 * (. != 0))
  
  #calculate and creat data set of RFCs
  RFCs <- RFCdata %>% dplyr::group_by(sp_name) %>%
    dplyr::summarize(RFCs = sum(FCps/(length(unique(informant))))) %>%
    dplyr::arrange(-RFCs) 
  
    #create subsettable data for RNUs
    RNUstestdata <- data
    
    #calculate RNUs
    RNUsdataaggr <- stats::aggregate(dplyr::select(RNUstestdata, -informant, -sp_name), 
                                     by = list(sp_name = RNUstestdata$sp_name),
                                     FUN = sum)
    
    #all UR greater than zero to count of '1' FC
    RNUsdataaggr <- RNUsdataaggr %>% dplyr::mutate_if(is.numeric, ~1 * (. != 0))
    
    #counts of all use per specise (NUs)
    RNUsdataaggr$NUs <- RNUsdataaggr %>% dplyr::select(-sp_name) %>% rowSums() 
    
    #divide uses per species by max uses
    RNUsdataaggr$RNUs <- RNUsdataaggr$NUs/max(RNUsdataaggr$NUs)
    
    RNUs <- dplyr::select(RNUsdataaggr, sp_name, RNUs)

    #merge RNUs and RFCs    
    RIs <- merge(RNUs, RFCs, by = "sp_name")
    RIs$RIs <- (RIs$RNUs + RIs$RFCs)/2
    
    #change sort order
    RIs <- dplyr::arrange(RIs, -RIs) %>% 
      dplyr::select(sp_name, RIs) %>%
      dplyr::mutate(RIs = round(RIs, 3))

    print(as.data.frame(RIs))
}
