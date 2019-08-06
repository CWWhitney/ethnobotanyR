#' Use Value (UV) index per species
#'
#' Calculates the use value (UV) index for each species in the data set (see Tardio and Pardo-de-Santayana 2008).
#' @source Tardio, Javier, and Manuel Pardo-de-Santayana. “Cultural Importance Indices: A Comparative Analysis Based on the Useful Wild Plants of Southern Cantabria (Northern Spain)1.” Economic Botany 62, no. 1 (May 2008): 24–39. <https://doi.org/10.1007/s12231-007-9004-5>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @keywords quantitative ethnobotany cultural importance use value
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
  
  URdata <- sp_name <- informant <- UVps <- UV <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  #add error stops with validate_that
  assertthat::validate_that("informant" %in% colnames(data), msg = "The required column called \"informant\" is missing from your data. Add it.")
  assertthat::validate_that("sp_name" %in% colnames(data), msg = "The required column called \"sp_name\" is missing from your data. Add it.")
  
  assertthat::validate_that(is.factor(data$informant), msg = "The \"informant\" is not a factor variable. Transform it.")
  assertthat::validate_that(is.factor(data$sp_name), msg = "The \"sp_name\" is not a factor variable. Transform it.")
  
  assertthat::validate_that(all(sum(dplyr::select(data, -informant, -sp_name)>0)) , msg = "The sum of all UR is not greater than zero. Perhaps not all uses have values or are not numeric.")
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  data_complete<-data[stats::complete.cases(data), ]
  
  URdata <- data_complete #create complete subset-able data
  
  #message about complete cases
  assertthat::see_if(length(data_complete) == length(data), msg = "Some of your observations included \"NA\" and were removed. Consider using \"0\" instead.")
  
  #Use-Value (UV) calculated after Tardio and Pardo-de-Santayana (2008)
  
  data_URs <- URs(URdata) #calculate URs()
  
  data_UV <- data_URs #create new subset-able data for UVs
  
  #calcualte UV
  data_UV$UV <- data_URs$URs/sum(dplyr::count_(URdata, vars=informant)) *
                ncol(dplyr::select(URdata, -informant, -sp_name))
  
  #UV differs from Ci (the CIs function) only in that 
  #it sums UR grouping by informant (the sum of the uses cited by each informant) 
  #then sums all these data 
  
  #change sort order, arrange and round
  UVs <- data_UV %>% dplyr::select(-URs) %>%
    dplyr::arrange(-UV) %>%
    dplyr::mutate(UV = round(UV, 3))
    
    as.data.frame(UVs)
}
