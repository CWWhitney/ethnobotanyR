#' Cultural Value of ethnospecies (CVe)
#'
#' Calculates the Cultural Value (CVe) per ethnospecies.
#' @source Reyes-Garcia, V., T. Huanca, V. Vadez, and W. Leonard. “Cultural, Practical, and Economic Value of Wild Plants: A Quantitative Study in the Bolivian Amazon.” Economic Botany, 2006. <https://doi.org/10.2307/4257061>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' @keywords quantitative ethnobotany cultural importance
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter summarize select left_join group_by arrange count_ bind_cols
#' @importFrom assertthat validate_that see_if
#' 
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' CVe(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant<-sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name<-sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' CVe(eb_data)
#'
#'@export CVe
#'
CVe <- function(data) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Package \"magrittr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  CVe <- FCs <- UR_UN_FC <- CVe <- URdata  <- data_Ci <- data_URs <- URps <- sp_name <- informant <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
  URdata <- data_complete #create complete subset-able data
  
  #calcualte Use Reports per species 
  #same as function URs()
  URS <- URs(URdata)
  
  #Uce is the total number of uses reported for ethnospecies e 
  #same as function NUs()
  NUS<-NUs(URdata)
  #divided by the potential uses of an ethnospecies in the study
  NUS$Uce <- NUS$NUs/ncol(dplyr::select(URdata, -informant, -sp_name))
  
  #Ice expresses the number of participants who 
  #listed the ethnospecies e as useful
  #Same as function FCs()
  FCS<-FCs(URdata)
  
  #divided by the total number of people (n) 
  FCS$Ice <- FCS$FCs/sum(dplyr::count_(URdata, vars=informant))
  
  #bind the three data sets 
  UR_UN_FC <- dplyr::bind_cols(NUS, URS, FCS)
  
  #IUce expresses the number of participants who mentioned 
  #each use of the ethnospecies e (also URs)
  #divided by the total number of participants 
  UR_UN_FC$IUce <- UR_UN_FC$URs/sum(dplyr::count_(URdata, vars=informant))
  
  #calcualte CVe = Uce * Ice*EIUce
  UR_UN_FC$CVe <- UR_UN_FC$Uce * UR_UN_FC$Ice * sum(UR_UN_FC$IUce)
    
  CVe <- dplyr::select(UR_UN_FC, sp_name, CVe) %>%
    dplyr::arrange(-CVe) 
  
  as.data.frame(CVe)
}
