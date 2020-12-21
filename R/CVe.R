#' Cultural Value of ethnospecies (CVe)
#'
#' Calculates the Cultural Value (CVe) per ethnospecies.
#' @usage CVe(data)
#'  
#' @references 
#' Reyes-Garcia, V., T. Huanca, V. Vadez, and W. Leonard. 2006. “Cultural, Practical, and Economic Value of Wild Plants: A Quantitative Study in the Bolivian Amazon.” Economic Botany.
#' 
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of ethnospecies names respectively.
#' 
#' @keywords arith math logic methods misc survey
#' 
#' @return Data frame of ethnospecies and Cultural Value (CVe) values.
#'
#' @section Warning:
#' 
#' Identification for informants and ethnospecies must be listed by the names 'informant' and 'sp_name' respectively in the data set.
#' The rest of the columns should all represent separate identified ethnobotany use categories. These data should be populated with counts of uses per informant (should be 0 or 1 values).
#' 
#' @importFrom dplyr filter select group_by arrange bind_cols mutate
#' @importFrom magrittr %>%
#' 
#' 
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' CVe(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' CVe(eb_data)
#'
#'@export CVe
#'
CVe <- function(data) {
  
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
  CVe <- FCs <- UR_UN_FC <- CVe <- URdata  <- data_Ci <- data_URs <- URps <- sp_name <- informant <- NULL 
  
  URdata <- data #create complete subset-able data
  
  # calculate Use Reports per ethnospecies 
  # with function URs()
  URS <- URs(URdata)
  
  # Uce is the total number of uses reported for ethnospecies e 
  # with function NUs()
  NUS <- NUs(URdata)
  # divided by the potential uses of an ethnospecies in the study
  NUS$Uce <- NUS$NUs/ncol(dplyr::select(URdata, -informant, -sp_name))
  
  # Ice expresses the number of participants who 
  # listed the ethnospecies e as useful
  # with function FCs()
  FCS <- FCs(URdata)
  
  # divided by the total number of people (n) 
  FCS$Ice <- FCS$FCs/length(unique(URdata$informant))
  
  # bind the three data sets 
  UR_UN_FC <- dplyr::left_join(NUS, URS) %>% dplyr::left_join(FCS)
  
  # IUce expresses the number of participants who mentioned 
  # each use of the ethnospecies e (also URs)
  # divided by the total number of participants 
  UR_UN_FC$IUce <- UR_UN_FC$URs/length(unique(URdata$informant))
  
  # calculate CVe = Uce * Ice*EIUce
  UR_UN_FC$CVe <- UR_UN_FC$Uce * UR_UN_FC$Ice * UR_UN_FC$IUce
  
  CVe <- dplyr::select(UR_UN_FC, sp_name, CVe) %>%
    dplyr::arrange(-CVe) %>%
    dplyr::mutate(CVe = round(CVe, 3))
  
  as.data.frame(CVe)
}
