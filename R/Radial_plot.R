#' Radial bar plot of use reports (UR) per species
#'
#' Creates a radial bar plot of use reports (UR) per species based on the `UR function`.
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @param analysis is one of the quantitative ethnobotany functions from ethnobotanyR, i.e. ethnobotanyR::FCs.
#' @keywords ethnobotany, cultural value, use report
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter summarize select left_join group_by 
#' @importFrom assertthat validate_that see_if
#' @importFrom ggplot2 ggplot aes geom_bar coord_polar theme_minimal geom_bar scale_y_continuous
#'  
#' @examples
#' 
#' #Use built-in ethnobotany data example and Frequency of Citation function FCs()
#' Radial_plot(ethnobotanydata, analysis = FCs)
#' 
#' #Generate random dataset of three informants uses for four species
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant<-sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name<-sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' Radial_plot(data = eb_data, analysis = FCs)
#' 
#' @export Radial_plot
Radial_plot <- function(data, analysis) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Package \"magrittr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  value <-  meltURdata <- URdata <- URs <- sp_name <- informant <- URps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
 
  Radial_plot_data <- analysis(data) #create subset-able data
  
  names(Radial_plot_data)[length(names(Radial_plot_data))]<-"value" 
  
  Radial_plot <- 
    ggplot2::ggplot(Radial_plot_data, ggplot2::aes(x = sp_name, y = value, fill = sp_name)) +
    ggplot2::geom_bar(width = 1, stat = "identity", color = "white") +
    ggplot2::scale_y_continuous(breaks = 0:nlevels(Radial_plot_data$sp_name), position = "right") +
    ggplot2::coord_polar() + 
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.x=ggplot2::element_blank())+
    ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank())+
    ggplot2::geom_text(ggplot2::aes(label=value), position=ggplot2::position_dodge(width=0.9), vjust=-0.25)+
    ggplot2::theme(legend.position = "none") 
  
  print(Radial_plot)
}
