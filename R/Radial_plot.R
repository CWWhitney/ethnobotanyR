#' Radial bar plot of ethnobotany indices
#'
#' Creates a radial bar plot of ethnobotany indices in ethnobotanyR package using the ggplot2 library \url{https://ggplot2.tidyverse.org/}.
#' @references 
#' Wickham, Hadley. “Reshaping Data with the Reshape Package.” Journal of Statistical Software 21, no. 12 (2007): 1–20.
#' @references 
#' Wickham, Hadley. ggplot2: Elegant Graphics for Data Analysis. Springer, 2016.
#' 
#' @usage Radial_plot(data, analysis)
#' 
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @param analysis is one of the quantitative ethnobotany functions from ethnobotanyR, i.e. ethnobotanyR::FCs.
#' 
#' @keywords graphs arith math logic methods misc survey
#'
#' @return Radial bar plot of chosen ethnobotany indices in ethnobotanyR package.
#' 
#' @section Warning:
#' 
#' Identification for informants and species must be listed by the names 'informant' and 'sp_name' respectively in the data set.
#' The rest of the columns should all represent separate identified ethnobotany use categories. These data should be populated with counts of uses per informant (should be 0 or 1 values).
#' 
#' @importFrom cowplot plot_grid
#' @importFrom dplyr filter summarize select left_join group_by 
#' @importFrom ggplot2 ggplot aes geom_bar coord_polar theme_minimal geom_bar scale_y_continuous
#' @importFrom magrittr %>%
#'  
#' @examples
#' 
#' #Use built-in ethnobotany data example and Frequency of Citation function FCs()
#' 
#' Radial_plot(ethnobotanydata, analysis = FCs)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#'  
#' Radial_plot(data = eb_data, analysis = URs)
#' 
#' @export Radial_plot
#' 
Radial_plot <- function(data, analysis) {
  
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
    }#end error stops
  
  # Set the variables to NULL first, appeasing R CMD check
  value <-  meltURdata <- URdata <- URs <- sp_name <- informant <- URps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
  Radial_plot
}
