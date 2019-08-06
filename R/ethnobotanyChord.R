#' Chord diagram of ethnobotany uses and species
#'
#' Creates a chord diagram of species and uses for ethnobotany studies.
#' @source Whitney, C. W., Bahati, J., and Gebauer, J. (2018), Ethnobotany and agrobiodiversity; valuation of plants in the homegardens of southwestern Uganda. Ethnobiology Letters, 9(2), 90-100. <https://doi.org/10.14237/ebl.9.2.2018.503>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @importFrom magrittr %>%
#' @importFrom reshape melt
#' @importFrom dplyr filter select 
#' @importFrom circlize chordDiagram
#' @importFrom circlize circos.text 
#' @importFrom circlize get.cell.meta.data
#' @importFrom graphics strwidth
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' 
#' @keywords ethnobotany
#'
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' ethnobotanyChord(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant<-sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name<-sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' ethnobotanyChord(eb_data)
#' 
#' @export ethnobotanyChord
ethnobotanyChord <- function(data) {
    if (!requireNamespace("reshape", quietly = TRUE)) {
        stop("Package \"reshape\" needed for this function to work. Please install it.",
            call. = FALSE)
    }
   if (!requireNamespace("circlize", quietly = TRUE)) {
     stop("Package \"circlize\" needed for this function to work. Please install it.",
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
  
  sp_name <- informant <- value <- strwidth <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
  #Melt ethnobotany data
  mat <- reshape::melt(data, id=c("informant","sp_name")) %>% dplyr::filter(value >=1)%>%
   dplyr::arrange(dplyr::desc(informant)) %>%  dplyr::arrange(dplyr::desc(sp_name)) %>% dplyr::select(2:3)
  
  #Create chord plot
  
  circlize::chordDiagram(mat, annotationTrack = "grid", 
                         preAllocateTracks = list(track.height = max(graphics::strwidth(unlist(dimnames(mat))))))
  circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
    xlim = circlize::get.cell.meta.data("xlim")
    xplot = circlize::get.cell.meta.data("xplot")
    ylim = circlize::get.cell.meta.data("ylim")
    sector.name = circlize::get.cell.meta.data("sector.index")
    
    if(abs(xplot[2] - xplot[1]) < 20) {
      circlize::circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                            niceFacing = TRUE, adj = c(0, 0.5), col = "black")
    } else {
      circlize::circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                            niceFacing = TRUE, adj = c(0, 0.5), col = "black")
    }
  }, bg.border = NA)
  
    print("Chord diagram for each use (top half) related to each species (bottom half) in the data set")
}
