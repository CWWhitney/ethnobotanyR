#' Chord diagram of ethnobotany uses and species
#'
#' Creates a simple chord diagram of species and uses for ethnobotany studies. For more on the circlize package see Zuguang Gu's 'Circular Visualization in R' <https://jokergoo.github.io/circlize_book/book/>
#' @source Whitney, C. W., Bahati, J., and Gebauer, J. (2018), Ethnobotany and agrobiodiversity; valuation of plants in the homegardens of southwestern Uganda. Ethnobiology Letters, 9(2), 90-100. <https://doi.org/10.14237/ebl.9.2.2018.503>
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @param by indicates the variable that should be mapped to the bottom of the chord diagram. This automatically defaults to the column referring to the species (by = "sp_name")
#' 
#' @keywords graphs arith math logic methods misc survey
#' 
#' @return Chord diagram figure for each use by 'informant' (top half) related to each 'sp_name' (bottom half) in the data set. 
#' To change variable names try using the dplyr rename function.
#' 
#' @section Warning:
#' 
#' Identification for informants and species must be listed by the names 'informant' and 'sp_name' respectively in the data set.
#' The rest of the columns represent the identified ethnobotany use categories. These data should be populated with counts of uses per person (should be 0 or 1 values).
#' 
#' @importFrom circlize chordDiagram circos.text get.cell.meta.data
#' @importFrom dplyr filter rename select 
#' @importFrom graphics strwidth
#' @importFrom magrittr %>%
#' @importFrom reshape melt
#'
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' ethnoChord(ethnobotanydata)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' ethnoChord(eb_data)
#' 
#' @export ethnoChord
#' 
ethnoChord <- function(data, by = "sp_name") {
  
  #Add error stops ####
  #Check that packages are loaded
    {
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
    }# end package loading check
  
  # Use 'complete.cases' from stats to get to the collection of obs without NA
  
    if (any(is.na(data))) {
      warning("Some of your observations included \"NA\" and were removed. Consider using \"0\" instead.")
    data<-data[stats::complete.cases(data), ]
    }# end error stops
  
  # Set the variables to NULL first, appeasing R CMD check
  informant <- sp_name <- variable <- value <- strwidth <- NULL 
  
  # Melt ethnobotany data
  mat <- reshape::melt(data, id=c("informant","sp_name")) %>% 
    dplyr::filter(value >=1)%>%
   dplyr::arrange(dplyr::desc(informant)) %>%  
    dplyr::arrange(dplyr::desc(sp_name)) %>% 
    dplyr::select(by, variable)
  
  # Create chord plot ####
  
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

}
