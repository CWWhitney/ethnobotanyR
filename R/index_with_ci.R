#' Calculate confidence intervals for species citation proportions
#'
#' Calculates the citation proportion and 95% confidence interval for each species in an ethnobotany data set.
#'
#' @usage index_with_ci(data, conf.level = 0.95)
#'
#' @param data A data frame with columns 'informant', 'sp_name', and use categories (numeric, 0/1).
#' @param conf.level Confidence level for the interval (default 0.95).
#'
#' @importFrom dplyr select all_of group_by summarise arrange mutate pick n_distinct
#' @importFrom stats prop.test complete.cases
#' @importFrom magrittr %>%
#' @return Data frame with columns: sp_name, citations, proportion, lower, upper.
#'
#' @note
#' The confidence interval (CI) shows the range where the true proportion of informants citing each species likely falls, given your sample. If CIs overlap between species, differences are not statistically reliable. Wide intervals mean less certainty; overlapping intervals mean no clear difference between species.
#'
#' @examples
#' # Use built-in ethnobotany data example
#' index_with_ci(ethnobotanydata)
#'
#' # Generate random dataset of three informants uses for four species
#' eb_data <- data.frame(replicate(10, sample(0:1, 20, rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' index_with_ci(eb_data)
#'
#' @export
index_with_ci <- function(data, conf.level = 0.95) {
      # User-friendly warnings
      use_cols <- setdiff(names(data), c("informant", "sp_name"))
      if (!all(c("informant", "sp_name") %in% names(data))) {
        warning("Data must include columns named 'informant' and 'sp_name'.")
        return(data.frame())
      }
      if (length(use_cols) == 0) {
        warning("No use category columns found. Data must include at least one use category column.")
        return(data.frame())
      }
      # Convert only use category columns to numeric if needed
      if (!all(sapply(data[use_cols], is.numeric))) {
        warning("Some use category columns are not numeric (may be factors or characters). Converting only use category columns to numeric.")
        data[use_cols] <- lapply(data[use_cols], function(x) as.numeric(as.character(x)))
      }
      if (!any(sum(dplyr::select(data, all_of(use_cols)) > 0))) {
        warning("No use category values greater than zero. Check your data for valid use reports.")
        return(data.frame())
      }
      if (any(is.na(data))) {
        warning("Some observations include NA and were removed. Consider using 0 instead.")
        data <- data[stats::complete.cases(data), ]
      }
      if (any(length(dplyr::select(data, all_of(use_cols))[dplyr::select(data, all_of(use_cols)) > 1]) > 0)) {
        warning("Some use category values are greater than 1. All non-zero numeric values have been changed to 1.")
        data <- dplyr::mutate_if(data, is.numeric, ~1 * (. != 0))
      }
    ## Check that values are '1' or '0'
    use_cols <- setdiff(names(data), c("informant", "sp_name"))
    if (any(length(dplyr::select(data, all_of(use_cols))[dplyr::select(data, all_of(use_cols)) > 1]) > 0)) {
      warning("Some of your use category data includes values greater than 1. All the non-zero numeric values have been changed to 1.")
      data <- dplyr::mutate_if(data, is.numeric, ~1 * (. != 0))
    }
  # Add error stops ####
  # Check that packages are loaded
  {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("Package \"dplyr\" needed for this function to work. Please install it.", call. = FALSE)
    }
    if (!requireNamespace("magrittr", quietly = TRUE)) {
      stop("Package \"magrittr\" needed for this function to work. Please install it.", call. = FALSE)
    }
  } # end package loading check

  # Clean data: remove NAs, check use categories
  if (!any(sum(dplyr::select(data, -informant, -sp_name) > 0))) {
    warning("The sum of all use categories is not greater than zero. Perhaps not all uses have values or are not numeric.")
    data <- data[stats::complete.cases(data), ]
  }
  if (any(is.na(data))) {
    warning("Some of your observations included NA and were removed. Consider using 0 instead.")
    data <- data[stats::complete.cases(data), ]
  }

  # Calculate citation proportion per species
  use_cols <- setdiff(names(data), c("informant", "sp_name"))
  n_informants <- length(unique(data$informant))
  grouped <- dplyr::group_by(data, sp_name)
  citation_counts <- dplyr::summarise(
    grouped,
    informant_count = n_distinct(informant[rowSums(pick(all_of(use_cols)) > 0)]),
    proportion = informant_count / n_informants
  )

  # Calculate confidence intervals (suppress warnings)
  citation_counts$lower <- NA
  citation_counts$upper <- NA
  for (i in seq_len(nrow(citation_counts))) {
    res <- suppressWarnings(prop.test(citation_counts$informant_count[i], n_informants, conf.level = conf.level))
    citation_counts$lower[i] <- res$conf.int[1]
    citation_counts$upper[i] <- res$conf.int[2]
  }

  # Arrange and round output
  citation_counts <- citation_counts %>%
    dplyr::select(sp_name, informant_count, proportion, lower, upper) %>%
    dplyr::arrange(-proportion) %>%
    dplyr::mutate(
      proportion = round(proportion, 3),
      lower = round(lower, 3),
      upper = round(upper, 3)
    )

  colnames(citation_counts) <- c("Species", "Citing_Informants", "Proportion", "CI_Lower", "CI_Upper")
  as.data.frame(citation_counts)
}
