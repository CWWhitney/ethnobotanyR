#' Beta-binomial probability of use
#'
#' Posterior probability that a random informant cites a use for a species, with a credible interval. Valid for small samples and rare uses, where the bootstrap in \code{ethno_boot} fails.
#'
#' @param data An ethnobotany data set with columns 'informant' and 'sp_name' and one column per use category. Any value above 0 counts as a citation.
#' @param level Credible interval level. Default 0.9.
#' @param prior Beta prior, c(a, b). Default c(1, 1) is uniform.
#'
#' @return A data frame with one row per species and use: 'k' informants citing, 'n' informants, posterior 'mean', 'lower' and 'upper'.
#'
#' @section Limitations:
#' Informants are treated as independent and as a random sample of the community. This is the share of informants citing a use, not the Use Value, which can exceed 1.
#'
#' @examples
#' ethno_beta(ethnobotanydata)
#'
#' @export ethno_beta
ethno_beta <- function(data, level = 0.9, prior = c(1, 1)) {
  .check_ethno_data(data)
  if (level <= 0 || level >= 1) stop("'level' must be between 0 and 1.")
  if (length(prior) != 2 || any(prior <= 0)) stop("'prior' must be two positive numbers, c(a, b).")
  uses <- setdiff(names(data), c("informant", "sp_name"))
  out <- lapply(split(data, data$sp_name, drop = TRUE), function(d) {
    cited <- rowsum(as.matrix(d[uses] > 0) * 1, as.character(d$informant)) > 0
    k <- colSums(cited)
    n <- nrow(cited)
    a <- k + prior[1]
    b <- n - k + prior[2]
    data.frame(sp_name = d$sp_name[1], use = uses, k = as.numeric(k), n = n,
               mean = a / (a + b),
               lower = stats::qbeta((1 - level) / 2, a, b),
               upper = stats::qbeta(1 - (1 - level) / 2, a, b),
               row.names = NULL)
  })
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

# Shared input check for ethnobotany data frames
.check_ethno_data <- function(data) {
  if (!is.data.frame(data) || !all(c("informant", "sp_name") %in% names(data))) {
    stop("'data' must be a data frame with columns 'informant' and 'sp_name'.")
  }
  uses <- setdiff(names(data), c("informant", "sp_name"))
  if (length(uses) == 0 || !all(vapply(data[uses], is.numeric, logical(1)))) {
    stop("All columns other than 'informant' and 'sp_name' must be numeric use categories.")
  }
  if (anyNA(data)) stop("Data contain NA. Recode missing values or remove those rows.")
  invisible(TRUE)
}
