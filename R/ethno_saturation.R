#' Informant saturation curve
#'
#' Number of distinct species-use citations as informants are added in random order. A curve that is still rising means more interviews would still find new uses, so use probabilities and rankings are likely to change.
#'
#' @param data An ethnobotany data set with columns 'informant' and 'sp_name' and one column per use category. Any value above 0 counts as a citation.
#' @param n_perm Number of random informant orderings. Default 200.
#' @param level Interval level across orderings. Default 0.9.
#'
#' @return A data frame with 'n_informants', and the 'mean', 'lower' and 'upper' number of distinct species-use pairs.
#'
#' @examples
#' sat <- ethno_saturation(ethnobotanydata)
#' plot(sat$n_informants, sat$mean, type = "l",
#'      xlab = "Informants", ylab = "Distinct species-use pairs")
#'
#' @export ethno_saturation
ethno_saturation <- function(data, n_perm = 200, level = 0.9) {
  .check_ethno_data(data)
  uses <- setdiff(names(data), c("informant", "sp_name"))
  pair <- paste(data$sp_name, rep(uses, each = nrow(data)), sep = "__")
  long <- data.frame(informant = as.character(data$informant),
                     pair = pair,
                     cited = as.vector(as.matrix(data[uses])) > 0)
  m <- rowsum(as.numeric(long$cited), paste(long$informant, long$pair, sep = "||")) > 0
  keys <- do.call(rbind, strsplit(rownames(m), "||", fixed = TRUE))
  inf <- unique(keys[, 1])
  prs <- unique(keys[, 2])
  mat <- matrix(FALSE, length(inf), length(prs), dimnames = list(inf, prs))
  mat[cbind(match(keys[, 1], inf), match(keys[, 2], prs))] <- m[, 1]
  curves <- replicate(n_perm, {
    sub <- mat[sample(nrow(mat)), , drop = FALSE]
    rowSums(apply(sub, 2, cummax))
  })
  if (is.null(dim(curves))) curves <- matrix(curves, nrow = 1)
  data.frame(n_informants = seq_len(nrow(mat)),
             mean = rowMeans(curves),
             lower = apply(curves, 1, stats::quantile, (1 - level) / 2),
             upper = apply(curves, 1, stats::quantile, 1 - (1 - level) / 2),
             row.names = NULL)
}
