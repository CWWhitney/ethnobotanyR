#' Cultural consensus with estimated informant competence
#'
#' One-parameter consensus model for binary use data. Each informant has a competence D (0 = guessing, 1 = always agrees with the consensus), estimated from agreement with other informants across all species-use questions. Returns the probability that each species-use is 'used' in the consensus answer key. Fitted by EM (maximum a posteriori; competence has a Laplace smoothing prior).
#'
#' @param data An ethnobotany data set with columns 'informant' and 'sp_name' and one column per use category. Any value above 0 counts as a citation.
#' @param prior_used Prior probability that a species-use is 'used'. Default 0.5.
#' @param max_iter Maximum EM iterations. Default 100.
#' @param tol Convergence tolerance. Default 1e-6.
#'
#' @return A list with 'truth' (data frame: sp_name, use, p_used), 'competence' (data frame: informant, D) and 'converged'.
#'
#' @section Limitations:
#' Assumes one shared answer key and informants of equal knowledge across questions. Minority knowledge, such as healer or gender-specific knowledge, is treated as error. Needs many questions per informant (species x uses) and informants (roughly 10 or more) to estimate competence; with few the results are unstable. Probabilities become extreme as informants are added, so read them as ranks, not as calibrated certainty. Check that informants who disagree are not a different knowledge group before using.
#'
#' @references
#' Oravecz, Z., Vandekerckhove, J., & Batchelder, W. H. (2014). Bayesian Cultural Consensus Theory. Field Methods. \doi{10.1177/1525822X13520280}
#'
#' Romney, A. K., Weller, S. C., & Batchelder, W. H. (1986). Culture as Consensus: A Theory of Culture and Informant Accuracy. American Anthropologist, 88(2), 313-338.
#'
#' @examples
#' ethno_consensus(ethnobotanydata)
#'
#' @export ethno_consensus
ethno_consensus <- function(data, prior_used = 0.5, max_iter = 100, tol = 1e-6) {
  .check_ethno_data(data)
  if (prior_used <= 0 || prior_used >= 1) stop("'prior_used' must be between 0 and 1.")
  uses <- setdiff(names(data), c("informant", "sp_name"))
  inf <- as.character(unique(data$informant))
  sp <- as.character(unique(data$sp_name))
  q <- expand.grid(use = uses, sp_name = sp, stringsAsFactors = FALSE)
  if (length(inf) < 3 || nrow(q) < 2) stop("Need at least 3 informants and 2 species-use questions.")
  x <- matrix(NA_real_, length(inf), nrow(q), dimnames = list(inf, NULL))
  for (j in seq_len(nrow(q))) {
    d <- data[data$sp_name == q$sp_name[j], , drop = FALSE]
    r <- tapply(d[[q$use[j]]] > 0, as.character(d$informant), any) * 1
    x[names(r), j] <- r
  }
  obs <- !is.na(x)
  x0 <- ifelse(obs, x, 0)
  p <- pmin(pmax(colMeans(x, na.rm = TRUE), 0.01), 0.99)
  a <- rep(0.75, length(inf))
  converged <- FALSE
  for (it in seq_len(max_iter)) {
    # M step: P(informant response equals the consensus), floored at chance
    agree <- (x0 * rep(p, each = nrow(x)) + (1 - x0) * rep(1 - p, each = nrow(x))) * obs
    a_new <- pmin(pmax((rowSums(agree) + 1) / (rowSums(obs) + 2), 0.5), 0.999)
    # E step: log odds of 'used'
    lo <- log(prior_used / (1 - prior_used)) +
      colSums(((x0 * 2 - 1) * log(a_new / (1 - a_new))) * obs)
    p_new <- stats::plogis(lo)
    done <- max(abs(p_new - p), abs(a_new - a)) < tol
    p <- p_new; a <- a_new
    if (done) { converged <- TRUE; break }
  }
  if (!converged) warning("EM did not converge; increase 'max_iter'.")
  list(truth = data.frame(sp_name = q$sp_name, use = q$use, p_used = p),
       competence = data.frame(informant = inf, D = 2 * a - 1, row.names = NULL),
       converged = converged)
}
