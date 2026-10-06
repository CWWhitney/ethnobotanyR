#' Gives a measure of the confidence we can have in the answers in the ethnobotany data.
#'
#' Determine the probability that informant citations for a given use are 'correct' given informant responses to the use category for each plant, an estimate of each person's prior_for_answers with this plant and use, and the number of possible answers about this plant use.
#' @usage ethno_bayes_consensus(data, answers = 2, prior_for_answers, prior = -1)
#' @return A matrix of posterior probabilities: rows are the answers 0 to answers - 1 (for binary data 0 = not used, 1 = used), columns are use categories.
#' 
#' @references 
#' Oravecz, Z., Vandekerckhove, J., & Batchelder, W. H. (2014). Bayesian Cultural Consensus Theory. Field Methods, 1525822X13520280. \doi{10.1177/1525822X13520280} 
#' @references 
#' Romney, A. K., Weller, S. C., & Batchelder, W. H. (1986). Culture as Consensus: A Theory of Culture and Informant Accuracy. American Anthropologist, 88(2), 313-338.
#' 
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @param answers The number of possible answers per question. Responses must be whole numbers from 0 to answers - 1: use 2 for 0/1 data, or 11 for counts of 0 to 10. 
#' @param prior_for_answers Informant competence (probability of knowing the answer, 0 to 1): a single value or one value per row of data. Required. Competence is supplied, not estimated.
#' @param prior a prior distribution of probabilities over all answers as a matrix. If this is not provided the function assumes a uniform distribution (prior = -1).
#' 
#' @keywords Bayes Bayesian ethnobotany consensus arith math logic methods misc survey
#' 
#' @return A matrix, where columns represent plant use categories and rows represent responses per person and plant (matching the data). Each value represents the bayes_consensus that an answer was 'correct' for a particular use, within the cultural consensus framework.
#' 
#' @section Warning:
#' 
#' Identification for informants and species must be listed by the names 'informant' and 'sp_name' respectively in the data set.
#' The rest of the columns should all represent separate identified ethnobotany use categories. These data should be populated with counts of uses per informant (should be 0 or 1 values).
#' 
#' @section Application:
#' 
#' ethnobotanyR users often have a large number of counts in cells of the data set after categorization (i.e one user cites ten different ‘food’ uses but this is just one category). 
#' Most quantitative ethnobotany tools are not equipped for cases where the theoretical maximum number of use reports in one category, for one species by one informant is >1. 
#' This function and the bayes_boot function may be useful to work with these richer datasets for the Bayes consensus analysis.
#' 
#' @importFrom dplyr filter summarize select left_join group_by 
#' @importFrom ggridges geom_density_ridges theme_ridges
#' 
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' #assign a non-informative prior to prior_for_answers with 'prior_for_answers=0.5'
#' ethno_bayes_consensus(ethnobotanydata, answers = 2, prior_for_answers = 0.5, prior = -1)
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' #assign a non-informative prior to prior_for_answers
#' eb_prior_for_answers <- rep(0.5, len = nrow(eb_data))
#' 
#' ethno_bayes_consensus(eb_data, answers = 2, prior_for_answers = eb_prior_for_answers)
#' 
#' @export ethno_bayes_consensus
#' 
ethno_bayes_consensus <-
  function(data, answers = 2, prior_for_answers, prior = -1){

    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("Package \"dplyr\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    if (any(is.na(data))) {
      warning("Some of your observations included \"NA\" and were removed. Consider using \"0\" instead.")
      data <- data[stats::complete.cases(data), ]
    }

    bayesdata <- as.matrix(dplyr::select(data, -informant, -sp_name))

    if (!is.numeric(bayesdata) || any(bayesdata != round(bayesdata)) ||
        any(bayesdata < 0) || any(bayesdata > answers - 1)) {
      stop("Responses must be whole numbers from 0 to answers - 1 (e.g. 0/1 for answers = 2, 0:10 for answers = 11).")
    }
    if (sum(bayesdata) == 0) {
      warning("The sum of all UR is not greater than zero. Perhaps not all uses have values or are not numeric.")
    }

    if (missing(prior_for_answers)) {
      stop("'prior_for_answers' (informant competence, 0 to 1) is required: one value, or one per row of data.")
    }
    if (!length(prior_for_answers) %in% c(1, nrow(bayesdata))) {
      stop("'prior_for_answers' must have length 1 or one value per row of data.")
    }
    if (any(prior_for_answers < 0 | prior_for_answers > 1)) {
      stop("'prior_for_answers' must be between 0 and 1.")
    }
    competence <- rep_len(prior_for_answers, nrow(bayesdata))

    if (is.matrix(prior)) {
      if (ncol(prior) != ncol(bayesdata) || nrow(prior) != answers) {
        stop("Something is wrong with the prior. It may have a different number of rows or columns than the data.")
      }
      if (any(prior < 0)) {
        stop("For this to work your prior needs to assign non-negative probability to all possible outcomes.")
      }
      if (!all(abs(colSums(prior) - 1) < 0.001)) {
        warning("Your prior for every question should add up to 1.")
      }
    } else if (identical(as.numeric(prior), -1)) {
      prior <- matrix(1 / answers, answers, ncol(bayesdata))
    } else {
      stop("'prior' must be -1 (uniform) or a matrix with one row per answer and one column per use.")
    }

    # Answer categories are the values 0, 1, ..., answers - 1
    # Likelihood of a response given the true answer k (Batchelder & Romney 1988):
    # P(match) = D + (1 - D) / L; P(any one specific wrong answer) = (1 - D) / L
    p_match <- competence + (1 - competence) / answers
    p_wrong <- (1 - competence) / answers

    bayes_consensus <- matrix(0, answers, ncol(bayesdata),
                              dimnames = list(0:(answers - 1), colnames(bayesdata)))
    for (use in seq_len(ncol(bayesdata))) {
      for (k in seq_len(answers)) {
        loglik <- sum(log(ifelse(bayesdata[, use] == k - 1, p_match, p_wrong)))
        bayes_consensus[k, use] <- loglik + log(prior[k, use])
      }
      lp <- bayes_consensus[, use]
      bayes_consensus[, use] <- exp(lp - max(lp)) / sum(exp(lp - max(lp)))
    }
    bayes_consensus
  }
