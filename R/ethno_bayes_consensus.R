#' Gives a measure of the confidence we can have in the answers in the ethnobotany data.
#'
#' Determine the probability that informant citations for a given use are 'correct' given informant responses to the use category for each plant, an estimate of each person's prior_for_answers with this plant and use, and the number of possible answers about this plant use.
#' @usage ethno_bayes_consensus(data, answers = 2, prior_for_answers, prior=-1)
#' 
#' @references 
#' Oravecz, Z., Vandekerckhove, J., & Batchelder, W. H. (2014). Bayesian Cultural Consensus Theory. Field Methods, 1525822X13520280. http://doi.org/10.1177/1525822X13520280 
#' @references 
#' Romney, A. K., Weller, S. C., & Batchelder, W. H. (1986). Culture as Consensus: A Theory of Culture and Informant Accuracy. American Anthropologist, 88(2), 313-338.
#' @references 
#' Alastair Jamieson Lane and Benjamin Grant Purzycki (2016), AnthroTools: Some custom tools for anthropology.
#' 
#' @param data is an ethnobotany data set with column 1 'informant' and 2 'sp_name' as row identifiers of informants and of species names respectively.
#' The rest of the columns are the identified ethnobotany use categories. The data should be populated with counts of uses per person (should be 0 or 1 values).
#' @param answers The number of answers available for each question. These are set to '2' because the way the package works at the moment users can have either a '1' or a '0' in the table. It is however, possible to have more. 
#' @param prior_for_answers A matrix representing the probability of a given use being more likely. If not provided the function assumes a uniform distribution across uses.
#' @param prior a prior distribution of probabilities over all answers as a matrix. If this is not provided the function assumes a uniform distribution (prior = -1).
#' 
#' @keywords bayes bayesian ethnobotany consensus arith math logic methods misc survey
#' 
#' @return A matrix, where columns represent plant use categories and rows represent responses per person and plant (matching the data). Each value represents the bayes_consensus that an answer was 'correct' for a particular use, within the cultural consensus framework.
#' 
#' @section Warning:
#' 
#' Identification for informants and species must be listed by the names 'informant' and 'sp_name' respectively in the data set.
#' The rest of the columns should all represent separate identified ethnobotany use categories. These data should be populated with counts of uses per informant (should be 0 or 1 values).
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
#' ethno_bayes_consensus(eb_data, answers = 5, prior_for_answers = eb_prior_for_answers)
#' 
#' @export ethno_bayes_consensus
#' 
ethno_bayes_consensus <-
  function(data, answers = 2, prior_for_answers, prior=-1){
    
    #Add error stops ####
    #Check that packages are loaded
    {
      if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("Package \"dplyr\" needed for this function to work. Please install it.", 
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
    } 
    
    # Set variables to NULL before use, appeasing R CMD check
    sp_name <- informant <- UVps <- NULL 
    
    #create a subsettable numeric data frame
    bayesdata <- dplyr::select(data, -informant, -sp_name)
    
    
    if (prior == -1) {
      prior <- matrix(1/answers, answers, ncol(bayesdata))
    }
     if ( !all(abs(colSums(prior) -1)<0.001) ){
       warning('Your prior for every question should add up to 1.')
     }

    if ( !all(prior>=0) ){
      stop('For this to work your prior needs to assign non-negative probability to all possible outcomes.')    
    }
    
    if ( ncol(prior)!=ncol(bayesdata) || nrow(prior)!=answers){
      stop('Something is wrong with the prior. It may have a different number of rows or columns than the data.')    
    }  #end error stops
    
    
    # generate a probability matrix
    bayes_consensus <- matrix(0, answers, ncol(bayesdata))
    # move column names from the ethnobotany data
    colnames(bayes_consensus) <- colnames(bayesdata)
    # rownames continue for each possible answer
    rownames(bayes_consensus) <- c(1:answers)
    # calculate prior chance of consensus
    prior_of_consensus <- (1-prior_for_answers) * (answers-1) / answers
    # calculate prior chance of non consensus
    prior_non_consensus <- 1-prior_of_consensus
    # for loops to calculate bayes consensus equation
    for (consensus in 1:ncol(bayesdata)) {
      for (non_consensus in 1:answers) {
        bayes_consensus[non_consensus,consensus] <- prod(ifelse(test = bayesdata[,consensus] == non_consensus, 
          yes = prior_non_consensus, no = prior_of_consensus) )      
      }
      bayes_consensus[,consensus] <- bayes_consensus[,consensus] * prior[,consensus]    
      bayes_consensus[,consensus] <- bayes_consensus[,consensus] / sum(bayes_consensus[,consensus])    
    }  
    return(bayes_consensus)
  }
