#' Bootstrap analyses of ethnobotany indices
#'
#' Creates a non-parametric bootstrap as a Bayesian Model 
#' \url{http://www.sumsar.net/blog/2015/04/the-non-parametric-bootstrap-as-a-bayesian-model/}. 
#' This is meant to be applied for ethnobotany data and indices in the ethnobotanyR package. 
#' Performs a Bayesian bootstrap, like a normal bootstrap but instead 
#' of simulating the sampling distribution of a statistic estimating a parameter, 
#' it simulates the posterior distribution of the parameter.
#' 
#' The function returns a sample of size 'n1' representing the posterior distribution 
#' of the chosen statistic (i.e. 'mean'). The function returns a vector if the 
#' statistic is one-dimensional (like for mean(...)) or a data.frame 
#' if the statistic is multi-dimensional (like for the coefficients 'coefs.' 
#' of a regression model 'lm').
#' 
#' @references 
#' Bååth, Rasmus. “The Non-Parametric Bootstrap as a Bayesian Model” Publishable Stuff, 2015. \url{http://www.sumsar.net/blog/2015/04/the-non-parametric-bootstrap-as-a-bayesian-model/}.
#' 
#' @usage ethno_boot(data, statistic, n1 = 1000, 
#' n2 = 1000 , use_weights = FALSE, weight_arg = NULL, ...)
#' 
#' @param data Can be either a vector, matrix or a data.frame.
#' @param statistic A function that accepts data as its first argument and possibly the weights as its second, if use_weights is TRUE. Should return a numeric vector.
#' @param n1 The size of the bootstrap sample.
#' @param n2 The sample size used to calculate the statistic for each bootstrap draw.
#' @param use_weights TRUE or FALSE about whether the statistic function accepts a weight argument or should be calculated using resampled data.
#' @param weight_arg If the statistic function includes a named argument for the weights this can be specified here.
#' @param ... Further arguments passed on to the statistic function.
#' 
#' @keywords Bayes Bayesian graphs arith math logic methods misc survey
#'
#' @return Bayesian bootstrap of chosen ethnobotany indices in ethnobotanyR package.
#' 
#' @section Application:
#' 
#'  This function was inspired by Rasmus Bååth's “The Non-Parametric Bootstrap as a Bayesian Model” Publishable Stuff, 2015. \url{http://www.sumsar.net/blog/2015/04/the-non-parametric-bootstrap-as-a-bayesian-model/}.
#'  In order to understand the various possible applications of this function for ethnobotany analyses it is important to read through that work. 
#' 
#'  ethnobotanyR users often have a large number of counts in cells of the data set after categorization (i.e one user cites ten different ‘food’ uses but this is just one category). 
#'  Most quantitative ethnobotany tools are not equipped for cases where the theoretical maximum number of use reports in one category, for one species by one informant is >1. 
#'  This function and the ethno_bayes_consensus function may be useful to work with these richer datasets for the Bayes consensus analysis.
#'  
#' @importFrom stats rexp complete.cases
#'  
#' @examples
#' 
#' #Use built-in ethnobotany data example
#' 
#' ethno_boot(data = ethnobotanydata$Use_1, 
#' statistic = mean, n1 = 1000)
#' 
#' 
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
#' names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
#' eb_data$informant <- sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
#' eb_data$sp_name <- sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
#' 
#' boot_data <- URs(eb_data)
#'  
#' ethno_boot(data = boot_data$URs, statistic = mean, 
#' n1 = 1000, n2 = 100)
#' 
#' @export ethno_boot
#' 
ethno_boot <- function(data, statistic, 
                       n1 = 1000, n2 = 1000, 
                       use_weights = FALSE, 
                       weight_arg = NULL, ...) {
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  if (any(is.na(data))) {
    warning("Some of your observations included \"NA\" and were removed. Consider using \"0\" instead.")
    data<-data[stats::complete.cases(data), ]
    
    ## Use 'complete.cases' from stats to get to the collection of obs without NA
    if (!is.data.frame(data)) {
      warning("Your data has been converted to a data.frame.")
      data<-as.data.frame(data)
      
  }#end error stops
  
  # Set the variables to NULL first, appeasing R CMD check
  value <-  meltURdata <- URdata <- URs <- sp_name <- informant <- URps <- NULL
  
  #Bayes boot
  
# Create Dirichlet distribution weights with rexp
    dirichlet_weights <- matrix(stats::rexp(nrow(data) * n1, 1), 
                                 ncol = nrow(data), byrow = TRUE)
    
    dirichlet_weights <- dirichlet_weights / rowSums(dirichlet_weights)
    
# Draw from a uniform Dirichlet distribution    
    
      stat_call <- quote(statistic(data))
      boot_sample <- apply(dirichlet_weights, 1, function(w) {
        eval(stat_call)
      })
      
      as.data.frame(t(boot_sample))
    }
  }
  
