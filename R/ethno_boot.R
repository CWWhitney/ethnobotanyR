#' Bootstrap analyses of ethnobotany indices
#'
#' Creates a non-parametric bootstrap as a Bayesian Model \url{http://www.sumsar.net/blog/2015/04/the-non-parametric-bootstrap-as-a-bayesian-model/}. This is meant to be applied for ethnobotany data and indices in the ethnobotanyR package. Performs a Bayesian bootstrap and returns a sample of size 'n1' representing the posterior distribution of the chose statistic (i.e. 'mean'). The function returns a vector if the statistic is one-dimensional (like for mean(...)) or a data.frame if the statistic is multi-dimensional (like for the coefficients 'coefs.' of a regression model 'lm').
#' @references 
#' Bååth, Rasmus. “The Non-Parametric Bootstrap as a Bayesian Model” Publishable Stuff, 2015. \url{http://www.sumsar.net/blog/2015/04/the-non-parametric-bootstrap-as-a-bayesian-model/}.
#' 
#' @usage ethno_boot(data, statistic, n1 = 1000, 
#' n2 = 1000 , use_weights = FALSE, weight_arg = NULL, ...)
#' 
#' @param data Can be either a vector, matrix or a data.frame.
#' @param statistic A function that accepts data as its first argument and possibly the weights as its second, if use_weights is TRUE. Should return a numeric vector.
#' @param n1 The size of the bootstrap sample.
#' @param n2 The sample size used to calculate the statistic each bootstrap draw.
#' @param use_weights TRUE or FALSE about whether the statistic function accepts a weight argument or should be calculated using resampled data.
#' @param weight_arg If the statistic function includes a named argument for the weights this can be specified here.
#' @param ... Further arguments passed on to the statistic function.
#' 
#' @keywords graphs arith math logic methods misc survey Bayes
#'
#' @return Radial bar plot of chosen ethnobotany indices in ethnobotanyR package.
#' 
#' @section Warning:
#' 
#' Identification for informants and species must be listed by the names 'informant' and 'sp_name' respectively in the data set.
#' The rest of the columns should all represent separate identified ethnobotany use categories. These data should be populated with counts of uses per informant (should be 0 or 1 values).
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
#' #Generate random dataset of three informants uses for four species
#' 
#' eb_data <- URs(ethnobotanydata)
#'  
#' ethno_boot(data = eb_data$URs, statistic = mean, 
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
  }#end error stops
  
  # Set the variables to NULL first, appeasing R CMD check
  value <-  meltURdata <- URdata <- URs <- sp_name <- informant <- URps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  #Bayes boot
  
    # Draw from a uniform Dirichlet dist. with alpha set to rep(1, n_dim).
    # Using the facts that you can transform gamma distributed draws into 
    # Dirichlet draws and that rgamma(n, 1) <=> rexp(n, 1)
    dirichlet_weights <- matrix( stats::rexp(NROW(data) * n1, 1) , 
                                 ncol = NROW(data), byrow = TRUE)
    dirichlet_weights <- dirichlet_weights / 
      rowSums(dirichlet_weights)
    
    if(use_weights) {
      stat_call <- quote(statistic(data, w, ...))
      names(stat_call)[3] <- weight_arg
      boot_sample <- apply(dirichlet_weights, 1, function(w) {
        eval(stat_call)
      })
    } else {
      if(is.null(dim(data)) || length(dim(data)) < 2) { 
        # data is a list type of object
        boot_sample <- apply(dirichlet_weights, 1, function(w) {
          data_sample <- sample(data, size = n2, 
                                replace = TRUE, prob = w)
          statistic(data_sample, ...)
        })
      } else { # data is a table type of object
        boot_sample <- apply(dirichlet_weights, 1, function(w) {
          index_sample <- sample(nrow(data), 
                                 size = n2, replace = TRUE, prob = w)
          statistic(data[index_sample, ,drop = FALSE], ...)
        })
      }
    }
    if(is.null(dim(boot_sample)) || length(dim(boot_sample)) < 2) {
      # If the bootstrap sample is just a simple vector return it.
      boot_sample
    } else {
      # Otherwise it is a matrix. Since apply returns one row per statistic
      # let's transpose it and return it as a data frame.
      as.data.frame(t(boot_sample))
    }
  }
  
