#==============================================================================#
#                            Define Functions                                  #
#===============================================================================
#                                                                              #
# Collection of misc small frequently used functions and running source for    #
# more complex functions.                                                      #
#                                                                              #
# ============================================================================ #

  
  # Transform rate to probability --------------------------------------------
  
    probabilityToRate <- function(probability) {
      
      rate <- -log(1 - probability)
    }
  
  # ---- #
  
  
  
  # Transform probability to rate --------------------------------------------
  
    rateToProbability <- function(rate) {
      
      rate <- 1 - exp(-rate)
    }
  
  # ---- #
  
  
  
  # Derive beta distribution parameters from mean and variance ---------------   
  
    deriveBetaParameters <- function(mu, var) {
      
      shape1 <- ((1 - mu) / var - 1 / mu) * mu ^ 2
      
      shape2 <- shape1 * (1 / mu - 1)
      
      return(params = list(shape1 = shape1, shape2 = shape2))
      
    }
  
  # ---- #
  
  
  
  # Derive gamma distribution parameters from mean and variance -------------   
  
    deriveGammaParameters <- function(mu, var) {
      
      shape <- mu^2 / var
      
      scale <- var/mu
      
      return(params = list(shape = shape, scale = scale))
      
    }
  
  # ---- #
  
  
  
  # Transform rate to probability --------------------------------------------
  
    varFromProp <- function(prop, N) {
      
      sqrt(prop*(1-prop)/N)
    }
    
  # ---- #
  
  
  
  # Derive lognormal parameters from mean and standard deviation -------------
  
    deriveLogNormalParameters <- function(mean, CI_Lower, CI_Higher) {
      
      selog = (log(CI_Higher) - log(CI_Lower))/3.92
      
      meanlog = log(mean)
      
      return(params = list(meanlog = meanlog, selog = selog))
      
    }
  
  # ---- #
  
  
  
  # Draw lognormal values given a vector of up to 2 seed values and log     --
  # mean and log SE - currently only returning 1 value to be repeated
  
    produceLogNormalValues <- function(seed1, seed2, 
                                       meanlog, selog, seedRunGroup) {
      
      dqset.seed(c(seed1[[1]], seed2[[1]]))
      
      dqrunif(seedRunN + seedRunGroup[[1]])
      
      as.list(exp(dqrnorm(1, mean = meanlog[[1]], sd = selog[[1]])))
      
      
    }
    
  # ---- #
  
  
