#' Run State-based Model
#'
#' Runs a state-based model to fit a Beverton-Holt curve using the provided observed data, hatchery rates, harvest
#' rates, and years of data.
#'
#' @param initValFunc Inital value function for productivity and capacity
#' @param Sobs the observed yearly population data
#' @param pHOS Vector contianing proportion of naturally spawning hatchery fish
#' @param HR Vector containing harvest rate for every year
#' @param Nyears Number of years of data
#' @param nIter Number of iterations to monitor
#'
#' @return a list containing nIter samples of the posterior distributions of capacity,
#' productivity, observed Error, and Process Error. Returns NULL if Sobs, pHOS, or HR
#' are not provided or their lengths do not match.
#'
#' @examples
#' library('rjags')
#' N <- 100
#' pHOS <- sample(rep(seq(0, .9, .1), rep(N/10,10)))
#' HR <- sample(rep(seq(0, .63, .07), rep(N/10,10)))
#' processErr <- .1
#' obsErr <- .15
#' Sobs <- genData(N, pHOS, HR, processErr, obsErr)
#' initValFunc <- function() {
#'   list(prod=1, cap=600)
#' }
#' stateModel(initValFunc, Sobs, pHOS, HR, N, 10000)
#'
#' @export
stateModel <- function(initValFunc, Sobs, pHOS, HR, Nyears, nIter) {
  if (is.null(Nyears) | is.null(Sobs) | is.null(pHOS) | Nyears != length(pHOS) | Nyears != length(HR) |
      Nyears != length(Sobs)) {
    return(NULL)
  }
  if (is.null(nIter)) {
    nIter <- 10000
  }
  if (is.null(initValFunc)) {
    initValFunc <- function() {
      list(prod=1, cap=600)
    }
  }
  state.bug <- '
  model {
    S[1] ~ dlnorm(0,0.0001)
    for(i in 1:(Nyears-1)) {
      S[i+1] ~ dlnorm(log((S[i]/(1/prod + S[i]/cap))*(1/(1-pHOS[i]))*(1-HR[i])), tauPro)
    }

    for (i in 1:Nyears) {
      Sobs[i] ~ dlnorm(log(S[i]), tauObs)
    }

    prod ~ dlnorm(log(3), 0.01)
    cap ~ dlnorm(log(15000), 0.001)
    tauPro ~ dgamma(0.001, 0.001)
    tauObs ~ dgamma(0.001, 0.001)
  }'
  jagsDat <- list(Sobs=Sobs, Nyears=Nyears, pHOS=pHOS, HR=HR)
  m.state <- jags.model(textConnection(state.bug), data=jagsDat, initValFunc, n.chains=3, n.adapt=5000)
  s.state <- jags.samples(m.state,n.iter=nIter,variable.names=c("prod","cap", "tauPro", "tauObs"))
  return(s.state)
}
