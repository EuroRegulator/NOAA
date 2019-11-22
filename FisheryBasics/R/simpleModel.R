#' Run Simple Model
#'
#' Runs a basic model to fit a Beverton-Holt curve using the provided observed data, hatchery rates, harvest
#' rates, and years of data.
#'
#' @param initValFunc Inital value function for productivity and capacity
#' @param Sobs the observed yearly population data
#' @param pHOS Vector contianing proportion of naturally spawning hatchery fish
#' @param HR Vector containing harvest rate for every year
#' @param Nyears Number of years of data
#' @param nIter Number of iterations to monitor
#'
#' @return a list containing nIter samples of the posterior distributions of both capacity,
#' productivity, and Error. Returns NULL if Sobs, pHOS, or HR are not provided or their
#' lengths do not match.
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
#' simpleModel(initValFunc, Sobs, pHOS, HR, N, 10000)
#'
#' @export
simpleModel <- function(initValFunc, Sobs, pHOS, HR, Nyears, nIter) {
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
  simple.bug <- '
  model {
    for (i in 1:(Nyears-1)) {
      R[i] ~ dlnorm(log(mu[i]), tau)
      mu[i] <- S[i]/(1/prod + S[i]/cap)
    }

    prod ~ dlnorm(log(3), 0.01)
    cap ~ dlnorm(log(15000), 0.001)
    tau ~ dgamma(0.001, 0.001)
  }'
  jagsDat <- list(S=Sobs[-Nyears], R=Sobs[-1]*(1-pHOS[-Nyears])/(1-HR[-Nyears]), Nyears=Nyears)
  m2 <- jags.model(textConnection(simple.bug), data=jagsDat, initValFunc, n.chains=3, n.adapt=5000)
  s2 <- jags.samples(m2,n.iter=nIter,variable.names=c("prod","cap", "tau"))
  return(s2)
}
