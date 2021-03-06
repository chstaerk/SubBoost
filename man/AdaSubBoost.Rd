% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AdaSubBoost.R
\name{AdaSubBoost}
\alias{AdaSubBoost}
\title{AdaSubBoost and RSubBoost}
\usage{
AdaSubBoost(
  data,
  Iter,
  K = 100,
  q = 10,
  size.fixed = NULL,
  tau = 0.01,
  const = 0,
  savings = 1,
  U_C = 25,
  family = "normal",
  conservative = TRUE,
  update = "S",
  adaptive = TRUE,
  s_max = 20,
  nstop = Iter,
  automatic.stopping = TRUE,
  marginal.screening = FALSE,
  plotting = FALSE
)
}
\arguments{
\item{data}{should be a list with data$x as design matrix and data$y as response}

\item{Iter}{iterations}

\item{K}{parameter K - default is set to 100}

\item{q}{parameter q - default is set to 10}

\item{size.fixed}{default is set to NULL}

\item{tau}{parameter tau - default is set to 0.01}

\item{const}{parameter const - default is set to 0}

\item{savings}{default is set to 1}

\item{U_C}{parameter is set to 25}

\item{family}{default is set to "normal"}

\item{conservative}{default is set to TRUE}

\item{update}{default is set to "S"}

\item{adaptive}{default is set to TRUE (AdaSubBoost), for RSubBoost specify as FALSE}

\item{s_max}{default is set to 20}

\item{nstop}{default is set to  Iter}

\item{automatic.stopping}{default is set to TRUE}

\item{marginal.screening}{default is set to FALSE}

\item{plotting}{default is set to FALSE}
}
\description{
This function implements the adaptive subspace boosting algorithm (AdaSubBoost) as well as random subspace boosting (RSubBoost).
}
\keyword{AdaSubBoost,}
\keyword{RSubBoost}
\examples{

### high-dimensional example (AdaSubBoost and RSubBoost)
# load "hdi" package for riboflavin data
require(hdi)

data(riboflavin)
n <- length(riboflavin$y)
p <- dim(riboflavin$x)[2]

# input data format:
# list with design matrix in data$x and response vector in data$y
data <- list()
data$x <- as.matrix(riboflavin$x)
data$y <- as.vector(riboflavin$y)

# constant (gamma) in EBIC
const <- 1

# expected search size
q <- 20

# adaptation parameter
K <- p/q

# maximum size for initial screening
s_max <- 15

# learning rate
tau <- 0.01

# (maximum) number of iterations
Iter <- 1000

# AdaSubBoost
set.seed(123)
output <- AdaSubBoost(data = data, Iter = Iter, const = const,
                      K = K, q = q, tau = tau, s_max = s_max)
output$selected # selected variables by AdaSubBoost
output$coef[names(output$selected)] # estimated non-zero coefficients by AdaSubBoost


# RSubBoost (no adaptation of sampling probabilites for base-learners)
set.seed(123)
outputRSub <- AdaSubBoost(data = data, Iter = Iter, const = const,
                          K = K, q = q, tau = tau, s_max = s_max,
                          adaptive = FALSE)
outputRSub$selected  # selected variables by RSubBoost
outputRSub$coef[names(outputRSub$selected)]


}
