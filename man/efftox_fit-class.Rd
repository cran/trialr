% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efftox_fit.R
\docType{class}
\name{efftox_fit-class}
\alias{efftox_fit-class}
\alias{efftox_fit}
\title{Class of model fit by \pkg{trialr} using the EffTox dose-finding design.}
\usage{
efftox_fit(
  dose_indices,
  num_patients,
  doses,
  tox,
  eff,
  prob_tox,
  prob_eff,
  median_prob_tox,
  median_prob_eff,
  prob_acc_tox,
  prob_acc_eff,
  utility,
  post_utility,
  prob_obd,
  acceptable,
  recommended_dose,
  dat,
  fit
)
}
\arguments{
\item{dose_indices}{A vector of integers representing the dose-levels under
consideration.}

\item{num_patients}{Integer, the number of patients analysed.}

\item{doses}{vector of integers representing the dose given to the patients.}

\item{tox}{vector of integers representing the toxicity status of the
patients.}

\item{eff}{vector of integers representing the efficacy status of the
patients.}

\item{prob_tox}{The posterior mean probabilities of toxicity at doses 1:n;
a vector of numbers between 0 and 1.}

\item{prob_eff}{The posterior mean probabilities of efficacy at doses 1:n;
a vector of numbers between 0 and 1.}

\item{median_prob_tox}{The posterior median probabilities of toxicity at
doses 1:n; a vector of numbers between 0 and 1.}

\item{median_prob_eff}{The posterior mean probabilities of efficacy at doses
1:n; a vector of numbers between 0 and 1.}

\item{prob_acc_tox}{The posterior mean probabilities that toxicity at the
doses is acceptable, i.e. that it is less than the maximum toxicity
threshold; a vector of numbers between 0 and 1.}

\item{prob_acc_eff}{The posterior mean probabilities that efficacy at the
doses is acceptable, i.e. that it exceeds the minimum acceptable efficacy
threshold; a vector of numbers between 0 and 1.}

\item{utility}{The utilities of doses 1:n, calculated by plugging the
posterior mean probabilities of efficacy and toxicity into the utility
formula, as advocated by Thall & Cook. Contrast to \code{post_utility};
a vector of numbers.}

\item{post_utility}{The posterior mean utilities of doses 1:n, calculated
from the posterior distributions of the utilities. This is in contrast to
\code{utility}, which uses plug-in posterior means of efficacy and toxicity,
as advocated by Thall & Cook; a vector of numbers.}

\item{prob_obd}{The posterior probability that each dose is the optimal
biological dose (OBD); a vector of numbers between 0 and 1. This probability
reflects the uncertainty remaining in the parameter distributions, whereas
\code{prob_tox} and \code{prob_eff} (etc) do not.}

\item{acceptable}{A vector of logical values to indicate whether doses 1:n
are acceptable, according to the rules for acceptable efficacy & toxicity,
and rules on not skipping untested doses.}

\item{recommended_dose}{An integer representing the dose-level recommended
for the next patient or cohort; or \code{NA} if stopping is recommended.}

\item{dat}{Object \code{\link{efftox_params}} containing data passed to
\code{\link[rstan:sampling]{sampling}}.}

\item{fit}{An object of class \code{\link[rstan:stanfit]{stanfit}},
containing the posterior samples.}
}
\description{
Phase I/II dose-finding trials, i.e. those that search for a dose my efficacy
and toxicity outcomes search for the optimal biological dose (OBD), rather
than the maximum tolerated dose (MTD) that is typically sought be traditional
toxicity-only dose-finding.
}
\details{
See \code{methods(class = "efftox_fit")} for an overview of available
methods.
}
\seealso{
\code{\link{stan_efftox}}
\code{\link{stan_efftox_demo}}
}
