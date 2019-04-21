## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE---------------------------------------
library(trialr)

## ------------------------------------------------------------------------
target <- 0.25
skeleton <- c(0.05, 0.15, 0.25, 0.4, 0.6)

## ---- results = "hide", warning=FALSE, message=FALSE---------------------
mod1 <- stan_crm(outcome_str = '2NN 3NN 4TT', skeleton = skeleton, 
                 target = target, model = 'empiric', beta_sd = sqrt(1.34), 
                 seed = 123)

## ------------------------------------------------------------------------
mod1

## ------------------------------------------------------------------------
mod1$recommended_dose

## ---- fig.width=7, fig.height=7------------------------------------------
library(ggplot2)
plot_df = data.frame(DoseLevel = 1:length(skeleton),
                     ProbTox = mod1$prob_tox)
ggplot(plot_df, aes(x = DoseLevel, y = ProbTox)) +
  geom_point() + geom_line() + ylim(0, 1) + 
  geom_hline(yintercept = target, col = 'orange', linetype = 'dashed') +
  labs(title = 'Posterior dose-toxicity curve under empiric CRM model')

## ---- results = "hide"---------------------------------------------------
outcomes <- '1NNE 2EEB'
mod2 <- stan_efftox_demo(outcomes, seed = 123)

## ------------------------------------------------------------------------
mod2

## ------------------------------------------------------------------------
mod2$recommended_dose

## ------------------------------------------------------------------------
mod2$utility

## ---- fig.width=7, fig.height=7------------------------------------------
efftox_contour_plot(mod2)
title('EffTox utility contours')

## ---- results = "hide"---------------------------------------------------
mod3 <- stan_hierarchical_response_thall(
  group_responses = c(0, 0, 1, 3, 5, 0, 1, 2, 0, 0), 
  group_sizes = c(0, 2 ,1, 7, 5, 0, 2, 3, 1, 0), 
  mu_mean = -1.3863,
  mu_sd = sqrt(1 / 0.1),
  tau_alpha = 2,
  tau_beta = 20)

## ------------------------------------------------------------------------
mod3

## ---- fig.width=7, fig.height=7, warning=FALSE, message=FALSE------------
rstan::plot(mod3, pars = 'prob_response') + 
  geom_vline(xintercept = 0.3, col = 'orange', linetype = 'dashed') +
  labs(title = 'Partially-pooled analysis of response rate in 10 sarcoma subtypes')

