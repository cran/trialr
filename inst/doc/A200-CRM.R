## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(trialr)

## -----------------------------------------------------------------------------
target <- 0.25
skeleton <- c(0.05, 0.15, 0.25, 0.4, 0.6)

## -----------------------------------------------------------------------------
outcomes <- '2NN 3NN 4TT'

## ---- results = "hide", warning=FALSE, message=FALSE--------------------------
fit1 <- stan_crm(outcomes, skeleton = skeleton, target = target, 
                 model = 'empiric', beta_sd = sqrt(1.34), seed = 123)

## -----------------------------------------------------------------------------
fit1

## -----------------------------------------------------------------------------
prob_tox_samp1 <- as.data.frame(fit1, 'prob_tox')
prob_tox1 <- colMeans(prob_tox_samp1)
prob_tox1

## -----------------------------------------------------------------------------
prob_too_toxic1 <- unname(colMeans(prob_tox_samp1 > target))
prob_too_toxic1

## ---- results = "hide", warning=FALSE, message=FALSE--------------------------
fit2 <- stan_crm(outcomes, skeleton = skeleton, target = target, 
                 model = 'logistic', a0 = 3, beta_mean = 0, beta_sd = sqrt(1.34), 
                 seed = 123)

## -----------------------------------------------------------------------------
fit2

## ---- results = "hide", warning=FALSE, message=FALSE--------------------------
fit3 <- stan_crm(outcomes, skeleton = skeleton, target = target, 
                 model = 'logistic_gamma', 
                 a0 = 3, beta_shape = 1, beta_inverse_scale = 1, 
                 seed = 123)

## -----------------------------------------------------------------------------
fit3

## ---- results = "hide", warning=FALSE, message=FALSE--------------------------
fit4 <- stan_crm(outcomes, skeleton = skeleton, target = target, 
                 model = 'logistic2', 
                 alpha_mean = 0, alpha_sd = 1, beta_mean =  0, beta_sd = 1, 
                 seed = 123)

## -----------------------------------------------------------------------------
fit4

## -----------------------------------------------------------------------------
post_curves <- data.frame(
  DoseLevel = 1:length(skeleton),
  Empiric = fit1$prob_tox,
  Logit1N = fit2$prob_tox,
  Logit1G = fit3$prob_tox,
  Logit2N = fit4$prob_tox
)
knitr::kable(post_curves, digits = 2)

## ---- fig.width=7, fig.height=7-----------------------------------------------
post_curves_tall <- data.frame(
  DoseLevel = rep(1:length(skeleton), times = 4),
  ProbTox = c(fit1$prob_tox, fit2$prob_tox, fit3$prob_tox, fit4$prob_tox),
  Model = rep(c('Empiric', 'Logit1N', 'Logit1G', 'Logit2N'), each = 5)
)

library(ggplot2)
ggplot(post_curves_tall, aes(x = DoseLevel, y = ProbTox, group = Model, col = Model)) + 
  geom_line(size = 1.2) + 
  geom_hline(yintercept = target, col = 'red', linetype = 'dashed')

## -----------------------------------------------------------------------------
knitr::kable(rstan::summary(fit4$fit, 'alpha')$summary, digits = 2)

## -----------------------------------------------------------------------------
knitr::kable(
  data.frame(
    DoseLevel = 1:length(skeleton),
    ProbMTD_Empiric = as.numeric(fit1$prob_mtd),
    ProbMTD_Logit2N = as.numeric(fit4$prob_mtd)
  ), digits = 2
)

