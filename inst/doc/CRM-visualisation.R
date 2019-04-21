## ------------------------------------------------------------------------
outcomes <- '2NN 3NN 4TT'

## ------------------------------------------------------------------------
skeleton <- c(0.05, 0.15, 0.25, 0.4, 0.6)
target <- 0.25

## ---- message=FALSE, warning=FALSE---------------------------------------
library(trialr)

## ---- results = "hide", warning=FALSE, message=FALSE---------------------
fit <- stan_crm(outcomes, skeleton = skeleton, target = target, 
                beta_sd = sqrt(1.34), seed = 123)
fit

## ---- warning=FALSE, message=FALSE---------------------------------------
library(tidyr)
library(dplyr)

prob_tox_samp <- as.data.frame(fit, 'prob_tox')
prob_tox_samp_tall <- prob_tox_samp %>%
  gather(Label, ProbTox) %>%
  mutate(
    DoseLevel = rep(1:ncol(prob_tox_samp), each = nrow(prob_tox_samp)),
    Draw = rep(1:nrow(prob_tox_samp), times = ncol(prob_tox_samp))
  )

## ------------------------------------------------------------------------
prob_tox_samp_tall %>% head(10)

## ---- fig.width=7, fig.height=7------------------------------------------
library(ggplot2)

prob_tox_samp_tall %>% 
  ggplot(aes(x = DoseLevel, y = ProbTox, group = DoseLevel)) +
  geom_boxplot() + 
  ylim(0, 1) + 
  labs(title = 'boxplot of Pr(DLT) under CRM')

## ---- fig.width=7, fig.height=7------------------------------------------
prob_tox_samp_tall %>% 
  ggplot(aes(x = DoseLevel, y = ProbTox, group = DoseLevel)) +
  geom_violin(fill = 'orange') + 
  ylim(0, 1) + 
  labs(title = 'violin plot of Pr(DLT) under CRM')

## ---- fig.width=7, fig.height=7, message=FALSE---------------------------
library(ggridges)

prob_tox_samp_tall %>% 
  mutate(DoseLevel = factor(DoseLevel)) %>% 
  ggplot(aes(x = ProbTox, y = DoseLevel, fill = DoseLevel)) +
  geom_density_ridges() + 
  theme(legend.position = 'none') +
  labs(title = 'joyplot of Pr(DLT) under CRM') + 
  theme(legend.position = 'bottom')

## ---- fig.width=7, fig.height=7, message=FALSE---------------------------
prob_tox_samp_tall %>% 
  filter(Draw <= 200) -> prob_tox_subset

prob_tox_subset %>% 
  left_join(
    prob_tox_subset %>% 
      group_by(Draw) %>% 
      summarise(MTD = which.min(abs(ProbTox - target)) %>% factor()),
    by = 'Draw') %>% 
  ggplot(aes(x = DoseLevel, y = ProbTox, group = Draw)) +
  geom_line(aes(col = MTD), alpha = 0.5) + 
  geom_hline(yintercept = target, col = 'red', linetype = 'dashed') + 
  labs(title = 'Identify of MTD is shrouded in mystery') +
  theme(legend.position = 'bottom')

## ---- fig.width=7, fig.height=7, message=FALSE, warning=FALSE------------
apply(prob_tox_samp, 1, function(x) which.min(abs(x - target))) %>% 
  data.frame(MTD = factor(.)) %>% 
  count(MTD) %>% 
  mutate(ProbMTD = n / sum(n)) %>% 
  ggplot(aes(x = MTD, y = ProbMTD, fill = MTD)) + 
  geom_col() +
  theme(legend.position = 'bottom')

## ---- fig.width=7, fig.height=7------------------------------------------
colMeans(prob_tox_samp > target) %>% 
  data.frame(ProbToxic = ., DoseLevel = 1:length(skeleton)) %>%
  ggplot(aes(x = DoseLevel, y = ProbToxic, fill = ProbToxic)) + 
  geom_col() + 
  scale_fill_gradient(low="green", high="red") + 
  labs(title = 'Posterior probability that each dose is too toxic',
       y = 'Prob(Tox > target)', fill = 'Prob too toxic') +
  theme(legend.position = 'bottom')

