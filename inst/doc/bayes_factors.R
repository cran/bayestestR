## ----setup, include=FALSE------------------------------------------------
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(comment = ">")
ggplot2::theme_set(see::theme_modern())
options(digits = 2)
set.seed(5)

## ----brms_disp, eval=FALSE, message=FALSE, warning=FALSE-----------------
#  library(brms)
#  
#  m0 <- brm(Sepal.Length ~ 1, data = iris, save_all_pars = TRUE)
#  m1 <- brm(Sepal.Length ~ Petal.Length, data = iris, save_all_pars = TRUE)
#  m2 <- brm(Sepal.Length ~ Species, data = iris, save_all_pars = TRUE)
#  m3 <- brm(Sepal.Length ~ Species + Petal.Length, data = iris, save_all_pars = TRUE)
#  m4 <- brm(Sepal.Length ~ Species * Petal.Length, data = iris, save_all_pars = TRUE)

## ----brms_models_disp, eval=FALSE----------------------------------------
#  library(bayestestR)
#  comparison <- bayesfactor_models(m1, m2, m3, m4, denominator = m0)
#  comparison

## ----brms_models_print, echo=FALSE, message=FALSE, warning=FALSE---------
# dput(comparison)
library(bayestestR)
comparison <- structure(
  list(
    Model = c("Petal.Length", "Species", "Species + Petal.Length", "Species * Petal.Length", "1"),
    BF = c(3.44736e+44, 5.628679e+29, 7.121386e+55, 9.149948e+55, 1)
  ),
  class = c("bayesfactor_models", "see_bayesfactor_models","data.frame"),
  row.names = c(NA, -5L),
  denominator = 5L,
  BF_method = "marginal likelihoods (bridgesampling)"
)
comparison

## ----update_models1, message=FALSE, warning=FALSE------------------------
update(comparison, reference = 3)

## ----update_models2, message=FALSE, warning=FALSE------------------------
update(comparison, reference = 2)

## ----lme4_models, message=FALSE, warning=FALSE---------------------------
library(lme4)

m0 <- lmer(Sepal.Length ~ (1 | Species), data = iris)
m1 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
m2 <- lmer(Sepal.Length ~ Petal.Length + (Petal.Length | Species), data = iris)
m3 <- lmer(Sepal.Length ~ Petal.Length + Petal.Width + (Petal.Length | Species), data = iris)
m4 <- lmer(Sepal.Length ~ Petal.Length * Petal.Width + (Petal.Length | Species), data = iris)

bayesfactor_models(m1, m2, m3, m4, denominator = m0)

## ----inclusion_brms, message=FALSE, warning=FALSE, eval=TRUE-------------
bayesfactor_inclusion(comparison)

## ----inclusion_brms2, message=FALSE, warning=FALSE, eval=TRUE------------
bayesfactor_inclusion(comparison, match_models = TRUE)

## ----JASP_all, message=FALSE, warning=FALSE, eval=TRUE-------------------
library(BayesFactor)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
BF_ToothGrowth <- anovaBF(len ~ dose*supp, ToothGrowth)

bayesfactor_inclusion(BF_ToothGrowth)

## ----JASP_all_fig, echo=FALSE, message=FALSE, warning=FALSE--------------
knitr::include_graphics("https://github.com/easystats/bayestestR/raw/master/man/figures/JASP1.PNG")

## ----JASP_matched, message=FALSE, warning=FALSE, eval=TRUE---------------
bayesfactor_inclusion(BF_ToothGrowth, match_models = TRUE)

## ----JASP_matched_fig, echo=FALSE, message=FALSE, warning=FALSE----------
knitr::include_graphics("https://github.com/easystats/bayestestR/raw/master/man/figures/JASP2.PNG")

## ----JASP_Nuisance, message=FALSE, warning=FALSE, eval=TRUE--------------
BF_ToothGrowth_against_dose <- BF_ToothGrowth[3:4]/BF_ToothGrowth[2] # OR: 
# update(bayesfactor_models(BF_ToothGrowth), subset = c(4,5), reference = 3)
BF_ToothGrowth_against_dose


bayesfactor_inclusion(BF_ToothGrowth_against_dose)

## ----JASP_Nuisance_fig, echo=FALSE, message=FALSE, warning=FALSE---------
knitr::include_graphics("https://github.com/easystats/bayestestR/raw/master/man/figures/JASP3.PNG")

## ----sleep_boxplot, echo=FALSE, message=FALSE, warning=FALSE-------------
library(ggplot2)

ggplot(sleep, aes(x = group, y = extra, fill= group)) +
  geom_boxplot() +
  theme_classic()

## ----rstanarm_disp, eval=FALSE, message=FALSE, warning=FALSE-------------
#  library(rstanarm)
#  model <- stan_glm(extra ~ group, data = sleep)

## ----rstanarm_fit, echo=FALSE, message=FALSE, warning=FALSE--------------
library(rstanarm)
junk <- capture.output(model <- stan_glm(extra ~ group, data = sleep))

## ----prior_n_post, message=FALSE, warning=FALSE, results='hide'----------
posterior <- as.data.frame(model)$group2

insight::get_priors(model)

## ----prior_table, echo=FALSE, message=FALSE, warning=FALSE---------------
knitr::kable(insight::get_priors(model))

## ----message=FALSE, warning=FALSE----------------------------------------
library(bayestestR)
prior <- distribution_normal(length(posterior), mean = 0, sd = 5.044799)

## ----prior_n_post_plot, echo=FALSE, message=FALSE, warning=FALSE---------

# Using "see"
bfsd <- bayesfactor_savagedickey(
  data.frame(group2 = posterior),
  data.frame(group2 = prior)
)

plot(bfsd) +
  theme(legend.position = c(0.2, 0.8))


## ----savagedickey, message=FALSE, warning=FALSE--------------------------
test_group2 <- bayesfactor_savagedickey(posterior = posterior, prior = prior)
test_group2

## ----prior_n_post_plot_one_sided, echo=FALSE, message=FALSE, warning=FALSE----

# Using "see"
bfsd <- bayesfactor_savagedickey(
  data.frame(group2 = posterior),
  data.frame(group2 = prior),
  direction = ">"
)

plot(bfsd) +
  theme(legend.position = c(0.8,0.8))


## ----savagedickey_one_sided, message=FALSE, warning=FALSE----------------
test_group2_right <- bayesfactor_savagedickey(posterior = posterior, prior = prior, direction = ">")
test_group2_right

## ------------------------------------------------------------------------
bayesfactor_savagedickey(model)

## ------------------------------------------------------------------------
library(emmeans)
group_diff <- pairs(emmeans(model, ~ group))
group_diff

# pass the original model via prior
bayesfactor_savagedickey(group_diff, prior = model)
