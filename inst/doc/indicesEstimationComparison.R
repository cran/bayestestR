## ----message=FALSE, warning=FALSE----------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("https://raw.github.com/easystats/circus/master/data/bayes_indices.csv")

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  select(noise, beta, prior_correct, effect, median, mean, map) %>%
  gather(estimate, value, -noise, -effect, -prior_correct) %>%
  mutate(noise = as.factor(noise),
         prior_correct = as.factor(prior_correct),
         value = value-effect) %>%
  ggplot(aes(x = noise, y = value, fill = estimate, color=prior_correct)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  theme_classic() +
  scale_fill_manual(values = c("beta" = "#607D8B", "map" = "#795548", "mean" = "#FF9800", "median" = "#FFEB3B"),
                    name = "Index") +
  scale_color_manual(values = c(`0`="#f44336", `1`="#8BC34A"),
                     name = "Correct Prior") +
  xlab("Point-estimate of the true value 0\n") +
  ylab("\nNoise") +
  coord_cartesian(ylim=c(-1, 1))

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  select(sample_size, beta, effect, prior_correct, median, mean, map) %>%
  gather(estimate, value, -sample_size, -effect, -prior_correct) %>%
  mutate(sample_size = as.factor(sample_size),
         prior_correct = as.factor(prior_correct),
         value = value-effect) %>%
  ggplot(aes(x = sample_size, y = value, fill = estimate, color=prior_correct)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  theme_classic() +
  scale_fill_manual(values = c("beta" = "#607D8B", "map" = "#795548", "mean" = "#FF9800", "median" = "#FFEB3B"),
                    name = "Index") +
  scale_color_manual(values = c(`0`="#f44336", `1`="#8BC34A"),
                     name = "Correct Prior") +
  ylab("Point-estimate of the true value 0\n") +
  xlab("\nSample Size") +
  coord_cartesian(ylim=c(-1, 1))

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  select(sample_size, beta, effect, prior_correct, median, mean, map, noise) %>%
  gather(estimate, value, -sample_size, -effect, -prior_correct, -noise) %>%
  mutate(noise= scale(noise),
         sample_size = scale(sample_size),
         prior_correct = as.factor(prior_correct)) %>%
  glm(effect ~ estimate/value * noise * sample_size * prior_correct, data=., family="binomial") %>%
  broom::tidy() %>%
  select(term, estimate, p=p.value) %>%
  filter(stringr::str_detect(term, 'value')) %>%
  mutate(term = stringr::str_remove(term, ":value"),
         term = stringr::str_remove(term, "estimate"),
         p = ifelse(p < .001, "< .001***", ifelse(p < .01, "< .01**", ifelse(p < .05, "< .05*", "> .05")))) %>%
  filter(stringr::str_detect(term, ':')) %>%
  knitr::kable(digits=2)

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  select(sample_size, beta, effect, prior_correct, median, mean, map, noise) %>%
  gather(estimate, value, -beta, -sample_size, -effect, -prior_correct, -noise) %>%
  mutate(effect = as.factor(effect),
         sample_size = as.factor(sample_size),
         estimate = factor(estimate, levels=c("mean", "median", "map"))) %>%
  ggplot(aes(x = beta, y = value, color = effect, shape=sample_size)) +
  geom_point(alpha=0.05) +
  facet_wrap(~estimate, scales = "free") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_color_manual(values = c(`0` = "#f44336", `1` = "#8BC34A"), name="Effect") +
  guides(colour = guide_legend(override.aes = list(alpha = 1)),
         shape = guide_legend(override.aes = list(alpha = 1), title="Sample Size"))

