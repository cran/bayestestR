## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
options(digits=2)

## ----message=FALSE, warning=FALSE----------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("https://raw.github.com/easystats/circus/master/data/bayesSim_study1.csv")

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  select(error, true_effect, outcome_type, beta, Median, Mean, MAP) %>%
  gather(estimate, value, -error, -true_effect, -outcome_type) %>%
  mutate(temp = as.factor(cut(error, 10, labels = FALSE))) %>% 
  group_by(temp) %>% 
  mutate(error_group = round(mean(error), 1)) %>% 
  ungroup() %>% 
  filter(value < 6) %>% 
  ggplot(aes(x = error_group, y = value, fill = estimate, group = interaction(estimate, error_group))) +
  # geom_hline(yintercept = 0) +
  # geom_point(alpha=0.05, size=2, stroke = 0, shape=16) +
  # geom_smooth(method="loess") +
  geom_boxplot(outlier.shape=NA) +
  theme_classic() +
  scale_fill_manual(values = c("beta" = "#607D8B", "MAP" = "#795548", "Mean" = "#FF9800", "Median" = "#FFEB3B"),
                    name = "Index") +
  ylab("Point-estimate of the true value 0\n") +
  xlab("\nNoise") +
  facet_wrap(~ outcome_type * true_effect, scales="free") 

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  select(sample_size, true_effect, outcome_type, beta, Median, Mean, MAP) %>%
  gather(estimate, value, -sample_size, -true_effect, -outcome_type) %>%
  mutate(temp = as.factor(cut(sample_size, 10, labels = FALSE))) %>% 
  group_by(temp) %>% 
  mutate(size_group = round(mean(sample_size))) %>% 
  ungroup() %>% 
  filter(value < 6) %>% 
  ggplot(aes(x = size_group, y = value, fill = estimate, group = interaction(estimate, size_group))) +
  # geom_hline(yintercept = 0) +
  # geom_point(alpha=0.05, size=2, stroke = 0, shape=16) +
  # geom_smooth(method="loess") +
  geom_boxplot(outlier.shape=NA) +
  theme_classic() +
  scale_fill_manual(values = c("beta" = "#607D8B", "MAP" = "#795548", "Mean" = "#FF9800", "Median" = "#FFEB3B"),
                    name = "Index") +
  xlab("Point-estimate of the true value 0\n") +
  ylab("\nNoise") +
  facet_wrap(~ outcome_type * true_effect, scales="free")

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  select(sample_size, error, true_effect, outcome_type, beta, Median, Mean, MAP) %>%
  gather(estimate, value, -sample_size, -error, -true_effect, -outcome_type) %>%
  glm(true_effect ~ outcome_type / value * estimate * sample_size * error, data=., family="binomial") %>%
  broom::tidy() %>%
  select(term, estimate, p=p.value) %>%
  filter(stringr::str_detect(term, 'outcome_type'),
         stringr::str_detect(term, ':value')) %>%
  mutate(
    sample_size = stringr::str_detect(term, 'sample_size'),
    error = stringr::str_detect(term, 'error'),
    term = stringr::str_remove(term, "estimate"),
    term = stringr::str_remove(term, "outcome_type"),
    p = paste0(sprintf("%.2f", p), ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))) %>%
  arrange(sample_size, error, term) %>% 
  select(-sample_size, -error) %>% 
  knitr::kable(digits=2) 

## ----message=FALSE, warning=FALSE----------------------------------------
df <- read.csv("https://raw.github.com/easystats/circus/master/data/bayesSim_study2.csv")

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  select(iterations, true_effect, outcome_type, beta, Median, Mean, MAP) %>%
  gather(estimate, value, -iterations, -true_effect, -outcome_type) %>%
  mutate(temp = as.factor(cut(iterations, 5, labels = FALSE))) %>% 
  group_by(temp) %>% 
  mutate(iterations_group = round(mean(iterations), 1)) %>% 
  ungroup() %>% 
  filter(value < 6) %>%
  ggplot(aes(x = iterations_group, y = value, fill = estimate, group = interaction(estimate, iterations_group))) +
  geom_boxplot(outlier.shape=NA) +
  theme_classic() +
  scale_fill_manual(values = c("beta" = "#607D8B", "MAP" = "#795548", "Mean" = "#FF9800", "Median" = "#FFEB3B"),
                    name = "Index") +
  ylab("Point-estimate of the true value 0\n") +
  xlab("\nNumber of Iterations") +
  facet_wrap(~ outcome_type * true_effect, scales="free") 

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  mutate(warmup = warmup / iterations) %>% 
  select(warmup, true_effect, outcome_type, beta, Median, Mean, MAP) %>%
  gather(estimate, value, -warmup, -true_effect, -outcome_type) %>%
  mutate(temp = as.factor(cut(warmup, 3, labels = FALSE))) %>% 
  group_by(temp) %>% 
  mutate(warmup_group = round(mean(warmup), 1)) %>% 
  ungroup() %>% 
  filter(value < 6) %>% 
  ggplot(aes(x = warmup_group, y = value, fill = estimate, group = interaction(estimate, warmup_group))) +
  geom_boxplot(outlier.shape=NA) +
  theme_classic() +
  scale_fill_manual(values = c("beta" = "#607D8B", "MAP" = "#795548", "Mean" = "#FF9800", "Median" = "#FFEB3B"),
                    name = "Index") +
  ylab("Point-estimate of the true value 0\n") +
  xlab("\nNumber of Iterations") +
  facet_wrap(~ outcome_type * true_effect, scales="free") 

