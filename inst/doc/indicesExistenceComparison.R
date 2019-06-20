## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
options(digits=2)

## ----message=FALSE, warning=FALSE----------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(see)

df <- read.csv("https://raw.github.com/easystats/circus/master/data/bayesSim_study1.csv")

## ---- message=FALSE, warning=FALSE, fig.height=25, fig.width=15----------
df %>%
  select(outcome_type, true_effect, error, sample_size, p_value, p_direction, p_MAP, p_ROPE, ROPE_89, ROPE_95, ROPE_full, BF_log) %>%
  gather(index, value, -error, -sample_size, -true_effect, -outcome_type) %>%
  mutate(true_effect = as.factor(true_effect),
         index = factor(index, levels=c("p_value", "p_direction", "p_MAP", "p_ROPE", "ROPE_89", "ROPE_95", "ROPE_full", "BF_log"))) %>%
  mutate(temp = as.factor(cut(error, 10, labels = FALSE))) %>% 
  group_by(temp) %>% 
  mutate(error_group = round(mean(error), 1)) %>% 
  ungroup() %>% 
  ggplot(aes(x = error_group, y = value, fill = index, colour=true_effect, group = interaction(index, true_effect, error_group))) +
  # geom_jitter(shape=16, alpha=0.02) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~index * outcome_type, scales = "free", ncol=2) +
  theme_modern() +
  scale_color_manual(values = c(`0` = "#f44336", `1` = "#8BC34A", name="Effect")) +
  scale_fill_manual(values = c("p_value"="#607D8B", "p_MAP" = "#4CAF50", "p_direction" = "#2196F3",
                               "ROPE_89" = "#FFC107", "ROPE_95" = "#FF9800", "ROPE_full" = "#FF5722",
                               "p_ROPE"="#E91E63", "BF_log"="#9C27B0"), guide=FALSE) +
  ylab("Index Value") +
  xlab("Noise")

## ---- message=FALSE, warning=FALSE, fig.height=25, fig.width=15----------
df %>%
  select(outcome_type, true_effect, error, sample_size, p_value, p_direction, p_MAP, p_ROPE, ROPE_89, ROPE_95, ROPE_full, BF_log) %>%
  gather(index, value, -error, -sample_size, -true_effect, -outcome_type) %>%
  mutate(true_effect = as.factor(true_effect),
         index = factor(index, levels=c("p_value", "p_direction", "p_MAP", "p_ROPE", "ROPE_89", "ROPE_95", "ROPE_full", "BF_log"))) %>%
  mutate(temp = as.factor(cut(sample_size, 10, labels = FALSE))) %>% 
  group_by(temp) %>% 
  mutate(size_group = round(mean(sample_size))) %>% 
  ungroup() %>% 
  ggplot(aes(x = size_group, y = value, fill = index, colour=true_effect, group = interaction(index, true_effect, size_group))) +
  # geom_jitter(shape=16, alpha=0.02) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~index * outcome_type, scales = "free", ncol=2) +
  theme_modern() +
  scale_color_manual(values = c(`0` = "#f44336", `1` = "#8BC34A", name="Effect")) +
  scale_fill_manual(values = c("p_value"="#607D8B", "p_MAP" = "#4CAF50", "p_direction" = "#2196F3",
                               "ROPE_89" = "#FFC107", "ROPE_95" = "#FF9800", "ROPE_full" = "#FF5722",
                               "p_ROPE"="#E91E63", "BF_log"="#9C27B0"), guide=FALSE) +
  ylab("Index Value") +
  xlab("Sample Size")

## ---- message=FALSE, warning=FALSE, fig.height=25, fig.width=15----------
df %>%
  select(outcome_type, true_effect, error, sample_size, p_value, p_direction, p_MAP, p_ROPE, ROPE_89, ROPE_95, ROPE_full, BF_log) %>%
  gather(index, value, -error, -sample_size, -true_effect, -outcome_type, -p_value) %>%
  mutate(true_effect = as.factor(true_effect),
         index = factor(index, levels=c("p_direction", "p_MAP", "p_ROPE", "ROPE_89", "ROPE_95", "ROPE_full", "BF_log"))) %>%
  mutate(temp = as.factor(cut(sample_size, 3, labels = FALSE))) %>% 
  group_by(temp) %>% 
  mutate(size_group = as.character(round(mean(sample_size)))) %>% 
  ungroup() %>% 
  ggplot(aes(x = p_value, y = value, color = true_effect, shape=size_group)) +
  geom_point(alpha=0.025, stroke = 0, shape=16) +
  facet_wrap(~index * outcome_type, scales = "free", ncol=2) +
  theme_modern() +
  scale_color_manual(values = c(`0` = "#f44336", `1` = "#8BC34A"), name="Effect") +
  guides(colour = guide_legend(override.aes = list(alpha = 1)),
         shape = guide_legend(override.aes = list(alpha = 1), title="Sample Size"))

## ---- message=FALSE, warning=FALSE, fig.height=15, fig.width=10----------
df$sig_1 <- factor(ifelse(df$p_value >= .1, "n.s.", "-"), levels=c("n.s.", "-"))
df$sig_05 <- factor(ifelse(df$p_value >= .05, "n.s.", "*"), levels=c("n.s.", "*"))
df$sig_01 <- factor(ifelse(df$p_value >= .01, "n.s.", "**"), levels=c("n.s.", "**"))
df$sig_001 <- factor(ifelse(df$p_value >= .001, "n.s.", "***"), levels=c("n.s.", "***"))


get_data <- function(predictor, outcome, lbound=0, ubound=0.3){
  fit <- suppressWarnings(glm(paste(outcome, "~ outcome_type * ", predictor), data=df, family = "binomial"))
  # data <- data.frame(x=rep(1:100, 2))
  data <- data.frame(outcome_type=rep(c("linear", "binary"), each=100))
  data[predictor] <- rep(seq(lbound, ubound, length.out = 100), 2)
  data$index <- predictor
  predict_fit <- predict(fit, newdata=data, type="response", se.fit = TRUE)
  data[outcome] <- predict_fit$fit 
  data$CI_lower <- predict_fit$fit - (qnorm(0.99) * predict_fit$se.fit)
  data$CI_upper <- predict_fit$fit + (qnorm(0.99) * predict_fit$se.fit)
  data <-
    select(
      data,
      "value" := !!predictor,
      !!outcome,
      .data$outcome_type,
      .data$index,
      .data$CI_lower,
      .data$CI_upper
    )
  return(data)
}



rbind(
  rbind(
    get_data(predictor="p_direction", outcome="sig_001", lbound=99.5, ubound=100),
    get_data(predictor="p_MAP", outcome="sig_001", lbound=0, ubound=0.01),
    get_data(predictor="p_ROPE", outcome="sig_001", lbound=97, ubound=100),
    get_data(predictor="ROPE_89", outcome="sig_001", lbound=0, ubound=0.5),
    get_data(predictor="ROPE_95", outcome="sig_001", lbound=0, ubound=0.5),
    get_data(predictor="ROPE_full", outcome="sig_001", lbound=0, ubound=0.5),
    get_data(predictor="BF_log", outcome="sig_001", lbound=0, ubound=10)
    ) %>% 
    rename("sig"=sig_001) %>% 
    mutate(threshold="p < .001"),
  rbind(
    get_data(predictor="p_direction", outcome="sig_01", lbound=98, ubound=100),
    get_data(predictor="p_MAP", outcome="sig_01", lbound=0, ubound=0.1),
    get_data(predictor="p_ROPE", outcome="sig_01", lbound=85, ubound=100),
    get_data(predictor="ROPE_89", outcome="sig_01", lbound=0, ubound=2),
    get_data(predictor="ROPE_95", outcome="sig_01", lbound=0, ubound=2),
    get_data(predictor="ROPE_full", outcome="sig_01", lbound=0, ubound=2),
    get_data(predictor="BF_log", outcome="sig_01", lbound=0, ubound=5)
    ) %>% 
    rename("sig"=sig_01) %>% 
    mutate(threshold="p < .01"),
  rbind(
    get_data(predictor="p_direction", outcome="sig_05", lbound=95, ubound=100),
    get_data(predictor="p_MAP", outcome="sig_05", lbound=0, ubound=0.3),
    get_data(predictor="p_ROPE", outcome="sig_05", lbound=50, ubound=100),
    get_data(predictor="ROPE_89", outcome="sig_05", lbound=0, ubound=10),
    get_data(predictor="ROPE_95", outcome="sig_05", lbound=0, ubound=10),
    get_data(predictor="ROPE_full", outcome="sig_05", lbound=0, ubound=10),
    get_data(predictor="BF_log", outcome="sig_05", lbound=0, ubound=2)
    ) %>%  
    rename("sig"=sig_05) %>% 
    mutate(threshold="p < .05"),
  rbind(
    get_data(predictor="p_direction", outcome="sig_1", lbound=90, ubound=100),
    get_data(predictor="p_MAP", outcome="sig_1", lbound=0, ubound=0.5),
    get_data(predictor="p_ROPE", outcome="sig_1", lbound=25, ubound=100),
    get_data(predictor="ROPE_89", outcome="sig_1", lbound=0, ubound=20),
    get_data(predictor="ROPE_95", outcome="sig_1", lbound=0, ubound=20),
    get_data(predictor="ROPE_full", outcome="sig_1", lbound=0, ubound=20),
    get_data(predictor="BF_log", outcome="sig_1", lbound=0, ubound=1)
    ) %>% 
    rename("sig"=sig_1) %>% 
    mutate(threshold="p < .1")
) %>% 
  mutate(index = as.factor(index)) %>%
  ggplot(aes(x=value, y=sig)) +
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), alpha=0.1) +
  geom_line(aes(color=index), size=1) +
  facet_wrap(~ index * threshold * outcome_type, scales = "free", ncol=8) +
  theme_modern() +
  scale_color_manual(values = c("p_value"="#607D8B", "p_MAP" = "#4CAF50", "p_direction" = "#2196F3",
                               "ROPE_89" = "#FFC107", "ROPE_95" = "#FF9800", "ROPE_full" = "#FF5722",
                               "p_ROPE"="#E91E63", "BF_log"="#9C27B0"), guide=FALSE) +
  ylab("Probability of being significant") +
  xlab("Index Value")

## ---- message=FALSE, warning=FALSE, fig.height=15, fig.width=10----------
df$equivalence_95 <- factor(ifelse(df$ROPE_95 == 0, "significant", "n.s."), levels=c("n.s.", "significant"))
df$equivalence_89 <- factor(ifelse(df$ROPE_89 == 0, "significant", "n.s."), levels=c("n.s.", "significant"))


rbind(
  rbind(
    get_data(predictor="p_direction", outcome="equivalence_95", lbound=97.5, ubound=100),
    get_data(predictor="p_MAP", outcome="equivalence_95", lbound=0, ubound=0.2),
    get_data(predictor="p_ROPE", outcome="equivalence_95", lbound=92.5, ubound=97.5),
    get_data(predictor="ROPE_89", outcome="equivalence_95", lbound=0, ubound=3),
    get_data(predictor="ROPE_full", outcome="equivalence_95", lbound=0, ubound=3),
    get_data(predictor="BF_log", outcome="equivalence_95", lbound=0.5, ubound=2.5)
    ) %>% 
    rename("equivalence"=equivalence_95) %>% 
    mutate(level="95 HDI"),
  rbind(
    get_data(predictor="p_direction", outcome="equivalence_89", lbound=99.5, ubound=100),
    get_data(predictor="p_MAP", outcome="equivalence_89", lbound=0, ubound=0.02),
    get_data(predictor="p_ROPE", outcome="equivalence_89", lbound=0, ubound=100),
    get_data(predictor="ROPE_95", outcome="equivalence_89", lbound=0, ubound=0.5),
    get_data(predictor="ROPE_full", outcome="equivalence_89", lbound=0, ubound=0.5),
    get_data(predictor="BF_log", outcome="equivalence_89", lbound=0, ubound=3)
    ) %>% 
    rename("equivalence"=equivalence_89) %>% 
    mutate(level="90 HDI")
) %>% 
  ggplot(aes(x=value, y=equivalence)) +
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), alpha=0.1) +
  geom_line(aes(color=index), size=1) +
  facet_wrap(~ index * level * outcome_type, scales = "free", nrow=7) +
  theme_modern() +
  scale_color_manual(values = c("p_value"="#607D8B", "p_MAP" = "#4CAF50", "p_direction" = "#2196F3",
                               "ROPE_89" = "#FFC107", "ROPE_95" = "#FF9800", "ROPE_full" = "#FF5722",
                               "p_ROPE"="#E91E63", "BF_log"="#9C27B0"), guide=FALSE) +
  ylab("Probability of rejecting H0 with the equivalence test") +
  xlab("Index Value")

## ---- message=FALSE, warning=FALSE, fig.height=6, fig.width=10.08--------
df %>%
  mutate(true_effect = as.factor(true_effect)) %>% 
  ggplot(aes(x=p_direction, y=ROPE_full, color=true_effect)) +
  geom_point(alpha=0.025, stroke = 0, shape=16) +
  facet_wrap(~ outcome_type, scales = "free", ncol=2) +
  theme_modern() +
  scale_color_manual(values = c(`0` = "#f44336", `1` = "#8BC34A"), name="Effect") +
  ylab("ROPE (full)") +
  xlab("Probability of Direction (pd)")

