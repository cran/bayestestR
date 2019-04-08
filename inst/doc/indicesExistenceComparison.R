## ----message=FALSE, warning=FALSE----------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("https://raw.github.com/easystats/circus/master/data/bayes_indices.csv")

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  select(noise, effect, sample_size, p_frequentist, p_direction, p_map, p_rope, rope, rope_full) %>%
  gather(index, value, -noise, -sample_size, -effect) %>%
  mutate(noise = as.factor(noise),
         effect = as.factor(effect),
         sample_size = as.factor(sample_size),
         index = factor(index, levels=c("p_frequentist", "p_direction", "p_map", "p_rope", "rope", "rope_full"))) %>%
  ggplot(aes(x = noise, y=value, color = effect, fill=index)) +
  geom_jitter(shape=16, alpha=0.02) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~index, scales = "free") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_color_manual(values = c(`0` = "#f44336", `1` = "#8BC34A", name="Effect")) +
  scale_fill_manual(values = c("p_frequentist"="#607D8B", "p_map" = "#E91E63", "p_direction" = "#2196F3",
                               "rope" = "#FF9800", "rope_full" = "#FF5722", "p_rope"="#FFC107"), guide=FALSE) +
  ylab("Index Value\n") +
  xlab("\nNoise")

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  select(noise, effect, sample_size, p_frequentist, p_direction, p_map, p_rope, rope, rope_full) %>%
  gather(index, value, -noise, -sample_size, -effect) %>%
  mutate(sample_size = as.factor(sample_size),
         effect = as.factor(effect),
         index = factor(index, levels=c("p_frequentist", "p_direction", "p_map", "p_rope", "rope", "rope_full"))) %>%
  ggplot(aes(x = sample_size, y=value, color = effect, fill=index)) +
  geom_jitter(shape=16, alpha=0.02) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~index, scales = "free") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_color_manual(values = c(`0` = "#f44336", `1` = "#8BC34A", name="Effect")) +
  scale_fill_manual(values = c("p_frequentist"="#607D8B", "p_map" = "#E91E63", "p_direction" = "#2196F3",
                               "rope" = "#FF9800", "rope_full" = "#FF5722", "p_rope"="#FFC107"), guide=FALSE) +
  ylab("Index Value\n") +
  xlab("\nSample Size")

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  select(noise, effect, sample_size, p_frequentist, p_direction, p_map, p_rope, rope, rope_full, prior_correct) %>%
  gather(index, value, -noise, -sample_size, -effect, -prior_correct) %>%
  mutate(sample_size = as.factor(sample_size),
         effect = as.factor(effect),
         prior_correct = as.factor(prior_correct),
         index = factor(index, levels=c("p_frequentist", "p_direction", "p_map", "p_rope", "rope", "rope_full"))) %>%
  ggplot(aes(x = effect, y=value, color = prior_correct, fill=index)) +
  geom_jitter(shape=16, alpha=0.02) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~index, scales = "free") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_color_manual(values = c(`0` = "#f44336", `1` = "#8BC34A", name="Correct Prior")) +
  scale_fill_manual(values = c("p_frequentist"="#607D8B", "p_map" = "#E91E63", "p_direction" = "#2196F3",
                               "rope" = "#FF9800", "rope_full" = "#FF5722", "p_rope"="#FFC107"), guide=FALSE) +
  ylab("Index Value\n") +
  xlab("\nEffect")

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  mutate(p_frequentist = scale(p_frequentist),
         p_direction = scale(p_direction),
         p_map = scale(p_map),
         p_rope = scale(p_rope),
         rope = scale(rope),
         rope_full = scale(rope_full)) %>%
  select(noise, effect, sample_size, p_frequentist, p_direction, p_map, p_rope, rope, rope_full, prior_correct) %>%
  gather(index, value, -noise, -sample_size, -effect, -prior_correct) %>%
  mutate(sample_size = scale(sample_size),
         noise=scale(noise),
         effect = as.factor(effect),
         prior_correct = as.factor(prior_correct),
         index = factor(index, levels=c("p_frequentist", "p_direction", "p_map", "p_rope", "rope", "rope_full"))) %>%
  glm(effect ~ index/value * noise * sample_size * prior_correct, data=., family="binomial") %>%
  broom::tidy() %>%
  select(term, estimate, p=p.value) %>%
  filter(stringr::str_detect(term, 'value')) %>%
  mutate(term = stringr::str_remove(term, ":value"),
         term = stringr::str_remove(term, "index"),
         p = ifelse(p < .001, "< .001***", ifelse(p < .01, "< .01**", ifelse(p < .05, "< .05*", "> .05")))) %>%
  knitr::kable(digits=2)

## ---- message=FALSE, warning=FALSE---------------------------------------
df %>%
  select(noise, sample_size, p_frequentist, p_direction, p_map, p_rope, rope, rope_full, effect) %>%
  gather(index, value, -noise, -p_frequentist, -sample_size, -effect) %>%
  mutate(effect = as.factor(effect),
         sample_size = as.factor(sample_size),
         index = factor(index, levels=c("p_frequentist", "p_direction", "p_map", "p_rope", "rope", "rope_full"))) %>%
  ggplot(aes(x = p_frequentist, y = value, color = effect, shape=sample_size)) +
  geom_point(alpha=0.025) +
  facet_wrap(~index, scales = "free") +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_color_manual(values = c(`0` = "#f44336", `1` = "#8BC34A"), name="Effect") +
  guides(colour = guide_legend(override.aes = list(alpha = 1)),
         shape = guide_legend(override.aes = list(alpha = 1), title="Sample Size"))

## ---- message=FALSE, warning=FALSE, fig.height=15, fig.width=10----------
df$sig_1 <- factor(ifelse(df$p_frequentist >= .1, "n.s.", "-"), levels=c("n.s.", "-"))
df$sig_05 <- factor(ifelse(df$p_frequentist >= .05, "n.s.", "*"), levels=c("n.s.", "*"))
df$sig_01 <- factor(ifelse(df$p_frequentist >= .01, "n.s.", "**"), levels=c("n.s.", "**"))
df$sig_001 <- factor(ifelse(df$p_frequentist >= .001, "n.s.", "***"), levels=c("n.s.", "***"))


get_data <- function(predictor, outcome, lbound=0, ubound=0.3){
  fit <- glm(paste(outcome, "~", predictor), data=df, family = "binomial")
  data <- data.frame(x=1:100)
  data[predictor] <- seq(lbound, ubound, length.out = 100)
  data$index <- predictor
  predict_fit <- predict(fit, newdata=data, type="response", se.fit = TRUE)
  data[outcome] <- predict_fit$fit 
  data$CI_lower <- predict_fit$fit - (qnorm(0.99) * predict_fit$se.fit)
  data$CI_upper <- predict_fit$fit + (qnorm(0.99) * predict_fit$se.fit)
  data <- select_(data, "value"=predictor, outcome, "index", "CI_lower", "CI_upper")
  return(data)
}



rbind(
  rbind(
    get_data(predictor="p_map", outcome="sig_001", lbound=0, ubound=0.02),
    get_data(predictor="p_direction", outcome="sig_001", lbound=99.5, ubound=100),
    get_data(predictor="rope", outcome="sig_001", lbound=0, ubound=0.5),
    get_data(predictor="rope_full", outcome="sig_001", lbound=0, ubound=0.5),
    get_data(predictor="p_rope", outcome="sig_001", lbound=95, ubound=100)
    ) %>% 
    rename("sig"=sig_001) %>% 
    mutate(threshold="p < .001"),
  rbind(
    get_data(predictor="p_map", outcome="sig_01", lbound=0, ubound=0.1),
    get_data(predictor="p_direction", outcome="sig_01", lbound=98, ubound=100),
    get_data(predictor="rope", outcome="sig_01", lbound=0, ubound=2),
    get_data(predictor="rope_full", outcome="sig_01", lbound=0, ubound=2),
    get_data(predictor="p_rope", outcome="sig_01", lbound=90, ubound=100)
    ) %>% 
    rename("sig"=sig_01) %>% 
    mutate(threshold="p < .01"),
  rbind(
    get_data(predictor="p_map", outcome="sig_05", lbound=0, ubound=0.3),
    get_data(predictor="p_direction", outcome="sig_05", lbound=95, ubound=100),
    get_data(predictor="rope", outcome="sig_05", lbound=0, ubound=10),
    get_data(predictor="rope_full", outcome="sig_05", lbound=0, ubound=10),
    get_data(predictor="p_rope", outcome="sig_05", lbound=70, ubound=100)
    ) %>%  
    rename("sig"=sig_05) %>% 
    mutate(threshold="p < .05"),
  rbind(
    get_data(predictor="p_map", outcome="sig_1", lbound=0, ubound=0.5),
    get_data(predictor="p_direction", outcome="sig_1", lbound=90, ubound=100),
    get_data(predictor="rope", outcome="sig_1", lbound=0, ubound=20),
    get_data(predictor="rope_full", outcome="sig_1", lbound=0, ubound=20),
    get_data(predictor="p_rope", outcome="sig_1", lbound=0, ubound=100)
    ) %>% 
    rename("sig"=sig_1) %>% 
    mutate(threshold="p < .1")
) %>% 
  mutate(index = as.factor(index)) %>%
  ggplot(aes(x=value, y=sig)) +
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), alpha=0.1) +
  geom_line(aes(color=index), size=1) +
  facet_wrap(~ index * threshold, scales = "free", nrow=5) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_color_manual(values = c("p_frequentist"="#607D8B", "p_map" = "#E91E63", "p_direction" = "#2196F3",
                               "rope" = "#FF9800", "rope_full" = "#FF5722", "p_rope"="#FFC107"), guide=FALSE) +
  ylab("Probability of being significant\n") +
  xlab("\nIndex Value")

## ---- message=FALSE, warning=FALSE, fig.height=15, fig.width=10----------
df$equivalence <- factor(ifelse(df$rope == 0, "significant", "n.s."), levels=c("n.s.", "significant"))

rbind(
  get_data(predictor="p_map", outcome="equivalence", lbound=0, ubound=0.4),
  get_data(predictor="p_direction", outcome="equivalence", lbound=92.5, ubound=100),
  get_data(predictor="rope_full", outcome="equivalence", lbound=0, ubound=7),
  get_data(predictor="p_frequentist", outcome="equivalence", lbound=0, ubound=0.15)
) %>% 
  ggplot(aes(x=value, y=equivalence)) +
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), alpha=0.1) +
  geom_line(aes(color=index), size=1) +
  facet_wrap(~ index, scales = "free", nrow=5) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_color_manual(values = c("p_frequentist"="#607D8B", "p_map" = "#E91E63", "p_direction" = "#2196F3",
                               "rope" = "#FF9800", "rope_full" = "#FF5722", "p_rope"="#FFC107"), guide=FALSE) +
  ylab("Probability of rejecting H0 with the equivalence test\n") +
  xlab("\nIndex Value")


## ---- message=FALSE, warning=FALSE, fig.height=6, fig.width=10.08--------
df %>%
  mutate(effect = as.factor(effect)) %>% 
  ggplot(aes(x=p_direction, y=rope_full, color=effect)) +
  geom_point(alpha=0.025) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_color_manual(values = c(`0` = "#f44336", `1` = "#8BC34A"), name="Effect") +
  ylab("ROPE (full)\n") +
  xlab("\nIndex Value")

