## ----echo=FALSE, fig.cap="Accurate depiction of a regular Bayesian user estimating a credible interval.", fig.align='center', out.width="50%"----
knitr::include_graphics("https://github.com/easystats/bayestestR/raw/master/man/figures/bayesianMaster.png")

## ----eval=FALSE, message=FALSE, warning=FALSE, eval=FALSE----------------
#  install.packages("devtools")
#  devtools::install_github("easystats/easystats")

## ----message=FALSE, warning=FALSE, eval=FALSE----------------------------
#  install.packages("rstanarm")
#  library(rstanarm)

## ----message=FALSE, warning=FALSE, eval=FALSE----------------------------
#  model <- lm(Sepal.Length ~ Petal.Length, data=iris)
#  summary(model)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA----------------
library(dplyr)

lm(Sepal.Length ~ Petal.Length, data=iris) %>% 
  summary()

## ----message=FALSE, warning=FALSE, eval=FALSE----------------------------
#  model <- stan_glm(Sepal.Length ~ Petal.Length, data=iris)
#  summary(model)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA, results='hide'----
library(rstanarm)
set.seed(333)

model <- stan_glm(Sepal.Length ~ Petal.Length, data=iris)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA----------------
model %>% 
  summary()

