## ----message=FALSE, warning=FALSE----------------------------------------
library(rstanarm)
library(bayestestR)

## ----message=FALSE, warning=FALSE, eval=FALSE----------------------------
#  model <- stan_glm(Sepal.Length ~ Petal.Length, data=iris)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA, results='hide'----
library(rstanarm)
set.seed(333)

model <- stan_glm(Sepal.Length ~ Petal.Length, data=iris)

## ----message=FALSE, warning=FALSE, eval=FALSE----------------------------
#  posteriors <- insight::get_parameters(model)

## ----message=FALSE, warning=FALSE, eval=FALSE----------------------------
#  plot(density(posteriors$Petal.Length))

