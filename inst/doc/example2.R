## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
library(bayestestR)
data(iris)
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
options(digits=2)

set.seed(333)

## ----message=FALSE, warning=FALSE, eval=TRUE-----------------------------
result <- cor.test(iris$Sepal.Width, iris$Sepal.Length)
result

## ----message=FALSE, warning=FALSE, results='hide'------------------------
library(BayesFactor)
result <- correlationBF(iris$Sepal.Width, iris$Sepal.Length)

## ----message=FALSE, warning=FALSE, eval=FALSE----------------------------
#  describe_posterior(result)

## ----echo=FALSE----------------------------------------------------------
structure(list(Parameter = "rho", Median = -0.114149129692488, 
    CI = 89, CI_low = -0.240766308855643, CI_high = 0.00794997655649642, 
    pd = 91.6, ROPE_CI = 89, ROPE_low = -0.1, ROPE_high = 0.1, 
    ROPE_Percentage = 42.0949171581017, BF = 0.509017511647702, 
    Prior_Distribution = "cauchy", Prior_Location = 0, Prior_Scale = 0.333333333333333), row.names = 1L, class = "data.frame")

## ----message=FALSE, warning=FALSE, eval=TRUE-----------------------------
bayesfactor(result)

