## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)
library(pairwiseLLM)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(knitr)

## -----------------------------------------------------------------------------
td <- trait_description("overall_quality")
td
