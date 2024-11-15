
# 1. LIBRARIES----

options(timeout = max(300, getOption("timeout")))
install.packages("devtools")
devtools::install_github("paezha/truchet")

library(dplyr)
library(gganimate)
library(ggplot2)
library(sf)
library(truchet)


