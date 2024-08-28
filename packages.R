## library() calls go here
library(targets)
library(tarchetypes)

# conflicts and other options
library(conflicted)
conflict_prefer("filter", "dplyr", quiet = TRUE)
options(usethis.quiet = TRUE)

# packages for this analysis
suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
  library(patchwork)
  library(ggtext)
  library(scales)
  library(broom)
  library(boot)
  library(readxl)
  library(quantreg)
  library(fixest)
})