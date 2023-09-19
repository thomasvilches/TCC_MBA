
# Pacotes -----------------------------------------------------------------

#remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)
library(tidyverse)


library(read.dbc)
library(foreign)

# install.packages("RPostgreSQL")
# install.packages("odbc")
library("RPostgreSQL")
library(odbc)

library(PNADcIBGE)

source("DB_creation/functions.R", encoding = "UTF-8")
