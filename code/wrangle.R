

# info --------------------------------------------------------------------

# Final processing of freshwater thermal tolerance dataset

# Helena S. Bayat

# last edited: 01/10/2024

# setup -------------------------------------------------------------------
library(tidyverse)

library(patchwork)
library(ggridges)
library(viridis)

library(data.table)

# load data ---------------------------------------------------------------

tt <- fread(file.path(data, 'thermtol_tests.csv'), na.strings = '', encoding = "UTF-8")

tol <- read.csv('data/thermtol_tests.csv')
tax <- read.csv('data/thermtol_taxonomy.csv')
clim <- read.csv('data/thermtol_climate.csv')
ref <- read.csv('data/thermtol_reference.csv')

tt1 <- merge(tol, tax, by = "tax_id")
tt2 <- merge(tt1, clim, by = "loc_id")
dat <- merge(tt2, ref, by = "study_id")

# wrangle -----------------------------------------------------------------

# put units into common currency

# for error measures - on thermal tolerance, body mass, and body length

# time - for acclimation and test


# write to file -----------------------------------------------------------

# save the pieces
data = 'data'
write.csv(tax, file.path(data, 'thermtol_taxonomy.csv'), row.names = FALSE, na = "")
write.csv(tol, file.path(data, 'thermtol_tests.csv'), row.names = FALSE, na = "")
write.csv(clim, file.path(data, 'thermtol_climate.csv'), row.names = FALSE, na = "")
write.csv(ref, file.path(data, 'thermtol_reference.csv'), row.names = FALSE, na = "")


# end ---------------------------------------------------------------------

rm(list = ls())
