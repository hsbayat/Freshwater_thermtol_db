
# info --------------------------------------------------------------------

# Adding new data from revisions

# by H. S. Bayat

# last edited: 16/04/2024

# setup -------------------------------------------------------------------

# no packages needed

# load data ---------------------------------------------------------------

# new data from revisions

toln <- read.csv('data/revision/thermtol_tests_rev.csv', na.strings = "")
taxn <- read.csv('data/revision/thermtol_taxonomy_rev.csv', na.strings = "")
climn <- read.csv('data/revision/thermtol_climate_rev.csv', na.strings = "")
refn <- read.csv('data/revision/thermtol_reference_rev.csv', na.strings = "")

# previous data

tolo <- read.csv('data/initial_submission/thermtol_tests.csv', na.strings = "")
tax <- read.csv('data/initial_submission/thermtol_taxonomy.csv', na.strings = "")
clim <- read.csv('data/initial_submission/thermtol_climate.csv', na.strings = "")
ref <- read.csv('data/initial_submission/thermtol_reference.csv', na.strings = "")


# combine data ------------------------------------------------------------

tol <- rbind(tol, toln)
tax <- rbind(tax, taxn)
clim <- rbind(clim, climn)
ref <- rbind(ref, refn)

# write to file -----------------------------------------------------------

# complete data files
data = 'data'

write.csv(tol, file.path(data, 'thermtol_tests_final.csv'), row.names = FALSE, na = "")
write.csv(tax, file.path(data, 'thermtol_taxonomy_final.csv'), row.names = FALSE, na = "")
write.csv(clim, file.path(data, 'thermtol_climate_final.csv'), row.names = FALSE, na = "")
write.csv(ref, file.path(data, 'thermtol_reference_final.csv'), row.names = FALSE, na = "")

# end ---------------------------------------------------------------------

rm(list = ls())

