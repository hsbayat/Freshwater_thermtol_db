

# info --------------------------------------------------------------------

# Final processing of freshwater thermal tolerance dataset

# by H. S. Bayat

# last edited: 21/10/2024

# setup -------------------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

tol <- read.csv('data/thermtol_tests.csv')
tax <- read.csv('data/thermtol_taxonomy.csv')
clim <- read.csv('data/thermtol_climate.csv')
ref <- read.csv('data/thermtol_reference.csv')

# wrangle -----------------------------------------------------------------

# put units into common currency
# for error measures - on thermal tolerance, body mass, and body length

# for thermal tolerance error measure
# convert standard deviation to standard error
# convert 95% ci to standard error using appropriate t value
# note that what is entered under "95% ci" is half of the interval

tol <- tol %>% mutate(error_se = case_when((!is.na(n) & error_measure == "sd") ~ error/(sqrt(n)),
                                   (error_measure == "se") ~ error,
                                   (error_measure == "95% ci") ~ error/qt(p = 0.975, df = n)))
tol <- tol %>% mutate(error_sd = case_when((error_measure == "sd") ~ error,
                                           (metric_note != "lt05" & metric != "lt50" & error_measure != "sd") ~ error_se*sqrt(n)))
ch <- tol %>% filter(error_sd > 20)
tol[2945,7] <- 32.1
tol[2946,7] <- 33
tol[2945,11] <- 2.4
tol[2946,11] <- 3.2
tol[c(2945:2946),8] <- 30
tol[2946,14] <- 72
tol[2946,15] <- "hours"
ch <- tol %>% filter(error_sd > 5)

# check distribution of error
ggplot(data = tol, aes(x = error_se)) + 
  geom_histogram(color = "black") +
  labs(x = "Standard deviation", y = "Number")   + theme_bw(base_size = 25) 

# body mass/length error measure
ch <- tol %>% group_by(error_measure_bm) %>% count()
ch <- tol %>% group_by(error_measure_bl) %>% count()

tol <- tol %>% mutate(error_bm_sd = case_when((error_measure_bm == "sd") ~ error,
                                           (error_measure_bm == "se") ~ error_bm*sqrt(n)))
tol <- tol %>% mutate(error_bl_sd = case_when((error_measure_bl == "sd") ~ error,
                                              (error_measure_bl == "se") ~ error_bl*sqrt(n)))
# check distributions
ggplot(data = tol, aes(x = error_bm_sd)) + 
  geom_histogram(color = "black") +
  labs(x = "Standard deviation", y = "Number")   + theme_bw(base_size = 25)

# check errors over 25
ch <- tol %>% filter(error_bm_sd > 25) # all large fish species, all ok

ggplot(data = tol, aes(x = error_bl_sd)) + 
  geom_histogram(color = "black") +
  labs(x = "Standard deviation", y = "Number")   + theme_bw(base_size = 25)

# put units into common currency
# time - for acclimation and test
# put final times into minutes

tol$acclim_time <- as.numeric(tol$acclim_time)
tol <- tol %>% mutate(acclim_time_minutes = case_when((acclim_time_unit == "minutes") ~ acclim_time,
                                                 (acclim_time_unit == "hours") ~ acclim_time*60,
                                                 (acclim_time_unit == "days") ~ acclim_time*1440,
                                                 (acclim_time_unit == "weeks") ~ acclim_time*10080,
                                                 (acclim_time_unit == "months") ~ acclim_time*302400))
ch <- tol %>% filter(acclim_time_minutes < 60) # look at short acclimation times

# fix misentered values
tol[c(471:477),14] <- 3
tol[c(471:477),15] <- "weeks"
tol[c(2226,2287,3081,3112,3188,3189),15] <- "hours"
tol[c(3068:3078),14] <- 24
tol[c(3068:3078),15] <- "hours"
# all of the rest are tested streamside, directly after collection

tol <- tol %>% mutate(acclim_time_minutes = case_when((acclim_time_unit == "minutes") ~ acclim_time,
                                                      (acclim_time_unit == "hours") ~ acclim_time*60,
                                                      (acclim_time_unit == "days") ~ acclim_time*1440,
                                                      (acclim_time_unit == "weeks") ~ acclim_time*10080,
                                                      (acclim_time_unit == "months") ~ acclim_time*302400))

# put units into common currency
# for test time
tol <- tol %>% mutate(test_time_minutes1 = case_when((metric == "ltmax") ~ (tol - acclim_temp)/ramp,
                                              (metric == "ltmin") ~ (acclim_temp - tol)/ramp,
                                              (metric == "ctmin") ~ (acclim_temp - tol)/ramp,
                                              (metric == "ctmax") ~ (tol - acclim_temp)/ramp,
                                              (test_time_unit == "minutes") ~ test_time,
                                              (test_time_unit == "hours") ~ test_time*60,
                                              (test_time_unit == "days") ~ test_time*1440,
                                              (test_time_unit == "weeks") ~ test_time*10080,
                                              (test_time_unit == "months") ~ test_time*302400))
tol <- tol %>% mutate(test_time_minutes = case_when((!is.na(test_time_minutes1)) ~ test_time_minutes1, 
                                                    (is.na(test_time_minutes1) & metric == "ctmax") ~ (tol - start_temp)/ramp,
                                                    (is.na(test_time_minutes1) & metric == "ltmax") ~ (tol - start_temp)/ramp,
                                                    (is.na(test_time_minutes1) & metric == "ctmin") ~ (start_temp - tol)/ramp,
                                                    (is.na(test_time_minutes1) & metric == "ltmin") ~ (start_temp - tol)/ramp))
tol <- tol[,-74] # remove unneeded column

# remove two records of terrestrial imagines
tol <- tol[-c(638,641),]

# combine into one dataset
tt1 <- merge(tol, tax, by = "tax_id")
tt2 <- merge(tt1, clim, by = "loc_id")
dat <- merge(tt2, ref, by = "study_id")


# explore -----------------------------------------------------------------


# how many species for each metric
dat %>% group_by(tol_class, taxon) %>% count() %>% group_by(tol_class) %>% count()

# how many records for each metric
dat %>% group_by(tol_class) %>% count()

# how many species have data for ctmax and lt50
# make identifier for study, taxon, and location

lt <- dat %>% filter(tol_class == "upper lt50") 
lt <- lt %>% mutate(id = paste(study_id, tax_id, loc_id, acclim_temp, sep = "-"))
lt <- lt[,c(107,68,6,9,10,15,38,40,41)]

ct <- dat %>% filter(tol_class == "upper ctmax") 
ct <- ct %>% mutate(id = paste(study_id, tax_id, loc_id, acclim_temp, sep = "-"))
ct <- ct[,c(107,68,6,9,10,15,38,40,41)]

t <- merge(lt, ct, by = "id") # 105 lt50 and ctmax pairs
# same species, collection location, study, and acclimation temperature
tt <- unique(t$taxon.x) # for 24 taxa

# how many species have data for both
# not restricted to study, location, acclimation temperature

lt <- dat %>% filter(tol_class == "upper lt50") %>% group_by(tax_id, taxon) %>% count()
ct <- dat %>% filter(tol_class == "upper ctmax") %>% group_by(tax_id, taxon) %>% count()

ch <- merge(lt, ct, by = "tax_id") # 66 taxa have both lt50 and ctmax data


# write to file -----------------------------------------------------------

# save the pieces
data = 'data'

write.csv(tol, file.path(data, 'thermtol_tests_processed.csv'), row.names = FALSE, na = "")
write.csv(dat, file.path(data, 'thermtol_comb.csv'), row.names = FALSE, na = "")

# end ---------------------------------------------------------------------

rm(list = ls())
