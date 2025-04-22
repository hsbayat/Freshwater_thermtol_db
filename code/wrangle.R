

# info --------------------------------------------------------------------

# Final processing of freshwater thermal tolerance dataset

# by H. S. Bayat

# last edited: 22/04/2024

# setup -------------------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

tol <- read.csv('data/thermtol_tests_final.csv', na.strings = "")
tax <- read.csv('data/thermtol_taxonomy_final.csv', na.strings = "")
clim <- read.csv('data/thermtol_climate_final.csv', na.strings = "")
ref <- read.csv('data/thermtol_reference_final.csv', na.strings = "")

# wrangle -----------------------------------------------------------------

# fix errors 
tol[tol$test_id == "t_3607",]$tol <- 32.1
tol[tol$test_id == "t_3608",]$tol <- 33
tol[tol$test_id == "t_3607",]$error <- 2.4
tol[tol$test_id == "t_3608",]$error <- 3.2
tol[tol$study_id == "s_317",]$n <- 30
tol[tol$test_id == "t_3608",]$acclim_time <- 72
tol[tol$test_id == "t_3608",]$acclim_time_unit <- "hours"
tol[tol$study_id == "s_475",]$group <- "fish"
tol <- tol[tol$test_id != "t_6355",]

tol[tol$test_id == "t_5271",]$tol_class <- "lower ctmin"
tol[tol$test_id == "t_5281",]$tol_class <- "lower ctmin"
tol[tol$test_id == "t_5279",]$tol_class <- "lower ctmin"
tol[tol$test_id == "t_5268",]$tol_class <- "lower ctmin"
tol[tol$test_id == "t_5271",]$tol_type <- "lower"
tol[tol$test_id == "t_5281",]$tol_type <- "lower"
tol[tol$test_id == "t_5279",]$tol_type <- "lower"
tol[tol$test_id == "t_5268",]$tol_type <- "lower"

# put units into common currency
# for error measures - on thermal tolerance, body mass, and body length

# for thermal tolerance error measure
# convert standard deviation to standard error
# convert 95% ci to standard error using appropriate t value
# note that what is entered under "95% ci" is half of the interval

# fix wrongly entered standard error values
tol[tol$test_id == "t_5828",]$error <- 0.32
tol[tol$test_id == "t_5855",]$error <- 0.48
tol[tol$test_id == "t_5859",]$error <- 0.25
tol[tol$test_id == "t_5903",]$error <- 0.3
tol[tol$test_id == "t_5928",]$error <- 0.34
tol[tol$test_id == "t_5936",]$error <- 0.28

tol <- tol %>% mutate(error_se = case_when((!is.na(n) & error_measure == "sd") ~ error/(sqrt(n)),
                                   (error_measure == "se") ~ error,
                                   (error_measure == "95% ci") ~ error/qt(p = 0.975, df = n)))
tol <- tol %>% mutate(error_sd = case_when((error_measure == "sd") ~ error,
                                           (metric_note != "lt05" & metric != "lt50" & error_measure != "sd") ~ error_se*sqrt(n)))

ch <- tol %>% filter(error_sd > 20) # check standard deviation values larger than 20
ch <- tol %>% filter(error_se > 20) # check standard error values larger than 20
ch <- tol %>% filter(error_sd > 5) # check standard deviation values larger than 5

# check distribution of error
ggplot(data = tol, aes(x = error_se)) + 
  geom_histogram(color = "black") +
  labs(x = "Standard deviation", y = "Number")   + theme_bw(base_size = 25) 

# body mass/length error measure
tol <- tol %>% mutate(error_bm_sd = case_when((error_measure_bm == "sd") ~ error,
                                           (error_measure_bm == "se") ~ error_bm*sqrt(n)))
tol <- tol %>% mutate(error_bl_sd = case_when((error_measure_bl == "sd") ~ error,
                                              (error_measure_bl == "se") ~ error_bl*sqrt(n)))

# check distributions - ones without data automatically get removed
ggplot(data = tol, aes(x = error_bm_sd)) + 
  geom_histogram(color = "black") +
  labs(x = "Standard deviation", y = "Number")   + theme_bw(base_size = 25)

# check body mass errors greater than 25 grams
ch <- tol %>% filter(error_bm_sd > 25) # all large fish species, all ok

ggplot(data = tol, aes(x = error_bl_sd)) + 
  geom_histogram(color = "black") +
  labs(x = "Standard deviation", y = "Number")   + theme_bw(base_size = 25)

# check body length errors greater than 100 mm
ch <- tol %>% filter(error_bl_sd > 100) # large fish species, all ok

# put units into common currency
# time - for acclimation and test
# put final times into minutes

# fix misentered values
tol[tol$study_id == "s_319",]$acclim_time <- 3
tol[tol$study_id == "s_319",]$acclim_time_unit <- "weeks"
tol[tol$study_id == "s_260",]$acclim_time_unit <- "hours"
tol[tol$study_id == "s_274",]$acclim_time <- 24
tol[tol$study_id == "s_274",]$acclim_time_unit <- "hours"


tol$acclim_time <- as.numeric(tol$acclim_time)
tol <- tol %>% mutate(acclim_time_minutes = case_when((acclim_time_unit == "minutes") ~ acclim_time,
                                                 (acclim_time_unit == "hours") ~ acclim_time*60,
                                                 (acclim_time_unit == "days") ~ acclim_time*1440,
                                                 (acclim_time_unit == "weeks") ~ acclim_time*10080,
                                                 (acclim_time_unit == "months") ~ acclim_time*302400))

ch <- tol %>% filter(acclim_time_minutes < 60) # look at short acclimation times
# tested streamside, directly after collection

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
tol <- tol[tol$test_id != "t_954",]
tol <- tol[tol$test_id != "t_957",]

# change one mislabelled location id
tol[tol$test_id == "t_1843",]$loc_id <- "l_1086"
clim[clim$crd_id == 74 & clim$loc_id == "l_723",]$loc_id <- "l_1086"

# remove one duplicate reference
ref <- ref[-545,]

# combine into one dataset
tt1 <- merge(tol, tax, by = "tax_id")
tt2 <- merge(tt1, clim, by = "loc_id")
dat <- merge(tt2, ref, by = "study_id")

# write to file -----------------------------------------------------------

# save the pieces
data = 'data'

write.csv(clim, file.path(data, 'thermtol_climate_final_ch.csv'), row.names = FALSE, na = "")
write.csv(ref, file.path(data, 'thermtol_reference_final_ch.csv'), row.names = FALSE, na = "")
write.csv(tol, file.path(data, 'thermtol_tests_processed_final.csv'), row.names = FALSE, na = "")
write.csv(dat, file.path(data, 'thermtol_comb_final.csv'), row.names = FALSE, na = "")

# end ---------------------------------------------------------------------

rm(list = ls())
