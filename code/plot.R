
# info --------------------------------------------------------------------

# Plots to showcase and explore freshwater thermal tolerance data

# by H. S. Bayat

# last edited: 08/07/2025

# setup -------------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(ggridges)
library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# load data ---------------------------------------------------------------

dat <- read.csv('data/thermtol_comb_final.csv', na.strings = "")
ref <- read.csv('data/thermtol_reference_final_ch.csv', na.strings = "")

# plot --------------------------------------------------------------------

# Figures 2, 3, 4, and 5

# Figure 2 ----------------------------------------------------------------
# map of CTmax data

ctmax <- dat %>% filter(metric == "ctmax")
p2 <- st_as_sf(ctmax, coords = c("long", "lat"), crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")

fig2 <- 
  ggplot(data = world) + geom_sf() + geom_sf(data = p2, aes(fill = tol, pch = group), size = 1.6, alpha = 0.55, stroke = 0.25) + 
  scale_shape_manual(values = c(21,24)) + 
  scale_fill_viridis(option = "inferno", begin = 0, end = 0.98) + 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(fill = "CTmax (°C)", pch = "Group") +
  theme_bw(base_size = 10)

ggsave(filename = 'figure2.png', path = 'figures', width = 1880, height = 1000, units = 'px', dpi = 'print', bg = "white")


# Figure 3 ----------------------------------------------------------------
# distribution of different metrics - ridgeline plot

dat <- dat %>% mutate(tol_class = case_when(tol_class == "upper other" ~ "Upper other", 
                                            tol_class == "upper ltmax" ~ "Upper LTmax",
                                            tol_class == "upper lt50" ~ "Upper LT50",
                                            tol_class == "upper ctmax" ~ "Upper CTmax",
                                            tol_class == "lower other" ~ "Lower other",
                                            tol_class == "lower ltmin" ~ "Lower LTmin",
                                            tol_class == "lower lt50" ~ "Lower LT50",
                                            tol_class == "lower ctmin" ~ "Lower CTmin"))

fig3 <- 
  ggplot(dat = dat, aes(x = tol, y = tol_class, fill = after_stat(x))) + 
  geom_density_ridges_gradient(jittered_points = TRUE, position = position_points_sina(rel_min = 0.05, rel_max = 0.95, seed = NULL), scale = 2, rel_min_height = 0.0001, point_size = 0.05, point_alpha = 0.35, linewidth = 0.4) +
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(0,0)) + 
  #scale_x_continuous
  labs(x = "Temperature (°C)", y = "Thermal tolerance metric") +
  scale_fill_viridis(name = "°C", option = "C") +
  theme_ridges(font_size = 15, center_axis_labels = TRUE) 

# save Figure 3
ggsave(filename = 'figure3.png', path = 'figures', width = 1880, height = 1320, units = 'px', dpi = 'print', bg = "white")


# Figure 4 ----------------------------------------------------------------

# Figure 4a - ridgeline plot (with bars) for data grouped by Köppen-Geiger classification
# scaled by sample size

fig4a <-
  ggplot(dat = dat, aes(x = tol, y = reorder(koeppen_gr, n), fill = koeppen_gr, height = after_stat(count/ max(count)))) + 
  geom_density_ridges(stat = "binline", binwidth = 5, scale = 0.95, linewidth = 0.2) +
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(0,0)) + 
  labs(x = "Temperature (°C)", y = "Köppen-Geiger climate classification") +
  scale_fill_viridis(begin = 0.1, end = 0.8, discrete = TRUE, option = "H") +
  theme_ridges(font_size = 8, center_axis_labels = TRUE)  + theme(legend.position = "none")

# Figure 4b - stacked bar plot for Köppen and language

lang <- dat %>% group_by(koeppen_gr, ref_language) %>% count() %>% arrange(desc(n))
lang$ref_language <- str_to_title(lang$ref_language)

fig4b <- 
  ggplot(data = lang, aes(fill = reorder(ref_language, -n), y = n, x = koeppen_gr)) +
  geom_bar(position = "fill", stat = "identity", color = "black", linewidth = 0.2) +
  labs(x = "Köppen-Geiger climate classification", y = "Fraction") +
  scale_fill_viridis(option = "G", name = "Language", discrete = T, begin = 0.9, end = 0) +
  theme_bw(base_size = 8) +
    scale_y_continuous(expand = c(0,0)) + theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

# Figure 4 complete
fig4a + fig4b & plot_annotation(tag_levels = 'a')

# save Figure 4
ggsave(filename = 'figure4.png', path = 'figures', width = 1880, height = 900, units = 'px', dpi = 'print')


# Figure 5 ----------------------------------------------------------------
# histogram of references by publication year 

fig5 <- 
  ggplot(data = ref, aes(x = pub_year)) + geom_histogram(binwidth = 3, color = "black", fill = "#575C6DFF", alpha = 0.7) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,110)) +
  labs(x = "Year", y = "Number")   + theme_bw(base_size = 15) 

# save Figure 5
ggsave(filename = 'figure5.png', path = 'figures', width = 1880, height = 1320, units = 'px', dpi = 'print')


# clean -------------------------------------------------------------------

rm(list = ls())

