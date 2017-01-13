## ------------------------ ##

## Analysis code for:
## White TE. (submitted). All that glitters: jewelled spiders manipulate colour lure 
## geometry to decieve prey. Biology Letters.

## ------------------------ ##

## Clear workspace
rm(list = ls())  

## Packages
library(tidyverse)
library(gridExtra)

# Pattern orientation manipulation data
data_manip <- read.csv('../data/manip_data.csv')

# Captures vs pattern orientation data
data_caps <- read.csv('../data/cap_data.csv')

### ----- ### 

## (a) Do spiders actively control their pattern orientation in the web?

### ----- ### 

# Convert raw positioning data to long format for plotting
data_manip_long <- gather(data_manip, key = time, value = angle, t_0:t_7, factor_key = TRUE)

# Pooled correlation between initial & final orientations, for plotting
corr_fst_lst <- cor.test(data_manip$t_0, data_manip$t_7)

## Plots
t0 <- ggplot(filter(data_manip_long, time == 't_0'), aes(angle)) +
             geom_histogram(data = filter(data_manip_long, time == 't_0', treat == 'exp'), 
                            binwidth = 10,
                            colour = 'black',
                            fill = 'white') +
             geom_histogram(data = filter(data_manip_long, time == 't_0', treat == 'control'), 
                            binwidth = 10,
                            colour = 'black',
                            fill = 'darkgrey') +
             scale_y_continuous(limits = c(0, 25)) + 
             scale_x_continuous('', breaks = seq(0, 90, 10), labels = NULL) +
             ylab('') + 
             xlab('') + 
             annotate('text', x = 75, y = 23, label = 'Original', size = 3) +
             annotate('text', x = 0, y = 23, label = '(a)', size = 4, fontface = 2) +
             theme_bw() +
             theme(
               panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(),
               axis.text.x = element_blank()
             )
             
t1 <- ggplot(filter(data_manip_long, time == 't_1'), aes(angle)) +
             geom_histogram(data = filter(data_manip_long, time == 't_1', treat == 'exp'), 
                            binwidth = 10,
                            colour = 'black',
                            fill = 'white') +
             geom_histogram(data = filter(data_manip_long, time == 't_1', treat == 'control'), 
                            binwidth = 10,
                            colour = 'black',
                            fill = 'darkgrey') +
             scale_y_continuous(limits = c(0, 25)) +
             scale_x_continuous('', breaks = seq(0, 90, 10), labels = NULL) + 
             annotate('text', x = 75, y = 23, label = 'Randomised', size = 3) +
             annotate('text', x = 0, y = 23, label = '(b)', size = 4, fontface = 2) +
             ylab('Number of spiders') +
             theme_bw() +
             theme(
               panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(),
               axis.text.x = element_blank(),
               axis.title = element_text(size = 8, face = "bold")
             )
             
t7 <- ggplot(filter(data_manip_long, time == 't_7'), aes(angle)) +
             geom_histogram(data = filter(data_manip_long, time == 't_7', treat == 'exp'), 
                            binwidth = 10,
                            colour = 'black',
                            fill = 'white') +
             geom_histogram(data = filter(data_manip_long, time == 't_7', treat == 'control'), 
                            binwidth = 10,
                            colour = 'black',
                            fill = 'darkgrey') +
             scale_x_continuous('Orientation (deg)', breaks = seq(0, 90, 10)) + 
             scale_y_continuous(limits = c(0, 25)) +
             ylab('') + 
             annotate('text', x = 75, y = 23, label = '3 hours', size = 3) +
             annotate('text', x = 0, y = 23, label = '(c)', size = 4, fontface = 2) +
             theme_bw() +
             theme(
               panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(),
               axis.title = element_text(size = 8, face = "bold")
             )

polar <- ggplot(filter(data_manip_long, time == 't_0'), aes(angle)) +
                  coord_polar() +
                  scale_x_continuous(breaks = seq(0, 360, by = 30), expand = c(0, 0), lim = c(0, 360)) +
                  xlab('') +
                  ylab('') +
                  scale_y_continuous(limits = c(0, 25)) +
                  theme_bw() + 
                  theme(legend.position = "none",
                        panel.border = element_blank(),
                        axis.title.y = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.title = element_text(size = 8, face = "bold"))

corr_1 <- ggplot(data_manip, aes(x = t_0, y = t_1)) +
              geom_point(aes(colour = morph)) +
              scale_colour_manual(values = c('black', 'goldenrod')) +
              scale_x_continuous(' ', breaks = seq(0, 90, 10), labels = NULL) +
              scale_y_continuous('Orientation, randomised (deg)', breaks = seq(0, 90, 10)) +
              annotate('text', x = 9, y = 83, label = '(e)', size = 4, fontface = 2) +
              theme_bw() +
              theme(legend.position = "none",
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    axis.title = element_text(size = 8, face = "bold"))

corr_7 <- ggplot(data_manip, aes(x = t_0, y = t_7)) +
              geom_point(aes(colour = morph)) +
              scale_colour_manual(values = c('black', 'goldenrod')) +
              stat_smooth(method = "lm", formula = y ~ x, size = 1, color = 'black', lty = 2) +
              scale_x_continuous('Initial orientation (deg)', breaks = seq(0, 90, 10)) +
              scale_y_continuous('Orientation, 3 hours (deg)', breaks = seq(0, 90, 10)) +
              annotate('text', x = 9, y = 83, label = '(f)', size = 4, fontface = 2) +
              annotate('text', x = 79, y = 12, label = paste0('r = ', round(corr_fst_lst$estimate, 3)), size = 3) +
              annotate('text', x = 79, y = 4, label = 'P < 0.001', size = 3) +
              xlab('Initial orientation (degrees)') +
              theme_bw() +
              theme(legend.position = "none",
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    axis.title = element_text(size = 8, face = "bold"))

# Save plots
png('../figs/fig_1_raw.png', width = 15, height = 20, units = 'cm', res = 300)
grid.arrange(t0, polar, t1, corr_1, t7, corr_7, ncol = 2)
dev.off()

tiff('../figs/fig_1_raw.tiff', width = 15, height = 20, units = 'cm', res = 300)
grid.arrange(t0, polar, t1, corr_1, t7, corr_7, ncol = 2)
dev.off()

## Statistics

# Multiple Pearsons correlations of initial orientation versus orientation at 30 min intervals
# Subset data
data_manip_exp <- filter(data_manip, treat == 'exp')
data_manip_con <- filter(data_manip, treat == 'control')

# Experimental group
cor.test(data_manip_exp$t_0, data_manip_exp$t_1)  # Immediately post-randomisation
cor.test(data_manip_exp$t_0, data_manip_exp$t_2)
cor.test(data_manip_exp$t_0, data_manip_exp$t_3)
cor.test(data_manip_exp$t_0, data_manip_exp$t_4)
cor.test(data_manip_exp$t_0, data_manip_exp$t_5)
cor.test(data_manip_exp$t_0, data_manip_exp$t_6)
cor.test(data_manip_exp$t_0, data_manip_exp$t_7)

# Control group
cor.test(data_manip_con$t_0, data_manip_con$t_1)  # Immediately post-randomisation
cor.test(data_manip_con$t_0, data_manip_con$t_2)
cor.test(data_manip_con$t_0, data_manip_con$t_3)
cor.test(data_manip_con$t_0, data_manip_con$t_4)
cor.test(data_manip_con$t_0, data_manip_con$t_5)
cor.test(data_manip_con$t_0, data_manip_con$t_6)
cor.test(data_manip_con$t_0, data_manip_con$t_7)

### ----- ### 

## (b) Is there a relationship between pattern orientation & prey interception rates?

### ----- ### 

# Calculate mean pattern orientation
angledat <- data_caps[, c(grep('angle', names(data_caps), value = TRUE))]
data_caps$angle_mean <- rowMeans(angledat)

# Calculate captures/hour
capdat <- data_caps[, c(grep('dam', names(data_caps), value = TRUE))]
data_caps$dam_hr <- rowSums(capdat)/3

# Statistics
mod <- lm(dam_hr ~ morph * angle_mean + morph * I(angle_mean^2), data = data_caps)
summary(mod)

# Plot the relationship w/quadratic fit
ggplot(data_caps, aes(x = angle_mean, y = dam_hr)) +
          geom_point(aes(colour = morph)) +
          scale_colour_manual(values = c('black', 'goldenrod')) +
          stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, color = 'black', lty = 2) +
          scale_y_continuous(limits = c(0, 8)) +
          scale_x_continuous('Mean pattern orientation (deg)', breaks = seq(0, 90, 10)) + 
          ylab('Intercepts/hour') +
          annotate('text', x = 77.5, y = 8, label = paste('Adj. R2 = ', round(summary(mod)$adj.r.squared, 3)), size = 3) +
          annotate('text', x = 79.5, y = 7.5, label = 'P = 0.002', size = 3) +
          theme_bw() +
          theme(legend.position = "none",
                panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave('../figs/fig_2.tiff', width = 14, height = 9, units = 'cm')
ggsave('../figs/fig_2.png', width = 14, height = 9, units = 'cm')

