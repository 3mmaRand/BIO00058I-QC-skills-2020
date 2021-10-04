## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(include = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      include = FALSE)


## ----pkgs--------------------------------------------------------------------------------------------
library(RefManageR)
library(tidyverse)
library(lsmeans)
library(multcompView)


## ---- load-refs, include=FALSE, cache=FALSE----------------------------------------------------------
BibOptions(check.entries = FALSE,
           bib.style = "authoryear",
           cite.style = "authoryear",
           style = "markdown",
           hyperlink = TRUE,
           dashed = FALSE,
           longnamesfirst = FALSE,
           max.names = 2)
myBib <- ReadBib("../refs/references.bib", check = FALSE)


## ----------------------------------------------------------------------------------------------------
# The effect of nicotinic acid treatment on the adiponectin secretion of an adipocytes cell line

# read in the data file
adip <- read_table("../data-raw/adipocytes.txt")

# identify the response and explanatory variables
# the response is adiponectin a continuous variable. treatment is the explanatory variable with two levels: nicotinic and control

# make a quick plot of the data and summarise it
# quick plot
ggplot(data = adip, aes(x = treatment, y = adiponectin)) +
  geom_violin()
# the control mean is a bit lower than the nicotinic treatment mean
# summarise
adip_summary <- adip %>% 
  group_by(treatment) %>%
  summarise(mean = mean(adiponectin),
            std = sd(adiponectin),
            n = length(adiponectin),
            se = std/sqrt(n))

# build a model with `lm()` and examine it using `summary()`
mod <- lm(data = adip, adiponectin ~ treatment)
summary(mod)
# what are the model coefficients and how do they relate to the group means
# The first group of treatment is control so  
# b0 = 5.546 pg/ml is the mean of the control treated cells. 
# b1 = 1.9627 is labelled treatmentnicotinic and is the difference between the control treated and nicotinic acid treated cells. Thus the mean of the these cells is 5.5460 + 1.9627= 7.5087 pg/ml

# what proportion of the variance in the response is explained by the model? 
# 0.2767

# evaluate whether the assumptions of the linear model are met
plot(mod, which = 2)
plot(mod, which = 1)
# these look fine

# determine whether the effects are significant  
# the effect of treating with nicotinic acid is significant ( p = 0.0028) we can tell because b1 is significant (the F test of model overall tells us the same)
# figure 
p <- ggplot() +
  geom_jitter(data = adip, 
              aes(x = treatment, y = adiponectin), 
              width = 0.25) +
  geom_errorbar(data = adip_summary,
                aes(x = treatment,
                    ymin = mean,
                    ymax = mean),
                width = .3) +
  geom_errorbar(data = adip_summary,
                aes(x = treatment,
                    ymin = mean - se,
                    ymax = mean + se),
                width = .6) +
  geom_segment(aes(x = 1, y = 11, xend = 2, yend = 11)) +
  geom_segment(aes(x = 1, y = 11, xend = 1, yend = 10.8)) +
  geom_segment(aes(x = 2, y = 11, xend = 2, yend = 10.8)) +
  annotate("text", x = 1.5, y = 11.2,  label = "**", size = 6) +
  scale_x_discrete(labels = c("Control", "Nicotinic acid"),
                   name = "Treatment") +
  scale_y_continuous(name = "Adiponectin (pg/ml)",
                     expand = c(0, 0),
                     limits = c(0, 12)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave("../figures/adiponectin.tif", 
       plot = p, 
       device = "tiff",
       width = 3.2, 
       height = 3.2,
       units = "in",
       dpi = 300)



## ----------------------------------------------------------------------------------------------------
# The effect of standardization method on the concentration of long-chain hydrocarbons determined (ppm) from a single sample

# read in the data file
analyte <- read_table("../data-raw/analyte.txt")

# identify the response and explanatory variables
# the response is ppm a continuous variable. method is the explanatory variable with three levels: internal_standard, standard and standard_addition

# make a quick plot of the data and summarise it
# quick plot
ggplot(data = analyte, aes(x = method, y = ppm)) +
  geom_violin()
# two of the methods give similar readings but the standard_addition method is giving lower concentrations
# summarise
analyte_summary <- analyte %>% 
  group_by(method) %>%
  summarise(mean = mean(ppm),
            std = sd(ppm),
            n = length(ppm),
            se = std/sqrt(n))

# build a model with `lm()` and examine it using `summary()`
mod <- lm(data = analyte, ppm ~ method)
summary(mod)
# what are the model coefficients and how do they relate to the group means
# The first group of method is internal_standard so  
# b0 = 170.425 ppm is the mean of the internal_standard.  
# b1 = 4.500 is labelled methodstandard and is the difference between the internal_standard and standard methods, i.e., 170.425 + 4.500= 174.925 ppm.
# b2 = -27.375 is labelled methodstandard_addition and is the difference between the internal_standard and standard_addition methods, i.e., 170.425 -27.375 = 143.05 ppm.

# what proportion of the variance in the response is explained by the model? 
# 0.8966

# evaluate whether the assumptions of the linear model are met
plot(mod, which = 2)
plot(mod, which = 1)
# to be honest, the total number of data points makes this difficult to assess robustly
# but from what we can see, there are no big issues.

# determine whether the effects are significant  
anova(mod)
# the effect of method is significant ( p < 0.001) When there is only one variable in the model, this is the same as the F test at the end of the summary.
# we know from the summary that the internal_standard and standard_addition methods differ (because b2 was significant) but we need a post-hoc test to check all the pairwise comparisons. I'm using `lsmeans()` from the **`lsmeans`** package and `pairs()` from the **`multcompView`** to do a Tukey test

lsmeans(mod, ~method) %>% pairs()

# the standard_addition method gives significantly lower concentrations than the other two methods.

# figure 
p <- ggplot() +
  geom_jitter(data = analyte, 
              aes(x = method, y = ppm), 
              width = 0.25) +
  geom_errorbar(data = analyte_summary,
                aes(x = method,
                    ymin = mean,
                    ymax = mean),
                width = .3) +
  geom_errorbar(data = analyte_summary,
                aes(x = method,
                    ymin = mean - se,
                    ymax = mean + se),
                width = .6) +
  geom_segment(aes(x = 1, y = 190, xend = 2, yend = 190)) +
  geom_segment(aes(x = 1, y = 190, xend = 1, yend = 187)) +
  geom_segment(aes(x = 2, y = 190, xend = 2, yend = 187)) +
  annotate("text", x = 1.5, y = 192,  label = "***", size = 4) +
  geom_segment(aes(x = 1, y = 198, xend = 3, yend = 198)) +
  geom_segment(aes(x = 1, y = 198, xend = 1, yend = 195)) +
  geom_segment(aes(x = 3, y = 198, xend = 3, yend = 195)) +
  annotate("text", x = 2, y = 200,  label = "***", size = 4) +
  scale_x_discrete(labels = c("Internal\nStandard", "Standard", "Standard\nAdditional"),
                   name = "Method") +
  scale_y_continuous(name = "LCHC concentration (ppm)",
                     expand = c(0, 0),
                     limits = c(0, 210)) +
  theme_classic() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 14))

ggsave("../figures/lchc.tif", 
       plot = p, 
       device = "tiff",
       width = 3.2, 
       height = 3.2,
       units = "in",
       dpi = 300)



## ----------------------------------------------------------------------------------------------------
# The effect of maternal choline deficiency on neuron cross sectional area in two brain regions in Mice
# read in the data file
neuron <- read_table("../data-raw/neuronregion.txt")

# identify the response and explanatory variables
# the response is CSA a continuous variable. There are two explanatory variables: Diet with two level (control and deficient) and BrainRegion with two levels (DB and MSN)

# make a quick plot of the data and summarise it
# quick plot
ggplot(data = neuron, aes(x = Diet, y = CSA, fill = BrainRegion)) +
  geom_violin()
# it looks like the deficient diet does affect the cross sectional area of neurons, but mainly in the DB region. There seems to be a difference between the brain regions in a normal diet but not in the deficiant diet.
# summarise
neuron_summary <- neuron %>% 
  group_by(Diet, BrainRegion) %>%
  summarise(mean = mean(CSA),
            std = sd(CSA),
            n = length(CSA),
            se = std/sqrt(n))

# build a model with `lm()` and examine it using `summary()`
mod <- lm(data = neuron, CSA ~ Diet * BrainRegion)
summary(mod)
# what are the model coefficients and how do they relate to the group means
# The first group of Diet is control and the first group of BrainRegion is DB so  
# b0 = 26.665 is the mean of the Control-DB 
# b1 = -5.440 is labelled DietDeficient and is the difference between the Control-DB and Deficient-DB, i.e., the mean of Deficient-DB is 26.665 - 5.440 = 21.225 is 
# b2 = -5.695 is labelled BrainRegionMSN and is the difference between the Control-DB and Control-MSN, i.e., the mean of Control-MSN is 26.665 - 5.695 = 20.97 
# b4 = 4.403 is labelled DietDeficient:BrainRegionMSN and is the effect of changing both diet and region that is additional to their independent effects, i.e., the mean of Deficient-MSN is 26.665 - 5.440 -5.695 + 4.403 =  19.933 

# what proportion of the variance in the response is explained by the model? 
# 0.4034

# evaluate whether the assumptions of the linear model are met
plot(mod, which = 2)
plot(mod, which = 1)
# these look ok

# determine whether the effects are significant  
anova(mod)
# the effect of Diet is significant (p = 0.004) 
# the effect of BrainRegion is significant (p = 0.002)
# but the interaction between them is significant (p = 0.046).

# we need a post-hoc test to check all the pairwise comparisons. 
lsmeans(mod, ~ Diet:BrainRegion) %>% pairs()
# The differences are between: Deficient DB and  Control DB (p = 0.0048); Control MSN and Control DB (p =  0.003); and Deficient MSN and Control DB (p < 0.001). Control.DB differs from all but there are no other differences





# figure 
# palette
# blue, pink, green triadic
pal4 <- c("#256c7a", "#7a256c", "#6c7a25")

p <- ggplot() +
  geom_point(data = neuron, aes(x = Diet,
                                    y = CSA,
                                    colour = BrainRegion),
             position = position_jitterdodge(dodge.width = 1,
                                             jitter.width = 0.15,
                                             jitter.height = 0),
             size = 2) +
  geom_errorbar(data = neuron_summary, 
                aes(x = Diet, ymin = mean - se, ymax = mean + se, group = BrainRegion),
                width = 0.5, size = 1,
                position = position_dodge(width = 1)) +
  geom_errorbar(data = neuron_summary, 
                aes(x = Diet, ymin = mean, ymax = mean, group = BrainRegion),
                width = 0.4, size = 1,
                position = position_dodge(width = 1) ) +
  scale_x_discrete(name = "Diet") +
  scale_y_continuous(name = "CSA (units)",
                     expand = c(0, 0),
                     limits = c(0, 45)) +
  scale_colour_manual(values = pal4[1:2], name = "Brain Region") +
  # Deficient.DB - Control.DB **
  annotate("segment",
           x = 0.75, xend = 1.25,
           y = 35, yend = 35,
           colour = "black") +
  annotate("segment",
           x = 1.25, xend = 1.25,
           y = 35, yend = 34,
           colour = "black") +
  annotate("segment",
           x = 0.75, xend = 0.75,
           y = 35, yend = 34,
           colour = "black") +
  annotate("text",
           x = 1,  y = 36,
           label = "***", size = 6) +
  # Control.MSN - Control.DB **
  annotate("segment",
           x = 0.75, xend = 1.75,
           y = 38, yend = 38,
           colour = "black") +
  annotate("segment",
           x = 0.75, xend = 0.75,
           y = 38, yend = 37,
           colour = "black") +
  annotate("segment",
           x = 1.75, xend = 1.75,
           y = 38, yend = 37,
           colour = "black") +
  annotate("text", x = 1.25,  y = 39,
           label = "**", size = 6) +
# Deficient.MSN - Control.DB ***
  annotate("segment",
           x = 0.75, xend = 2.25,
           y = 41, yend = 41,
           colour = "black") +
  annotate("segment",
           x = 0.75, xend = 0.75,
           y = 41, yend = 40,
           colour = "black") +
  annotate("segment",
           x = 2.25, xend = 2.25,
           y = 41, yend = 40,
           colour = "black") +
  annotate("text", x = 1.5,  y = 42,
           label = "***", size = 6) +
  theme_classic() +
  theme(legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        legend.position = c(0.2, 0.2),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14))

ggsave("../figures/neuron.tif", 
       plot = p, 
       device = "tiff",
       width = 3.5, 
       height = 4,
       units = "in",
       dpi = 300)





## ----refs, echo=FALSE, results="asis", include=TRUE--------------------------------------------------
PrintBibliography(myBib)  

