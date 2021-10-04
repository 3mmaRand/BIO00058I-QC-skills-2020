## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(include = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      include = FALSE)


## ----pkgs--------------------------------------------------------------------------------------------
library(RefManageR)
library(tidyverse)



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
# Disease progression in Parkinson's disease-associated mutants

# read in the data file
park <- read_table("../data-raw/park.txt")

# check you understand the structure of the data
str(park)
# there are two numeric variables

# identify the response and explanatory variables
# the response is the number of flies in a vial that can climb to the given height within a set time; age is the explanatory variable

# make a quick plot of the data and summarise it
ggplot(data = park, aes(x = age, y = flies)) +
  geom_point()
# it looks at the fewer older flies are able to make the climb

# build a model with `glm()` and examine it using `summary()`
mod <- glm(data = park, flies ~ age, family = poisson)

# what are the model coefficients?
mod
exp(mod$coefficients)
# b0 is 1.9306 and b1 is -0.0386 
# the predicts 6.89 age 0 flies will be able to make the climb and this changes by a factor of 0.96 for each da (i.e., decreases)
# the equation of the line is ln(flies)  = 1.9306 -0.0386 × age or
# flies = 6.8935499 * 0.9621374^age

# what is the null deviance of the response? And the residual deviance of model? 
summary(mod)
# the Null deviance is 23.484  and the Residual deviance is 16.177  

# determine the reduction in deviance and whether it is significant 
# the deviance has reduced by 7.307
anova(mod, test = "Chisq")
# this is a significant reduction p = 0.006868

# evaluate whether the assumptions of the model are met
plot(mod, which = 1)
plot(mod, which = 2)
# these look 'ok' although the sample size is modest

# use `predict()` to make predictions for a set sensible values for the explanatory variable(s)   
# her I do it for ages from 10 to 30 days in steps of 5 days (5 values)
# create a data frame of the x values from which you want to predict
predict_for <- data.frame(age = seq(10, 30, 5))
# add predictions to that data.frame
predict_for$pred <- predict(mod, newdata = predict_for, type = "response")

# write a report on the results. This will be no more than a few sentences reporting the results and an accompanying figure which you also write to file. 
p <- ggplot(data = park, aes(x = age, y = flies)) +
  geom_point() +
geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              se = FALSE,
            colour = "black") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 35),
                     name = "Age (days)") +
    scale_y_continuous(expand = c(0, 0.03),
                      breaks = seq(0, 10, 2),
                     limits = c(0, 10),
                     name = "Number of flies") +
  theme_classic()

ggsave("../figures/flies.tif", 
       plot = p, 
       device = "tiff",
       width = 3.2, 
       height = 3.2,
       units = "in",
       dpi = 300)
#


## ----------------------------------------------------------------------------------------------------
# The effect of a MAPK Kinase Inhibitor on the number of nuclei in neurons
# library(tidyverse)
# read in the data file
mapk <- read_table("../data-raw/kinase.txt")

# check you understand the structure of the data
str(mapk)
# there are two numeric variables

# identify the response and explanatory variables
# the response is the number of nuclei in a call; pdconc, the concentration of PD089059 is the explanatory variable

# make a quick plot of the data and summarise it
ggplot(data = mapk, aes(x = pdconc, y = nuclei)) +
  geom_point()
# the three cells with the highest number of nuclei were treated with highest concentrations but the pattern is not strong.

# build a model with `glm()` and examine it using `summary()`
mod <- glm(data = mapk, nuclei ~ pdconc, family = poisson)

# what are the model coefficients?
mod
exp(mod$coefficients)
# b0 is 0.007736 and b1 is 0.085826   
# this predicts 1.007766 nuclei at concentration of zero. This changes by a factor of 1.089617 for each unit of concentration (ie., increases)
# the equation of the line is ln(nuclei)  = 0.007736 + 0.085826   × conc or
# nuclei = 1.007766 * 1.089617^conc

# what is the null deviance of the response? And the residual deviance of model? 
summary(mod)
# the Null deviance is 63.03  and the Residual deviance is 55.24  

# determine the reduction in deviance and whether it is significant 
# the deviance has reduced by 7.7891
anova(mod, test = "Chisq")
# this is a significant reduction p = 0.005256 

# evaluate whether the assumptions of the model are met
plot(mod, which = 1)
plot(mod, which = 2)
# these look 'ok' 

# use `predict()` to make predictions for a set sensible values for the explanatory variable(s)   
# her I do it for concentrations from 0 to 10 days in steps of 2.5 units.
# In reality, you probably wouldn't need this at the concentrations are already in nice round units - 0 - 14 in steps of one. So you could just predict for the model with
# mapk$pred <- predict(mod, type = "response")
# I've done it for different values just to provide another example fo doing it
# create a data frame of the x values from which you want to predict
predict_for <- data.frame(pdconc = seq(0, 10, 2.5))
# add predictions to that data.frame
predict_for$pred <- predict(mod, newdata = predict_for, type = "response")

# write a report on the results. This will be no more than a few sentences reporting the results and an accompanying figure which you also write to file. 
p <- ggplot(data = mapk, aes(x = pdconc, y = nuclei)) +
  geom_point() +
geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              se = FALSE,
            colour = "black") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 15),
                     name = "PD089059 concentration (arbitrary units)") +
    scale_y_continuous(expand = c(0, 0.03),
                      breaks = seq(0, 10, 1),
                     limits = c(0, 10),
                     name = "Number of nuclei") +
  theme_classic()

ggsave("../figures/mapk.tif", 
       plot = p, 
       device = "tiff",
       width = 3.2, 
       height = 3.2,
       units = "in",
       dpi = 300)




## ----------------------------------------------------------------------------------------------------
# The effect of age and cancer status on the number of mutations in a hypermutable tetranucleotide marker
# read in the data file
mutation <- read_table("../data-raw/mutation.txt")

# check you understand the structure of the data
str(mutation)
# there are three variables and 73 observations. Two variables are numbers, 
# one is a factor with two levels

# identify the response and explanatory variables
# the response is 'mut'; the explanatory variables are 'age' and 'cat'

# build a model with `glm()`
mod <- glm(data = mutation, mut ~ cat * age, family = "poisson")

# examine the model result using `summary()` and `anova()`
summary(mod)
anova(mod, test = "Chisq")

# what are the model estimates?
# can be read from summary or accessed like this:
mod$coefficients

# the model estimates are logged (to base e), therefore need to be anti logged for interpretation. Antilog is exp()
exp(mod$coefficients) 
# (Intercept)     cattumour           age cattumour:age 
#    7.863668      1.335158      1.008159      1.015422  

# interpret the results 
# the intercept is the number of mutations when the cat factor is control (level 1) and age is zero, 7.89 mutations. This increases but not significantly, by a factor of 1.34 in the tumour group. For every unit increase in age in the control group, the number of mutations increases by a factor of 1.01 (NS) but there is a bigger effect of age on the number of mutations in the tumour group. Based on the effects of cat and age we would expect an increase  of 7.863668*1.008159*1.335158 = 10.5849 mutations per unit of age in the tumour group. But we get 7.863668*1.008159*1.335158*1.015422 = 10.74814

# one way to help you think about the results is to make the predictions
# this requires creating a data frame of the x values from which you want to predict
predict_for <- data.frame(cat = rep(c("control", "tumour"), each = 3), age = rep(c(0:2), times = 2))
# then predicting
predict_for$pred <- predict(mod, newdata = predict_for, type = "response")
# The Analysis of Deviance Table shows that the effects of age, cat and the interaction between them are significant.
# For glm() it is deviance rather than variance that tells us about model fit.

# use `plot(mod, which = 1)` and `plot(mod, which = 2)` to examine the assumptions
plot(mod, which = 1)
plot(mod, which = 2)
# These look ok.

# plot
ggplot(data = mutation, aes(x = age, y = mut, colour = cat)) +
  geom_point() + 
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              se = FALSE) +
  scale_color_manual(values = c("black", "gray75"), 
                       name = "Patient\nCategory",
                       labels = c("Control", "Tumour")) +
  xlab("Age (years)") +
  ylab("Number of mutations") +
  theme_classic()



## ----refs, echo=FALSE, results="asis", include=TRUE--------------------------------------------------
PrintBibliography(myBib)  

