## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(include = FALSE,
                      messgsize = FALSE,
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
# The effect of sand grain size on the presence of wolf spiders

# library(tidyverse)
# read in the data file
beach <- read_table("../data-raw/grainsize.txt")

# check you understand the structure of the data
str(beach)
# there is one continuous numeric and one variable which takes the value zero or 1

# identify the response and explanatory variables
# the response is 'spiders', the presence or absence of wolf spiders on a beach; gsize, sand grain size, is the explanatory variable

# make a quick plot of the data and summarise it
ggplot(data = beach, aes(x = gsize, y = spiders)) +
  geom_point()
# perhaps spiders are less likely to be present when the grain size is small

# build a model with `glm()` and examine it using `summary()`
mod <- glm(data = beach, spiders ~ gsize, family = binomial)

# what are the model coefficients?
mod # coefficient are log odds, need to be exponentiated for odds
exp(mod$coefficients)
# exp(b0) is the odds of a spider being present is 0.19 at a grain size of 0. Odds are P(present) / P(absent). As this is less than 1, the probability of being present must be smaller than the probability of being absent at a grainsize of 0. 
# exp(b1) is the factor by which this odds changes with each unit of grainsize. b1 is 167.6. This is very large because the grain sizes are in mm and 1 mm is a huge change in grainsize
# the equation of the line is ln(spiders)  = -1.647625 + 5.121553 × gsize or
# spiders = 0.1925066 * 167.5954097^gsize

# what is the null deviance of the response? And the residual deviance of model? 
summary(mod)
# the Null deviance is 35.165  and the Residual deviance is 30.632

# determine the reduction in deviance and whether it is significant 
35.165 - 30.632
# the deviance has reduced by 4.533
anova(mod, test = "Chisq")
# this is a significant reduction p = 0.03324


# use `predict()` to make predictions for a set sensible values for the explanatory variable(s)   
# here I do it for gsizes of 0, 0.5 and 1 mm
# create a data frame of the x values from which you want to predict
predict_for <- data.frame(gsize = c(0, 0.5, 1))
# add predictions to that data.frame
predict_for$pred <- predict(mod, newdata = predict_for, type = "response")
predict_for
#   gsize      pred
# 1   0.0 0.1614302
# 2   0.5 0.7136446
# 3   1.0 0.9699368
# a bit on understanding odds
# An odds is P(present) / P(absent)
# At a grain size of 0, P(present) is 0.1614302, therefore p(absent) is 1-0.1614302
# 0.1614302/(1-0.1614302) = 0.1925066. Look! that's exp(b0)!
# How does this change from grain size of 0 to grain size of 1?
# At a grain size of 1, P(present) is 0.9699368, therefore p(absent) is 1-0.9699368
# the factor by which the odds change is the odds of present at grainsize 1 /
# odds of present at grainsize 0
# (0.9699368/(1-0.9699368)) / (0.1614302/(1-0.1614302))
# = 167.5956 Look, that's exp(b1)
# Odds are not intuitive for most people. I almost always use the predict function to calculate probabilities to help me understand the effects in my model.
# You can still make quick judgement on the basis of the coefficients
# negative b0 means probability of absence (or died/0/no) is higher than probability of presence (or survived/1/yes); positive b0 means probability of presence is higher than probability of absence
# negative b0 means odds of get lower, presence gets less likely
# positive b0 means odds of get higher, presence gets less likely

# write a report on the results. This will be no more than a few sentences reporting the results and an accompanying figure which you also write to file. 
p <- ggplot(data = beach, aes(x = gsize, y = spiders)) +
  geom_point() +
geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
            colour = "black") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 1.2),
                     name = "Grain size (mm)") +
    scale_y_continuous(expand = c(0, 0.03),
                      breaks = seq(0, 1, 0.1),
                     limits = c(0, 1.05),
                     name = "Presence of wolf spiders") +
  theme_classic()

ggsave("../figures/spiders.tif", 
       plot = p, 
       device = "tiff",
       width = 3.2, 
       height = 3.2,
       units = "in",
       dpi = 300)
#


## ----------------------------------------------------------------------------------------------------
# The effect alcohol consumption on the occurrence of Oesophageal cancer
# library(tidyverse)
# read in the data file
cancer <- read_table("../data-raw/oesoph.txt")

# check you understand the structure of the data
str(cancer)
# there is one continuous numeric and one variable which takes the value zero or 1

# identify the response and explanatory variables
# the response is 'status', the presence or absence Oesophageal cancer; alcohol, the amount of alcohol consumed per week in grams, is the explanatory variable

# make a quick plot of the data and summarise it
ggplot(data = cancer, aes(x = alcohol, y = status)) +
  geom_point()
# perhaps cancer is more common for those that consume more alcohol 

# build a model with `glm()` and examine it using `summary()`
mod <- glm(data = cancer, status ~ alcohol, family = binomial)

# what are the model coefficients?
mod # coefficient are log odds, need to be exponentiated for odds
exp(mod$coefficients)
# exp(b0) is the odds of cancer being present is 0.006841727 at an alcohol consumption of 0. Odds are P(present) / P(absent). As this is less than 1, the probability of cancer being present must be smaller than the probability of cancer being absent at an alcohol of 0. 
# exp(b1) is the factor by which this odds changes with each gram of alcohol b1 is 1.088605775. 
# the equation of the line is ln(status)  = -4.9847 + 0.0849 × alcohol or
# status = 0.006841727 * 1.088605775 ^alcohol

# what is the null deviance of the response? And the residual deviance of model? 
summary(mod)
# the Null deviance is 27.034    and the Residual deviance is 22.863  

# determine the reduction in deviance and whether it is significant 
27.034 - 22.863  
# the deviance has reduced by 4.171
anova(mod, test = "Chisq")
# this is a significant reduction p = 0.04112 


# use `predict()` to make predictions for a set sensible values for the explanatory variable(s)   
# here I do it for alcohol of 0 to 60 in steps of 10
# create a data frame of the x values from which you want to predict
predict_for <- data.frame(alcohol = seq(0, 60, 10))
# add predictions to that data.frame
predict_for$pred <- predict(mod, newdata = predict_for, type = "response")
predict_for
#   alcohol        pred
# 1       0 0.006795236
# 2      10 0.015739186
# 3      20 0.036028214
# 4      30 0.080336635
# 5      40 0.169552201
# 6      50 0.323042084
# 7      60 0.527260746
# a bit on understanding odds
# to demonstrate the link between the model estimates and the predicted probabilities like I did in the first example, I will need the predicted probabilities at alcohol = 1 (because a b1, by definition, relates to one unit change)
predict(mod, newdata = data.frame(alcohol = 1), type = "response")
# which gives 0.007392882 
# An odds is P(present) / P(absent)
# At alcohol of 0, P(present) is 0.006841727, therefore p(absent) is 1-0.006795236
# 0.006795236/(1-0.006795236) = 0.006841727. Look! that's exp(b0)!
# How does this change from alcohol of 0 to alcohol of 1?
# At alcohol of 1, P(present) is 0.006841727, therefore p(absent) is 1-0.006841727
# the factor by which the odds change is the odds of present at alcohol of 1 /
# odds of present at alcohol of 0
# (0.007392882 /(1-0.007392882 )) / (0.006841727/(1-0.006841727))
# = 1.081158 Look, that's exp(b1)
# Odds are not intuitive for most people. I almost always use the predict function to calculate probabilities to help me understand the effects in my model.
# You can still make quick judgement on the basis of the coefficients
# negative b0 means probability of absence (or died/0/no) is higher than probability of presence (or survived/1/yes); positive b0 means probability of presence is higher than probability of absence
# negative b0 means odds of get lower, presence gets less likely
# positive b0 means odds of get higher, presence gets less likely

# write a report on the results. This will be no more than a few sentences reporting the results and an accompanying figure which you also write to file. 
p <- ggplot(data = cancer, aes(x = alcohol, y = status)) +
  geom_point() +
geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
            colour = "black") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 70),
                     name = "Alcohol consumed (g/week)") +
    scale_y_continuous(expand = c(0, 0.03),
                      breaks = seq(0, 1, 0.1),
                     limits = c(0, 1.05),
                     name = "Presence of Oesophageal cancer") +
  theme_classic()

ggsave("../figures/oesophageal_cancer.tif", 
       plot = p, 
       device = "tiff",
       width = 3.2, 
       height = 3.2,
       units = "in",
       dpi = 300)



## ----refs, echo=FALSE, results="asis", include=TRUE--------------------------------------------------
PrintBibliography(myBib)  

