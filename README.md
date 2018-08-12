---
title: "Creating simulated random effects data for two and three level models"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Here is the model that I am trying to recreate

Level 1: There is the intercept that varies for each person over time.  Then there is the slope for time that varies for each person over time.  Finally there is the error term that is unique for each data point.

$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}Time_{ij} + e_{ij}}~~~ (1.1)$$

Level 2 Intercept: Here the intercept is broken down into the constant plus the effect of the intervention, which is at level 2 in the intercept because it does not vary over time only by person and the error term which varies by person. 

$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + \gamma_{01}Intervention_{j} + u_{0j}} ~~~ (1.2)$$

Then there is level the two slope which has the constant effect, plus the slope for the intervention for each person, plus a random error term that unique to each person.  

$$ Level~2~Slope~Time:~~~{\beta_{1j} = \gamma_{10} + \gamma_{11}Intervention_{j} + u_{1j}} ~~~ (1.3)$$
Then we have the mixed model, which has all the components combined
$$Mixed~model: ~~~{y_{ij} =   (\gamma_{00}+ \gamma_{01}Intervention_{j} + u_{0j}) + (\gamma_{10}}+\gamma_{11}*Intervention_{j} +u_{1j})*Time_{ij} + e_{ij} $$
I am basing this example on the example below and extending it by adding an intervention variable: http://m-clark.github.io/docs/sem/latent-growth-curves.html

I am creating a data set with 500 total people across 4-time points (ranging from 0 to 3) totaling 2,000 data points.  
I then create the number of subjects, which are 500 people replicated four times each.

To add an intervention I create a treat variable which I then sample from 500 times and then replicate these values 4 times.
```{r}
n = 100
timepoints = 3
time = timepoints-1
time = rep(0:time, times=n)
subject = rep(1:n, each=timepoints)
treat = c(1,0)
intervention = sample(treat, replace = TRUE, prob = c(.5, .5), n)
intervention = rep(intervention, each = timepoints)
```
I am assuming I have an outcome that is normally distributed and in standard normal form.     

Then I am setting the intercept to .5, a slope for the variable time to .25, and a slope for the intervention variable to .25.

Then I am creating the random effects for the intercept and time, because each person gets a unique intercept and a unique slope for time.  

I am also creating a slope for the interaction effect between time and intervention, which is also .25
```{r}
library(MASS)
intercept = .5
slopeT = .25
slopeI = .25
slopeTI = .25
randomEffectsCorr = matrix(c(1,.2,.2, 1), ncol = 2)
randomEffectsCorr

randomEffects = mvrnonnorm(n, mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffects = data.frame(randomEffects)
dim(randomEffects)
colnames(randomEffects) = c("Int", "SlopeT")
dim(randomEffects)
```
Now I am trying to create the outcome variable that has the parameters above.  I am creating random effects for the intercept and slope across time because each person will get their own random effect over this variable, because it is nested within people.  Then I am creating the fixed effects, which are constant across the people according the variable.  For example, the slope for the intervention only varies across the whether someone get the intervention or not. 

Ok so when you have [subject] that just means that each subject gets the same value.  Then it makes sense, because you want the random effects for the person to be the same and then vary by time for the slope, because we want this estimate to vary over time instead of just the same data point for each person.
```{r}
sigma = .1
y1 = (intercept + randomEffects$Int[subject])+(slopeT + randomEffects$SlopeT[subject])*time + slopeI*intervention + slopeTI*time*intervention+ rnorm(n*timepoints, mean = 0, sd = sigma)
d = data.frame(subject, time, intervention, y1)
dim(d)
head(d)
```
Generate the data using the model that has the intervention effect that I want with time nested within participants.
```{r}
library(lme4)
model1 = lmer(y1 ~ time*intervention + (time|subject), data = d)
summary(model1)
```


Ok now let us run it in the simr.  Models two and three run fine.  Problem seems to be when we add multiple covariates I can only run the function with the first covariate entered and cannot seem to change it.

If I only have the interaction term, it won't run.  
```{r}
library(simr)
model6 = lmer(y1 ~ time + intervention + time*intervention + (time | subject), data = d)
summary(model6)
fixef(model6)["time:intervention"] = 2
set.seed(1234)
powerCurve(model6, along = "time", nsim = 10, test = fixed("time:intervention"))
```
