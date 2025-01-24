suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)
library(DHARMa)

here()

## LOAD DATA ####
# scup data 
scup_fall <- readRDS(here("sdmtmb", "scup", "data", "scup_fall.rds")) |> 
  mutate(AREA = as.factor(AREA), 
         EST_YEAR = as.factor(EST_YEAR))

# best models
m_fall_tw <- readRDS(here("sdmtmb", "scup", "data","mods","comps", "m7_fall_tw.rds"))


set.seed(143)

#Residuals tweedie model
#

scup_fall_tweedie <- predict(m_fall_tw)
scup_fall_tweedie$resids <- residuals(m_fall_tw, type="mle-mvn") # randomized quantile residuals
qqnorm(scup_fall_tweedie$resids) 
qqline(scup_fall_tweedie$resids)


ggplot(scup_fall_tweedie) +
  geom_point(aes(x=est, y=resids)) +
  facet_wrap(~YEAR)


ggplot(scup_fall_tweedie, aes(X, Y, col = resids)) +
  scale_colour_gradient2() +
  geom_point() +
  facet_wrap(~EST_YEAR) +
  coord_fixed()


#Dharma residuals
simulate(m_fall_tw, nsim = 300, type = "mle-mvn") %>% 
  dharma_residuals(m_fall_tw)


# My reading of DHARMa documation is that the predicted response for the 
# residuals vs. fitted plot should ideally not include the random effects:
pred_fixed_m_fall_tw <- m_fall_tw$family$linkinv(predict(m_fall_tw)$est_non_rf)
set.seed(10)
sim_m_fall_tw <- simulate(m_fall_tw, nsim = 300, type = "mle-mvn")


dharmares.mf.tw <- DHARMa::createDHARMa(
  simulatedResponse = sim_m_fall_tw,
  observedResponse = scup_fall_tweedie$EXPCATCHWT,
  fittedPredictedResponse = pred_fixed_m_fall_tw
)
plot(dharmares.mf.tw)
DHARMa::testResiduals(dharmares.mf.tw)

#Plot residuals against a specific predictor = AVGDEPTH
plotResiduals(dharmares.mf.tw, form = scup_fall_tweedie$AVGDEPTH)


testDispersion(dharmares.mf.tw)
testZeroInflation(dharmares.mf.tw)

DHARMa::testSpatialAutocorrelation(dharmares.mf.tw, x = scup_fall_tweedie$X, y = scup_fall_tweedie$Y)
DHARMa::testZeroInflation(dharmares.mf.tw)



