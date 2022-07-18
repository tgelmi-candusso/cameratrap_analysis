library(unmarked)
library(dplyr)
library(gpplot2)


###################START HERE IF WANTING TO USE DIRECTLY THE DETECTION MATRIX #############
###read directly the detection matrix RDS to avoid every step of this script up to here###
detection_matrix  <- readRDS(gzcon(url("https://github.com/tgelmi-candusso/cameratrap_analysis/raw/main/detection_matrix_Scarborough.rds")))

##now to call for the detection matrix of a specific animal you can call it this way
detection_matrix$deer
detection_matrix$coyote

###save species specific detection matrix into an object:
coyote<-detection_matrix$coyote
deer<-detection_matrix$deer

#### COVARIATES ########

##GENERATE COVARIATE dataframes for the model , make sure to readapt the site_names AND add human/dog presence####
urlfile500="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_500.csv"
urlfile1000="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_1000.csv"
urlfile2000="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_2000.csv"
urlfilehumans="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/human_dog_df.csv"

human_dog_df <- read.csv(urlfilehumans) %>% 
  select(-1) %>% 
  select(site_name, total_freq_humans ,total_freq_dogs )

b500 <- read_csv(urlfile500)%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b500 <- left_join(b500, human_dog_df, by="site_name")%>%
  dplyr::filter(site_name %in% unique(detection_matrix$deer$site_name)) ##filter those for relevant for the analysis

b1000 <- read_csv(urlfile1000)%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b1000 <- left_join(b1000, human_dog_df, by="site_name")%>%
  dplyr::filter(site_name %in% unique(detection_matrix$deer$site_name)) ##filter those for relevant for the analysis


b2000 <- read_csv(urlfile2000)%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b2000 <- left_join(b2000, human_dog_df, by="site_name")%>%
  dplyr::filter(site_name %in% unique(detection_matrix$deer$site_name)) ##filter those for relevant for the analysis



##call occupancy covariates
cov<- b1000 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area)
cov<- b1000 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area)
cov1 <- cov[,1:2]
cov2 <- as.data.frame(scale(cov[,3:ncol(cov)]))
cov<- cbind(cov1,cov2)
##call detection covariate matrix here if using
##det_list <- list(season = det_covs)


###MULTISPECIES OCCUPANCY
##call animal data
y_list_cd <- list(coyote = as.matrix(detection_matrix$coyote %>% select(-1)),
                  deer = as.matrix(detection_matrix$deer %>% select(-1)))
coyote_deer <- unmarkedFrameOccuMulti(y = y_list_cd,
                                      siteCovs = cov)
#obsCovs = det_list)

mdata <- coyote_deer
fit_null <- occuMulti(detformulas = c('~1', '~1'),
                      stateformulas = c('~1', '~1', '~1'),
                      maxOrder = 2,
                      data = mdata)

fit_LFT <- occuMulti(detformulas = c('~1', '~1'),
                     stateformulas = c('~1', '~1', '~LFT_dist'),
                     maxOrder = 2,
                     data = mdata)

fit_H2O <- occuMulti(detformulas = c('~1', '~1'),
                     stateformulas = c('~1', '~1', '~H2O_dist'),
                     maxOrder = 2,
                     data = mdata)

fit_WV <- occuMulti(detformulas = c('~1', '~1'),
                    stateformulas = c('~1', '~1', '~WV_dist'),
                    maxOrder = 2,
                    data = mdata)

fit_MV <- occuMulti(detformulas = c('~1', '~1'),
                    stateformulas = c('~1', '~1', '~MV_dist'),
                    maxOrder = 2,
                    data = mdata)

fit_WVF <- occuMulti(detformulas = c('~1', '~1'),
                     stateformulas = c('~1', '~1', '~WVF_dist'),
                     maxOrder = 2,
                     data = mdata)

fit_WVO <- occuMulti(detformulas = c('~1', '~1'),
                     stateformulas = c('~1', '~1', '~WVO_dist'),
                     maxOrder = 2,
                     data = mdata)

fit_built <- occuMulti(detformulas = c('~1', '~1'),
                       stateformulas = c('~1', '~1', '~built'),
                       maxOrder = 2,
                       data = mdata)

fit_DEM_median <- occuMulti(detformulas = c('~1', '~1'),
                            stateformulas = c('~1', '~1', '~DEM_median'),
                            maxOrder = 2,
                            data = mdata)

fit_DEM_mean <- occuMulti(detformulas = c('~1', '~1'),
                          stateformulas = c('~1', '~1', '~DEM_mean'),
                          maxOrder = 2,
                          data = mdata)

fit_NDVI_median <- occuMulti(detformulas = c('~1', '~1'),
                             stateformulas = c('~1', '~1', '~NDVI_median'),
                             maxOrder = 2,
                             data = mdata)

fit_NDVI_mean <- occuMulti(detformulas = c('~1', '~1'),
                           stateformulas = c('~1', '~1', '~NDVI_mean'),
                           maxOrder = 2,
                           data = mdata)

fit_POP_median <- occuMulti(detformulas = c('~1', '~1'),
                            stateformulas = c('~1', '~1', '~POP_median'),
                            maxOrder = 2,
                            data = mdata)

fit_POP_mean <- occuMulti(detformulas = c('~1', '~1'),
                          stateformulas = c('~1', '~1', '~POP_mean'),
                          maxOrder = 2,
                          data = mdata)

fit_WVO_PA <- occuMulti(detformulas = c('~1', '~1'),
                        stateformulas = c('~1', '~1', '~WVO_PA'),
                        maxOrder = 2,
                        data = mdata)

fit_WVF_PA <- occuMulti(detformulas = c('~1', '~1'),
                        stateformulas = c('~1', '~1', '~WVF_PA'),
                        maxOrder = 2,
                        data = mdata)

fit_MV_PA <- occuMulti(detformulas = c('~1', '~1'),
                       stateformulas = c('~1', '~1', '~MV_PA'),
                       maxOrder = 2,
                       data = mdata)

fit_FD_PA <- occuMulti(detformulas = c('~1', '~1'),
                       stateformulas = c('~1', '~1', '~Fdec_PA'),
                       maxOrder = 2,
                       data = mdata)

fit_FM_PA <- occuMulti(detformulas = c('~1', '~1'),
                       stateformulas = c('~1', '~1', '~Fmix_PA'),
                       maxOrder = 2,
                       data = mdata)

fit_FC_PA <- occuMulti(detformulas = c('~1', '~1'),
                       stateformulas = c('~1', '~1', '~Fcon_PA'),
                       maxOrder = 2,
                       data = mdata)
fit_cor <- occuMulti(detformulas = c('~1', '~1'),
                     stateformulas = c('~1', '~1', '~corridor'),
                     maxOrder = 2,
                     data = mdata)
fit_hum <- occuMulti(detformulas = c('~1', '~1'),
                     stateformulas = c('~1', '~1', '~total_freq_humans'),
                     maxOrder = 2,
                     data = mdata)
fit_dog <- occuMulti(detformulas = c('~1', '~1'),
                     stateformulas = c('~1', '~1', '~total_freq_dogs'),
                     maxOrder = 2,
                     data = mdata)

fit <- fitList(fit_null, fit_LFT, fit_H2O, fit_WV, fit_MV, fit_WVF, fit_WVO,
               fit_built, fit_DEM_median, fit_DEM_mean, fit_NDVI_median,
               fit_NDVI_mean, fit_POP_median, fit_POP_mean, fit_WVO_PA, fit_WVF_PA, 
               fit_MV_PA, 
               #fit_FC_PA, 
               fit_FM_PA, fit_FD_PA,
               fit_cor, fit_hum, fit_dog)
modSel(fit)

##notes for interpretation
##at 500 buffer: from the models with AIC <2 null model, WVF_PA is the only one with a significant effect

##predict
nd_cond1 <- data.frame(
  WVF_PA = seq(min(cov$WVF_PA), max(cov$WVF_PA), length.out = 1000))  #cov of interest is the only one not averaged out
coy_deer1 <- unmarked::predict(fit_WVF_PA, type = 'state', species = 'coyote', cond = 'deer', 
                               newdata = nd_cond1)
coy_deer0 <- unmarked::predict(fit_WVF_PA, type = 'state', species = 'coyote',
                               cond = '-deer', newdata = nd_cond1)

gg_coy_cond <- data.frame(
  WVF_PA = rep(nd_cond1$WVF_PA, 2),
  occupancy = c(coy_deer1$Predicted, coy_deer0$Predicted),
  low = c(coy_deer1$lower, coy_deer0$lower),
  high = c(coy_deer1$upper, coy_deer0$upper),
  conditional = rep(c('Deer absent', 'Deer present'),
                    each = 1000))

ggplot(gg_coy_cond, aes(x = WVF_PA, y = occupancy, color = conditional)) +
  #geom_ribbon(aes(ymin = low, ymax = high, fill = conditional)) +
  geom_line() +
  ylab('Conditional coyote\noccupancy probability') +
  xlab('WVF_PA') +
  labs(fill = 'Deer state') # +
#theme(text = element_text(size = 25),
#legend.position = c(0.75, 0.85))


