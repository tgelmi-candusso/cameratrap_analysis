library(MuMIn)
library(unmarked)
library(dplyr)

###################START HERE IF WANTING TO USE DIRECTLY THE DETECTION MATRIX #############
###read directly the detection matrix RDS to avoid every step of this script up to here###
detection_matrix  <- readRDS(gzcon(url("https://github.com/tgelmi-candusso/cameratrap_analysis/raw/main/detection_matrix_revsites_15072022.rds")))

#### COVARIATES ########

##GENERATE COVARIATE dataframes for the model , make sure to readapt the site_names AND add human/dog presence####
urlfile100="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_100.csv"
urlfile500="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_500.csv"
urlfile1000="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_1000.csv"
urlfile2000="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_2000.csv"
urlfilehumans="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/human_dog_df.csv"

human_dog_df <- read.csv(urlfilehumans) %>% 
  select(-1) %>% 
  select(site_name, total_freq_humans ,total_freq_dogs )

b100 <- read.csv(urlfile100, fileEncoding = 'UTF-8-BOM')%>% # added , fileEncoding = 'UTF-8-BOM' to avoid i.. before site_name
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b100 <- left_join(b100, human_dog_df, by="site_name")%>%
  dplyr::filter(site_name %in% unique(detection_matrix$deer$site_name))%>% ##filter those for relevant for the analysis
  mutate(total_freq_humans = ifelse(is.na(total_freq_humans), 0, total_freq_humans)) %>%
  mutate(total_freq_dogs = ifelse(is.na(total_freq_dogs), 0, total_freq_dogs))

b500 <- read.csv(urlfile500, fileEncoding = 'UTF-8-BOM')%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b500 <- left_join(b500, human_dog_df, by="site_name")%>%
  dplyr::filter(site_name %in% unique(detection_matrix$deer$site_name))%>% ##filter those for relevant for the analysis
  mutate(total_freq_humans = ifelse(is.na(total_freq_humans), 0, total_freq_humans)) %>%
  mutate(total_freq_dogs = ifelse(is.na(total_freq_dogs), 0, total_freq_dogs))

b1000 <- read.csv(urlfile1000, fileEncoding = 'UTF-8-BOM')%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b1000 <- left_join(b1000, human_dog_df, by="site_name")%>%
  dplyr::filter(site_name %in% unique(detection_matrix$deer$site_name))%>% ##filter those for relevant for the analysis
  mutate(total_freq_humans = ifelse(is.na(total_freq_humans), 0, total_freq_humans)) %>%
  mutate(total_freq_dogs = ifelse(is.na(total_freq_dogs), 0, total_freq_dogs))

b2000 <- read.csv(urlfile2000, fileEncoding = 'UTF-8-BOM')%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
b2000 <- left_join(b2000, human_dog_df, by="site_name")%>%
  dplyr::filter(site_name %in% unique(detection_matrix$deer$site_name))%>% ##filter those for relevant for the analysis
  mutate(total_freq_humans = ifelse(is.na(total_freq_humans), 0, total_freq_humans)) %>%
  mutate(total_freq_dogs = ifelse(is.na(total_freq_dogs), 0, total_freq_dogs))

#ignoring cam 37 for now 
##note: these should be filtered following the revised sites.
rev_sites <- c("TUW17",
               #"TUW18",
               "TUW19", 
               "TUW21", 
               "TUW23",
               "TUW24",
               "TUW25",
               "TUW26",
               "TUW27",
               "TUW28",
               "TUW29",
               "TUW29b",
               "TUW39",
               "TUW42",
               "TUW36",
               "TUW38",
               "TUW37b",
               "TUW35a",
               "TUW37",
               #"TUW36b",
               #"TUW33b",
               #"TUW34",
               "TUW38b",
               #"TUW35b",
               #"TUW32",
               "TUW31",
               #"TUW33",
               "TUW2")
#"TUW14",
#"TUW1")

b100 <- b100 %>% dplyr::filter(site_name %in% rev_sites) %>% 
  dplyr::filter(site_name != "TUW37") 
b500 <- b500 %>% dplyr::filter(site_name %in% rev_sites)  %>% 
  dplyr::filter(site_name != "TUW37") 
b1000 <- b1000 %>% dplyr::filter(site_name %in% rev_sites)  %>% 
  dplyr::filter(site_name != "TUW37") 
b2000 <- b2000 %>% dplyr::filter(site_name %in% rev_sites)  %>% 
  dplyr::filter(site_name != "TUW37") 


##call occupancy covariates
###scaling should omit the first two columns that are categorical, I will correct that

cov<- b100 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area)
cov1 <- cov[,1:2]
cov2 <- as.data.frame(scale(cov[,3:ncol(cov)]))
cov<- cbind(cov1,cov2)
siteCovs_100 <- cov

cov<- b500 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area)
cov1 <- cov[,1:2]
cov2 <- as.data.frame(scale(cov[,3:ncol(cov)]))
cov<- cbind(cov1,cov2)
siteCovs_500 <- cov

cov<- b1000 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area)
cov1 <- cov[,1:2]
cov2 <- as.data.frame(scale(cov[,3:ncol(cov)]))
cov<- cbind(cov1,cov2)
siteCovs_1000 <- cov

cov<- b2000 %>% select(-1, -BUFF_DIST, -SHAPE_Length, -ORIG_FID, -SHAPE_Area)
cov1 <- cov[,1:2]
cov2 <- as.data.frame(scale(cov[,3:ncol(cov)]))
cov<- cbind(cov1,cov2)
siteCovs_2000 <- cov


##call detection covariate matrix here if using
##det_list <- list(season = det_covs)

# setting up for occupancy for deer 
y <- detection_matrix$deer %>% dplyr::filter(site_name %in% rev_sites) %>%
  dplyr::filter(site_name != "TUW37") %>% 
  select(-1) ## select final columns select(2:54), this could also be written select(-1) to avoid the first column with the site names

###I will comment the next chunk because the covriate frames are already data frames so I an simply name the previous relevant chunk as siteCovs_100 ####
# siteCovs_100 <- as.data.frame(b100)
# #siteCovs_100 <- siteCovs_100[,1:23] ## the covariate dataframes are ready from above, doing this might cut off final columns, potentially added later I will comment this line of code
# 
# siteCovs_500 <- as.data.frame(b500)
# #siteCovs_500 <- siteCovs_500[,1:23]
# 
# siteCovs_1000 <- as.data.frame(b1000)
# #siteCovs_1000 <- siteCovs_1000[,1:23]
# 
# siteCovs_2000 <- as.data.frame(b2000)
# #siteCovs_2000 <- siteCovs_2000[,1:23]

#######
umf_deer_100 <- unmarkedFrameOccu(y = y, siteCovs = siteCovs_100)
umf_deer_500 <- unmarkedFrameOccu(y = y, siteCovs = siteCovs_500)
umf_deer_1000 <- unmarkedFrameOccu(y = y, siteCovs = siteCovs_1000)
umf_deer_2000 <- unmarkedFrameOccu(y = y, siteCovs = siteCovs_2000)

##SINGLE SPECES
# y_list_c <- list(coyote = as.matrix(detection_matrix$coyote %>% select(-1)))
# coyote <- unmarkedFrameOccu(y = (detection_matrix$coyote%>%select(-1)),
#                             siteCovs = cov)
#obsCovs = det_list
#mdata <- coyote
## in order to change deer model with buffer change swap mdata variable with umf_deer_500, umf_deer_1000, or umf_deer_2000
mdata <- umf_deer_2000

##single covariate comparison

fit_null <- occu(formula = ~ 1
                 ~ 1,
                 data = mdata)

fit_LFT <- occu(formula = ~1
                     ~LFT_dist,
                data = mdata)

fit_H2O <- occu(formula = ~1
                     ~H2O_dist,
                data = mdata)

fit_WV <- occu(formula = ~1
                    ~WV_dist,
               data = mdata)

fit_MV <- occu(formula = ~1
                    ~MV_dist,
               data = mdata)

fit_WVF <- occu(formula = ~1
                     ~WVF_dist,
                data = mdata)

fit_WVO <- occu(formula = ~1
                     ~WVO_dist,
                data = mdata)

fit_built <- occu(formula = ~1
                  ~built,
                  data = mdata)

fit_DEM_median <- occu(formula = ~1
                  ~DEM_median,
                  
                  data = mdata)

fit_DEM_mean <- occu(formula = ~1
                          ~DEM_mean,
                          
                          data = mdata)

fit_NDVI_median <- occu(formula = ~1
                             ~NDVI_median,
                             
                             data = mdata)

fit_NDVI_mean <- occu(formula = ~1
                           ~NDVI_mean,
                           
                           data = mdata)

fit_POP_median <- occu(formula = ~1
                            ~POP_median,
                            
                            data = mdata)

fit_POP_mean <- occu(formula = ~1
                          ~POP_mean,
                          
                          data = mdata)

fit_WVO_PA <- occu(formula = ~1
                        ~WVO_PA,
                        
                        data = mdata)

fit_WVF_PA <- occu(formula = ~1
                        ~WVF_PA,
    
                        data = mdata)

fit_MV_PA <- occu(formula = ~1
                       ~MV_PA,
                       
                       data = mdata)

fit_FD_PA <- occu(formula = ~1
                       ~Fdec_PA,
                       
                       data = mdata)

fit_FM_PA <- occu(formula = ~1
                       ~Fmix_PA,
                       
                       data = mdata)

fit_FC_PA <- occu(formula = ~1
                       ~Fcon_PA,
                       
                       data = mdata)
fit_cor <- occu(formula = ~1
                     ~corridor,
                     
                     data = mdata)
fit_hum <- occu(formula = ~1
                     ~total_freq_humans,
                     
                     data = mdata)
fit_dog <- occu(formula = ~1
                     ~total_freq_dogs,
                     
                     data = mdata)

fit <- fitList(fit_null, fit_LFT, fit_H2O, fit_WV, fit_MV, fit_WVF, fit_WVO,
               fit_built, fit_DEM_median, fit_DEM_mean, fit_NDVI_median,
               fit_NDVI_mean, fit_POP_median, fit_POP_mean, fit_WVO_PA, fit_WVF_PA, 
               fit_MV_PA, 
               fit_FC_PA, 
               fit_FM_PA, fit_FD_PA,
               fit_cor, fit_hum, fit_dog)
modSel(fit)

fit2000 <-  occu(formula = ~1
                 ~ built + NDVI_mean + WVF_dist + total_freq_humans + total_freq_dogs + Fdec_PA + WV_dist + POP_mean + 1,
                 
                 data = mdata)

modelList_2000 <- dredge(fit2000,
                         rank = "AIC")



fit1000 <-  occu(formula = ~1
                ~ NDVI_mean + WVF_dist + total_freq_humans + total_freq_dogs + Fdec_PA + WV_dist + 1 ,
                
                data = mdata)

modelList_1000 <- dredge(fit1000,
                        rank = "AIC")

fit500 <-  occu(formula = ~1
                ~WVF_dist + total_freq_humans + total_freq_dogs + WV_dist + 1 ,
                
                data = mdata)

modelList_500 <- dredge(fit500,
                        rank = "AIC")

fit100 <-  occu(formula = ~1
                ~WVF_dist + total_freq_humans + total_freq_dogs + WV_dist + POP_mean + 1 ,
                
                data = mdata)

modelList_100 <- dredge(fit100,rank = "AIC")

coy_SOM <- occu(formula = ~ 1
                ~ built + NDVI_median + WVF_PA,
                data = mdata)
coy_best_fit_cov_list <- dredge(coy_SOM, 
                                rank = "AIC")

models2000 <- get.models(modelList_2000, subset = delta < 1)
##to get a specific model from the dredge list
models2000[[1]]

avgm <- model.avg(models2000)



#######going the AICmodavg way and  to increase reproducibility you will change the following lines
###based on the scale and the covariate
library(AICcmodavg) #install package if you dont have it

#####change input HERE based on the scale ####
models <- models2000
site_cov <-siteCovs_2000
#####


occu_modavg_psi_predict <- modavgPred(models, 
                                      # c.hat = 1, # to change variance inflation factor, default = 1) 
                                      parm.type = "psi", # psi = occupancy, can also be "det" for detection probability
                                      newdata = mdata@siteCovs)[c("mod.avg.pred",
                                                                                     "lower.CL",
                                                                               "upper.CL")]

occu_modavg_psi_predict_df <- data.frame(Predicted = occu_modavg_psi_predict$mod.avg.pred,
                                         lower = occu_modavg_psi_predict$lower.CL,
                                         upper = occu_modavg_psi_predict$upper.CL,
                                         site_cov)
##check it worked, this is the reference dataframe for predictions regardless of covariates
##at the given scale 
head(occu_modavg_psi_predict_df)

###reference graph
occu_modavg_psi_predict_df <- occu_modavg_psi_predict_df %>%
  mutate(x = rep(1:19, 1), # Adding pseudo-coordinates, these could eventually have the actual covariates 
         y = rep(1:19, each = 1))

# Note, you will probably plot on map and use polygons, not points
# This is an example just to show how you can easily visualize the predicted occupancy across all sites
ggplot(data = occu_modavg_psi_predict_df, aes(x = x, y = y, fill = Predicted)) +
  geom_point(size = 10, pch = 22) +
  theme_classic() +
  scale_fill_gradient(low = "blue", high = "yellow", name = "Predicted\noccupancy") +
  ggtitle("Predicted occupancy")

### PREDICTIONS FOR EACH COVARIATE AND SUBSEQUENT GRAPHS
######generate newdata data frame for each covariate separately, change covariate name at "HERE"
nseq <- function(x, len = length(x)) seq(min(x, na.rm = TRUE),
                                         max(x, na.rm=TRUE), length = len)
newdata <- as.data.frame(lapply(lapply(site_cov, mean), rep, 25))
###change input HERE based on the covariate of interest after the $ ####
newdata$built <- nseq(site_cov$built, nrow(newdata)) #HERE change covariate name after $

occu_pred <- modavgPred(models,
                               # c.hat =    # to change variance inflation factor, default = 1) 
                               parm.type = "psi", # psi = occupancy
                               newdata = newdata)[c("mod.avg.pred",
                                                                "lower.CL",
                                                                "upper.CL")]

occu_pred_df <- data.frame(Predicted = occu_pred$mod.avg.pred,
                                  lower = occu_pred$lower.CL,
                                  upper = occu_pred$upper.CL,
                                  newdata)

##to create the plot for different covariates change the x = 
occu_pred_plot <- ggplot(occu_pred_df, aes(x = built, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = "dashed") +
  geom_path(size = 1) +
  theme_classic() +
  coord_cartesian(ylim = c(0,1)) 
occu_pred_plot + 
  labs(x = "built cover (standardized)", y = "Occupancy probability")
