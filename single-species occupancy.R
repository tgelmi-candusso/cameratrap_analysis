library(MuMIn)
library(unmarked)
library(dplyr)

###################START HERE IF WANTING TO USE DIRECTLY THE DETECTION MATRIX #############
###read directly the detection matrix RDS to avoid every step of this script up to here###
detection_matrix  <- readRDS(gzcon(url("https://github.com/tgelmi-candusso/cameratrap_analysis/raw/main/detection_matrix_Scarborough.rds")))

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
cov <- as.data.frame(scale(cov))

##call detection covariate matrix here if using
##det_list <- list(season = det_covs)

##SINGLE SPECES
y_list_c <- list(coyote = as.matrix(detection_matrix$coyote %>% select(-1)))
coyote <- unmarkedFrameOccu(y = (detection_matrix$coyote%>%select(-1)),
                            siteCovs = cov)
#obsCovs = det_list
mdata <- coyote


##single covariate comparison

fit_null <- occu(formula = ~ 1
                 ~ 1,
                 data = mdata)


fit_null <- occu(formula = ~1
                      ~1
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

coy_SOM <- occu(formula = ~ 1
                ~ built + NDVI_median + WVF_PA,
                data = mdata)
coy_best_fit_cov_list <- dredge(coy_SOM, 
                                rank = "AIC")

###notes for interpretation
##at 1000 buffer, built was the best model , however not under 2 AIC score from null, just 0.62
