###################START HERE IF WANTING TO USE DIRECTLY THE DETECTION MATRIX #############
###read directly the detection matrix RDS to avoid every step of this script up to here###
detection_matrix  <- readRDS(gzcon(url("https://github.com/tgelmi-candusso/cameratrap_analysis/raw/main/detection_matrix_Scarborough.rds")))

##now to call for the detection matrix of a specific animal you can call it this way
detection_matrix$deer
detection_matrix$coyote

###save species specific detection matrix into an object:
coyote<-detection_matrix$coyote

#### COVARIATES ########  

##GENERATE COVARIATE dataframes for the model , make sure to readapt the site_names AND add human/dog presence####
urlfile500="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_500.csv"
urlfile1000="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_1000.csv"
urlfile2000="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_2000.csv"
urlfilehumans="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/human_dog_df.csv"

human_dog_df <- read.csv(urlfilehumans) %>% 
  select(-1) %>% 
  select(site_name, total_freq_humans ,total_freq_dogs )

b500 <- read.csv(urlfile500)%>% select(-1)%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))%>%
  select(-deployment_start, -deployment_end, -status)
b500 <- left_join(b500, human_dog_df, by="site_name")

b1000 <- read.csv(urlfile1000)%>% select(-1)%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))%>%
  select(-deployment_start, -deployment_end, -status)
b2000 <- left_join(b1000, human_dog_df, by="site_name")

b2000 <- read.csv(urlfile2000)%>% select(-1)%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))%>%
  select(-deployment_start, -deployment_end, -status)
b2000 <- left_join(b2000, human_dog_df, by="site_name")


####
