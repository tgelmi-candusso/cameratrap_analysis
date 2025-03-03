###trying to clean undeployed periods of time

library(dplyr)
library(plyr)
library(tidyverse)

getwd()

detection_matrix  <- readRDS(gzcon(url("https://github.com/tgelmi-candusso/cameratrap_analysis/raw/main/detection_matrix_revsites_15072022.rds")))
detection_matrix[1]
##now to call for the detection matrix of a specific animal you can call it this way
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


species_count <- as.data.frame(rev_sites)
colnames(species_count)<- "site_name"

#change the 
matrix <- detection_matrix$deer
matrix<- matrix %>% dplyr::filter(site_name %in% rev_sites) %>%
  mutate(count =  select(.,2:54) %>% rowSums(na.rm = TRUE))
weeks_deployed <- human_dog_df %>% dplyr::filter(site_name %in% rev_sites) %>% select(site_name, total_weeks_deployed) 
matrix <- left_join(matrix, weeks_deployed, by= "site_name")
matrix <- matrix %>% mutate(freq =  count/total_weeks_deployed) 
matrix_sel <- matrix %>% select(site_name, freq)
species_count <- left_join(species_count, matrix_sel, by="site_name")
colnames(species_count) <- c("site_name","deer")


matrix <- detection_matrix$coyote
matrix<- matrix %>% dplyr::filter(site_name %in% rev_sites) %>%
  mutate(count =  select(.,2:54) %>% rowSums(na.rm = TRUE))
weeks_deployed <- human_dog_df %>% dplyr::filter(site_name %in% rev_sites) %>% select(site_name, total_weeks_deployed) 
matrix <- left_join(matrix, weeks_deployed, by= "site_name")
matrix <- matrix %>% mutate(freq =  count/total_weeks_deployed) 
matrix_sel <- matrix %>% select(site_name, freq)
species_count <- left_join(species_count, matrix_sel, by="site_name")
colnames(species_count) <- c("site_name","deer","coyote")

matrix <- detection_matrix$fox
matrix<- matrix %>% dplyr::filter(site_name %in% rev_sites) %>%
  mutate(count =  select(.,2:54) %>% rowSums(na.rm = TRUE))
weeks_deployed <- human_dog_df %>% dplyr::filter(site_name %in% rev_sites) %>% select(site_name, total_weeks_deployed) 
matrix <- left_join(matrix, weeks_deployed, by= "site_name")
matrix <- matrix %>% mutate(freq =  count/total_weeks_deployed) 
matrix_sel <- matrix %>% select(site_name, freq)
species_count <- left_join(species_count, matrix_sel, by="site_name")
colnames(species_count) <- c("site_name","deer","coyote", "fox")

matrix <- detection_matrix$rabbit
matrix<- matrix %>% dplyr::filter(site_name %in% rev_sites) %>%
  mutate(count =  select(.,2:54) %>% rowSums(na.rm = TRUE))
weeks_deployed <- human_dog_df %>% dplyr::filter(site_name %in% rev_sites) %>% select(site_name, total_weeks_deployed) 
matrix <- left_join(matrix, weeks_deployed, by= "site_name")
matrix <- matrix %>% mutate(freq =  count/total_weeks_deployed) 
matrix_sel <- matrix %>% select(site_name, freq)
species_count <- left_join(species_count, matrix_sel, by="site_name")
colnames(species_count) <- c("site_name","deer","coyote", "fox", "rabbit")


###add the species here sequencially, by copy pasting rows 73 to 81, changing $species
###to the species of interest in line 73 and adding the species of interest to the vector in line 81)
###that way it adds the row to the species_count dataframe, this would be the workflow of a function if we iterated through the a vector with the species of interest)

write.csv(species_count, "species_frequencies.csv")

##this is all we need for the pie charts in arcgis###


###for loop attempt: succeeded

##loop to extract weekly count/ weeks deployed, per species

#initiation objects
species_count <- as.data.frame(rev_sites) 
colnames(species_count) <- "site_name"

#loop
for (i in seq(1, length(detection_matrix), by=1)){
  matrix <- as.data.frame(detection_matrix[[i]])  #convert listed df as df with no name here the two [[]] are key.
  matrix<- matrix %>% dplyr::filter(site_name %in% rev_sites) %>%
    mutate(count =  select(.,2:54) %>% rowSums(na.rm = TRUE)) #sum across rows
  weeks_deployed <- human_dog_df %>% dplyr::filter(site_name %in% rev_sites) %>% 
    select(site_name, total_weeks_deployed)  #number of weeks deployed and filter for used sites
  matrix <- left_join(matrix, weeks_deployed, by= "site_name") #add the weeks deployed data to main df
  matrix <- matrix %>% mutate(freq =  (count+0.01)/total_weeks_deployed) #frequency math
  matrix_sel <- matrix %>% select(site_name, freq) #simplify for final output
  species_count <- left_join(species_count, matrix_sel, by="site_name") #append results to initiation object with the site_names 
}
##clean the final output by fixing column names to species names
names <- names(detection_matrix)
names <- c("site_name", names)
colnames(species_count) <- names

##save csv for further use in arcgis
write.csv(species_count, "species_frequencies.csv")


####note I just learnt that if we create a list with several matrices like the detection 
###matrix, when we write as.data.frame(list[i]) with only ONE [] all the column names become bird.site_name, etc
###This will be useful when creating the multi scale dataframe