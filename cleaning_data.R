###trying to clean undeployed periods of time
library(dplyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyverse)
library(lubridate)
library(rstan)
library(unmarked)
library(ggplot2)

getwd()
setwd("C:/Users/tizge/Documents/UWIN projects/prey-predator")

data<-read_csv("TimelapseDatabase_FULL_04072022 (1).csv")%>%
  filter(DeleteFlag == FALSE)%>%
  filter(revised==TRUE)
#write.csv(data, "TimelapseDatabase_FULL_04072022_nodeletes_onlyrevised.csv")##not getting smaller
###load raw timelapse export csv ####
data<-data%>%
  dplyr::rename(site_name_original = RelativePath, common_name=Species)%>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "America/New_York")) %>%
  mutate(DateTime = force_tz(DateTime, "America/New_York", roll = FALSE))%>%
  mutate(site_name_original = as.character(site_name_original))%>% 
  mutate(site_name_clean = gsub("TUWCPC_", "CPC", site_name_original))%>%
  mutate(site_name_clean = gsub("_", "", site_name_clean))%>%
  mutate(site_name_clean = gsub("TUW0", "TUW", site_name_clean))%>%
  separate(site_name_clean, c("site_name","checkup"), fill="right")%>%
  dplyr::arrange(site_name, common_name, DateTime)%>%
  dplyr::mutate(delta.time = DateTime - lag(DateTime),
                delta.time_secs = as.numeric(delta.time, units = 'secs')) %>%
  dplyr::filter(delta.time_secs > 20)%>% #key line to decide independent events, here 20 seconds, but not important if doing presence/absence
  dplyr::mutate(season = factor(month(DateTime)))%>% 
  dplyr::mutate(week = factor(week(DateTime)))%>% 
  dplyr::mutate(site_name = factor(site_name))%>% 
  dplyr::mutate(date = factor(date(DateTime)))%>% 
  dplyr::mutate(common_name = factor(common_name))

#View(data)

##add deployment periods####
dp<- read_csv("deployment_period.csv")%>%
  mutate(start = as.POSIXct(deployment_start, format = "%m/%d/%Y %H:%M"))%>%
  mutate(end = as.POSIXct(deployment_end, format = "%m/%d/%Y %H:%M")) %>%
  select(site_name, start, end)%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))
data<- left_join(data, dp, by="site_name")

#####checking depoloyment period is correct (for record)####
####
# v <- data %>% 
#   mutate(within_deployment_period = ifelse(as.Date(DateTime)>=as.Date(start)&as.Date(DateTime)<=as.Date(end),1,0))%>%
#   select(site_name, common_name, DateTime, start, end, within_deployment_period)
# v.0 <- v%>% filter(within_deployment_period==0)
# factor(v.0$site_name) ##CPC5, TUW40, TUW41 have pictures beyond deployment period (must be the checkup, there were subfolders for these sites)

##filter cameras that have inconsistent data ####
data <- data %>% 
  dplyr::filter(site_name != "TUW40")%>%
  dplyr::filter(site_name != "TUW41")%>%
  dplyr::filter(site_name != "CPC5") %>%
  dplyr::filter(site_name != "TUW20") 

##correct TUW42
d42 <- data %>% filter(site_name == "TUW42") %>%
  filter(!(year(DateTime)==2017))
d42_no2020 <- d42%>%  filter(!(year(DateTime)==2020))
d42_fix <- d42 %>%
  filter((year(DateTime)==2020))%>%
  mutate(DateTime = DateTime %m+% years(1))
d42_2020_fix <- rbind(d42_fix, d42_no2020)
d_no42 <- data %>% filter(site_name != "TUW42")
data <- rbind(d_no42, d42_2020_fix) #the hole of data stil existing in TUW42 is not due to not deployment nor malfunctioning, rather recovered photos for which I recovered only those with animals


##correct TUW31 and TUW32 switcharoo
data<-data %>% 
  mutate(site_name = ifelse(site_name_original == "TUW31" & (DateTime>= "2021-07-01 17:28:26" & DateTime<= "2021-08-29 11:23:51"), "TUW32", site_name)) %>%
  mutate(site_name = ifelse(site_name_original == "TUW31" & (DateTime>= "2021-08-29 13:02:26" & DateTime<= "2021-08-29 13:03:42"), "TUW32", site_name)) %>%
  mutate(site_name = ifelse(site_name_original == "TUW32" & (DateTime>= "2021-07-01 17:55:22" & DateTime<= "2021-08-29 11:57:01"), "TUW31", site_name))


##transects
Sca <- c("TUW16",
         "TUW17",
         "TUW18",
         "TUW19", 
         "TUW20", 
         "TUW21", 
         "TUW22", 
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
         "TUW40",
         "TUW41",
         "TUW42")
         
Hum <- c("TUW10", 
         "TUW11", 
         "TUW12", 
         "TUW1",
         "TUW2",
         "TUW3",
         "TUW4",
         "TUW5",
         "TUW6",
         "TUW7",
         "TUW8",
         "TUW9",
         "TUW9b")

Don <- c("TUW13",
         "TUW14",
         "TUW15",
         "TUW30",
         "TUW31",
         "TUW32",
         "TUW33",
         "TUW33b",
         "TUW33c",
         "TUW34",
         "TUW35",
         "TUW35b",
         "TUW36",
         "TUW36b",
         "TUW37",
         "TUW37b",
         "TUW38",
         "TUW38b")
CPC <- c("CPC1","CPC2", "CPC3", "CPC4", "CPC5", "CPC6")

# ###determining malfunction dates (for record) visually check for periods when camera was not functioning#####
# dpS <- data %>% filter(site_name == Sca)%>% ### I replotted this one after fixing TUW42
#   filter(site_name != "TUW20")%>%
#   ggplot(aes(x = as.POSIXct(DateTime))) + 
#   geom_histogram(bins=120) + 
#   facet_wrap(~site_name, scales="free_y")
# ggsave(dpS, "Humber_images.png")
# 
# dpH <- data %>% filter(site_name == Hum)%>%
#   ggplot(aes(x = as.POSIXct(DateTime))) + 
#   geom_histogram(bins=120) + 
#   facet_wrap(~site_name, scales="free_y")+
#   scale_x_datetime(date_labels = "%Y", date_minor_breaks = "1 month", name = "time scale")
# ggsave(dpH, "Humber_images.png")
# 
# dpD <- data %>% filter(site_name == Don)%>%
#   ggplot(aes(x = as.POSIXct(DateTime))) + 
#   geom_histogram(bins=120) + 
#   facet_wrap(~site_name, scales="free_y")+
#   scale_x_datetime(date_labels = "%Y", date_minor_breaks = "1 month", name = "time scale")
# ggsave(dpD, "Don_images.png")
#
# dpC <- data %>% filter(site_name == CPC)%>%
#   ggplot(aes(x = as.POSIXct(DateTime))) + 
#   geom_histogram(bins=120) + 
#   facet_wrap(~site_name, scales="free_y")+
#   scale_x_datetime(date_labels = "%Y", date_minor_breaks = "1 month", name = "time scale")
# ggsave(dpC, "CPCimages.png")

###check specific cameras and correct#

# data %>% filter(site_name == "TUW20")%>%
#   #filter((month(DateTime)==3:4))%>% ##to zoom into a specific month
#   #filter(DateTime < "2021-01-01") %>% ##to zoom into over a specific date
#   ggplot(aes(x = as.POSIXct(DateTime))) + 
#   geom_histogram(bins=120) + 
#   facet_wrap(~site_name, scales="free_y")
# 
# ###check table of values
# d <- data %>% filter(site_name == "TUW36b") %>%
#   #filter((month(DateTime)==3:4))%>%
#   #filter(DateTime < "2021-01-01") %>%
#   dplyr::arrange(DateTime)
# View(d)
###add periods of malfunctioning to a dataframe

#### add periods of malfunctioning to a dataframe #
# 
# site_name<-"TUW2"
# malf1_start<-"2020-10-02 10:28:46"
# malf1_end<-"2020-10-26 13:09:11"
# 
# TUW2 <- as.data.frame(cbind(site_name, malf1_start, malf1_end))
# 
# site_name<-"TUW7"
# malf1_start<-"2020-12-09 15:05:16"
# malf1_end<-"2020-12-16 09:08:33"
# malf2_start<-"2020-12-29 09:41:13"
# malf2_end<-"2021-01-15 11:48:53"
# 
# TUW7 <- as.data.frame(cbind(site_name, malf1_start, malf1_end, malf2_start, malf2_end))
# 
# site_name<-"TUW9"
# malf1_start<-"2020-10-09 12:43:50"
# malf1_end<-"2020-10-14 14:50:04"
# malf2_start<-"2020-12-31 13:26:08"
# malf2_end<-"2021-02-27 16:59:48"
# 
# TUW9 <- as.data.frame(cbind(site_name, malf1_start, malf1_end, malf2_start, malf2_end))
# 
# site_name<-"TUW10"
# malf1_start<-"2020-12-29 15:46:24"
# malf1_end<-"2021-01-04 13:20:58"
# 
# TUW10 <- as.data.frame(cbind(site_name, malf1_start, malf1_end))
# 
# site_name<-"TUW11"
# malf1_start<-"2020-11-23 09:38:16"
# malf1_end<-"2020-12-14 08:42:10"
# 
# TUW11 <- as.data.frame(cbind(site_name, malf1_start, malf1_end))
# 
# site_name<-"TUW19"
# malf1_start<-"2020-11-03 10:41:19"
# malf1_end<-"2020-11-16 14:44:50"
# malf2_start<-"2021-04-18 14:38:24"
# malf2_end<-"2021-05-01 16:00:31"
# 
# TUW19 <- as.data.frame(cbind(site_name, malf1_start, malf1_end, malf2_start, malf2_end))
# 
# site_name<-"TUW21"
# malf1_start<-"2020-11-03 09:45:24"
# malf1_end<-"2020-11-16 11:18:10"
# malf2_start<-"2021-03-25 13:31:40"
# malf2_end<-"2021-04-01 13:38:21"
# 
# TUW21 <- as.data.frame(cbind(site_name, malf1_start, malf1_end, malf2_start, malf2_end))
# 
# site_name<-"TUW23"
# malf1_start<-"2021-04-18 15:34:29"
# malf1_end<-"2021-05-01 17:47:00"
# 
# TUW23 <- as.data.frame(cbind(site_name, malf1_start, malf1_end))
# 
# site_name<-"TUW24"
# malf1_start<-"2021-05-24 10:24:57"
# malf1_end<-"2021-05-29 03:50:54"
# 
# TUW24 <- as.data.frame(cbind(site_name, malf1_start, malf1_end))
# 
# site_name<-"TUW27"
# malf1_start<-"2020-10-27 10:50:37"
# malf1_end<-"2020-11-07 13:26:36"
# malf2_start<-"2021-01-17 11:39:29"
# malf2_end<-"2021-01-23 09:50:09"
# malf3_start<-"2021-04-08 14:27:24"
# malf3_end<-"2021-05-01 06:25:39"
# 
# TUW27 <- as.data.frame(cbind(site_name, malf1_start, malf1_end, malf2_start, malf2_end, malf3_start, malf3_end))
# 
# site_name<-"TUW28"
# malf1_start<-"2020-10-09 10:47:01"
# malf1_end<-"2020-10-24 11:37:21"
# malf2_start<-"2021-08-05 15:37:56"
# malf2_end<-"2021-08-28 17:16:56"
# 
# TUW28 <- as.data.frame(cbind(site_name, malf1_start, malf1_end, malf2_start, malf2_end))
# 
# site_name<-"TUW30"
# malf1_start<-"2020-11-11 14:33:34"
# malf1_end<-"2020-11-17 10:13:29"
# malf2_start<-"2021-02-02 12:21:47"
# malf2_end<-"2021-02-09 20:58:18"
# malf3_start<-"2021-03-08 05:16:06"
# malf3_end<-"2021-04-01 19:01:59"
# malf4_start<-"2021-04-23 20:35:33"
# malf4_end<-"2021-05-04 17:47:57"
# malf5_start<-"2021-06-19 08:52:10"
# malf5_end<-"2021-07-01 17:39:56"
# 
# TUW30 <- as.data.frame(cbind(site_name, malf1_start, malf1_end, malf2_start, malf2_end, malf3_start, malf3_end, malf4_start, malf4_end, malf5_start, malf5_end))
# 
# site_name<-"TUW31"
# malf1_start<-"2021-05-04 17:23:39"
# malf1_end<-"2021-07-01 17:28:26"
# 
# TUW31 <- as.data.frame(cbind(site_name, malf1_start, malf1_end))
# 
# site_name<-"TUW33"
# malf1_start<-"2020-10-05 13:59:54"
# malf1_end<-"2020-10-11 13:43:19"
# malf2_start<-"2020-10-14 20:10:54"
# malf2_end<-"2020-10-22 07:21:55"
# 
# TUW33 <- as.data.frame(cbind(site_name, malf1_start, malf1_end, malf2_start, malf2_end))
# 
# site_name<-"TUW34"
# malf1_start<-"2020-11-22 09:54:53"
# malf1_end<-"2020-11-27 09:52:16"
# malf2_start<-"2020-12-23 22:14:08"
# malf2_end<-"2020-12-28 15:13:49"
# malf3_start<-"2021-02-15 12:37:56"
# malf3_end<-"2021-02-21 12:52:48"
# 
# TUW34 <- as.data.frame(cbind(site_name, malf1_start, malf1_end, malf2_start, malf2_end, malf3_start, malf3_end))
# 
# site_name<-"TUW35b"
# malf1_start<-"2020-12-05 15:56:19"
# malf1_end<-"2020-12-28 15:36:03"
# 
# TUW35b <- as.data.frame(cbind(site_name, malf1_start, malf1_end))
# 
# site_name<-"TUW36b"
# malf1_start<-"2021-06-02 08:07:32"
# malf1_end<-"2021-06-08 18:11:39"
# 
# TUW36b <- as.data.frame(cbind(site_name, malf1_start, malf1_end))
# 
# site_name<-"TUW37b"
# malf1_start<-"2021-01-02 11:46:40"
# malf1_end<-"2021-01-07 14:52:05"
# malf2_start<-"2021-01-17 15:29:06"
# malf2_end<-"2021-01-24 10:57:07"
# 
# TUW37b <- as.data.frame(cbind(site_name, malf1_start, malf1_end, malf2_start, malf2_end))
# 
# site_name<-"TUW39"
# malf1_start<-"2021-04-05 10:37:29"
# malf1_end<-"2021-05-01 12:08:29"
# 
# TUW39 <- as.data.frame(cbind(site_name, malf1_start, malf1_end))
# 
# site_name<-"CPC1"
# malf1_start<-"2020-10-28 15:08:40"
# malf1_end<-"2020-11-04 13:23:14"
# 
# CPC1 <- as.data.frame(cbind(site_name, malf1_start, malf1_end))
# 
# site_name<-"CPC3"
# malf1_start<-"2020-11-01 05:21:05"
# malf1_end<-"2020-11-08 09:49:28"
# malf2_start<-"2020-11-20 00:57:49"
# malf2_end<-"2020-12-28 10:45:44"
# 
# CPC3 <- as.data.frame(cbind(site_name, malf1_start, malf1_end, malf2_start, malf2_end))
# 
# 
# # TUW42 <- will be sorted separately, not doing here
# 
# malf <- rbind.fill(TUW2, TUW7, TUW9, TUW10, TUW11, TUW19, TUW21, TUW23, TUW24, TUW27, TUW28,
#                    TUW30, TUW31, TUW33, TUW34, TUW35b, TUW36b, TUW37b, TUW39, CPC1, CPC3)
# #write.csv(malf, "malf.csv")
# 
# data <- left_join(data, malf, by="site_name")
# 
# 

####reading malfunction dates from csv ####
malf<-read_csv("malf.csv") %>% select(-...1)
dp_malf<- left_join(dp, malf, by="site_name")
dp_malf<- dp_malf%>%
  mutate(date_start = as.Date(start))%>%
  mutate(date_end = as.Date(end))%>%
  select(!start)%>%
  select(!end)

# add malfunction dates
data<- left_join(data, dp_malf, by = "site_name")


####counting presence per week #####

#filter malfunction dates
data_counts_pre<- data %>% 
  # remove empty records
  filter(!is.na(common_name)) %>% 
  # change the malfunction dates to dates instead of character
  mutate(malf1_start = ymd_hms(malf1_start),
         malf1_end = ymd_hms(malf1_end),
         malf2_start = ymd_hms(malf2_start),
         malf2_end = ymd_hms(malf2_end),
         start = ymd_hms(start),
         end = ymd_hms(end)) %>% 
  #filter dates outside of deployment limits %>%
  filter(DateTime>=start | DateTime<= end) %>% 
  # filter to only the dates that are outside of the malfunctioning periods
  filter(is.na(malf1_start) | !is.na(malf1_start) & (DateTime<=malf1_start | DateTime>= malf1_end)) %>%
  # add a column for the time (in weeks) since the deployment of the camera. 
  mutate(work_week = as.integer(ceiling(difftime(DateTime, start, units = 'weeks')))) %>%
  mutate(year_week = week(DateTime))%>%
  mutate(work_day = as.integer(ceiling(difftime(DateTime, start, units = 'days')))) %>%
  mutate(year_day = yday(DateTime))

##count animals per species per week####
data_counts_week<- data_counts_pre%>%
  # get the table with the number of records per site, species, and week
  dplyr::count(site_name, year_week, common_name,  .drop=FALSE) 

##daily counts if we ever need higher temporal resolution (for future use)####
# data_counts_day<- data_counts_pre%>%
#   dplyr::count(site_name, common_name, work_day1, .drop=FALSE) %>%


##turn counts of abundance into presence/absence####
data_counts_week_presence_absence <- data_counts_week%>% mutate(n=ifelse(n>0,1,n)) #turn abundance into presence/absence



#fill NAs for periods where camera was not deployed####
### define object data2, with whatever count you use, either abundance or by week or by day 
data2<-data_counts_week_presence_absence %>%
  mutate(site_name = factor(site_name))
##create dataframes to fill with the for loop
d <- data.frame(site_name = NA_real_, year_week = NA_real_, common_name = NA_real_, n = NA_real_ )
f <- data.frame(site_name = NA_real_, year_week = NA_real_, common_name = NA_real_, n = NA_real_ )

##for loop to add missing weeks as NAs
for (site in unique(data2$site_name)){
  d1 <- data2[which(data2$site_name == site),]
  for (sp in unique(data2$common_name)){
    d2 <- d1[which(d1$common_name == sp),]
    allweeks <- seq(1, 53, 1)
    absentweeks_len <- length(allweeks[!(allweeks %in% d2$year_week)])
    if (absentweeks_len > 0){
      absentweeks <- allweeks[!(allweeks %in% d2$year_week)]
      missed <- data.frame(site_name = NA_real_, year_week = absentweeks, common_name = NA_real_, n = NA_real_ )
      d2.filled <- rbind(d2, missed)
      d2.filled$common_name <- sp
      d2.filled$site_name <- site
      f<-rbind(f, d2.filled)
    } else {
      f<-rbind(f, d2)
    }
  }
}
d<-f %>% filter(!(is.na(site_name)))

###write dataframe of counts per species, per week, per site (with undeployed/unrevised weeks as NAs) as csv
write.csv(d, "data_counts_week.csv")

###check results of for loop
d%>%
  dplyr::filter(common_name == "coyote")%>%
  dplyr::filter(site_name == "TUW33")

#####create human presence, dogs presence dataframe as covariates ####

###estimate total weeks "deployed" in this case with revised data because we filtered by revised ####
total_weeks_deployed <- aggregate(data = data_counts_pre,                # Applying aggregate
                          year_week ~ site_name,
                          function(year_week) length(unique(year_week)))
total_weeks_deployed <- total_weeks_deployed %>% dplyr::rename("total_weeks_deployed" = year_week)

##estimate total number of humans per site####
data_humans_total<- data_counts_pre%>%
  filter(humans == TRUE) %>%
  # get the table with the number of records per site, species, and week
  dplyr::count(site_name, humans, .drop=FALSE)%>%
  dplyr::select(!humans)%>%
  dplyr::rename(total_humans = "n")
##create dataframe with information
d5 <- left_join(total_weeks_deployed, data_humans_total, by="site_name") 
##divide by total number of days deployed
d5 <- d5 %>% mutate(total_freq_humans = total_humans/total_weeks_deployed)

##estimate total humans per week per site ## in case we need weekly presence of humans for the detection ####
data_humans_weekly <- data_counts_pre%>%
  filter(humans == TRUE) %>%
  # get the table with the number of records per site, species, and week
  dplyr::count(site_name, year_week, humans, .drop=FALSE) 


#count dogs
data_dogs_total <- data_counts_pre %>% mutate(dogs = ifelse(common_name=="dog", TRUE, FALSE)) %>%
  filter(dogs==TRUE) %>%
  # get the table with the number of records per site, species, and week
  dplyr::count(site_name, dogs, .drop=FALSE) %>%
  dplyr::select(!dogs)%>%
  dplyr::rename(total_dogs = "n")
##add dogs to d5 dataframe
d5 <- left_join(d5, data_dogs_total, by="site_name") 
##divide by total number of days deployed
d5 <- d5 %>% mutate(total_freq_dogs = total_dogs/total_weeks_deployed)

###free ranging dogs
data_dogs_free_total <- data_counts_pre %>% mutate(dogs = ifelse(common_name=="dog", TRUE, FALSE))%>% 
  mutate(free_dogs = ifelse(dogs==TRUE & humans==FALSE, TRUE, FALSE)) %>%
  filter(free_dogs==TRUE) %>%
  dplyr::count(site_name, free_dogs, .drop=FALSE) %>%
  dplyr::select(!free_dogs)%>%
  dplyr::rename(total_dogs_free = "n")
d5 <- left_join(d5, data_dogs_free_total, by="site_name")
d5 <- d5 %>% mutate(total_dogs_free = ifelse(is.na(total_dogs_free), 0, total_dogs_free)) %>% 
  mutate(total_freq_dogs_free = total_dogs_free/total_weeks_deployed) %>%
  mutate(total_prop_dogs_free = total_dogs_free/total_dogs)


human_dog_df <- d5
View(human_dog_df)
write.csv(human_dog_df, "human_dog_df.csv")

###number of dogs per wee
d%>% dplyr::filter(common_name == "dog")


# GENERATE DETETION MATRICES FOR OCCU MODELS turn single column data into detection matrix#####

###read "data_counts_week.csv" to avoid all previous code using:
### use the github version for the latest update

d <- read.csv("https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/data_counts_week.csv")

#1. filter ANIMAL SPECIES of interest ### E.G detection matrix for coyotes

d1 <- d %>% dplyr::filter(common_name == "coyote")

#2. split column into weely tables and rejoin by site_name
d2<- d1%>%
  mutate(n= as.numeric(n))%>%
  group_split(year_week)%>% 
  join_all(by="site_name", type="left")
d3<- as.data.frame(d2)
names(d3)<- make.names(names(d3), unique=TRUE)
d4 <- d3%>% select(-starts_with('y'), - starts_with('c'))

coyote<-d4
###trying to do loop in progress
for (sp in unique(d$common_name)){
  d1<- d %>% dplyr::filter(common_name == sp)
  d2<- d1%>%
    mutate(n= as.numeric(n))%>%
    group_split(year_week)%>% 
    join_all(by="site_name", type="left")
  d3<- as.data.frame(d2)
  names(d3)<- make.names(names(d3), unique=TRUE)
  d4 <- d3%>% select(-starts_with('y'), - starts_with('c'))
  assign(paste0("test", sp), sp) <- d4
  write.csv(d4, "ad.csv")
}


##GENERATE COVARIATE dataframes for the model , make sure to readapt the site_names AND add human/dog presence####
urlfile500="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_500.csv"
urlfile1000="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_1000.csv"
urlfile2000="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/cov_2000.csv"
urlfilehumans="https://raw.githubusercontent.com/tgelmi-candusso/cameratrap_analysis/main/human_dog_df.csv"

human_dog_df <- read.csv(urlfilehumans) %>% select(-1)

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
