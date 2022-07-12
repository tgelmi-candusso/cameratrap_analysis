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

#####checking depoloyment period is correct####
####
v <- data %>% 
  mutate(within_deployment_period = ifelse(as.Date(DateTime)>=as.Date(start)&as.Date(DateTime)<=as.Date(end),1,0))%>%
  select(site_name, common_name, DateTime, start, end, within_deployment_period)
v.0 <- v%>% filter(within_deployment_period==0)
factor(v.0$site_name) ##CPC5, TUW40, TUW41 have pictures beyond deployment period (must be the checkup, there were subfolders for these sites)

##filter cameras that have inconsistent data
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

# ###visually check for periods when camera was not functioning#####
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

###check specific cameras and correct#####

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

#### add periods of malfunctioning to a dataframe ####
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
#### correct deployment start times for TUW20, 36B & 42 ####

####
malf<-read_csv("malf.csv") %>% select(-...1)
dp_malf<- left_join(dp, malf, by="site_name")
dp_malf<- dp_malf%>%
  mutate(date_start = as.Date(start))%>%
  mutate(date_end = as.Date(end))%>%
  select(!start)%>%
  select(!end)
#View(dp_malf)
## then we will use these malf columns and the real start and deployment date columns to set the NAs where we did with weeks but using DateTime > malf1_start and so on

##Im rethinking this and there should be a better way for this, using camtrapR, that
# data1 <- data%>%
#   dplyr::filter(!(is.na(common_name)))%>%
#   mutate(date = as.Date(DateTime))%>%
#   dplyr::count(site_name, date,common_name,  .drop=FALSE) #key line to change if we want daily occurrence
#   ##double check the deployment start and end, because somehow if they are  minute over or under they are considered as outside, see line
#   ##line 50, if we fix it there then we just use the column if within deployment is 1, then yes and if 0 then NA, instead of the next line
#   
# data2<- left_join(data1, dp_malf, by = "site_name")
# 
# data3<-data2%>%
#   mutate(n = ifelse(date>=date_start&date<=date_end,n,NA))
#   ##here we put the malf_start and end dates
# data4<- data3%>%
#   #filter(site_name == Sca) %>%
#   mutate(malf_n=ifelse(date>as.Date(malf1_start)&date<as.Date(malf1_end),1,
#                        (ifelse(date>as.Date(malf2_start)&date<as.Date(malf2_end),1,
#                                (ifelse(date>as.Date(malf3_start)&date<as.Date(malf3_end),1,0))))))
#   ## and so forth if there there are cameras with a malf3 and malf4, columns, it should be ok with the NAs, it should just give n

#####trying stuff out####
View(data4)
write.csv(data4, "TUW_datacounts.csv")
data1$common_name
data4 %>% 
  #filter(site_name == "TUW19")%>%
  ggplot(aes(x = as.POSIXct(date))) + 
  geom_histogram(bins=364) + 
  facet_wrap(~site_name, scales="free")+
  scale_x_datetime(date_labels = "%Y", date_minor_breaks = "1 month", name = "time scale")
##when dataframe is ready we add covarites here is a script with the correct fix for the site_names.
d<- as.data.frame(seq(as.Date("2020/9/15"), by = "day", length.out = 390))
colnames(d)<- "date"

f<-left_join(d, data4, by="date")
View(f)
sp<- levels(data$common_name)
sn<- levels(data$site_name)
for (i in unique(data4$common_name)){
  for (j in unique(data4$site_name)){
    g<- data4 %>% 
      dplyr::filter(common_name == i)%>%
      dplyr::filter(site_name == j) %>%
      tidyr:: complete(date = seq.Date(min(date), max(date), by="day"))
    data5 <- rbind(data5, g)
  }
}
for (i in sp){
  g<- data4 %>% 
    dplyr::filter(common_name == i)
  for (j in sn){
    g<- data4 %>% 
      dplyr::filter(site_name == j) %>%
      tidyr:: complete(date = seq.Date(min(date), max(date), by="day"))
    data6<- rbind(data5, g)
  }
}
data5 <-data4%>% filter(common_name=="not listed") %>% 
  filter(site_name == "TUW27")%>%
complete(date = seq.Date(min(date), max(date), by="day"))
View(data5)
data6



#### Juan edits 8/7/22 ####
## I am editing the code to obtain a table of records for each station, species, and week. 
# We can change the time unit for the occupancy analysis, but a week is usually appropriate. 
data<- left_join(data, dp_malf, by = "site_name")
#View(data)
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

data_counts_week<- data_counts_pre%>%
  # mutate(year_week = factor(year_week))%>%
  # mutate(site_name = factor(site_name)) %>%
  # mutate(common_name = factor(common_name)) %>%
  # get the table with the number of records per site, species, and week
  dplyr::count(site_name, year_week, common_name,  .drop=FALSE) 
#View(data_counts_week)

##checking out whether undeployed weeks appear, they dont
tr<-data_counts_week%>%
  filter(site_name == "TUW36b") %>%
  filter(common_name== "deer")
View(tr)
write.csv(data_counts_week, "data_counts_week.csv")

##daily counts if we ever need higher temporal resolution
# data_counts_day<- data_counts_pre%>%
#   # get the table with the number of records per site, species, and week
#   dplyr::count(site_name, common_name, work_day1, .drop=FALSE) %>%
#   separate(work_day1, c("work_day", "day_of_year"))

#turn abundance into presence/absence
data_counts_week_presence_absence <- data_counts_week%>% mutate(n=ifelse(n>0,1,n)) #turn abundance into presence/absence

#fill NAs
data2<-data_counts_week_presence_absence %>%
  mutate(site_name = factor(site_name))


d <- data.frame(site_name = NA_real_, year_week = NA_real_, common_name = NA_real_, n = NA_real_ )
f <- data.frame(site_name = NA_real_, year_week = NA_real_, common_name = NA_real_, n = NA_real_ )

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
  d<-rbind(d,f)
}
d<-d %>% filter(!(is.na(site_name)))


#turn single column data into detection matrix
#1. filter animal of interest
d1 <- d%>%
  dplyr::filter(common_name == "coyote")
d2<- d1%>%
  mutate(n= as.numeric(n))%>%
  group_split(year_week)%>% 
  join_all(by="site_name", type="left")
d3<- as.data.frame(d2)
names(d3)<- make.names(names(d3), unique=TRUE)
d4 <- d3 %>% dplyr::select(n, n.1, n.2, n.3, n.4, n.5, n.6, n.7, n.8, n.9, n.10, n.11, n.12, n.13, n.14, n.15, n.16, n.17, n.18, n.19, n.20, n.21)
colnames(d4)<- c("week1", "week2", "week3", "week4", "week5", "week13", "week14", "week15", "week16", "week17", "week18", "week26", "week27", "week28", "week29", "week30", "week31", "week40", "week41", "week42", "week43", "week44")
View(d3)

d2<- d%>%
  mutate(n= as.numeric(n))%>%
  group_split(year_week)%>% 
  join_all(by="site_name", type="left")
d3<- as.data.frame(d2)
names(d3)<- make.names(names(d3), unique=TRUE)
d4 <- d3 %>% dplyr::select(n, n.1, n.2, n.3, n.4, n.5, n.6, n.7, n.8, n.9, n.10, n.11, n.12, n.13, n.14, n.15, n.16, n.17, n.18, n.19, n.20, n.21)
colnames(d4)<- c("week1", "week2", "week3", "week4", "week5", "week13", "week14", "week15", "week16", "week17", "week18", "week26", "week27", "week28", "week29", "week30", "week31", "week40", "week41", "week42", "week43", "week44")
View(d3)



##add covariates, make sure to readapt the site_names####
b500 <- read_csv("cov_500.csv")%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))%>%
  select(-deployment_start, -deployment_end, -status)
data.500 <- left_join(data, b500, by="site_name")

b1000 <- read_csv("cov_1000.csv")%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))%>%
  select(-deployment_start, -deployment_end, -status)
data.1000 <- left_join(data, b1000, by="site_name")

b2000 <- read_csv("cov_2000.csv")%>%
  mutate(site_name = gsub("_", "", site_name))%>%
  mutate(site_name = gsub("TUW0", "TUW", site_name))%>%
  select(-deployment_start, -deployment_end, -status)
data.2000 <- left_join(data, b2000, by="site_name")



