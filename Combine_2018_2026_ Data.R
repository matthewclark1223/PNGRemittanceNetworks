#The goal of this script is to combine 2018 and 2026 household data for a panel regression.
library(tidyverse)

#Load 2018 and 2026 data
HH2018<-readRDS("data_clean/HH2018_clean.rds")
HH2026<-readRDS("data_clean/HH2026_clean.rds")

##add leadership position from indiv data
Ind2018<-readRDS("data_clean/Ind2018_clean.rds") 
Ind2026<-readRDS("data_clean/Ind2026_clean.rds")

HH2018<-HH2018 %>% 
  left_join(Ind2018 %>% select(IndivID, leader18,birthyear18), #get leadership status of HHH 
            join_by("HHH_IndivID" == "IndivID"))%>% 
  rename("leader"="leader18")%>%mutate(
    # First, turn 999 into NA
    leader = na_if(leader, 999),
    # Then, map 0 to FALSE and 1 to TRUE (leaving NAs alone)
    leader = case_when(
      leader == 0 ~ FALSE,
      leader == 1 ~ TRUE,
      TRUE       ~ leader
    )
  )%>%rename("ageHHH"="birthyear18")%>%mutate(ageHHH=2018-ageHHH)
  
HH2026<-HH2026 %>% 
  left_join(Ind2026 %>% select(IndivID, leader,birthyear), #get leadership status 
            join_by("HHH_IndivID" == "IndivID"))%>% 
  rename("leader"="leader")%>%mutate(
    # First, turn 999 into NA
    leader = na_if(leader, 999),
    # Then, map 0 to FALSE and 1 to TRUE (leaving NAs alone)
    leader = case_when(
      leader == 0 ~ FALSE,
      leader == 1 ~ TRUE,
      TRUE       ~ leader
    )
  )%>%rename("ageHHH"="birthyear")%>%mutate(ageHHH=2026-ageHHH)

rm(Ind2026)
rm(Ind2018)


#Select only columns we need for this analysis 
HH2018<-HH2018%>%select(
  SharingUnitID,
  n_adults,n_children,
  livelihoods.activity,
  livelihoods.rank.activity_fishing,
  livelihoods.rank.activity_gleaning,                    
  livelihoods.rank.activity_marketing_mp,
  livelihoods.rank.activity_salaried_employment,         
  livelihoods.rank.activity_informal_ea,
  livelihoods.rank.activity_tourism,                     
  livelihoods.rank.activity_farming,                    
  livelihoods.rank.activity_remittance,                  
  livelihoods.rank.activity_other,
  livelihoods.remittances,
  MSL2.expenditure,
  MSL2.credit_access,ageHHH,
  fishingq.food_consumption,
  #food security q's
  food_security.no_eat_day,
  food_security.sleep_hungry,
  food_security.no_food_HH,
  food_security.fewer_meals,
  food_security.smaller_meal,leader
)

food_vars <- c(
  "food_security.no_eat_day",
  "food_security.sleep_hungry",
  "food_security.no_food_HH",
  "food_security.fewer_meals",
  "food_security.smaller_meal"
)

HH2018 <- HH2018 %>%
  mutate(
    across(
      all_of(food_vars),
      ~ case_when(
        . %in% c("never", "Never") ~ 0,
        . %in% c("rarely", "Rarely") ~ 1,
        . %in% c("sometimes", "Sometimes") ~ 2,
        . %in% c("often", "Often") ~ 3,
        TRUE ~ NA_real_
      )
    ))%>%
  mutate(
    food_insecurity_score = rowSums(
      select(., all_of(food_vars)),
      na.rm = TRUE
    )
  )

# #####
# RankCols<-names(HH2018)[grep("rank",names(HH2018))]
# 
# HH2018 <- HH2018 %>%
#   rowwise() %>%
#   mutate(
#     max_livelihood_rank = max(c_across(RankCols), na.rm = TRUE)
#   ) %>%
#   mutate(
#     across(RankCols,
#            ~ ifelse(is.na(.), NA, max_livelihood_rank + 1 - .),
#            .names = "w_{.col}")
#   ) %>%
#   ungroup()%>%filter(is.finite(max_livelihood_rank)==TRUE)
# 
# HH2018 <- HH2018 %>%
#   rowwise() %>%
#   mutate(
#     total_weight = sum(c_across(starts_with("w_")), na.rm = TRUE)
#   ) %>%
#   mutate(
#     across(starts_with("w_"),
#            ~ . / total_weight,
#            .names = "share_{.col}")
#   ) %>%
#   ungroup()


# HH2018 <- HH2018 %>%
#   mutate(fishing_dependence = share_w_livelihoods.rank.activity_fishing,
#          remittance_dependence = share_w_livelihoods.rank.activity_remittance)


#Now do the same for 2026
# 
# #Select only columns we need for this analysis
 HH2026<-HH2026%>%select(
   SharingUnitID,
   n_adults,n_children,
   livelihoods.activity,
   livelihoods.rank.activity_fishing,
   livelihoods.rank.activity_gleaning,                    
   livelihoods.rank.activity_marketing_mp,
   livelihoods.rank.activity_atoll_farming,
   livelihoods.rank.activity_salaried_employment,         
   livelihoods.rank.activity_informal_ea,
   livelihoods.rank.activity_tourism,                     
   livelihoods.rank.activity_farming,                    
   livelihoods.rank.activity_remittance,                  
   livelihoods.rank.activity_other,
   livelihoods.remittances,ageHHH,
   MSL2.credit_access,
   MSL2.expenditure,
   fishingq.food_consumption,
   #food security q's
   food_security.no_eat_day,
   food_security.sleep_hungry,
   food_security.no_food_HH,
   food_security.fewer_meals,
   food_security.smaller_meal,leader
 )



HH2026 <- HH2026 %>%
  mutate(
    across(
      all_of(food_vars),
      ~ case_when(
        . %in% c("never", "Never") ~ 0,
        . %in% c("rarely", "Rarely") ~ 1,
        . %in% c("sometimes", "Sometimes") ~ 2,
        . %in% c("often", "Often") ~ 3,
        TRUE ~ NA_real_
      )
    ))%>%
  mutate(
    food_insecurity_score = rowSums(
      select(., all_of(food_vars)),
      na.rm = TRUE
    )
  )

# 
# RankCols<-names(HH2026)[grep("rank",names(HH2026))]
# 
# HH2026 <- HH2026 %>%
#   rowwise() %>%
#   mutate(
#     max_livelihood_rank = max(c_across(RankCols), na.rm = TRUE)
#   ) %>%
#   mutate(
#     across(RankCols,
#            ~ ifelse(is.na(.), NA, max_livelihood_rank + 1 - .),
#            .names = "w_{.col}")
#   ) %>%
#   ungroup()%>%filter(is.finite(max_livelihood_rank)==TRUE)
# 
# HH2026 <- HH2026 %>%
#   rowwise() %>%
#   mutate(
#     total_weight = sum(c_across(starts_with("w_")), na.rm = TRUE)
#   ) %>%
#   mutate(
#     across(starts_with("w_"),
#            ~ . / total_weight,
#            .names = "share_{.col}")
#   ) %>%
#   ungroup()
# 
# 
# HH2026 <- HH2026 %>%
#   mutate(fishing_dependence = share_w_livelihoods.rank.activity_fishing,
#          remittance_dependence = share_w_livelihoods.rank.activity_remittance)


#Harmonize column names and add Year column
HH2018<-HH2018%>%select(
  SharingUnitID,
  n_adults,n_children,
  livelihoods.remittances,
  livelihoods.activity,
  MSL2.credit_access,
  MSL2.expenditure,
  livelihoods.rank.activity_fishing,
  livelihoods.rank.activity_gleaning,                    
  livelihoods.rank.activity_marketing_mp,
  livelihoods.rank.activity_salaried_employment,         
  livelihoods.rank.activity_informal_ea,
  livelihoods.rank.activity_tourism,                     
  livelihoods.rank.activity_farming,                    
  livelihoods.rank.activity_remittance,                  
  livelihoods.rank.activity_other,ageHHH,
  fishingq.food_consumption,food_insecurity_score,leader
)%>%mutate(Year="2018")%>%mutate(livelihoods.rank.activity_atoll_farming=NA)

HH2026<-HH2026%>%select(
  SharingUnitID,
  n_adults,n_children,
  livelihoods.remittances,
  MSL2.credit_access,
  MSL2.expenditure,
  livelihoods.rank.activity_fishing,
  livelihoods.activity,
  livelihoods.rank.activity_gleaning,
  livelihoods.rank.activity_marketing_mp,
  livelihoods.rank.activity_atoll_farming,
  livelihoods.rank.activity_salaried_employment,
  livelihoods.rank.activity_informal_ea,
  livelihoods.rank.activity_tourism,
  livelihoods.rank.activity_farming,
  livelihoods.rank.activity_remittance,
  livelihoods.rank.activity_other,ageHHH,
  fishingq.food_consumption,food_insecurity_score,leader
)%>%mutate(Year="2026")


#Combine years

HH_comb<-rbind(HH2018,HH2026)
#HH_comb$fishing_dependence<-ifelse(is.na(HH_comb$fishing_dependence),0,HH_comb$fishing_dependence)
#HH_comb$remittance_dependence<-ifelse(is.na(HH_comb$remittance_dependence),0,HH_comb$remittance_dependence)
HH_comb <- HH_comb %>%
  mutate(
    n_credit_sources = sapply(str_split(MSL2.credit_access, " "), function(x) {
      x <- unique(x)                 # remove duplicates if any
      if ("no" %in% x) return(0)     # treat "no" as zero
      length(x)
    })
  )

HH_comb<-HH_comb %>%
  group_by(SharingUnitID) %>%
  filter(all(c("2018", "2026") %in% Year)) %>%
  ungroup()


source("SummarizeNetworkData.R")

final_df <-  network_summary %>%
  inner_join(HH_comb, by = c("SharingUnitID", "Year"))

source("FishingCommercializationIndex.R")

final_df <-  final_df %>%
  inner_join(HH_FishComm, by = c("SharingUnitID", "Year"))


# HH_comb<-HH_comb %>%
#   mutate(post = ifelse(Year == "2026", 1, 0))
# 
# HH_comb <- HH_comb %>%
#   group_by(SharingUnitID) %>%
#   mutate(fishing_dep_2018 = fishing_dependence[Year == "2018"][1]) %>%
#   ungroup()
# 
# HH_comb%>%
#   filter(Year=="2026")%>%
#   ggplot(.,aes(x=fishing_dep_2018,y=remittance_dependence))+geom_point()


# ggplot(HH_comb,aes(x=as.integer(Year),y=fishingq.food_consumption))+
#   geom_point()+geom_line(aes(color=SharingUnitID)) +
#   theme(legend.position = "none")

