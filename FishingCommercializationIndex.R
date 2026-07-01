########### Make fishing commercialization indices. Load data again, then carry over summarized variables
#Load 2018 and 2026 data
HH2018<-readRDS("data_clean/HH2018_clean.rds")
HH2026<-readRDS("data_clean/HH2026_clean.rds")

#commerial share
HH2018$Com_index_commercial_share<- 1 - (HH2018$fishingq.food_consumption / 10)
HH2026$Com_index_commercial_share<- 1 - (HH2026$fishingq.food_consumption / 10)
#place of sale
HH2018<-HH2018%>%mutate(
  
  Com_index_markets =
    
    ifelse(
      !is.na(fishingq.rank_fishmarket.fish_market_village),
      1 / fishingq.rank_fishmarket.fish_market_village,
      0
    ) +
    
    ifelse(
      !is.na(fishingq.rank_fishmarket.fish_market_local_market),
      2 / fishingq.rank_fishmarket.fish_market_local_market,
      0
    ) +
    
    ifelse(
      !is.na(fishingq.rank_fishmarket.fish_market_provincial_market),
      3 / fishingq.rank_fishmarket.fish_market_provincial_market,
      0
    ) +
    
    ifelse(
      !is.na(fishingq.rank_fishmarket.fish_market_fish_trader),
      4 / fishingq.rank_fishmarket.fish_market_fish_trader,
      0
    )
  
)




HH2026<-HH2026%>%mutate(
  
  Com_index_markets =
    
    ifelse(
      !is.na(fishingq.rank_fishmarket.fish_market_village),
      1 / fishingq.rank_fishmarket.fish_market_village,
      0
    ) +
    
    ifelse(
      !is.na(fishingq.rank_fishmarket.fish_market_local_market),
      2 / fishingq.rank_fishmarket.fish_market_local_market,
      0
    ) +
    
    ifelse(
      !is.na(fishingq.rank_fishmarket.fish_market_provincial_market),
      3 / fishingq.rank_fishmarket.fish_market_provincial_market,
      0
    ) +
    
    ifelse(
      !is.na(fishingq.rank_fishmarket.fish_market_fish_trader),
      4 / fishingq.rank_fishmarket.fish_market_fish_trader,
      0
    )
  
)

## score for fishing gear
HH2018<-HH2018%>%
  
  mutate(
    
    boat_motor = ifelse(fishingq.access_boat == "yes", 1, 0),
    esky = ifelse(fishingq.access_esky == "yes", 1, 0),
    freezer = ifelse(fishingq.access_freezer == "yes", 1, 0),
    
    Com_index_capital =
      2 * boat_motor +
      1 * esky +
      3 * freezer
  )%>%

  
  mutate(
 
    #Need to differentiate between canoe trolling and motorboat trolling
    fishingq.equipment.rank_gear.gear_canoe_trolling= fishingq.equipment.rank_gear.gear_trollingline*abs(1-boat_motor),
  fishingq.equipment.rank_gear.gear_pelagic_trolling= fishingq.equipment.rank_gear.gear_trollingline*boat_motor)%>%
  
  mutate(fishingq.equipment.rank_gear.gear_canoe_trolling=
           ifelse(fishingq.equipment.rank_gear.gear_canoe_trolling==0,NA,fishingq.equipment.rank_gear.gear_canoe_trolling),
         fishingq.equipment.rank_gear.gear_pelagic_trolling=
           ifelse(fishingq.equipment.rank_gear.gear_pelagic_trolling==0,NA,fishingq.equipment.rank_gear.gear_pelagic_trolling))%>%
  mutate(
    
    Com_index_gear =
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_gleaning),
        0 / fishingq.equipment.rank_gear.gear_gleaning,
        0
      ) +
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_handline),
        1 / fishingq.equipment.rank_gear.gear_handline,
        0
      ) +
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_canoe_trolling),
        1 / fishingq.equipment.rank_gear.gear_canoe_trolling,
        0
      ) +
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_harpoon),
        2 / fishingq.equipment.rank_gear.gear_harpoon,
        0
      ) +
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_speargun),
        1 / fishingq.equipment.rank_gear.gear_speargun,
        0
      ) +
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_pelagic_trolling),
        3 / fishingq.equipment.rank_gear.gear_pelagic_trolling,
        0
      ) +
      
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_small_gillnet),
        2 / fishingq.equipment.rank_gear.gear_small_gillnet,
        0
      ) +
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_big_gillnet),
        4 / fishingq.equipment.rank_gear.gear_big_gillnet,
        0
      )
    
  )
 

## score for fishing gear
HH2026<-HH2026%>% mutate(
  
  boat_motor = ifelse(fishingq.access_boat == "yes", 1, 0),
  esky = ifelse(fishingq.access_esky == "yes", 1, 0),
  freezer = ifelse(fishingq.access_freezer == "yes", 1, 0),
  
  Com_index_capital =
    2 * boat_motor +
    1 * esky +
    3 * freezer
)%>%

  mutate(
    #Need to differentiate between canoe trolling and motorboat trolling
    fishingq.equipment.rank_gear.gear_canoe_trolling= fishingq.equipment.rank_gear.gear_trollingline*abs(1-boat_motor),
    fishingq.equipment.rank_gear.gear_pelagic_trolling= fishingq.equipment.rank_gear.gear_trollingline*boat_motor)%>%
  
  mutate(fishingq.equipment.rank_gear.gear_canoe_trolling=
           ifelse(fishingq.equipment.rank_gear.gear_canoe_trolling==0,NA,fishingq.equipment.rank_gear.gear_canoe_trolling),
         fishingq.equipment.rank_gear.gear_pelagic_trolling=
           ifelse(fishingq.equipment.rank_gear.gear_pelagic_trolling==0,NA,fishingq.equipment.rank_gear.gear_pelagic_trolling))%>%
  mutate(
    
    Com_index_gear =
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_gleaning),
        0 / fishingq.equipment.rank_gear.gear_gleaning,
        0
      ) +
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_handline),
        1 / fishingq.equipment.rank_gear.gear_handline,
        0
      ) +
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_canoe_trolling),
        1 / fishingq.equipment.rank_gear.gear_canoe_trolling,
        0
      ) +
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_harpoon),
        2 / fishingq.equipment.rank_gear.gear_harpoon,
        0
      ) +
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_speargun),
        1 / fishingq.equipment.rank_gear.gear_speargun,
        0
      ) +
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_pelagic_trolling),
        3 / fishingq.equipment.rank_gear.gear_pelagic_trolling,
        0
      ) +
      
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_small_gillnet),
        2 / fishingq.equipment.rank_gear.gear_small_gillnet,
        0
      ) +
      
      ifelse(
        !is.na(fishingq.equipment.rank_gear.gear_big_gillnet),
        4 / fishingq.equipment.rank_gear.gear_big_gillnet,
        0
      )
    
  )


### add an indicator of pelagic focus
HH2018<-HH2018%>%
  mutate(
    pelagic_focus_index =
      
      (
        ifelse(!is.na(fishingq.equipment.rank_gear.gear_gleaning),
               0 * (1/fishingq.equipment.rank_gear.gear_gleaning),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_speargun),
                 0 * (1/fishingq.equipment.rank_gear.gear_speargun),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_harpoon),
                 0 * (1/fishingq.equipment.rank_gear.gear_harpoon),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_handline),
                 0 * (1/fishingq.equipment.rank_gear.gear_handline),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_canoe_trolling),
                 0.5 * (1/fishingq.equipment.rank_gear.gear_canoe_trolling),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_small_gillnet),
                 0 * (1/fishingq.equipment.rank_gear.gear_small_gillnet),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_pelagic_trolling),
                 1 * (1/fishingq.equipment.rank_gear.gear_pelagic_trolling),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_big_gillnet),
                 1 * (1/fishingq.equipment.rank_gear.gear_big_gillnet),0)
      ) /
      
      (
        ifelse(!is.na(fishingq.equipment.rank_gear.gear_gleaning),
               1/fishingq.equipment.rank_gear.gear_gleaning,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_speargun),
                 1/fishingq.equipment.rank_gear.gear_speargun,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_harpoon),
                 1/fishingq.equipment.rank_gear.gear_harpoon,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_handline),
                 1/fishingq.equipment.rank_gear.gear_handline,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_canoe_trolling),
                 1/fishingq.equipment.rank_gear.gear_canoe_trolling,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_small_gillnet),
                 1/fishingq.equipment.rank_gear.gear_small_gillnet,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_pelagic_trolling),
                 1/fishingq.equipment.rank_gear.gear_pelagic_trolling,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_big_gillnet),
                 1/fishingq.equipment.rank_gear.gear_big_gillnet,0)
      )
  )

HH2026<-HH2026%>%
  mutate(
    pelagic_focus_index =
      
      (
        ifelse(!is.na(fishingq.equipment.rank_gear.gear_gleaning),
               0 * (1/fishingq.equipment.rank_gear.gear_gleaning),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_speargun),
                 0 * (1/fishingq.equipment.rank_gear.gear_speargun),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_harpoon),
                 0 * (1/fishingq.equipment.rank_gear.gear_harpoon),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_handline),
                 0 * (1/fishingq.equipment.rank_gear.gear_handline),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_canoe_trolling),
                 0.5 * (1/fishingq.equipment.rank_gear.gear_canoe_trolling),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_small_gillnet),
                 0 * (1/fishingq.equipment.rank_gear.gear_small_gillnet),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_pelagic_trolling),
                 1 * (1/fishingq.equipment.rank_gear.gear_pelagic_trolling),0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_big_gillnet),
                 1 * (1/fishingq.equipment.rank_gear.gear_big_gillnet),0)
      ) /
      
      (
        ifelse(!is.na(fishingq.equipment.rank_gear.gear_gleaning),
               1/fishingq.equipment.rank_gear.gear_gleaning,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_speargun),
                 1/fishingq.equipment.rank_gear.gear_speargun,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_harpoon),
                 1/fishingq.equipment.rank_gear.gear_harpoon,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_handline),
                 1/fishingq.equipment.rank_gear.gear_handline,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_canoe_trolling),
                 1/fishingq.equipment.rank_gear.gear_canoe_trolling,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_small_gillnet),
                 1/fishingq.equipment.rank_gear.gear_small_gillnet,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_pelagic_trolling),
                 1/fishingq.equipment.rank_gear.gear_pelagic_trolling,0) +
          
          ifelse(!is.na(fishingq.equipment.rank_gear.gear_big_gillnet),
                 1/fishingq.equipment.rank_gear.gear_big_gillnet,0)
      )
  )





HH_FishComm2018<-HH2018[,c("SharingUnitID","Com_index_commercial_share","Com_index_markets",
                           "Com_index_capital","Com_index_gear","pelagic_focus_index")]%>%mutate(Year="2018")
HH_FishComm2026<-HH2026[,c("SharingUnitID","Com_index_commercial_share","Com_index_markets",
                           "Com_index_capital","Com_index_gear","pelagic_focus_index")]%>%mutate(Year="2026")

HH_FishComm<-rbind(HH_FishComm2018,HH_FishComm2026)

# plot(HH2018[,c("Com_index_commercial_share","Com_index_markets",
#                "Com_index_capital","Com_index_gear")])
# plot(HH2026[,c("Com_index_commercial_share","Com_index_markets",
#                "Com_index_capital","Com_index_gear")])
# 
# GGally::ggpairs(HH2026[,c("Com_index_commercial_share","Com_index_markets",
#                           "Com_index_capital","Com_index_gear")],columnLabels = c("commercial share","Markets","Assets","Gear"))+
#   theme_bw()


 GGally::ggpairs(HH_FishComm[,c("Com_index_commercial_share","Com_index_markets",
                           "Com_index_capital","Com_index_gear","pelagic_focus_index")])

 