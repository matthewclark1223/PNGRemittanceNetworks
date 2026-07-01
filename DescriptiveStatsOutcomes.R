library(tidyverse)


# Variables for commercialization
com_vars <- c(
  "Com_index_gear",
  "Com_index_markets",
  "Com_index_commercial_share",
  "Com_index_capital"
)

# Long format for plotting
com_long <- final_df %>%
  select(SharingUnitID, Year, all_of(com_vars)) %>%
  mutate(Year = factor(Year, levels = c("2018", "2026"))) %>%
  pivot_longer(
    cols = all_of(com_vars),
    names_to = "indicator",
    values_to = "value"
  )%>%mutate(
    indicator = indicator |>
      recode_values(
        "Com_index_gear" ~ "Gear commercialization",
        "Com_index_markets" ~ "Market commercialization",
        "Com_index_commercial_share" ~ "Commercial share",
        "Com_index_capital" ~ "Asset commercialization"
      )
  )%>%mutate(indicator=factor(indicator,levels=c("Market commercialization",
                                                 "Gear commercialization",
                                                 "Commercial share",
                                                 "Asset commercialization")))

# Plot
p1<-ggplot(com_long,aes(x = Year,y = value,
           group = SharingUnitID)) +
  geom_line(alpha = 0.4,
            color = "grey50") +
  geom_point(alpha = 0.65,
             size = 2) +
  stat_summary(aes(group = 1,color=indicator),
               fun = mean,
               geom = "line",
               linewidth = 1.5) +
  stat_summary(aes(group = 1,color=indicator,fill=indicator),
               fun = mean,
               geom = "point",
               size = 5,shape=21,alpha=0.75,stroke=2) +
  facet_wrap(~indicator,
             scales = "free_y") +
  scale_color_discrete(palette=c("#009E73","#56B4E9","#E69F00","#CC79A7"))+
  scale_fill_discrete(palette=c("#009E73","#56B4E9","#E69F00","#CC79A7"))+
  
  labs(
    x = NULL,
    y = "Index value") +
  ggthemes::theme_clean() +  theme(
    legend.position = "none",
    axis.text = element_text(color="black",
                             size=16),
    axis.title=element_text(color="black",size=18),
      strip.background =element_rect(fill="white"),
      strip.text = element_text(colour = 'black',size=16),
    plot.background = element_rect(colour = "black", fill=NA, linewidth = 1))


ggsave(filename = "DescriptiveOutcomes.png",
         plot = p1,
         width = 10,
         height = 6,
         units = "in",
         dpi = 350,
         bg = "white")



food_vars <- c(
  "food_insecurity_score",
  "fish_indegree"
)

food_long <- final_df %>%
  select(SharingUnitID, Year, all_of(food_vars)) %>%
  mutate(Year = factor(Year, levels = c("2018", "2026"))) %>%
  pivot_longer(
    cols = all_of(food_vars),
    names_to = "indicator",
    values_to = "value"
  )



p2<-ggplot(food_long,
       aes(x = Year,
           y = value,
           group = SharingUnitID)) +
  
  geom_line(alpha = 0.25,
            color = "grey50") +
  
  geom_point(alpha = 0.5,
             size = 1.2) +
  
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "line",
               linewidth = 1.5,
               color = "firebrick") +
  
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "point",
               size = 3,
               color = "firebrick") +
  
  facet_wrap(~indicator,
             scales = "free_y") +
  
  labs(
    x = NULL,
    y = "Value",
    title = "Household-level changes in food insecurity and fish network position"
  ) +
  
  theme_bw()

ggsave(filename = "Descriptive_SecondaryOutcomes.png",
       plot = p2,
       width = 10,
       height = 6,
       units = "in",
       dpi = 350,
       bg = "white")



### Proportion external
ext_long <- final_df %>%
  select(SharingUnitID, Year,external_lenders_prop ) %>%
  mutate(Year = factor(Year, levels = c("2018", "2026"))) %>%
  pivot_longer(
    cols = external_lenders_prop,
    names_to = "indicator",
    values_to = "value"
  )

delta_ext_lend<-ext_long%>%
select(
  SharingUnitID,
  Year,
  value
) %>%
  pivot_wider(
    names_from = Year,
    values_from = value
  ) %>%
  mutate(
    delta_ext_lend = `2026` - `2018`
  )

range(delta_ext_lend$delta_ext_lend)

p_ext<-ggplot(ext_long,
           aes(x = Year,
               y = value,
               group = SharingUnitID)) +
  
  geom_line(alpha = 0.25,
            color = "grey50") +
  
  geom_point(alpha = 0.5,
             size = 1.2) +
  
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "line",
               linewidth = 1.5,
               color = "firebrick") +
  
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "point",
               size = 3,
               color = "firebrick") +
  
  labs(
    x = NULL,
    y = "External proportion",
    title = "Household-level changes in external proportion\nof remittance in-degree"
  ) +
  
  theme_bw()

ggsave(filename = "PropExternal.png",
       plot = p_ext,
       width = 8,
       height = 6,
       units = "in",
       dpi = 350,
       bg = "white")


### cross sectional differences in more commercialized fishing and weekly expenditures, food security, pelagic focus.

GGally::ggpairs(final_df[,c("Com_index_commercial_share","Com_index_markets",
                                       "Com_index_capital","Com_index_gear","pelagic_focus_index",
                            "food_insecurity_score","MSL2.expenditure")])


#==============================================================================
# Livelihood outcomes
#============================================================================

# Source final data frame
source("Combine_2018_2026_ Data.R")

# Filter by sharing unit ID, year, livelihoods
livelihoods <- final_df %>%
  select(SharingUnitID, Year, livelihoods.activity, livelihoods.rank.activity_fishing, livelihoods.rank.activity_gleaning, livelihoods.rank.activity_marketing_mp, livelihoods.rank.activity_salaried_employment, livelihoods.rank.activity_informal_ea, livelihoods.rank.activity_tourism, livelihoods.rank.activity_farming, livelihoods.rank.activity_remittance, livelihoods.rank.activity_other) %>%
  mutate(Year = factor(Year, levels = c("2018", "2026")))

# Count total number of livelihoods for each HH
livelihoods <- livelihoods %>%
  mutate(
    n_livelihood_activities = sapply(
      str_split(livelihoods.activity, "\\s+"),
      \(x) if (all(is.na(x))) NA_integer_ else length(unique(x))))

# Add presence/absence cols for each marine livelihood
livelihoods <- livelihoods %>%
  mutate(fishing = grepl("fishing", livelihoods$livelihoods.activity)) %>%
  mutate(gleaning = grepl("gleaning", livelihoods$livelihoods.activity)) %>%
  mutate(marketing_mp = grepl("marketing_mp", livelihoods$livelihoods.activity)) %>%
  mutate(tourism = grepl("tourism", livelihoods$livelihoods.activity))

# Add col for if HH participates in marine livelihood 
livelihoods <- livelihoods %>%
  mutate(marine_livelihood = if_any(.cols = c(fishing, gleaning, marketing_mp, tourism))) 

# Format TRUE = 1 and FALSE = 0 for all marine livelihoods cols
livelihoods <- livelihoods %>%
  mutate(marine_livelihood = ifelse(marine_livelihood == TRUE, 1, 0)) %>%
  mutate(fishing = ifelse(fishing == TRUE, 1, 0)) %>%
  mutate(gleaning = ifelse(gleaning == TRUE, 1, 0)) %>%
  mutate(marketing_mp = ifelse(marketing_mp == TRUE, 1, 0)) %>%
  mutate(tourism = ifelse(tourism == TRUE, 1, 0))

# Primary livelihood is marine: livelihoods.rank.activity_fishing, livelihoods.rank.activity_gleaning, livelihoods.rank.activity_marketing_mp, livelihoods.rank.activity_tourism
livelihoods <- livelihoods %>% 
  mutate(primary_marine = ifelse(livelihoods.rank.activity_fishing == 1 | livelihoods.rank.activity_gleaning == 1 | livelihoods.rank.activity_marketing_mp ==1 | livelihoods.rank.activity_tourism == 1, 1, 0))

view(livelihoods)

# Subset by year
livelihoods2018 <- subset(livelihoods, Year == 2018)
livelihoods2026 <- subset(livelihoods, Year == 2026)

# HHs with marine livelihoods in each year
marine_livelihoods2018 <- livelihoods2018 %>%
  filter(marine_livelihood == 1) %>%
  nrow()
marine_livelihoods2018

marine_livelihoods2026 <- livelihoods2026 %>% 
  filter(marine_livelihood == 1) %>%
  nrow()
marine_livelihoods2026

# Percent with marine livelihoods in each year
percent_marine2018 <- marine_livelihoods2018/length(livelihoods2018$SharingUnitID)
percent_marine2018

percent_marine2026 <- marine_livelihoods2026/length(livelihoods2026$SharingUnitID)
percent_marine2026

# HHs with primarily marine livelihoods in each year
primary_marine2018 <- livelihoods2018 %>%
  filter(primary_marine == 1) %>%
  nrow()
primary_marine2018

primary_marine2026 <- livelihoods2026 %>%
  filter(primary_marine == 1) %>%
  nrow()
primary_marine2026

# Percent with primarily marine livelihoods in each year
percent_primarymarine2018 <- primary_marine2018/length(livelihoods2018$SharingUnitID)
percent_primarymarine2018

percent_primarymarine2026 <- primary_marine2026/length(livelihoods2026$SharingUnitID)
percent_primarymarine2026




