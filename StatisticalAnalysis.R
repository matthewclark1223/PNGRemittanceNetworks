library(brms)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(tidybayes)
library(ggdist)
library(ggeffects)
library(patchwork)


# we want to know whether changes in:
# 1. number of internal borrowing ties
# 2. number of external borrowing ties and, 
# 3. network position 
# affect the commercialization of fishing practices. 

# Do these changes also have any bearing on household food security?

# Do these changes affect the degree of fish sharing? 

#restrict to fishing households



source("Combine_2018_2026_ Data.R")


#Restrict to just households that fish in both years
final_df<-final_df[grep("fishing",final_df$livelihoods.activity),]

#restrict to just sharing units that are present in both years
panel_df <- final_df %>%
  # keep only households observed in BOTH years
  group_by(SharingUnitID) %>%
  filter(n_distinct(Year) == 2) %>%
  ungroup()

#get just the columns we want to use for the first analysis
panel_df<-panel_df%>%select(
  #outcomes
  #pelagic_focus_index,
  Com_index_capital,Com_index_gear,
  Com_index_markets,Com_index_commercial_share,
  #predictors of interest
  external_lenders_n,
  financial_betweenness,
  internal_lenders_n,
  #controls
  n_adults,n_children,leader,ageHHH,
  #fixed effects (for panel)
  Year,SharingUnitID)%>%
  mutate(
    Year = factor(Year),
    SharingUnitID = factor(SharingUnitID)
  )




# outcomes to model
  #Com_index_capital
  #Com_index_gear
  #Com_index_markets
  #Com_index_commercial_share


fit_capital<-brm(
  bf(Com_index_capital~
       # commercialization dimensions
       scale(external_lenders_n) +
       scale(financial_betweenness) +
       scale(internal_lenders_n) +
       # household controls
       leader+
       scale(n_adults) +
       scale(ageHHH)+
       scale(n_children) +
       # fixed effects
       Year +
       SharingUnitID),data = panel_df,family = gaussian(),
  prior = c(prior(cauchy(0, 1), class = "b"),
   prior(cauchy(0, 2.5), class = "Intercept")),
  backend = "cmdstanr",chains = 4,cores = 4,iter = 10000,
  warmup = 1500,control = list(
    adapt_delta = 0.99,
    max_treedepth = 15), seed = 123)

mcmc_plot(fit_capital, type = "intervals",variable = c(  "b_scaleinternal_lenders_n",
                                                         "b_scalefinancial_betweenness",
                                                         "b_scaleexternal_lenders_n"))


fit_gear<-
  brm(
    bf(Com_index_gear ~
         # commercialization dimensions
         scale(external_lenders_n) +
         scale(financial_betweenness) +
         scale(internal_lenders_n) +
         # household controls
         leader+
         scale(n_adults) +
         scale(ageHHH)+
         scale(n_children) +
         # fixed effects
         Year +
         SharingUnitID),data = panel_df,family = gaussian(),
    prior = c(prior(cauchy(0, 1), class = "b"),
              prior(cauchy(0, 2.5), class = "Intercept")),
    backend = "cmdstanr",chains = 4,cores = 4,iter = 10000,
    warmup = 1500,control = list(
      adapt_delta = 0.99,
      max_treedepth = 15), seed = 123)

mcmc_plot(fit_gear, type = "intervals",variable = c(  "b_scaleinternal_lenders_n",
                                                         "b_scalefinancial_betweenness",
                                                         "b_scaleexternal_lenders_n"))

fit_markets<- brm(
  bf(Com_index_markets ~
       # commercialization dimensions
       scale(external_lenders_n) +
       scale(financial_betweenness) +
       scale(internal_lenders_n) +
       # household controls
       leader+
       scale(n_adults) +
       scale(ageHHH)+
       scale(n_children) +
       # fixed effects
       Year +
       SharingUnitID),data = panel_df,family = gaussian(),
  prior = c(prior(cauchy(0, 1), class = "b"),
            prior(cauchy(0, 2.5), class = "Intercept")),
  backend = "cmdstanr",chains = 4,cores = 4,iter = 10000,
  warmup = 1500,control = list(
    adapt_delta = 0.99,
    max_treedepth = 15), seed = 123)

mcmc_plot(fit_markets, type = "intervals",variable = c(  "b_scaleinternal_lenders_n",
                                                      "b_scalefinancial_betweenness",
                                                      "b_scaleexternal_lenders_n"))

fit_comercial_share<- brm(
  bf(Com_index_commercial_share ~
       # commercialization dimensions
       scale(external_lenders_n) +
       scale(financial_betweenness) +
       scale(internal_lenders_n) +
       # household controls
       leader+
       scale(n_adults) +
       scale(ageHHH)+
       scale(n_children) +
       # fixed effects
       Year +
       SharingUnitID),data = panel_df,family = zero_one_inflated_beta(),
  prior = c(prior(cauchy(0, 1), class = "b"),
            prior(cauchy(0, 2.5), class = "Intercept")),
  backend = "cmdstanr",chains = 4,cores = 4,iter = 10000,
  warmup = 1500,control = list(
    adapt_delta = 0.99,
    max_treedepth = 15), seed = 123)

mcmc_plot(fit_comercial_share, type = "intervals",variable = c(  "b_scaleinternal_lenders_n",
                                                         "b_scalefinancial_betweenness",
                                                         "b_scaleexternal_lenders_n"))




fits<-list(fit_capital,fit_markets,fit_gear,fit_comercial_share)
names(fits) <- c("capital","markets","gear","commercial_share")

# extract coefficients of interest
# extract posterior draws
coef_df <- map2_dfr(
  fits,
  names(fits),
  ~ tidybayes::spread_draws(
    .x,
    b_scaleexternal_lenders_n,
    b_scaleinternal_lenders_n,
    b_scalefinancial_betweenness
  ) %>%
    
    pivot_longer(
      cols = starts_with("b_"),
      names_to = "term",
      values_to = "estimate"
    ) %>%
    
    mutate(outcome = .y)
)


p1<-coef_df%>%mutate(term=factor(term,levels = c("b_scaleinternal_lenders_n","b_scalefinancial_betweenness","b_scaleexternal_lenders_n")))%>%
ggplot(
  .,
  aes(
    x = estimate,
    y = term,
    color = outcome
  )
) +
  
  stat_pointinterval(
    
    aes(size = after_stat(.width)),
    
    # nested HDIs
    .width = c(.50, .90),
    
    # highest density intervals
    point_interval = mean_qi,
    
    # dodge outcomes
    position = position_dodge(width = 0.3)
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    alpha = 0.7
  ) +
  
  # make narrower intervals thicker
  scale_size_continuous(
    range = c(15, 4),
    guide = "none"
  ) +
  scale_x_continuous(breaks=c(-0.25,0,0.25))+
  scale_y_discrete(labels=c("Internal\nties","Financial network\nbetweenness","External\nties"))+
  scale_color_discrete(palette=c("#CC79A7","#E69F00","#56B4E9","#009E73"))+
  labs(
    x = "Posterior coefficient estimate",
    y = "Predictor",
    color = "Outcome",
  ) +
  ggthemes::theme_clean() +
  annotate(geom="text",label="Market commercialization",x=-0.25,y=2.25,fontface="bold",size=5,color="#009E73")+
  annotate(geom="text",label="Asset commercialization",x=0.22,y=1.75,fontface="bold",size=5,color="#CC79A7")+
  annotate(geom="text",label="Commercial share",x=0.28,y=2.1,fontface="bold",size=5,color="#E69F00")+
  annotate(geom="text",label="Gear commercialization",x=-0.20,y=1.9,fontface="bold",size=5,color="#56B4E9")+

  theme(
    legend.position = "none",
    axis.text = element_text(color="black",
                             size=16),
    axis.title=element_text(color="black",size=18))+ theme(plot.background = element_rect(colour = "black", fill=NA, linewidth = 1))
    
ggsave(filename = "./Ahus/TimeseriesAnalysis/Figures/CoefPlot.png",
             plot = p1,
             width = 8,
             height = 6,
             units = "in",
             dpi = 350,
             bg = "white")

#conditional effects (figure 3B in the paper)


# market commercialization ~ external lenders
ce_market1 <- conditional_effects(
  fits[["markets"]],
  effects = "external_lenders_n",
  re_formula = NA,
  prob=0.9
)$external_lenders_n

# commercial share ~ financial betweenness
ce_comshare <- conditional_effects(
  fits[["commercial_share"]],
  effects = "financial_betweenness",
  re_formula = NA,
  prob=0.9
)$financial_betweenness

# gear commercialization ~ external lenders
ce_gear <- conditional_effects(
  fits[["gear"]],
  effects = "external_lenders_n",
  re_formula = NA,
  prob=0.9
)$external_lenders_n

# asset commercialization ~ financial betweenness
ce_capital <- conditional_effects(
  fits[["capital"]],
  effects = "financial_betweenness",
  re_formula = NA,
  prob=0.9
)$financial_betweenness



# plot


p1<-ggplot(
  ce_market1, aes(x = effect1__,y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),alpha = 0.4,fill = "#009E73") +
  geom_line(linewidth = 1.4,color="#009E73") +
  labs(
    x = " ",
    y = "Market\ncommercialization") +
  ggthemes::theme_clean() +
  theme(
    axis.text = element_text(
      size = 16,
      color = "black"),
    axis.title = element_text(
      size = 18,
      color = "black"))+ theme(plot.background = element_rect(colour = "black", fill=NA, linewidth = 1))

p2<-ggplot(
  ce_comshare, aes(x = effect1__,y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),alpha = 0.4,fill = "#E69F00") +
  geom_line(linewidth = 1.4,color="#E69F00") +
  labs(
    x = " ",
    y = "Commercial\nshare") +
  scale_x_continuous(breaks=c(0,150,350))+
  ggthemes::theme_clean() +
  theme(
    axis.text = element_text(
      size = 16,
      color = "black"),
    axis.title = element_text(
      size = 18,
      color = "black"))+ theme(plot.background = element_rect(colour = "black", fill=NA, linewidth = 1))

p3<-ggplot(
  ce_capital, aes(x = effect1__,y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),alpha = 0.4,fill = "#CC79A7") +
  geom_line(linewidth = 1.4,color="#CC79A7") +
  labs(
    x = "Financial network\nbetweenness",
    y = "Asset\ncommercialization") +
  ggthemes::theme_clean() +
  scale_x_continuous(breaks=c(0,150,350))+
  theme(
    axis.text = element_text(
      size = 16,
      color = "black"),
    axis.title = element_text(
      size = 18,
      color = "black"))+ theme(plot.background = element_rect(colour = "black", fill=NA, linewidth = 1))

p4<-ggplot(
  ce_gear, aes(x = effect1__,y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),alpha = 0.4,fill = "#56B4E9") +
  geom_line(linewidth = 1.4,color="#56B4E9") +
  labs(
    x = "External ties\n(one week's wage)",
    y = "Gear\ncommercialization") +
  ggthemes::theme_clean() +
  theme(
    axis.text = element_text(
      size = 16,
      color = "black"),
    axis.title = element_text(
      size = 18,
      color = "black"))+ theme(plot.background = element_rect(colour = "black", fill=NA, linewidth = 1))


pc <- ((p2 | p1) / (p3 | p4)) &
  plot_layout(guides = "collect") &
  theme(
    plot.background = element_rect(colour = "black", fill = NA, linewidth = 1),
    #plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0, "pt")
  )
  
#plot_annotation(theme = theme(plot.background = element_rect(color  = 'black', linewidth  = 1, fill =NULL)))

ggsave(filename = "./Ahus/TimeseriesAnalysis/Figures/CondEff.png",
       plot = pc,
       width = 8,
       height = 6,
       units = "in",
       dpi = 350,
       bg = "white",
       limitsize = FALSE)

############# secondary effects of commercialization on food insecurity and fish sharing

source("./RCode/Combine_2018_2026_ Data.R")

#Restrict to just households that fish in both years
final_df<-final_df[grep("fishing",final_df$livelihoods.activity),]

#restrict to just sharing units that are present in both years
panel_df <- final_df %>%
  # keep only households observed in BOTH years
  group_by(SharingUnitID) %>%
  filter(n_distinct(Year) == 2) %>%
  ungroup()

#get just the columns we want to use for the first analysis
panel_df<-panel_df%>%select(
  #outcomes
  food_insecurity_score,
  fish_outdegree,

  #predictors of interest
  Com_index_capital,
  Com_index_gear,
  Com_index_markets,
  Com_index_commercial_share,
  fish_indegree,
  external_lenders_n,
  financial_betweenness,
  internal_lenders_n,
  #controls
  n_adults,n_children,leader,ageHHH,
  #fixed effects (for panel)
  Year,SharingUnitID)%>%
  mutate(
    Year = factor(Year),
    SharingUnitID = factor(SharingUnitID)
  )

#secondary effects on food insecurity (Figure 3C in the paper)

fit_food <- brm(
  bf(food_insecurity_score ~
      #network effects
       scale(external_lenders_n) +
       scale(financial_betweenness) +
       scale(internal_lenders_n) +
       # commercialization dimensions
      scale(Com_index_capital) +
      scale(Com_index_gear) +
      scale(Com_index_markets)+
      scale(Com_index_commercial_share)+
      # household controls
      leader+
      scale(n_adults) +
      scale(n_children) +
      scale(ageHHH)+
      # fixed effects
      Year +
      SharingUnitID),
  data = panel_df,family = gaussian(),
   prior = c(
    
    # regularizing priors for coefficients
    prior(cauchy(0, 1), class = "b"),
    
    # intercept prior
    prior(cauchy(0, 2.5), class = "Intercept")),
  
  backend = "cmdstanr",
  chains = 4,
  cores = 4,
  iter = 10000,
  warmup = 1500,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 15
  ),
  seed = 123
)

mcmc_plot(fit_food, type = "intervals",variable = c(  "b_scaleCom_index_capital",
                                                      "b_scaleCom_index_gear",
                                                      "b_scaleexternal_lenders_n",
                                                      "b_scalefinancial_betweenness",
                                                      "b_scaleinternal_lenders_n",
                                                      "b_scaleCom_index_capital",
                                                      "b_scaleCom_index_gear",
                                                      "b_scaleCom_index_markets",
                                                      "b_scaleCom_index_commercial_share"))
coef_df <- fit_food %>%
  gather_draws(
    b_scaleinternal_lenders_n,
    b_scalefinancial_betweenness,
    b_scaleexternal_lenders_n,
    b_scaleCom_index_capital,
    b_scaleCom_index_gear,
    b_scaleCom_index_markets,
    b_scaleCom_index_commercial_share
  ) %>%
  rename(term = .variable, estimate = .value)

coef_df<-coef_df%>%mutate(term=factor(term,levels=c("b_scaleCom_index_markets","b_scaleCom_index_gear",
                                      "b_scaleCom_index_commercial_share","b_scaleCom_index_capital",
                                      "b_scaleexternal_lenders_n","b_scalefinancial_betweenness",
                                      "b_scaleinternal_lenders_n")))
                                        
                                        
                                     

  p_food<-ggplot(coef_df,aes(x = estimate,y = term)) +
  stat_pointinterval(
    aes(size = after_stat(.width),color=term),
    .width = c(.50, .90),
    point_interval = mean_qi
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    alpha = 0.7
  ) +
  scale_size_continuous(
    range = c(15, 4),
    guide = "none"
  ) +
    scale_color_discrete(guide = "none",palette=c("#009E73","#56B4E9","#E69F00","#CC79A7","#636363","#636363","#636363","#636363"))+
  scale_y_discrete(limits=rev,
    labels = c(
      "Internal ties",
      "Financial network\nbetweenness",
      "External ties",
      "Asset commercialization",
      "Commercial share",
      "Gear commercialization",
      "Market commercialization")) +
  labs(
    x = "Posterior coefficient estimate",
    y = "Predictor",title = "Secondary relation with food insecurity") +
  ggthemes::theme_clean() +
  theme(axis.text = element_text(color = "black",size = 16),
    axis.title = element_text(
      color = "black",
      size = 18))+ theme(plot.background = element_rect(colour = "black", fill=NA, linewidth = 1))

  ggsave(filename = "./Ahus/TimeseriesAnalysis/Figures/Pars_food.png",
         plot = p_food,
         width = 8,
         height = 6,
         units = "in",
         dpi = 350,
         bg = "white")
  
# Secondary effects on fish sharing (Figure 3D in the paper)
fit_fish <- brm(
  bf(fish_outdegree ~
       #network effects
       scale(external_lenders_n) +
       scale(financial_betweenness) +
       scale(internal_lenders_n) +
       # commercialization dimensions
       scale(Com_index_capital) +
       scale(Com_index_gear) +
       scale(Com_index_markets)+
       scale(Com_index_commercial_share)+
       # household controls
       leader+
       scale(n_adults) +
       scale(n_children) +
       scale(ageHHH)+
       # fixed effects
       Year +
       SharingUnitID),
  data = panel_df,family = poisson(),
  prior = c(
    
    # regularizing priors for coefficients
    prior(cauchy(0, 1), class = "b"),
    
    # intercept prior
    prior(cauchy(0, 2.5), class = "Intercept")),
  
  backend = "cmdstanr",
  chains = 4,
  cores = 4,
  iter = 10000,
  warmup = 1500,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 15
  ),
  seed = 123
)

mcmc_plot(fit_fish, type = "intervals",variable = c(  "b_scaleCom_index_capital",
                                                      "b_scaleCom_index_gear",
                                                      "b_scaleexternal_lenders_n",
                                                      "b_scalefinancial_betweenness",
                                                      "b_scaleinternal_lenders_n",
                                                      "b_scaleCom_index_capital",
                                                      "b_scaleCom_index_gear",
                                                      "b_scaleCom_index_markets",
                                                      "b_scaleCom_index_commercial_share"))


coef_df <- fit_fish %>%
  gather_draws(
    b_scaleinternal_lenders_n,
    b_scalefinancial_betweenness,
    b_scaleexternal_lenders_n,
    b_scaleCom_index_capital,
    b_scaleCom_index_gear,
    b_scaleCom_index_markets,
    b_scaleCom_index_commercial_share
  ) %>%
  rename(term = .variable, estimate = .value)

coef_df<-coef_df%>%mutate(term=factor(term,levels=c("b_scaleCom_index_markets","b_scaleCom_index_gear",
                                                    "b_scaleCom_index_commercial_share","b_scaleCom_index_capital",
                                                    "b_scaleexternal_lenders_n","b_scalefinancial_betweenness",
                                                    "b_scaleinternal_lenders_n")))




p_fish<-ggplot(coef_df,aes(x = estimate,y = term)) +
  stat_pointinterval(
    aes(size = after_stat(.width),color=term),
    .width = c(.50, .90),
    point_interval = mean_hdi
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    alpha = 0.7
  ) +
  scale_size_continuous(
    range = c(15, 4),
    guide = "none"
  ) +
  scale_color_discrete(guide = "none",palette=c("#009E73","#56B4E9","#E69F00","#CC79A7","#636363","#636363","#636363","#636363"))+
  scale_y_discrete(limits=rev,
                   labels = c(
                     "Internal ties",
                     "Financial network\nbetweenness",
                     "External ties",
                     "Asset commercialization",
                     "Commercial share",
                     "Gear commercialization",
                     "Market commercialization")) +
  labs(
    x = "Posterior coefficient estimate",
    y = "Predictor",title = "Secondary relation with fish sharing (out-degree)") +
  ggthemes::theme_clean() +
  theme(axis.text = element_text(color = "black",size = 16),
        axis.title = element_text(
          color = "black",
          size = 18))+ theme(plot.background = element_rect(colour = "black", fill=NA, linewidth = 1))

ggsave(filename = "./Ahus/TimeseriesAnalysis/Figures/pars_fish_sharing.png",
       plot = p_fish,
       width = 8,
       height = 6,
       units = "in",
       dpi = 350,
       bg = "white")





