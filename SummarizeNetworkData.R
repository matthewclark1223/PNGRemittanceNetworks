library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(patchwork)

Net2018<-readRDS("data_clean/Net2018_clean.rds") 
Net2026<-readRDS("data_clean/Net2026_clean.rds") 
Net2018<-Net2018%>% rename("receivefish"="receive")%>% rename("givefish"="give")
Net2018<-Net2018%>% rename("borrow200"="borrow")%>% rename("lend200"="lend") #this isn't really 200K, but making consistent name w 2026

#First we need to swap out the Individual IDs in the network data for Sharing Unit IDs 
Ind2018<-readRDS("data_clean/Ind2018_clean.rds") 
Ind2026<-readRDS("data_clean/Ind2026_clean.rds")

Net2018<-Net2018 %>% 
  left_join(Ind2018 %>% select(IndivID, SharingUnitID), #get ego sharing unit ID 
                               join_by("EgoID" == "IndivID"))%>% 
  rename("EgoSharingUnitID"="SharingUnitID")%>% 
  left_join(Ind2018 %>% select(IndivID, SharingUnitID), join_by("AlterID" == "IndivID"))%>% 
  rename("AlterSharingUnitID"="SharingUnitID")%>% 
  left_join(Ind2018 %>% select(IndivID, AhusOrAlter18), join_by("AlterID" == "IndivID"))%>%
 rename("AlterResidence"="AhusOrAlter18")%>% 
  #Now, we don;t want NAs in the Sharing unit data. #Let's fill that with EA + the individual ID 
  mutate( AlterSharingUnitID = ifelse( is.na(AlterSharingUnitID), paste0("EA_", AlterID), AlterSharingUnitID ) ) 


Net2026<-Net2026 %>% left_join(Ind2026 %>% select(IndivID, SharingUnitID), join_by("EgoID" == "IndivID"))%>% 
  rename("EgoSharingUnitID"="SharingUnitID")%>% left_join(Ind2026 %>% select(IndivID, SharingUnitID), join_by("AlterID" == "IndivID"))%>%
  rename("AlterSharingUnitID"="SharingUnitID")%>% left_join(Ind2026 %>% select(IndivID, AhusOrAlter), join_by("AlterID" == "IndivID"))%>%
  rename("AlterResidence"="AhusOrAlter")%>% 
  #Now, we don;t want NAs in the Sharing unit data. #Let's fill that with EA + the individual ID 
  mutate( AlterSharingUnitID = ifelse( is.na(AlterSharingUnitID), paste0("EA_", AlterID), AlterSharingUnitID ) )

###########################################################
# Helper function to build network + centrality summaries
###########################################################

build_network <- function(df,
                          incoming_var,
                          outgoing_var,
                          year,
                          flow_name,
                          external = TRUE){
  
  # Incoming ties:
  # ego says they RECEIVE from alter
  incoming_edges <- df %>%
    filter(.data[[incoming_var]] == 1) %>%
    transmute(
      from = AlterSharingUnitID,
      to   = EgoSharingUnitID
    )
  
  # Outgoing ties:
  # ego says they GIVE to alter
  outgoing_edges <- df %>%
    filter(.data[[outgoing_var]] == 1) %>%
    transmute(
      from = EgoSharingUnitID,
      to   = AlterSharingUnitID
    )
  
  edges <- bind_rows(incoming_edges, outgoing_edges) %>%
    distinct()
  
  g <- graph_from_data_frame(
    edges,
    directed = TRUE
  )
  
  # Centrality metrics
  V(g)$indegree <- degree(g, mode = "in")
  V(g)$outdegree <- degree(g, mode = "out")
  V(g)$betweenness <- betweenness(
    g,
    directed = TRUE,
    weights = NA
  )
  
  centrality_df <- data.frame(
    SharingUnitID = V(g)$name,
    indegree      = V(g)$indegree,
    outdegree     = V(g)$outdegree,
    betweenness   = V(g)$betweenness
  )
  
  # External lenders/supporters only for financial flows
  if(external){
    
    EA_df <- edges %>%
      filter(grepl("^EA_", from)) %>%
      count(to, name = "num_external") %>%
      rename(SharingUnitID = to)
    
    centrality_df <- centrality_df %>%
      left_join(EA_df, by = "SharingUnitID") %>%
      mutate(
        num_external = ifelse(
          is.na(num_external),
          0,
          num_external
        ),
        prop_external =
          num_external / indegree
      )
    
  }
  
  nodes <- data.frame(
    name = V(g)$name
  ) %>%
    left_join(
      centrality_df,
      by = c("name" = "SharingUnitID")
    )
  
  # Only distinguish EA nodes for financial networks
  if(external){
    
    nodes <- nodes %>%
      mutate(
        external = grepl("^EA_", name)
      )
    
  } else {
    
    nodes <- nodes %>%
      mutate(
        external = FALSE
      )
    
  }
  
  V(g)$external <- nodes$external
  
  p <- ggraph(g, layout = "nicely") +
    geom_edge_link(
      alpha = 0.75,color="#080808",edge_width=1,
      arrow = arrow(length = unit(1.5, "mm")),
      end_cap = circle(4, "mm")) +
    geom_node_point(aes(
      size = indegree,
      fill = betweenness,
      shape = external),color="black",stroke = 0.6) +
    scale_shape_manual(values = c(21,24),
                       name="Location",labels=c("Internal","External"),
                       guide = guide_legend(
                         order = 3,
                         override.aes = list(size = 8) )) +
    labs(size = "In-degree",
      fill = "Betweenness") +
    theme_void()
  
  centrality_df <- centrality_df %>%
    mutate(
      Year = year,
      FlowType = flow_name
    )
  

  
  return(list(
    graph = g,
    centrality = centrality_df,
    plot = p
  ))
}

###########################################################
# Build all four networks
###########################################################

# Financial flows
fin2018 <- build_network(
  Net2018,
  incoming_var = "borrow200",
  outgoing_var = "lend200",
  year = "2018",
  flow_name = "Financial flows",
  external = TRUE
)

fin2026 <- build_network(
  Net2026,
  incoming_var = "borrow200",
  outgoing_var = "lend200",
  year = "2026",
  flow_name = "Financial flows",
  external = TRUE
)

# Fish flows
fish2018 <- build_network(
  Net2018,
  incoming_var = "receivefish",
  outgoing_var = "givefish",
  year = "2018",
  flow_name = "Fish flows",
  external = FALSE
)

fish2026 <- build_network(
  Net2026,
  incoming_var = "receivefish",
  outgoing_var = "givefish",
  year = "2026",
  flow_name = "Fish flows",
  external = FALSE
)

all_centrality <- bind_rows(
  #fish2018$centrality,
  #fish2026$centrality,
  fin2018$centrality,
  fin2026$centrality
)

###########################################################
# Combine into one 4-panel figure
###########################################################

indegree_limits <- range(all_centrality$indegree, na.rm = TRUE)
betweenness_limits <- range(all_centrality$betweenness, na.rm = TRUE)

common_scales <- list(
  scale_size_continuous(
    limits = indegree_limits,
    range = c(4, 12),      # larger minimum node size
    name = "In-degree",
    breaks = c(
      indegree_limits[1],
      indegree_limits[2]
    )),
  scale_fill_viridis_c(
    limits = betweenness_limits,
    name = "Betweenness",option="mako",begin=0.0,end=0.8,alpha=0.6,direction=-1))



fin2018$plot2  <- fin2018$plot  + common_scales+ 
  labs(title = paste("Remittance flows", 2018))+
  theme(plot.title = element_text(hjust = 0.5,size=16))
  
fin2026$plot2  <- fin2026$plot  + common_scales+ labs(title = 2026)+
  theme(plot.title = element_text(hjust = 0.5,size=16))
#(fish2018$plot | fish2026$plot)/
Remitplot<- (fin2018$plot2 | fin2026$plot2)+
  plot_layout(guides = "collect")&
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.just = "center",
    
    legend.spacing.x = unit(1, "cm"),
    legend.margin = margin(5, 5, 5, 5),
    
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11))&
  plot_annotation(theme = theme(plot.background = element_rect(color  = 'black', linewidth  = 1, fill =NULL)))
  
  

# ggsave(filename = "./Ahus/TimeseriesAnalysis/Figures/FinancialNetworks.png",
#          plot = Remitplot,
#          width = 10,
#          height = 6,
#          units = "in",
#          dpi = 350,
#          bg = "white")



all_centrality <- bind_rows(
  fish2018$centrality,
  fish2026$centrality,
  fin2018$centrality,
  fin2026$centrality
)


network_summary <- all_centrality %>%
  
  # drop external nodes (only households)
  filter(!grepl("^EA_", SharingUnitID)) %>%
  
  select(
    SharingUnitID,
    Year,
    FlowType,
    indegree,
    outdegree,
    betweenness,
    num_external,
    prop_external
  ) %>%
  
  pivot_wider(
    names_from = FlowType,
    values_from = c(
      indegree,
      outdegree,
      betweenness,
      num_external,
      prop_external
    )
  ) %>%
  
  rename(
    financial_indegree   = `indegree_Financial flows`,
    financial_outdegree  = `outdegree_Financial flows`,
    financial_betweenness = `betweenness_Financial flows`,
    external_lenders_n   = `num_external_Financial flows`,
    external_lenders_prop = `prop_external_Financial flows`,
    
    fish_indegree        = `indegree_Fish flows`,
    fish_outdegree       = `outdegree_Fish flows`,
    fish_betweenness     = `betweenness_Fish flows`
  ) %>%
  
  select(
    SharingUnitID,
    Year,
    financial_indegree,
    financial_outdegree,
    financial_betweenness,
    external_lenders_n,
    external_lenders_prop,
    fish_indegree,
    fish_outdegree,
    fish_betweenness
  )%>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))%>%
  mutate(internal_lenders_n=financial_indegree -external_lenders_n )

