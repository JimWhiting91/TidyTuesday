# Data viz for voting clusters and networks within Eurovision history
# Author - Dr James R Whiting

# Fetch all our packages 
lib <- c("data.table","ggplot2","tidyr","dplyr","tidygraph","ggraph","cowplot","showtext","rnaturalearth")
sapply(lib,library,character.only=T)

# Fetch the data
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')
vote_dd <- data.table(readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv'))

# Get Rubik - An official Eurovision Font
font_add_google("Rubik", family = "rubik_custom")
font_add("EurofanEurovision","220517_eurovision/data/538EurofanEurovision-veLA.ttf")
showtext_auto()

# We will only look at jury voting
vote_jury_dd <- vote_dd[jury_or_televoting == "J",]

# Divide the data up into year groups
vote_jury_dd <- vote_jury_dd %>%
  mutate(decade = case_when(
    between(year, 1970, 1979) ~ "1970s",
    between(year, 1980, 1989) ~ "1980s",
    between(year, 1990, 1999) ~ "1990s",
    between(year, 2000, 2010) ~ "2000s",
    between(year, 2010, 2020) ~ "2010s")
  )
vote_jury_dd <- vote_jury_dd[!is.na(decade),]

# Also only look at top 3 vote amounts
vote_jury_dd <- vote_jury_dd[points %in% c(12,10,8),]

# And only look at the grand final
vote_jury_dd <- vote_jury_dd[semi_final == "f",]

# Now loop over each decade and perform the network analysis...
network_list <- lapply(unique(vote_jury_dd$decade),function(decade_tmp){
  
  # Build node and edge lists for all decades...
  vote_jury_dd_tmp <- vote_jury_dd[decade == decade_tmp,]
  country_nodes <- data.table(label = sort(unique(c(vote_jury_dd_tmp$from_country,vote_jury_dd_tmp$to_country))))
  country_nodes$id <- 1:nrow(country_nodes)
  country_nodes <- country_nodes[,.(id,label)]
  
  # Build edge lists for each decade
  county_edges <- vote_jury_dd_tmp[,.(point_sum=sum(points)),by=.(from_country,to_country,decade)]
  
  # Merge with the IDs
  edges <- county_edges %>% 
    left_join(country_nodes, by = c("from_country" = "label")) %>% 
    rename(from = id)
  edges <- edges %>% 
    left_join(country_nodes, by = c("to_country" = "label")) %>% 
    rename(to = id) %>%
    rename(weight = point_sum)
  edges <- edges[,.(from,to,weight)]
  
  # Remove self-votes
  edges <- edges[from != to,]
  
  # And remove cases with 0
  edges <- edges[weight > 0,]
  
  # Build tidy graph...
  votes_tidy <- tbl_graph(nodes = country_nodes, edges = edges, directed = TRUE)
 
  set.seed(1000)
  network_graph <- votes_tidy %>%
    ggraph(layout = "fr") +
    geom_edge_arc(colour= "gold",
                  lineend = "round",
                  strength = .1,
                  aes(edge_width = weight,
                      alpha = weight)) +
    geom_node_point(colour="gold")+
    scale_edge_width(range = c(0, 2.5)) +
    scale_edge_alpha(range = c(0, .8)) +
    theme_graph(background = "gray20") +
    theme(legend.position = "top",
          panel.background = element_rect(fill = "gray20",
                                          colour = "gray20"),
          plot.title = element_text(colour="white",
                                    size=24,
                                    family = "rubik_custom",
                                    face = "bold")) +
    guides(edge_width = FALSE,
           edge_alpha = FALSE)+
    ggtitle(decade_tmp)
  
  if(decade_tmp == "2010s"){
    network_graph <- network_graph + 
      geom_node_text(aes(label = label),
                     family="rubik_custom",
                     repel = TRUE,
                     point.padding = unit(0.2, "lines"),
                     colour="gray85")
  }
  
  return(list(network_graph,votes_tidy))
})

# Make a title for total plot
network_title_theme <- ggdraw() +
  draw_label("Voting networks by decade\n12/10/8 points", 
             fontfamily = "rubik_custom", 
             x = 0.05, hjust = 0,colour="white",size=28,fontface = "bold")+
  theme(panel.background = element_rect(fill = "gray20",
                                        colour = "gray20"))


# How has network clustering changed over time? ---------------------------
# We can also plot the transitivity by year...
annual_network_list <- lapply(unique(vote_jury_dd$year),function(year_tmp){
  
  # Build node and edge lists for all decades...
  vote_jury_dd_tmp <- vote_jury_dd[year == year_tmp,]
  country_nodes <- data.table(label = sort(unique(c(vote_jury_dd_tmp$from_country,vote_jury_dd_tmp$to_country))))
  country_nodes$id <- 1:nrow(country_nodes)
  country_nodes <- country_nodes[,.(id,label)]
  
  # Build edge lists for each decade
  county_edges <- vote_jury_dd_tmp[,.(point_sum=sum(points)),by=.(from_country,to_country,decade)]
  
  # Merge with the IDs
  edges <- county_edges %>% 
    left_join(country_nodes, by = c("from_country" = "label")) %>% 
    rename(from = id)
  edges <- edges %>% 
    left_join(country_nodes, by = c("to_country" = "label")) %>% 
    rename(to = id) %>%
    rename(weight = point_sum)
  edges <- edges[,.(from,to,weight)]
  
  # Remove self-votes
  edges <- edges[from != to,]
  
  # And remove cases with 0
  edges <- edges[weight > 0,]
  
  # Build tidy graph...
  votes_tidy <- tbl_graph(nodes = country_nodes, edges = edges, directed = TRUE)
  
  # Also find node closeness and reformat to countries...
  node_res <- data.frame(country = country_nodes$label,
                         local_cluster = transitivity(votes_tidy, type="local"))
  
  return(list(votes_tidy,node_res))
})

# Plot transitivty through time
cluster_plot <- data.frame(year = unique(vote_jury_dd$year),
                           cluster = sapply(lapply(annual_network_list,'[[',1),transitivity))

cluster_through_time <- ggplot(cluster_plot,aes(year,cluster))+
  geom_line(colour="deeppink1",size=2)+
  labs(y="Voting Network Clustering",x="Year")+
  ggtitle("Political voting has\nDECREASED since 2000")+ 
  theme_minimal()+
  theme(legend.position = "top",
        panel.background = element_rect(fill = "gray20",
                                        colour = "gray20"),
        plot.title = element_text(colour="white",
                                  size=32,
                                  family="rubik_custom",
                                  face="bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(colour = "gray30"),
        plot.background = element_rect(fill = "gray20"),
        axis.title = element_text(size = 20,colour="white",
                                  family="rubik_custom"),
        axis.text = element_text(size = 16,colour="white",
                                 family = "rubik_custom"))


# Which countries give their favourite douze points the most? -------------
# For each country, find the number of countries it has given top points to
vote_jury_dd_douze <- vote_jury_dd[points == 12,]
voting_biases <- vote_jury_dd_douze[,.(receiving_countries=length(unique(.SD$to_country)),
                                       years_given=length(unique(.SD$year)),
                                       fave_country=max(table(.SD$to_country))),by=from_country]


voting_biases$fave_country_prop <- voting_biases$fave_country/voting_biases$years_given
voting_biases <- voting_biases[years_given > 10,]
voting_biases[order(-fave_country_prop),]

# Plot on a map of Europe
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe_map <- world[which(world$continent == "Europe"),]

# Fix some inconsistencies
voting_biases$from_country <- gsub("Bosnia & Herzegovina","Bosnia and Herzegovina",voting_biases$from_country)
voting_biases$from_country <- gsub("F.Y.R. Macedonia","Macedonia",voting_biases$from_country)
voting_biases$from_country <- gsub("The Netherlands","Netherlands",voting_biases$from_country)
voting_biases$from_country <- gsub("Serbia","Republic of Serbia",voting_biases$from_country)

# Make a data.frame to add features of voting bias...
voting_biases$geounit <- voting_biases$from_country
Europe_map$voting_bias <- NA
for(country in Europe_map$geounit){
  if(country %in% voting_biases$from_country){
    Europe_map[Europe_map$geounit == country,"voting_bias"] <- voting_biases[from_country == country,fave_country_prop]
  }
}


bias_map <- ggplot(Europe_map) +
  geom_sf(aes(fill = voting_bias)) +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE) +
  scale_fill_viridis(option = "C",direction = -1) +
  theme_void()+
  theme(panel.background = element_rect(fill = "gray20",
                                        colour = "gray20"),
        legend.position = "bottom",
        legend.title = element_text(family="rubik_custom",
                                    size = 20,
                                    colour="white"),
        legend.text = element_text(family="rubik_custom",
                                   angle=45,hjust=1,size = 12,
                                   colour="white"),
        plot.title = element_text(colour="white",
                                  size=24,
                                  family="rubik_custom",
                                  face="bold"),
        plot.background = element_rect(fill = "gray20"))+
  guides(fill = guide_colourbar(title.position="top"))+
  labs(fill = "Proportion of competitions")+
  ggtitle("Which countries tend\nto give the same\ncountry DOUZE points?")

# Combine everything ------------------------------------------------------
# Make a grand title for total plot
grand_title_theme <- ggdraw() +
  draw_label("Political voting over 50 years of Eurovision", 
             fontfamily = "EurofanEurovision", 
             x = 0.05, hjust = 0,colour="orange3",size=80,fontface = "bold")+
  theme(panel.background = element_rect(fill = "gray20",
                                        colour = "gray20"))

# Bring it all together
pdf("220517_eurovision/figs/final_euro_fig.pdf",width=16,height=14)
plot_grid(
  grand_title_theme,
  plot_grid(
    plot_grid(network_title_theme,
    plot_grid(plotlist=lapply(network_list,'[[',1)[1:4],ncol=2),rel_heights = c(1.2,8),ncol=1),
              network_list[[5]][[1]],ncol=2,rel_widths = c(1,1.8)),
  plot_grid(
    cluster_through_time,bias_map,ncol=2,rel_widths = c(3,1.555)),
  ncol=1,rel_heights=c(1,5,5)
)
dev.off()


