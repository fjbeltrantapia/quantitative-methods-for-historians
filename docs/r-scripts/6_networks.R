###### Social networks ######

# clear de "Global Environment"
rm(list=ls()) 

# intro - simulated networks (run the following code: up to line 48)
### nodes, edges, etc.
### network structure
### agents: centrality, bridges, etc.
library(tidyverse)
library(igraph)      
library(tidygraph)   
library(ggraph)

set.seed(123)
# Random (Erdős–Rényi): lower p increases sparsity
g_random <- sample_gnp(n = 50, p = 0.03, directed = FALSE) |>
  as_tbl_graph() |>
  mutate(degree = centrality_degree(),
         betweenness = centrality_betweenness(),
         closeness = centrality_closeness()) |>
  ggraph(layout = "nicely") +
  geom_edge_link(alpha = 0.2) +
  geom_node_point(size = 2, color = "steelblue") +
  theme_void()


# Scale-free (Barabási–Albert)
g_scalefree <- sample_pa(n = 50, power = 2, m = 1, directed = FALSE) |>
  as_tbl_graph() |>
  mutate(degree = centrality_degree(),
         betweenness = centrality_betweenness(),
         closeness = centrality_closeness()) |>
  ggraph(layout = "nicely") +
  geom_edge_link(alpha = 0.2) +
  geom_node_point(size = 2, color = "steelblue") +
  theme_void()

# Clustered (high transitivity)
g_clustered <- sample_k_regular(no.of.nodes = 50, k = 4, directed = FALSE) |>
  as_tbl_graph() |>
  mutate(degree = centrality_degree(),
         betweenness = centrality_betweenness(),
         closeness = centrality_closeness()) |>
  ggraph(layout = "nicely") +
  geom_edge_link(alpha = 0.2) +
  geom_node_point(size = 2, color = "steelblue") +
  theme_void()

library(patchwork)
g_random + g_scalefree + g_clustered

# set working directory
setwd("/Volumes/francijb/Documents/FRAN/Teaching/QM_2024/session") 

# upload basic packages
library(tidyverse)

# import the data
letters <- read_csv("data/tnp/tnp_letters.csv")
letters

# exploring the data (basic desc stats)
letters |>
  count(from, sort = TRUE) |>
  mutate(perc = 100*n/sum(n)) |>
  mutate(cum_perc = cumsum(perc))

senders <- letters |> count(from) |> rename(n_out = n, agent = from)
receivers <- letters |> count(to) |> rename(n_in = n, agent = to)
library(ggrepel)
senders |> full_join(receivers, by = "agent") |>
  ggplot(aes(x = n_out, y = n_in)) +
  geom_point() + 
  geom_text_repel(aes(label = ifelse(n_out > 10 | n_in > 25, agent, "")),
                  vjust = -0.5, size = 2)

### Exploring networks: all links simultaneously

## convert the object into a network

# install.packages("igraph")
library(igraph)

network <- graph_from_data_frame(
  d = letters, 
  # vertices = people,      
  directed = TRUE) 

network

## use a different package so it is "tidy"
library(tidygraph)

network_tidy <- letters |>
  as_tbl_graph()
network_tidy

### visualising the network

library(ggraph)

set.seed(345) # so we see the same every time
network_tidy |>
  ggraph("fr") + 
  geom_edge_link0(alpha = 0.3) + 
  geom_node_point(size = 0.5, alpha = 0.3, 
                  color = "blue") + 
  theme_graph()

## connected vs disconnected nodes
comps <- components(network_tidy)
  # three elements:
    # 1. a vector assigning each node to a component (comps\$membership); 
    # 2. the size of each component, that is, the number of nodes (comps\$csize); 
    # 3. the total number of components (comp\$no).

comps$csize

## extract the biggest component
main_network <- network_tidy |>
  induced_subgraph(
    which(comps$membership == which(comps$csize>=10))) |> # <1>
  as_tbl_graph() # <2>
main_network

set.seed(456)
main_network |> # visualise it
  ggraph("fr") + 
  geom_edge_link0(alpha = 0.3) + 
  geom_node_point(size = 0.5, alpha = 0.3,
                  color = "blue") + 
  theme_graph()

##### Network metrics: global and local

## Global metrics
  # characterising the network itself
    # compared to other networks or the same network over time
  # size, density, diameter, average path length, connectedness...

#### Extract two networks: before and during Elizabeth I's reign (1558-1603)
n1 <- network_tidy |>
  activate("edges") |>
  mutate(year = date_from %/% 10000) |> # <1>
  filter(year<1558)
comps <- components(n1) # <2>
n1 <- n1 |>
  induced_subgraph(which(comps$membership == which.max(comps$csize))) |> # <3>
  as_tbl_graph()

n2 <- network_tidy |>
  activate("edges") |>
  mutate(year = date_from %/% 10000) |>
  filter(year>=1558)
comps <- components(n2)
n2 <- n2 |>
  induced_subgraph(which(comps$membership == which.max(comps$csize))) |>
  as_tbl_graph()

set.seed(567)
s <- 3
x <- 11
y <- 26
g1 <- n1 |>
  ggraph("fr") + 
  geom_edge_link0(alpha = 0.3) + 
  geom_node_point(size = 0.5, alpha = 0.3,
                  color = "blue") + 
  annotate("text", 
           x = x, y = y,  
           label = "1509-1557",
           size = s, fontface = "bold") +
  theme_graph() 
g2 <- n2 |>
  ggraph("fr") + 
  geom_edge_link0(alpha = 0.3) + 
  geom_node_point(size = 0.5, alpha = 0.3,
                  color = "blue") + 
  annotate("text", 
           x = x, y = y,  
           label = "1558-1603",
           size = s, fontface = "bold") +
  theme_graph() 
library(patchwork)
g1 + g2

## compute the measures: gorder(), gsize(), diameter(), ...
  # and construct a table reporting them
tibble(
  period = c("1509-1557", "1558-1603"),
  nodes  = c(gorder(n1), gorder(n2)),
  edges  = c(gsize(n1), gsize(n2)),
  density  = c(edge_density(n1), edge_density(n2)),
  diameter  = c(diameter(n1, directed = TRUE), 
                diameter(n2, directed = TRUE)),  
  avg_path  = c(mean_distance(n1, directed = TRUE), 
                mean_distance(n2,  directed = TRUE)))

## Local metrics
### discriminating between the nodes: allows identifying important actors
  # yields a metric for each node (agent)

  # - Degree: number of connections (neighbours).
  # - Closeness: distance to all other nodes.
  # - Betweenness: how a node facilitates accessing other nodes. 
  # - Eigenvector: connectedness to other influential nodes. 

n2 <- n2 |>
  activate("nodes") |> # probably not needed
  mutate(degree_out = centrality_degree(mode = "out"),
         degree_in = centrality_degree(mode = "in"),
         degree_tot = centrality_degree(mode = "total"),
         closeness = centrality_closeness(), 
         betweenness = centrality_betweenness(), 
         eigen = centrality_eigen(directed = TRUE))

n2 |>
  activate("nodes") |>
  arrange(desc(degree_out)) |> 
  as_tibble()

n2 |>
  arrange(desc(closeness)) |> 
  as_tibble()

## visualising local metrics
set.seed(678)
n2 |>
  ggraph("fr") + 
  geom_edge_link0(alpha = 0.3) +
  geom_node_point(aes(size = degree_out,
                      color = closeness), 
                  shape = 16, alpha = 0.6) +
  geom_node_text(aes(label = if_else(closeness>0.5 |
                                       degree_out>10,
                                     name, NA)), size = 1.5) +
  scale_color_gradient(low="#104E8B",high="#CD2626") +
  scale_size(range = c(0.5, 4)) +
  labs(color = "Closeness", 
       size = "Degree")
theme_graph()

### Community (cluster) detection
network_tidy # components

# identifying connected/unconnected nodes
set.seed(345)
network_tidy |>
  mutate(component = group_components(),
         component = as_factor(component)) |>
  ggraph("fr") + 
  geom_edge_link0(alpha = 0.3) + 
  geom_node_point(aes(color = component), size = 0.5) + 
  scale_color_viridis_d() + # <1>  
  theme_graph() + 
  theme(legend.position = "none") 

## find clusters
  # groups of nodes that are densely connected between them
  # many different algorithms: lovain, leiden...

set.seed(456)
main_network |>
  as_undirected(mode = "collapse") |> 
  as_tbl_graph() |> 
  mutate(cluster = group_louvain(),
         cluster = as_factor(cluster)) |>
  ggraph("fr") + 
  geom_edge_link0(alpha = 0.3) + 
  geom_node_point(aes(color = cluster), size = 0.5) + 
  theme_graph() + 
  scale_color_viridis_d() +
  theme(legend.position = "none") 

## explore clusters' properties
main_network |>
  as_undirected(mode = "collapse") |> 
  as_tbl_graph() |> 
  mutate(cluster = group_louvain(),
         cluster = as_factor(cluster)) |>
  activate(nodes) |>
  as_tibble() |>
  count(cluster, sort = TRUE) 

## spatial networks (map them to "real" locations)
