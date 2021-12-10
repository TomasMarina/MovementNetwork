## Network analysis
# Author: TIM
# Date: December 2021


## Load packages ----

pkg_list <- c("dplyr", "tidyr", "igraph", "sf", "ggplot2", "gridExtra", "assertthat")

lapply(pkg_list, FUN = function(pkg_list) {
  do.call("require", list(pkg_list)) 
})


## Load data ----

# Load curated data (node & link lists) from 1_datawrangling.R
load(file = "../data/data_curated.rda")
# Isla de los Estados shapefile
shapefile_idle <- "../shapes_plot/idle/idle.shp" %>%
  st_read()

## 'igraph' objects ----

net_colonia <- graph_from_data_frame(links_colonia, directed = TRUE, vertices = nodes_colonia)
net_col_prepen <- graph_from_data_frame(links_col_prepen, directed = TRUE, vertices = nodes_col_prepen)
net_col_pospen <- graph_from_data_frame(links_col_pospen, directed = TRUE, vertices = nodes_col_pospen)


## Network analysis ----

# Centrality indices
# Degree (total, in- & out-links)
V(net_colonia)$tot.deg <- degree(net_colonia, mode = "total")
V(net_colonia)$in.deg <- degree(net_colonia, mode = "in")
V(net_colonia)$out.deg <- degree(net_colonia, mode = "out")
vertex.attributes(net_colonia)

data_totdeg <- degree(net_colonia, mode = "total")
hist_deg <- ggplot(data.frame(data_totdeg), aes(x = data.frame(data_totdeg)[,1])) +
    geom_bar(width = 2, position = position_dodge(width = 10)) +
    labs(x = "Total number of links", y = "Frequency") +
    theme_bw() +
    theme(axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))

data_indeg <- degree(net_colonia, mode = "in")
hist_indeg <- ggplot(data.frame(data_indeg), aes(x = data.frame(data_indeg)[,1])) +
  geom_bar() +
  labs(x = "Total number of in-links", y = "Frequency") +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

data_outdeg <- degree(net_colonia, mode = "out")
hist_outdeg <- ggplot(data.frame(data_outdeg), aes(x = data.frame(data_outdeg)[,1])) +
  geom_bar() +
  labs(x = "Total number of out-links", y = "Frequency") +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

# Shortest paths from 'colony' site (#10) to sea lion-associated sites
caminos <- shortest.paths(net_colonia, v = V(net_colonia)$colonia == "1", to = V(net_colonia)$lobos == "1", mode = "out")
caminos <- data.frame(caminos)
caminos <- reshape(caminos, varying = list(names(caminos)[1:32]), v.names = "Frecuencia", timevar = "Sitios", direction = "long")
hist_cam <- ggplot(caminos, aes(x = Frecuencia)) +
    geom_bar() +
    scale_x_continuous("Shortest path (links)", breaks = c(1,2,5,10)) +
    labs(x = "Shortest path (links)", y = "Frequency") +
    theme_bw() +
    theme(axis.line = element_line(),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
