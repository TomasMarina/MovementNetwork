## Data visualization
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


## Nodes viz ----

# Viz of nodes on Isla de los Estados map (idle)
# Isla de los Estados shapefile
shapefile_idle <- "../shapes_plot/idle/idle.shp" %>%
  st_read()

# All sites
no_sites_all <- nrow(nodes_all)
nodemap_all <- ggplot() +
  geom_sf(data = shapefile_idle) + 
  geom_point(data = nodes_all, aes(lon, lat, size = log(frequency), colour = no.ind)) +
  scale_color_gradient(low = "blue", high = "red", "# individuals") +
  theme_bw() +
  theme(axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  guides(size = guide_legend("log(frec-visita)"), colour = guide_legend("# Individuos")) +
  labs(title = "All sites") +
  annotate(geom = "text", x = Inf, y = Inf, vjust=1, hjust=1, label=paste('# sites:', no_sites_all))

# All sites & shape of penguin colony
# Penguins' colony shapefile
shapefile_col <- "../shapes_plot/poligono_colonia/franklin_utm.shp" %>%
  st_read()
nodemap_all_penguin <- ggplot() +
  geom_sf(data = shapefile_idle) +
  geom_point(data = nodes_all, aes(lon, lat), colour = "blue") +
  geom_sf(data = shapefile_col, colour = "red", fill = NA) +
  theme_bw() +
  theme(axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Striated Caracara sites and Penguins colony (red polygon)")


## Filtered by 'colonia' site

no_sites_colonia <- nrow(nodes_colonia)
nodemap_colonia <- ggplot() +
  geom_sf(data = shapefile_idle) +
  geom_point(data = nodes_colonia, aes(lon, lat, colour = as.factor(colonia))) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_bw() +
  theme(axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  guides(colour = guide_legend("Colonia")) +
  labs(title = "Sites filtered by 'colonia' site") +
  annotate(geom = "text", x = Inf, y = Inf, vjust=1, hjust=1, label=paste('# sites:', no_sites_colonia))

# Number of individuals and frequency of visit
int_breaks <- function(x, n = 5) {
  l <- pretty(x, n)
  l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
}
nodemap_ind <- ggplot() +
  geom_sf(data = shapefile_idle) +
  geom_point(data = nodes_colonia, aes(lon, lat, colour = no.ind,  size = frequency)) +
  scale_color_gradient(low = "blue", high = "red", "# individuals", breaks = int_breaks) +
  theme_bw() +
  theme(axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  guides(colour = guide_legend("# individuals", order = 1), size = guide_legend("Frequency", order = 2)) +
  labs(title = "How much and who visits the sites?")

# Plots by time (day, night)
nodemap_time <- ggplot() +
  geom_sf(data = shapefile_idle) +
  geom_point(data = nodes_colonia, aes(lon, lat, colour = time, size = log(frequency))) +
  scale_color_discrete(labels = c("Both", "Day", "Night")) +
  theme_bw() +
  theme(axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  guides(colour = guide_legend("Time", order = 1), size = guide_legend("log(visit.freq)", order = 2))

# For night plot replace 'nodes_col_day' by 'nodes_col_night',
# 'no_sites_col_day' by 'no_sites_col_night' & colour = "#619CFF"
no_sites_col_day <- nrow(nodes_col_day)
no_sites_col_night <- nrow(nodes_col_night)

nodemap_col_day <- ggplot() +
    geom_sf(data = shapefile_idle) +
    geom_point(data = nodes_col_day, aes(lon, lat, size = log(frequency)), colour = "#00BA38") +
    labs(title = "DAY") +
    theme_bw() +
    theme(axis.line = element_line(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(face = "bold", size = 14)) +
    guides(size = guide_legend("log(visit.freq)", order = 2)) +
    annotate(geom = "text", x = Inf, y = Inf, vjust=1, hjust=1, label=paste('sites:', no_sites_col_day), size = 4.5)

# ’Sea lion-associated’ sites
no_sites_lobos <- length(which(nodes_colonia$lobos == "1"))
nodemap_col_lobos <- ggplot() +
  geom_sf(data = shapefile_idle) +
  geom_point(data = nodes_colonia, aes(lon, lat, colour = as.factor(lobos))) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_bw() +
  theme(axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  guides(colour = guide_legend("Sea lion")) +
  labs(title = "’Sea lion-associated’ sites") +
  annotate(geom = "text", x = Inf, y = Inf, vjust=1, hjust=1, label=paste('# sites:', no_sites_lobos))


## Save plots

# pdf("../figures/nodemap_bytime.pdf")
# grid.arrange(nodemap_col_day, nodemap_col_night, nrow = 2)
# dev.off()

# pdf("../figures/nodemaps_sites.pdf")
# grid.arrange(arrangeGrob(nodemap_ind, top = "How much and who visits the sites?"),
#              arrangeGrob(nodemap_time, top = "Time of visit and frequency"),
#              nrow = 2)
# dev.off()


## Network viz ----

# Firstly, we need to organize link data for plotting on map

# All sites and links
links_all$from <- as.integer(links_all$from)
links_all$to <- as.integer(links_all$to)
links_plot_all <- links_all %>%
  inner_join(nodes_all %>% select(site, lon, lat), by = c('from' = 'site')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(nodes_all %>% select(site, lon, lat), by = c('to' = 'site')) %>%
  rename(xend = lon, yend = lat)
assert_that(nrow(links_plot_all) == nrow(links_all))

netmap_all <- ggplot() +
  geom_sf(data = shapefile_idle) +
  geom_segment(data = links_plot_all,
               aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
               size = 0.5, alpha = 0.5) +
  geom_point(data = nodes_all, aes(lon, lat, colour = as.factor(colonia))) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_bw() +
  theme(axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  guides(colour = guide_legend("Colonia"))

# Filtered by 'colonia' site
links_colonia$from <- as.integer(links_colonia$from)
links_colonia$to <- as.integer(links_colonia$to)
links_plot_colonia <- links_colonia %>%
  inner_join(nodes_colonia %>% select(site, lon, lat), by = c('from' = 'site')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(nodes_colonia %>% select(site, lon, lat), by = c('to' = 'site')) %>% 
  rename(xend = lon, yend = lat)
assert_that(nrow(links_plot_colonia) == nrow(links_colonia))

netmap_colonia <- ggplot() +
  geom_sf(data = shapefile_idle) +
  geom_segment(data = links_plot_colonia,
               aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.05, "inches"), type = "closed"),
               size = 0.5, alpha = 0.5) +
  geom_point(data = nodes_colonia, aes(lon, lat),
             colour = ifelse(nodes_colonia$colonia == 1, "red", "blue")) +
  theme_bw() +
  theme(axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  guides(colour = guide_legend("Colonia"))

# Pre and pos-penguin networks (2020-03-10)
# For pospen plot replace 'links_col_prepen' by 'links_col_pospen',
# 'links_plot_col_prepen' by 'links_plot_col_pospen' & 
# 'nodes_col_prepen' by 'nodes_col_pospen'

links_col_prepen$from <- as.integer(links_col_prepen$from)
links_col_prepen$to <- as.integer(links_col_prepen$to)
links_plot_col_prepen <- links_col_prepen %>%
  inner_join(nodes_col_prepen %>% select(site, lon, lat), by = c('from' = 'site')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(nodes_col_prepen %>% select(site, lon, lat), by = c('to' = 'site')) %>%
  rename(xend = lon, yend = lat)
assert_that(nrow(links_plot_col_prepen) == nrow(links_col_prepen))

no_links_col_prepen <- nrow(links_col_prepen)  # or no_links_col_pospen <- nrow(links_col_pospen)
no_sites_col_prepen <- nrow(nodes_col_prepen)  # or no_links_col_pospen <- nrow(links_col_pospen)
netmap_col_prepen <- ggplot() +
    geom_sf(data = shapefile_idle) +
    geom_segment(data = links_plot_col_prepen,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 arrow = arrow(length = unit(0.075, "inches"), type = "closed"),
                 size = 0.5, alpha = 0.5) +
    geom_point(data = nodes_col_prepen, aes(lon, lat),
               colour = ifelse(nodes_col_prepen$colonia == 1, "red", "blue")) +
    labs(title = "BEFORE") +
    theme_bw() +
    theme(axis.line = element_line(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(face = "bold")) +
    annotate(geom = "text", x = Inf, y = Inf, vjust=1, hjust=1, label= paste('links:', no_links_col_prepen), size = 5) +
    annotate(geom = "text", x = -Inf, y = Inf, vjust=1, hjust=-.1, label=paste('sites:', no_sites_col_prepen), size = 5)

# By individual
# Replace 'indA' in every line to refer to B, C, D, or E ind
# Change 'scale_color_manual' to highlight ind sites & links

nodes_colonia$indA <- "NA"
nodes_colonia$indA <- ifelse(nodes_colonia$site %in% nodes_indA$site, "TRUE", "FALSE")

no_sites_indA <- nrow(nodes_indA)
no_links_indA <- nrow(links_indA)
netmap_indA <- ggplot() +
    geom_sf(data = shapefile_idle) +
    geom_segment(data = links_plot_colonia,
                 aes(x = x, xend = xend, y = y, yend = yend, colour = ind),
                 arrow = arrow(length = unit(0.075, "inches"), type = "closed"),
                 size = 0.75, alpha = 0.5) +
    scale_color_manual(values = c("A" = "red", "B" = "gray60", "C" = "gray60",
                                  "D" = "gray60", "E" = "gray60")) +
    theme_bw() +
    theme(axis.line = element_line(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    guides(colour = guide_legend("Individual")) +
    annotate(geom = "text", x = Inf, y = Inf, vjust=1, hjust=1, label= paste('links:', no_links_indA), size = 5) +
    annotate(geom = "text", x = -Inf, y = Inf, vjust=1, hjust=-.1, label=paste('sites:', no_sites_indA), size = 5)
netmap_indA <- netmap_indA +  geom_point(data = nodes_colonia, aes(lon, lat), colour = ifelse(nodes_colonia$indA == "TRUE", "red", "gray60"), show.legend = F)


# 'Sea lion-associated' network
links_plot_col_pospen$lobos <- ifelse(links_plot_col_pospen$to %in% c("77", "81", "85", "90", "91", "93", "100", "109", "110", "114", "128", "130", "135", "139", "140", "142", "143", "144", "145", "146", "147", "149", "150", "154", "155", "158", "162", "163", "166", "167", "178", "181"), "1", "0")
no_links_to_lobos <- sum(links_plot_col_pospen$lobos == "1")

netmap_col_lobos_con <- ggplot() +
  geom_sf(data = shapefile_idle) +
  geom_segment(data = links_plot_col_pospen,
               aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.05, "inches"), type = "closed"),
               colour = ifelse(links_plot_col_pospen$lobos == 1, "red", "gray60"),
               size = 0.5, alpha = 0.5) +
  geom_point(data = nodes_col_pospen, aes(lon, lat),
             colour = ifelse(nodes_col_pospen$lobos == 1, "red", "gray60")) +
  theme_bw() +
  theme(axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
  annotate(geom = "text", x = Inf, y = Inf, vjust=1, hjust=1, label=paste('# in-links:', no_links_to_lobos)) +
  annotate(geom = "text", x = -Inf, y = Inf, vjust = 1, hjust = -.1, label=paste('# sea lion-associated nodes:', no_sites_lobos))
