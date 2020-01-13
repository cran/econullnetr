## ---- include = FALSE-----------------------------------------------------------------------------
options(width = 100)

## ---- results="hide"------------------------------------------------------------------------------
library(econullnetr)

set.seed(1234) # To create a reproducible example
sil.null <- generate_null_net(Silene[, 2:7], Silene.plants[, 2:6], sims = 100,
                              data.type = "names", summary.type = "sum",
                              c.samples = Silene[, 1],
                              r.samples = Silene.plants[, 1])


## -------------------------------------------------------------------------------------------------
sil.links <- test_interactions(sil.null, signif.level = 0.95)

## -------------------------------------------------------------------------------------------------
sil.links[40:51, ]

## -------------------------------------------------------------------------------------------------
sum(sil.links$Test == "Weaker" | sil.links$Test == "Stronger")

## ----fig.width=8, fig.height=4, warning = FALSE---------------------------------------------------
# Calculate the mean abundance of each plant species across all samples to 
#   use in the bipartite plot
mean.abunds <- colMeans(Silene.plants[, 2:6]) 

plot_bipartite(sil.null, signif.level = 0.95, 
               edge.cols = c("#67a9cf", "#F7F7F7", "#ef8a62"),
               low.abun = mean.abunds, abuns.type = "independent", 
               low.abun.col = "black", high.abun.col = "black",
               high.lablength = 5, low.lablength = 5)

## ----fig.width = 7, fig.height = 5----------------------------------------------------------------
plot_preferences(sil.null, "Sphaerophoria.scripta", signif.level = 0.95, 
                 type = "counts", xlab = "Number of visits", p.cex = 2, 
                 l.cex = 1, lwd = 2, font = 3)

## ---- results="hide"------------------------------------------------------------------------------
net.stats <- bipartite_stats(sil.null, index.type = "networklevel", 
                             indices = c("linkage density", 
                             "weighted connectance", "weighted nestedness",  
                             "interaction evenness"), intereven = "sum")

## -------------------------------------------------------------------------------------------------
net.stats

## ---- results="hide"------------------------------------------------------------------------------
set.seed(1234)
stream.1 <- generate_null_net(WelshStreams[, 2:18], WelshStreams.prey[, 2:17],
                              sims = 100, data.type = "names", 
                              summary.type = "sum",
                              c.samples = WelshStreams[,1], 
                              r.samples = WelshStreams.prey[,1],
                              r.weights = WelshStreams.fl)

## ---- fig.height=4, fig.width=8-------------------------------------------------------------------
par(mfrow = c(1, 2))
plot_preferences(stream.1, "Rhyacophila", signif.level = 0.95, type = "counts", 
                 xlab = "Num. of prey detections", res.order = 
                 WelshStreams.order, p.cex = 1.5, l.cex = 0.9, lwd = 2)
plot_preferences(stream.1, "Dinocras", signif.level = 0.95, type = "counts", 
                 xlab = "Num. of prey detections", res.order = 
                 WelshStreams.order, p.cex = 1.5, l.cex = 0.9, lwd = 2)

## -------------------------------------------------------------------------------------------------
export1 <- generate_edgelist(stream.1, signif.level = 0.95, 
                             edge.cols = c("#2c7bb6", "#000000", "#d7191c"))

## ---- fig.height = 6, fig.width = 6---------------------------------------------------------------
if (requireNamespace("igraph", quietly = TRUE)) {

net.1 <- igraph::graph_from_edgelist(as.matrix(export1[, c("Resource", 
                                                           "Consumer")]),
                                     directed = TRUE)

# Add in the null modelling results 
igraph::E(net.1)$obs.str <- export1$Observed
igraph::E(net.1)$test.res <- export1$Test
igraph::E(net.1)$edge.cols <- export1$edge.col

igraph::plot.igraph(net.1, layout = igraph::layout_in_circle, 
             edge.color = igraph::E(net.1)$edge.cols, 
             edge.width = sqrt(igraph::E(net.1)$obs.str))
}

## ---- fig.height=5, fig.width=8-------------------------------------------------------------------
# Create a data frame to assist with plotting
# trph.level = a simple trophic level: primary consumers = 1, predators = 2
if (requireNamespace("igraph", quietly = TRUE)) {
  n.names <- data.frame(species = igraph::V(net.1)$name, 
                        trph.level = c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                       1, 2, 1, 1)) 

# Calculate prey abundance across the six streams
abund <- as.matrix(colSums(WelshStreams.prey[, 2:17]) )
n.names <- merge(n.names, abund, by.x = "species", by.y = "row.names", 
                 sort = FALSE)
colnames(n.names)[3] <- "Abundance"

# Specify x-coordinates for the nodes, based on their order in the Centre for Ecology and Hydrology's Coded Macroinvertebrate List, positions for the labels and abbreviated names for the nodes
n.names$x.coord <- c(1, 4.5, 3, 11, 14, 5, 2, 10, 7, 8, 12, 9, 4, 10.5, 13, 6)
n.names$lab.deg<-c(pi/2,-pi/2, pi/2, pi/2, pi/2, pi/2, pi/2, pi/2, pi/2, pi/2, 
                   pi/2, pi/2, pi/2, -pi/2, pi/2, pi/2)
n.names$short.names <- strtrim(n.names$species, 4)
n.names$short.names[2] <- "Dinocras"
n.names$short.names[14] <- "Rhyacophila"

# Create curved edges between Dinocras & Rhyacophila so that predation in both directions can be shown clearly.
igraph::E(net.1)[13] # Confirms that edge 13 = Rhyacophila to Dinocras 
igraph::E(net.1)[18] # Confirms that edge 18 = Dinocras to Rhyacophila
curve.edge <- rep(0, igraph::ecount(net.1)) 
curve.edge[c(13, 18)] <- 0.5
curve.arrows <- rep(0, igraph::ecount(net.1))
curve.arrows[c(13, 18)] <- 2

# Create the food web plot
igraph::plot.igraph(net.1, layout = as.matrix(n.names[, c("x.coord", 
                                                          "trph.level")]),
             vertex.shape = "rectangle", 
             vertex.size = log(n.names$Abundance),
             vertex.size2 = 20, edge.curved = curve.edge, 
             edge.arrow.mode = curve.arrows, 
             edge.color = igraph::E(net.1)$edge.cols,
             edge.width = sqrt(igraph::E(net.1)$obs.str), 
             vertex.color = "#000000", 
             vertex.label = n.names$short.names, 
             vertex.label.degree = n.names$lab.deg, vertex.label.family = "",
             vertex.label.dist = rep(3, 16), vertex.label.cex = 0.75, 
             asp = .4)
}

