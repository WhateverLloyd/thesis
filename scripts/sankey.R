# Libraries
library(tidyverse)
library(networkD3)

NLCDsankey <- read.csv("./data/NLCD_sankey2.csv")

links <-    data.frame(source = paste0(NLCDsankey$Source, " (2001)"),
                      target = paste0(NLCDsankey$Target, " (2016)"),
                      value = NLCDsankey$Hectares,
                      stringsAsFactors = FALSE)

# build a nodes data frame from the new links data frame
nodes <-    data.frame(name = unique(c(links$source, links$target)), 
                       stringsAsFactors = FALSE)

# change the source and target variables to be the zero-indexed position of
# each node in the new nodes data frame
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# remove the year indicator from the node names
nodes$name <- substring(nodes$name, 1, nchar(nodes$name) - 7)

# plot it
landcover_sankey <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                    Target = "target", Value = "value", NodeID = "name",
                    fontSize = 14, nodeWidth = 30, nodePadding = 12)
