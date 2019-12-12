library("TreeSim")
library("ggtree")
library("ggplot2")
library("phytools")
library("dplyr")

set.seed(643)
fulltree <- sim.bd.taxa(30, 1, 0.1, 0.09)[[1]]
moltree <- drop.extinct(fulltree)
moltree <- drop.tip(moltree, tip = "t127")

pfull <- ggtree(fulltree)
pmol <- ggtree(moltree, ladderize = FALSE) + scale_y_reverse()

dfull <- pfull$data
dmol <- pmol$data

## reverse x-axis and 
## set offset to make the tree in the right hand side of the first tree
dmol$x <- max(dmol$x) - dmol$x + max(dfull$x) + 100
dmol$y <- scales::rescale(dmol$y, to = c(min(dfull$y), max(dfull$y)))

pp <- pfull +
    geom_tree(data = dmol)

dd <- bind_rows(dfull, dmol) %>% 
  filter(!is.na(label))

final.plot <- pp + geom_line(aes(x, y, group = label), data = dd, color = 'red', size = 1, linetype = "dashed")

ggsave(filename = "sim_tree_plot.png", plot = final.plot)

ggsave(filename = "sim_tree_plot.svg", plot = final.plot)
