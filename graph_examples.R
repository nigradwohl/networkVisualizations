library(igraph)


# lattice graph: -------------------------------
  graph_reg <- make_chordal_ring(15,
                                 matrix(rep(3, 3), nr = 1))
  plot(graph_reg, layout = layout.circle(graph_reg))
  
  graph_lat <- make_lattice(length = 6, dim = 2, circular = TRUE)
  
  graph_lat <- make_lattice(c(4, 4), circular = TRUE)
  
  plot(graph_lat, layout = layout.circle(graph_lat))
  
  # plot(graph_lat, layout = layout.reingold.tilford(graph_lat))
  
  
  # Rewiring:
  lat_rewire <- rewire(graph_lat, with = each_edge(prob = 0.5))
  
  plot(lat_rewire, layout = layout.circle(lat_rewire))
  
  
# From Judd: full graphs connected:
  nodefull <- 8
  graph_full <- make_full_graph(n = nodefull)
  
  plot(graph_full, layout = layout.circle(graph_full))
  
  # https://stackoverflow.com/questions/48000074/combining-merging-two-graphs-in-igraph
  
  graph_full3 <- graph_full2 <- graph_full
  
  V(graph_full)$name <- 1:nodefull
  V(graph_full2)$name <- (nodefull+1):(2*nodefull)
  V(graph_full3)$name <- (nodefull*2+1):(3*nodefull)
  
  attrs <- rbind(as_data_frame(graph_full, "vertices"), 
                 as_data_frame(graph_full2, "vertices"),
                 as_data_frame(graph_full3, "vertices"))
  el <- rbind(as_data_frame(graph_full), as_data_frame(graph_full2),
              as_data_frame(graph_full3))

  new_g <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)  

  # Connect in one node:
  new_g <- add.edges(new_g, c(3, 11, 19, 3))
  
  layout.circle(graph_full)
  
  layout.circle(graph_full)
  
  layout_in_circles <- function(g, group=3) {
    layout <- lapply(split(V(g), group), function(x) {
      layout_in_circle(induced_subgraph(g,x))
    })
    # layout <- Map(`*`, layout, seq_along(layout))
    layout <- lapply(1:length(unique(group)), FUN = function(lay){
      # print(lay)
      laycur <- layout[[lay]]
      laycur[,1] <- laycur[,1] + (lay - 1) * 3  # add offset.
      # laycur[,2] <- laycur[,2]
      return(laycur)
    })
    x <- matrix(0, nrow=vcount(g), ncol=2)
    split(x, group) <- layout
    x[,1] <- x[,1]/max(x[,1])
    x
  }
  
  plot(new_g, 
       layout = layout_in_circles(new_g, 
                                  group = cut(V(new_g), 
                                              breaks = seq(0, max(V(new_g)), by = nodefull))))
  
  plot(rewire(new_g, with = each_edge(prob = 0.1)),
       layout = layout_in_circles(new_g))
  
  # TODO: check component_wise()
  