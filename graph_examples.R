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
  new_g <- add.edges(new_g, c(8, 9, 16, 17, 24, 1))
  
  plot(new_g, layout = layout.circle(new_g))
  
  plot(rewire(new_g, with = each_edge(prob = 0.1)),
       layout = layout.circle(new_g))
  
  # TODO: check component_wise()
  