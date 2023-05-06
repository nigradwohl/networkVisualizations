library(igraph)


# Example graph in presentation:
  adjmat <- cbind(c(0, 0, 0, 0, 0),
                  c(0, 0, 1, 0, 0),
                  c(0, 1, 0, 1, 1),
                  c(0, 0, 1, 0, 1),
                  c(1, 0, 0, 1, 0))
  
  gr <- graph_from_adjacency_matrix(adjmat)
  
  V(gr)$color <- c(2, 2, 1, 1, 1)
  
  
  ## End(Not run)
  curve_multiple(gr, start = 0)
  

  # Add edges:
  gr <- add.edges(gr, c(1, 2, 5, 3))
    
  plot(gr, layout = layout.circle(gr),
       nodecolor = V(gr)$color,
       edge.curved = 0.2)
  

  
  assortativity_nominal(gr, types = V(gr)$color, directed = TRUE)
  
  #    a   b
  # a 5/8 1/8  6/8
  # b 2/8 0/8  2/8
  #   7/8 1/8
  
  ai_bi <- (5/8+2/8) * (5/8+1/8) +  # row-col a_i.
    (2/8+0/8) * (1/8 + 0/8) # b-row + bcol.
    
    
  (5/8 - ai_bi) / (1 - ai_bi)
  
  # Disssortative:
  gr <- make_ring(n = 4, directed = FALSE)
  
  V(gr)$color <- c(2, 1, 2, 1)
  
  plot(gr, layout = layout.circle(gr),
       nodecolor = V(gr)$color,
       edge.curved = 0.2)
  
  assortativity_nominal(gr, types = V(gr)$color, directed = TRUE)

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
  
  
# Simple model example: ---------------------------
  library(ggplot2)

  dat <- data.frame(
    mude = seq(-2, 2, by = 0.1)
  )

  dat$doof <- ifelse(dat$mude < 1,
                     exp(1),
                     exp(dat$mude)) - 2
  
  
  ggplot(data = dat, aes(x = mude, y = doof)) +
    geom_line(size = 2, color = "black", alpha = 0.8) +
    theme_classic() +
    labs(x = "Müde", y = "Doof") +
    ylim(0, 10) +
    theme(text = element_text(size = 18))
  
  
  # Add "data":
  dat$data <- ifelse(dat$mude < 0,
                     3,
                     7) + rnorm(nrow(dat))
  
  ggplot(data = dat, aes(x = mude, y = doof)) +
    geom_point(aes(y = data), size = 4, color = "grey") +
    geom_line(size = 2, color = "black", alpha = 0.8) +
    theme_classic() +
    labs(x = "Müde", y = "Doof") +
    ylim(0, 10) +
    theme(text = element_text(size = 18))

  
# Modelle: ----- 
  
  library(deSolve) # using the "ode" function
  
  sir_equations <- function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS <- -beta * I * S
      dI <-  beta * I * S - gamma * I
      dR <-  gamma * I
      return(list(c(dS, dI, dR)))
    })
  }
  
  parameters_values <- c(
    beta  = 0.01, # infectious contact rate (/person/day)
    gamma = 0.5    # recovery rate (/day)
  )
  
  initial_values <- c(
    S = 95,  # number of susceptibles at time = 0
    I =   5,  # number of infectious at time = 0
    R =   0   # number of recovered (and immune) at time = 0
  )
  
  time_values <- seq(0, 30) # days

  
  sir_values_1 <- ode(
    y = initial_values,
    times = time_values,
    func = sir_equations,
    parms = parameters_values 
  )  

  
  sir_values_1 <- as.data.frame(sir_values_1)
  names(sir_values_1)[-1] <- paste0("n_", names(sir_values_1)[-1])
  
  sirdat <- reshape(sir_values_1, direction = "long", idvar = "time", sep = "_",
          varying = names(sir_values_1)[-1],
          timevar = "category")
  
  sirdat$category <- factor(sirdat$category,
                            levels = c("S", "I", "R"))

  
  ggplot(data = sirdat, aes(x = time, y = n, color = category)) +
    geom_line(size = 2) +
    scale_color_viridis_d() +
    theme_classic() +
    labs(x = "Zeit", y = "Anzahl", color = "Kategorie") +
    theme(text = element_text(size = 18))
  
# Alternative: Frequency-based social learning ---------------
  ################################################################
  ## Individual learning (delta rule) 
  ################################################################
  
  # Function to update the vector of option weights:
  update_A <- function(A_it = c(0, 0),
                       pi_it,  # payoff in round t for each option.
                       phi
  ){
    
    # Get the new option weight:
    opWeight <- (1 - phi) * A_it + phi * pi_it
    
    return(opWeight)
  }
  
  ################################################################
  ## Frequency-based learning (McElreath)
  ################################################################
  
  freqbias <- function(iprobs, soc_weight, n_freq, freq_dep = 1){
    
    p_freq <- n_freq^freq_dep / rowSums(n_freq^freq_dep)
    
    # TODO: We are more interested in negative frequency dependence.
    
    
    p <- (1 - soc_weight) * iprobs + soc_weight * p_freq
    
    return(p)
    
  }
  
  softmax <- function(vals, temp){
    evals <- exp(vals * temp)
    
    return(evals/sum(evals))
  }
  
  
  # Example: -------------
  datsoc <- expand.grid(n = 0:10, freq_dep = c(0, 1, 2, 10))
  
  datsoc$p_a <- NA
  
  for(fdep in unique(datsoc$freq_dep)){
    
    n <- datsoc$n[datsoc$freq_dep == fdep]
    ns <- cbind(n, max(n) - n)
    
    datsoc$p_a[datsoc$freq_dep == fdep] <-
      freqbias(0, soc_weight = 1,  freq_dep = fdep,
               n_freq = ns)[,1]
  }

  

  ggplot(data = datsoc, aes(x = n, y = p_a, color = factor(freq_dep))) +
    geom_line(size = 2) +
    scale_color_viridis_d() +
    theme_classic() +
    labs(x = "Anzahl in Option A", 
         y = "Wahrscheinlichkeit A gewählt", 
         color = "Non-linearität") +
    theme(text = element_text(size = 18))
  
  
  ggplot(data = datsoc[datsoc$freq_dep == 2,], aes(x = n, y = p_a, color = factor(freq_dep))) +
    geom_line(size = 2) +
    scale_color_viridis_d() +
    theme_classic() +
    labs(x = "Anzahl in Option A", 
         y = "Wahrscheinlichkeit A gewählt", 
         color = "Non-linearität") +
    theme(text = element_text(size = 18))
  
  
  # Add data:
  data_maj <- data.frame(n = rep(0:10, each = 10),
                         freq_dep = NA)
  
  data_maj$logits = log(ifelse(data_maj$n < 5,
                            0.01/(1-0.01), 0.99/(1-0.99))) + rlogis(nrow(data_maj), scale = 1)
  
  ggplot(data = datsoc[datsoc$freq_dep == 2,], aes(x = n, y = p_a, color = factor(freq_dep))
         ) +
    geom_point(data = data_maj, aes(y = 1/(1 + exp(-logits))),
               color = "grey") +
    geom_line(size = 2) +
    scale_color_viridis_d() +
    theme_classic() +
    labs(x = "Anzahl in Option A", 
         y = "Wahrscheinlichkeit A gewählt", 
         color = "Non-linearität") +
    theme(text = element_text(size = 18))
  
  
  # Add step function:
  ggplot(data = datsoc[datsoc$freq_dep %in% c(2, 10),], aes(x = n, y = p_a, color = factor(freq_dep))
  ) +
    geom_point(data = data_maj, aes(y = 1/(1 + exp(-logits))),
               color = "grey") +
    geom_line(size = 2) +
    scale_color_viridis_d() +
    theme_classic() +
    labs(x = "Anzahl in Option A", 
         y = "Wahrscheinlichkeit A gewählt", 
         color = "Non-linearität") +
    theme(text = element_text(size = 18))
  
  
  ggplot(data = datsoc, aes(x = n, y = p_a, color = factor(freq_dep))) +
    geom_point(data = data_maj, aes(y = 1/(1 + exp(-logits))),
               color = "grey") +
    geom_line(size = 2) +
    scale_color_viridis_d() +
    theme_classic() +
    labs(x = "Anzahl in Option A", 
         y = "Wahrscheinlichkeit A gewählt", 
         color = "Non-linearität") +
    theme(text = element_text(size = 18))
  
# DeGroot process:
  groot_dat <- 