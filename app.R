#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# It creates nodes that can be connected to build a network and returns its features.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(igraph)
library(unikn)
library(ggnetwork)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("My network"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            sliderInput("n",
                        "n(nodes):",
                        min = 2,
                        max = 50,
                        step = 1,
                        value = 15),
            
            selectInput("graph",
                        "Graph type",
                        choices = list("Random (Erdös Renyi)",
                                       "Random (Small World)",
                                       "Ring lattice",
                                       "Ring",
                                       "Preferential attachment",
                                       "Fully connected"),
                        multiple = FALSE),
            
            
            # TODO: Change node color?
            sliderInput("p_a",
                        "Proportion petrol:",
                        min = 0,
                        max = 1,
                        step = 0.1,
                        value = 0),
            
            
            # Re-add p_rewire
            # shinyWidgets::sliderTextInput("p_rewire","p(rewire):",
            #                               choices=c(seq(0, 0.2, by = 0.02),
            #                                         seq(0.3, 1, by = 0.1)),
            #                               selected=0, grid = TRUE),
            
            
            checkboxInput("show_nodelabs",
                          "Knotenlabels zeigen")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("netPlot",
                       click = "netPlot_click",
                       hover = "netPlot_hover"),
            
            verbatimTextOutput("node_info")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Initialze selected nodes:
    sel_nodes <- c()
    # TODO: allow to create an edgelist?
    
    cur_edges <- c()
    
    cur_n_nodes <- 15
    
    colvec <- c(a = unname(Petrol), b = unname(Peach))
    
    cur_graph_type <- "Random (Erdös Renyi)"
    
    seed_small_world <- sample(0:99999)
    
    
    
    # Create the "fixed" part of the data: ---------------------------
    gcur <- reactive({
        
        # Create graph without connections:
        # g <- erdos.renyi.game(n = input$n, 
        #                  p.or.m = 0, 
        #                  type = "gnp",
        #                  directed = TRUE)
        
        n_nodes <- input$n
        
        # TODO: Currently adding and removing (but not only removing) is possible!
        
        set.seed(seed_small_world)
        g <- switch(input$graph,
                    "Random (Erdös Renyi)" = erdos.renyi.game(n = n_nodes, 
                                                              p.or.m = 0, 
                                                              type = "gnp",
                                                              directed = TRUE),
                    "Random (Small World)" = watts.strogatz.game(1, size = n_nodes, 
                                                                 nei = 3, 
                                                                 p = 0.2, loops = FALSE, multiple = FALSE),
                    # TODO: Sensibel defaults for random graphs or make flexible!
                    "Ring lattice" = make_chordal_ring(n = 15,  # input$n,
                                                       matrix(rep(3, 3), nr = 1),
                                                       directed = TRUE),
                    "Ring" = make_ring(n = n_nodes,
                                       directed = TRUE, mutual = TRUE  # make mutual edges the default.
                    ),
                    "Preferential attachment" = sample_pa(n = n_nodes, 
                                                          power = 3, directed = TRUE),
                    "Fully connected" = make_full_graph(n = n_nodes),
                    directed = TRUE)
        
        # Determine layout:
        g_lay <- layout.circle(g)
        
        # If the current number of nodes changed:
        if(cur_n_nodes != input$n | cur_graph_type != input$graph){
            sel_nodes <<- c()
            cur_edges <<- c(t(get.edgelist(g)))
            
            cur_graph_type <<- input$graph
        }
        
        # Save the current number of nodes:
        cur_n_nodes <<- input$n
        
        # NODE FEATUERS: --------------------------------
        n_nodes <- length(V(g))
        # Determine node types: -------------------------
        
        # n_a <- ceiling(n_nodes * 0.5)  # get requested type a.
        n_a <- ceiling(n_nodes * input$p_a)  # get requested type a.
        # TODO
        
        # Determine colors randomly:
        rancols <- c(rep("a", n_a), rep("b", n_nodes - n_a))
        
        V(g)$color <- rancols  # sample(rancols)
        colvec <- c(a = unname(Petrol), b = unname(Peach))
        
        V(g)$name <- 1:length(V(g))
        
        # TODO: Allow to change node color through clicking?
        
        # Selection process:
        gnet <- ggnetwork(g, layout = g_lay)  # get current intermediate network.
        
        # Get where the click was:
        pts <- nearPoints(as.data.frame(gnet),
                          input$netPlot_click,
                          addDist = FALSE,
                          threshold = 10,
                          maxpoints = 1)
        
        # print(pts)
        
        # gcur()
        
        # Save selected nodes globally:
        sel_nodes <<- c(sel_nodes, pts$name)
        
        if(length(sel_nodes) == 2){
            
            
            if(var(sel_nodes) > 0){
                # Add the edge to the (persistent) list!
                cur_edges <<- c(cur_edges, sel_nodes)
            }
            
            # Deal with duplicates:
            edgemat <- matrix(cur_edges, ncol = 2, byrow = 2)
            rem <- edgemat[duplicated(edgemat)]  # entries to remove.
            
            if(any(duplicated(edgemat))){
                row_rem <- rowSums(edgemat == rem[col(edgemat)]) == ncol(edgemat)  # rows to remove.
                edgemat <- edgemat[!row_rem, ]  # remove nodes.
                
                # Update the current edges:
                cur_edges <<- c(t(edgemat))
            }
            
            # print(edgemat)
            
            # Nullify the selected nodes:
            sel_nodes <<- c()
        }
        
        # Add edges:
        g <- add.edges(g, cur_edges)
        
        
        # Render selected node(s):
        V(g)$selected <- ifelse(V(g)$name %in% sel_nodes, TRUE, FALSE)
        
        # print(curnet)
        
        return(g)
    })
    
    
    # Build the current graph:
    cur_g <- reactive({
        
        # gcur <- gcur()  # curnet  # non-reactive... # gcur()  
        gcur <- gcur()  # get the igraph object.
        
        # print(gcur)
        
        # Determine layout:
        g_lay <- layout.circle(gcur)
        
        
        
        # Centrality on each node: ----------------------
        V(gcur)$degree <- degree(gcur, loops = FALSE)
        V(gcur)$indegree <- degree(gcur, mode = "in", loops = FALSE)
        V(gcur)$outdegree <- degree(gcur, mode = "out", loops = FALSE)
        V(gcur)$between <- betweenness(gcur)
        V(gcur)$eigen <- eigen_centrality(gcur)$vector
        
        # print(transitivity(gcur, type = "local"))
        V(gcur)$trans <- transitivity(gcur, type = "local")
        
        
        # Determine assortment: -------------------------
        # gr_ass <- assortativity(g_rewire, 
        #                                 types1 = as.numeric(V(g_rewire)$color == "a"),
        #                                 directed = TRUE)
        gr_ass <- assortativity_nominal(gcur, 
                                        types = as.numeric(V(gcur)$color == "a") + 1,
                                        directed = TRUE)
        
        
        gr_dia <- diameter(gcur)  # diameter.
        
        gr_dens <- edge_density(gcur)  # density.
        
        gr_centr <- centralization.degree(gcur)$centralization  # centralization.
        
        gr_trans <- transitivity(gcur)
        
        cur_g <- ggnetwork(gcur, layout = g_lay,
                           arrow.gap = 0.03)
        
        return(list(cur_g = cur_g,
                    graph_attr = c(gr_ass = gr_ass,
                                   gr_dia = gr_dia,
                                   gr_dens = gr_dens,
                                   gr_centr = gr_centr,
                                   gr_trans = gr_trans)))
    })
    
    # TODO: Hover over nodes to see properties?
    # observeEvent(input$netPlot_hover,
    #              {
    # 
    #                  # print("Hovering!")
    #                  cur_g <- cur_g()
    #                  # gcur <- gcur()  # get the igraph object.
    #                  np <- nearPoints(as.data.frame(cur_g$cur_g),
    #                                   input$netPlot_hover,
    #                                   addDist = FALSE,
    #                                   threshold = 5,
    #                                   maxpoints = 1)
    #                  print(np)
    #                  
    #                  # As tooltip?
    #                  # https://ebailey78.github.io/shinyBS/docs/Tooltips_and_Popovers.html
    #              })
    
    
    output$node_info <- renderText({
        cur_g <- cur_g()
        
        np <- nearPoints(as.data.frame(cur_g$cur_g),
                         input$netPlot_hover,
                         addDist = FALSE,
                         threshold = 5,
                         maxpoints = 1)
        
        degree_all <- paste0("Grad: ", round(np["degree"], 2), 
                             " (in: ", round(np["indegree"], 2), ", ",
                             "out: ", round(np["outdegree"], 2), ")")
        
        paste0(
            c("", "\nZwischenzentralität: ", "\nEigenvektor: ", "\nTransitivität: "), 
            c(degree_all, round(np[c("between", "eigen", "trans")], 2)), 
            sep = "\n")
        
    })
    
    
    # Plot output:
    output$netPlot <- renderPlot({
        
        cur_g <- cur_g()
        
        # print(cur_g$cur_g)
        
        # Plot: ------------------------------
        cur_graph <- ggplot(cur_g$cur_g,
                            aes(x = x, y = y, xend = xend, yend = yend),
                            curvature = 15) +
            geom_edges(color = "darkgrey", size = 1,
                       arrow = arrow(length = unit(8, "pt"), type = "closed",
                       )
            ) +
            geom_nodes(data = cur_g$cur_g[cur_g$cur_g$selected,],
                       color = "black", fill = "black", size = 15) +
            geom_nodes(aes(color = color, fill = color), size = 10) +
            # geom_nodetext_repel(aes(label = degree)) +
            scale_color_manual(values = colvec) +
            scale_fill_manual(values = colvec) +
            guides(color = "none", fill = "none", size = "none") +
            labs(caption = paste0("Durchmesser =", cur_g$graph_attr["gr_dia"],
                                  "\n",
                                  "Zentralisierung = ", sprintf("%.2f", round(cur_g$graph_attr["gr_centr"], 2)),
                                  "\n",
                                  "Dichte = ", sprintf("%.2f", round(cur_g$graph_attr["gr_dens"], 2)),
                                  "\n",
                                  "Transitivität = ", sprintf("%.2f", round(cur_g$graph_attr["gr_trans"], 2)),
                                  "\n",
                                  "Assortativität = ", sprintf("%.2f", round(cur_g$graph_attr["gr_ass"], 2))
            )
            ) +
            theme_blank() +
            theme(plot.caption = element_text(size = 12))
        
        
        if(input$show_nodelabs){
            cur_graph + 
                geom_nodetext(aes(label = paste0("Grad=", degree, 
                                                 "\n", 
                                                 "Zws=", sprintf("%.2f", round(between, 2)),
                                                 "\n",
                                                 "Eig=", sprintf("%.2f", round(eigen, 2)))),
                              hjust = 0,
                              nudge_x = 0.01, nudge_y = -0.01,
                              size = 3)
        } else{
            cur_graph
        }       
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
