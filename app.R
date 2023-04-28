#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
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
    titlePanel("Illustration of network graphs"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            selectInput("graph",
                        "Graph type",
                        choices = list("Random (Erdös Renyi)",
                                       "Random (Small World)",
                                       "Ring lattice",
                                       "Ring",
                                       "Preferential attachment",
                                       "Fully connected"),
                        multiple = FALSE),
            
            sliderInput("n",
                        "n(nodes):",
                        min = 2,
                        max = 50,
                        step = 1,
                        value = 15),
            
            sliderInput("p_rewire",
                        "p(rewire):",
                        min = 0,
                        max = 1,
                        step = 0.05,
                        value = 0),
            
            sliderInput("p_a",
                        "Proportion petrol:",
                        min = 0,
                        max = 1,
                        step = 0.1,
                        value = 0),
            
            checkboxInput("show_nodelabs",
                          "Knotenlabels zeigen")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Pre-calculated graphs:
        # Regular graph:
        graph_reg <- make_chordal_ring(15,
                                       matrix(rep(3, 3), nr = 1))
        
        # Preferrential attachment:
        graph_pa <- sample_pa(n = 30, power = 2, directed = FALSE)
        
        g_rewire <- graph_pa
        

            
            # Development area! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # pcol_a <- 0.5  # probability of a-nodes.
            # n <- 15
            # 
            # n_a <- ceiling(n * pcol_a)
            # 
            # # Determine colors randomly:
            # rancols <- c(rep("a", n_a), rep("b", n - n_a))
            # 
            # V(g_rewire)$color <- sample(rancols)
            # colvec <- c(a = unname(Petrol), b = unname(Peach))
            # 
            # gr_ass <- assortativity(g_rewire, types1 = as.numeric(V(g_rewire)$color == "a"))
            # 
            # betweenness(g_rewire) 
            # 
            # V(g_rewire)$degree <- degree(g_rewire)
            # 
            # 
            # ggplot(ggnetwork(g_rewire, layout = layout.circle(g_rewire)),
            #        aes(x = x, y = y, xend = xend, yend = yend)) +
            #     geom_edges(color = "darkgrey", size = 1
            #                # arrow = arrow(length = unit(8, "pt"), type = "closed")
            #     ) +
            #     geom_nodes(aes(color = color, fill = color), size = 10) +
            #     # geom_nodetext_repel(aes(label = degree)) +
            #     geom_nodetext(aes(label = paste0("Grad=", degree)),
            #                   nudge_x = 0.05, nudge_y = -0.01) +
            #     scale_color_manual(values = colvec) +
            #     scale_fill_manual(values = colvec) +
            #     guides(color = "none", fill = "none") +
            #     labs(caption = paste0("Assortativität = ", sprintf("%.2f", round(gr_ass, 2)),
            #                           "\n",
            #                           "Dichte = ", sprintf("%.2f", round(edge_density(g_rewire))))
            #          ) +
            #     theme_blank() +
            #     theme(plot.caption = element_text(size = 12))
            # eof. dev ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # Graph creating and rewiring:

                     
         cur_graph <- reactive({
             
             n_nodes <- input$n
             
             switch(input$graph,
                     "Random (Erdös Renyi)" = erdos.renyi.game(n = n_nodes, 
                                                               p.or.m = input$p_rewire, 
                                                               type = "gnp"),
                     "Random (Small World)" = watts.strogatz.game(1, size = n_nodes, 
                                                                  nei = 3, 
                                                                  p = input$p_rewire, loops = FALSE, multiple = FALSE),
                     # TODO: Sensibel defaults for random graphs or make flexible!
                     "Ring lattice" = make_chordal_ring(n = 15,  # input$n,
                                                        matrix(rep(3, 3), nr = 1)),
                     "Ring" = make_ring(n = n_nodes),
                     "Preferential attachment" = sample_pa(n = n_nodes, 
                                                           power = 3, directed = FALSE),
                     "Fully connected" = make_full_graph(n = n_nodes))
         
         # TODO: Add Judd-type layout (3 fully conencted graphs)?
         })
         
         
         g_rewire <- reactive({
             cur_graph <- cur_graph()
             
         if(!input$graph == "Random (Erdös Renyi)"){
             # No additional rewiring:
              rewire(cur_graph, with = each_edge(prob = input$p_rewire))
         } else {
             cur_graph
             }
         })
         
         # print(g_rewire)

                
            
    output$distPlot <- renderPlot({
        
        # Select plot-type:
        # tst <- "b"
        # switch(tst,
        #        a = "Hello",
        #        b = "No")
            
            # TODO: Make the ring lattice flexible!
        

        g_rewire <- g_rewire()  # get rewired graph.
        
        # Determine layout:
        g_lay <- switch(input$graph,
                        "Preferential attachment" = layout.auto(g_rewire),
                        layout.circle(g_rewire)
        )
        
        
        # TODO: Make conditional
        V(g_rewire)$label <- round(eigen_centrality(g_rewire)$vector, 2)
        V(g_rewire)$label <- round(betweenness(g_rewire), 2)
        
        
        # NODE FEATUERS: --------------------------------
            n_nodes <- length(V(g_rewire))
            # Determine node types: -------------------------
                
                n_a <- ceiling(n_nodes * input$p_a)  # get requested type a.
            
                # Determine colors randomly:
                rancols <- c(rep("a", n_a), rep("b", n_nodes - n_a))
                
                V(g_rewire)$color <- sample(rancols)
                colvec <- c(a = unname(Petrol), b = unname(Peach))
                
            # Centrality on each node: ----------------------
                V(g_rewire)$degree <- degree(g_rewire)
                V(g_rewire)$between <- betweenness(g_rewire)
                V(g_rewire)$eigen <- eigen_centrality(g_rewire)$vector
                
            
        # GRAPH-LEVEL FAETUERS: -------------------------
            # Determine assortment: -------------------------
                gr_ass <- assortativity(g_rewire, 
                                                types1 = as.numeric(V(g_rewire)$color == "a"),
                                                directed = TRUE)
        
            
                gr_dia <- diameter(g_rewire)  # diameter.
                
                gr_dens <- edge_density(g_rewire)  # density.
                
                gr_centr <- centralization.degree(g_rewire)$centralization  # centralization.

            
        
        # Plot: -----------------------------------------
        # plot(g_rewire, layout = g_lay,
        #      vertex.label = V(g_rewire)$label,
        #      vertex.size = 25)
        
            cur_graph <- ggplot(ggnetwork(g_rewire, layout = g_lay),
                   aes(x = x, y = y, xend = xend, yend = yend),
                   curvature = 15) +
                geom_edges(color = "darkgrey", size = 1
                           # arrow = arrow(length = unit(8, "pt"), type = "closed")
                ) +
                geom_nodes(aes(color = color, fill = color), size = 10) +
                # geom_nodetext_repel(aes(label = degree)) +
                scale_color_manual(values = colvec) +
                scale_fill_manual(values = colvec) +
                guides(color = "none", fill = "none") +
                    labs(caption = paste0("Durchmesser =", gr_dia,
                                          "\n",
                                          "Zentralisierung = ", sprintf("%.2f", round(gr_centr, 2)),
                                          "\n",
                                          "Dichte = ", sprintf("%.2f", round(gr_dens, 2)),
                                          "\n",
                                          "Assortativität = ", sprintf("%.2f", round(gr_ass, 2))
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

# Can be passed on to users to run directly from github:
# runGitHub( "networkVisualizations", "nigradwohl")
