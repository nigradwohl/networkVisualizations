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
            
            
            checkboxInput("show_nodelabs",
                          "Knotenlabels zeigen")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("netPlot",
                       click = "netPlot_click")
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
    

    
    # Create the "fixed" part of the data: ---------------------------
    gcur <- reactive({
        
        # Create graph without connections:
            g <- erdos.renyi.game(n = input$n, 
                             p.or.m = 0, 
                             type = "gnp",
                             directed = TRUE)
        
            # Determine layout:
            g_lay <- layout.circle(g)
        
            # If the current number of nodes changed:
                if(cur_n_nodes != input$n){
                    sel_nodes <<- c()
                    cur_edges <<- c()
                }
            
            # Save the current number of nodes:
                cur_n_nodes <<- input$n
        
        # NODE FEATUERS: --------------------------------
            n_nodes <- length(V(g))
            # Determine node types: -------------------------
            
            n_a <- ceiling(n_nodes * 0.5)  # get requested type a.
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
                                  addDist = FALSE)

            # print(pts)

            # gcur()

            # Save selected nodes globally:
                sel_nodes <<- c(sel_nodes, pts$name)

                if(length(sel_nodes) == 2){
                    
                    
                    
                    # Add the edge to the (persistent) list!
                        cur_edges <<- c(cur_edges, sel_nodes)
                        
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
    
    # g_update <- reactive({
    #     # Selection process:
    #     gnet <- gcur()  # intermediate network.
    #     
    #     
    #     # pts <- nearPoints(as.data.frame(gnet), 
    #     #            input$netPlot_click, 
    #     #            addDist = FALSE)
    #     
    #     # print(pts)
    #     
    #     # Determine layout:
    #     g_lay <- layout.circle(gnet)
    #     
    #     
    #     # TODO: Current issue is that the whle function is called again!
    #     
    #     # curnet <<- ggnetwork(g, layout = g_lay)  # global assignment.
    #     pts <- nearPoints(as.data.frame(ggnetwork(gnet, g_lay)), 
    #                       input$netPlot_click, 
    #                       addDist = FALSE)
    #     
    #     print(pts)
    #     
    #     # gcur()
    #     
    #     # Save selected nodes globally:
    #     sel_nodes <<- c(sel_nodes, pts$name)
    #     
    #     if(length(sel_nodes) == 2){
    #         
    #         gnet <- add.edges(gnet, sel_nodes)
    #         
    #         # Nullify:
    #         sel_nodes <<- c()
    #     }
    #     
    #     # Render selected nodes:
    #     V(gnet)$selected <- ifelse(V(gnet)$name %in% sel_nodes, TRUE, FALSE)
    #     
    #     return(ggnetwork(gnet, layout = g_lay))
    # })
    
    # Functions to (a) select node and (b) create links to other nodes:
    # TODO
    # For collecting information about the plot:
    # https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html
    # https://stackoverflow.com/questions/39916465/click-events-for-visnetwork-with-shiny
    # Additional js?
    # https://stackoverflow.com/questions/32057164/adding-hyperlinks-to-shiny-plots
    
    # https://stackoverflow.com/questions/46354853/using-linked-brushing-with-a-network-graph-from-ggraph-in-a-shiny-app
    
    
    
    # TODO: Save node info in current data!
    
    # Listen to info:
    # observeEvent(input$netPlot_click,
    #              {
    #                  pts <- nearPoints(as.data.frame(gcur()), 
    #                                    input$netPlot_click, 
    #                                    addDist = FALSE)
    #                  
    #                  print(pts)
    #                  
    #                  # gcur()
    #                  
    #                  sel_nodes <<- c(sel_nodes, pts$name)
    #                  print(sel_nodes)
    # 
    #              })
    
    # Plot output:
    output$netPlot <- renderPlot({
        
        colvec <- c(a = unname(Petrol), b = unname(Peach))

        # gcur <- gcur()  # curnet  # non-reactive... # gcur()  
            gcur <- gcur()  # get the igraph object.
        
            # print(gcur)
        
        # Determine layout:
            g_lay <- layout.circle(gcur)
        
        
        
        # Centrality on each node: ----------------------
            V(gcur)$degree <- degree(gcur)
            V(gcur)$between <- betweenness(gcur)
            V(gcur)$eigen <- eigen_centrality(gcur)$vector
        
        
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
        
            cur_g <- ggnetwork(gcur, layout = g_lay,
                               arrow.gap = 0.03)
            
        # Plot: ------------------------------
            cur_graph <- ggplot(cur_g,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   curvature = 15) +
                geom_edges(color = "darkgrey", size = 1,
                           arrow = arrow(length = unit(8, "pt"), type = "closed",
                                         )
                ) +
                geom_nodes(data = cur_g[cur_g$selected,],
                           color = "black", fill = "black", size = 15) +
                geom_nodes(aes(color = color, fill = color), size = 10) +
                # geom_nodetext_repel(aes(label = degree)) +
                scale_color_manual(values = colvec) +
                scale_fill_manual(values = colvec) +
                guides(color = "none", fill = "none", size = "none") +
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
