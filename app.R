library(shiny)
library(dplyr)
library(igraph)
library(visNetwork)

early_june_trips <- readRDS("C:/Users/Konrad/Desktop/Intro to Networks/Term Project/Early June Trips.rds")

early_june_trips <- early_june_trips %>%
  select(from = StartHub, to = EndHub, weight = n)

early_june_trips$from <- as.character(early_june_trips$from)
early_june_trips$to <- as.character(early_june_trips$to)
hub_locations <- readRDS("C:/Users/Konrad/Desktop/Intro to Networks/Term Project/Hub Locations.rds")

ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel("Network and Stats",
        visNetworkOutput("network", height = "auto", width = "auto"),
        tableOutput("avgDegree")
      ),
      tabPanel("Degree Distributions",
        plotOutput("degreeDist"))
    )
  )

  )

server <- function(input, output){

  network_graph <- reactive({
    #od_matrix <- network_od()
   

    early_graph <- graph_from_edgelist(cbind(early_june_trips$from, early_june_trips$to), directed = TRUE)
    #early_graph <- igraph::graph_from_edgelist(cbind(early_june_trips$from, early_june_trips$to), directed = TRUE)

    #early_graph <- igraph::graph_from_adjacency_matrix(as.matrix(od_matrix), weighted = TRUE, mode = "directed")

    return(early_graph)
  })
  
  network_vis_graph <- reactive({
    early_graph <- network_graph()

    vis_early <- visIgraph(early_graph)

    #vis_early$x$nodes$id <- V(early_graph)$name
    #vis_early$x$nodes$label <- V(early_graph)$name
    
    vis_early$x$nodes <- vis_early$x$nodes %>%
      left_join(hub_locations, by = c("id" ="StartHub"))
    
    vis_early$x$nodes$value <- sqrt(degree(early_graph))
    vis_early$x$nodes$color <- "#ff8d00"
    
    #vis_early$x$edges <- vis_early$x$edges %>%
    #  left_join(early_june_trips) %>%
    #  mutate(width = weight)
    
    
    vis_early$x$edges$width = vis_early$x$edges$weight
    
    vis_early$x$edges$start_part <- substr(vis_early$x$edges$from, start = 1, stop = 2)
    vis_early$x$edges$end_part <- substr(vis_early$x$edges$to, start = 1, stop = 2)
    
    vis_early$x$edges$color <- NA
    vis_early$x$edges <- vis_early$x$edges %>%
      mutate(color = ifelse(start_part != end_part, "#b6bcc6", "blue"))
    
    return(vis_early)
    
  })
  
  
  
  network_summary <- reactive({
    early_graph <- network_graph()
    #vis_early <- network_vis_graph()
    #od_matrix <- network_od()
    #edge_info <- vis_early$x$edges

    avg_connected_nodes <- early_june_trips %>%
      group_by(from) %>%
      count() %>%
      ungroup() %>%
      summarise(Mean = mean(n))


    density = ecount(early_graph)/(vcount(early_graph)^2)
    
    summary_stats <- tibble(numNodes = vcount(early_graph),
                            numEdges = ecount(early_graph),
                            AvgEdgeWeight = mean(early_june_trips$weight),
                            AvgDegree = avg_connected_nodes$Mean,
                            AvgPathLength =  average.path.length(early_graph, directed = TRUE),
                            #Diameter = diameter(early_graph),
                           Density = density)
    
    average.path.length(early_graph, directed = TRUE)
    return(summary_stats)
  })
  
  output$avgDegree <- renderTable({
    network_summary()
  })
  
  output$network <- renderVisNetwork({
    
    vis_early <- network_vis_graph()
    
    vis_early %>%
      visIgraphLayout(layout = "layout.norm", layoutMatrix = cbind(vis_early$x$nodes$Lon, -vis_early$x$nodes$Lat)) %>%
      visNodes(color = list(background = "#ff8d00", highlight = "black")) %>%
      #visEdges(color = "#b6bcc6", arrows = "none") %>%
      visEdges(arrows = "none") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  })
}

shinyApp(ui = ui, server = server)
