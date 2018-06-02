library(readr)
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

library(visNetwork)
library(igraph)


hub_locations <- readRDS("Hub Locations.rds")

ui <- fluidPage(theme = shinytheme("flatly"),
                #shinythemes::themeSelector(),
  titlePanel("BikeTown Trips Network"),
  #mainPanel(
    navlistPanel(
      "Introduction",
      tabPanel("Bikeshare background",
               p('Bike-sharing systems are becoming more and more prevalent in cities in both the United States and worldwide. 
A variety of systems exist, but a common "docked" setup consists of hundreds (or thousands) of bikes deployed at one of tens (or hundreds) of docking stations, 
alternately known as hubs or stands. People can unlock one of these bikes and ride them around, then leave them locked at a hub or on the street 
(depending on their payment plan). Both bikes and hubs are equipped with sensors that send and receive real-time feed data which can be accessed through an API. 
When a user borrows a bike, information on trip start time, location, user id, payment plan type, duration, end time, end location, and 
distance traveled are stored in a database. Some bike-sharing systems have publicly released anonymized journey data with fields as listed above; 
Austwick et. al analyzed data from London, UK; Boston, MA; Denver, CO; Minneapolis, MN; and Washington, DC. 
Recently, BikeTown, the bike-sharing system in Portland, OR, released all anonymized trip information from their inception, July 2016, to March 2018. 
Portland is a "semi-dockless" system in that trips can start or end at one of over 100 hubs or by parking the bike on the street 
(though there is a fee for not returning the bike to a hub and therefore most trips are from hub to hub).')
),
      tabPanel("Bikeshare as a network",
               h3('Adjacency Matrix approach'),
               p('It is logical to think about or model docked or semi-dockless bike-sharing systems with a networks approach. 
Hubs are well-defined, stationary places at which trips start/end, and trip data presents information on the flow of travelers between them 
(i.e., the edges connecting two nodes). Some trips did not start or end at a hub and were therefore not included in this analysis. The simplest networks approach would be
to create a symmetric adjacency matrix where two hubs are connected if a trip took place between them. While this is is an okay start, it misses much of what makes bikesharing
interesting. Though easily represented in network form, starting from an adjacency matrix the nature of trips in bikeshare networks requires that the network representation 
needs to be spatial, directed, temporal, weighted, with self-loops and non-sparsity (Austwick et. al), and additionally is non-planar in that edges between nodes 
intersect or overlap (Barthelemey). More detail on these characteristics is given below'),
               h3('Spatial approach'),
               p('Trips are inherently spatial, taking place in two-dimensional Euclidean space with a defined start- and end-point, which may or may not be the same. The 
network topology is intrinsically tied to the topology of the city and the location of points of interest within the city. The spatial layout of the city 
(and by extension bike trips) can be considered with a core-periphery structure approach with densely core nodes and sparsely-connected periphery nodes (Rombach et al). 
This can be identified in the network visualization below.'),
               h3('Temporal approach'),
               p('The temporal aspect of trips is present in several ways: the time of when trips start and end is not uniform throughout the day, week, or time of year 
but instead shows interesting patterns; likewise, the trips themselves have a non-zero duration, of importance when optimizing bike placement to not run out of available bikes.
The majority of bikes are rented around 4-6PM, though the specific pattern of rides is different between weekdays and weekends or holidays; likewise, summer months see 
many more rides than the winter months do. Over time, the connectivity of the network increases as rides take place between hubs that had not been connected previously'),
               h3('Multiplex approach'),
               p('The users themselves have not yet been considered, but their attributes can have non-trivial effects on the network structure. There are many types of membership
available to BikeTown users. The dataset provided to the public only has two membership types: "subscriber" or "casual". These users differ in their ridership habits, both
in what time of day/week/year they ride, but also in the nodes they are likely to visit'),
               h3('Multigraph (directed graph) approach'),
               p('Given an appropriate amount of time for trips to occur, multiple trips will occur between hubs, and the flow of bikes between hubs has strong importance
for the operational aspect of supplying bikes. On an aggregated level, which can be useful when dealing with thousands or millions of trips, 
the flows between hubs can be intuitively seen as a weighted property: while each trip has a weight of only one, on a larger scale the directed edge weight between hubs 
is the sum of the trips between the start and end hub. Which hubs see lots of traffic and which do not is an interesting and useful characteristic of the system.')

),
      tabPanel("Materials",
               h3('References'),
               p('Barthelemy, Marc. "Spatial Networks." Physics Reports, vol. 499, no. 1, 2011, pp. 1-101.'),
               p('Rombach, Puck, et al. "Core-Periphery Structure in Networks (Revisited)." SIAM Review, vol. 59, no. 3, 2017, pp. 619-646.'),
               p('Expert, Paul, et al. "Uncovering Space-Independent Communities in Spatial Networks." 
                 Proceedings of the National Academy of Sciences of the United States of America, vol. 108, no. 19, 2011, pp. 7663-8.'),
               p('Zaltz Austwick, Martin, et al. "The Structure of Spatial Networks and Communities in Bicycle Sharing Systems." PLoS ONE, vol. 8, no. 9, 2013, p. e74685.'),
               h3('Dataset'),
               a(href="https://www.biketownpdx.com/system-data", "BikeTown System Data", target = "_blank"),
               h3('Graphical interpretation packages'),
               strong('shiny'), p(),
               strong('igraph'), p(),
               strong('visNetwork')
               ),
      "Network",
      #tabsetPanel("Network Visualiztion", type = "tabs",
        tabPanel("Network Properties",
                 h4("By default, the network shown is from the entire trips dataset. To dig deeper, filter by date or time."),
                 h3("Network properties: "),
                 p("Node size is proportional to the total degree of the node, while edge width is proportional to the out-degree. 
                   Edges are colored gray if they both nodes are in the same neighborhood (N, NE, NW, etc) and colored blue if they are not.")
                 ),
        tabPanel("Dynamic Network",
          sidebarLayout(
            sidebarPanel(width = 2, position = "left",
              dateRangeInput("tripDate", label = "Trip Date Range", start = "2016-07-01", end = Sys.Date()),
              #sliderInput("tripDate", "Choose Date Range:", 
              #            min = as.Date("2016-07-19"), max = Sys.Date(), 
              #            value = c(as.Date("2016-07-19"), as.Date("2016-07-19")),
              #            animate = TRUE),
              sliderInput("time", label = "Trip start time", min = 0, max = 23.5, step = 0.5, value = c(0,23.5)),
              radioButtons("weekend", "Weekend Trips", choices = c("Yes", "No", "Both"), selected = "Both")
            ),
            mainPanel(
              visNetworkOutput("network", height = "500", width = "800")
            )
        )),

        tabPanel("Summary Statistics", 
          tableOutput("avgDegree")),
      #),
      tabPanel("Degree Distributions",
        plotOutput("degreeDist"),
        plotOutput("components")),
      #tabPanel("Static Networks"),

      widths = c(2,10)
    )

  )

server <- function(input, output){

  network_data <- reactive({
    #trips <- readRDS("C:/Users/Konrad/Desktop/Intro to Networks/Term Project/All Trips.rds")
    trips <- readRDS("All Trips.rds")
    
    filt_trips <- trips %>%
      filter(StartDate >= input$tripDate[1] & StartDate <= input$tripDate[2] &
        StartTime >= input$time[1] * 60 * 60 & StartTime < input$time[2] * 60 * 60 & 
          !is.na(StartHub) & !is.na(EndHub))
    
    to_from_trips <- filt_trips %>%
      group_by(StartHub, EndHub) %>%
      count() %>%
      select(from = StartHub, to = EndHub, weight = n)
    
    return(to_from_trips)
  })
  
  
  
  network_graph <- reactive({
    #od_matrix <- network_od()
    trips_data <- network_data()

    early_graph <- igraph::graph_from_edgelist(cbind(trips_data$from, trips_data$to), directed = TRUE)
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
    vis_early$x$nodes$title = paste0(vis_early$x$nodes$id, "<br>", "In-degree: ", degree(early_graph, mode = "in"),
                                     "<br>", "Out-degree: ", degree(early_graph, mode = "out"))
    
    #vis_early$x$edges <- vis_early$x$edges %>%
    #  left_join(early_june_trips) %>%
    #  mutate(width = weight)
    #degree(early_graph,)
    
    vis_early$x$edges$width = vis_early$x$edges$weight
    
    vis_early$x$edges$start_part <- substr(vis_early$x$edges$from, start = 1, stop = 2)
    vis_early$x$edges$end_part <- substr(vis_early$x$edges$to, start = 1, stop = 2)
    
    vis_early$x$edges$color <- NA
    vis_early$x$edges <- vis_early$x$edges %>%
      mutate(color = ifelse(start_part != end_part, "#b6bcc6", "blue"))
    #vis_early$x$edges$color.highlight.background <- "red"
    
    return(vis_early)
    
  })
  
  
  
  network_summary <- reactive({
    
    trips_data <- network_data()
    early_graph <- network_graph()
    #vis_early <- network_vis_graph()
    #od_matrix <- network_od()
    #edge_info <- vis_early$x$edges

    avg_connected_nodes <- trips_data %>%
      group_by(from) %>%
      count() %>%
      ungroup() %>%
      summarise(Mean = mean(n))


    density = ecount(early_graph)/(vcount(early_graph)^2)
    
    summary_stats <- tibble(trips = sum(trips_data$weight),
                            numNodes = vcount(early_graph),
                            numEdges = ecount(early_graph),
                            AvgEdgeWeight = mean(trips_data$weight),
                            AvgDegree = avg_connected_nodes$Mean,
                            AvgPathLength =  average.path.length(early_graph, directed = TRUE),
                            #Diameter = diameter(early_graph),
                           Density = density,
                           Clustering = transitivity(early_graph))
    
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
      visOptions(highlightNearest = TRUE) %>%
      visInteraction(dragNodes = FALSE, hover = TRUE, keyboard = TRUE) 
      #visEvents(click = "function(nodes){
      #            Shiny.onInputChange('click', nodes.nodes[0]);
      #          ;}"
      #)
      #visEvents(selectNode = "function(properties) {
      #alert('selected nodes ' + this.body.data.nodes.get(properties.nodes[0]).id);}")
    #visEvents(selectNode = "function myFunction() {
    #  var popup = document.getElementById('myPopup');
    #  popup.classList.toggle('show');}")
    
    })
  
  output$degreeDist <- renderPlot({
    early_graph <- network_graph()
    
    my_data <- data.frame(Degree = degree(early_graph))
    ggplot(my_data, aes(Degree)) +
      geom_histogram() + 
      theme_minimal()
  })
  
  output$components <- renderPlot({
    early_graph <- network_graph()
    
    my_data <- data.frame(Var = c("Components", "Nodes"), Val= c(components(early_graph)$no,
                                                                 vcount(early_graph)))
    
    ggplot(my_data, aes(Var, Val)) + 
      geom_bar(stat = "identity") + 
      ylab("Count") + 
      xlab("Variable") + 
      theme_minimal()
  })
  
    
  
}

shinyApp(ui = ui, server = server)
