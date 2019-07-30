setwd("~/projects/Social Network Analysis/asoiaf-master/data")

# Libraries
library(igraph)
library(dplyr)
library(ggplot2)

# -- * 1- 'A Song of Ice and Fire Network * --

# Import data
data = read.csv("asoiaf-all-edges.csv");class(data); head(data,10)
# Select useful columns
data = data[,c("Source","Target","weight")]

# Create an undirected weighted graph
# -> Printing igraph object its possible to see that 
# -> # vertices  = 796
# -> # edges = 2823
ig = graph_from_data_frame(data, directed = F, vertices = NULL); ig

# Check if is a simple graph:
# -> Simple graphs are graphs which do not contain loop and multiple edges.
is_simple(ig) 

# -- * 2- Network Properties * --

## Number of vertices
gorder(ig)
## Number of edges
gsize(ig) 
## Diameter of the graph 
diameter(ig, directed=F)
## Number of triangles
sum(count_triangles(ig, vids = V(ig)))
## The top-10 characters of the network as far as their degree is concerned
sort(degree(ig, mode = c("all")), decreasing = T)[1:10]
## The top-10 characters of the network as far as their weighted degree is concerned
sort(strength(ig, vids = V(ig), mode = c("all"),loops = F), decreasing = T)[1:10]                 

# -- * 3- Subgraph * --

## Plot the entire network 

set.seed(1)
# Set layout (to understand better)
l  = layout.kamada.kawai(ig)
l2 = layout.fruchterman.reingold(ig)
l3 = layout.lgl(ig)

# Plot
plot(ig,
     vertex.label = NA,
     vertex.color = "yellow",
     vertex.frame.color = "red",
     vertex.shape="sphere", 
     vertex.size=1.7, 
     edge.color  = "black",
     edge.width = 0.5,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.lty = "twodash", 
     edge.curved = 0.05,
     frame = T,
     main  = "Entire Network",
     sub   = "A Song of Ice and Fire"
     )


# 3d
coordsFR <- layout.fruchterman.reingold(ig, dim=3)
rglplot(ig,
     vertex.label = NA,
     vertex.color = "yellow",
     vertex.frame.color = "red",
     vertex.shape="sphere", 
     vertex.size=2, 
     edge.color  = "grey",
     edge.width = 0.5,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.lty = "twodash", 
     edge.curved = 0.1,
     frame = T,
     main  = "Entire Network",
     sub   = "A Song of Ice and Fire",
        layout = coordsFR)

## Plot the subgraph (take vertices > 10 connection)

# Store degree as a variable
degree = degree(ig, mode = c("all"))
# Subset
list_degree_gt_10 =  degree[degree > 10] ; length(list_degree_gt_10); tail(list_degree_gt_10)
# Take the vertices name
vertices = V(ig)$name

# Obtain sub_graph selecting the vertices in list_degree_gt_10 
#  -> vertices = 128
#  -> edges = 1023
sub_graph<-induced.subgraph(ig, which(vertices %in% names(list_degree_gt_10)))

# Plot 
set.seed(123)
plot(sub_graph,
     vertex.label = NA,
     vertex.color = "yellow",
     vertex.frame.color = "red",
     vertex.shape="sphere", 
     vertex.size=2.5, 
     edge.color  = "black",
     edge.width = 0.5,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.lty = "twodash", 
     edge.curved = 0.1,
     frame = T,
     main  = "Subgraph - Vertex > 10 connection",
     sub   = "A Song of Ice and Fire"
     )

## Edge Density 
# -> The density of a graph is the ratio of the number
# -> of edges and the number of possible edges.
# -> The subnetwork is more compact ....

# Entire graph 
edge_density(ig)
# Check maths 
total_edges = vcount(ig)*(vcount(ig) - 1) / 2
edge_density =  ecount(ig) / total_edges; edge_density

# Subnetwork 
edge_density(sub_graph)
# Check maths 
total_edges_sub = vcount(sub_graph)*(vcount(sub_graph) - 1) / 2
edge_density_sub =  ecount(sub_graph) / total_edges_sub; edge_density_sub


# -- * 4 Centrality * -- 

# Top15 Nodes according to betweeness 
between_top15 = sort(betweenness(ig, directed = F), decreasing = T)[1:15]; between_top15

# This calculates closeness
close_top15 = sort(closeness(ig), decreasing = T)[1:15]; close_top15 

# -- * 5 Page Rank * 

# Calculate page_rank 
page_rank <- page.rank(ig, directed = FALSE)

# Create a df with all characters 
page_rank_centrality <- data.frame(name = names(page_rank$vector),
                                   page_rank = page_rank$vector) %>%
                                   mutate(name = as.character(name))


# Create a list of Colors
cols <- c("azure","azure1","azure2","aquamarine","aquamarine2","aquamarine3",
          "cyan","cyan1","cyan2","cyan3",
          "coral","coral1","coral2","coral3","coral4",
          "darkolivegreen1","darkolivegreen2","darkolivegreen3","orange","red","darkorchid1"
          )  

# Pass the list to the quantile intervals 
k <- cols[findInterval(page_rank_centrality$page_rank,
                       quantile(page_rank_centrality$page_rank,probs=seq(0,1, by=0.05) ),
                       rightmost.closed=T, all.inside=F) + 1]
# Add the column 
page_rank_centrality$color = k

# Take the top15
to_plot = page_rank_centrality %>%
                arrange(-page_rank) %>%
                .[1:15, ]
to_plot


## --> Different tests in plotting 

# Final Report 

# 1 . Plot spheres  BY SIZE 
plot(ig,
     vertex.label = NA ,
     vertex.color = k,
     vertex.frame.color = "white",
     vertex.shape="sphere", 
     vertex.size=(page_rank_centrality$page_rank)*150, 
     edge.color  = "peachpuff",
     edge.width = 0.5,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.lty = "twodash", 
     edge.curved = 0.1,
     frame = T,
     main  = "Importance by Page Rank measure",
     sub   = "A Song of Ice and Fire"
     )


# 2. Plot spheres BY COLOR 

# --> ! TODO : I should fix later
# --> ! MAPPING COLORS IS NOT CORRECT
# --> ! ADD a legend

# Create a column with a Size of nodes associates to quantile intervals
size = seq(0.1, 2, by = 0.1)

page_rank_centrality$size = cut(page_rank_centrality$page_rank,
                                    breaks=quantile(page_rank_centrality$page_rank, probs=seq(0,1, by=0.05), na.rm=TRUE),
                                    labels(size))

# Transform in numeric and the assign a reasonable size for plotting
page_rank_centrality$size = (as.numeric(as.character(page_rank_centrality$size)) / 10) + 0.25

# Remove one Na for the sake of visualization : Aron-Santagar
page_rank_centrality = na.omit(page_rank_centrality); nrow(page_rank_centrality)

plot (ig,
     vertex.label = NA ,
     vertex.color = page_rank_centrality$color,
     vertex.frame.color = "white",
     vertex.shape="sphere", 
     vertex.size= page_rank_centrality$size, 
     edge.color  = "peachpuff",
     edge.width = 0.5,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.lty = "twodash", 
     edge.curved = 0.1,
     frame = T,
     main  = "Importance by Page Rank measure",
     sub   = "A Song of Ice and Fire"
     )

# Add a legend
legend(1, 95, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

# Plot 1 (circles - white)
plot(ig,
     vertex.label = NA,
     vertex.color = k,
     vertex.frame.color = "white",
     vertex.shape="circle", 
     vertex.size=(page_rank_centrality$page_rank)*550, 
     edge.color  = "black",
     edge.width = 0.5,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.lty = "solid", 
     edge.curved = 0.1,
     frame = T,
     main  = "Importance by Page Rank measure",
     sub   = "A Song of Ice and Fire"
     )

# Plot 2 - 3d
coordsFR <- layout.fruchterman.reingold(ig, dim=3)
rglplot(ig, layout = coordsFR,
     vertex.label = NA,
     vertex.color = k,
     vertex.frame.color = "white",
     vertex.shape="circle", 
     vertex.size=(page_rank_centrality$page_rank)*550, 
     edge.color  = "white",
     edge.width = 0.5,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.lty = "solid", 
     edge.curved = 0.1,
     frame = T,
     main  = "Importance by Page Rank measure",
     sub   = "A Song of Ice and Fire"
     )



### ---> Additional plot <---

to_plot_2 = page_rank_centrality %>%
                arrange(-page_rank) %>%
                .[1:10, ]

top10 = induced_subgraph(ig, vids = to_plot_2$name)

plot(top15,
     vertex.color = "deeppink",   
     vertex.label = to_plot_2$name,
     vertex.label.family = "Times",
     vertex.label.font = 4,
     vertex.label.cex = 0.9,
     vertex.label.dist = 4,
     vertex.frame.color = "white",
     vertex.label.degree =0.5,
     vertex.shape="sphere", 
     vertex.size=(to_plot_2$page_rank)*1500, 
     edge.color  = "black",
     edge.width = 0.5,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.lty = "solid", 
     edge.curved = 0.1,
     frame = T,
     main  = "Subgraph - Vertex > 10 connection",
     sub   = "A Song of Ice and Fire"
     )
  

## Plot top5 with labels 
#  plot(ig,
#      vertex.label = ifelse(page_rank_centrality$page_rank > 0.01979001, page_rank_centrality$name, NA),
#      vertex.color = k ,
#      vertex.frame.color = "white",
#      vertex.label.family = "Times",
#      vertex.label.font = 2,
#      vertex.label.cex = 1,
#      vertex.label.dist = 0.5,
#      vertex.frame.color = "white",
#      vertex.label.degree =0,
#      vertex.shape="sphere", 
#      vertex.size=(page_rank_centrality$page_rank)*550, 
#      edge.color  = "yellow",
#      edge.width = 0.5,
#      edge.arrow.size = 0.5,
#      edge.arrow.width = 0.5,
#      edge.lty = "solid", 
#      edge.curved = 0.1,
#      frame = T,
#      main  = "Importance by Page Rank measure",
#      sub   = "A Song of Ice and Fire"
#      )
