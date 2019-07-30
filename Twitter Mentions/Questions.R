setwd("~/projects/Social Network Analysis/Hmw2")

library(data.table)
library(dplyr)
library(reshape2)
library(tidyr)
library(igraph)
library(intergraph)
library(ggnetwork)
library(GGally)
library(ggiraph)
library(networkD3)
library(HiveR)

# READ THE FILES

df1 = read.csv("July_1.csv", sep = ";" )
df2 = read.csv("July_2.csv", sep = ";" )
df3 = read.csv("July_3.csv", sep = ";" )
df4 = read.csv("July_4.csv", sep = ";" )
df5 = read.csv("July_5.csv", sep = ";" )

# IGRAPH
ig1 <- graph_from_data_frame(df1, directed = T); is.weighted(ig1) # DNW- 479640 525932
ig2 <- graph_from_data_frame(df2, directed = T); is.weighted(ig2)
ig3 <- graph_from_data_frame(df3, directed = T); is.weighted(ig3)
ig4 <- graph_from_data_frame(df4, directed = T); is.weighted(ig4)
ig5 <- graph_from_data_frame(df5, directed = T); is.weighted(ig5)

# ---------------------------
## 2 AVERAGE DEGREE OVER TIME

# Number of vertices over 5 days
v1 = gorder(ig1); v1
v2 = gorder(ig2); v2
v3 = gorder(ig3); v3
v4 = gorder(ig4); v4
v5 = gorder(ig5); v5
vertices = c(v1,v2,v3,v4,v5)

# Number of edges 
e1 = gsize(ig1); e1
e2 = gsize(ig2); e2
e3 = gsize(ig3); e3
e4 = gsize(ig4); e4
e5 = gsize(ig5); e5
edges = c(e1,e2,e3,e4,e5)

# Diameter of the graph 
d1 = diameter(ig1, directed=T, weights= E(ig1)$weights); d1
d2 = diameter(ig2, directed=T, weights= E(ig2)$weights); d2
d3 = diameter(ig3, directed=T, weights= E(ig3)$weights); d3
d4 = diameter(ig4, directed=T, weights= E(ig4)$weights); d4
d5 = diameter(ig5, directed=T, weights= E(ig5)$weights); d5
diameter = c(d1,d2,d3,d4,d5)

# Calculate the "in" degree 
avg_in_deg1 <- mean(degree(ig1, mode="in")); avg_in_deg1
avg_in_deg2 <- mean(degree(ig2, mode="in")); avg_in_deg2
avg_in_deg3 <- mean(degree(ig3, mode="in")); avg_in_deg3
avg_in_deg4 <- mean(degree(ig4, mode="in")); avg_in_deg4
avg_in_deg5 <- mean(degree(ig5, mode="in")); avg_in_deg5

indegree = c(avg_in_deg1,avg_in_deg2,avg_in_deg3, avg_in_deg4, avg_in_deg5)

# ... same for "out" degrees
avg_out_deg1 <- mean(degree(ig1, mode="out")); avg_out_deg1
avg_out_deg2 <- mean(degree(ig2, mode="out")); avg_out_deg2
avg_out_deg3 <- mean(degree(ig3, mode="out")); avg_out_deg3
avg_out_deg4 <- mean(degree(ig4, mode="out")); avg_out_deg4
avg_out_deg5 <- mean(degree(ig5, mode="out")); avg_out_deg5

outdegree = c(avg_out_deg1,avg_out_deg2,avg_out_deg3, avg_out_deg4, avg_out_deg5)

# The sum of total in-degree should be equal 
# to the sum of total out-degree.
in_deg1 <-degree(ig1, mode="in")
out_deg1 <-degree(ig1, mode="out")

sum(in_deg1) == sum(out_deg1)

# PLOT 
library(highcharter)
# Set x-axis
days = c("July_1","July_2","July_3","July_4","July_5")

# Vertice - Edges 
plot1 <- highchart() %>% 
  hc_xAxis(categories = days) %>% 
  hc_add_series(name = " Vertices", 
                data =  vertices ) %>%
  hc_add_series(name = "Edges", 
                data =  edges ) %>%
  hc_title(text = "Number of Vertices / Edges: 1 July 2009 - 5 July 2009",
           margin = 20, 
           align = "left",
           style = list(color = "steelblue")) %>% 
  hc_subtitle(text = "Twitter Data",
              align = "left",
              style = list(color = "#2b908f", 
                           fontWeight = "bold")) %>% 
  hc_credits(enabled = TRUE, # add credits
             text = "Social Network Project",
             href = "Highcharter library") %>% 
  hc_legend(align = "left", 
            verticalAlign = "top",
            layout = "vertical", 
            x = 0, 
            y = 100) %>%
  hc_tooltip(crosshairs = TRUE, 
             backgroundColor = "#FCFFC5",
             shared = TRUE, 
             borderWidth = 4) %>% 
  hc_exporting(enabled = TRUE)

# Indegree - Outdegree
library("viridisLite")
cols <- viridis(2)

plot2 <- highchart() %>% 
  hc_xAxis(categories = days) %>% 
  hc_add_series(name = "In-Degree", 
                data =  indegree ) %>%
  hc_add_series(name = "Out-Degree", 
                data =  outdegree ) %>%
  hc_colors(cols) %>% 
  hc_title(text = "In-degree & Out-degree measure : 1 July 2009 - 5 July 2009",
           margin = 20, 
           align = "left",
           style = list(color = "steelblue")) %>% 
  hc_subtitle(text = "Twitter Data",
              align = "left",
              style = list(color = "#2b908f", 
                           fontWeight = "bold")) %>% 
  hc_credits(enabled = TRUE, # add credits
             text = "Social Network Project",
             href = "Highcharter library") %>% 
  hc_legend(align = "left", 
            verticalAlign = "top",
            layout = "vertical", 
            x = 0, 
            y = 100) %>%
  hc_tooltip(crosshairs = TRUE, 
             backgroundColor = "#FCFFC5",
             shared = TRUE, 
             borderWidth = 4) %>% 
  hc_exporting(enabled = TRUE)

# Diameter 
plot3 <- highchart() %>% 
  hc_xAxis(categories = days) %>% 
  hc_add_series(name = "Diameter", 
                data =  diameter ) %>%
  hc_colors(cols) %>% 
  hc_title(text = "In-degree & Out-degree measure : 1 July 2009 - 5 July 2009",
           margin = 20, 
           align = "left",
           style = list(color = "steelblue")) %>% 
  hc_subtitle(text = "Twitter Data",
              align = "left",
              style = list(color = "#2b908f", 
                           fontWeight = "bold")) %>% 
  hc_credits(enabled = TRUE, # add credits
             text = "Social Network Project",
             href = "Highcharter library") %>% 
  hc_legend(align = "left", 
            verticalAlign = "top",
            layout = "vertical", 
            x = 0, 
            y = 100) %>%
  hc_tooltip(crosshairs = TRUE, 
             backgroundColor = "#FCFFC5",
             shared = TRUE, 
             borderWidth = 4) %>% 
  hc_exporting(enabled = TRUE)

# Display
plot1
plot2
plot3


# ---------------------------
## 3 IMPORTANT NODES

top10_in_deg1  = sort(degree(ig1, mode="in"), decreasing = T)[1:10]
top10_in_deg2  = sort(degree(ig2, mode="in"), decreasing = T)[1:10]
top10_in_deg3  = sort(degree(ig3, mode="in"), decreasing = T)[1:10]
top10_in_deg4  = sort(degree(ig4, mode="in"), decreasing = T)[1:10]
top10_in_deg5  = sort(degree(ig5, mode="in"), decreasing = T)[1:10]

top10_out_deg1 = sort(degree(ig1, mode="out"), decreasing = T)[1:10]
top10_out_deg2 = sort(degree(ig2, mode="out"), decreasing = T)[1:10]
top10_out_deg3 = sort(degree(ig3, mode="out"), decreasing = T)[1:10]
top10_out_deg4 = sort(degree(ig4, mode="out"), decreasing = T)[1:10]
top10_out_deg5 = sort(degree(ig5, mode="out"), decreasing = T)[1:10]

top10_page_rank1 <- sort(page.rank(ig1, directed = T)$vector, decreasing = T)[1:10]
top10_page_rank2 <- sort(page.rank(ig2, directed = T)$vector, decreasing = T)[1:10]
top10_page_rank3 <- sort(page.rank(ig3, directed = T)$vector, decreasing = T)[1:10]
top10_page_rank4 <- sort(page.rank(ig4, directed = T)$vector, decreasing = T)[1:10]
top10_page_rank5 <- sort(page.rank(ig5, directed = T)$vector, decreasing = T)[1:10]

# Create a dataframe indegree
user_in_1 = names(top10_in_deg1)
user_in_2 = names(top10_in_deg2)
user_in_3 = names(top10_in_deg3)
user_in_4 = names(top10_in_deg4)
user_in_5 = names(top10_in_deg5)

matrix_in = cbind(user_in_1, top10_in_deg1,
                 user_in_2, top10_in_deg2,
             user_in_3, top10_in_deg3,
               user_in_4, top10_in_deg4,
               user_in_5, top10_in_deg5)

df_in = as.data.frame(matrix_in); rownames(df_in) = NULL ; df_in

library(gridExtra)
pdf("df_in.pdf", height=20, width=20)
grid.table(df_in)
dev.off()

# Create a dataframe outdegree

user_out_1 = names(top10_out_deg1)
user_out_2 = names(top10_out_deg2)
user_out_3 = names(top10_out_deg3)
user_out_4 = names(top10_out_deg4)
user_out_5 = names(top10_out_deg5)

matrix_out = cbind(user_out_1, top10_out_deg1,
               user_out_2, top10_out_deg2,
               user_out_3, top10_out_deg3,
               user_out_4, top10_out_deg4,
               user_out_5, top10_out_deg5)

df_out = as.data.frame(matrix_out); rownames(df_out) = NULL ; df_out

library(gridExtra)
pdf("df_out.pdf", height=20, width=20)
grid.table(df_out)
dev.off()


# Create a dataframe pagerank
user_prk_1 = names(top10_page_rank1)
user_prk_2 = names(top10_page_rank2)
user_prk_3 = names(top10_page_rank3)
user_prk_4 = names(top10_page_rank4)
user_prk_5 = names(top10_page_rank5)

matrix_page = cbind(user_prk_1, round(top10_page_rank1,5),
               user_prk_2, round(top10_page_rank2,5),
               user_prk_3, round(top10_page_rank3,5),
               user_prk_4, round(top10_page_rank4,5),
               user_prk_5, round(top10_page_rank5,5))

df_page = as.data.frame(matrix_page); rownames(df_page) = NULL ; df_page

library(gridExtra)
pdf("df_pagerank.pdf", height=20, width=20)
grid.table(df_page)
dev.off()


## 4 * Communities * 

# Create undirected graphs
ig1_un <- as.undirected(ig1) ; is.directed(ig1) ; is.directed(ig1_un)
ig2_un <- as.undirected(ig2)
ig3_un <- as.undirected(ig3)
ig4_un <- as.undirected(ig4)
ig5_un <- as.undirected(ig5)

?communities
#  Applying fast greedy clustering, infomap clustering, and louvain clustering

# -------- DON'T RUN  -------------------------------        
# Find communities with fast greedy clustering           
communities_fast_greedy1 <- cluster_fast_greedy(ig1_un) 
communities_fast_greedy2 <- cluster_fast_greedy(ig2_un) 
communities_fast_greedy3 <- cluster_fast_greedy(ig3_un)  
communities_fast_greedy4 <- cluster_fast_greedy(ig4_un)  
communities_fast_greedy5 <- cluster_fast_greedy(ig5_un)  
                                          
# ... and again with infomap clustering
communities_infomap1 <- cluster_infomap(ig1_un,nb.trials = 2)
communities_infomap2 <- cluster_infomap(ig2_un)
communities_infomap3 <- cluster_infomap(ig3_un)
communities_infomap4 <- cluster_infomap(ig4_un)
communities_infomap5 <- cluster_infomap(ig5_un)

# --------------  TILL HERE ----------------

# ... and again with louvain clustering
communities_louvain1 <- cluster_louvain(ig1_un)
communities_louvain2 <- cluster_louvain(ig2_un)
communities_louvain3 <- cluster_louvain(ig3_un)
communities_louvain4 <- cluster_louvain(ig4_un)
communities_louvain5 <- cluster_louvain(ig5_un)

# Find user who twitted each day 
common1 = as.data.frame(intersect(df1$from, df2$from)); names(common1) = c("from");
common2 = as.data.frame(intersect(df3$from, common1$from)); names(common2) = c("from");          
common3 = as.data.frame(intersect(df4$from, common2$from)); names(common3) = c("from");          
common4 = as.data.frame(intersect(df5$from, common3$from)); names(common4) = c("from");          

# Take a random user
tail(common4,1)
random_user = c("zz23377737")

# # ... and for communities_louvain
r1 = membership(communities_louvain1)[random_user];r1
r2 = membership(communities_louvain2)[random_user];r2
r3 = membership(communities_louvain3)[random_user];r3
r4 = membership(communities_louvain4)[random_user];r4
r5 = membership(communities_louvain5)[random_user];r5

# Get the sizes of each community
community_size1 <- sizes(communities_louvain1)
community_size2 <- sizes(communities_louvain2)
community_size3 <- sizes(communities_louvain3)
community_size4 <- sizes(communities_louvain4)
community_size5 <- sizes(communities_louvain5)

# Check the size of the community of the random user 
community_size1[r1]
community_size2[r2]
community_size1[r3]
community_size1[r4]
community_size1[r5]

# Unique values
u1 = sort(unique(community_size1), decreasing = F); u1
u2 = sort(unique(community_size2), decreasing = F); u2
u3 = sort(unique(community_size3), decreasing = F); u3
u4 = sort(unique(community_size4), decreasing = F); u4
u5 = sort(unique(community_size5), decreasing = F); u5

# Largest community 
l1 = u1[length(u1)]; l1
l2 = u2[length(u2)]; l2
l3 = u3[length(u3)]; l3
l4 = u4[length(u4)]; l4
l5 = u5[length(u5)]; l5

# Number of communities
g1 = length(community_size1); g1
g2 = length(community_size2); g2
g3 = length(community_size3); g3
g4 = length(community_size4); g4
g5 = length(community_size5); g5

# %
p1 = l1/vcount(ig1_un); p1
p2 = l2/vcount(ig1_un); p2
p3 = l3/vcount(ig1_un); p3
p4 = l4/vcount(ig1_un); p4
p5 = l5/vcount(ig1_un); p5

# List of colors
colrs1 = rainbow(g1)
colrs2 = rainbow(g2)
colrs3 = rainbow(g3)
colrs4 = rainbow(g4)
colrs5 = rainbow(g5)

# Color vertices by community membership, as a factor
V(ig1)$color <- colrs1[factor(membership(communities_louvain1))]
V(ig2)$color <- colrs2[factor(membership(communities_louvain2))]
V(ig3)$color <- colrs3[factor(membership(communities_louvain3))]
V(ig4)$color <- colrs4[factor(membership(communities_louvain4))]
V(ig5)$color <- colrs5[factor(membership(communities_louvain5))]

# Does the edge cross betwen commmunities?
is_crossing1 <- crossing(ig1, communities = communities_louvain1)
is_crossing2 <- crossing(ig2, communities = communities_louvain2)
is_crossing3 <- crossing(ig3, communities = communities_louvain3)
is_crossing4 <- crossing(ig4, communities = communities_louvain4)
is_crossing5 <- crossing(ig5, communities = communities_louvain5)

# Set edge linetype: solid for crossings, dotted otherwise 
E(ig1)$lty <- ifelse(is_crossing1, "solid", "dotted")
E(ig2)$lty <- ifelse(is_crossing2, "solid", "dotted")
E(ig3)$lty <- ifelse(is_crossing3, "solid", "dotted")
E(ig4)$lty <- ifelse(is_crossing4, "solid", "dotted")
E(ig5)$lty <- ifelse(is_crossing5, "solid", "dotted")

# Desiderable communities (check the array size)
arr_size1 =  communities_louvain1[community_size1 > 40 & community_size1 < 150]; length(arr_size1)
arr_size2 =  communities_louvain2[community_size2 > 40 & community_size2 < 150]; length(arr_size2)
arr_size3 =  communities_louvain3[community_size3 > 40 & community_size3 < 150]; length(arr_size3)
arr_size4 =  communities_louvain4[community_size4 > 40 & community_size4 < 150]; length(arr_size4)
arr_size5 =  communities_louvain5[community_size5 > 40 & community_size5 < 150]; length(arr_size5)

# Desiderable communities
des_size1 =  unlist(communities_louvain1[community_size1 > 40 & community_size1 < 150])
des_size2 =  unlist(communities_louvain2[community_size2 > 40 & community_size2 < 150])
des_size3 =  unlist(communities_louvain3[community_size3 > 40 & community_size3 < 150])
des_size4 =  unlist(communities_louvain4[community_size4 > 40 & community_size4 < 150])
des_size5 =  unlist(communities_louvain5[community_size5 > 40 & community_size5 < 150])

# Induce a subgraph of mention_graph using in_mid_community
mention_subgraph1 <- induced.subgraph(ig1, des_size1)
mention_subgraph2 <- induced.subgraph(ig2, des_size2)
mention_subgraph3 <- induced.subgraph(ig3, des_size3)
mention_subgraph4 <- induced.subgraph(ig4, des_size4)
mention_subgraph5 <- induced.subgraph(ig5, des_size5)

# Plot 1 
paste("The # of different communities is",length(arr_size1))
plot(mention_subgraph1, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     coords = layout_with_fr(mention_subgraph2),
     margin = 0, 
     vertex.size = 3,
     main='Communities July 1')

legend("left", legend= seq(1,12, by =1), 
       title="Communities", title.col ="black",
       col = unique(vertex_attr(mention_subgraph1, index = V(mention_subgraph1))$color), 
       pch=20, 
       pt.cex = 1.0,
       cex = 0.8, 
       horiz = F,
       inset = c(0.1, 0.1),
       text.font=5, 
       bg  = "ghostwhite")

#------
# Plot 2
paste("The # of different communities is",length(arr_size2))

# For this graph the rainbow list of colors provide
# two almost identical colours. That's a problem for the sake
# of visualization! Let's change it :) (changed also for graph3 and 5)

# Check uniqueness andf get ID of color 
unique(vertex_attr(mention_subgraph2, index = V(mention_subgraph2))$color)
# Modify 
vec = vertex_attr(mention_subgraph2, index = V(mention_subgraph2))$color; vec
vec = replace(vec, vec == c("#FF002FFF"), "red")
vec = replace(vec, vec == c("#DEFF00FF"), "green")
vec = replace(vec, vec == c("#FF00ABFF"), "yellow")
vec = replace(vec, vec == c("#FF00EBFF"), "black")
vec = replace(vec, vec == c("#FFFE00FF"), "salmon")
vec = replace(vec, vec == c("#FF0092FF"), "purple")
vec = replace(vec, vec == c("#BFFF00FF"), "cyan")
vec = replace(vec, vec == c("#00FF1BFF"), "white")
vertex_attr(mention_subgraph2, index = V(mention_subgraph2))$color = vec

plot(mention_subgraph2, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     coords = layout_with_fr(mention_subgraph3),
     margin = 0, 
     vertex.size = 3,
     main='Communities July 2')

legend("left", legend= seq(1,8, by =1), 
       title="Communities", title.col ="black",
       col = unique(vertex_attr(mention_subgraph2, index = V(mention_subgraph2))$color), 
       pch=20, 
       pt.cex = 1.0,
       cex = 0.8, 
       horiz = F,
       inset = c(0.1, 0.1),
       text.font=5, 
       bg  = "ghostwhite")

#-------
# Plot 3 
paste("The # of different communities is",length(arr_size3))

# Check uniqueness andf get ID of color 
unique(vertex_attr(mention_subgraph3, index = V(mention_subgraph3))$color)
# Modify 
vec = vertex_attr(mention_subgraph3, index = V(mention_subgraph3))$color; vec
vec = replace(vec, vec == c("#FF0007FF"), "red")
vec = replace(vec, vec == c("#2800FFFF"), "green")
vec = replace(vec, vec == c("#FF0048FF"), "yellow")
vec = replace(vec, vec == c("#00FF14FF"), "black")
vertex_attr(mention_subgraph3, index = V(mention_subgraph3))$color = vec

# Plot again 
plot(mention_subgraph3, 
     vertex.label = NA,
     vertex_colors = vec,
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     coords = layout_with_fr(mention_subgraph1),
     margin = 0, 
     vertex.size = 3,
     main='Communities July 3')

legend("left", legend= seq(1,4, by =1), 
       title="Communities", title.col ="black",
       col = unique(vertex_attr(mention_subgraph3, index = V(mention_subgraph3))$color), 
       pch=20, 
       pt.cex = 1,
       cex = 0.8, 
       horiz = F,
       inset = c(0.1, 0.1),
       text.font=5, 
       bg  = "ghostwhite")

# -------
# Plot 4
paste("The # of different communities is",length(arr_size4))
plot(mention_subgraph4, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     coords = layout_with_fr(mention_subgraph4),
     margin = 0, 
     vertex.size = 3,
     main='Communities July 4')

legend("left", legend= seq(1,11, by =1), 
       title="Communities", title.col ="black",
       col = unique(vertex_attr(mention_subgraph4, index = V(mention_subgraph4))$color), 
       pch=20, 
       pt.cex = 1,
       cex = 0.8, 
       horiz = F,
       inset = c(0.1, 0.1),
       text.font=5, 
       bg  = "ghostwhite")

# --------
# Plot 5 
paste("The # of different communities is",length(arr_size5))

# Check uniqueness andf get ID of color 
unique(vertex_attr(mention_subgraph5, index = V(mention_subgraph5))$color)
# Modify 
vec = vertex_attr(mention_subgraph5, index = V(mention_subgraph5))$color; vec
vec = replace(vec, vec == c("#FF00CAFF"), "red")
vec = replace(vec, vec == c("#FF0071FF"), "green")
vec = replace(vec, vec == c("#FF6C00FF"), "yellow")
vec = replace(vec, vec == c("#FF00DBFF"), "black")
vec = replace(vec, vec == c("#00FAFFFF"), "cyan")
vec = replace(vec, vec == c("#FF0026FF"), "salmon")
vec = replace(vec, vec == c("#FF0088FF"), "blue")

vertex_attr(mention_subgraph5, index = V(mention_subgraph5))$color = vec

plot(mention_subgraph5, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     coords = layout_with_fr(mention_subgraph5),
     margin = 0, 
     vertex.size = 3,
     main='Communities July 5')

legend("left", legend= seq(1,7, by =1), 
       title="Communities", title.col ="black",
       col = unique(vertex_attr(mention_subgraph5, index = V(mention_subgraph5))$color), 
       pch=20, 
       pt.cex = 1,
       cex = 0.8, 
       horiz = F,
       inset = c(0.1, 0.1),
       text.font=5, bg  = "ghostwhite")

