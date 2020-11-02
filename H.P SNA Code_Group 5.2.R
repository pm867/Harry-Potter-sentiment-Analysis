#######################################################################################
######### H.P Project: Text Pre-Processing Codes-Group 5.2  ###########################
#######################################################################################

######################################
######## Part 1: load packages #######
######################################

library(dplyr) 
library(igraph)
# install.packages('Hmisc')
library(Hmisc)


######################################
##### Part 2: read the data in R #####
######################################
setwd("~/Downloads/MSBA/MIS/Data Quilting Project")

SNA_Scripts <- read.csv("TM_5_Coversation_Characters.csv",quote = '' )

SNA_Scripts <- SNA_Scripts %>% select(Character_Who,Character_Whom)
SNA_Scripts <- SNA_Scripts %>% filter(Character_Whom != "")


# lower and capitalize for all the characters
SNA_Scripts <- SNA_Scripts %>% mutate(Character_Who = tolower(Character_Who))
SNA_Scripts <- SNA_Scripts %>% mutate(Character_Who = capitalize(Character_Who))

SNA_Scripts <- SNA_Scripts %>% mutate(Character_Whom = tolower(Character_Whom))
SNA_Scripts <- SNA_Scripts %>% mutate(Character_Whom = capitalize(Character_Whom))

# there are 73 different characters in the first column
length(unique(SNA_Scripts$Character_Who))
unique(SNA_Scripts$Character_Who)
# there are 72 different characters in the second column
length(unique(SNA_Scripts$Character_Whom))
unique(SNA_Scripts$Character_Whom)

# make sure all names are correct
sort(unique(SNA_Scripts$Character_Who))
sort(unique(SNA_Scripts$Character_Whom))

#########################################
##### Part 3:Create nodes and edges #####
#########################################

conversations <- SNA_Scripts %>% group_by(Character_Who, Character_Whom) %>% summarise(counts = n())

set.seed(24)
conversations <- conversations[sample(nrow(conversations), 50), ]

nodes <- c(as.character(conversations$Character_Who), as.character(conversations$Character_Whom))
nodes <- unique(nodes)
str(nodes)


my_graph <- graph_from_data_frame(d=conversations, vertices=nodes, directed=FALSE)
my_graph # 45 nodes & 50 edges
str(my_graph)

# view the names of each node
V(my_graph)$name

# view the edges 
E(my_graph)

#########################################
##### Part 4: Visualization #############
#########################################

# Ⅰ. Nodes and Edges
# plot the graph
plot(my_graph, vertex.label.color = "black")

# different layouts of plotting the graph
# circle layout
plot(my_graph, vertex.label.color = "black", layout = layout_in_circle(my_graph))
# Fruchterman-Reingold layout 
plot(my_graph, vertex.label.color = "black", layout = layout_with_fr(my_graph))
# tree layout 
plot(my_graph, vertex.label.color = "black", layout = layout_as_tree(my_graph))

# Create a vector of weights based on the number of conversations each pair has
w1 <- E(my_graph)$counts

# plot the network varying edges by weights
# the thicker the width of the edge, the more conversations that pair has
plot(my_graph, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = sqrt(w1),  # put w1 in sqrt() so that the lines don't become too wide
     layout = layout_nicely(my_graph))

# create a new igraph object by keeping just the pairs that have at least 2 conversations 
my_graph_2more_conv <- delete_edges(my_graph, E(my_graph)[counts < 2])

# plot the new graph 
plot(my_graph_2more_conv, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = sqrt(E(my_graph_2more_conv)$counts),
     layout = layout_nicely(my_graph_2more_conv))


# Direction of Conversations
g <- graph_from_data_frame(conversations, directed = TRUE)
g
is.directed(g)
str(g)

# plot the directed network; notice the direction of the arrows, they show the direction of the conversation
plot(g, 
     vertex.label.color = "black", 
     edge.color = 'orange',
     vertex.size = 0,
     edge.arrow.size = 0.03,
     layout = layout_nicely(g))

# identify all neighbors of 'Harry' regardless of direction
neighbors(g, 'Harry', mode = c('all'))

# identify the nodes that go towards 'Harry'
neighbors(g, 'Harry', mode = c('in'))

# identify the nodes that go from 'Harry'
neighbors(g, 'Harry', mode = c('out'))

# identify any vertices that receive an edge from 'Harry' and direct an edge to 'Hagrid'
n1 <- neighbors(g, 'HARRY', mode = c('out'))
n2 <- neighbors(g, 'Hagrid', mode = c('in'))
intersection(n1, n2)

# determine which 2 vertices are the furthest apart in the graph
farthest_vertices(g) 
# shows the path sequence between two furthest apart vertices
get_diameter(g)  

# identify vertices that are reachable within two connections from 'Harry'
ego(g, 2, 'Harry', mode = c('out'))

# identify vertices that can reach Harry' within two connections
ego(g, 2, 'Harry', mode = c('in'))


# calculate the out-degree of each vertex
# out-degree represents the number of vertices that are leaving from a particular node
g.outd <- degree(g, mode = c("out"))
g.outd

# find the vertex that has the maximum out-degree
which.max(g.outd)

# calculate betweenness of each vertex
# betweeness is an index of how frequently the vertex lies on shortest paths between any two vertices 
# in the network. It can be thought of as how critical the vertex is to the flow of information 
# through a network. Individuals with high betweenness are key bridges between different parts of 
# a network.
g.b <- betweenness(g, directed = TRUE)
g.b

# Create plot with vertex size determined by betweenness score
plot(g, 
     vertex.label.color = 'black',
     edge.color = 'black',
     vertex.size = sqrt(g.b) / 1.2,
     edge.arrow.size = 0.03,
     layout = layout_nicely(g))


# geodesic distances of connections going out from 'Hagrid'
# create a plot of these distances from 'Hagrid'
# this graph will only show those that are wiithin 2 connections of Hagrid
# you can show the maximal number of connections by replacing 2 by diameter(g)
g184 <- make_ego_graph(g, 2, nodes = 'Hagrid', mode = c("all"))[[1]]
g184

# Get a vector of geodesic distances of all vertices from vertex 'Hagrid' 
dists <- distances(g184, "Hagrid")

# Create a color palette of length equal to the maximal geodesic distance plus one.
colors <- c("black", "blue", "orange", "red", "green")

# Set color attribute to vertices of network g184.
V(g184)$color <- colors[dists+1]

# Visualize the network based on geodesic distance from vertex 184 (patient zero).
plot(g184, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = .6,
     edge.color = 'black',
     vertex.size = 7,
     edge.arrow.size = .05)


# Ⅱ. Others

# install.packages('geomnet')
library(geomnet)

hp.all <- fortify(as.edgedf(hp.edges), hp.chars, )
#> Using name1 as the from node column and name2 as the to node column.
#> If this is not correct, rewrite dat so that the first 2 columns are from and to node, respectively.
#> Joining edge and node information by from_id and name respectively.
# only plot the characters with any connections in a given book. 
ggplot(data=SNA_Scripts, aes(from_id = Character_Who, to_id = Character_Whom)) + 
  geom_net(fiteach=T, directed = T, size = 3, linewidth = .5, 
           ealpha = .5, labelon = T, fontsize = 3, repel = T, 
           labelcolour = "black", arrowsize = .5, singletons = FALSE,
           ) + 
  scale_colour_manual(values = c("#941B08","#F1F31C", 
                                 "#071A80", "#154C07")) + 
  theme_net() + theme(panel.background = element_rect(colour = 'black'))

