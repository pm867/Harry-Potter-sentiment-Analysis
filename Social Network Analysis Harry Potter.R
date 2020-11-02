#########################################################################################
############################### Social Network Analysis #################################
#########################################################################################


##############################
######## load packages #######
##############################
# if the packages below are not installed, then uncomment the install.packages() lines and run them
#install.packages("dplyr")
#install.packages("igraph")
library(dplyr) # dplyr package is used for data manipulation; it uses pipes: %>%
library(igraph) # used to do social network analysis



##############################
##### read the data in R #####
##############################
# it's good practice to set up the working directory
# all the files youo read in R or write from R will be in the working directory you set up
# if you copy the path of your file from the foler, change all the \ to /
setwd("your path here")
scripts <- read.csv("Harry Potter Script.csv")

# keep only the first 2 columns to which we will apply Social Network Analysis
scripts <- scripts %>% select(Character1, Character2)
# there in the Character 2 column, there are some instances in which the character name is not populated
# remove these instances
scripts <- scripts %>% filter(Character2 != "")

# there are 58 different characters in the first column
length(unique(scripts$Character1))
unique(scripts$Character1)
# there are 56 different characters in the second column
length(unique(scripts$Character2))
unique(scripts$Character2)


# keep only the first 2 columns to which we will apply Social Network Analysis
scripts <- scripts %>% select(Character1, Character2)

# in social network analysis, we need 2 types of files:
    # conversations: a data frame that shows who talks to whom and how many times
    # nodes: a vector that stores all the character names

# in social network analysis, we shouldn't have duplicate rows, while keeping track of how many times a character talks to another
# in the counts column, write how many times characters communicate with each other
conversations <- scripts %>% group_by(Character1, Character2) %>% summarise(counts = n())

# keep just 50 random conversations in this notebook
set.seed(42) # setting a seed allows us to select the same sample every time
conversations <- conversations[sample(nrow(conversations), 50), ]


# store the character names in a vector
nodes <- c(as.character(conversations$Character1), as.character(conversations$Character2))
nodes <- unique(nodes)


# create the igraph object
# the graph_from_data_frame() function takes in 2 objects: 
# edges: who talks to whom
# nodes (vertices): the unique list of all the characters included in the conversations 
my_graph <- graph_from_data_frame(d=conversations, vertices=nodes, directed=FALSE)
my_graph # 36 nodes & 50 edges


# view the names of each node
V(my_graph)$name

# view the edges 
E(my_graph)

# plot the graph (click on Zoom to see it larger)
plot(my_graph, vertex.label.color = "black")

# try different layouts of plotting the graph
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


# up until this point, we have only displayed undirected graphs
# therefore, the direction of the conversation was not accounted for

# create a new graph that takes into consideration the direction of the conversation
g <- graph_from_data_frame(conversations, directed = TRUE)
g
# Is the graph directed?
is.directed(g)

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
n1 <- neighbors(g, 'Harry', mode = c('out'))
n2 <- neighbors(g, 'Hagrid', mode = c('in'))
intersection(n1, n2)


# determine which 2 vertices are the furthest apart in the graph
farthest_vertices(g) 
# shows the path sequence between two furthest apart vertices
get_diameter(g)  


# identify vertices that are reachable within two connections from 'Harry'
ego(g, 2, 'Snape', mode = c('out'))

# identify vertices that can reach Harry' within two connections
ego(g, 2, 'Snape', mode = c('in'))


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