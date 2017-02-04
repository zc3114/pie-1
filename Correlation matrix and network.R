# make a correlation matrix & then a network

# load libraries
library(corrgram)
library(igraph)

# laod data for all genes interacting with target gene

#d <- read.csv('rpl_39data.csv', header = TRUE)


#===========Make a correlation matrix=============================

# make the data for the correlation matrix by removinge "time-min" column
cd<-d[,2:dim(d)[2]]

cm<-as.matrix(cor(cd, use="everything", method="kendall")) #pearson kendall spearman

  #====================make adjacency graph=======================

g<-graph.adjacency(cm, mode=c("undirected"), weighted=TRUE, diag=FALSE, add.colnames=NULL, add.rownames=NA)

#look at the distribution of distances

hist(cm) # in this example, most genes are very highly correlated with each other

# simplify the graph by removing weights(correlations) that are closer than the 80th percentile of the distribution (or whatever). In this case, I choose a very high cutoff because these genes are really tightly correlated. Note, how, once you've implemented "delete.edges" the number of edges in g decreases.


#specify the cutoff

cutoff<-quantile(abs(cm), 0.8) # note: the absolute keeps the negative correlations

# delete edges below the cutoff
g=delete.edges(g, which(abs(E(g)$weight) <=cutoff)) # note: the absolute keeps the negative correlations


#examine the list of vertices

V(g)

#examine the list of edges

E(g)


#=================== make the graph =====================


# specify the look of the vertices and edges. See the igraph manual for further options

V(g)$color<-"gray71" # specify the vertex colour 
V(g)$size<-5  # specify vertex size
V(g)$label.color <- "gray18" # specify the label colour of the graph
V(g)$label.cex <- 0.75 # specify the label size
V(g)$label.dist <- -0.3 # specify the label distance from the centre of the vertex
V(g)$label.family <- "sans" # specify the label font. Alternative is "serif"
V(g)$label.font <- 2 # specify the label type (italic etc)
E(g)[weight > 0]$color <- "tomato4" # specify the edge colour.  Here, positive correlations are red; negative correlations are blue (in this example all are positive)
E(g)[weight < 0]$color <- "steelblue4"
E(g)$curved <- TRUE # specify the edge shape; FALSE gives straight

# specify the layout of the graph.  This is all stuff that will help give an intelligible layout. There are other layouts that you can use, see igraph manual
minC <- rep(-Inf, vcount(g))
maxC <- rep(Inf, vcount(g))
minC[1] <- maxC[1] <- 0
fr <- layout_with_fr(g, minx=minC, maxx=maxC, miny=minC, maxy=maxC)
                                  

# plot the graph

plot(g, layout=fr,edge.width=E(g)$weight)


# detect & plot communities.  See https://users.dimi.uniud.it/~massimo.franceschet/R/communities.html for other methods

c1 = cluster_fast_greedy(g) # run community detection algorithm
modularity(c1) # gives a modularity value
membership(c1) # membership of communities
length(c1) # number of communities; note here there are many "communities" with single vertices because of the cutoff
sizes(c1) # size of communities


plot(g, vertex.color=membership(c1), edge.color="grey71", layout=fr,edge.width=E(g)$weight) # plot with vertices coloured as communities


plot(c1, g, layout=fr,edge.width=E(g)$weight, edge.color="grey71") # plot with communities overlain in colours









