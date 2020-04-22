library(igraph)
library(xlsx)
library(RColorBrewer)
source("functions.R")

setwd("C:/Users/Amit/OneDrive/1 - GWU MSBA/DNSC 6290 Social Network Analytics/Project")

# Gather data from report
reportData <- read.xlsx(file="Madrid Data from Report.xlsx",head=TRUE,sheetName="Sheet1")

# Edges and general plot formatting
edges = matrix(scan("madrid-edges.dat", 0), ncol=3, byrow=TRUE)
g = graph_from_edgelist(edges[,1:2], directed=FALSE)
E(g)$weight = edges[,3]
g = simplify(g)

# Names data
names = as.list(levels(reportData$NAME))
V(g)$names = names
names = V(g)$names

# Basic Numbered Plot
V(g)$color = "orange"
plot(g, layout=layout.fruchterman.reingold, vertex.label=NULL, vertex.size = 8)

# Nationality Plot
colors <- rainbow(13)
V(g)$countries = reportData$Nationality
V(g)$color = colors[V(g)$countries]
plot(g, layout=layout.fruchterman.reingold, vertex.label.color="black", vertex.size = 8)
legend(x=1.3, y=1, c(levels(reportData$Nationality)), pch=21,
       col="#777777", pt.bg=colors, pt.cex=2, cex=.8, bty="n", ncol=1)

# Other Connection Subsets
V(g)$Subset = reportData$Link.to.9.11
V(g)$color = colors[V(g)$Subset]
plot(g, layout=layout.fruchterman.reingold, vertex.label.color="black", vertex.size = 8, main="Links to 9/11")

V(g)$Subset = reportData$Link.to.Casablanca.Bombings
V(g)$color = colors[V(g)$Subset]
plot(g, layout=layout.fruchterman.reingold, vertex.label.color="black", vertex.size = 8, main="Links to Casablanca")

V(g)$Subset = reportData$Link.to.AVE.Incident
V(g)$color = colors[V(g)$Subset]
plot(g, layout=layout.fruchterman.reingold, vertex.label.color="black", vertex.size = 8, main="Links to AVE Incident")

V(g)$Subset = reportData$Training.Camps...Wars
V(g)$color = colors[V(g)$Subset]
plot(g, layout=layout.fruchterman.reingold, vertex.label.color="black", vertex.size = 8, main="Involvement in Training Camps/Wars")

V(g)$Subset = reportData$Links.to.Al.Qaeda
V(g)$color = colors[V(g)$Subset]
plot(g, layout=layout.fruchterman.reingold, vertex.label.color="black", vertex.size = 8, main="Links to Al Qaeda")