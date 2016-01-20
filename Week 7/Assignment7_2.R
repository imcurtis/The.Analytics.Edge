#1.1
edges <- read.csv("edges.csv")
users <- read.csv("users.csv")
str(users)
str(edges)
unique(edges$V1)
146/40
table(users$school, users$locale)
table(users$gender, users$school)

#2.1
This can be carried out with the following commands:
  
  install.packages("igraph")

library(igraph)

g = graph.data.frame(edges, FALSE, users)
?graph.data.frame

#2.2
g = graph.data.frame(edges, FALSE, users) 
plot(g, vertex.size=5, vertex.label=NA)
degree(g)>=10

#2.4
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
sort(V(g)$size)

#3.1
V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

#3.2
V(g)$color = "black"

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "AB"] = "gray"

#3.3
table(users$locale)

V(g)$color = "black"

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label=NA)


#4

?igraph.plotting

