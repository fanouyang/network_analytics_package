#' Plot sociographs for networks.
#'
#' \code{sna_plot} plot sociograph for a network.
#'
#' This function plots sociograph for a network in fruchtermanreingold mode, with node size and color representing
#' a participant's degree (indegree and outdegree) and edge width representing the frequency of interactions between
#' two participants
#'
#' @param x a network
#' @param y a matrix
#' @import sna
#' @import network
#' @import grDevices
#' @export
sna_plot<-function(x,y){
  od<-degree(x,gmode="digraph",cmode="outdegree")
  id<-degree(x,gmode="digraph",cmode="indegree")
  d<-od+id
  gplot(x,vertex.cex=d^0.5, gmode="graph",boxed.labels=FALSE,
        label.cex=0.7, label.pos=5, label.col="grey17",
        vertex.col=rgb((d)/max(d),0,(d)/max(d)),edge.col="grey17",
        label=network.vertex.names(x),
        edge.lwd=y*3,mode = "fruchtermanreingold")
}

#' Return node-level measures of a network.
#'
#' \code{node_measure} calculate node-level measures of a network.
#'
#' This function returns a network's node-level measures, namely every participant's in/outdegree, betweenness,
#' closeness.
#'
#' @param x a network
#' @import sna
#' @import network
#' @export
node_measure<-function(x){
  central.nodes<-cbind(degree(x,cmode="indegree"), degree(x,cmode="outdegree"),
                       betweenness(x,rescale=T),
                       closeness(x,cmode="directed",rescale=T))
  colnames(central.nodes)<-c("indegree","outdegree","betweenness","closeness")
  rownames(central.nodes)<-x%v%"vertex.names"

  list(ranking=central.nodes)
}

#' Return network-level measures of a network.
#'
#' \code{network_measure} calculate network-level measures of a network.
#'
#' This function returns results of a network's network-level measures, including network size, density,
#' average degree, number of edges, reciprocity, transitivity, centralization, hierarchy,
#' number of components, connectedness
#'
#' @param x a network
#' @import sna
#' @import network
#' @export
network_measure<-function(x){
  network.measure<-cbind(network.size(x), gden(x,mode="graph"), mean(degree(x)),
                         network.edgecount(x,na.omit = F),
                         grecip(x, measure = "dyadic.nonnull"), gtrans(x),
                         centralization(x,degree), hierarchy(x, measure = "reciprocity"),
                         components(x,connected="weak"), connectedness(x, g=NULL))

  colnames(network.measure)<-c("network size","density","average degree",
                               "number of edges","reciprocity","transitivity",
                               "centralization","hierarchy",
                               "number of components","connectedness")
  list(network.measure)
}
