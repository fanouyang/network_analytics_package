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
#' @example
#' sna_plot(week02_network,week02_matrix)
#'
#' @export
sna_plot<-function(x,y){
  od<-sna::degree(x,gmode="digraph",cmode="outdegree")
  id<-sna::degree(x,gmode="digraph",cmode="indegree")
  d<-od+id
  sna::gplot(x,vertex.cex=d^0.5, gmode="graph",boxed.labels=FALSE,
        label.cex=0.7, label.pos=5, label.col="grey17",
        vertex.col=rgb((d)/max(d),0,(d)/max(d)),edge.col="grey17",
        label=network::network.vertex.names(x),
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
#' @example
#' node_measure(week02_network)
#'
#' @export
node_measure<-function(x){
  central.nodes<-cbind(sna::degree(x,cmode="indegree"), sna::degree(x,cmode="outdegree"),
                       sna::betweenness(x,rescale=T),
                       sna::closeness(x,cmode="directed",rescale=T))
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
#' @example
#' network_measure(week02_network)
#'
#' @export

network_measure<-function(x){
  network.measure<-cbind(sna::network.size(x), sna::gden(x,mode="graph"), mean(sna::degree(x)),
                         sna::network.edgecount(x,na.omit = F),
                         sna::grecip(x, measure = "dyadic.nonnull"), sna::gtrans(x),
                         sna::centralization(x,degree), sna::hierarchy(x, measure = "reciprocity"),
                         sna::components(x,connected="weak"), sna::connectedness(x, g=NULL))

  colnames(network.measure)<-c("network size","density","average degree",
                               "number of edges","reciprocity","transitivity",
                               "centralization","hierarchy",
                               "number of components","connectedness")
  list(network.measure)
}
