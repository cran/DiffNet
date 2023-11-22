#' dummy graph generation
#'
#' @param n.nodes number of nodes
#' @param prob.connection node connection probability (default=0.5)
#'
#' @details
#' Generate a random graph
#'
#'
#' @return
#' igraph object
#'
#' @examples
#' graph = graph_generation(n.nodes = 10, prob.connection = 0.5)
#' initial_score = c(rep(0,5),0.2, 0.3, 0, 0, 0.5)
#' names(initial_score) = igraph::V(graph)
#'
graph_generation = function(n.nodes = 10, prob.connection = 0.5){

  # Generate a random Erdős-Rényi graph
  graph = igraph::erdos.renyi.game(n.nodes, prob.connection)

  # Create a vector of names
  node_names = paste("Node", 1:igraph::vcount(graph), sep="")

  # Assign names to the nodes
  igraph::V(graph)$name = node_names
  return(graph)
}

