
#' Calculation of diffusion score for each node
#'
#' @param graph an igraph object with the length of N
#' @param initial.score a named vector of node preferences of length N served as the initial values for diffusion algorithm.
#' @param damping The damping factor of the diffusion algorithm.
#'
#' @details
#' This function calculates the diffusion score for each node using the personalized page rank algorithm.
#'
#' @return
#' a vector of diffusion scores.
#'
#' @examples
#' graph = graph_generation(n.nodes = 10, prob.connection = 0.5)
#' initial_score = c(rep(0,5),0.2, 0.3, 0, 0, 0.5)
#' names(initial_score) = igraph::V(graph)
#' Actual_score = actual_score(graph = graph, initial.score = initial_score, damping = 0.7)
#'
actual_score = function(graph, initial.score, damping = 0.7){

  if (length(initial.score)==0){
    stop("initial.score is an empty vector")
  }

  actual_score = igraph::page_rank(
      graph,
      directed = FALSE,
      damping = damping,
      personalized = initial.score)$vector

  return(actual_score)
}






