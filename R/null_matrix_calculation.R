
#' Calculation of diffusion null scores for each node

#' @param graph an igraph object with the length of N
#' @param initial.score a named vector of node preferences of length N served as the initial values for diffusion algorithm.
#' @param damping The damping factor of the diffusion algorithm.
#' @param N.repeat number of permutation repeats of null scores.
#' @param n.cores number of cores for parallel processing.

#' @details
#' This function calculates the null diffusion score for each node using the personalized page rank algorithm.
#' The initial values are obtained by permuting the given \code{initial.score}
#'
#' @return
#' a matrix of null diffusion scores (\code{N.repeat}—BY—number_of_nodes).
#'
#' @examples
#' graph = graph_generation(n.nodes = 10, prob.connection = 0.5)
#' initial_score = c(rep(0,5),0.2, 0.3, 0, 0, 0.5)
#' names(initial_score) = igraph::V(graph)
#' Null = null_score(graph, initial_score)
#'
null_score = function(graph, initial.score, damping = 0.7, N.repeat = 10, n.cores = 1){
  assertthat::assert_that(n.cores>0)

  if (length(initial.score)==0){
    stop("initial.score is an empty vector")
  }
    cl = parallel::makeCluster(n.cores)
    on.exit(parallel::stopCluster(cl))

    parallel::clusterExport(cl, c("graph", "initial.score", "damping"), envir = environment())
    parallel::clusterEvalQ(cl, library(igraph))
    ParLoop = function(i){
      local_initial_score = sample(initial.score)
      null_score = igraph::page_rank(
        graph,
        directed = FALSE,
        damping = damping,
        personalized = local_initial_score)$vector

      return(null_score)
    }
    null_scores = parallel::parLapply(cl, 1:N.repeat, ParLoop)
    Null_scores = do.call(rbind, null_scores)

  return(t(Null_scores))

}


