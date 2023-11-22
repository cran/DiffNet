
#'  Correction for multiple testing
#'
#' @param p.values a vector of p.values
#' @param method method of correction: c(\code{"BH"}, \code{"bonferroni"})
#'
#' @details
#' Correction for multiple testing
#'
#' @return
#' vector of q-values
#'
#' @examples
#' graph = graph_generation(n.nodes = 10, prob.connection = 0.5)
#' initial_score = c(rep(0,5),0.2, 0.3, 0, 0, 0.5)
#' names(initial_score) = igraph::V(graph)
#' Actual_score = actual_score(graph = graph, initial.score = initial_score, damping = 0.7)
#' Null_score = null_score(graph = graph, initial.score = initial_score, damping = 0.7, N.repeat = 10)
#' pvalue = pval(actual.scores = Actual_score, null.scores = Null_score, method = "non_parametric")
#' adj_nodes = multiple_testing_correction(pvalue)

multiple_testing_correction = function(p.values, method = "BH"){
  if(length(p.values) == 0 || !is.numeric(p.values)){
    stop("p.values must be a non-empty numeric vector")
  }

  supported_methods = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
  if(!(method %in% supported_methods)){
    stop("Unsupported method. Choose from: ", paste(supported_methods, collapse = ", "))
  }

  adj_pvalues = stats::p.adjust(p.values, method = method)
  return(adj_pvalues)
}
