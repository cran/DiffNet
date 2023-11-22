
#' Calculation of p-values for each score with respect to the null.
#'
#' @param actual.scores a vector including actual scores with the length of number of nodes (N_nodes).
#' @param null.scores a matrix of null scores with the dimension of N_nodes x N_repeat
#' @param method statistical test method: c(\code{"exponential"}, \code{"gamma"}, \code{"non_parametric"})
#'
#' @details
#' Calculate the p-value for each node based on the actual and null diffusion scores.
#'
#' @return
#' vector of p-values
#'
#' @examples
#' graph = graph_generation(n.nodes = 10, prob.connection = 0.5)
#' initial_score = c(rep(0,5),0.2, 0.3, 0, 0, 0.5)
#' names(initial_score) = igraph::V(graph)
#' Actual_score = actual_score(graph = graph, initial.score = initial_score, damping = 0.7)
#' Null_score = null_score(graph = graph, initial.score = initial_score, damping = 0.7, N.repeat = 10)
#' pvalue = pval(actual.scores = Actual_score, null.scores = Null_score, method = "exponential")


pval = function(actual.scores, null.scores, method = "exponential"){

  if(length(actual.scores) == 0 || dim(null.scores)[1] == 0){
    stop("actual.scores or null.scores is empty")
  }

  if(!method %in% c("exponential", "gamma", "non_parametric")) {
    stop("Invalid method. Choose from 'exponential', 'gamma', 'non_parametric'")
  }

  pvalue = length(actual.scores)
  for(j in 1:length(actual.scores)){

    if(method == "exponential"){

      # P-value calculation: exponential
      rate = MASS::fitdistr(null.scores[j,], "exponential")$estimate
      pvalue[j] = stats::pexp(actual.scores[j], rate, lower.tail = FALSE)


    }else if(method == "gamma"){

        # P-value calculation: exponential
        param = MASS::fitdistr(null.scores[j,], "gamma")
        shape = param$estimate["shape"]
        rate = param$estimate["rate"]
        pvalue[j] = stats::pgamma(actual.scores[j], shape, rate, lower.tail = FALSE)

    }else if(method == "non_parametric"){

      # P-value calculation: non_parametric
      pvalue[j] = sum(null.scores[j,] >= actual.scores[j]) / ncol(null.scores)
    }
  }
  return(pvalue)
}


