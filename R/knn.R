#' @export
buildKnnFunction <- function(target, k, removeNaRows = TRUE) {
  
  if (removeNaRows) {
    naRows <- apply(target, 1, \(x) any(is.na(x)))
    target <- target[!naRows, ]
  }
    
  # Separately treat the case of <= k target points.
  if (k >= nrow(target)) {
    return(
      function(query) {
        dstSqr <- distSqrToVec(target, query)
        rdr <- order(dstSqr)
        list(idx = rdr, distSqr = dstSqr[rdr])
      }
    )
  }
  
  # Use kdTree.
  pointers <- buildKnnTree(target, k)
  function(query) {
    if (is.null(pointers)) stop("The knnFunction is not valid. It may have been deleted.")
    res <- queryKnnTree(query, k, pointers)
    res$idx <- res$idx + 1
    return(res)
  }
  
}


#' @export
deleteQueryFunction <- function(queryFunction) {
  env <- environment(queryFunction)
  if ("pointers" %in% names(env) && !is.null(env$pointers)) destroyKnnTree(env$pointers)
  env$pointers <- NULL
}
