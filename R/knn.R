#' @export
buildKnnFunction <- function(target, k) {
  if (k >= nrow(target)) {
    return(
      function(query) {
        dstSqr <- distSqrToVec(target, query)
        rdr <- order(dstSqr)
        list(idx = rdr, distSqr = dstSqr[rdr])
      }
    )
  }
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
