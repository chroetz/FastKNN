#' @export
buildKnnDataStructure <- function(target, k) {
  pointers <- buildKnnTree(target, k)
  return(list(pointers=pointers, k=k))
}

#' @export
queryKnn <- function(query, dataStructure) {
  res <- queryKnnTree(query, dataStructure$k, dataStructure$pointers)
  res$idx <- res$idx + 1
  return(res)
}

#' @export
destroyKnnDataStructure <- function(dataStructure) {
  destroyKnnTree(dataStructure$pointers)
}
