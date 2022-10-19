knnR <- function(target, query, k) {
  dstSqr <- rowSums((target - rep(query, each=nrow(target)))^2)
  sel <- rank(dstSqr) <= k
  dstSqrSel <- dstSqr[sel]
  rdr <- order(dstSqrSel)
  idx <- which(sel)
  list(idx = idx[rdr], distSqr = dstSqrSel[rdr])
}

testRandom <- function(n, d, k, reps) {
  for (i in seq_len(reps)) {
    target <- matrix(rnorm(n*d), ncol=d)
    query <- rnorm(d)
    
    knnFun <- buildKnnFunction(target, k)
    resC <- knnFun(query)
    deleteQueryFunction(knnFun)
  
    resR <- knnR(target, query, k)
    
    expect_equal(resR, resC)
  }
}

test_that("multiplication works", {
  testRandom(1, 1, 1, reps = 10)
  testRandom(2, 1, 1, reps = 10)
  testRandom(1, 2, 1, reps = 10)
  testRandom(1, 1, 2, reps = 10)
  testRandom(2, 2, 1, reps = 10)
  testRandom(2, 1, 2, reps = 10)
  testRandom(1, 2, 2, reps = 10)
  testRandom(2, 2, 2, reps = 10)
  testRandom(10, 3, 2, reps = 10)
  testRandom(100, 3, 20, reps = 10)
})
