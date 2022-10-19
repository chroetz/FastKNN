#include <Rcpp.h>

#include <ANN/ANN.h>
using namespace Rcpp;


// [[Rcpp::export]]
List buildKnnTree(NumericMatrix target, int k) {
    
    int n = target.nrow();
    int d = target.ncol();
    
    // Allocate memory for further use.
    ANNkd_tree *nnTree;
	ANNpointArray pts = annAllocPts(n, d);
    ANNidx *nnIdxs = new ANNidx[k];
	ANNdist *nnDists = new ANNdist[k];
    
    // Prepare data structures.
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < d; j++) {
			pts[i][j] = target(i, j);
		}
	}
	nnTree = new ANNkd_tree(pts, n, d);
	
    // Create R friedly pointers to output variables.
    XPtr<ANNkd_tree> ptr_tree(nnTree);
    XPtr<ANNidx> ptr_idxs(nnIdxs);
    XPtr<ANNdist> ptr_dists(nnDists);
    
    List out = List::create(ptr_tree, ptr_idxs, ptr_dists);
    return out;
}

// [[Rcpp::export]]
List queryKnnTree(NumericVector query, int k, List pointers) {
    
    // Read in previously created data structures.
    SEXP x;
    x = pointers[0];
    XPtr<ANNkd_tree> ptr_tree = (XPtr<ANNkd_tree>)(x);
    x = pointers[1];
    XPtr<ANNidx> ptr_idxs = (XPtr<ANNidx>)(x);
    x = pointers[2];
    XPtr<ANNdist> ptr_dists = (XPtr<ANNdist>)(x);
    ANNkd_tree *nnTree = ptr_tree.get();
    ANNidxArray nnIdxs = ptr_idxs.get();
	ANNdistArray nnDists = ptr_dists.get();
    
    nnTree->annkSearch(	// search
        query.begin(),	// query point
        k,	// number of near neighbors
        nnIdxs,	// nearest neighbors (returned)
        nnDists,	// distance (returned)
        0.0); // error bound	

    List out = List::create(
        Named("idx") = NumericVector(nnIdxs, nnIdxs + k),
        Named("distance") = NumericVector(nnDists, nnDists + k)
    );

    return out;
}

// [[Rcpp::export]]
void destroyKnnTree(List pointers) {
    
    SEXP x;
    x = pointers[0];
    XPtr<ANNkd_tree> ptr_tree = (XPtr<ANNkd_tree>)(x);
    ANNkd_tree *nnTree = ptr_tree.get();
    
    ANNpointArray pts = nnTree->thePoints();
    annDeallocPts(pts);
}
