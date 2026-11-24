#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(FNN,spam,Matrix)

getWknn<-function(xy,k) {
  #construct Wmatrix
  n<- NROW(xy)
  nn<-knn.index(xy,k=k)
  W<-spam(0,n,n)
  cseq = c(1:n)
  for (i in 1:k) {
    tmp1<-cbind( cseq,nn[,i],1/k)
    tmp2<-cbind( cseq,cseq,0)
    tmp3<-rbind(tmp1,tmp2)
    if (i==1) {
      W<-sparseMatrix(i=tmp3[,1],j=tmp3[,2],x=tmp3[,3])
    } else {
      W <- W + sparseMatrix(i=tmp3[,1],j=tmp3[,2],x=tmp3[,3])
    }
  }
  diag(W) = 0
  return(W)
}

# for flexible W-matrix
constructW<-function(Ws,phi) {
  phisum = sum(phi^c(1:length(Ws)))
  W = (phi * Ws[[1]]) / phisum
  for (i in 2:length(Ws)) {
    W = W + (phi^i * Ws[[i]])/phisum
  }
  return(W)
}

# Queen contiguity W-matrix from shapefile polygons
# Returns a row-standardized sparse weight matrix
getWcontiguity <- function(shape_sf) {
  nb <- spdep::poly2nb(shape_sf, queen = TRUE)
  # Convert to sparse matrix (row-standardized)
  listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  W <- as(spdep::listw2mat(listw), "sparseMatrix")
  return(W)
}

# Inverse-distance decay W-matrix
# bandwidth_km: controls the decay rate (in km)
# Returns a row-standardized sparse weight matrix
getWdistdecay <- function(coords, bandwidth_km = 200, cutoff_km = NULL) {
  n <- nrow(coords)
  # Compute pairwise Euclidean distances (coords assumed in meters for EPSG:3035)
  D <- as.matrix(dist(coords)) / 1000  # convert to km

  # Exponential decay kernel
  W_raw <- exp(-D / bandwidth_km)
  diag(W_raw) <- 0

  # --- Apply Distance Cutoff ---
  if (!is.null(cutoff_km)) {
    W_raw[D > cutoff_km] <- 0
  }
  # ------------------------------

  # Row-standardize
  row_sums <- rowSums(W_raw)
  row_sums[row_sums == 0] <- 1  # avoid division by zero
  W <- W_raw / row_sums

  # Convert to sparse matrix
  W <- as(W, "sparseMatrix")
  return(W)
}
