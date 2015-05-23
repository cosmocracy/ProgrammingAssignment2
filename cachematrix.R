## CacheMatrix
##
## This script implements the functions and data structures necessary to calcuate the
## inverse/solution of a matrix as well as store this result for future cached re-use.
##
## SAMPLE USAGE:
##
##   > m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
##   > cm <- makeCacheMatrix()
##   > cm$set(m)
##   > cacheSolve(cm)
##   [,1] [,2]
##   [1,]  0.0    1
##   [2,]  0.5    0
##   > cacheSolve(cm)
##   Returning cached matrix inverse
##   [,1] [,2]
##   [1,]  0.0    1
##   [2,]  0.5    0
##
## NOTE: This module only handles square/invertible matrices.
## SEE ALSO: Coursera R Programming - Programming Assignment 2: https://class.coursera.org/rprog-014/human_grading/view/courses/973495/assessments/3/submissions

## Constructs a new CacheMatrix.  A CacheMatrix is an R object--as a list--with
## supporting set/get function for both the matrix and the cached value.  Note
## that while the cached inverse/solution value is available within the structure
## (via setInverse() and getInverse() methods), it is important to instead use
## cacheSolve() as it will properly implement the cache/refresh pattern.
makeCacheMatrix <- function(mat = matrix()) {
  
  # Initialize the cached inverse as NULL until we compute it
  inv <- NULL
  
  # Set accessor for the matrix to be inverted/solved
  set <- function(newMat) {
    # Save the new matrix
    mat <<- newMat
    # Clear/invalidate the cache
    inv <<- NULL
  }
  
  # Get accessor for the matrix to be inverted/solved
  get <- function() {
    mat
  }
  
  # Set accessor for the cached matrix inverse/solution
  # NOTE: Do not call this directly--this is meant to be used internally by 
  # the CacheMatrix.
  setInverse <- function(newInv) {
    inv <<- newInv
  }
  
  # Get accessor for the cached matrix inverse/solution
  # NOTE: Do not call this directly--this is meant to be used internally by 
  # The CacheMatrix.  Instead call cacheSolve(...)
  getInverse <- function() {
    inv
  }
  
  # Return the initialized CacheMatrix structure, including both the objects
  # and accessor functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse/solution of the matrix in the given CacheMatrix structure.
## If a prior inverse/solution was computed (and the matrix within the CacheMatrix
## was not subsequently reset), this function returns the cached value.  Otherwise,
## the function will compute the inverse/solution--possibly an expensive operation--and
## stores the result internally in the cache before returning it to the caller.
##
## NOTES: 
## 1. This function assumes that the given matrix is square/invertable/solvable
## 2. Any additional parameters passed (beyond the CacheMatrix instance) will the
##    provided to the solve() method used to calculate the inverse.
cacheSolve <- function(cachematrix, ...) {
  # Fetch the cached inverse (if any)
  inv <- cachematrix$getInverse()
  # If we have a previously-calculated value, return it to the caller
  if(! is.null(inv)) {
    message("Returning cached matrix inverse")
    return(inv)
  } else {
    # Otherwise, fetch the underlying matrix and calculate the inverse/solution
    mat <- cachematrix$get()
    inv <- solve(mat, ...)
    # Save the result in the cache before returning it to the caller
    cachematrix$setInverse(inv)
    return(inv)
  }
}