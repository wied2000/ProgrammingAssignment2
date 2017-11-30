## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here). Below are a pair of functions 
## that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
          inverse <- x$getinverse()
  if(!is.null(inverse)) {
      message("getting cached data...")
      return(inverse)
  }
 data <- x$get()
 ##check if matrix is a square invertible matrix
  if(det(data) !=0){
       inverse <- solve(data, ...)
       x$setinverse(inverse)
       inverse
  }else{
        message("not a square invertible matrix...")
    
  }
}

## Example
## > A <- matrix( c(5, 1, 0,3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)
## > cmatrix <- makeChaceMatrix(A)
## > cmatrix$get()
##      [,1] [,2] [,3]
## [1,]    5    1    0
## [2,]    3   -1    2
## [3,]    4    0   -1
## > cacheSolve(cmatrix)
##        [,1]    [,2]   [,3]
## [1,] 0.0625  0.0625  0.125
## [2,] 0.6875 -0.3125 -0.625
## [3,] 0.2500  0.2500 -0.500
## > cacheSolve(cmatrix)
## getting cached data...
##        [,1]    [,2]   [,3]
## [1,] 0.0625  0.0625  0.125
## [2,] 0.6875 -0.3125 -0.625
## [3,] 0.2500  0.2500 -0.500
