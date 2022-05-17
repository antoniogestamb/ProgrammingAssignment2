
if(!require(MASS))+
  install.packages("MASS")

## makeCacheMatrix is a function that creates a special object in matrix format 
## that can cache the inverse of a quadratic matrix as an input argument; and
## The input argument of the matrix function is given by “x”.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that calculates the inverse of the matrix input 
## object returned by makeCacheMatrix;
## Briefly, cacheSolve has the function of storing numerical information from
## an inverse matrix through the solution of linear equations; and
## The solve() function was used to solve a system of linear equations.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


my_matrix <- makeCacheMatrix(matrix(1:9, 3, 3))
my_matrix$get()
