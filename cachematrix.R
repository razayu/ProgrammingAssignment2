## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  cache <- NULL
  
  # store a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # since the matrix is assigned a new value, flush the cache
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the given argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(y, ...) {

  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists, return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # else, get the matrix 
  # calculate the inverse
  # store result in cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # return the inverse
  inverse
}
