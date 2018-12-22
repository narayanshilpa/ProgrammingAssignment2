##Programming Assignment2 - Inspired from makeVector and cacheMean examples.

## makeCacheMatrix creates a matrix and caches it's inverse.


makeCacheMatrix <- function(m1 = matrix()) {
  
  inv<- NULL
  set <- function(m2) {
    m1 <<- m2
    inv <<- NULL
  }
  get <- function() m1
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse= getInverse)
  
}

## cacheSolve function retreives inverse from the cache

cacheSolve <- function(m1, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- m1$getInverse()
  if(!is.null(inv)) {
    message("getting matrix inverse from cached data")
    return(inv)
  }
  m1Data <- m1$get()
  inv <- solve(m1Data, ...)
  m1$setInverse(inv)
  inv
}