
##makeCacheMatrix:
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(matrix){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##cacheSolve:
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated, the cacheSolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...){
  d <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
