## Put comments here that give an overall description of what your
## functions do

## this function allows for a matrix's inverse to be cached
makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## this function calls the cached matrix to calculate
## an inverse, and store it, so that it is not recalculated
## everytime
cacheSolve <- function(x, ...) {
  browser()
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message('This is cached')
    return(inverse)
  }
  cache_matrix <- x$get()
  inverse <- solve(cache_matrix, ...)
  x$setInverse(inverse)
  return(inverse)
        ## Return a matrix that is the inverse of 'x'
}

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
