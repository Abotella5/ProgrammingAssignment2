## Calcule and caching the inverse of a matrix for don't calcule it
## again if the inverse has already been calculated

## Return a list with functions to set and get the matrix, and
## set an get the matrix inverse calculated

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Cacule the inverse matrix and stored it in cache or, if
## the inverse matrix has been calculated, return its value
### from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}